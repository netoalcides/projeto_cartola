info( logger, "MODELO_CARTOLA::Prepara os dados" )


info( logger, "MODELO_CARTOLA::Ler os dados" )

informacoes_jogadores <- list.files( path = '/data-science/projetos/databol/projeto_cartola/dados/atual/data/scouts', full.names = TRUE ) %>% 
  map_df(., read_csv )

informacoes_partidas <- list.files( path = '/data-science/projetos/databol/projeto_cartola/dados/atual/data/partidas', full.names = TRUE ) %>% 
  map_df(., read_csv )

informacoes_classificacao <- list.files( path = '/data-science/projetos/databol/projeto_cartola/dados/atual/data/classificacao', full.names = TRUE ) %>% 
  map_df(., read_csv )



info( logger, "MODELO_CARTOLA::partidas e classificacao" )

rodadas <- informacoes_partidas %>% 
  distinct(rodada_id)

 informacoes_partidas_classificacao <- foreach( rod = rodadas$rodada_id, .combine = rbind ) %do% {
  
  partidas <- informacoes_partidas %>% 
    filter( rodada_id == rod )
  
  classificacao <- informacoes_classificacao %>%
    filter(rodada_id == rod) %>%
    select( clube_id, clube_rank )

  partidas %>%
    left_join(., classificacao,
              by = c( 'clube_casa_id' = 'clube_id' ) ) %>%
    rename(., clube_rank_casa = clube_rank ) %>%
    left_join(., classificacao,
              by = c( 'clube_visitante_id' = 'clube_id' ) ) %>%
    rename(., clube_rank_visitante = clube_rank )
  
} 

rm(partidas, classificacao, informacoes_partidas, informacoes_classificacao, rod)



info( logger, "MODELO_CARTOLA::jogadores, partidas e classificacao" )

informacoes_jogadores %<>% 
  replace(., is.na(.), 0 )

dados_cartola <- foreach( rod = rodadas$rodada_id, .combine = rbind ) %do% {
  
  jogadores <- informacoes_jogadores %>% 
    filter( rodada_id == rod )
  
  partidas <- informacoes_partidas_classificacao %>%
    filter( rodada_id == rod ) %>%
    select( clube_casa_id, clube_visitante_id,
            clube_casa, clube_visitante,
            clube_rank_casa, clube_rank_visitante )

  casa  <- jogadores %>%
    right_join(., y = partidas, by = c( "clube_id" = "clube_casa_id" ) ) %>%
    mutate( clube_casa_id = clube_id ) %>%
    filter( !is.na(clube) )

  fora <- jogadores %>%
    right_join(., y = partidas, by = c( "clube_id" = "clube_visitante_id" ) ) %>%
    mutate( clube_visitante_id = clube_id ) %>%
    filter( !is.na(clube) )

  bind_rows(casa, fora) %>%
    mutate( mandante = ifelse( clube_id == clube_casa_id, "S", "N") )
  
}

rm(informacoes_jogadores, casa, fora, jogadores, informacoes_partidas_classificacao, partidas, rodadas, rod)



info( logger, "MODELO_CARTOLA::ajusta dados" )

dados_cartola %<>% 
  arrange( atleta_id, rodada_id ) %>% 
  group_by( atleta_id ) %>%
  mutate_at( vars( FC, PE, RB, SG, CA, FD, FS, FF, I, G, DD, GS, A, CV, FT, GC, DP, PP ),
             funs( ajuste_lag(.) ) ) %>% 
  ungroup() %>% 
  mutate( clube_adv_id = ifelse( clube_id == clube_casa_id,
                                 clube_visitante_id,
                                 clube_casa_id ),
          clube_adv = ifelse( clube_id == clube_casa_id,
                              clube_visitante,
                              clube_casa ) )

