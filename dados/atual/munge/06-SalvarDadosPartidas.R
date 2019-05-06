info( logger, "CARTOLA_DADOS::salvar dados de partidas" )

info( logger, "CARTOLA_DADOS::dados de partidas rodada atual" )

informacoes_partidas %<>% 
  right_join(., x = label_clubes, by = c("clube_id" = "partidas.clube_visitante_id") ) %>%
  rename(., partidas.clube_visitante_id = clube_id ) %>%
  rename(., partidas.clube_visitante_nome = clube ) %>%
  right_join(., x = label_clubes, by = c("clube_id" = "partidas.clube_casa_id") ) %>%
  rename(., partidas.clube_casa_id = clube_id ) %>%
  rename(., partidas.clube_casa_nome = clube ) %>%
  select( partidas.clube_casa_id,
          partidas.clube_casa_nome,
          partidas.clube_casa_posicao,
          partidas.clube_visitante_id,
          partidas.clube_visitante_nome,
          partidas.clube_visitante_posicao,
          partidas.partida_data,
          partidas.local,
          partidas.placar_oficial_mandante,
          partidas.placar_oficial_visitante )

partidas <- informacoes_partidas %>% 
  mutate( rodada_id = label_clubes$rodada_id[1]+1,
          ano = year( Sys.Date() ) ) %>% 
  select( rodada_id,
          clube_casa_id = partidas.clube_casa_id,
          clube_visitante_id = partidas.clube_visitante_id,
          placar_oficial_mandante = partidas.placar_oficial_mandante,
          placar_oficial_visitante = partidas.placar_oficial_visitante,
          clube_casa = partidas.clube_casa_nome,
          clube_visitante = partidas.clube_visitante_nome,
          ano,
          partidas.partida_data,
          partidas.local)



info( logger, "CARTOLA_DADOS::dados de partidas rodada anterior" )

informacoes_partidas_anterior %<>% 
  right_join(., x = label_clubes, by = c("clube_id" = "partidas.clube_visitante_id") ) %>%
  rename(., partidas.clube_visitante_id = clube_id ) %>%
  rename(., partidas.clube_visitante_nome = clube ) %>%
  right_join(., x = label_clubes, by = c("clube_id" = "partidas.clube_casa_id") ) %>%
  rename(., partidas.clube_casa_id = clube_id ) %>%
  rename(., partidas.clube_casa_nome = clube ) %>%
  select( partidas.clube_casa_id,
          partidas.clube_casa_nome,
          partidas.clube_casa_posicao,
          partidas.clube_visitante_id,
          partidas.clube_visitante_nome,
          partidas.clube_visitante_posicao,
          partidas.partida_data,
          partidas.local,
          partidas.placar_oficial_mandante,
          partidas.placar_oficial_visitante )

partidas_anterior <- informacoes_partidas_anterior %>% 
  mutate( rodada_id = label_clubes$rodada_id[1],
          ano = year( Sys.Date() ) ) %>% 
  select( rodada_id,
          clube_casa_id = partidas.clube_casa_id,
          clube_visitante_id = partidas.clube_visitante_id,
          placar_oficial_mandante = partidas.placar_oficial_mandante,
          placar_oficial_visitante = partidas.placar_oficial_visitante,
          clube_casa = partidas.clube_casa_nome,
          clube_visitante = partidas.clube_visitante_nome,
          ano,
          partidas.partida_data,
          partidas.local)

write_csv( partidas_anterior,
           path = paste0( 'data/partidas/', year( Sys.Date() ), '_partidas_', label_clubes$rodada_id[1], '.csv' ) )





