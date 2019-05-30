info( logger, "FORECAST_CARTOLA::Prepara os dados das partidas" )

dados_partidas <- fromJSON( 
  paste( 
    readLines( "https://api.cartolafc.globo.com/partidas" ), 
    collapse = "" ) 
)



info( logger, "FORECAST_CARTOLA::Ajusta dados partidas" )

informacoes_partidas <- dados_partidas[1] %>%
  data.frame() %>%
  select( -partidas.url_transmissao,
          -partidas.url_confronto )

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



info( logger, "FORECAST_CARTOLA::Partidas atuais" )

partidas_atuais <- informacoes_partidas %>% 
  mutate( rodada_id = max(informacoes_jogadores_atuais$rodada_id),
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
          partidas.local) %>% 
  as_tibble()

partidas_atuais %<>% 
  mutate( partidas.partida_data = ymd_hms(partidas.partida_data) )

rm(dados_partidas, informacoes_partidas, label_clubes)
