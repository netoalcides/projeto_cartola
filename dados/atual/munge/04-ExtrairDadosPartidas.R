info( logger, "CARTOLA_DADOS::obtem partidas" )

informacoes_partidas <- dados_partidas[1] %>%
  data.frame() %>%
  select( -partidas.url_transmissao,
          -partidas.url_confronto )
rm(dados_partidas)

informacoes_partidas_anterior <- dados_partidas_anterior[1] %>%
  data.frame() %>%
  select( -partidas.url_transmissao,
          -partidas.url_confronto )
rm(dados_partidas_anterior)
