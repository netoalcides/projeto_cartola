info( logger, "CARTOLA_DADOS::obtem informacoes gerais dos jogadores" )

informacoes_jogadores <- dados_mercado$atletas[1:14] %>%
  as_data_frame %>%
  select( -foto,
          -nome,
          -preco_num,
          -variacao_num )


info( logger, "CARTOLA_DADOS::obtem precos dos jogadores" )

precos_jogadores <- dados_mercado$atletas[1:13] %>%
  as_data_frame %>%
  select( atleta_id,
          apelido,
          rodada_id,
          status_id,
          preco_num,
          variacao_num ) %>%
  mutate( rodada_id = rodada_id + 1,
          preco_num_anterior = preco_num - variacao_num )


info( logger, "CARTOLA_DADOS::obtem labels posicoes" )

label_posicoes <- dados_mercado$posicoes %>%
  unlist() %>%
  matrix(., 3, 6) %>%
  t() %>%
  data.frame(., stringsAsFactors = FALSE ) %>%
  rename( posicao_id = X1,
          posicao = X3 ) %>%
  mutate( posicao_id = as.integer( posicao_id ) ) %>%
  select( -X2 )


info( logger, "CARTOLA_DADOS::obtem labels status" )

label_status <- dados_mercado$status %>%
  unlist() %>%
  matrix(., 2, 5) %>%
  t() %>%
  data.frame(., stringsAsFactors = FALSE ) %>%
  rename( status_id = X1,
          status = X2 ) %>%
  mutate( status_id = as.integer(status_id) )


info( logger, "CARTOLA_DADOS::obtem labels clubes" )

label_clubes <- dados_mercado[2] %>%
  unlist() %>%
  matrix(., 8, 20) %>%
  t() %>%
  data.frame(., stringsAsFactors = FALSE ) %>%
  rename( clube_id = X1,
          clube = X2,
          classificacao = X4) %>%
  mutate( clube_id = as.integer(clube_id),
          classificacao = as.integer(classificacao),
          rodada_id = informacoes_jogadores$rodada_id[1] ) %>%
  select( clube_id,
          clube,
          rodada_id,
          classificacao )



info( logger, "CARTOLA_DADOS::obtem scouts" )

scouts_jogadores <- dados_mercado$atletas$scout %>% 
  as_data_frame()
