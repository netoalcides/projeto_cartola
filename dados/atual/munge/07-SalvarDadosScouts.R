info( logger, "CARTOLA_DADOS::salvar dados de scouts jogadores" )

label_clubes %<>%
  select( -rodada_id )

precos_jogadores %<>%
  select( preco_num,
          variacao_num,
          preco_num_anterior )

informacoes_jogadores %<>%
  right_join(., x = label_posicoes, by = "posicao_id" ) %>%
  right_join(., x = label_status, by = "status_id" ) %>%
  right_join(., x = label_clubes, by = "clube_id" ) %>%
  bind_cols(., precos_jogadores ) %>%
  bind_cols(., scouts_jogadores ) %>% 
  mutate( ano = year(Sys.Date()) ) %>% 
  select( -classificacao, -slug ) %>% 
  as_data_frame()
  
write_csv( informacoes_jogadores,
           path = paste0( 'data/scouts/', year( Sys.Date() ), '_scouts_', informacoes_jogadores$rodada_id[1], '.csv' ) )
