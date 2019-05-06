info( logger, "CARTOLA_DADOS::salvar dados de classificacao" )

classificacao <- label_clubes %>% 
  rename( clube_rank = classificacao ) %>% 
  mutate( ano = year( Sys.Date() ) )

write_csv( classificacao,
           path = paste0( 'data/classificacao/', year( Sys.Date() ), '_classificacao_', label_clubes$rodada_id[1], '.csv' ) )

