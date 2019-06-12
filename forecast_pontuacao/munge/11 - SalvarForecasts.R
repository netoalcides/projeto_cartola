info( logger, "FORECAST_CARTOLA::forecast de todos os jogadores" )

forecast_jogadores <- bind_rows( projecoes_atacantes,
                                 projecoes_meiocampo,
                                 projecoes_laterais,
                                 projecoes_zagueiros,
                                 projecoes_goleiros,
                                 projecoes_treinadores )

write_csv( forecast_jogadores,
           path = 'data/forecast_pontuacao_cartola.csv' )

rm( list = ls( pattern = "projecoes*") )



### enquanto a API nao sai

bind_rows( forecast_jogadores %>% 
             filter( status == 'Provável',
                     posicao == 'ata' ) %>% 
             arrange( desc(pontuacao_projetada) ) %>% 
             head(8),
           forecast_jogadores %>% 
             filter( status == 'Provável',
                     posicao == 'mei' ) %>% 
             arrange( desc(pontuacao_projetada) ) %>% 
             head(10),
           forecast_jogadores %>% 
             filter( status == 'Provável',
                     !posicao %in% c('ata', 'mei') ) %>% 
             group_by( posicao ) %>% 
             top_n( n = 5, wt = pontuacao_projetada)
) %>% 
  select( apelido, posicao, 
         preco_num, variacao_num, pontuacao_projetada,
         clube, clube_adv, mandante ) %>% 
  mutate( pontuacao_projetada = round(pontuacao_projetada, 2) ) %>% 
  write_csv(., path = 'data/maiores_esperados.csv' )

