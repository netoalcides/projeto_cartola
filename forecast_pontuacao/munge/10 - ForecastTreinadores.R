info( logger, "FORECAST_CARTOLA::forecast treinadores" )

dados_treinadores_rodada <- dados_cartola %>%
  filter( posicao == "tec",
          rodada_id == rodada ) %>% 
  select( atleta_id, apelido, posicao, status, 
          preco_num, variacao_num, mandante,
          clube_id, clube, clube_adv_id, clube_adv )

pontuacao_esperada_clubes <- bind_rows( projecoes_atacantes,
           projecoes_meiocampo,
           projecoes_laterais,
           projecoes_zagueiros,
           projecoes_goleiros) %>% 
  group_by( clube_id ) %>% 
  summarise( pontuacao_projetada = median(pontuacao_projetada) ) %>% 
  ungroup

projecoes_treinadores <- dados_treinadores_rodada %>% 
  left_join(., pontuacao_esperada_clubes,
            by = "clube_id") %>% 
  na.omit()

projecoes_treinadores %<>% 
  mutate( mandante = ifelse( mandante == 'S', 1, 0 ) )

rm(pontuacao_esperada_clubes, dados_treinadores_rodada)
