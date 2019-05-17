info( logger, "MODELO_CARTOLA::modelo meio" )
info( logger, "MODELO_CARTOLA::filtra meio" )

meio <- dados_cartola %>%
  filter( posicao == "mei" ) %>%
  mutate( fin = FF + FD + FT,
          RB_FC = ifelse( is.infinite(RB/FC) == TRUE, 0, RB/FC ),
          A_PE = ifelse( is.infinite(A/PE) == TRUE, 0, A/PE ),
          FS_FC = ifelse( is.infinite(FS/FC) == TRUE, 0, FS/FC ) ) %>%
  replace_na( list( RB_FC = 0,
                    A_PE = 0,
                    FS_FC = 0 ) ) %>%
  mutate( fez = FC + PE + RB + SG + CA + FD + FS + FF + I + G + DD + GS + A + CV + FT + GC + DP + PP ) %>%
  filter( fez != 0 & variacao_num != 0 )



info( logger, "MODELO_CARTOLA::criar defasagens" )

meio %<>%
  group_by( atleta_id ) %>%
  mutate( lfin = lag(fin, n = 1),
          lfin2 = lag(fin, n = 2),
          lRB_FC = lag(RB_FC, n = 1),
          lRB_FC2 = lag(RB_FC, n = 2),
          lA_PE = lag(A_PE, n = 1),
          lA_PE2 = lag(A_PE, n = 2),
          lFS_FC = lag(FS_FC, n = 1),
          lFS_FC2 = lag(FS_FC, n = 2),
          lG = lag(G, n = 1),
          lG2 = lag(G, n = 2),
          lI = lag(I, n = 1),
          lI2 = lag(I, n = 2),
          lPE = lag(PE, n = 1),
          lPE2 = lag(PE, n = 2),
          lFC = lag(FC, n = 1),
          lFC2 = lag(FC, n = 2),
          lCA = lag(CA, n = 1),
          lCA2 = lag(CA, n = 2) ) %>%
  ungroup %>% 
  na.omit()



info( logger, "MODELO_CARTOLA::variaveis da forca do adversario" )

meio %<>%
  left_join(., y = meio_data %>%
              select( clube_id, meio_ptos_clube, mandante, rodada_id ),
            by = "clube_id" ) %>%
  filter( mandante.x == mandante.y,
          rodada_id.x == rodada_id.y ) %>%
  left_join(., y = zag_data %>%
              select( clube_id, zag_ptos_clube, mandante, rodada_id ),
            by = c( "clube_adv_id" = "clube_id" ) ) %>%
  filter( mandante.x != mandante,
          rodada_id.x == rodada_id ) %>%
  select( -mandante, -rodada_id ) %>%
  left_join(., y = lat_data %>%
              select( clube_id, lat_ptos_clube, mandante, rodada_id ),
            by = c( "clube_adv_id" = "clube_id" ) ) %>%
  filter( mandante.x != mandante,
          rodada_id.x == rodada_id ) %>%
  select( -mandante, -rodada_id ) %>%
  left_join(., y = gol_data %>%
              select( clube_id, gol_ptos_clube, mandante, rodada_id ),
            by = c( "clube_adv_id" = "clube_id" ) ) %>%
  filter( mandante.x != mandante,
          rodada_id.x == rodada_id ) %>%
  select( -fez, -mandante.x, -mandante,
          -rodada_id.x, -rodada_id.y ) %>%
  rename(., zag_ptos_clube_adv = zag_ptos_clube,
         lat_ptos_clube_adv = lat_ptos_clube,
         gol_ptos_clube_adv = gol_ptos_clube ) %>%
  mutate( mandante = ifelse( mandante.y == "S", 1, 0 ) ) %>% 
  select( -mandante.y )



info( logger, "MODELO_CARTOLA::dados treino" )

dados_treino <- meio %>% 
  select( atleta_id, apelido,
          clube_casa,
          clube_visitante,
          pontos_num,
          lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA,
          lfin2, lRB_FC2, lA_PE2, lFS_FC2, lG2, lI2, lPE2, lFC2, lCA2,
          meio_ptos_clube,
          zag_ptos_clube_adv,
          gol_ptos_clube_adv,
          mandante,
          rodada_id ) 



info( logger, "CARTOLA_2017::etapa de clustering iniciada" )

set.seed(54321)
cluster_model_meio <- dados_treino %>%
  group_by( atleta_id ) %>%
  summarise_at( vars( lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA ),
                funs( sum ) ) %>%
  select( -atleta_id ) %>%
  mutate_all( funs(rescale) ) %>%
  kcca(., 9, family = kccaFamily("kmeans") ) # novo

grupos_dadosTreinoMei <- dados_treino %>%
  group_by( atleta_id ) %>%
  summarise_at( vars( lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA ),
                funs( sum ) ) %>%
  mutate_at( vars( lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA ),
             funs(rescale) ) %>%
  mutate( grupos = predict(cluster_model_meio) ) %>%
  select( atleta_id, grupos ) %>%
  mutate( grupos = paste( "gr_kmeans_m1", grupos, sep = "_" ) ) %>%
  spread( key = grupos, value = grupos ) %>%
  mutate_at( .vars = vars(-atleta_id),
             .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) ) %>% 
  mutate( base = "Treino" ) # novo

dados_treino %<>%
  right_join(., y = grupos_dadosTreinoMei, by = "atleta_id" ) %>%
  select( -base, -atleta_id, -gr_kmeans_m1_1,
          -apelido,
          -clube_casa,
          -clube_visitante,
          -rodada_id )



info( logger, "MODELO_CARTOLA::obtem modelo" )

# Linear
modelo_meio <- lm( pontos_num ~., data = dados_treino )



info( logger, "MODELO_CARTOLA::salva modelo meio" )

save( cluster_model_meio, modelo_meio, file = "data/modelo_meio.RData" )

rm( meio, cluster_model_meio, modelo_meio, grupos_dadosTreinoMei )



