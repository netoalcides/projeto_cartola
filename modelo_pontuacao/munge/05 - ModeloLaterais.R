info( logger, "MODELO_CARTOLA::modelo laterais" )

info( logger, "MODELO_CARTOLA::filtra laterais" )

laterais <- dados_cartola %>%
  filter( posicao == "lat" ) %>%
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

laterais %<>%
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
          lCA2 = lag(CA, n = 2),
          lSG = lag(SG, n = 1),
          lSG2 = lag(SG, n = 2) ) %>%
  ungroup %>% 
  na.omit()



info( logger, "MODELO_CARTOLA::variaveis da forca do adversario" )

laterais %<>%
  left_join(., y = lat_data %>%
              select( clube_id, lat_ptos_clube, mandante, rodada_id ),
            by = "clube_id" ) %>%
  filter( mandante.x == mandante.y,
          rodada_id.x == rodada_id.y ) %>%
  left_join(., y = zag_data %>%
              select( clube_id, zag_ptos_clube, mandante, rodada_id ),
            by = c( "clube_adv_id" = "clube_id" ) ) %>%
  filter( mandante.x != mandante,
          rodada_id.x == rodada_id ) %>%
  select( -mandante, -rodada_id ) %>%
  left_join(., y = ata_data %>%
              select( clube_id, ata_ptos_clube, mandante, rodada_id ),
            by = c( "clube_adv_id" = "clube_id" ) ) %>%
  filter( mandante.x != mandante,
          rodada_id.x == rodada_id ) %>%
  select( -mandante, -rodada_id ) %>%
  left_join(., y = lat_data %>%
              select( clube_id, lat_ptos_clube, mandante, rodada_id ) %>%
              rename(lat_ptos_clube_adv = lat_ptos_clube ),
            by = c( "clube_adv_id" = "clube_id" ) ) %>%
  filter( mandante.x != mandante,
          rodada_id.x == rodada_id ) %>%
  select( -fez, -mandante.x, -mandante,
          -rodada_id.x, -rodada_id.y ) %>%
  rename(., zag_ptos_clube_adv = zag_ptos_clube,
         ata_ptos_clube_adv = ata_ptos_clube ) %>%
  mutate( mandante = ifelse( mandante.y == "S", 1, 0 ) ) %>% 
  select( -mandante.y )



info( logger, "MODELO_CARTOLA::dados treino" )

dados_treino <- laterais %>% 
  select( atleta_id, apelido,
          clube_casa,
          clube_visitante,
          pontos_num,
          lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA, lSG,
          lfin2, lRB_FC2, lA_PE2, lFS_FC2, lG2, lI2, lPE2, lFC2, lCA2, lSG2,
          lat_ptos_clube,
          zag_ptos_clube_adv,
          ata_ptos_clube_adv,
          lat_ptos_clube_adv,
          mandante,
          rodada_id )



info( logger, "MODELO_CARTOLA::etapa de clustering iniciada" )

set.seed(4321)
cluster_model_laterais <- dados_treino %>%
  group_by( atleta_id ) %>%
  summarise_at( vars( lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA, lSG ),
                funs( sum ) ) %>%
  select( -atleta_id ) %>%
  mutate_all( funs(rescale) ) %>%
  kcca(., 5, family = kccaFamily("kmeans") )

grupos_dadosTreinoLat <- dados_treino %>%
  group_by( atleta_id ) %>%
  summarise_at( vars( lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA, lSG ),
                funs( sum ) ) %>%
  mutate_at( vars( lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA, lSG ),
             funs(rescale) ) %>%
  mutate( grupos = predict(cluster_model_laterais) ) %>%
  select( atleta_id, grupos ) %>%
  mutate( grupos = paste( "gr_kmeans_m1", grupos, sep = "_" ) ) %>%
  spread( key = grupos, value = grupos ) %>%
  mutate_at( .vars = vars(-atleta_id),
             .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) ) %>% 
  mutate( base = "Treino" )  

dados_treino %<>%
  right_join(., y = grupos_dadosTreinoLat, by = "atleta_id" ) %>%
  select( -base, -atleta_id, -gr_kmeans_m1_1,
          -apelido,
          -clube_casa,
          -clube_visitante,
          -rodada_id )



info( logger, "MODELO_CARTOLA::obtem modelo" )

# Linear
modelo_laterais <- lm( pontos_num ~., data = dados_treino )



info( logger, "CARTOLA_2017::salva modelo laterais" )

save( cluster_model_laterais, modelo_laterais, file = "data/modelo_laterais.RData" )

rm( laterais, cluster_model_laterais, modelo_laterais, grupos_dadosTreinoLat )

