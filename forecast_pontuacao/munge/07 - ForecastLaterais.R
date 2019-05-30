info( logger, "FORECAST_CARTOLA::forecast laterais" )

load( "/data-science/projetos/databol/projeto_cartola/modelo_pontuacao/data/modelo_laterais.RData" )


info( logger, "FORECAST_CARTOLA::filtra meio-campo" )

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

dados_laterais_rodada <- dados_cartola %>%
  filter( posicao == "lat",
          rodada_id == rodada ) %>% 
  select( rodada_id, clube_id, clube, status, atleta_id, apelido,
          preco_num, variacao_num, posicao,
          mandante, clube_adv_id, clube_adv )



info( logger, "FORECAST_CARTOLA::preparar dados forecast laterais" )

info( logger, "FORECAST_CARTOLA::clusterizar laterais" )

base_cluster <- laterais %>%
  group_by( atleta_id ) %>%
  mutate( lfin = fin,
          lfin2 = lag(fin, n = 1),
          lRB_FC = RB_FC,
          lRB_FC2 = lag(RB_FC, n = 1),
          lA_PE = A_PE,
          lA_PE2 = lag(A_PE, n = 1),
          lFS_FC = FS_FC,
          lFS_FC2 = lag(FS_FC, n = 1),
          lG = G,
          lG2 = lag(G, n = 1),
          lI = I,
          lI2 = lag(I, n = 1),
          lPE = PE,
          lPE2 = lag(PE, n = 1),
          lFC = FC,
          lFC2 = lag(FC, n = 1),
          lCA = CA,
          lCA2 = lag(CA, n = 1),
          lSG = SG,
          lSG2 = lag(SG, n = 1) ) %>%
  na.omit() %>% 
  select( atleta_id,
          lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA, lSG,
          lfin2, lRB_FC2, lA_PE2, lFS_FC2, lG2, lI2, lPE2, lFC2, lCA2, lSG2 ) %>% 
  ungroup

grupos_cluster <- base_cluster %>%
  group_by( atleta_id ) %>%
  summarise_at( vars( lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA, lSG ), funs(sum) ) %>%
  mutate_at( vars( lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA, lSG ), funs(rescale) ) %>%
  mutate( grupos = predict( cluster_model_laterais, .[-1] ) ) %>%
  select( atleta_id, grupos )

grupos_cluster %<>% 
  mutate( grupos = paste( "gr_kmeans_m1", grupos, sep = "_" ) ) %>%
  spread( key = grupos, value = grupos ) %>%
  mutate_at( .vars = vars(-atleta_id),
             .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) )



info( logger, "FORECAST_CARTOLA::dados das defasagens" )

dados_lags <- laterais %>%
  group_by( atleta_id ) %>%
  mutate( lfin = fin,
          lfin2 = lag(fin, n = 1),
          lRB_FC = RB_FC,
          lRB_FC2 = lag(RB_FC, n = 1),
          lA_PE = A_PE,
          lA_PE2 = lag(A_PE, n = 1),
          lFS_FC = FS_FC,
          lFS_FC2 = lag(FS_FC, n = 1),
          lG = G,
          lG2 = lag(G, n = 1),
          lI = I,
          lI2 = lag(I, n = 1),
          lPE = PE,
          lPE2 = lag(PE, n = 1),
          lFC = FC,
          lFC2 = lag(FC, n = 1),
          lCA = CA,
          lCA2 = lag(CA, n = 1),
          lSG = SG,
          lSG2 = lag(SG, n = 1) ) %>%
  na.omit() %>% 
  select( atleta_id, rodada_id,
          lfin, lRB_FC, lA_PE, lFS_FC, lG, lI, lPE, lFC, lCA, lSG,
          lfin2, lRB_FC2, lA_PE2, lFS_FC2, lG2, lI2, lPE2, lFC2, lCA2, lSG2 ) %>% 
  ungroup() %>% 
  arrange( desc(rodada_id) ) %>% 
  distinct( atleta_id, .keep_all = TRUE ) %>% 
  select( -rodada_id )



info( logger, "FORECAST_CARTOLA::dados da forca do adversario" )

dados_forca_clubes <- dados_laterais_rodada %>% 
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
  select( -mandante.x, -mandante,
          -rodada_id.x, -rodada_id.y ) %>%
  rename(., zag_ptos_clube_adv = zag_ptos_clube,
         ata_ptos_clube_adv = ata_ptos_clube ) %>%
  mutate( mandante = ifelse( mandante.y == "S", 1, 0 ) ) %>% 
  select( -mandante.y )



info( logger, "FORECAST_CARTOLA::obtem as previsoes" )

dados_forecast <- dados_lags %>% 
  left_join(., dados_forca_clubes,
            by = "atleta_id" ) %>% 
  left_join(., grupos_cluster,
            by = "atleta_id" ) %>% 
  na.omit

projecoes_laterais <- dados_forecast %>% 
  mutate( pontuacao_projetada = predict( modelo_laterais, .) ) %>% 
  select( atleta_id, apelido, posicao, status, preco_num, variacao_num, 
          clube_id, clube, clube_adv_id, clube_adv, pontuacao_projetada )

rm(laterais, dados_laterais_rodada, base_cluster, grupos_cluster, dados_lags, dados_forca_clubes, dados_forecast, cluster_model_laterais, modelo_laterais )

