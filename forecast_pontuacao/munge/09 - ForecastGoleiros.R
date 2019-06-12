info( logger, "FORECAST_CARTOLA::forecast goleiros" )

load( "/data-science/projetos/databol/projeto_cartola/modelo_pontuacao/data/modelo_goleiros.RData" )


info( logger, "FORECAST_CARTOLA::filtra goleiros" )

goleiros <- dados_cartola %>%
  filter( posicao == "gol" ) %>%
  mutate( fin = FF + FD + FT,
          RB_FC = ifelse( is.infinite(RB/FC) == TRUE, 0, RB/FC ),
          A_PE = ifelse( is.infinite(A/PE) == TRUE, 0, A/PE ),
          FS_FC = ifelse( is.infinite(FS/FC) == TRUE, 0, FS/FC ) ) %>%
  replace_na( list( RB_FC = 0,
                    A_PE = 0,
                    FS_FC = 0 ) ) %>%
  mutate( fez = FC + PE + RB + SG + CA + FD + FS + FF + I + G + DD + GS + A + CV + FT + GC + DP + PP ) %>%
  filter( fez != 0 & variacao_num != 0 )

dados_goleiros_rodada <- dados_cartola %>%
  filter( posicao == "gol",
          rodada_id == rodada ) %>% 
  select( rodada_id, clube_id, clube, status, atleta_id, apelido,
          preco_num, variacao_num, posicao,
          mandante, clube_adv_id, clube_adv )



info( logger, "FORECAST_CARTOLA::preparar dados forecast goleiros" )

info( logger, "FORECAST_CARTOLA::clusterizar goleiros" )

base_cluster <- goleiros %>%
  group_by( atleta_id ) %>%
  mutate( lPE = PE,
          lPE2 = lag(PE, n = 1),
          lSG = SG,
          lSG2 = lag(SG, n = 1),
          lCA = CA,
          lCA2 = lag(CA, n = 1),
          lFS = FS,
          lFS2 = lag(FS, n = 1),
          lDD = DD,
          lDD2 = lag(DD, n = 1),
          lGS = GS,
          lGS2 = lag(GS, n = 1),
          lDP = DP,
          lDP2 = lag(DP, n = 1) ) %>%
  na.omit() %>% 
  select( atleta_id, 
          lPE, lSG, lCA, lFS, lDD, lGS, lDP,
          lPE2, lSG2, lCA2, lFS2, lDD2, lGS2, lDP2 ) %>% 
  ungroup

grupos_cluster <- base_cluster %>%
  group_by( atleta_id ) %>%
  summarise_at( vars( lPE, lSG, lCA, lFS, lDD, lGS, lDP ), funs(sum) ) %>%
  mutate_at( vars( lPE, lSG, lCA, lFS, lDD, lGS, lDP ), funs(rescale) ) %>%
  mutate( grupos = predict( cluster_model_goleiros, .[-1] ) ) %>%
  select( atleta_id, grupos )

grupos_cluster %<>% 
  mutate( grupos = paste( "gr_kmeans_m1", grupos, sep = "_" ) ) %>%
  spread( key = grupos, value = grupos ) %>%
  mutate_at( .vars = vars(-atleta_id),
             .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) )



info( logger, "FORECAST_CARTOLA::dados das defasagens" )

dados_lags <- goleiros %>%
  group_by( atleta_id ) %>%
  mutate( lPE = PE,
          lPE2 = lag(PE, n = 1),
          lSG = SG,
          lSG2 = lag(SG, n = 1),
          lCA = CA,
          lCA2 = lag(CA, n = 1),
          lFS = FS,
          lFS2 = lag(FS, n = 1),
          lDD = DD,
          lDD2 = lag(DD, n = 1),
          lGS = GS,
          lGS2 = lag(GS, n = 1),
          lDP = DP,
          lDP2 = lag(DP, n = 1) ) %>%
  na.omit() %>% 
  select( atleta_id, rodada_id,
          lPE, lSG, lCA, lFS, lDD, lGS, lDP,
          lPE2, lSG2, lCA2, lFS2, lDD2, lGS2, lDP2 ) %>% 
  ungroup %>% 
  arrange( desc(rodada_id) ) %>% 
  distinct( atleta_id, .keep_all = TRUE ) %>% 
  select( -rodada_id )



info( logger, "FORECAST_CARTOLA::dados da forca do adversario" )

dados_forca_clubes <- dados_goleiros_rodada %>%
  left_join(., y = gol_data %>%
              select( clube_id, gol_ptos_clube, mandante, rodada_id ),
            by = "clube_id" ) %>%
  filter( mandante.x == mandante.y,
          rodada_id.x == rodada_id.y ) %>%
  left_join(., y = zag_data %>%
              select( clube_id, zag_ptos_clube, mandante, rodada_id ),
            by = "clube_id" ) %>%
  filter( mandante.x == mandante,
          rodada_id.x == rodada_id ) %>%
  select( -mandante, -rodada_id ) %>%
  left_join(., y = ata_data %>%
              select( clube_id, ata_ptos_clube, mandante, rodada_id ),
            by = c( "clube_adv_id" = "clube_id" ) ) %>%
  filter( mandante.x != mandante,
          rodada_id.x == rodada_id ) %>%
  select( -mandante, -rodada_id ) %>%
  left_join(., y = meio_data %>%
              select( clube_id, meio_ptos_clube, mandante, rodada_id ),
            by = c( "clube_adv_id" = "clube_id" ) ) %>%
  filter( mandante.x != mandante,
          rodada_id.x == rodada_id ) %>%
  select( -mandante.x, -mandante,
          -rodada_id.x, -rodada_id.y ) %>%
  rename(., ata_ptos_clube_adv = ata_ptos_clube,
         meio_ptos_clube_adv = meio_ptos_clube ) %>%
  mutate( mandante = ifelse( mandante.y == "S", 1, 0 ) ) %>% 
  select( -mandante.y )



info( logger, "FORECAST_CARTOLA::obtem as previsoes" )

dados_forecast <- dados_lags %>% 
  left_join(., dados_forca_clubes,
            by = "atleta_id" ) %>% 
  left_join(., grupos_cluster,
            by = "atleta_id" ) %>% 
  na.omit

projecoes_goleiros <- dados_forecast %>% 
  mutate( pontuacao_projetada = predict( modelo_goleiros, .) ) %>% 
  select( atleta_id, apelido, posicao, status, preco_num, variacao_num, 
          clube_id, clube, clube_adv_id, clube_adv, mandante, pontuacao_projetada )

rm(goleiros, dados_goleiros_rodada, base_cluster, grupos_cluster, dados_lags, dados_forca_clubes, dados_forecast, cluster_model_goleiros, modelo_goleiros,
   ata_data, gol_data, lat_data, meio_data, zag_data )















