info( logger, "MODELO_CARTOLA::modelo goleiros" )

info( logger, "MODELO_CARTOLA::filtra goleiros" )

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



info( logger, "MODELO_CARTOLA::criar defasagens" )

goleiros %<>%
  group_by( atleta_id ) %>%
  mutate( lPE = lag(PE, n = 1),
          lPE2 = lag(PE, n = 2),
          lSG = lag(SG, n = 1),
          lSG2 = lag(SG, n = 2),
          lCA = lag(CA, n = 1),
          lCA2 = lag(CA, n = 2),
          lFS = lag(FS, n = 1),
          lFS2 = lag(FS, n = 2),
          lDD = lag(DD, n = 1),
          lDD2 = lag(DD, n = 2),
          lGS = lag(GS, n = 1),
          lGS2 = lag(GS, n = 2),
          lDP = lag(DP, n = 1),
          lDP2 = lag(DP, n = 2) ) %>%
  ungroup %>% 
  na.omit()



info( logger, "MODELO_CARTOLA::variaveis da forca do adversario" )

goleiros %<>%
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
  select( -fez, -mandante.x, -mandante,
          -rodada_id.x, -rodada_id.y ) %>%
  rename(., ata_ptos_clube_adv = ata_ptos_clube,
         meio_ptos_clube_adv = meio_ptos_clube ) %>%
  mutate( mandante = ifelse( mandante.y == "S", 1, 0 ) ) %>% 
  select( -mandante.y )



info( logger, "MODELO_CARTOLA::dados treino" )

dados_treino <- goleiros %>% 
  select( atleta_id, apelido,
          clube_casa,
          clube_visitante,
          pontos_num,
          lPE, lSG, lCA, lFS, lDD, lGS, lDP,
          lPE2, lSG2, lCA2, lFS2, lDD2, lGS2, lDP2,
          gol_ptos_clube,
          zag_ptos_clube,
          ata_ptos_clube_adv,
          meio_ptos_clube_adv,
          mandante,
          rodada_id )



info( logger, "MODELO_CARTOLA::etapa de clustering iniciada" )

set.seed(54321)
cluster_model_goleiros <- dados_treino %>%
  group_by( atleta_id ) %>%
  summarise_at( vars( lPE, lSG, lCA, lFS, lDD, lGS, lDP ),
                funs( sum ) ) %>%
  select( -atleta_id ) %>%
  mutate_all( funs(rescale) ) %>%
  kcca(., 3, family = kccaFamily("kmeans") )

grupos_dadosTreinoGol <- dados_treino %>%
  group_by( atleta_id ) %>%
  summarise_at( vars( lPE, lSG, lCA, lFS, lDD, lGS, lDP ),
                funs( sum ) ) %>%
  mutate_at( vars( lPE, lSG, lCA, lFS, lDD, lGS, lDP ),
             funs(rescale) ) %>%
  mutate( grupos = predict(cluster_model_goleiros) ) %>%
  select( atleta_id, grupos ) %>%
  mutate( grupos = paste( "gr_kmeans_m1", grupos, sep = "_" ) ) %>%
  spread( key = grupos, value = grupos ) %>%
  mutate_at( .vars = vars(-atleta_id),
             .funs = funs( ifelse( is.na(.) == TRUE, 0, 1) ) ) %>% 
  mutate( base = "Treino" )

dados_treino %<>%
  right_join(., y = grupos_dadosTreinoGol, by = "atleta_id" ) %>%
  select( -base, -atleta_id, -gr_kmeans_m1_1,
          -apelido,
          -clube_casa,
          -clube_visitante,
          -rodada_id )



info( logger, "MODELO_CARTOLA::obtem modelo" )

modelo_goleiros <- lm( pontos_num ~., data = dados_treino )



info( logger, "MODELO_CARTOLA::salva modelo goleiros" )

save( cluster_model_goleiros, modelo_goleiros, file = "data/modelo_goleiros.RData" )

rm( goleiros, cluster_model_goleiros, modelo_goleiros, dados_treino, grupos_dadosTreinoGol )









