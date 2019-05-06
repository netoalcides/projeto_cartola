library(tidyverse)
library(doFuture)

registerDoFuture()
plan(multiprocess)

load('../old/cartola_dados_2014_2017.RData')
load('dados_cartola_2018_por_rodada.RData')

cartola_dados_2014_2017 %>% 
  colnames()

cartola_dados_2014_2017 %>% 
  select( atleta_id, rodada_id, ano, apelido, clube, posicao, G) %>% 
  filter( apelido == 'Dudu', clube == 'Palmeiras' ) %>% 
  arrange(atleta_id, ano, rodada_id) %>% 
  print(n=50)


dados_cartola_2018_por_rodada %>% 
  colnames()

cartola_dados_2014_2017 %>% 
  data.frame() %>% 
  head()

dados_cartola_2018_por_rodada %>% 
  head()


dados_cartola_2018_por_rodada %>% 
  filter( slug == 'dudu') %>% 
  select( apelido, clube, FS, PE, A, FT, FD, FF, G, I, PP, RB, FC, 
          GC, CA, CV, SG, DD, DP, GS) %>% 
  head(60)




dados_1 <- cartola_dados_2014_2017 %>% 
  select( atleta_id, apelido,
          rodada_id, ano,
          clube_id, clube,
          posicao_id, posicao,
          jogos_num, pontos_num, media_num,
          preco_num_anterior, preco_num, variacao_num,            
          FS, PE, A, FT, FD, FF, G, I, PP, RB, FC, 
          GC, CA, CV, SG, DD, DP, GS ) %>% 
  mutate( status_id = NA,
          status = NA )

dados_2 <- dados_cartola_2018_por_rodada %>% 
  select( atleta_id, apelido,
          rodada_id,
          clube_id, clube,
          posicao_id, posicao,
          status_id, status,
          pontos_num, media_num,
          preco_num_anterior, preco_num, variacao_num,
          FS, PE, A, FT, FD, FF, G, I, PP, RB, FC, GC,
          CA, CV, SG, DD, DP, GS ) %>% 
  mutate( ano = 2018,
          jogos_num = NA )




dados_cartola_2018_por_rodada %>% 
  distinct(rodada_id) %>% 
  arrange(rodada_id)



rodadas <- 23:38

dados_faltantes <- foreach( rod = rodadas ) %dopar% {
  
  read_csv( file = paste0( 'https://raw.githubusercontent.com/henriquepgomide/caRtola/master/data/2018/rodada-', rod, '.csv' ) )
  
  
} 

dados_faltantes <- bind_rows( dados_faltantes )

dados_faltantes %>% 
  distinct( atletas.rodada_id )

dados_faltantes %>% 
  data.frame() %>% 
  head()

dados_faltantes %>% 
  colnames()


ddd <- dados_faltantes %>% 
  select( atleta_id = atletas.atleta_id,
          apelido = atletas.apelido,
          rodada_id = atletas.rodada_id,
          clube = atletas.clube.id.full.name,
          posicao = atletas.posicao_id,
          status = atletas.status_id,
          pontos_num = atletas.pontos_num,
          media_num = atletas.media_num,
          preco_num = atletas.preco_num,
          variacao_num = atletas.variacao_num,
          FS, PE, A, FT, FD, FF, G, I, PP, RB, FC, GC,
          CA, CV, SG, DD, DP, GS ) %>% 
  mutate( ano = 2018,
          preco_num_anterior = preco_num - variacao_num )



statuss <- dados_cartola %>% 
  distinct( status_id, status) %>% 
  filter( is.na(status_id) != TRUE )


club <- dados_cartola %>% 
  distinct( clube_id, clube ) %>% 
  filter( is.na(clube_id) != TRUE )

poss <- dados_cartola %>% 
  distinct( posicao_id, posicao )

# clube_id = NA,
# posicao_id = NA,
# status_id = NA

# replace(., is.na(.), 0 )

dados_faltantes_att <- ddd %>% 
  left_join(., statuss,
            by = 'status' ) %>% 
  left_join(., club,
            by = 'clube' ) %>% 
  left_join(., poss,
            by = 'posicao' )

# ajustar os scouts

ajuste_lag <- function(x){
  
  ifelse( is.na( x - lag(x) ) == TRUE, x, x - lag(x) )
  
}

resumo <- dados_cartola %>% 
  filter( ano == 2018 ) %>% 
  select(atleta_id, apelido, FS, PE, A, FT, FD, FF, G, I, PP, RB, FC, GC,
         CA, CV, SG, DD, DP, GS) %>% 
  group_by( atleta_id, apelido ) %>%
  summarise_at( vars( -contains('apelido'), -contains('atleta_id') ),
                funs(sum) ) %>% 
  mutate( rodada_id = 22 ) %>% 
  ungroup

scouts <- resumo %>% 
  bind_rows(., dados_faltantes_att %>% 
              select(atleta_id, apelido, rodada_id, FS, PE, A, FT, FD, FF, G, I, PP, RB, FC, GC,
                     CA, CV, SG, DD, DP, GS) %>% 
              replace(., is.na(.), 0 ) ) %>% 
  arrange( atleta_id, rodada_id ) %>%
  group_by( atleta_id ) %>%
  mutate_at( vars( FC, PE, RB, SG, CA, FD, FS, FF, I, G, DD, GS, A, CV, FT, GC, DP, PP ),
             funs( ajuste_lag(.) ) ) %>% 
  ungroup %>% 
  filter( rodada_id != 22 )


dados_faltantes_aj <- dados_faltantes_att %>% 
  arrange( atleta_id, rodada_id ) %>% 
  select( -FS, -PE, -A, -FT, -FD, -FF, -G, -I, -PP, -RB, -FC, -GC,
          -CA, -CV, -SG, -DD, -DP, -GS ) %>% 
  bind_cols(., scouts %>% 
              select( -atleta_id, -apelido, -rodada_id ) )


dados_cartola <- bind_rows( dados_1, dados_2, dados_faltantes_aj )


dados_cartola %>% 
  filter( atleta_id == 68920) %>%
  tail

rodadas <- dados_cartola %>% 
  distinct(rodada_id)

anos <- dados_cartola %>% 
  distinct(ano)


# a = 2014
# rod = 1
ehnois <- foreach( a = anos$ano ) %:%
  foreach( rod = rodadas$rodada_id ) %dopar% {
    
    dados_cartola %>% 
      filter( ano == a,
              rodada_id == rod ) %>% 
      write_csv(., path = paste0( 'historico/', a, '/scouts/', a, '_scouts_', rod, '.csv' ) )
    
    
  } 

paste0( 'historico/', 2014, '/scouts/', 2014, '_scouts_', 1, '.csv' )





  
  
  