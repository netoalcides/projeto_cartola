library(tidyverse)
library(doFuture)

registerDoFuture()
plan(multiprocess)



load('../old/partidas_cartola_2018_por_rodada.RData')

load('../old/dados_cartola_2018_por_rodada.RData')


dados_cartola_2018_por_rodada %>% 
  head()

partidas <- read_csv2( file = '../old/placar_brasileirao_02_01.csv',
                       locale = locale( encoding = 'latin1') )


partidas2 <- read_csv2( file = '../old/placar_brasileirao_02_01_ajustado.csv',
                       locale = locale( encoding = 'latin1') )


partidas3 <- read_csv2( file = '../old/placar_2018.csv',
                        locale = locale( encoding = 'latin1') )

# partidas %>% 
#   write_csv(., 'xx.csv')


# partidas_cartola_2018_por_rodada %<>%
#   select(rodada_id,
#          partidas.clube_casa_id,
#          partidas.clube_visitante_id,
#          partidas.placar_oficial_mandante,
#          partidas.placar_oficial_visitante,
#          partidas.partida_data, 
#          partidas.local) %>%
#   rename(clube_casa_id = partidas.clube_casa_id,
#          clube_visitante_id = partidas.clube_visitante_id,
#          placar_oficial_mandante = partidas.placar_oficial_mandante,
#          placar_oficial_visitante = partidas.placar_oficial_visitante ) %>% 
#   mutate( ano = 2018,
#           rodada_id = as.integer(rodada_id) )


times <- dados_cartola_2018_por_rodada %>% 
  distinct( clube_id, clube )

# partidas_cartola_2018_por_rodada %<>%
#   left_join(., times,
#             by = c( 'clube_casa_id' = 'clube_id') ) %>%
#   rename( clube_casa = clube ) %>%
#   left_join(., times,
#             by = c( 'clube_visitante_id' = 'clube_id') ) %>%
#   rename( clube_visitante = clube )

partidas_2018_ajustada <- partidas3 %>% 
  filter( is.na(clube_casa_id) == TRUE ) %>% 
  left_join(., times,
            by = c( 'clube_casa' = 'clube' ) ) %>% 
  select( -clube_casa_id ) %>% 
  rename( clube_casa_id = clube_id ) %>% 
  left_join(., times,
            by = c( 'clube_visitante' = 'clube' ) ) %>% 
  select( -clube_visitante_id ) %>% 
  rename( clube_visitante_id = clube_id )

partidas_2018_ajustada %>% 
  distinct( rodada_id ) %>% 
  print(n=Inf)


partidas_2018_ajustada_completa <- partidas3 %>% 
  filter( is.na(clube_casa_id) == FALSE ) %>% 
  bind_rows(., partidas_2018_ajustada ) %>% 
  arrange( rodada_id )
  




partidas2 <- partidas2 %>% 
  bind_rows(., partidas_2018_ajustada_completa )



partidas2 %>% 
  filter( ano == 2018 ) %>% 
  arrange( rodada_id ) %>% 
  write_csv(., path = 'xx.csv')



rodadas <- partidas2 %>% 
  distinct(rodada_id)

anos <- partidas2 %>% 
  distinct(ano)


a = 2014
rod = 1
partidas <- foreach( a = anos$ano ) %:%
  foreach( rod = rodadas$rodada_id ) %dopar% {
    
    partidas2 %>% 
      filter( ano == a,
              rodada_id == rod ) %>% 
      write_csv(., path = paste0( 'historico/', a, '/partidas/', a, '_partidas_', rod, '.csv' ) )
    
    
  } 

paste0( 'historico/', 2014, '/partidas/', 2014, '_partidas_', 1, '.csv' )
