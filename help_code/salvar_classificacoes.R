library(tidyverse)
library(doFuture)
library(magrittr)

registerDoFuture()
plan(multiprocess)


load('../old/classificacao.RData')

classificacao %>% 
  write_csv(., path = 'xx.csv')

classificacao %>% 
  distinct(id, nome) %>% 
  arrange(nome) %>% 
  print(n=Inf)

classificacao <- read_csv2( file = '../old/classificacao_brasileirao.csv',
                       locale = locale( encoding = 'latin1') )

classificacao %<>% 
  rename( clube_id = id,
          clube = nome )

rodadas <- classificacao %>% 
  distinct(rodada_id)

anos <- classificacao %>% 
  distinct(ano)


#a = 2014
#rod = 1
yeah <- foreach( a = anos$ano ) %:%
  foreach( rod = rodadas$rodada_id ) %dopar% {
    
    classificacao %>% 
      filter( ano == a,
              rodada_id == rod ) %>% 
      write_csv(., path = paste0( 'historico/', a, '/classificacao/', a, '_classificacao_', rod, '.csv' ) )
    
    
  } 

paste0( 'historico/', 2014, '/classificacao/', 2014, '_classificacao_', 1, '.csv' )
