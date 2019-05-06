library(tidyverse)
library(doFuture)

registerDoFuture()
plan(multiprocess)


rodadas <- 1:38
anos <- list.files('historico/')

partidas <- foreach( ano = anos, .combine = rbind ) %:% 
  foreach( rod = rodadas, .combine = rbind ) %dopar% {
  
  read_csv( paste0( 'historico/', ano, '/', 'partidas/', ano, '_partidas_', rod, '.csv' ) )
  
} 


classificacao <- foreach( ano = anos, .combine = rbind ) %:% 
  foreach( rod = rodadas, .combine = rbind ) %dopar% {
    
    read_csv( paste0( 'historico/', ano, '/', 'classificacao/', ano, '_classificacao_', rod, '.csv' ) )
    
} 

# dados_jogadores <- foreach( ano = anos, .combine = rbind ) %:% 
#   foreach( rod = rodadas, .combine = rbind ) %dopar% {
#     
#     partidas <- read_csv( paste0( 'historico/', ano, '/', 'partidas/', ano, '_partidas_', rod, '.csv' ) )
#     
#     classificacao <- read_csv( paste0( 'historico/', ano, '/', 'classificacao/', ano, '_classificacao_', rod, '.csv' ) )
#     
#     jogadores <- read_csv( paste0( 'historico/', ano, '/', 'scouts/', ano, '_scouts_', rod, '.csv' ) )
# 
# } 


ano = 2014
rod = 1

partidas <- read_csv( paste0( 'historico/', ano, '/', 'partidas/', ano, '_partidas_', rod, '.csv' ) )

classificacao <- read_csv( paste0( 'historico/', ano, '/', 'classificacao/', ano, '_classificacao_', rod, '.csv' ) )

jogadores <- read_csv( paste0( 'historico/', ano, '/', 'scouts/', ano, '_scouts_', rod, '.csv' ) )

jogadores %>% 
  left_join(., classificacao %>% 
              select( clube_id, clube_rank ),
            by = 'clube_id' )






partidas
