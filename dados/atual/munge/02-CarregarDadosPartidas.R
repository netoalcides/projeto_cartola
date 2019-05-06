info( logger, "CARTOLA_DADOS::obtem dados das partidas atuais" )

dados_partidas <- fromJSON( 
  paste( 
    readLines( "https://api.cartolafc.globo.com/partidas" ), 
    collapse = "" ) 
)


info( logger, "CARTOLA_DADOS::obtem dados das partidas anteriores" )

rodada_anterior <- dados_partidas$rodada - 1

dados_partidas_anterior <- fromJSON( 
  paste( 
    readLines( paste0( "https://api.cartolafc.globo.com/partidas/", rodada_anterior ) ), 
    collapse = "" ) 
)

rm(rodada_anterior)

