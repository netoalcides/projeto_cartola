info( logger, "CARTOLA_DADOS::iniciado" )

info( logger, "CARTOLA_DADOS::obtem dados do mercado" )

dados_mercado <- fromJSON(
  paste(
    readLines( "https://api.cartolafc.globo.com/atletas/mercado" ),
    collapse = "" )
)
