dados_atuais <- informacoes_jogadores %>% 
  select( atleta_id, apelido, clube, status_id, status, posicao, jogos_num, preco_num, pontos_num, media_num, variacao_num ) %>% 
  filter( status_id == 7 )


jogadores <- read_csv("../historico/2018/scouts/2018_scouts_38.csv")

dados_ultimo_ano <- jogadores %>% 
  select( atleta_id, media_num, preco_num,  variacao_num ) %>% 
  rename( preco_num_last = preco_num )


dados_atuais %>% 
  left_join(., dados_ultimo_ano,
            by = "atleta_id" )

partidas %>% 
  select( clube_casa, clube_visitante )


m2 <- summary( lm( variacao_num ~ pontos_num + preco_num, 
                   data = dados_atuais %>% 
                     filter( jogos_num == 2 ) ) )


# atacante

dados_atuais %>% 
  left_join(., dados_ultimo_ano,
            by = "atleta_id" ) %>% 
  filter( posicao == 'ata' ) %>% 
  arrange( preco_num ) %>% 
  print(n=Inf)


# meio

dados_atuais %>% 
  left_join(., dados_ultimo_ano,
            by = "atleta_id" ) %>% 
  filter( posicao == 'mei' ) %>% 
  arrange( preco_num ) %>% 
  print(n=Inf)

# zag

dados_atuais %>% 
  left_join(., dados_ultimo_ano,
            by = "atleta_id" ) %>% 
  filter( posicao == 'zag' ) %>% 
  arrange( preco_num ) %>% 
  print(n=Inf)

# lat

dados_atuais %>% 
  left_join(., dados_ultimo_ano,
            by = "atleta_id" ) %>% 
  filter( posicao == 'lat' ) %>% 
  arrange( preco_num ) %>% 
  print(n=Inf)

# gol

dados_atuais %>% 
  left_join(., dados_ultimo_ano,
            by = "atleta_id" ) %>% 
  filter( posicao == 'gol' ) %>% 
  arrange( preco_num ) %>% 
  print(n=Inf)


####################################################################################################

# atacante

dados_atuais %>% 
  filter( posicao == 'ata' ) %>% 
  arrange( preco_num ) %>% 
  print(n=Inf)


# meio

dados_atuais %>% 
  filter( posicao == 'mei' ) %>% 
  arrange( preco_num ) %>% 
  print(n=Inf)

# zag

dados_atuais %>% 
  filter( posicao == 'zag' ) %>% 
  arrange( preco_num ) %>% 
  print(n=Inf)

# lat

dados_atuais %>% 
  filter( posicao == 'lat' ) %>% 
  arrange( preco_num ) %>% 
  print(n=Inf)

# gol

dados_atuais %>% 
  filter( posicao == 'gol' ) %>% 
  arrange( preco_num ) %>% 
  print(n=Inf)

# tec

dados_atuais %>% 
  filter( posicao == 'tec' ) %>% 
  arrange( preco_num ) %>% 
  print(n=Inf)
