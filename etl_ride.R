# ==============================================================================
# Dengue RIDE | etl_ride.R
# Extrai do PostgreSQL ("Info_Dengue".dengue, base do InfoDengue) a serie dos
# 34 municipios da RIDE-DF e grava data/dengue_ride_df.parquet, a unica fonte
# de dados lida pelo app.
#
# Uso (working directory = raiz do projeto):  Rscript etl_ride.R
# ==============================================================================

library(DBI)
library(RPostgres)
library(arrow)
library(dplyr)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("PG_HOST"),
  port     = as.integer(Sys.getenv("PG_PORT", "5432")),
  dbname   = Sys.getenv("PG_DB"),
  user     = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASS")  # credenciais no .Renviron (ver .Renviron.example)
)

# Códigos IBGE oficiais da RIDE-DF (LC 163/2018) - 34 municípios
ride_codes_oficiais <- c(
  # Distrito Federal (1)
  5300108, # Brasília
  
  # Minas Gerais (4)
  3104502, # Arinos
  3109303, # Buritis
  3109451, # Cabeceira Grande
  3170404, # Unaí
  
  # Goiás (29)
  5200100, # Abadiânia
  5200175, # Água Fria de Goiás
  5200258, # Águas Lindas de Goiás
  5200308, # Alexânia
  5200605, # Alto Paraíso de Goiás
  5200803, # Alvorada do Norte
  5203203, # Barro Alto
  5204003, # Cabeceiras (CORRIGIDO)
  5205307, # Cavalcante
  5205497, # Cidade Ocidental
  5205513, # Cocalzinho de Goiás
  5205802, # Corumbá de Goiás
  5206206, # Cristalina
  5207907, # Flores de Goiás (CORRIGIDO)
  5208004, # Formosa
  5208608, # Goianésia
  5212501, # Luziânia
  5213053, # Mimoso de Goiás (CORRIGIDO)
  5214606, # Niquelândia
  5215231, # Novo Gama
  5215603, # Padre Bernardo
  5217302, # Pirenópolis
  5217609, # Planaltina (GO)
  5219753, # Santo Antônio do Descoberto (CORRIGIDO)
  5220009, # São João d'Aliança (CORRIGIDO)
  5220686, # Simolândia (CORRIGIDO)
  5221858, # Valparaíso de Goiás
  5222203, # Vila Boa
  5222302  # Vila Propício
)

query_ride <- sprintf(
  "SELECT * FROM \"Info_Dengue\".dengue WHERE municipio_geocodigo IN (%s);",
  paste(ride_codes_oficiais, collapse = ", ")
)

dados_ride_brutos <- dbGetQuery(con, query_ride)

dir.create("data", showWarnings = FALSE, recursive = TRUE)
write_parquet(dados_ride_brutos, "data/dengue_ride_df.parquet")

dbDisconnect(con)
print("Parquet da RIDE-DF gerado com sucesso com os 34 municípios!")