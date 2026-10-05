# ==============================================================================
# Dengue RIDE | etl_ride_api.R
# Alternativa ao etl_ride.R para quem NAO tem acesso ao banco do projeto:
# baixa a serie semanal dos 34 municipios da RIDE-DF direto da API publica do
# InfoDengue (https://info.dengue.mat.br/services/api) e grava
# data/dengue_ride_df.parquet no MESMO formato que o etl_ride.R gera a partir
# do banco - o app nao percebe a diferenca.
#
# Nao precisa de credenciais nem de .Renviron, so de internet.
#
# Uso (working directory = raiz do projeto):
#   Rscript etl_ride_api.R              # 2015 ate o ano corrente
#   Rscript etl_ride_api.R 2020         # a partir de 2020
#   Rscript etl_ride_api.R 2020 2024    # de 2020 a 2024
#
# Variavel de ambiente opcional: DENGUE_PARQUET (caminho de saida; o padrao e
# o mesmo lido pelo app, data/dengue_ride_df.parquet).
# ==============================================================================

library(arrow)

args <- commandArgs(trailingOnly = TRUE)
ANO_INICIO <- if (length(args) >= 1) as.integer(args[1]) else 2015L
ANO_FIM    <- if (length(args) >= 2) as.integer(args[2]) else as.integer(format(Sys.Date(), "%Y"))
SAIDA      <- Sys.getenv("DENGUE_PARQUET", file.path("data", "dengue_ride_df.parquet"))

API_URL <- "https://info.dengue.mat.br/api/alertcity"

# Codigos IBGE oficiais da RIDE-DF (LC 163/2018) - 34 municipios. Mesma lista
# do etl_ride.R; o nome fica aqui para nao depender da grafia devolvida pela API.
RIDE_MUNICIPIOS <- c(
  # Distrito Federal (1)
  "5300108" = "Brasília",
  # Minas Gerais (4)
  "3104502" = "Arinos",
  "3109303" = "Buritis",
  "3109451" = "Cabeceira Grande",
  "3170404" = "Unaí",
  # Goiás (29)
  "5200100" = "Abadiânia",
  "5200175" = "Água Fria de Goiás",
  "5200258" = "Águas Lindas de Goiás",
  "5200308" = "Alexânia",
  "5200605" = "Alto Paraíso de Goiás",
  "5200803" = "Alvorada do Norte",
  "5203203" = "Barro Alto",
  "5204003" = "Cabeceiras",
  "5205307" = "Cavalcante",
  "5205497" = "Cidade Ocidental",
  "5205513" = "Cocalzinho de Goiás",
  "5205802" = "Corumbá de Goiás",
  "5206206" = "Cristalina",
  "5207907" = "Flores de Goiás",
  "5208004" = "Formosa",
  "5208608" = "Goianésia",
  "5212501" = "Luziânia",
  "5213053" = "Mimoso de Goiás",
  "5214606" = "Niquelândia",
  "5215231" = "Novo Gama",
  "5215603" = "Padre Bernardo",
  "5217302" = "Pirenópolis",
  "5217609" = "Planaltina",
  "5219753" = "Santo Antônio do Descoberto",
  "5220009" = "São João d'Aliança",
  "5220686" = "Simolândia",
  "5221858" = "Valparaíso de Goiás",
  "5222203" = "Vila Boa",
  "5222302" = "Vila Propício"
)

# Nomes da API (CSV) -> nomes da tabela "Info_Dengue".dengue do banco, que e o
# que o app espera. As demais colunas so passam para minusculas.
RENOMEAR <- c(
  data_iniSE    = "data_ini_se",
  SE            = "se",
  Rt            = "rt",
  Localidade_id = "localidade_id",
  id            = "id_origem",
  municipio_nome = "municipio_nome_api"
)

#' Baixa a serie de um municipio (CSV da API), com ate 3 tentativas.
baixar_municipio <- function(geocodigo) {
  url <- sprintf(
    "%s?geocode=%s&disease=dengue&format=csv&ew_start=1&ew_end=53&ey_start=%d&ey_end=%d",
    API_URL, geocodigo, ANO_INICIO, ANO_FIM
  )
  for (tentativa in 1:3) {
    d <- tryCatch(
      # id como texto: tem 18 digitos e perderia precisao como numero.
      utils::read.csv(url, stringsAsFactors = FALSE, encoding = "UTF-8",
                      colClasses = c(id = "character")),
      error = function(e) e
    )
    if (!inherits(d, "error")) return(d)
    message(sprintf("  tentativa %d falhou (%s)", tentativa, conditionMessage(d)))
    Sys.sleep(2 * tentativa)
  }
  stop("Nao foi possivel baixar o municipio ", geocodigo, " da API do InfoDengue.", call. = FALSE)
}

message(sprintf("[etl_ride_api] InfoDengue: %d municipios, %d a %d.",
                length(RIDE_MUNICIPIOS), ANO_INICIO, ANO_FIM))

partes <- lapply(names(RIDE_MUNICIPIOS), function(geocodigo) {
  message(sprintf("- %s %s", geocodigo, RIDE_MUNICIPIOS[[geocodigo]]))
  d <- baixar_municipio(geocodigo)
  if (!nrow(d)) {
    warning("Sem dados na API para ", geocodigo, call. = FALSE)
    return(NULL)
  }

  idx <- names(d) %in% names(RENOMEAR)
  names(d)[idx] <- RENOMEAR[names(d)[idx]]
  names(d) <- tolower(names(d))

  d$municipio_geocodigo <- as.integer(geocodigo)
  d$municipio_nome      <- RIDE_MUNICIPIOS[[geocodigo]]
  d$uf_ibge             <- as.integer(geocodigo) %/% 100000L
  d$uf                  <- as.character(d$uf_ibge)   # o app espera o codigo em texto ("53")
  d
})

partes <- Filter(Negate(is.null), partes)
if (!length(partes)) stop("A API nao devolveu dados para nenhum municipio.", call. = FALSE)

# Une as partes mesmo que algum municipio venha com colunas a mais ou a menos.
colunas <- unique(unlist(lapply(partes, names)))
dados <- do.call(rbind, lapply(partes, function(d) {
  d[setdiff(colunas, names(d))] <- NA
  d[colunas]
}))

# Tipos iguais aos do banco (o app converte, mas assim o Parquet fica igual).
dados$data_ini_se <- as.Date(dados$data_ini_se)
for (col in intersect(c("se", "casos", "casos_est_min", "casos_est_max", "casprov",
                        "casprov_est_min", "casprov_est_max", "casconf",
                        "notif_accum_year", "nivel", "nivel_inc", "receptivo",
                        "transmissao", "localidade_id"), names(dados))) {
  dados[[col]] <- as.integer(dados[[col]])
}
for (col in intersect(c("casos_est", "casprov_est", "p_rt1", "rt", "p_inc100k", "pop",
                        "tempmin", "tempmed", "tempmax", "umidmin", "umidmed",
                        "umidmax"), names(dados))) {
  dados[[col]] <- as.numeric(dados[[col]])
}

dados <- dados[order(dados$municipio_geocodigo, dados$data_ini_se), , drop = FALSE]
rownames(dados) <- NULL

# Grava num temporario e troca de uma vez: o app nunca le um arquivo pela metade.
dir.create(dirname(SAIDA), showWarnings = FALSE, recursive = TRUE)
tmp <- paste0(SAIDA, ".tmp")
tabela <- arrow::Table$create(dados)
# int64 como no banco (o R so tem inteiro de 32 bits).
tabela$municipio_geocodigo <- tabela$municipio_geocodigo$cast(arrow::int64())
if ("id_origem" %in% names(tabela)) tabela$id_origem <- tabela$id_origem$cast(arrow::int64())
arrow::write_parquet(tabela, tmp)
if (file.exists(SAIDA)) invisible(file.remove(SAIDA))
if (!file.rename(tmp, SAIDA)) stop("Nao foi possivel gravar ", SAIDA, call. = FALSE)

n_mun <- length(unique(dados$municipio_geocodigo))
message(sprintf(
  "[etl_ride_api] %s gravado: %d linhas, %d municipio(s), SE %d a %d.",
  SAIDA, nrow(dados), n_mun, min(dados$se, na.rm = TRUE), max(dados$se, na.rm = TRUE)
))
if (n_mun < length(RIDE_MUNICIPIOS)) {
  warning(sprintf("Apenas %d de %d municipios vieram da API.", n_mun, length(RIDE_MUNICIPIOS)),
          call. = FALSE)
}
