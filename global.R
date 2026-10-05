# ==============================================================================
# Dengue RIDE | global.R
# Ambiente, fonte de dados (Parquet via DuckDB), dicionario geografico e
# helpers de consulta. Executado UMA vez, antes de ui.R e server.R.
# ==============================================================================

library(shiny)
library(bslib)
library(DBI)
library(duckdb)
library(dplyr)
library(echarts4r)
library(DT)  # tabela da aba Analises (R/mod_epi_scanner_server.R)


# ------------------------------------------------------------ fonte de dados --
# Parquet local lido pelo DuckDB.
#
# Por que DuckDB e nao o backend dplyr do arrow:
#   O arrow traduz um subconjunto de dplyr. As consultas deste painel usam
#   n_distinct(), if_else() dentro de sum(), max(na.rm=) e um summarise()
#   aninhado em dois estagios (mod_mapa). O arrow nao cobre tudo isso e cairia
#   para collect() antecipado - justamente o que a lazy evaluation deveria
#   evitar. O DuckDB fala SQL completo via dbplyr, entao as consultas dos
#   modulos seguem identicas as da versao PostgreSQL, e o pushdown de filtros
#   e de colunas acontece na leitura do Parquet.
PARQUET_DENGUE <- Sys.getenv(
  "DENGUE_PARQUET",
  file.path("data", "dengue_ride_df.parquet")
)

if (!file.exists(PARQUET_DENGUE)) {
  stop(
    "Arquivo Parquet nao encontrado em '", PARQUET_DENGUE, "'. ",
    "Verifique o caminho ou defina DENGUE_PARQUET em ~/.Renviron.",
    call. = FALSE
  )
}

# Uma unica conexao, sem pool. O pool existia para nao esgotar conexoes de um
# servidor Postgres remoto; um DuckDB em memoria no mesmo processo nao tem esse
# custo, e o Shiny processa as consultas de forma serial de qualquer modo.
# bigint = "numeric": municipio_geocodigo tem 7 digitos e cabe em double sem
# perda; evita a dependencia de integer64, que quebra selectInput.
#
# CONEXAO AUTO-RECUPERAVEL. Num servidor (RStudio Server) a MESMA sessao do R
# roda o app varias vezes. Ao parar o app o onStop() abaixo faz
# dbDisconnect(shutdown = TRUE), que DESTROI o banco em memoria. Se algo
# ainda segurar o `con` antigo na re-execucao, toda consulta falha com
# "Invalid connection". Por isso a conexao e (re)criada por uma funcao e
# tbl_dengue() reconecta sozinho quando o `con` esta invalido.
con <- NULL

.dengue_conectar <- function() {
  con <<- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir  = ":memory:",
    bigint = "numeric"
  )

  # Deixa o DuckDB usar os nucleos disponiveis. Falha silenciosamente em
  # versoes que nao aceitam o PRAGMA - nao vale derrubar o app por isso.
  try(DBI::dbExecute(con, "SET threads TO 4"), silent = TRUE)

  # View sobre o Parquet: o arquivo NAO e carregado na RAM aqui. Cada consulta
  # le so as colunas e as linhas que o filtro exige.
  caminho <- normalizePath(PARQUET_DENGUE, winslash = "/", mustWork = TRUE)
  caminho <- gsub("'", "''", caminho, fixed = TRUE)   # escapa aspas no path
  DBI::dbExecute(con, sprintf(
    "CREATE OR REPLACE VIEW dengue AS SELECT * FROM read_parquet('%s')",
    caminho
  ))

  invisible(con)
}

.dengue_conectar()

onStop(function() {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    message("[Dengue RIDE] Encerrando DuckDB.")
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
  }
})

#' Referencia lazy para a base de dengue.
#'
#' Sem filtro de escopo: o Parquet ja chega com o recorte correto (RIDE-DF,
#' LC 163/2018). Qualquer trava aqui seria uma segunda fonte de verdade a
#' manter em sincronia com o arquivo - e a primeira a ficar desatualizada.
#'
#' Reconecta sozinho se o `con` tiver caido (ver nota sobre RStudio Server
#' acima) - assim o app roda N vezes na mesma sessao do R sem reiniciar.
tbl_dengue <- function() {
  if (is.null(con) || !DBI::dbIsValid(con)) .dengue_conectar()
  dplyr::tbl(con, "dengue")
}

message(sprintf(
  "[Dengue RIDE] Fonte: %s (%.1f MB).",
  PARQUET_DENGUE, file.size(PARQUET_DENGUE) / 1024^2
))

# --------------------------------------------------------- escopo da base ----
# A base cobre a RIDE-DF, nao o Brasil. O rotulo do nivel agregado reflete
# isso; a chave interna continua "brasil" para nao alterar aplicar_filtro_geo()
# nem o contrato de saida do modulo geografico.
ROTULO_BASE <- Sys.getenv("DENGUE_ROTULO_BASE", "RIDE-DF")

# ------------------------------------------------------- dominio das 27 UFs --
# A coluna `uf` guarda o codigo IBGE em texto ("53"), nao a sigla.
# Esta tabela traduz codigo -> sigla/nome. Continua com as 27 UFs de proposito:
# e uma tabela de dominio, e quem restringe o escopo e a BASE, nao a lookup.
UF_LOOKUP <- data.frame(
  uf_ibge = c(11L, 12L, 13L, 14L, 15L, 16L, 17L,
              21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L,
              31L, 32L, 33L, 35L,
              41L, 42L, 43L,
              50L, 51L, 52L, 53L),
  uf_sigla = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
               "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA",
               "MG", "ES", "RJ", "SP",
               "PR", "SC", "RS",
               "MS", "MT", "GO", "DF"),
  uf_nome = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins",
              "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba",
              "Pernambuco", "Alagoas", "Sergipe", "Bahia",
              "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo",
              "Paraná", "Santa Catarina", "Rio Grande do Sul",
              "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal"),
  regiao = c(rep("Norte", 7), rep("Nordeste", 9), rep("Sudeste", 4),
             rep("Sul", 3), rep("Centro-Oeste", 4)),
  stringsAsFactors = FALSE
)

# ------------------------------------------------- dicionario geografico -----
# Um DISTINCT sobre o Parquet. E AQUI que o escopo do painel se define: os
# seletores de Estado e Municipio sao construidos a partir deste resultado,
# entao so aparece o que existe na base. Nao ha lista de municipios codificada
# em lugar nenhum do app - trocar o Parquet troca os seletores.
message("[Dengue RIDE] Carregando dicionário geográfico...")
.t0 <- Sys.time()

GEO_DICT <- tbl_dengue() |>
  dplyr::distinct(uf, uf_ibge, municipio_geocodigo, municipio_nome) |>
  dplyr::collect() |>
  dplyr::mutate(
    uf_ibge             = as.integer(uf_ibge),
    municipio_geocodigo = as.integer(municipio_geocodigo),
    # Fallback: se uf_ibge vier NA, deriva da raiz do geocodigo.
    uf_ibge             = dplyr::coalesce(uf_ibge, municipio_geocodigo %/% 100000L)
  ) |>
  dplyr::left_join(UF_LOOKUP, by = "uf_ibge") |>
  dplyr::mutate(
    # Se a coluna `uf` trouxer sigla de verdade, ela tem prioridade.
    uf_sigla = dplyr::if_else(grepl("^[A-Za-z]{2}$", uf), toupper(uf), uf_sigla),
    uf_nome  = dplyr::coalesce(uf_nome, uf_sigla, as.character(uf_ibge))
  ) |>
  dplyr::filter(!is.na(municipio_geocodigo)) |>
  dplyr::arrange(uf_nome, municipio_nome)

message(sprintf(
  "[Dengue RIDE] Dicionário pronto: %d municípios / %d UF(s) em %.1fs.",
  nrow(GEO_DICT), dplyr::n_distinct(GEO_DICT$uf_ibge),
  as.numeric(difftime(Sys.time(), .t0, units = "secs"))
))
rm(.t0)

# Tabela de UFs derivada da BASE - so entra UF que tem municipio no Parquet.
UF_DISPONIVEIS <- GEO_DICT |>
  dplyr::distinct(uf_ibge, uf_sigla, uf_nome, regiao) |>
  dplyr::arrange(uf_nome)

# UF padrao dos seletores: DF se estiver na base, senao a primeira disponivel.
UF_PADRAO <- if (53L %in% UF_DISPONIVEIS$uf_ibge) {
  53L
} else {
  UF_DISPONIVEIS$uf_ibge[1]
}

message(sprintf(
  "[Dengue RIDE] Escopo: %s — %d município(s) em %s.",
  ROTULO_BASE, nrow(GEO_DICT), paste(UF_DISPONIVEIS$uf_sigla, collapse = ", ")
))

# ------------------------------------------------- cobertura temporal --------
PERIODO_DADOS <- tbl_dengue() |>
  dplyr::summarise(
    inicio = min(data_ini_se, na.rm = TRUE),
    fim    = max(data_ini_se, na.rm = TRUE)
  ) |>
  dplyr::collect()

ANO_MIN <- as.integer(format(as.Date(PERIODO_DADOS$inicio), "%Y"))
ANO_MAX <- as.integer(format(as.Date(PERIODO_DADOS$fim),    "%Y"))

message(sprintf("[Dengue RIDE] Cobertura temporal: %d a %d.", ANO_MIN, ANO_MAX))

# ------------------------------------------------- ano sanitario (jul-jun) ---
# O canal endemico e a curva epidemica usam o ANO SANITARIO, nao o calendario:
# a temporada comeca na semana epidemiologica 27 (fim de junho / inicio de
# julho) e termina na semana 26 do ano seguinte. Ancorar no inicio da baixa
# transmissao (inverno), em vez de 1o de janeiro, evita partir ao meio a
# temporada de pico (verao) - que e exatamente o periodo que o canal precisa
# comparar inteiro contra o historico.
SEMANA_CORTE_SANITARIA <- 27L

#' Ano sanitario (temporada) a que uma semana epidemiologica pertence.
#' Semanas >= 27 ficam na temporada = ano calendario; semanas < 27 pertencem
#' a temporada do ano anterior (ex.: semana 10 de 2025 -> temporada 2024).
ano_sanitario <- function(ano, semana) {
  ano - as.integer(semana < SEMANA_CORTE_SANITARIA)
}

#' Rotulo "2024/2025" de uma temporada (ano sanitario).
rotulo_ano_sanitario <- function(ano_san) {
  sprintf("%d/%d", ano_san, ano_san + 1L)
}

# Ordem de exibicao das semanas no eixo dos graficos por temporada: comeca em
# 27 e fecha em 1..26 - sempre 52 semanas. A semana 53 (existe so em alguns
# anos) NAO vira categoria propria: e dobrada metade em 52, metade em 1 antes
# de chegar aqui (ver dobrar_semana_53() abaixo), porque raramente tem anos
# de referencia suficientes pra formar uma estatistica propria (quartil,
# mediana) e apareceria como um buraco isolado no meio do grafico.
NIVEIS_SEMANA_SANITARIA <- c(
  seq(SEMANA_CORTE_SANITARIA, 52L),
  seq(1L, SEMANA_CORTE_SANITARIA - 1L)
)

#' Dobra a semana epidemiologica 53 nas semanas vizinhas da MESMA temporada.
#'
#' A semana 53 so existe em anos "longos" do calendario ISO, entao raramente
#' tem anos de referencia suficientes pra formar uma estatistica propria.
#' Em vez de virar categoria propria no eixo, ela e DOBRADA: metade do valor
#' de cada coluna em `colunas` vai pra semana 52, a outra metade pra semana 1
#' - ambas da MESMA temporada (ano_san), ja que a semana 1 pertence ao ano
#' sanitario que comecou na semana 27 anterior. O resultado e que toda
#' temporada, tenha ela 52 ou 53 semanas de calendario, ocupa exatamente 52
#' posicoes no eixo.
#'
#' @param d `data.frame` com colunas `semana`, `ano_san` e as colunas listadas
#'   em `colunas`.
#' @param colunas nomes das colunas numericas a dobrar (ex.: c("inc",
#'   "inc_nowcast"), ou c("casos", "casos_est")).
dobrar_semana_53 <- function(d, colunas) {
  com_53 <- d[d$semana == 53L, , drop = FALSE]
  d <- d[d$semana != 53L, , drop = FALSE]
  if (!nrow(com_53)) return(d)
  
  for (i in seq_len(nrow(com_53))) {
    linha <- com_53[i, ]
    idx52 <- which(d$ano_san == linha$ano_san & d$semana == 52L)
    idx1  <- which(d$ano_san == linha$ano_san & d$semana == 1L)
    
    for (col in colunas) {
      metade <- linha[[col]] / 2
      if (is.na(metade)) next
      if (length(idx52)) d[[col]][idx52] <- sum(d[[col]][idx52], metade, na.rm = TRUE)
      if (length(idx1))  d[[col]][idx1]  <- sum(d[[col]][idx1],  metade, na.rm = TRUE)
    }
  }
  d
}

# --------------------------------------------------------- malhas (GeoJSON) --
GEO_DIR <- Sys.getenv("DENGUE_GEO_DIR", file.path("data", "geo"))

# Propriedade do GeoJSON usada como chave de juncao com os dados.
# "codarea" e o geocodigo IBGE, padrao da API de malhas do IBGE.
GEO_NAME_PROPERTY <- Sys.getenv("DENGUE_GEO_NAME_PROP", "codarea")

# Cache de processo: o parse do GeoJSON acontece uma vez por chave.
.cache_geojson <- new.env(parent = emptyenv())

#' Carrega (e memoriza) uma malha GeoJSON pronta para o echarts4r.
carregar_geojson <- function(chave) {
  if (!is.null(.cache_geojson[[chave]])) {
    return(.cache_geojson[[chave]])
  }
  caminho <- file.path(GEO_DIR, paste0(chave, ".geojson"))
  if (!file.exists(caminho)) return(NULL)
  
  js <- jsonlite::fromJSON(caminho, simplifyVector = FALSE)
  assign(chave, js, envir = .cache_geojson)
  js
}

#' Recorta as malhas municipais do IBGE ao escopo da base.
#'
#' Roda uma vez no start e PRE-POPULA o cache. A partir dai:
#'   carregar_geojson("BASE_MUN")  -> todos os municipios da base
#'   carregar_geojson("MUN_52")    -> so os de Goias QUE ESTAO na base
#'
#' O recorte segue GEO_DICT, entao acompanha o Parquet automaticamente: nao ha
#' lista de geocodigos a atualizar aqui quando a base muda.
#'
#' CACHE EM DISCO (.rds): parsear o GeoJSON completo de UFs grandes (MG tem
#' 853 municipios, GO tem 246) so pra filtrar ~30 e pra fora custa ~1-1,5s a
#' cada vez que o processo R sobe - sempre o MESMO resultado, ja que o
#' recorte so muda se o Parquet ou os arquivos-fonte mudarem. Um .rds do
#' resultado ja filtrado carrega em uma fracao do tempo (medido: ~30x mais
#' rapido). O cache e validado por uma "impressao digital" (municipios da
#' base + data de modificacao dos GeoJSON fonte); se qualquer um mudar, o
#' cache e reconstruido automaticamente - nao exige passo manual.
MALHAS_CACHE_RDS <- file.path(GEO_DIR, ".malhas_base_cache.rds")

#' Fingerprint do recorte de malhas - SO por CONTEUDO (municipios da base +
#' tamanho em bytes dos GeoJSON fonte), de proposito sem `mtime`: um mtime
#' mais recente sem o conteudo ter mudado e comum (sync de nuvem, `git
#' checkout`, copia de arquivo tocam o mtime sem alterar um byte) e nao deve
#' forcar o reparse de todas as malhas - mesma logica do fingerprint do
#' Parquet em forecasting/utils_forecast.R::fc_fingerprint_ok().
.malhas_fingerprint <- function() {
  ufs      <- sort(unique(GEO_DICT$uf_ibge))
  arquivos <- file.path(GEO_DIR, sprintf("MUN_%d.geojson", ufs))
  list(
    geocodigos = sort(GEO_DICT$municipio_geocodigo),
    bytes      = as.numeric(suppressWarnings(file.info(arquivos)$size))
  )
}

preparar_malhas_base <- function() {
  fp <- .malhas_fingerprint()
  
  if (file.exists(MALHAS_CACHE_RDS)) {
    cache <- tryCatch(readRDS(MALHAS_CACHE_RDS), error = function(e) NULL)
    if (!is.null(cache) && identical(cache$fingerprint, fp)) {
      for (chave in names(cache$malhas)) {
        assign(chave, cache$malhas[[chave]], envir = .cache_geojson)
      }
      return(length(cache$malhas[["BASE_MUN"]]$features))
    }
  }
  
  # Cache ausente ou desatualizado: parse + filtro do zero (caminho lento -
  # so acontece na primeira vez, ou quando a base/malhas fonte mudam).
  codigos <- as.character(GEO_DICT$municipio_geocodigo)
  todas   <- list()
  
  for (uf in sort(unique(GEO_DICT$uf_ibge))) {
    caminho <- file.path(GEO_DIR, sprintf("MUN_%d.geojson", uf))
    if (!file.exists(caminho)) {
      warning("[Dengue RIDE] Malha ausente: MUN_", uf, ".geojson", call. = FALSE)
      next
    }
    
    js <- jsonlite::fromJSON(caminho, simplifyVector = FALSE)
    feicoes <- Filter(
      function(f) as.character(f$properties[[GEO_NAME_PROPERTY]]) %in% codigos,
      js$features
    )
    if (!length(feicoes)) next
    
    assign(
      sprintf("MUN_%d", uf),
      list(type = "FeatureCollection", features = feicoes),
      envir = .cache_geojson
    )
    todas <- c(todas, feicoes)
  }
  
  if (length(todas)) {
    assign(
      "BASE_MUN",
      list(type = "FeatureCollection", features = todas),
      envir = .cache_geojson
    )
  }
  
  tryCatch(
    saveRDS(list(fingerprint = fp, malhas = as.list(.cache_geojson)), MALHAS_CACHE_RDS),
    error = function(e) {
      warning("[Dengue RIDE] Nao foi possivel gravar cache de malhas: ", conditionMessage(e), call. = FALSE)
    }
  )
  
  length(todas)
}

local({
  n <- preparar_malhas_base()
  if (n < nrow(GEO_DICT)) {
    warning(sprintf(
      "[Dengue RIDE] %d de %d municípios sem polígono. O mapa ficará incompleto.",
      nrow(GEO_DICT) - n, nrow(GEO_DICT)
    ), call. = FALSE, immediate. = TRUE)
  } else {
    message(sprintf("[Dengue RIDE] Malhas recortadas: %d municípios.", n))
  }
})

# ------------------------------------------ serie semanal por municipio -----
# Usada por R/mod_mapa_rt.R (mapa de tendencia/quadrantes) e por
# R/mod_epi_scanner_server.R (aba Analises). Ao contrario de
# consultar_serie_semanal(), aqui NADA e somado entre municipios: cada linha
# e um municipio numa semana. A base tem so 29 municipios e o historico
# completo (~25 mil linhas) - trivial para manter inteira em memoria,
# calculada uma unica vez no start, como GEO_DICT.
#
# A coluna `rt` (max por municipio/semana) entra aqui para que o Epi Scanner
# derive as metricas dele desta mesma tabela, em vez de disparar uma segunda
# agregacao da base inteira no DuckDB a cada primeira visita da aba.
message("[Dengue RIDE] Carregando série semanal por município...")

SERIE_MUNICIPIOS <- tbl_dengue() |>
  dplyr::group_by(municipio_geocodigo, data_ini_se) |>
  dplyr::summarise(
    se    = max(se, na.rm = TRUE),
    casos = sum(casos, na.rm = TRUE),
    pop   = max(pop, na.rm = TRUE),
    # NULL (todas as semanas do municipio sem estimativa) vem como NA do
    # collect() - o SQL ignora NULL no max(); nunca vira -Inf aqui.
    rt    = max(rt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::collect() |>
  dplyr::mutate(
    municipio_geocodigo = as.integer(municipio_geocodigo),
    data_ini_se          = as.Date(data_ini_se),
    se                   = as.integer(se),
    ano                  = se %/% 100L,
    semana               = se %%  100L,
    casos                = as.numeric(casos),
    pop                  = as.numeric(pop),
    rt                   = as.numeric(rt)
  ) |>
  dplyr::left_join(
    dplyr::distinct(GEO_DICT, municipio_geocodigo, municipio_nome),
    by = "municipio_geocodigo"
  ) |>
  dplyr::arrange(municipio_geocodigo, data_ini_se)

message(sprintf(
  "[Dengue RIDE] Série por município pronta: %d linhas / %d município(s).",
  nrow(SERIE_MUNICIPIOS), dplyr::n_distinct(SERIE_MUNICIPIOS$municipio_geocodigo)
))

# ----------------------------------------------------- paleta chocolate -----
# Rampa sequencial creme -> chocolate. Definida AQUI, e nao em R/utils_ui.R,
# porque os modulos em R/ sao carregados em ordem alfabetica: utils_ui.R viria
# por ultimo e as constantes de cor no topo dos modulos nao a encontrariam.
PALETA_CHOCOLATE <- c(
  creme     = "#FDF7F0",
  baunilha  = "#F3E4D2",
  areia     = "#EBD5B8",
  caramelo  = "#CBA074",
  cacau     = "#A9743F",
  chocolate = "#7A4A22",
  amargo    = "#4A2A10",
  ambar     = "#C9791F",  # acento quente, para series que precisam se separar
  taupe     = "#8A6A4F",  # neutro da familia, para linhas de referencia
  tinta     = "#3E2110"   # quase preto amarronzado, para a serie em foco
)

#' Rampa continua, do creme ao amargo. Para e_visual_map(type = "continuous").
RAMPA_CHOCOLATE <- unname(PALETA_CHOCOLATE[
  c("creme", "areia", "caramelo", "cacau", "chocolate", "amargo")
])

# ------------------------------------------------- escala de incidencia -----
# Cortes usados pelo Ministerio da Saude (casos/100 mil hab.):
# < 100 baixa, 100-299 media, >= 300 alta. Os dois ultimos niveis separam
# situacoes epidemicas que a faixa ">= 300" sozinha achataria.
PECAS_INCIDENCIA <- list(
  list(min = 1000,             label = "≥ 1.000",   color = PALETA_CHOCOLATE[["amargo"]]),
  list(min = 500,  max = 1000, label = "500 – 999", color = PALETA_CHOCOLATE[["chocolate"]]),
  list(min = 300,  max = 500,  label = "300 – 499", color = PALETA_CHOCOLATE[["cacau"]]),
  list(min = 100,  max = 300,  label = "100 – 299", color = PALETA_CHOCOLATE[["caramelo"]]),
  list(min = 0.01, max = 100,  label = "< 100",     color = PALETA_CHOCOLATE[["areia"]]),
  list(value = 0,              label = "Sem casos", color = PALETA_CHOCOLATE[["creme"]])
)

# ----------------------------------------------------------------- helpers ---

#' Aplica o recorte geografico a uma query lazy.
#'
#' O filtro entra no WHERE; nada e materializado aqui. A chave "brasil"
#' significa "toda a base" (RIDE-DF), nao o pais.
aplicar_filtro_geo <- function(dados, geo) {
  switch(
    geo$nivel,
    brasil    = dados,
    uf        = dplyr::filter(dados, uf_ibge == !!geo$uf_ibge),
    municipio = dplyr::filter(dados, municipio_geocodigo == !!geo$geocodigo),
    stop("Nivel geografico desconhecido: ", geo$nivel, call. = FALSE)
  )
}

#' Serie temporal semanal agregada para um recorte geografico.
#'
#' O filtro desce ate a leitura do Parquet (predicate pushdown) e so as colunas
#' citadas sao lidas do disco (projection pushdown). Volta uma linha por semana.
#'
#' Colunas devolvidas, por semana:
#'   data_ini_se  inicio da semana epidemiologica
#'   se           semana epidemiologica no formato AAAASS
#'   casos        notificados
#'   casos_est    estimados pelo nowcast do InfoDengue
#'   pop          populacao somada dos municipios do recorte naquela semana
#'   rt_direto    rt da linha (so e o proprio rt no nivel municipal)
#'   rt_num/rt_peso  numerador e peso da media de rt ponderada por casos
#'   n_mun        municipios com registro na semana
#'   n_mun_rt1    municipios com rt > 1 na semana
consultar_serie_semanal <- function(geo) {
  aplicar_filtro_geo(tbl_dengue(), geo) |>
    dplyr::group_by(data_ini_se) |>
    dplyr::summarise(
      # se e constante dentro de uma data_ini_se (a semana epidemiologica e
      # nacional); max() e so a forma de carrega-la pela agregacao.
      se        = max(se, na.rm = TRUE),
      casos     = sum(casos, na.rm = TRUE),
      casos_est = sum(casos_est, na.rm = TRUE),
      pop       = sum(pop, na.rm = TRUE),
      rt_direto = max(rt, na.rm = TRUE),
      rt_num    = sum(rt * casos, na.rm = TRUE),
      rt_peso   = sum(dplyr::if_else(!is.na(rt), casos, 0L), na.rm = TRUE),
      n_mun     = dplyr::n_distinct(municipio_geocodigo),
      n_mun_rt1 = sum(dplyr::if_else(rt > 1, 1L, 0L), na.rm = TRUE),
      .groups   = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      data_ini_se = as.Date(data_ini_se),
      se          = as.integer(se),
      dplyr::across(c(casos, casos_est, pop, rt_direto, rt_num, rt_peso), as.numeric),
      n_mun       = as.integer(n_mun),
      n_mun_rt1   = as.integer(n_mun_rt1)
    ) |>
    dplyr::arrange(data_ini_se)
}

#' Reagrupa a serie semanal por mes, ou devolve como esta.
#'
#' A semana e atribuida inteira ao mes em que COMECA (data_ini_se). Semanas a
#' cavalo entre dois meses nao sao rateadas - convencao usual em vigilancia,
#' que mantem a semana epidemiologica como unidade indivisivel.
reagrupar_periodo <- function(d, agrupamento = c("semana", "mes")) {
  agrupamento <- match.arg(agrupamento)
  
  if (identical(agrupamento, "semana")) {
    d$periodo <- d$data_ini_se
    return(d)
  }
  
  d |>
    dplyr::mutate(periodo = as.Date(format(data_ini_se, "%Y-%m-01"))) |>
    dplyr::group_by(periodo) |>
    dplyr::summarise(
      casos     = sum(casos, na.rm = TRUE),
      casos_est = sum(casos_est, na.rm = TRUE),
      # Populacao nao se soma ao longo do tempo: o denominador do mes e o
      # mesmo conjunto de pessoas das semanas que o compoem.
      pop       = suppressWarnings(max(pop, na.rm = TRUE)),
      rt_direto = mean(rt_direto, na.rm = TRUE),
      rt_num    = sum(rt_num, na.rm = TRUE),
      rt_peso   = sum(rt_peso, na.rm = TRUE),
      n_mun     = suppressWarnings(max(n_mun, na.rm = TRUE)),
      n_mun_rt1 = round(mean(n_mun_rt1, na.rm = TRUE)),
      .groups   = "drop"
    ) |>
    dplyr::mutate(
      pop   = dplyr::if_else(is.finite(pop), pop, NA_real_),
      n_mun = dplyr::if_else(is.finite(n_mun), n_mun, NA_integer_)
    )
}

#' Deriva incidencia e Rt a partir da serie agregada.
#'
#' Rt depende do nivel do recorte: no municipio e o valor da coluna; acima
#' disso e a media ponderada pelos casos, que e aproximacao - ver mod_rt.R.
calcular_indicadores <- function(d, geo) {
  d |>
    dplyr::mutate(
      incidencia     = dplyr::if_else(pop > 0, casos     / pop * 1e5, NA_real_),
      incidencia_est = dplyr::if_else(pop > 0, casos_est / pop * 1e5, NA_real_),
      rt = if (identical(geo$nivel, "municipio")) {
        rt_direto
      } else {
        dplyr::if_else(rt_peso > 0, rt_num / rt_peso, NA_real_)
      }
    )
}

#' Expressao JS que formata a data do eixo (pt-BR), operando sobre a variavel `v`.
#'
#' O eixo X do echarts4r e categorico e entrega a data como texto "AAAA-MM-DD".
#' A formatacao e feita por manipulacao de string, NAO por `new Date(v)`:
#' "2024-01-05" e interpretado como UTC pelo JS, e toLocaleDateString num
#' navegador em UTC-3 devolveria 04/01/2024 - um dia a menos.
js_formato_data <- function(escala = c("mes", "dia")) {
  escala <- match.arg(escala)
  meses <- "['jan','fev','mar','abr','mai','jun','jul','ago','set','out','nov','dez']"
  
  if (identical(escala, "mes")) {
    sprintf(
      "(function(s){var p=String(s).split('-');return %s[parseInt(p[1],10)-1]+'/'+p[0];})(v)",
      meses
    )
  } else {
    "(function(s){var p=String(s).split('-');return p[2]+'/'+p[1]+'/'+p[0];})(v)"
  }
}

#' Formata inteiros no padrao brasileiro (12.345).
fmt_int <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  format(round(as.numeric(x)), big.mark = ".", decimal.mark = ",",
         scientific = FALSE, trim = TRUE)
}

#' Formata numeros com casas decimais no padrao brasileiro (1.234,5).
fmt_num <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  format(round(as.numeric(x), digits), big.mark = ".", decimal.mark = ",",
         nsmall = digits, scientific = FALSE, trim = TRUE)
}

#' Coalesce nulo/vazio (util nos modulos; forecasting/ tambem define o seu).
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---------------------------------------------------- forecasting (previsao) --
# Backend isolado em forecasting/ (treino offline via Rscript; ver
# README-FORECAST.md). Aqui so expomos get_dengue_forecast() aos modulos e
# carregamos as metricas do backtest. Se os pacotes (forecast/mgcv) ou os
# artefatos .rds nao existirem, o painel segue funcionando SEM a camada de
# previsao - FORECAST_DISPONIVEL controla isso nos modulos.
FORECAST_DISPONIVEL <- FALSE
FORECAST_METRICS    <- NULL   # linhas "todos" do model_metrics.csv (com coluna `classe`)
FORECAST_CLASSES    <- NULL   # c(chave = rotulo) das classes com modelo

local({
  arq_fun <- file.path("forecasting", "get_forecast.R")
  arq_cfg <- file.path("forecasting", "models", "ensemble_config.rds")
  arq_met <- file.path("forecasting", "model_metrics.csv")

  if (!file.exists(arq_fun) || !file.exists(arq_cfg)) {
    message("[Dengue RIDE] Forecasting: artefatos ausentes - camada de previsao desativada.")
    return(invisible())
  }

  ok <- tryCatch({
    source(arq_fun, local = FALSE, encoding = "UTF-8")
    is.function(get_dengue_forecast)
  }, error = function(e) {
    message("[Dengue RIDE] Forecasting indisponivel: ", conditionMessage(e))
    FALSE
  })
  if (!isTRUE(ok)) return(invisible())

  if (file.exists(arq_met)) {
    m <- tryCatch(utils::read.csv(arq_met, stringsAsFactors = FALSE, encoding = "UTF-8"),
                  error = function(e) NULL)
    if (!is.null(m) && "horizonte" %in% names(m)) {
      keep <- intersect(c("classe", "modelo", "RMSE", "MAE", "MAPE", "sMAPE", "MASE", "peso"),
                        names(m))
      FORECAST_METRICS <<- m[m$horizonte == "todos", keep]
    }
  }
  # Classes com modelo registrado (FC_CLASSES / fc_classes_com_modelo vem de
  # utils_forecast.R, ja carregado pelo source de get_forecast.R acima).
  if (exists("fc_classes_com_modelo")) {
    cc <- fc_classes_com_modelo()
    FORECAST_CLASSES <<- stats::setNames(vapply(cc, fc_rotulo_classe, character(1)), cc)
  }
  FORECAST_DISPONIVEL <<- TRUE
  message(sprintf("[Dengue RIDE] Forecasting ativo (%d modelo(s), %d classe(s): %s).",
                  if (exists("FC_MODELOS")) length(FC_MODELOS) else 0L,
                  length(FORECAST_CLASSES),
                  paste(FORECAST_CLASSES, collapse = ", ")))
})

# Rotulo amigavel por chave de modelo/ensemble - usado pelos modulos do painel.
FORECAST_ROTULO <- function(chave) {
  if (!isTRUE(FORECAST_DISPONIVEL)) return(chave)
  if (identical(chave, "global"))    return("Ensemble global")
  if (startsWith(chave, "ens_"))     return(sprintf("Ensemble %s",
                                    fc_rotulo_classe(sub("^ens_", "", chave))))
  fc_rotulo_modelo(chave)
}

# --------------------------------------------------------- modulos do app ----
# Carregamento explicito: nao depende da ordem de auto-source do diretorio R/.
for (.f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(.f, local = FALSE, encoding = "UTF-8")
}
rm(.f)