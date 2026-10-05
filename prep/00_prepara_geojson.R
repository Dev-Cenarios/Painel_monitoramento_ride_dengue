# ==============================================================================
# Dengue RIDE | prep/00_prepara_geojson.R
#
# Executar UMA VEZ (ou quando a malha territorial mudar).
# Baixa as malhas da API oficial do IBGE (servicodados.ibge.gov.br/api/v3):
#
#   data/geo/BR_UF.geojson    -> 27 unidades da federacao
#   data/geo/MUN_11.geojson   -> municipios de Rondonia
#   ...                          (um arquivo por UF)
#
# Por que a API do IBGE e nao o geobr:
#   - Nao exige sf/GDAL/GEOS/PROJ no servidor; so jsonlite e curl.
#   - Ja entrega fatiado por UF, que e a unidade de carga do app.
#   - Simplificacao geometrica embutida (parametro `qualidade`).
#   - O geocodigo ja vem em properties.codarea, entao nao ha nada a reescrever:
#     o download e o arquivo final.
#
# ATENCAO ao formato: o valor "application/vnd.geo+json" PRECISA ir
# percent-encoded. Numa query string o "+" e decodificado como espaco, e a API
# responde 400 Bad Request.
#
# Uso:  setwd("~/shiny-server/dengue_ride"); source("prep/00_prepara_geojson.R")
# ==============================================================================

if (!requireNamespace("curl", quietly = TRUE)) {
  stop("Instale antes: install.packages('curl')", call. = FALSE)
}

DESTINO <- file.path("data", "geo")
dir.create(DESTINO, recursive = TRUE, showWarnings = FALSE)

# qualidade: "minima", "intermediaria" ou "maxima" (a API rejeita numeros).
# "intermediaria" e o equilibrio para mapas em tela: contorno reconhecivel,
# arquivo pequeno. Se a malha municipal de SP/MG ficar pesada, use "minima".
QUALIDADE <- "intermediaria"

BASE <- "https://servicodados.ibge.gov.br/api/v3/malhas"
FMT  <- utils::URLencode("application/vnd.geo+json", reserved = TRUE)

CODIGOS_UF <- c(11, 12, 13, 14, 15, 16, 17,
                21, 22, 23, 24, 25, 26, 27, 28, 29,
                31, 32, 33, 35,
                41, 42, 43,
                50, 51, 52, 53)

# --------------------------------------------------------------- utilitarios --

#' Variantes de URL, da mais completa para a mais simples.
#' Se `qualidade` nao for aceito num endpoint, a segunda variante ainda funciona.
urls_uf <- function() {
  c(
    sprintf("%s/paises/BR?formato=%s&intrarregiao=UF&qualidade=%s", BASE, FMT, QUALIDADE),
    sprintf("%s/paises/BR?formato=%s&intrarregiao=UF",              BASE, FMT)
  )
}

urls_municipios <- function(uf) {
  c(
    sprintf("%s/estados/%d?formato=%s&intrarregiao=municipio&qualidade=%s", BASE, uf, FMT, QUALIDADE),
    sprintf("%s/estados/%d?formato=%s&intrarregiao=municipio",              BASE, uf, FMT)
  )
}

#' Busca a primeira URL que devolva 200 com um FeatureCollection valido.
#' Em caso de erro, imprime o corpo da resposta - a API do IBGE explica o
#' motivo do 400, o que economiza adivinhacao.
baixar <- function(urls, destino) {
  for (url in urls) {
    r <- tryCatch(curl::curl_fetch_memory(url), error = function(e) {
      message("    rede: ", conditionMessage(e))
      NULL
    })
    if (is.null(r)) next
    
    if (r$status_code != 200) {
      corpo <- rawToChar(r$content[seq_len(min(300, length(r$content)))])
      message(sprintf("    HTTP %d: %s", r$status_code,
                      gsub("[\r\n]+", " ", corpo)))
      next
    }
    
    writeBin(r$content, destino)
    return(TRUE)
  }
  FALSE
}

#' Confere que o arquivo e um FeatureCollection e que codarea esta presente.
#' Sem isso, um HTML/JSON de erro salvo como .geojson so apareceria como mapa
#' em branco muito depois, dentro do app.
validar <- function(caminho) {
  js <- try(jsonlite::fromJSON(caminho, simplifyVector = FALSE), silent = TRUE)
  if (inherits(js, "try-error") || is.null(js$features)) {
    return(list(ok = FALSE, n = 0L, exemplo = NA_character_))
  }
  cod <- js$features[[1]]$properties$codarea
  list(
    ok      = !is.null(cod),
    n       = length(js$features),
    exemplo = if (is.null(cod)) NA_character_ else as.character(cod)
  )
}

# ------------------------------------------------------------ estados (UF) --
message("[prep] Baixando malha estadual...")

arq_uf <- file.path(DESTINO, "BR_UF.geojson")

if (!baixar(urls_uf(), arq_uf)) {
  stop("Nao foi possivel baixar a malha estadual. Veja o HTTP acima.", call. = FALSE)
}

v <- validar(arq_uf)
if (!v$ok) {
  stop("BR_UF.geojson invalido (sem properties.codarea).", call. = FALSE)
}
message(sprintf("[prep] BR_UF.geojson: %d feicoes, %.0f KB, codarea ex.: %s",
                v$n, file.size(arq_uf) / 1024, v$exemplo))

# -------------------------------------------------------------- municipios --
message("[prep] Baixando malhas municipais por UF...")

linhas <- lapply(CODIGOS_UF, function(uf) {
  arq <- file.path(DESTINO, sprintf("MUN_%d.geojson", uf))
  
  if (!baixar(urls_municipios(uf), arq)) {
    message(sprintf("  UF %d: FALHOU", uf))
    return(data.frame(uf = uf, municipios = NA_integer_, kb = NA_real_, ok = FALSE))
  }
  
  v <- validar(arq)
  message(sprintf("  UF %d: %d municipios, %.0f KB", uf, v$n, file.size(arq) / 1024))
  data.frame(uf = uf, municipios = v$n, kb = round(file.size(arq) / 1024), ok = v$ok)
})

resumo <- do.call(rbind, linhas)
print(resumo)

falhas <- resumo[!resumo$ok, "uf"]
if (length(falhas)) {
  warning("UFs com falha: ", paste(falhas, collapse = ", "),
          ". Rode o script de novo para completar.", call. = FALSE)
}

message(sprintf(
  "[prep] Concluido: %d/%d UFs, %s municipios, %.1f MB em '%s'.",
  sum(resumo$ok), nrow(resumo),
  format(sum(resumo$municipios, na.rm = TRUE), big.mark = "."),
  sum(resumo$kb, na.rm = TRUE) / 1024, DESTINO
))