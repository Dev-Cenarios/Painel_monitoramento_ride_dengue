# ==============================================================================
# Dengue RIDE | prep/01_simplifica_malhas.R
#
# Executar UMA VEZ (ou de novo, se prep/00_prepara_geojson.R baixar malhas
# novas). Reduz o numero de vertices dos poligonos municipais nos GeoJSON de
# data/geo/MUN_<uf>.geojson - so das UFs que aparecem no Parquet atual (as
# outras nunca sao lidas pelo app, entao simplifica-las seria trabalho
# perdido).
#
# POR QUE ISSO IMPORTA:
#   O mapa coropletico (R/mod_mapa.R) embute a malha inteira no payload do
#   grafico a cada sessao do navegador - nao e um arquivo estatico cacheavel,
#   viaja de novo em cada pagina aberta.
#
# POR QUE ISSO E UM PASSO DE PREPARO, NAO CODIGO DO APP:
#   sf/GDAL/GEOS sao dependencias pesadas de instalar num servidor de deploy.
#   Simplificar aqui, uma vez, e regravar os MESMOS arquivos .geojson menores
#   mantem o app rodando so com jsonlite. O cache em disco de R/global.R
#   (data/geo/.malhas_base_cache.rds) detecta a mudanca de data dos
#   arquivos-fonte sozinho e se reconstroi na proxima subida do app.
#
# CAUSA RAIZ DO "0% DE REDUCAO" (corrigida): a primeira versao deste script
# rodava st_simplify() com dTolerance em GRAUS, direto sobre EPSG:4326
# (lon/lat). Nessa configuracao a simplificacao remove pouco ou nada de forma
# silenciosa - o mesmo achado documentado no ALTERACOES-PERFORMANCE do painel
# de tuberculose. Agora o script projeta cada malha para um CRS METRICO
# (CRS_METRICO, abaixo), simplifica com tolerancia EM METROS e reprojeta de
# volta para 4326 antes de gravar (GeoJSON tem de ser lon/lat).
#
# DIAGNOSTICO EMBUTIDO: o script imprime SEMPRE contagem de vertices (nao so
# KB) e a config do sf/GEOS. Se ainda aparecer "SEM REDUCAO" no relatorio, a
# causa restante e o arquivo-fonte ja ter vindo simplificado por fora (ex.:
# baixado com QUALIDADE = "minima" em vez de "intermediaria" no prep/00) -
# nesse caso nao ha mais o que remover nesta escala, e o numero de vertices
# antes/depois deixa isso claro sem adivinhar.
#
# Uso:  setwd("~/shiny-server/dengue_ride"); source("prep/01_simplifica_malhas.R")
# ==============================================================================

if (!requireNamespace("sf", quietly = TRUE) || !requireNamespace("duckdb", quietly = TRUE)) {
  stop("Instale antes: install.packages(c('sf', 'duckdb'))", call. = FALSE)
}

# Tolerancia de simplificacao, EM METROS (a simplificacao roda num CRS
# metrico - ver CRS_METRICO).
#
# As malhas do IBGE ja chegam pre-simplificadas (prep/00 usa
# qualidade = "intermediaria"), entao ate ~150 m nao ha praticamente nada a
# remover. Medido no recorte RIDE (34 municipios, o que de fato viaja no
# payload do mapa):
#   sem simplificar : ~6.000 vertices / ~144 KB em disco
#   250 m           : ~5.000 vertices / ~120 KB   (~20% menor, sem efeito visivel)
#   500 m           : ~3.000 vertices /  ~75 KB   (~48% menor, corta canto em zoom)
# 250 m e o meio-termo: o mapa usa roam = TRUE (zoom livre) e a 250 m o
# contorno continua fiel mesmo ampliado. Suba para 500 so se o payload pesar
# mais que o detalhe.
TOLERANCIA_METROS <- 250

# SIRGAS 2000 / Brazil Polyconic (EPSG:5880): projecao metrica oficial do
# IBGE, valida para todo o territorio nacional. O fluxo e sempre
# projeta(5880) -> st_simplify(metros) -> reprojeta(4326).
CRS_METRICO <- 5880

GEO_DIR        <- file.path("data", "geo")
PARQUET_DENGUE <- Sys.getenv("DENGUE_PARQUET", file.path("data", "dengue_ride_df.parquet"))

sf::sf_use_s2(FALSE)

message(sprintf("[prep] sf %s | GEOS %s | s2 ativo: %s",
                as.character(utils::packageVersion("sf")),
                sf::sf_extSoftVersion()[["GEOS"]],
                sf::sf_use_s2()))

contar_vertices <- function(shp) sum(vapply(sf::st_geometry(shp), function(g) nrow(sf::st_coordinates(g)), integer(1)))

# -------------------------------------------------- UFs realmente usadas ----
# So le a coluna uf_ibge do Parquet (via DuckDB, ja dependencia do app - evita
# exigir o arrow so para este passo de preparo).
local({
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  caminho <- normalizePath(PARQUET_DENGUE, winslash = "/", mustWork = TRUE)
  ufs <<- sort(as.integer(DBI::dbGetQuery(con, sprintf(
    "SELECT DISTINCT uf_ibge FROM read_parquet('%s') WHERE uf_ibge IS NOT NULL",
    gsub("'", "''", caminho, fixed = TRUE)
  ))$uf_ibge))
})
message(sprintf("[prep] UFs em uso no Parquet: %s\n", paste(ufs, collapse = ", ")))

# ------------------------------------------------------------ simplifica ----
resumo <- lapply(ufs, function(uf) {
  caminho <- file.path(GEO_DIR, sprintf("MUN_%d.geojson", uf))
  if (!file.exists(caminho)) {
    message(sprintf("  UF %d: arquivo ausente, pulando.", uf))
    return(data.frame(uf = uf, antes_kb = NA, depois_kb = NA, antes_v = NA, depois_v = NA, ok = FALSE))
  }
  
  antes_kb <- file.size(caminho) / 1024
  shp <- sf::st_read(caminho, quiet = TRUE)
  antes_v <- contar_vertices(shp)
  
  if (!"codarea" %in% names(shp)) {
    warning(sprintf("[prep] MUN_%d.geojson sem coluna 'codarea' - nao mexido.", uf), call. = FALSE)
    return(data.frame(uf = uf, antes_kb = antes_kb, depois_kb = antes_kb,
                      antes_v = antes_v, depois_v = antes_v, ok = FALSE))
  }
  
  # projeta -> simplifica em metros -> volta para lon/lat (GeoJSON = 4326)
  shp_m       <- sf::st_transform(shp, CRS_METRICO)
  shp_m       <- sf::st_simplify(shp_m, dTolerance = TOLERANCIA_METROS, preserveTopology = TRUE)
  shp_simples <- sf::st_transform(shp_m, 4326)
  
  vazio <- sf::st_is_empty(shp_simples)
  if (any(vazio)) {
    sf::st_geometry(shp_simples)[vazio] <- sf::st_geometry(shp)[vazio]
    message(sprintf("  UF %d: %d poligono(s) mantido(s) sem simplificar (ficariam vazios).",
                    uf, sum(vazio)))
  }
  
  depois_v <- contar_vertices(shp_simples)
  
  if (depois_v >= antes_v) {
    message(sprintf(
      "  UF %d: SEM REDUCAO (%d -> %d vertices). Nao regravado - ver diagnostico no topo do script.",
      uf, antes_v, depois_v
    ))
    return(data.frame(uf = uf, antes_kb = round(antes_kb), depois_kb = round(antes_kb),
                      antes_v = antes_v, depois_v = depois_v, ok = FALSE))
  }
  
  sf::st_write(shp_simples, caminho, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  depois_kb <- file.size(caminho) / 1024
  
  message(sprintf(
    "  UF %d: %.0f KB -> %.0f KB (%.0f%%) | %d -> %d vertices (%.0f%%) | %d municipios",
    uf, antes_kb, depois_kb, 100 * depois_kb / antes_kb,
    antes_v, depois_v, 100 * depois_v / antes_v, nrow(shp)
  ))
  
  data.frame(uf = uf, antes_kb = round(antes_kb), depois_kb = round(depois_kb),
             antes_v = antes_v, depois_v = depois_v, ok = TRUE)
})

resumo <- do.call(rbind, resumo)
cat("\n")
print(resumo)

if (all(!resumo$ok)) {
  message(paste0(
    "\n[prep] NENHUM arquivo foi reduzido. Antes de tentar de novo, roda isto e me manda o resultado:\n",
    "  packageVersion('sf'); sf::sf_extSoftVersion(); sf::sf_use_s2()\n",
    "  shp <- sf::st_read('data/geo/MUN_", ufs[1], ".geojson', quiet=TRUE)\n",
    "  sf::st_bbox(shp)  # confirma se as coordenadas sao mesmo lon/lat (graus)\n"
  ))
} else {
  message(sprintf(
    "\n[prep] Concluido: %.0f KB -> %.0f KB no total (%.0f%%).",
    sum(resumo$antes_kb, na.rm = TRUE), sum(resumo$depois_kb, na.rm = TRUE),
    100 * sum(resumo$depois_kb, na.rm = TRUE) / sum(resumo$antes_kb, na.rm = TRUE)
  ))
}