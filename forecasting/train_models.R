# ==============================================================================
# Dengue RIDE | forecasting/train_models.R
#
# Pipeline OFFLINE de treino e avaliacao dos modelos de previsao de casos de
# dengue (padrao Mosqlimate). NAO e chamado pelo Shiny - roda a mao:
#
#     cd dengue_ride
#     Rscript forecasting/train_models.R
#
# Etapas:
#   1. Le a base historica via DuckDB (agregado RIDE-DF, = serie_semanal).
#   2. Backtest de origem rolante de TODOS os modelos do registro FC_MODELOS.
#   3. Metricas por classe/modelo/horizonte -> forecasting/model_metrics.csv.
#   4. Pesos: inverso do RMSE DENTRO da classe (modelo) e entre classes (global).
#   5. Reajusta os modelos `cacheavel` (ARIMA, GAM, RF) na serie inteira e salva:
#        forecasting/models/ensemble_config.rds   (pesos + metricas + params)
#        forecasting/models/agg_ride_df.rds       (fits pre-ajustados)
#   6. Atualiza a tabela de metricas em forecasting/README-FORECAST.md.
#
# Escopo do treino: a RIDE-DF agregada (serie mais longa e estavel; coerente
# com a filosofia da aba "Analises", que ignora o recorte de proposito). A
# funcao get_dengue_forecast() reaproveita estes pesos e reajusta as specs ao
# recorte pedido em tempo de chamada.
# ==============================================================================

t0 <- Sys.time()
set.seed(20260903)
options(warn = 1)   # avisos com contexto na hora, sem o vago "Houve N avisos" no fim

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(nome, default) {
  hit <- grep(sprintf("^--%s=", nome), args, value = TRUE)
  if (length(hit)) sub(sprintf("^--%s=", nome), "", hit[1]) else default
}
N_ORIGENS  <- as.integer(get_arg("origens", "24"))
MIN_TREINO <- as.integer(get_arg("min_treino", "156"))
PARQUET    <- get_arg("parquet", "")
if (!nzchar(PARQUET)) PARQUET <- NULL

# --------------------------------------------------------------- utils --------
this_dir <- local({
  cand <- character()
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) cand <- c(cand, dirname(normalizePath(sub("^--file=", "", a[1]), mustWork = FALSE)))
  cand <- c(cand, file.path(getwd(), "forecasting"), file.path(getwd(), "dengue_ride", "forecasting"), getwd())
  hit <- cand[file.exists(file.path(cand, "utils_forecast.R"))]
  if (!length(hit)) stop("train_models.R: utils_forecast.R nao encontrado. Rode a partir de dengue_ride/ (Rscript forecasting/train_models.R).", call. = FALSE)
  hit[1]
})
source(file.path(this_dir, "utils_forecast.R"), local = FALSE, encoding = "UTF-8")

dir.create(fc_models_dir(), showWarnings = FALSE, recursive = TRUE)

# ============================================================ 1. base ========
message("\n== 1. Base historica (RIDE-DF agregada) ==")
con <- fc_connect(PARQUET)
on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)

fp  <- fc_fingerprint(con, PARQUET)
geo <- fc_resolve_geo(NULL)                    # base inteira
d   <- fc_prepare(fc_query_serie(con, geo$where))

message(sprintf("   %d semanas | %s a %s | %d casos no total",
                nrow(d), format(min(d$data_ini_se)), format(max(d$data_ini_se)),
                round(sum(d$casos))))

# ====================================================== 2. backtest =========
message(sprintf("\n== 2. Backtest de origem rolante (%d origens, h = %d) ==",
                N_ORIGENS, FC_HORIZON_DEFAULT))

# Os ajustes de ets()/auto.arima() em dezenas de janelas emitem avisos benignos
# de convergencia. Sao contabilizados por tipo e resumidos abaixo, em vez do
# generico "Houve N avisos" do R.
.warn_tally <- new.env(parent = emptyenv())
bt <- withCallingHandlers(
  fc_backtest(d, h = FC_HORIZON_DEFAULT, n_origens = N_ORIGENS,
              min_treino = MIN_TREINO, verbose = TRUE),
  warning = function(w) {
    msg <- conditionMessage(w)
    .warn_tally[[msg]] <- (.warn_tally[[msg]] %||% 0L) + 1L
    invokeRestart("muffleWarning")
  }
)
if (length(ls(.warn_tally))) {
  message("   avisos de convergencia (benignos) durante o backtest:")
  for (m in ls(.warn_tally)) message(sprintf("     %2dx  %s", .warn_tally[[m]], m))
}

# ============================================== 3-4. metricas + pesos =======
message("\n== 3. Metricas e pesos do ensemble ==")
cons    <- fc_consolidar_backtest(bt, d, h = FC_HORIZON_DEFAULT)
metrics <- cons$metrics
pesos   <- cons$pesos

metrics$escopo <- geo$rotulo
metrics <- metrics[, c("escopo", "classe", "modelo", "horizonte", "RMSE", "MAE",
                       "MAPE", "sMAPE", "MASE", "n", "n_mape", "peso")]
num <- c("RMSE", "MAE", "MAPE", "sMAPE", "MASE")
metrics[num] <- lapply(metrics[num], function(x) round(x, 3))

csv_path <- file.path(fc_dir(), "model_metrics.csv")
utils::write.csv(metrics, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
message("   -> ", csv_path)

print(metrics[metrics$horizonte == "todos",
              c("classe", "modelo", "RMSE", "MAE", "MAPE", "MASE", "peso")],
      row.names = FALSE)

# ===================================== 5. modelos finais + artefatos ========
message("\n== 4. Reajuste no corte de treino e serializacao ==")

# Reajusta os modelos marcados como `cacheavel` no registro (arima, sarima,
# gam, rf - STL+ETS refaz na chamada, e barato) no MESMO corte que
# get_dengue_forecast() usaria hoje: fc_corte_treino() (utils_forecast.R),
# NAO na serie inteira. Critico para o cache: se o fit fosse na serie
# inteira mas a chamada ao vivo SEMPRE reprojeta as ultimas
# FC_SEMANAS_RECENTES semanas (regra "nunca ancora na ultima disponivel"),
# o numero de linhas nunca bateria e get_dengue_forecast() jamais
# reaproveitaria o cache - foi exatamente esse descompasso, nao so o
# fingerprint por mtime, que fazia o forecasting refazer TODOS os fits em
# TODA chamada (ver auditoria de performance, README-FORECAST.md secao 8).
.tally2 <- new.env(parent = emptyenv())
modelos_cache <- names(Filter(function(m) isTRUE(m$cacheavel), FC_MODELOS))

#' Ajusta os modelos `cacheavel` no corte de treino de `d_serie` e devolve os
#' fits + o numero de linhas usado (guardado como `n_treino` no artefato -
#' e o que get_dengue_forecast() compara pra decidir se o cache ainda vale).
.fc_ajustar_cacheaveis <- function(d_serie) {
  d_fit <- fc_corte_treino(d_serie)
  fits <- withCallingHandlers(
    stats::setNames(lapply(modelos_cache, function(k)
      tryCatch(FC_MODELOS[[k]]$fit(d_fit), error = function(e) {
        message("   [", k, "] fit falhou: ", conditionMessage(e)); NULL
      })), modelos_cache),
    warning = function(w) {
      m <- conditionMessage(w)
      .tally2[[m]] <- (.tally2[[m]] %||% 0L) + 1L
      invokeRestart("muffleWarning")
    }
  )
  list(modelos = fits[!vapply(fits, is.null, logical(1))], n_treino = nrow(d_fit))
}

ag <- .fc_ajustar_cacheaveis(d)
if (length(ls(.tally2))) {
  message("   avisos (benignos) no reajuste final:")
  for (m in ls(.tally2)) message(sprintf("     %2dx  %s", .tally2[[m]], m))
}
message("   modelos pre-ajustados (RIDE-DF, n_treino = ", ag$n_treino, "): ",
        paste(names(ag$modelos), collapse = ", "))

saveRDS(
  list(
    modelos = ag$modelos,          # named: arima/sarima/gam/rf -> objeto de fit
    n_treino = ag$n_treino,        # linhas do corte de treino (ver fc_corte_treino())
    series  = d, geo = geo,
    parquet_fingerprint = fp,
    created_at = Sys.time()
  ),
  file.path(fc_models_dir(), "agg_ride_df.rds")
)
message("   -> ", file.path(fc_models_dir(), "agg_ride_df.rds"))

# ============================== 5b. pre-aquecimento dos recortes (UF/municipio) =
# O Painel segue o recorte geografico hierarquicamente (municipio/UF/RIDE-DF)
# - cada troca de recorte, sem isso, forcaria get_dengue_forecast() a
# reajustar TODOS os modelos `cacheavel` ao vivo (ver README-FORECAST.md
# secao 8). Pre-ajusta aqui, OFFLINE, um fit por recorte que a base
# realmente tem - lista vinda de um DISTINCT no Parquet (escopo dinamico,
# nunca hardcoded, mesma regra do resto do app - ver global.R::GEO_DICT).
# Pesos do ensemble continuam SO da RIDE-DF agregada (secao 2-3 acima); aqui
# so os OBJETOS DE FIT mudam por recorte.
message("\n== 4b. Pre-aquecimento dos recortes (UF / municipio) ==")
rm(list = ls(.tally2), envir = .tally2)   # zera o contador p/ nao somar com o reajuste da RIDE-DF

recortes <- DBI::dbGetQuery(con, "
  SELECT DISTINCT uf_ibge, municipio_geocodigo FROM dengue
  WHERE uf_ibge IS NOT NULL OR municipio_geocodigo IS NOT NULL
")
ufs  <- sort(unique(stats::na.omit(as.integer(recortes$uf_ibge))))
muns <- sort(unique(stats::na.omit(as.integer(recortes$municipio_geocodigo))))

cuts_cache <- list()
ajustar_recorte <- function(loc, rotulo) {
  g <- fc_resolve_geo(loc)
  d_g <- tryCatch(fc_prepare(fc_query_serie(con, g$where)), error = function(e) NULL)
  if (is.null(d_g) || nrow(d_g) < 160L) {
    message("   ", rotulo, ": serie curta demais (", if (is.null(d_g)) 0L else nrow(d_g),
            " semanas) - pulado, get_dengue_forecast() reajusta ao vivo se pedido.")
    return(invisible())
  }
  ag_g <- .fc_ajustar_cacheaveis(d_g)
  cuts_cache[[fc_cache_key(g)]] <<- list(modelos = ag_g$modelos, n_treino = ag_g$n_treino)
  message(sprintf("   %s (n_treino = %d): %s", rotulo, ag_g$n_treino,
                  paste(names(ag_g$modelos), collapse = ", ")))
}

t_recortes <- Sys.time()
for (uf in ufs)   ajustar_recorte(uf,  sprintf("UF %d",        uf))
for (mu in muns)  ajustar_recorte(mu,  sprintf("Municipio %d", mu))
if (length(ls(.tally2))) {
  message("   avisos (benignos) acumulados no pre-aquecimento dos recortes:")
  for (m in ls(.tally2)) message(sprintf("     %2dx  %s", .tally2[[m]], m))
}
message(sprintf("   %d recorte(s) pre-aquecido(s) em %.1f min.",
                length(cuts_cache),
                as.numeric(difftime(Sys.time(), t_recortes, units = "mins"))))

saveRDS(cuts_cache, file.path(fc_models_dir(), "cuts_cache.rds"))
message("   -> ", file.path(fc_models_dir(), "cuts_cache.rds"))

cfg <- list(
  created_at          = Sys.time(),
  parquet_fingerprint = fp,
  training_scope      = geo$rotulo,
  series_range        = range(d$data_ini_se),
  horizon             = FC_HORIZON_DEFAULT,
  fourier_K           = FC_FOURIER_K,
  lag_steps           = FC_LAG,
  pi_level            = FC_PI_LEVEL,
  classes             = FC_CLASSES,
  modelos             = lapply(FC_MODELOS, function(m) list(rotulo = m$rotulo, classe = m$classe)),
  weights             = pesos,                # list(modelo=, classe=)
  rmse                = cons$rmse,            # list(modelo=, classe=)
  seasonal_scale_mae  = cons$escala_sazonal,
  backtest            = list(n_origens = N_ORIGENS, min_treino = MIN_TREINO),
  metrics             = metrics
)
saveRDS(cfg, file.path(fc_models_dir(), "ensemble_config.rds"))
message("   -> ", file.path(fc_models_dir(), "ensemble_config.rds"))

# ===================================== 6. atualiza o README ================
message("\n== 5. Atualizacao do README-FORECAST.md ==")
readme <- file.path(fc_dir(), "README-FORECAST.md")

fmt <- function(x) ifelse(is.na(x), "--", formatC(x, format = "f", digits = 2))
tbl <- metrics[metrics$horizonte == "todos", ]
ordem <- c(names(FC_MODELOS), paste0("ensemble_", names(FC_CLASSES)), "ensemble_global")
tbl <- tbl[order(match(tbl$modelo, ordem)), ]
rotula <- function(k) {
  if (k == "ensemble_global") return("**Ensemble global**")
  if (startsWith(k, "ensemble_")) return(sprintf("*Ensemble %s*", fc_rotulo_classe(sub("^ensemble_", "", k))))
  fc_rotulo_modelo(k)
}

linhas <- c(
  sprintf("_Backtest de origem rolante: %d origens, horizonte %d semanas, janela expansivel (>= %d semanas de treino). Escopo: %s. Atualizado em %s._",
          N_ORIGENS, FC_HORIZON_DEFAULT, MIN_TREINO, geo$rotulo,
          format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "Modelos agrupados por classe; um ensemble por classe (peso = inverso do RMSE dentro da classe) e um ensemble global que combina os ensembles de classe (peso = inverso do RMSE de cada classe).",
  "",
  "| Classe | Modelo | RMSE | MAE | MAPE (%) | sMAPE (%) | MASE | Peso |",
  "|---|---|---:|---:|---:|---:|---:|---:|",
  vapply(seq_len(nrow(tbl)), function(i) sprintf(
    "| %s | %s | %s | %s | %s | %s | %s | %s |",
    tbl$classe[i], rotula(tbl$modelo[i]),
    fmt(tbl$RMSE[i]), fmt(tbl$MAE[i]), fmt(tbl$MAPE[i]),
    fmt(tbl$sMAPE[i]), fmt(tbl$MASE[i]),
    ifelse(is.na(tbl$peso[i]), "--", formatC(tbl$peso[i], format = "f", digits = 3))
  ), character(1)),
  "",
  "> Peso de um modelo = seu peso DENTRO da classe. Peso de um ensemble de classe = seu peso no ensemble global. MAPE só em semanas com casos observados >= 5. MASE < 1 = melhor que o ingênuo sazonal (lag 52)."
)

if (file.exists(readme)) {
  txt <- readLines(readme, encoding = "UTF-8", warn = FALSE)
  ini <- grep("<!-- METRICS:START -->", txt, fixed = TRUE)
  fim <- grep("<!-- METRICS:END -->",   txt, fixed = TRUE)
  if (length(ini) == 1 && length(fim) == 1 && fim > ini) {
    txt <- c(txt[seq_len(ini)], "", linhas, "", txt[seq(fim, length(txt))])
    writeLines(txt, readme, useBytes = TRUE)
    message("   Tabela de metricas atualizada no README.")
  } else {
    message("   Marcadores <!-- METRICS:START/END --> nao encontrados; README nao alterado.")
  }
} else {
  message("   README ausente; execute apos criar forecasting/README-FORECAST.md.")
}

message(sprintf("\n== Concluido em %.1f min ==",
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
