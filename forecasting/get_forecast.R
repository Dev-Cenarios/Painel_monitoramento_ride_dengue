# ==============================================================================
# Dengue RIDE | forecasting/get_forecast.R
#
# Funcao EXPORTAVEL consumida pelo painel Shiny. Le a serie_semanal do recorte
# via DuckDB, aplica o ensemble treinado (forecasting/models/) e devolve um
# data.frame de observado + previsto, respeitando o Ano Sanitario.
#
# Uso no app (server.R), sob demanda e com cache por recorte:
#
#     source("forecasting/get_forecast.R", local = FALSE)
#
#     forecast_recorte <- shiny::reactive({
#       g <- geo()                                   # list(nivel, uf_ibge, geocodigo)
#       loc <- switch(g$nivel,
#         municipio = g$geocodigo,
#         uf        = g$uf_ibge,
#         NULL)                                      # brasil -> RIDE-DF inteira
#       get_dengue_forecast(loc, data_atual = Sys.Date(), con = con)
#     }) |>
#       shiny::bindCache(geo(), Sys.Date())
#
# `con` e a conexao DuckDB ja aberta em global.R: passe-a para evitar abrir
# outra. Sem `con`, a funcao abre e fecha a sua propria.
# ==============================================================================

if (!exists("fc_connect", mode = "function")) {
  .gf_dir <- local({
    tem_utils <- function(d) {
      !is.null(d) && !is.na(d) && nzchar(d) &&
        file.exists(file.path(d, "utils_forecast.R"))
    }
    # 1) diretorio deste proprio arquivo quando sourced (source() define $ofile)
    for (i in rev(seq_len(sys.nframe()))) {
      of <- sys.frame(i)$ofile
      if (!is.null(of)) {
        d <- dirname(normalizePath(of, winslash = "/", mustWork = FALSE))
        if (tem_utils(d)) return(d)
      }
    }
    # 2) --file= (execucao via Rscript deste arquivo)
    a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(a)) {
      d <- dirname(normalizePath(sub("^--file=", "", a[1]), winslash = "/", mustWork = FALSE))
      if (tem_utils(d)) return(d)
    }
    # 3) caminhos relativos usuais a partir do working dir
    cand <- c(file.path(getwd(), "forecasting"),
              file.path(getwd(), "dengue_ride", "forecasting"),
              getwd())
    hit <- cand[vapply(cand, tem_utils, logical(1))]
    if (length(hit)) return(hit[1])
    stop("get_forecast.R: nao encontrei utils_forecast.R. ",
         "Rode a partir de dengue_ride/ ou faca source() do caminho completo.",
         call. = FALSE)
  })
  source(file.path(.gf_dir, "utils_forecast.R"), local = FALSE, encoding = "UTF-8")
}

# ------------------------------------------------------------------------------
#' Previsao de casos de dengue por recorte geografico (ensemble Mosqlimate-like).
#'
#' @param location_id  Recorte. `NULL` / `"RIDE-DF"` / `0` = base inteira;
#'   inteiro de 2 digitos (11..53) = UF (`uf_ibge`); inteiro de 7 digitos =
#'   municipio (`municipio_geocodigo`). Aceita tambem o `list(nivel, uf_ibge,
#'   geocodigo)` de `geo()`.
#' @param data_atual   Teto de data ("nao olhe alem disso"). O campo
#'   `Casos_Observados` usa semanas com `data_ini_se <= data_atual`. O corte
#'   de treino, porem, NAO e contado a partir de `data_atual` - e contado a
#'   partir da ULTIMA SEMANA QUE A BASE REALMENTE TEM (ate esse teto). Essa
#'   base e atualizada em lote: "hoje" pode estar semanas/meses a frente do
#'   ultimo dado publicado, e nesse caso recuar a partir de "hoje" nao
#'   recuaria nada. Ancorando na ultima linha disponivel em vez de em
#'   `data_atual`, a regra abaixo vale SEMPRE, nao só quando a base esta em dia.
#' @param horizon      Semanas a prever. `NULL` (padrao) = projeta ate a SE 26
#'   do fim da temporada da ultima semana disponivel
#'   (`fc_horizon_fim_temporada()`), calculado a partir do corte de treino -
#'   varia a cada chamada, NAO e fixo.
#' @param semanas_recentes  Quantas semanas recentes SEMPRE reprojetar em vez
#'   de tratar como definitivas (a notificacao das ultimas semanas costuma
#'   estar incompleta) - contadas para tras a partir da ULTIMA semana que a
#'   base tem, nunca de `data_atual`. `NULL` (padrao) = `FC_SEMANAS_RECENTES`
#'   (4). Essas semanas saem com dois registros no retorno: `Tipo =
#'   "observado"` (o que foi notificado) e `Tipo = "previsto"` (reestimativa
#'   do ensemble) - da pra comparar os dois.
#' @param n_semanas_hist  Semanas observadas a devolver junto (contexto do
#'   grafico). `NULL` = serie inteira.
#' @param parquet      Caminho do Parquet. `NULL` = resolucao padrao (global.R).
#' @param models_dir   Pasta dos `.rds`. `NULL` = `forecasting/models/`.
#' @param con          Conexao DuckDB existente (opcional).
#'
#' @return `data.frame` ordenado por `Data_SE` com:
#'   `Data_SE` (Date, inicio da SE), `SE` (AAAASS), `Semana_Epi`,
#'   `Ano_Sanitario`, `Temporada` ("2024/2025"), `Tipo` ("observado"/"previsto"),
#'   `Pop`, `Casos_Observados`, `Casos_Nowcast` (casos_est do InfoDengue),
#'   `Casos_Previstos_Ensemble` / `Limite_Inferior` / `Limite_Superior`
#'   (ponto + IP 80% do ENSEMBLE GLOBAL),
#'   `Prev_<chave>` (ponto de cada modelo do registro; `NA` se nao rodou),
#'   `Prev_ens_<classe>` / `Lim_inf_<classe>` / `Lim_sup_<classe>` (ensemble de
#'   cada classe com modelo).
#'   Atributos: `location`, `classes`, `rotulos`, `pesos`, `pesos_efetivos`
#'   (global + por classe), `data_atual`, `horizon`, `modelo_cache`.
#'
#'   Regra do Ano Sanitario: linhas rotuladas por temporada (SE 27 -> SE 26) e
#'   ordenaveis nesse eixo via `Ano_Sanitario` + `Semana_Epi`; a SE 53 e dobrada
#'   nas vizinhas (SE 52 / SE 1) da mesma temporada, como no restante do painel.
get_dengue_forecast <- function(location_id   = NULL,
                                data_atual    = Sys.Date(),
                                horizon       = NULL,
                                semanas_recentes = NULL,
                                n_semanas_hist = 104L,
                                parquet       = NULL,
                                models_dir    = NULL,
                                con           = NULL) {

  models_dir <- models_dir %||% fc_models_dir()
  cfg_path   <- file.path(models_dir, "ensemble_config.rds")
  if (!file.exists(cfg_path)) {
    stop("Modelos nao treinados: ", cfg_path,
         " ausente. Rode `Rscript forecasting/train_models.R`.", call. = FALSE)
  }
  cfg     <- fc_ler_rds_memo(cfg_path)
  weights <- cfg$weights

  # -- conexao ---------------------------------------------------------------
  # `con` passado pode ter caido (RStudio Server re-executa a mesma sessao):
  # nesse caso abrimos a nossa, em vez de estourar em dbGetQuery().
  if (!is.null(con) && !isTRUE(tryCatch(DBI::dbIsValid(con), error = function(e) FALSE))) {
    con <- NULL
  }
  own_con <- is.null(con)
  if (own_con) {
    con <- fc_connect(parquet)
    on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  }

  # -- serie do recorte ----------------------------------------------------
  geo   <- fc_resolve_geo(location_id)
  d_all <- fc_prepare(fc_query_serie(con, geo$where))

  data_atual <- as.Date(data_atual)
  d_obs <- d_all[d_all$data_ini_se <= data_atual, , drop = FALSE]
  if (nrow(d_obs) < 160L) {
    stop(sprintf("Serie observada curta demais para previsao confiavel (%d semanas < 160).",
                 nrow(d_obs)), call. = FALSE)
  }

  # -- corte de treino: SEMPRE recua N semanas a partir da ULTIMA DISPONIVEL --
  # As `semanas_recentes` mais recentes nunca sao tratadas como definitivas
  # (notificacao ainda pode estar incompleta) - os modelos treinam e a
  # previsao comeca a partir desse corte, nao da ultima semana disponivel.
  # Resultado: essas semanas saem com DOIS registros no retorno (observado =
  # o que foi notificado ate agora; previsto = reestimativa do ensemble).
  #
  # O corte e medido a partir da ULTIMA LINHA DE d_obs (a semana mais recente
  # que a base realmente tem, respeitando o teto de data_atual) - NAO a partir
  # de data_atual em si. Esta base e atualizada em lote (nao ao vivo): "hoje"
  # pode estar semanas ou meses a frente do ultimo dado publicado, e nesse
  # caso recuar a partir de "hoje" nao recuaria nada (o dado ja seria "velho"
  # por conta propria, sem qualquer efeito da regra). Ancorando na ultima
  # linha disponivel, a reprojecao das `semanas_recentes` vale SEMPRE,
  # independente da distancia entre data_atual e o dado mais recente.
  n_recentes        <- as.integer(semanas_recentes %||% FC_SEMANAS_RECENTES)
  ultima_disponivel <- d_obs$data_ini_se[nrow(d_obs)]
  d_fit             <- fc_corte_treino(d_obs, n_recentes)   # ver utils_forecast.R

  # -- horizonte -------------------------------------------------------------
  # Padrao (horizon = NULL): conta as semanas do CORTE DE TREINO ate a SE 26
  # do fim da temporada da ULTIMA DISPONIVEL (mesma ancora do corte acima, nao
  # data_atual/"hoje") - garante que o horizonte sempre cobre pelo menos as
  # `semanas_recentes` reprojetadas, mesmo quando o corte cai perto do fim de
  # uma temporada. NAO e um numero fixo de semanas (o horizonte do backtest,
  # cfg$horizon = 4, e so para avaliacao de acuracia).
  h <- as.integer(horizon %||%
    fc_horizon_fim_temporada(d_fit$data_ini_se[nrow(d_fit)], referencia = ultima_disponivel))

  # -- cache dos modelos pre-ajustados --------------------------------------
  # train_models.R pre-ajusta os modelos `cacheavel` no MESMO corte que
  # fc_corte_treino() produziria hoje (ver nota na propria funcao) - tanto
  # para a RIDE-DF inteira (agg_ride_df.rds) quanto para cada UF/municipio da
  # base (cuts_cache.rds, secao "pre-aquecimento dos recortes"). O cache so
  # vale se: (1) o Parquet nao mudou de CONTEUDO desde o treino (bytes+nrow -
  # fc_fingerprint_ok() ignora `mtime` de proposito, ver utils_forecast.R) e
  # (2) o corte de treino ao vivo tem EXATAMENTE o mesmo numero de semanas que
  # o corte usado no treino (`n_treino`) - diverge assim que uma nova semana
  # de dados chega ao Parquet (a janela desliza) ou quando `data_atual`/
  # `semanas_recentes` pedem algo fora do padrao (ex.: previsao "as of" no
  # passado). Fora dessas condicoes cai pro reajuste ao vivo, mais lento mas
  # sempre correto.
  fp_ok <- fc_fingerprint_ok(cfg$parquet_fingerprint, fc_fingerprint(con, parquet))
  cache <- NULL
  if (fp_ok) {
    if (identical(geo$nivel, "brasil")) {
      agg_path <- file.path(models_dir, "agg_ride_df.rds")
      agg <- fc_ler_rds_memo(agg_path)
      if (!is.null(agg) && identical(agg$n_treino %||% NA_integer_, nrow(d_fit))) {
        # compat: formato antigo guardava $m1/$m2; novo guarda $modelos (por chave)
        cache <- agg$modelos %||% list(arima = agg$m1, gam = agg$m2)
      }
    } else {
      cuts_path <- file.path(models_dir, "cuts_cache.rds")
      cuts <- fc_ler_rds_memo(cuts_path)
      entry <- cuts[[fc_cache_key(geo)]]
      if (!is.null(entry) && identical(entry$n_treino, nrow(d_fit))) {
        cache <- entry$modelos
      }
    }
    if (!is.null(cache)) cache <- cache[!vapply(cache, is.null, logical(1))]
  }

  # -- previsao: modelos, ensembles de classe, ensemble global -----------
  res <- fc_prever_todos(d_fit, weights, h = h, cache = cache)
  gl  <- res$global

  vec_ou_na <- function(p, campo) if (is.null(p)) rep(NA_real_, h) else round(p[[campo]], 1)

  # -- montagem: observado + previsto ------------------------------------
  # `fut` comeca no corte de treino (d_fit), NAO na ultima semana disponivel
  # (d_obs) - por isso reprojeta as `semanas_recentes` mais recentes.
  ult   <- d_fit$data_ini_se[nrow(d_fit)]
  fdate <- ult + 7L * seq_len(h)
  fse   <- as.integer(lubridate::epiyear(fdate) * 100L + lubridate::epiweek(fdate))

  base_cols <- data.frame(Data_SE = d_obs$data_ini_se, SE = d_obs$se, Tipo = "observado",
                          Pop = d_obs$pop, stringsAsFactors = FALSE)
  hist <- data.frame(
    base_cols,
    Casos_Observados         = round(d_obs$casos,     1),
    Casos_Nowcast            = round(d_obs$casos_est, 1),
    Casos_Previstos_Ensemble = NA_real_,
    Limite_Inferior          = NA_real_,
    Limite_Superior          = NA_real_,
    stringsAsFactors = FALSE
  )
  fut <- data.frame(
    Data_SE = fdate, SE = fse, Tipo = "previsto", Pop = d_fit$pop[nrow(d_fit)],
    Casos_Observados         = NA_real_,
    Casos_Nowcast            = NA_real_,
    Casos_Previstos_Ensemble = round(gl$mean,  1),
    Limite_Inferior          = round(gl$lower, 1),
    Limite_Superior          = round(gl$upper, 1),
    stringsAsFactors = FALSE
  )

  # colunas por MODELO: Prev_<chave>
  for (k in names(FC_MODELOS)) {
    hist[[paste0("Prev_", k)]] <- NA_real_
    fut[[ paste0("Prev_", k)]] <- vec_ou_na(res$modelos[[k]], "mean")
  }
  # colunas por CLASSE: Prev_ens_<classe> + limites
  for (cl in fc_classes_com_modelo()) {
    ec <- res$classes[[cl]]
    hist[[paste0("Prev_ens_", cl)]] <- NA_real_
    hist[[paste0("Lim_inf_",  cl)]] <- NA_real_
    hist[[paste0("Lim_sup_",  cl)]] <- NA_real_
    fut[[ paste0("Prev_ens_", cl)]] <- vec_ou_na(ec, "mean")
    fut[[ paste0("Lim_inf_",  cl)]] <- vec_ou_na(ec, "lower")
    fut[[ paste0("Lim_sup_",  cl)]] <- vec_ou_na(ec, "upper")
  }

  if (!is.null(n_semanas_hist) && is.finite(n_semanas_hist)) {
    hist <- utils::tail(hist, as.integer(n_semanas_hist))
  }
  out <- rbind(hist, fut)

  # -- rotulos do Ano Sanitario + dobra da SE 53 ------------------------
  out$Semana_Epi     <- out$SE %%  100L
  out$Ano_Calendario <- out$SE %/% 100L
  out$Ano_Sanitario  <- ano_sanitario(out$Ano_Calendario, out$Semana_Epi)
  out$Temporada      <- rotulo_ano_sanitario(out$Ano_Sanitario)

  cols_num <- c("Casos_Observados", "Casos_Nowcast", "Casos_Previstos_Ensemble",
                "Limite_Inferior", "Limite_Superior",
                grep("^(Prev_|Lim_inf_|Lim_sup_)", names(out), value = TRUE))
  out <- fc_dobrar_semana_53(out, colunas = cols_num)

  cols_ini <- c("Data_SE", "SE", "Semana_Epi", "Ano_Sanitario", "Temporada", "Tipo", "Pop",
                "Casos_Observados", "Casos_Nowcast",
                "Casos_Previstos_Ensemble", "Limite_Inferior", "Limite_Superior")
  out <- out[order(out$Data_SE),
             c(cols_ini, setdiff(names(out), c(cols_ini, "Ano_Calendario")))]
  rownames(out) <- NULL

  attr(out, "location")     <- geo$rotulo
  attr(out, "pesos")        <- weights
  attr(out, "pesos_efetivos") <- list(
    global  = gl$pesos,
    classes = lapply(res$classes, `[[`, "pesos")
  )
  attr(out, "classes")      <- FC_CLASSES[fc_classes_com_modelo()]
  attr(out, "rotulos")      <- vapply(names(FC_MODELOS), fc_rotulo_modelo, character(1))
  attr(out, "data_atual")   <- data_atual
  attr(out, "horizon")      <- h
  attr(out, "modelo_cache") <- !is.null(cache) && length(cache) > 0
  out
}
