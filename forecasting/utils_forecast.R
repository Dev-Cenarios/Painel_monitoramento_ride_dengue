# ==============================================================================
# Dengue RIDE | forecasting/utils_forecast.R
#
# Camada compartilhada do pipeline de forecasting (padrao Mosqlimate).
# Sourced tanto por train_models.R (treino/backtest offline) quanto por
# get_forecast.R (funcao consumida pelo Shiny).
#
# NAO depende de global.R: replica de proposito a leitura Parquet -> DuckDB e a
# agregacao de `serie_semanal`, para poder rodar via `Rscript` sem subir o app.
#
# Pacotes: apenas o que ja esta instalado no projeto.
#   forecast  -> M1 (ARIMA + Fourier) e M3 (STL + ETS)
#   mgcv      -> M2 (GAM Binomial Negativa com offset populacional)
#   duckdb/DBI, dplyr, lubridate
# ==============================================================================

# Pacotes OBRIGATORIOS: `forecast` e `mgcv` vem no projeto (`mgcv` e ate
# recomendado do R base). Se faltar algum aqui, quase sempre e a sessao
# apontando para um `.libPaths()` diferente do que roda o app.
local({
  req   <- c("DBI", "duckdb", "dplyr", "forecast", "mgcv", "lubridate")
  falta <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
  if (length(falta)) {
    stop(
      "Pacote(s) obrigatorio(s) ausente(s) nesta sessao: ", paste(falta, collapse = ", "), ".\n",
      "  R em uso : ", R.version.string, "\n",
      "  .libPaths: ", paste(.libPaths(), collapse = "\n             "), "\n",
      "Rode o pipeline com o mesmo R do app (Rscript forecasting/train_models.R\n",
      "a partir de dengue_ride/) ou ajuste .libPaths()/R_LIBS_USER.",
      call. = FALSE
    )
  }
})
suppressWarnings(suppressMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(forecast)
  library(mgcv)
  library(lubridate)
}))

# Pacote OPCIONAL: `ranger` habilita a classe "machine learning" (Random
# Forest). Ausente -> a classe ML fica vazia (como a mecanicista) e o resto do
# pipeline segue normal. Para ativar: install.packages("ranger").
FC_TEM_RANGER <- requireNamespace("ranger", quietly = TRUE) &&
  !nzchar(Sys.getenv("DENGUE_FC_NO_RANGER"))   # forca o caminho sem ranger em teste
if (FC_TEM_RANGER) {
  suppressWarnings(suppressMessages(library(ranger)))
} else {
  message("[forecast] pacote 'ranger' ausente - classe 'machine learning' desativada ",
          "(instale com install.packages(\"ranger\") para ligar o Random Forest).")
}

# ------------------------------------------------------------- parametros -----
# FC_HORIZON_DEFAULT e o horizonte do BACKTEST (avaliacao de acuracia curta,
# 4 semanas - ver train_models.R). NAO e mais o horizonte da previsao ao vivo:
# get_dengue_forecast() projeta por padrao ate a SE 26 do fim da temporada
# corrente (fc_horizon_fim_temporada(), abaixo), que muda a cada chamada
# conforme a ultima semana observada.
FC_HORIZON_DEFAULT <- 4L    # horizonte do BACKTEST (semanas)
FC_FOURIER_K       <- 3L    # harmonicas de Fourier para a sazonalidade anual
FC_LAG             <- 4L    # defasagem dos preditores exogenos do M2 (ver nota)
FC_PI_LEVEL        <- 80L   # nivel do intervalo de predicao (%)
FC_MC_SIM          <- 1500L # simulacoes Monte Carlo para o IP do M2

# Semanas mais recentes SEMPRE reprojetadas em vez de tomadas como definitivas:
# a contagem de `casos` das ultimas semanas tende a estar incompleta (atraso
# de notificacao ainda nao decantou), entao get_dengue_forecast() nunca ancora
# a previsao na ULTIMA semana disponivel - ela recua FC_SEMANAS_RECENTES
# semanas a partir de `data_atual` antes de treinar/projetar. As semanas nesse
# intervalo ficam com os DOIS valores no retorno (Tipo "observado", o que foi
# notificado ate agora, E Tipo "previsto", a reestimativa do ensemble) - da
# pra comparar os dois no grafico.
FC_SEMANAS_RECENTES <- 4L

# ---------------------------------------------------------- ano sanitario -----
# Identico a global.R: a temporada comeca na SE 27 e fecha na SE 26 do ano
# seguinte. Semanas < 27 pertencem a temporada do ano anterior.
SEMANA_CORTE_SANITARIA  <- 27L
NIVEIS_SEMANA_SANITARIA <- c(seq(SEMANA_CORTE_SANITARIA, 52L),
                             seq(1L, SEMANA_CORTE_SANITARIA - 1L))

ano_sanitario <- function(ano, semana) {
  ano - as.integer(semana < SEMANA_CORTE_SANITARIA)
}
rotulo_ano_sanitario <- function(ano_san) sprintf("%d/%d", ano_san, ano_san + 1L)

#' Horizonte (em semanas), contado a partir de `ultima_data`, ate a SE
#' `semana_alvo` (26 por padrao) do fim da temporada de `referencia`.
#'
#' Este e o horizonte OPERACIONAL padrao de get_dengue_forecast(): "projete
#' ate a SE 26 do fim da temporada" nao e um numero fixo de semanas - muda a
#' cada chamada conforme a data e o calendario epidemiologico (anos de 52 ou
#' 53 semanas). Independente do horizonte do BACKTEST (FC_HORIZON_DEFAULT).
#'
#' `ultima_data` e `referencia` sao parametros SEPARADOS de proposito: o corte
#' de treino (`ultima_data`, ver FC_SEMANAS_RECENTES) recua alguumas semanas em
#' relacao a "hoje" (`referencia`, tipicamente `data_atual`), mas a temporada
#' alvo (de onde sai a SE 26) tem que ser a de "hoje" - senao, se o corte de
#' treino cair perto do fim de uma temporada, o horizonte para exatamente ali
#' e nao cobre nem as proprias semanas recentes que deveriam ser reprojetadas
#' (ficariam sem correspondente "previsto"). Por isso `h` sempre e
#' >= semanas(ultima_data, referencia).
#'
#' A temporada de `referencia` e a que comecou na SE 27 mais recente
#' (ano_sanitario); ela termina na SE 26 do ano-calendario SEGUINTE a esse ano
#' sanitario - a SE 26 alvo pode ja ter passado ou nao, o calculo e o mesmo.
fc_horizon_fim_temporada <- function(ultima_data, referencia = ultima_data,
                                     semana_alvo = 26L, max_h = 110L) {
  ultima_data <- as.Date(ultima_data)
  referencia  <- as.Date(referencia)
  epiano_ref <- lubridate::epiyear(referencia)
  episem_ref <- lubridate::epiweek(referencia)
  ano_san  <- ano_sanitario(epiano_ref, episem_ref)
  ano_alvo <- ano_san + 1L
  # Caso de borda: a referencia JA E a SE-alvo do fim da temporada corrente
  # (nada restando pra prever nesta temporada) - mira a temporada SEGUINTE em
  # vez de tentar achar uma data no passado buscando so pra frente.
  if (epiano_ref == ano_alvo && episem_ref == semana_alvo) ano_alvo <- ano_alvo + 1L

  d <- ultima_data
  for (h in seq_len(max_h)) {
    d <- d + 7L
    if (lubridate::epiyear(d) == ano_alvo && lubridate::epiweek(d) == semana_alvo) return(h)
  }
  stop("fc_horizon_fim_temporada: nao encontrei SE ", semana_alvo, "/", ano_alvo,
       " em ", max_h, " semanas a partir de ", format(ultima_data), ".", call. = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Corte de treino: SEMPRE recua `n_recentes` semanas a partir da ULTIMA linha
#' que `d_obs` realmente tem (nao de "hoje" - mesma nota de FC_SEMANAS_RECENTES
#' acima). Extraida aqui para ser a MESMA regra usada tanto no treino offline
#' (train_models.R, ao pre-ajustar os modelos `cacheavel`) quanto na inferencia
#' ao vivo (get_dengue_forecast()) - critico para o cache: um modelo
#' pre-ajustado so serve de cache se foi treinado no MESMO corte que a
#' chamada ao vivo pediria, senao get_dengue_forecast() teria que descartar
#' o cache e reajustar (ver fc_cache_key() e a secao 8 do README-FORECAST.md).
#'
#' @return `d_obs` cortado (`d_fit`). Se ficar curto demais (< 160 semanas),
#'   devolve `d_obs` inteiro (fallback - serie curta demais pra folga).
fc_corte_treino <- function(d_obs, n_recentes = FC_SEMANAS_RECENTES) {
  n_recentes <- as.integer(n_recentes)
  ultima_disponivel <- d_obs$data_ini_se[nrow(d_obs)]
  corte <- ultima_disponivel - 7L * n_recentes
  d_fit <- d_obs[d_obs$data_ini_se <= corte, , drop = FALSE]
  if (nrow(d_fit) < 160L) d_fit <- d_obs
  d_fit
}

# ------------------------------------------------- cache de artefatos .rds ----
# Memoriza o CONTEUDO de um .rds por processo R (chave = caminho + tamanho +
# mtime do arquivo): evita desserializar de novo em toda chamada de
# get_dengue_forecast() na mesma sessao - agg_ride_df.rds/cuts_cache.rds
# crescem conforme mais recortes/modelos entram no registro. Se o arquivo for
# reescrito (retrain sem reiniciar o processo), o tamanho/mtime mudam e a
# proxima leitura pega a versao nova sozinha.
.fc_rds_mem <- new.env(parent = emptyenv())
fc_ler_rds_memo <- function(caminho) {
  if (!file.exists(caminho)) return(NULL)
  fi    <- file.info(caminho)
  chave <- paste(caminho, fi$size, as.numeric(fi$mtime))
  if (is.null(.fc_rds_mem[[chave]])) {
    .fc_rds_mem[[chave]] <- readRDS(caminho)
  }
  .fc_rds_mem[[chave]]
}

# --------------------------------------------------- diretorio deste script ---
# Resolve o proprio caminho quando o arquivo e sourced, para achar models/.
.fc_script_dir <- local({
  ofile <- NULL
  for (i in rev(seq_len(sys.nframe()))) {
    fr <- sys.frame(i)
    if (!is.null(fr$ofile)) { ofile <- fr$ofile; break }
  }
  if (!is.null(ofile)) {
    dirname(normalizePath(ofile, winslash = "/"))
  } else {
    cand <- c(file.path(getwd(), "forecasting"),
              file.path(getwd(), "dengue_ride", "forecasting"),
              getwd())
    hit <- cand[file.exists(file.path(cand, "utils_forecast.R"))]
    if (length(hit)) normalizePath(hit[1], winslash = "/") else normalizePath(getwd(), winslash = "/")
  }
})

fc_dir       <- function() .fc_script_dir
fc_models_dir <- function() file.path(.fc_script_dir, "models")

# ============================================================================
# 1. FONTE DE DADOS  (Parquet lido sob demanda pelo DuckDB)
# ============================================================================

#' Resolve o caminho do Parquet, na mesma ordem de precedencia do global.R.
fc_parquet_path <- function(parquet = NULL) {
  if (!is.null(parquet)) return(normalizePath(parquet, winslash = "/", mustWork = TRUE))

  env <- Sys.getenv("DENGUE_PARQUET", "")
  if (nzchar(env) && file.exists(env)) return(normalizePath(env, winslash = "/"))

  guesses <- c(
    file.path("data", "dengue_ride_df.parquet"),
    file.path("dengue_ride", "data", "dengue_ride_df.parquet"),
    file.path(dirname(.fc_script_dir), "data", "dengue_ride_df.parquet"),
    file.path(dirname(getwd()), "data", "dengue_ride_df.parquet")
  )
  for (g in guesses) if (file.exists(g)) return(normalizePath(g, winslash = "/"))

  stop("Parquet nao encontrado. Defina DENGUE_PARQUET ou passe `parquet=`.", call. = FALSE)
}

#' Conexao DuckDB em memoria com uma VIEW `dengue` sobre o Parquet.
#' Mesma receita de global.R::.dengue_conectar() - nada e carregado na RAM aqui.
fc_connect <- function(parquet = NULL) {
  path <- fc_parquet_path(parquet)
  con  <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", bigint = "numeric")
  try(DBI::dbExecute(con, "SET threads TO 4"), silent = TRUE)
  DBI::dbExecute(con, sprintf(
    "CREATE OR REPLACE VIEW dengue AS SELECT * FROM read_parquet('%s')",
    gsub("'", "''", path, fixed = TRUE)
  ))
  con
}

#' Impressao digital do Parquet - valida o cache dos modelos treinados.
#'
#' `mtime` entra so para registro/depuracao - NAO faz parte da comparacao de
#' validade (ver fc_fingerprint_ok()). Um `mtime` mais recente sem o conteudo
#' ter mudado e comum (sync de nuvem, `git checkout`, copia de arquivo tocam
#' o mtime sem alterar um byte) e NAO deve invalidar o cache.
fc_fingerprint <- function(con, parquet = NULL) {
  path <- fc_parquet_path(parquet)
  fi   <- file.info(path)
  list(
    arquivo = basename(path),
    bytes   = as.numeric(fi$size),
    mtime   = as.numeric(fi$mtime),
    nrow    = as.numeric(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM dengue")$n)
  )
}

#' Compara duas impressoes digitais SO pelo CONTEUDO (bytes + numero de
#' linhas) - de proposito ignora `mtime`. Ver nota em fc_fingerprint().
fc_fingerprint_ok <- function(a, b) {
  isTRUE(tryCatch(
    identical(a$bytes, b$bytes) && identical(a$nrow, b$nrow),
    error = function(e) FALSE
  ))
}

#' Traduz um `location_id` para um filtro SQL, no mesmo espirito de
#' global.R::aplicar_filtro_geo().
#'
#'   NULL / NA / "RIDE-DF" / "brasil" / 0  -> base inteira (RIDE-DF)
#'   inteiro de 2 digitos (11..53)         -> uf_ibge
#'   inteiro de 7 digitos                  -> municipio_geocodigo
#'   list(nivel, uf_ibge, geocodigo)       -> aceita o formato de geo() do app
#'
#' `id` (alem de `nivel`/`where`/`rotulo`) e o geocodigo/uf_ibge resolvido -
#' usado por fc_cache_key() para achar o recorte no cache de modelos
#' pre-ajustados (ver train_models.R secao "pre-aquecimento dos recortes").
fc_resolve_geo <- function(location_id) {
  vazio <- is.null(location_id) ||
    (length(location_id) == 1 && (is.na(location_id) ||
       tolower(trimws(as.character(location_id))) %in%
         c("", "0", "brasil", "ride", "ride-df", "ride_df", "all", "total", "geral")))
  if (vazio) return(list(nivel = "brasil", where = "TRUE", rotulo = "RIDE-DF", id = NA_integer_))

  if (is.list(location_id)) {
    location_id <- switch(location_id$nivel %||% "brasil",
      municipio = location_id$geocodigo,
      uf        = location_id$uf_ibge,
      return(list(nivel = "brasil", where = "TRUE", rotulo = "RIDE-DF", id = NA_integer_))
    )
  }

  x <- suppressWarnings(as.integer(round(as.numeric(location_id))))
  if (is.na(x)) stop("location_id invalido: ", location_id, call. = FALSE)

  if (x >= 1000000L) {
    list(nivel = "municipio",
         where = sprintf("municipio_geocodigo = %d", x),
         rotulo = sprintf("Municipio %d", x), id = x)
  } else if (x >= 11L && x <= 53L) {
    list(nivel = "uf",
         where = sprintf("uf_ibge = %d", x),
         rotulo = sprintf("UF %d", x), id = x)
  } else {
    stop("location_id fora do padrao IBGE (2 ou 7 digitos): ", x, call. = FALSE)
  }
}

#' Chave curta e estavel do recorte, para indexar o cache de modelos
#' pre-ajustados (agg_ride_df.rds guarda so "brasil"; cuts_cache.rds guarda
#' "uf_<cod>" e "mun_<geocodigo>" - ver train_models.R e get_forecast.R).
fc_cache_key <- function(geo) {
  switch(geo$nivel,
    brasil    = "brasil",
    uf        = sprintf("uf_%d", geo$id),
    municipio = sprintf("mun_%d", geo$id),
    NA_character_
  )
}

#' Serie semanal agregada para um recorte - replica em SQL puro a agregacao de
#' global.R::consultar_serie_semanal(). Uma linha por semana epidemiologica.
#'
#' Clima e agregado por MEDIA PONDERADA pela populacao (somar temperatura entre
#' municipios nao faz sentido); casos/pop somam. No nivel municipal a media
#' ponderada colapsa no proprio valor.
fc_query_serie <- function(con, where_sql) {
  sql <- sprintf("
    SELECT
      data_ini_se,
      max(se)                                    AS se,
      sum(casos)                                 AS casos,
      sum(casos_est)                             AS casos_est,
      sum(pop)                                   AS pop,
      sum(tempmed * pop) / nullif(sum(pop), 0)   AS tempmed,
      sum(umidmed * pop) / nullif(sum(pop), 0)   AS umidmed
    FROM dengue
    WHERE %s
    GROUP BY data_ini_se
    ORDER BY data_ini_se", where_sql)

  d <- DBI::dbGetQuery(con, sql)
  d$data_ini_se <- as.Date(d$data_ini_se)
  d$se          <- as.integer(d$se)
  for (col in c("casos", "casos_est", "pop", "tempmed", "umidmed")) {
    d[[col]] <- as.numeric(d[[col]])
  }
  d
}

# ============================================================================
# 2. PREPARO DA SERIE  (grade semanal regular + rotulos + features)
# ============================================================================

.fc_locf <- function(x) {
  if (all(is.na(x))) return(x)
  primeiro <- which(!is.na(x))[1]
  if (primeiro > 1) x[seq_len(primeiro - 1)] <- x[primeiro]
  for (i in which(is.na(x))) x[i] <- x[i - 1]
  x
}
.fc_interp <- function(x) {
  ok <- !is.na(x)
  if (sum(ok) < 2) return(.fc_locf(x))
  idx <- seq_along(x)
  x[!ok] <- stats::approx(idx[ok], x[ok], xout = idx[!ok], rule = 2)$y
  x
}

#' Regulariza a serie numa grade semanal continua e adiciona rotulos temporais.
#'
#' Semanas ausentes recebem casos = 0 (convencao de vigilancia: ausencia de
#' registro na base = ausencia de notificacao). Populacao e clima sao
#' carregados/interpolados. `se` ausente e derivado por lubridate::epiweek/epiyear
#' (calendario MMWR, o mesmo do InfoDengue).
fc_prepare <- function(d) {
  d <- d[!is.na(d$data_ini_se), , drop = FALSE]
  d <- d[order(d$data_ini_se), , drop = FALSE]
  if (!nrow(d)) stop("Serie vazia apos filtro geografico.", call. = FALSE)

  grade <- data.frame(data_ini_se = seq(min(d$data_ini_se), max(d$data_ini_se), by = 7L))
  d <- merge(grade, d, by = "data_ini_se", all.x = TRUE)
  d <- d[order(d$data_ini_se), , drop = FALSE]

  n_gap <- sum(is.na(d$casos))
  if (n_gap > 0) {
    message(sprintf("[forecast] %d semana(s) sem registro preenchida(s) com casos = 0.", n_gap))
  }
  d$casos     <- ifelse(is.na(d$casos), 0, d$casos)
  d$casos_est <- ifelse(is.na(d$casos_est), d$casos, d$casos_est)
  d$pop       <- .fc_locf(d$pop)
  d$tempmed   <- .fc_interp(d$tempmed)
  d$umidmed   <- .fc_interp(d$umidmed)

  epiano <- lubridate::epiyear(d$data_ini_se)
  episem <- lubridate::epiweek(d$data_ini_se)
  d$se     <- ifelse(is.na(d$se), epiano * 100L + episem, d$se)
  d$ano    <- d$se %/% 100L
  d$semana <- d$se %%  100L
  d$ano_san <- ano_sanitario(d$ano, d$semana)
  d$t <- seq_len(nrow(d))                 # indice global continuo (trend do M2)
  rownames(d) <- NULL
  d
}

#' Defasagens dos preditores exogenos do M2.
#'
#' A defasagem e FC_LAG (= horizonte). Assim, prever h = 1..4 semanas nao exige
#' extrapolar clima nem casos: os valores defasados de 4 semanas ja sao todos
#' observados. Para h > FC_LAG o ultimo valor observado e repetido (clamp).
fc_features <- function(d, lag = FC_LAG) {
  d$casos_l   <- dplyr::lag(d$casos,   lag)
  d$tempmed_l <- dplyr::lag(d$tempmed, lag)
  d$umidmed_l <- dplyr::lag(d$umidmed, lag)
  d
}

# ============================================================================
# 3. MODELOS BASE  (horizonte h, saida sempre >= 0, IP de FC_PI_LEVEL%)
# ============================================================================

.fc_ts   <- function(y) stats::ts(y, frequency = 52L)
.fc_clamp <- function(x) pmax(as.numeric(x), 0)

# ---- M1: ARIMA nao-sazonal + termos de Fourier (sazonalidade anual) ---------
fit_fc_m1 <- function(d, K = FC_FOURIER_K) {
  y    <- .fc_ts(d$casos)
  xreg <- forecast::fourier(y, K = K)
  lam  <- if (min(d$casos, na.rm = TRUE) > 0) "auto" else NULL  # Box-Cox so sem zeros
  fit  <- forecast::auto.arima(
    y, xreg = xreg, seasonal = FALSE, lambda = lam,
    stepwise = TRUE, approximation = TRUE
  )
  structure(list(fit = fit, K = K), class = "fc_m1")
}
predict_fc_m1 <- function(obj, d, h = FC_HORIZON_DEFAULT, level = FC_PI_LEVEL) {
  y  <- .fc_ts(d$casos)
  xf <- forecast::fourier(y, K = obj$K, h = h)
  f  <- forecast::forecast(obj$fit, xreg = xf, h = h, level = level)
  list(mean = .fc_clamp(f$mean), lower = .fc_clamp(f$lower[, 1]), upper = .fc_clamp(f$upper[, 1]))
}

# ---- M1b: SARIMA  (ARIMA sazonal "de verdade", periodo 52) -----------------
# Diferente do M1: aqui a sazonalidade anual NAO entra como xreg de Fourier -
# entra pela propria estrutura ARIMA, com diferenca sazonal (lag 52) e termos
# AR/MA sazonais. auto.arima busca livremente a parte nao-sazonal (p, d, q); a
# parte sazonal fica limitada a P, Q <= 1 (com frequencia 52 cada termo
# sazonal e caro de estimar) e a diferenca sazonal D e FORCADA a 1 quando ha
# pelo menos 2 estacoes completas - e isso que garante um SARIMA de fato, sem
# o auto.arima cair de volta num ARIMA simples (D = 0). Com < 2 estacoes,
# deixa D a criterio do auto.arima. Complementa o M1: os dois capturam o ciclo
# anual por caminhos distintos e o ensemble aproveita a diferenca.
fit_fc_sarima <- function(d) {
  y   <- .fc_ts(d$casos)
  lam <- if (min(d$casos, na.rm = TRUE) > 0) "auto" else NULL   # Box-Cox so sem zeros
  duas_estacoes <- nrow(d) >= 2L * stats::frequency(y)

  # method = "CSS": o reajuste final do auto.arima usa por padrao "CSS-ML"
  # (otimizacao via filtro de Kalman, stats::arima -> upARIMA). Com sazonalidade
  # de verdade em periodo 52 essa etapa pode travar (loop de otimizacao muito
  # caro/instavel no upARIMA - bug conhecido do stats::arima para periodos
  # longos), nao so ficar lenta - trava a sessao Shiny inteira (single-thread).
  # CSS evita o filtro de Kalman por completo. setTimeLimit() e rede de
  # seguranca extra: se mesmo assim demorar demais, vira erro (capturado pelo
  # tryCatch de quem chama fit()), em vez de travar o processo.
  setTimeLimit(elapsed = 120, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf, transient = TRUE), add = TRUE)

  fit <- forecast::auto.arima(
    y, seasonal = TRUE, lambda = lam, method = "CSS",
    D = if (duas_estacoes) 1L else NA,
    max.p = 3L, max.q = 3L, max.d = 2L,
    max.P = 1L, max.Q = 1L, max.D = 1L,
    stepwise = TRUE, approximation = TRUE
  )
  structure(list(fit = fit), class = "fc_sarima")
}
predict_fc_sarima <- function(obj, d, h = FC_HORIZON_DEFAULT, level = FC_PI_LEVEL) {
  f <- forecast::forecast(obj$fit, h = h, level = level)
  list(mean  = .fc_clamp(f$mean),
       lower = .fc_clamp(f$lower[, 1]),
       upper = .fc_clamp(f$upper[, 1]))
}

# ---- M2: GAM Binomial Negativa, offset populacional -------------------------
# casos ~ sazonalidade ciclica(SE) + tendencia lenta(t) + clima e casos
#         defasados(4 sem) + offset(log pop),  family = nb()
fit_fc_m2 <- function(d, lag = FC_LAG) {
  df <- fc_features(d, lag)
  req <- c("casos", "pop", "semana", "t", "tempmed_l", "umidmed_l", "casos_l")
  df <- df[stats::complete.cases(df[, req]) & df$pop > 0, , drop = FALSE]
  if (nrow(df) < 104) stop("Serie curta demais para o M2 (GAM): ", nrow(df), " semanas.", call. = FALSE)

  k_sem <- min(20L, max(5L, length(unique(df$semana)) - 1L))
  k_c   <- min(10L, max(4L, length(unique(df$casos_l)) - 1L))
  fit <- mgcv::gam(
    casos ~ s(semana, bs = "cc", k = k_sem) +
            s(t,        bs = "cr", k = 8) +
            s(tempmed_l, k = 5) +
            s(umidmed_l, k = 5) +
            s(casos_l,   bs = "cr", k = k_c) +
            offset(log(pop)),
    family = mgcv::nb(), data = df, method = "REML",
    knots  = list(semana = c(0.5, 53.5))
  )
  structure(list(fit = fit, lag = lag), class = "fc_m2")
}
predict_fc_m2 <- function(obj, d, h = FC_HORIZON_DEFAULT, level = FC_PI_LEVEL, nsim = FC_MC_SIM) {
  fit <- obj$fit; lag <- obj$lag
  n   <- nrow(d)
  fdate <- d$data_ini_se[n] + 7L * seq_len(h)
  li  <- pmax(pmin(n + seq_len(h) - lag, n), 1L)   # indice do valor defasado

  nd <- data.frame(
    semana    = as.integer(lubridate::epiweek(fdate)),
    t         = n + seq_len(h),
    pop       = d$pop[n],
    tempmed_l = d$tempmed[li],
    umidmed_l = d$umidmed[li],
    casos_l   = d$casos[li]
  )
  pr  <- predict(fit, newdata = nd, type = "link", se.fit = TRUE)  # inclui o offset
  th  <- fit$family$getTheta(TRUE)                                  # theta da NB

  eta <- matrix(stats::rnorm(nsim * h, rep(pr$fit, each = nsim), rep(pr$se.fit, each = nsim)),
                nrow = nsim)
  sim <- matrix(stats::rnbinom(nsim * h, mu = exp(eta), size = th), nrow = nsim)
  a   <- (1 - level / 100) / 2
  list(
    mean  = .fc_clamp(colMeans(sim)),
    lower = .fc_clamp(apply(sim, 2, stats::quantile, probs = a,     names = FALSE)),
    upper = .fc_clamp(apply(sim, 2, stats::quantile, probs = 1 - a, names = FALSE))
  )
}

# ---- M3: STL + ETS  (analogo de Holt-Winters para frequencia 52) ------------
# ets() puro nao aceita periodo > 24; stlf() decompoe via STL e aplica ETS na
# serie sazonalmente ajustada, reprojetando a sazonalidade ao final.
fit_fc_m3 <- function(d) structure(list(), class = "fc_m3")
predict_fc_m3 <- function(obj, d, h = FC_HORIZON_DEFAULT, level = FC_PI_LEVEL) {
  y <- .fc_ts(d$casos)
  f <- forecast::stlf(y, h = h, level = level, method = "ets",
                      s.window = "periodic", robust = TRUE)
  list(mean = .fc_clamp(f$mean), lower = .fc_clamp(f$lower[, 1]), upper = .fc_clamp(f$upper[, 1]))
}

# ---- M4: Random Forest (ranger), regressao quantilica ----------------------
# Mesmas features do GAM (defasagem 4 = horizonte, entao nada e extrapolado),
# mais `inc_l` (casos defasados em taxa /100 mil) para o modelo generalizar
# entre recortes de populacao diferente. IP via quantis do proprio ranger
# (quantreg = TRUE) - nao ha suposicao parametrica.
fit_fc_rf <- function(d, lag = FC_LAG) {
  df  <- fc_features(d, lag)
  req <- c("casos", "pop", "semana", "t", "tempmed_l", "umidmed_l", "casos_l")
  df  <- df[stats::complete.cases(df[, req]) & df$pop > 0, , drop = FALSE]
  if (nrow(df) < 104) stop("Serie curta demais para o RF: ", nrow(df), " semanas.", call. = FALSE)
  df$inc_l <- df$casos_l / df$pop * 1e5
  fit <- ranger::ranger(
    casos ~ semana + t + pop + tempmed_l + umidmed_l + casos_l + inc_l,
    data = df, num.trees = 500L, quantreg = TRUE,
    min.node.size = 5L, seed = 20260903L
  )
  structure(list(fit = fit, lag = lag), class = "fc_rf")
}
predict_fc_rf <- function(obj, d, h = FC_HORIZON_DEFAULT, level = FC_PI_LEVEL) {
  fit <- obj$fit; lag <- obj$lag; n <- nrow(d)
  fdate <- d$data_ini_se[n] + 7L * seq_len(h)
  li    <- pmax(pmin(n + seq_len(h) - lag, n), 1L)
  nd <- data.frame(
    semana    = as.integer(lubridate::epiweek(fdate)),
    t         = n + seq_len(h),
    pop       = d$pop[n],
    tempmed_l = d$tempmed[li],
    umidmed_l = d$umidmed[li],
    casos_l   = d$casos[li]
  )
  nd$inc_l <- nd$casos_l / nd$pop * 1e5
  a  <- (1 - level / 100) / 2
  qp <- predict(fit, data = nd, type = "quantiles", quantiles = c(a, 1 - a))$predictions
  mn <- predict(fit, data = nd, type = "response")$predictions
  list(mean = .fc_clamp(mn), lower = .fc_clamp(qp[, 1]), upper = .fc_clamp(qp[, 2]))
}

# ============================================================================
# 3b. REGISTRO DE MODELOS E CLASSES
# ============================================================================
# Para adicionar um modelo: escreva fit_*/predict_* (contrato: predict devolve
# list(mean, lower, upper) de comprimento h, tudo >= 0) e registre uma entrada
# aqui com a `classe`. O backtest, o ensemble por classe, o ensemble global, a
# saida de get_dengue_forecast() e os checkboxes do painel se ajustam sozinhos.

FC_CLASSES <- c(
  estatistico = "Estatísticos",
  ml          = "Machine Learning",
  mecanicista = "Mecanicistas"          # sem modelo ainda - estrutura pronta
)

FC_MODELOS <- list(
  arima = list(rotulo = "ARIMA + Fourier",     classe = "estatistico",
               fit = fit_fc_m1, predict = predict_fc_m1, cacheavel = TRUE),
  sarima = list(rotulo = "SARIMA",             classe = "estatistico",
               fit = fit_fc_sarima, predict = predict_fc_sarima, cacheavel = TRUE),
  gam   = list(rotulo = "GAM (Bin. Negativa)", classe = "estatistico",
               fit = fit_fc_m2, predict = predict_fc_m2, cacheavel = TRUE),
  stlets = list(rotulo = "STL + ETS",          classe = "estatistico",
               fit = fit_fc_m3, predict = predict_fc_m3, cacheavel = FALSE)
)
# Random Forest so entra no registro se o `ranger` estiver disponivel.
if (isTRUE(FC_TEM_RANGER)) {
  FC_MODELOS$rf <- list(rotulo = "Random Forest", classe = "ml",
                        fit = fit_fc_rf, predict = predict_fc_rf, cacheavel = TRUE)
}

fc_modelos_da_classe   <- function(cl) names(Filter(function(m) identical(m$classe, cl), FC_MODELOS))
fc_classes_com_modelo  <- function() {
  Filter(function(cl) length(fc_modelos_da_classe(cl)) > 0, names(FC_CLASSES))
}
fc_classe_do_modelo    <- function(k) vapply(k, function(x) FC_MODELOS[[x]]$classe %||% NA_character_,
                                             character(1), USE.NAMES = FALSE)
fc_rotulo_modelo       <- function(k) vapply(k, function(x) {
  r <- FC_MODELOS[[x]]$rotulo; if (is.null(r)) x else r
}, character(1), USE.NAMES = FALSE)
fc_rotulo_classe       <- function(cl) {
  r <- FC_CLASSES[cl]; if (is.na(r)) cl else unname(r)
}

# ============================================================================
# 4. ENSEMBLE  (media ponderada pelo inverso do RMSE historico)
#    Dois niveis: um ensemble por CLASSE, e um ensemble GLOBAL que combina os
#    ensembles de classe. Pesos sempre pelo inverso do RMSE (estavel mesmo com
#    semanas de ~0 casos, ao contrario do inverso do MAPE).
# ============================================================================

#' Pesos w_i = (1/RMSE_i) / sum_j (1/RMSE_j).
fc_pesos_inv_rmse <- function(rmse) {
  inv <- 1 / rmse
  inv / sum(inv)
}

#' Combina previsoes (media ponderada de mean/lower/upper). Renormaliza os pesos
#' sobre os componentes efetivamente disponiveis. Serve tanto para combinar
#' modelos dentro de uma classe quanto para combinar ensembles de classe.
fc_ensemble <- function(preds, weights) {
  w <- weights[names(preds)]
  w[is.na(w)] <- 0
  if (sum(w) <= 0) w <- rep(1, length(preds))   # fallback: media simples
  w <- w / sum(w)
  comb <- function(campo) Reduce(`+`, Map(function(p, wi) wi * p[[campo]], preds, w))
  list(mean = comb("mean"), lower = comb("lower"), upper = comb("upper"), pesos = w)
}

#' Ajusta todos os modelos do registro e monta os ensembles.
#'
#' @param pesos  list(modelo = <vetor nomeado, pesos DENTRO da classe>,
#'                     classe = <vetor nomeado, pesos das classes no global>).
#' @param cache  list opcional com fits ja prontos por chave de modelo.
#' @return list(modelos = <list por modelo>, classes = <list por classe>,
#'              global = <ensemble global>).
fc_prever_todos <- function(d_obs, pesos, h = FC_HORIZON_DEFAULT, cache = NULL) {
  P <- list()
  for (k in names(FC_MODELOS)) {
    spec <- FC_MODELOS[[k]]
    fitk <- if (!is.null(cache[[k]])) cache[[k]] else tryCatch(
      spec$fit(d_obs),
      error = function(e) { message("[forecast] ", k, " (ajuste) falhou: ", conditionMessage(e)); NULL }
    )
    if (is.null(fitk)) next
    P[[k]] <- tryCatch(
      spec$predict(fitk, d_obs, h),
      error = function(e) { message("[forecast] ", k, " (previsao) falhou: ", conditionMessage(e)); NULL }
    )
  }
  P <- P[!vapply(P, is.null, logical(1))]
  if (!length(P)) stop("Todos os modelos falharam para este recorte.", call. = FALSE)

  ENSC <- list()
  for (cl in fc_classes_com_modelo()) {
    mk <- intersect(fc_modelos_da_classe(cl), names(P))
    if (length(mk)) ENSC[[cl]] <- fc_ensemble(P[mk], pesos$modelo %||% stats::setNames(rep(1, length(mk)), mk))
  }
  if (!length(ENSC)) stop("Nenhum ensemble de classe disponivel.", call. = FALSE)

  GL <- fc_ensemble(ENSC, pesos$classe %||% stats::setNames(rep(1, length(ENSC)), names(ENSC)))

  list(modelos = P, classes = ENSC, global = GL)
}

# ============================================================================
# 5. METRICAS  (MAPE guardado, RMSE, MAE + sMAPE / MASE de apoio)
# ============================================================================

#' Metricas de erro entre observado e previsto.
#' MAPE so usa semanas com obs >= `mape_floor` (dengue tem semanas de ~0 casos
#' na entressafra, onde o erro percentual explode). MASE usa a escala do
#' ingenuo sazonal (diferenca de 52 semanas) informada em `scale_mae`.
fc_metricas <- function(obs, pred, scale_mae = NA_real_, mape_floor = 5) {
  obs <- as.numeric(obs); pred <- as.numeric(pred)
  e   <- obs - pred
  idx <- is.finite(obs) & is.finite(pred)
  obs <- obs[idx]; pred <- pred[idx]; e <- e[idx]

  usar_mape <- obs >= mape_floor
  out <- c(
    RMSE   = sqrt(mean(e^2)),
    MAE    = mean(abs(e)),
    MAPE   = if (any(usar_mape)) mean(abs(e[usar_mape] / obs[usar_mape])) * 100 else NA_real_,
    sMAPE  = mean(2 * abs(e) / pmax(abs(obs) + abs(pred), .Machine$double.eps)) * 100,
    MASE   = if (is.finite(scale_mae) && scale_mae > 0) mean(abs(e)) / scale_mae else NA_real_,
    n      = length(obs),
    n_mape = sum(usar_mape)
  )
  as.list(out)
}

#' Escala do MASE: MAE do ingenuo sazonal (lag 52) sobre a serie de treino.
fc_escala_sazonal <- function(y, periodo = 52L) {
  mean(abs(diff(as.numeric(y), lag = periodo)), na.rm = TRUE)
}

# ============================================================================
# 6. BACKTEST  (origem rolante, janela expansivel, h passos a frente)
# ============================================================================

#' Para cada origem `o`, treina em 1:o e preve o+1..o+h, para TODOS os modelos
#' do registro FC_MODELOS. Devolve um data.frame longo: origem, h, modelo,
#' classe, obs, pred, lower, upper.
fc_backtest <- function(d, h = FC_HORIZON_DEFAULT, n_origens = 24L, min_treino = 156L,
                        verbose = TRUE) {
  n <- nrow(d)
  primeira <- max(min_treino, n - h - n_origens + 1L)
  origens  <- seq.int(primeira, n - h)
  if (length(origens) < 3L) stop("Serie curta demais para o backtest.", call. = FALSE)

  linhas <- vector("list", length(origens))
  for (k in seq_along(origens)) {
    o  <- origens[k]
    tr <- d[seq_len(o), , drop = FALSE]
    te <- d[o + seq_len(h), , drop = FALSE]

    pr <- lapply(names(FC_MODELOS), function(mk) {
      spec <- FC_MODELOS[[mk]]
      tryCatch(spec$predict(spec$fit(tr), tr, h), error = function(e) NULL)
    })
    names(pr) <- names(FC_MODELOS)
    ok <- names(pr)[!vapply(pr, is.null, logical(1))]

    if (length(ok)) {
      linhas[[k]] <- do.call(rbind, lapply(ok, function(m) data.frame(
        origem = o, h = seq_len(h), modelo = m, classe = fc_classe_do_modelo(m),
        obs = te$casos, pred = pr[[m]]$mean, lower = pr[[m]]$lower, upper = pr[[m]]$upper
      )))
    }
    if (verbose) message(sprintf("[backtest] origem %d/%d (n_treino = %d)  ok: %s",
                                 k, length(origens), o, paste(ok, collapse = ", ")))
  }
  bt <- do.call(rbind, linhas)
  rownames(bt) <- NULL
  bt
}

#' Metricas "todos" + por horizonte para um conjunto (origem,h,obs,pred[,lower,upper]).
.fc_met_bloco <- function(df, rotulo_modelo, classe, escala) {
  g <- data.frame(modelo = rotulo_modelo, classe = classe, horizonte = "todos",
                  as.data.frame(fc_metricas(df$obs, df$pred, escala)))
  ph <- do.call(rbind, lapply(sort(unique(df$h)), function(hh) {
    x <- df[df$h == hh, ]
    data.frame(modelo = rotulo_modelo, classe = classe, horizonte = sprintf("h%d", hh),
               as.data.frame(fc_metricas(x$obs, x$pred, escala)))
  }))
  rbind(g, ph)
}

#' Consolida o backtest em tres niveis:
#'   1. cada modelo                -> metricas + peso DENTRO da sua classe
#'   2. ensemble de cada classe    -> metricas + peso no ensemble GLOBAL
#'   3. ensemble global            -> metricas
#'
#' @return list(metrics, pesos = list(modelo=, classe=),
#'              rmse = list(modelo=, classe=), escala_sazonal=)
fc_consolidar_backtest <- function(bt, d, h = FC_HORIZON_DEFAULT) {
  escala  <- fc_escala_sazonal(d$casos, 52L)
  modelos <- intersect(names(FC_MODELOS), unique(bt$modelo))   # ordem do registro

  # -- 1. metricas + RMSE por modelo -----------------------------------------
  met_modelo <- do.call(rbind, lapply(modelos, function(m) {
    .fc_met_bloco(bt[bt$modelo == m, ], m, fc_classe_do_modelo(m), escala)
  }))
  rmse_modelo <- vapply(modelos, function(m) {
    b <- bt[bt$modelo == m & bt$h <= h, ]
    sqrt(mean((b$obs - b$pred)^2))
  }, numeric(1))

  # -- pesos DENTRO de cada classe (somam 1 por classe) --------------------
  pesos_modelo <- stats::setNames(rep(NA_real_, length(modelos)), modelos)
  for (cl in unique(fc_classe_do_modelo(modelos))) {
    mk <- modelos[fc_classe_do_modelo(modelos) == cl]
    pesos_modelo[mk] <- fc_pesos_inv_rmse(rmse_modelo[mk])
  }

  # -- 2. ensemble de cada classe reconstruido sobre a grade do backtest ----
  bt$w  <- pesos_modelo[bt$modelo]
  ens_cl <- bt |>
    dplyr::group_by(origem, h, classe, obs) |>
    dplyr::summarise(
      pred  = sum(w * pred,  na.rm = TRUE) / sum(w, na.rm = TRUE),
      lower = sum(w * lower, na.rm = TRUE) / sum(w, na.rm = TRUE),
      upper = sum(w * upper, na.rm = TRUE) / sum(w, na.rm = TRUE),
      .groups = "drop"
    ) |>
    as.data.frame()

  classes <- intersect(names(FC_CLASSES), unique(ens_cl$classe))
  met_classe <- do.call(rbind, lapply(classes, function(cl) {
    .fc_met_bloco(ens_cl[ens_cl$classe == cl, ], paste0("ensemble_", cl), cl, escala)
  }))
  rmse_classe <- vapply(classes, function(cl) {
    x <- ens_cl[ens_cl$classe == cl & ens_cl$h <= h, ]
    sqrt(mean((x$obs - x$pred)^2))
  }, numeric(1))
  pesos_classe <- fc_pesos_inv_rmse(rmse_classe)

  # -- 3. ensemble global = combinacao dos ensembles de classe -------------
  ens_cl$wc <- pesos_classe[ens_cl$classe]
  ens_gl <- ens_cl |>
    dplyr::group_by(origem, h, obs) |>
    dplyr::summarise(
      pred  = sum(wc * pred,  na.rm = TRUE) / sum(wc, na.rm = TRUE),
      lower = sum(wc * lower, na.rm = TRUE) / sum(wc, na.rm = TRUE),
      upper = sum(wc * upper, na.rm = TRUE) / sum(wc, na.rm = TRUE),
      .groups = "drop"
    ) |>
    as.data.frame()
  met_global <- .fc_met_bloco(ens_gl, "ensemble_global", "global", escala)

  # -- montagem final -----------------------------------------------------
  metrics <- rbind(met_modelo, met_classe, met_global)
  metrics$peso <- NA_real_
  im <- metrics$modelo %in% modelos & metrics$horizonte == "todos"
  metrics$peso[im] <- round(pesos_modelo[metrics$modelo[im]], 4)
  ic <- metrics$modelo %in% paste0("ensemble_", classes) & metrics$horizonte == "todos"
  metrics$peso[ic] <- round(pesos_classe[sub("^ensemble_", "", metrics$modelo[ic])], 4)
  rownames(metrics) <- NULL

  list(
    metrics = metrics,
    pesos   = list(modelo = pesos_modelo, classe = pesos_classe),
    rmse    = list(modelo = rmse_modelo,  classe = rmse_classe),
    escala_sazonal = escala
  )
}

# ============================================================================
# 7. SAIDA  (ano sanitario: rotulos + dobra da SE 53)
# ============================================================================

#' Dobra a SE 53 nas semanas vizinhas da MESMA temporada e do MESMO tipo
#' (observado/previsto), como global.R::dobrar_semana_53(). Metade dos valores
#' vai para a SE 52, metade para a SE 1.
fc_dobrar_semana_53 <- function(d, colunas) {
  com53 <- d[d$Semana_Epi == 53L, , drop = FALSE]
  if (!nrow(com53)) return(d)
  d <- d[d$Semana_Epi != 53L, , drop = FALSE]

  for (i in seq_len(nrow(com53))) {
    l <- com53[i, ]
    i52 <- which(d$Ano_Sanitario == l$Ano_Sanitario & d$Semana_Epi == 52L & d$Tipo == l$Tipo)
    i1  <- which(d$Ano_Sanitario == l$Ano_Sanitario & d$Semana_Epi ==  1L & d$Tipo == l$Tipo)
    if (!length(i52) && !length(i1)) { d <- rbind(d, l); next }   # vizinhas ausentes: mantem
    for (col in colunas) {
      m <- l[[col]] / 2
      if (is.na(m)) next
      if (length(i52)) d[[col]][i52] <- sum(d[[col]][i52], m, na.rm = TRUE)
      if (length(i1))  d[[col]][i1]  <- sum(d[[col]][i1],  m, na.rm = TRUE)
    }
  }
  d
}
