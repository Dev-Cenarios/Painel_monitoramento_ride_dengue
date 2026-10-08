# ==============================================================================
# Dengue RIDE | prep/02_municipios_sem_historico_rt.R
#
# Diagnostico do mapa "Tendencia por municipio" (R/mod_mapa_rt.R): lista os
# municipios que caem no quadrante "sem_base" (rotulo no grafico: "Sem
# historico suficiente") e exporta o historico semanal completo deles, pra
# investigar POR QUE falta historico (municipio novo na base, buraco de
# notificacao, populacao ausente, etc.).
#
# Um municipio cai em "sem_base" (ver calcular_quadro_tendencia() em
# R/mod_mapa_rt.R) quando, na janela de comparacao escolhida:
#   - a janela ANTERIOR (mesmo ano) nao tem nenhum caso (denominador do eixo Y), OU
#   - a mediana historica (mesmas semanas-do-ano, outros anos) e NA ou zero, OU
#   - tem menos de MIN_ANOS_HISTORICO_RT (2) anos contribuindo pra essa mediana.
#
# Nao depende do app rodando - conecta direto no Parquet via DuckDB. So usa
# calcular_quadro_tendencia() de R/mod_mapa_rt.R (funcao pura, sem efeito
# colateral de UI).
#
# Uso:  cd dengue_ride; Rscript prep/02_municipios_sem_historico_rt.R [janela]
#       janela (opcional) = tamanho da janela de comparacao em semanas (1-8,
#       igual ao slider do mapa). Padrao: 4 (o default do app).
#
# Saida:
#   prep/out/municipios_sem_historico_rt.csv   lista + motivo (diagnostico)
#   prep/out/historico_sem_historico_rt.csv    serie semanal completa desses
#                                               municipios (todas as SE/anos)
# ==============================================================================

suppressWarnings(suppressMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
}))

arg1   <- commandArgs(trailingOnly = TRUE)[1]
janela <- suppressWarnings(as.integer(arg1))
if (is.na(janela)) janela <- 4L

# Reaproveita SOMENTE a funcao de calculo (pura) do modulo do mapa - nao
# carrega Shiny nem o resto do app. R/mod_mapa_rt.R usa TONS_AZUL, TONS_DENGUE
# e PALETA_DENGUE (de global.R) so pra definir cores de UI no topo do arquivo
# (CORES_QUADRANTE); um stub basta pra esse source nao quebrar - as cores nao
# sao usadas aqui.
if (!exists("PALETA_DENGUE")) {
  TONS_AZUL     <- c(`400` = "#4FA4D3", `700` = "#00638C")
  TONS_DENGUE   <- c(`400` = "#D97F55", `700` = "#853B15")
  PALETA_DENGUE <- c(sub = "#7A8B95", tinta = "#12222B", superficie = "#FFFFFF",
                     linha = "#D8E3EA")
}
source("R/mod_mapa_rt.R", local = FALSE, encoding = "UTF-8")

parquet <- Sys.getenv("DENGUE_PARQUET", file.path("data", "dengue_ride_df.parquet"))
if (!file.exists(parquet)) {
  stop("Parquet nao encontrado em '", parquet, "'. Rode a partir de dengue_ride/ ",
       "ou defina DENGUE_PARQUET.", call. = FALSE)
}

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", bigint = "numeric")
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
caminho <- gsub("'", "''", normalizePath(parquet, winslash = "/"), fixed = TRUE)
invisible(DBI::dbExecute(con, sprintf(
  "CREATE OR REPLACE VIEW dengue AS SELECT * FROM read_parquet('%s')", caminho
)))

message(sprintf("[diagnostico] Parquet: %s | janela de comparacao: %d semana(s)", parquet, janela))

# ============================================================ 1. serie base ==
# Mesma agregacao de SERIE_MUNICIPIOS (global.R): uma linha por
# municipio/semana, casos somados e pop/rt como max (constantes na semana).
serie_mun <- DBI::dbGetQuery(con, "
  SELECT
    municipio_geocodigo,
    max(municipio_nome) AS municipio_nome,
    data_ini_se,
    max(se)             AS se,
    sum(casos)          AS casos,
    sum(casos_est)      AS casos_est,
    max(pop)            AS pop,
    max(rt)             AS rt
  FROM dengue
  GROUP BY municipio_geocodigo, data_ini_se
  ORDER BY municipio_geocodigo, data_ini_se
")
serie_mun <- serie_mun |>
  mutate(
    municipio_geocodigo = as.integer(municipio_geocodigo),
    data_ini_se          = as.Date(data_ini_se),
    se                    = as.integer(se),
    ano                   = se %/% 100L,
    semana                = se %%  100L,
    casos                 = as.numeric(casos),
    casos_est             = as.numeric(casos_est),
    pop                   = as.numeric(pop),
    rt                    = as.numeric(rt)
  )

message(sprintf("[diagnostico] %d linhas / %d municipio(s) / %d a %d",
                nrow(serie_mun), n_distinct(serie_mun$municipio_geocodigo),
                min(serie_mun$ano), max(serie_mun$ano)))

# ================================================== 2. quadro de tendencia ===
quadro <- calcular_quadro_tendencia(serie_mun, janela = janela)
d <- quadro$dados

sem_historico <- d |>
  filter(quadrante == "sem_base") |>
  mutate(
    motivo = case_when(
      coalesce(casos_anterior, 0) == 0                         ~ "sem casos na janela anterior",
      is.na(mediana_historica) | mediana_historica == 0         ~ "sem mediana historica (0 casos nas semanas de referencia em anos passados)",
      coalesce(n_anos_historico, 0L) < MIN_ANOS_HISTORICO_RT    ~ sprintf("so %d ano(s) de historico (precisa >= %d)",
                                                                          coalesce(n_anos_historico, 0L), MIN_ANOS_HISTORICO_RT),
      TRUE                                                       ~ "outro"
    )
  ) |>
  select(municipio_geocodigo, municipio_nome, casos_atual, casos_anterior,
        mediana_historica, n_anos_historico, motivo) |>
  arrange(municipio_nome)

message(sprintf("\n[diagnostico] %d de %d municipios SEM historico suficiente (janela = %d semanas):",
                nrow(sem_historico), nrow(d), janela))
print(sem_historico, row.names = FALSE)

dir.create("prep/out", showWarnings = FALSE, recursive = TRUE)
write.csv(sem_historico, "prep/out/municipios_sem_historico_rt.csv", row.names = FALSE, fileEncoding = "UTF-8")
message("\n[diagnostico] lista -> prep/out/municipios_sem_historico_rt.csv")

# ============================================ 3. historico completo deles ===
if (nrow(sem_historico) > 0) {
  historico <- serie_mun |>
    filter(municipio_geocodigo %in% sem_historico$municipio_geocodigo) |>
    left_join(sem_historico[, c("municipio_geocodigo", "motivo")], by = "municipio_geocodigo") |>
    arrange(municipio_nome, data_ini_se) |>
    select(municipio_geocodigo, municipio_nome, motivo, data_ini_se, ano, semana, se,
          casos, casos_est, pop, rt)

  write.csv(historico, "prep/out/historico_sem_historico_rt.csv", row.names = FALSE, fileEncoding = "UTF-8")
  message(sprintf("[diagnostico] historico completo (%d linhas, %d municipios) -> prep/out/historico_sem_historico_rt.csv",
                  nrow(historico), n_distinct(historico$municipio_geocodigo)))

  resumo <- historico |>
    group_by(municipio_geocodigo, municipio_nome, motivo) |>
    summarise(
      primeira_semana = min(data_ini_se), ultima_semana = max(data_ini_se),
      n_semanas = n(), n_anos = n_distinct(ano), total_casos = sum(casos, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(municipio_nome)

  message("\n[diagnostico] resumo por municipio:")
  print(resumo, n = Inf, width = Inf)
} else {
  message("[diagnostico] nenhum municipio sem historico suficiente nessa janela - nada a exportar.")
}
