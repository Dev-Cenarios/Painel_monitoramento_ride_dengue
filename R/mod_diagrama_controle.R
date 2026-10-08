# ==============================================================================
# Dengue RIDE | Modulo: Diagrama de Controle (+ Previsao por classe de modelo)
#
# Diagrama de controle epidemiologico classico: o coeficiente de incidencia
# semanal da temporada em analise sobreposto ao canal endemico construido com
# o historico (Q1, Q2, Q3 das temporadas de referencia). Fiel ao script de
# referencia em ggplot2 do professor.
#
# TEMPORADA (ano sanitario, ver global.R): o eixo comeca na SE 27 e fecha na
# SE 26 do ano seguinte, com uma linha vertical marcando a virada. O rotulo do
# eixo X traz a SE em cima e o ano-calendario embaixo.
#
# REGRA DE NEGOCIO do canal (estrita): EXCLUSIVAMENTE quartis - faixa inferior
# Q1..Q2, faixa superior Q2..Q3, linha central Q2. Nenhuma outra estatistica.
#
# ---------------------------------------------------------------------------
# CAMADA DE PREVISAO. Backend isolado em forecasting/ (ver README-FORECAST.md),
# exposto pelo global.R: get_dengue_forecast(), FORECAST_DISPONIVEL,
# FORECAST_METRICS, FORECAST_CLASSES, FORECAST_ROTULO(), e o registro
# FC_MODELOS / FC_CLASSES de utils_forecast.R.
#
#   * Modelos agrupados por CLASSE (estatisticos / machine learning /
#     mecanicistas). Um ensemble por classe (peso = inverso do RMSE dentro da
#     classe) e um ENSEMBLE GLOBAL que combina os ensembles de classe.
#   * Painel: checkboxes para cada ensemble (global + por classe) e para cada
#     modelo, agrupados por classe, com RMSE/MAPE do backtest. "Ensemble
#     global" marcado por padrao.
#   * Cada linha selecionada entra como CONTINUACAO TRACEJADA da linha de casos
#     NOTIFICADOS da temporada corrente, ancorada no ultimo ponto notificado
#     (sem buraco). Faixa sombreada = IP 80% do ensemble GLOBAL.
#   * Previsao em incidencia = casos previstos / populacao do recorte * 1e5.
#     So aparece para a temporada corrente.
#   * Checkbox "Mostrar linha de nowcast": incidencia por casos_est como serie
#     SEPARADA (pontilhada), ao lado da linha de casos notificados.
#
# Incidencia = sum(casos) / sum(pop) * 100.000 na semana. NUNCA media de razoes.
# ==============================================================================

# Minimo de anos de referencia para os quartis de uma semana. Com 1-2
# observacoes Q1/Q2/Q3 nao descrevem distribuicao nenhuma.
MIN_ANOS_REFERENCIA <- 3L

# Temporadas anteriores que formam o canal endemico (era um slider; virou
# constante para enxugar o painel). Antigo default = 5.
N_TEMPORADAS_REFERENCIA <- 5L

# Canal endemico na paleta comum (PALETA_DENGUE, em global.R): faixas em azul
# claro (referencia historica, recua), mediana em azul escuro e a temporada em
# analise em terracota forte - o olho vai direto para a curva atual. O nowcast
# e a mesma grandeza estimada, entao fica no mesmo tom, mais claro e pontilhado.
COR_MEDIANA          <- PALETA_DENGUE[["azul_titulo"]]  # mediana (Q2)
COR_ATUAL            <- PALETA_DENGUE[["dengue_forte"]] # incidencia da temporada em analise
COR_NOWCAST          <- TONS_DENGUE[["400"]]            # linha de nowcast
COR_LIMITE_SUPERIOR  <- TONS_AZUL[["100"]]              # area mediana..Q3
COR_LIMITE_INFERIOR  <- TONS_AZUL[["50"]]               # area Q1..mediana

# Linhas de previsao: o ensemble global e a "linha principal" (tinta, mais
# grossa, com banda); classes e modelos usam a paleta categorica Cenarios
# (SERIES_CENARIOS) a partir da serie 3 - as series 1 (azul) e 2 (terracota)
# ja estao ocupadas pelo canal e pela curva observada.
COR_PREV_GLOBAL <- PALETA_DENGUE[["tinta"]]  # Ensemble global

.DIAG_CORES_CLASSE <- SERIES_CENARIOS[3:5]  # Estatisticos, Machine Learning, Mecanicistas
# Uma cor por modelo, na ordem de FC_MODELOS (arima, sarima, gam, stlets, rf).
# Ciclo por modulo se entrarem mais modelos que cores.
.DIAG_CORES_MODELO <- SERIES_CENARIOS[3:8]

#' Catalogo das linhas de previsao disponiveis.
#'
#' Devolve list nomeada por CHAVE (global | ens_<classe> | <modelo>), cada item
#' com: nome (legenda), cor, w (espessura), banda (TRUE so no global), tipo e
#' metrica_key (linha correspondente em FORECAST_METRICS$modelo). Vazio se o
#' forecasting nao estiver disponivel.
diag_catalogo_previsao <- function() {
  if (!isTRUE(FORECAST_DISPONIVEL) || !exists("FC_MODELOS")) return(list())

  cls <- names(FORECAST_CLASSES)
  mk  <- names(FC_MODELOS)
  cat <- list()

  cat[["global"]] <- list(
    nome = "Previsão — Ensemble global", cor = COR_PREV_GLOBAL,
    w = 2.6, banda = TRUE, tipo = "global", metrica_key = "ensemble_global"
  )
  for (i in seq_along(cls)) {
    ch <- paste0("ens_", cls[i])
    cat[[ch]] <- list(
      nome = sprintf("Previsão — Ensemble %s", unname(FORECAST_CLASSES[cls[i]])),
      cor  = .DIAG_CORES_CLASSE[(i - 1L) %% length(.DIAG_CORES_CLASSE) + 1L],
      w = 1.9, banda = FALSE, tipo = "classe", metrica_key = paste0("ensemble_", cls[i])
    )
  }
  for (i in seq_along(mk)) {
    cat[[mk[i]]] <- list(
      nome = sprintf("Previsão — %s", fc_rotulo_modelo(mk[i])),
      cor  = .DIAG_CORES_MODELO[(i - 1L) %% length(.DIAG_CORES_MODELO) + 1L],
      w = 1.2, banda = FALSE, tipo = "modelo", metrica_key = mk[i]
    )
  }
  cat
}

#' Chave da linha -> coluna de PONTO no data.frame de get_dengue_forecast().
diag_col_ponto <- function(ch) {
  if (identical(ch, "global"))     return("Casos_Previstos_Ensemble")
  if (startsWith(ch, "ens_"))      return(paste0("Prev_ens_", sub("^ens_", "", ch)))
  paste0("Prev_", ch)
}


# ------------------------------------------------------------------ painel ----
#' Painel lateral do card: temporada + nowcast + camada de previsao.
mod_diagrama_controle_painel <- function(id) {
  ns <- shiny::NS(id)
  temporadas <- rev(seq(ANO_MIN, ANO_MAX))

  if (isTRUE(FORECAST_DISPONIVEL) && length(diag_catalogo_previsao()) > 0) {
    cat <- diag_catalogo_previsao()
    met <- FORECAST_METRICS

    metricas_span <- function(metrica_key) {
      if (is.null(met) || !("modelo" %in% names(met))) return(NULL)
      r <- met[met$modelo == metrica_key, , drop = FALSE]
      if (!nrow(r)) return(NULL)
      shiny::tags$small(
        class = "text-muted",
        sprintf("RMSE %s · MAPE %s%%", fmt_num(r$RMSE[1], 1), fmt_num(r$MAPE[1], 1))
      )
    }
    item_span <- function(ch) {
      rot <- sub("^Previsão — ", "", cat[[ch]]$nome)
      shiny::tags$span(shiny::tags$strong(rot), shiny::tags$br(),
                       metricas_span(cat[[ch]]$metrica_key))
    }

    ens_keys <- c("global", paste0("ens_", names(FORECAST_CLASSES)))
    ui_ens <- shiny::checkboxGroupInput(
      ns("prev_ens"), "Ensembles",
      choiceNames  = lapply(ens_keys, item_span),
      choiceValues = as.list(ens_keys),
      selected     = "global"
    )
    ui_mods <- lapply(names(FORECAST_CLASSES), function(cl) {
      mods <- fc_modelos_da_classe(cl)
      if (!length(mods)) return(NULL)
      shiny::checkboxGroupInput(
        ns(paste0("prev_mod_", cl)),
        sprintf("Modelos — %s", unname(FORECAST_CLASSES[cl])),
        choiceNames  = lapply(mods, item_span),
        choiceValues = as.list(mods),
        selected     = character(0)
      )
    })

    bloco_prev <- shiny::tagList(
      shiny::tags$hr(class = "my-2"),
      shiny::tags$h6(class = "mt-0 mb-1 fw-semibold", "Previsão — até o fim da temporada"),
      ui_ens, ui_mods,
      shiny::uiOutput(ns("nota_previsao"))
    )
  } else {
    bloco_prev <- shiny::tagList(
      shiny::tags$hr(class = "my-2"),
      shiny::tags$small(
        class = "text-muted",
        "Previsão indisponível. Rode ",
        shiny::tags$code("Rscript forecasting/train_models.R"), " e reinicie o app."
      )
    )
  }

  shiny::tagList(
    shiny::selectInput(
      ns("ano"), "Temporada em análise",
      choices  = stats::setNames(temporadas, vapply(temporadas, rotulo_ano_sanitario, character(1))),
      selected = ANO_MAX
    ),
    # Linha de nowcast: serie SEPARADA da observada (casos_est vs casos).
    shiny::checkboxInput(
      ns("nowcast"),
      shiny::tags$span("Mostrar linha de nowcast ",
                       shiny::tags$small(class = "text-muted", "(casos_est)")),
      value = FALSE
    ),
    shiny::uiOutput(ns("nota_referencia")),
    bloco_prev
  )
}

# Compat: o ui.R antigo chamava _controles() no popover.
mod_diagrama_controle_controles <- function(id) mod_diagrama_controle_painel(id)

mod_diagrama_controle_saida <- function(id, height = "440px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height)
}


# ------------------------------------------------------------------ server ----
mod_diagrama_controle_server <- function(id, serie_semanal, geo) {

  shiny::moduleServer(id, function(input, output, session) {

    ano_analise <- shiny::reactive(as.integer(input$ano))

    anos_referencia <- shiny::reactive({
      alvo <- ano_analise()
      candidatos <- seq(alvo - N_TEMPORADAS_REFERENCIA, alvo - 1L)
      candidatos[candidatos >= ANO_MIN & candidatos <= ANO_MAX]
    })

    mostrar_nowcast <- shiny::reactive(isTRUE(input$nowcast))

    # -- serie semanal do recorte, restrita as temporadas necessarias -------
    serie <- shiny::reactive({
      anos_temporada <- c(anos_referencia(), ano_analise())
      shiny::req(length(anos_temporada) > 0)

      d <- serie_semanal()[, c("se", "casos", "casos_est", "pop")]
      d$se     <- as.integer(d$se)
      d$ano    <- d$se %/% 100L
      d$semana <- d$se %%  100L
      d$pop    <- as.numeric(d$pop)
      d$inc         <- dplyr::if_else(d$pop > 0, as.numeric(d$casos)     / d$pop * 1e5, NA_real_)
      d$inc_nowcast <- dplyr::if_else(d$pop > 0, as.numeric(d$casos_est) / d$pop * 1e5, NA_real_)

      d <- d[d$semana >= 1L & d$semana <= 53L, , drop = FALSE]
      d$ano_san <- ano_sanitario(d$ano, d$semana)
      d <- d[d$ano_san %in% anos_temporada, , drop = FALSE]
      dobrar_semana_53(d, c("inc", "inc_nowcast"))
    })

    # -- canal endemico: EXCLUSIVAMENTE Q1, Q2, Q3 --------------------------
    canal <- shiny::reactive({
      d <- serie()
      ref <- d[d$ano_san %in% anos_referencia(), , drop = FALSE]
      shiny::req(nrow(ref) > 0)
      ref |>
        dplyr::filter(!is.na(inc)) |>
        dplyr::group_by(semana) |>
        dplyr::summarise(
          n_anos = dplyr::n(),
          q1 = stats::quantile(inc, 0.25, na.rm = TRUE, names = FALSE),
          q2 = stats::quantile(inc, 0.50, na.rm = TRUE, names = FALSE),
          q3 = stats::quantile(inc, 0.75, na.rm = TRUE, names = FALSE),
          .groups = "drop"
        ) |>
        dplyr::mutate(dplyr::across(
          c(q1, q2, q3),
          ~ dplyr::if_else(n_anos >= MIN_ANOS_REFERENCIA, .x, NA_real_)
        ))
    })

    # -- previsao ate a SE 26 do fim da temporada (uma chamada por recorte/dia) --
    # Horizonte DINAMICO (fc_horizon_fim_temporada em utils_forecast.R): nao e
    # um numero fixo de semanas, muda conforme a ultima semana observada.
    previsao <- shiny::reactive({
      if (!isTRUE(FORECAST_DISPONIVEL)) return(NULL)
      g   <- geo()
      loc <- switch(g$nivel %||% "brasil",
                    municipio = g$geocodigo, uf = g$uf_ibge, NULL)
      con_fc <- if (exists("con") && !is.null(con) &&
                    isTRUE(tryCatch(DBI::dbIsValid(con), error = function(e) FALSE))) con else NULL
      shiny::withProgress(message = "Calculando previsão…", value = 0.6, {
        tryCatch(
          get_dengue_forecast(loc, data_atual = Sys.Date(), con = con_fc, n_semanas_hist = 0L),
          error = function(e) {
            warning("[diagrama] previsão indisponível: ", conditionMessage(e)); NULL
          }
        )
      })
    }) |>
      shiny::bindCache(geo(), Sys.Date(), VERSAO_DADOS, cache = CACHE_PREVISAO)

    # Temporadas (ano sanitario) que TEM alguma linha "previsto". Normalmente
    # uma so, mas a reprojecao das `semanas_recentes` (get_dengue_forecast())
    # pode "vazar" pra temporada ANTERIOR quando a corrente e muito jovem
    # (poucas semanas desde a virada) - por isso e um vetor, nao um unico ano.
    temporadas_com_previsao <- shiny::reactive({
      fc <- previsao()
      if (is.null(fc) || !nrow(fc)) return(integer(0))
      f <- fc[fc$Tipo == "previsto", , drop = FALSE]
      sort(unique(f$Ano_Sanitario))
    })

    # -- montagem: uma linha por semana, na ordem da temporada ------------
    dados_grafico <- shiny::reactive({
      d   <- serie()
      cnl <- canal()

      atual <- d[d$ano_san == ano_analise(), , drop = FALSE] |>
        dplyr::select(semana, inc_notif = inc, inc_now = inc_nowcast)

      df <- data.frame(semana = NIVEIS_SEMANA_SANITARIA) |>
        dplyr::left_join(cnl,   by = "semana") |>
        dplyr::left_join(atual, by = "semana") |>
        dplyr::mutate(
          faixa_base         = q1,
          amplitude_inferior = dplyr::if_else(!is.na(q1) & !is.na(q2), pmax(q2 - q1, 0), NA_real_),
          amplitude_superior = dplyr::if_else(!is.na(q2) & !is.na(q3), pmax(q3 - q2, 0), NA_real_)
        )

      chaves <- names(diag_catalogo_previsao())
      for (ch in chaves) df[[paste0("inc_", ch)]] <- NA_real_
      df$inc_lo <- NA_real_
      df$inc_hi <- NA_real_

      fc <- previsao()
      if (!is.null(fc) && nrow(fc) > 0 && length(chaves) > 0) {
        fcw <- fc[fc$Tipo == "previsto" & fc$Ano_Sanitario == ano_analise() &
                    fc$Pop > 0, , drop = FALSE]
        if (nrow(fcw) > 0) {
          k   <- 1e5 / fcw$Pop
          idx <- match(fcw$Semana_Epi, df$semana)
          ok  <- !is.na(idx)

          for (ch in chaves) {
            cp <- diag_col_ponto(ch)
            if (cp %in% names(fcw)) {
              df[[paste0("inc_", ch)]][idx[ok]] <- round(fcw[[cp]][ok] * k, 2)
            }
          }

          # Faixa do IP: SO nas primeiras FC_HORIZON_DEFAULT semanas previstas
          # (o horizonte validado no backtest). Alem disso o IP cresce sem
          # freio - os preditores de GAM/RF ja congelaram no ultimo valor
          # observado (ver utils_forecast.R) e a incerteza deixa de refletir
          # algo calibrado; uma faixa de 80% cobrindo dezenas de semanas
          # dominaria o grafico (a escala do eixo Y teria que acomodar o topo
          # dela) e esconderia o canal endemico e as linhas de previsao. As
          # linhas de PONTO (acima) continuam ate a SE 26 normalmente.
          if (all(c("Limite_Inferior", "Limite_Superior") %in% names(fcw))) {
            fcw_banda <- fcw[order(fcw$Data_SE), , drop = FALSE]
            fcw_banda <- utils::head(fcw_banda, FC_HORIZON_DEFAULT)
            k_banda   <- 1e5 / fcw_banda$Pop
            idx_banda <- match(fcw_banda$Semana_Epi, df$semana)
            ok_banda  <- !is.na(idx_banda)
            df$inc_lo[idx_banda[ok_banda]] <- round(fcw_banda$Limite_Inferior[ok_banda] * k_banda[ok_banda], 2)
            df$inc_hi[idx_banda[ok_banda]] <- round(fcw_banda$Limite_Superior[ok_banda] * k_banda[ok_banda], 2)
          }

          # Ancora: ultima semana observada ANTES da 1a semana prevista.
          prim <- fcw$Semana_Epi[which.min(fcw$Data_SE)]
          pos  <- match(prim, NIVEIS_SEMANA_SANITARIA)
          if (!is.na(pos) && pos > 1L) {
            anc <- pos - 1L
            val <- df$inc_notif[anc]
            if (is.finite(val)) {
              for (ch in chaves) {
                cc <- paste0("inc_", ch)
                if (any(!is.na(df[[cc]][idx[ok]]))) df[[cc]][anc] <- round(val, 2)
              }
              df$inc_lo[anc] <- round(val, 2)
              df$inc_hi[anc] <- round(val, 2)
            }
          }
        }
      }

      df$faixa_ic_base <- df$inc_lo
      df$faixa_ic_amp  <- dplyr::if_else(!is.na(df$inc_hi) & !is.na(df$inc_lo),
                                        pmax(df$inc_hi - df$inc_lo, 0), NA_real_)

      cols_round <- intersect(
        c("q1", "q2", "q3", "faixa_base", "amplitude_inferior", "amplitude_superior",
          "inc_notif", "inc_now", paste0("inc_", chaves),
          "inc_lo", "inc_hi", "faixa_ic_base", "faixa_ic_amp"),
        names(df)
      )
      df |>
        dplyr::mutate(
          dplyr::across(dplyr::all_of(cols_round), ~ round(.x, 2)),
          semana_rotulo = factor(as.character(semana),
                                 levels = as.character(NIVEIS_SEMANA_SANITARIA))
        )
    })

    # -- notas ----------------------------------------------------------
    output$nota_referencia <- shiny::renderUI({
      anos <- anos_referencia()
      cnl  <- canal()
      curtas <- sum(cnl$n_anos < MIN_ANOS_REFERENCIA, na.rm = TRUE)
      shiny::tags$div(
        class = "small text-muted",
        shiny::tags$strong("Referência: "),
        if (length(anos)) {
          sprintf("%s – %s", rotulo_ano_sanitario(min(anos)), rotulo_ano_sanitario(max(anos)))
        } else "—",
        shiny::tags$br(),
        sprintf("%d temporada(s) no canal", length(anos)),
        if (curtas > 0) shiny::tagList(
          shiny::tags$br(),
          shiny::tags$span(class = "text-warning",
                           sprintf("%d semana(s) sem histórico suficiente", curtas))
        )
      )
    })

    output$nota_previsao <- shiny::renderUI({
      if (!isTRUE(FORECAST_DISPONIVEL)) return(NULL)
      fc    <- previsao()
      temps <- temporadas_com_previsao()
      if (!length(temps)) {
        return(shiny::tags$div(class = "small text-muted",
                               "Previsão indisponível para este recorte."))
      }
      # A reprojecao das semanas recentes pode tocar a temporada ANTERIOR (ver
      # nota acima) - so avisa "fora da temporada corrente" se a temporada em
      # analise nao tiver NENHUMA linha de previsao, nao so pela primeira.
      if (!(ano_analise() %in% temps)) {
        return(shiny::tags$div(
          class = "small text-warning",
          sprintf("Previsão exibida só n%s temporada%s %s.",
                  if (length(temps) > 1) "as" else "a", if (length(temps) > 1) "s" else "",
                  paste(vapply(temps, rotulo_ano_sanitario, character(1)), collapse = ", "))
        ))
      }
      f    <- fc[fc$Tipo == "previsto", , drop = FALSE]           # todas (p/ o fim real, SE 26)
      f_ta <- f[f$Ano_Sanitario == ano_analise(), , drop = FALSE] # so as desta temporada (p/ contagem)
      shiny::tags$div(
        class = "small text-muted",
        sprintf("Previsão até SE %02d/%d — %d semana%s nesta temporada.",
                f$Semana_Epi[nrow(f)], f$SE[nrow(f)] %/% 100L,
                nrow(f_ta), if (nrow(f_ta) == 1L) "" else "s")
      )
    })

    # -- grafico -----------------------------------------------------------
    output$grafico <- echarts4r::renderEcharts4r({
      d <- dados_grafico()
      shiny::req(nrow(d) > 0)

      anos <- anos_referencia()
      shiny::validate(shiny::need(
        length(anos) >= 1,
        sprintf("Sem temporadas de referência disponíveis antes de %s.",
                rotulo_ano_sanitario(ano_analise()))
      ))

      nome_obs <- sprintf("Coef. incidência %s", rotulo_ano_sanitario(ano_analise()))
      nome_now <- sprintf("Nowcast %s",          rotulo_ano_sanitario(ano_analise()))
      com_nowcast <- isTRUE(mostrar_nowcast()) && any(!is.na(d$inc_now))

      cat_prev <- diag_catalogo_previsao()
      sel <- character(0)
      if (isTRUE(FORECAST_DISPONIVEL) && length(cat_prev)) {
        sel <- c(input$prev_ens,
                 unlist(lapply(names(FORECAST_CLASSES),
                               function(cl) input[[paste0("prev_mod_", cl)]])))
        sel <- intersect(sel, names(cat_prev))
      }
      tem_banda <- ("global" %in% sel) && any(!is.na(d$faixa_ic_amp))

      idx_corte <- length(seq(SEMANA_CORTE_SANITARIA, 52L)) - 1L

      # Rotulo do eixo X: SE em cima, ano-calendario embaixo.
      ano_base  <- ano_analise()
      js_eixo_x <- htmlwidgets::JS(sprintf(
        "function(v){var se=parseInt(v,10);var a=(se>=%d)?%d:%d;return '{se|'+v+'}\\n{yr|'+a+'}';}",
        SEMANA_CORTE_SANITARIA, ano_base, ano_base + 1L
      ))

      # Tabela do tooltip (valores reais, nao os acumulados das pilhas).
      tab_cols <- c("semana", "q1", "q2", "q3", "inc_notif", "inc_now",
                    "inc_global", "inc_lo", "inc_hi", "n_anos")
      tab_cols <- intersect(tab_cols, names(d))
      tabela_js <- jsonlite::toJSON(d[, tab_cols], dataframe = "rows", na = "null",
                                    auto_unbox = TRUE, digits = 2)

      js_tooltip <- htmlwidgets::JS(sprintf(
        "function(params){
           var tab = %s;
           if (!params || !params.length) return '';
           var r = tab[params[0].dataIndex];
           if (!r) return '';
           function f(v){return (v===null||v===undefined)?'—':v.toLocaleString('pt-BR',{minimumFractionDigits:1,maximumFractionDigits:1});}
           var out = '<b>Semana epidemiológica ' + r.semana + '</b>';
           if (r.inc_notif !== null && r.inc_notif !== undefined) {
             var sit = '';
             if (r.q3 !== null && r.q1 !== null) {
               if (r.inc_notif > r.q3)      sit = '<span style=\"color:%s\">acima do esperado</span>';
               else if (r.inc_notif < r.q1) sit = '<span style=\"opacity:.7\">abaixo do esperado</span>';
               else                          sit = '<span style=\"opacity:.7\">dentro do esperado</span>';
             }
             out += '<br/>%s: <b>' + f(r.inc_notif) + '</b> ' + sit;
           }
           if (r.inc_now !== null && r.inc_now !== undefined)
             out += '<br/><span style=\"color:%s\">%s: <b>' + f(r.inc_now) + '</b></span>';
           if (r.inc_global !== null && r.inc_global !== undefined) {
             out += '<br/><span style=\"color:%s\">Previsão (Ensemble global): <b>' + f(r.inc_global) + '</b></span>';
             if (r.inc_lo !== null && r.inc_hi !== null)
               out += '<br/><span style=\"opacity:.7\">IP 80%%: ' + f(r.inc_lo) + ' – ' + f(r.inc_hi) + '</span>';
           }
           if (r.q2 !== null && r.q2 !== undefined) {
             out += '<hr style=\"margin:4px 0;opacity:.3\"/>' +
                    '<span style=\"opacity:.75\">Limite superior (Q3): ' + f(r.q3) + '<br/>' +
                    'Mediana (Q2): ' + f(r.q2) + '<br/>' +
                    'Limite inferior (Q1): ' + f(r.q1) + '</span>' +
                    '<br/><span style=\"opacity:.55\">' + (r.n_anos || 0) + ' temporada(s) de referência</span>';
           }
           return out;
         }",
        tabela_js, COR_ATUAL, nome_obs, COR_NOWCAST, nome_now, COR_PREV_GLOBAL
      ))

      g <- d |>
        echarts4r::e_charts(semana_rotulo) |>
        echarts4r::e_area(
          faixa_base, name = "faixa_base", stack = "canal", symbol = "none",
          silent = TRUE, legend = FALSE,
          lineStyle = list(color = "transparent", opacity = 0, width = 0),
          areaStyle = list(color = "transparent", opacity = 0),
          itemStyle = list(color = "transparent", opacity = 0),
          emphasis  = list(disabled = TRUE), tooltip = list(show = FALSE)
        ) |>
        echarts4r::e_area(
          amplitude_inferior, name = "Limite inferior", stack = "canal",
          symbol = "none", silent = TRUE, lineStyle = list(opacity = 0),
          areaStyle = list(color = COR_LIMITE_INFERIOR, opacity = 1),
          itemStyle = list(color = COR_LIMITE_INFERIOR)
        ) |>
        echarts4r::e_area(
          amplitude_superior, name = "Limite superior", stack = "canal",
          symbol = "none", silent = TRUE, lineStyle = list(opacity = 0),
          areaStyle = list(color = COR_LIMITE_SUPERIOR, opacity = 1),
          itemStyle = list(color = COR_LIMITE_SUPERIOR)
        ) |>
        echarts4r::e_line(
          q2, name = "Mediana (Q2)", symbol = "none",
          lineStyle = list(color = COR_MEDIANA, width = 1.2, type = "solid"),
          itemStyle = list(color = COR_MEDIANA)
        ) |>
        echarts4r::e_line(
          inc_notif, name = nome_obs, symbol = "none",
          lineStyle = list(color = COR_ATUAL, width = 2.2, type = "solid"),
          itemStyle = list(color = COR_ATUAL), connectNulls = FALSE
        ) |>
        echarts4r::e_mark_line(
          serie = nome_obs, data = list(xAxis = idx_corte), title = "",
          symbol = "none", lineStyle = list(color = PALETA_DENGUE[["sub"]], type = "solid", width = 1)
        )

      legenda <- c("Limite inferior", "Limite superior", "Mediana (Q2)", nome_obs)

      if (com_nowcast) {
        g <- g |>
          echarts4r::e_line(
            inc_now, name = nome_now, symbol = "none",
            lineStyle = list(color = COR_NOWCAST, width = 2, type = "dotted"),
            itemStyle = list(color = COR_NOWCAST), connectNulls = FALSE
          )
        legenda <- c(legenda, nome_now)
      }

      # Faixa do IP 80% do ensemble global (duas areas empilhadas). Restrita as
      # primeiras FC_HORIZON_DEFAULT semanas (ver dados_grafico()) - alem
      # disso o IP nao e mais calibrado e so cresceria, dominando o grafico.
      nome_ip <- sprintf("IP 80%% (global, %d sem.)", FC_HORIZON_DEFAULT %||% 4L)
      if (tem_banda) {
        g <- g |>
          echarts4r::e_area(
            faixa_ic_base, name = "faixa_ic_base", stack = "ic_prev", symbol = "none",
            silent = TRUE, legend = FALSE, lineStyle = list(opacity = 0),
            areaStyle = list(color = "transparent", opacity = 0),
            itemStyle = list(color = "transparent"), emphasis = list(disabled = TRUE),
            tooltip = list(show = FALSE)
          ) |>
          echarts4r::e_area(
            faixa_ic_amp, name = nome_ip, stack = "ic_prev", symbol = "none",
            silent = TRUE, lineStyle = list(opacity = 0),
            areaStyle = list(color = COR_PREV_GLOBAL, opacity = 0.10),
            itemStyle = list(color = COR_PREV_GLOBAL)
          )
        legenda <- c(legenda, nome_ip)
      }

      # Linhas de previsao selecionadas (na ordem do catalogo: global, classes, modelos).
      for (ch in names(cat_prev)) {
        if (!(ch %in% sel)) next
        cc <- paste0("inc_", ch)
        if (!(cc %in% names(d)) || !any(!is.na(d[[cc]]))) next
        spec <- cat_prev[[ch]]
        g <- echarts4r::e_line_(
          g, cc, name = spec$nome,
          symbol = "circle", symbolSize = if (identical(ch, "global")) 6 else 4,
          lineStyle = list(color = spec$cor, width = spec$w, type = "dashed"),
          itemStyle = list(color = spec$cor), connectNulls = TRUE
        )
        legenda <- c(legenda, spec$nome)
      }

      g |>
        echarts4r::e_x_axis(
          name = "Semana epidemiológica (temporada jul–jun)",
          nameLocation = "middle", nameGap = 42,
          axisLabel = list(
            interval = 3, formatter = js_eixo_x, lineHeight = 15,
            rich = list(
              se = list(fontSize = 11),
              yr = list(fontSize = 9, color = PALETA_DENGUE[["sub"]],
                        padding = list(3, 0, 0, 0))
            )
          )
        ) |>
        echarts4r::e_y_axis(
          name = "Coeficiente de Incidência /100 mil",
          nameLocation = "middle", nameGap = 48, scale = FALSE
        ) |>
        echarts4r::e_legend(data = as.list(legenda), bottom = 0, type = "scroll") |>
        echarts4r::e_grid(left = 70, right = 24, top = 24, bottom = 88) |>
        echarts4r::e_tooltip(trigger = "axis", formatter = js_tooltip) |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Salvar imagem") |>
        echarts4r::e_text_style(fontFamily = FONTE_GRAFICO)
    })
  })
}
