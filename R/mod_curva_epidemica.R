# ==============================================================================
# Dengue RIDE | Modulo: Curva Epidemica
#
# Curva epidemica por temporada: casos notificados de cada semana da
# temporada em analise, contra o volume historico das temporadas de
# referencia. Mesma ordenacao de semana (ano sanitario, comeca na semana 27 -
# ver global.R) do diagrama de controle (R/mod_diagrama_controle.R), pra
# manter as duas leituras comparaveis.
#
# DIFERENCA para o diagrama de controle:
#   - metrica: casos ABSOLUTOS (nao incidencia /100 mil).
#   - fundo historico: SOMA dos casos de cada semana em todas as temporadas
#     de referencia (barra) + MEDIANA historica (linha) - nao Q1/Q2/Q3. A
#     leitura aqui e "volume total notificado naquela semana ao longo dos
#     anos", nao "distribuicao/faixa esperada" (isso e o diagrama de controle).
#
# Series desenhadas:
#   1. soma historica por semana        (barra, mais escura perto do maximo)
#   2. mediana historica por semana     (linha solida)
#   3. casos notificados da temporada em analise (linha tracejada)
#   4. nowcast da temporada em analise  (linha pontilhada, a partir de casos_est)
# ==============================================================================

# Nomes com prefixo CURVA_ de proposito: global.R carrega todos os modulos de
# R/ no MESMO ambiente global (source(local = FALSE)), em ordem alfabetica.
# COR_ATUAL/COR_NOWCAST sem prefixo colidiriam com as constantes de mesmo
# nome em R/mod_diagrama_controle.R (que carrega depois, em ordem alfabetica,
# e sobrescreveria estas aqui silenciosamente - sem erro, so cor errada).
COR_CURVA_HIST_MEDIANA <- PALETA_CHOCOLATE[["taupe"]]
COR_CURVA_ATUAL        <- PALETA_CHOCOLATE[["tinta"]]
COR_CURVA_NOWCAST      <- PALETA_CHOCOLATE[["ambar"]]


mod_curva_epidemica_controles <- function(id) {
  ns <- shiny::NS(id)
  
  temporadas <- rev(seq(ANO_MIN, ANO_MAX))
  
  shiny::tagList(
    shiny::selectInput(
      ns("ano"), "Temporada em análise",
      choices  = stats::setNames(temporadas, vapply(temporadas, rotulo_ano_sanitario, character(1))),
      selected = ANO_MAX
    ),
    shiny::sliderInput(
      ns("n_ref"), "Temporadas de referência",
      min = 3, max = 10, value = 5, step = 1
    ),
    shiny::checkboxInput(ns("mostrar_nowcast"), "Mostrar nowcast", value = TRUE),
    shiny::helpText(
      shiny::tags$small(
        "Barras: soma dos casos notificados daquela semana em todas as ",
        "temporadas de referência (mais escuras perto do máximo histórico). ",
        "Linha cinza: mediana histórica. Semana no eixo segue o ano ",
        "sanitário (27 a 26), igual ao diagrama de controle."
      )
    )
  )
}

mod_curva_epidemica_saida <- function(id, height = "420px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height)
}


mod_curva_epidemica_server <- function(id, serie_semanal) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    ano_analise <- shiny::reactive(as.integer(input$ano))
    
    anos_referencia <- shiny::reactive({
      alvo <- ano_analise()
      candidatos <- seq(alvo - as.integer(input$n_ref), alvo - 1L)
      candidatos[candidatos >= ANO_MIN & candidatos <= ANO_MAX]
    })
    
    # -- serie semanal por temporada: mesmo padrao do diagrama de controle --
    # Reaproveita serie_semanal() (ja buscada uma vez por geo(), compartilhada
    # com os outros graficos) - nao dispara consulta propria ao trocar
    # temporada/temporadas de referencia.
    serie <- shiny::reactive({
      anos_temporada <- c(anos_referencia(), ano_analise())
      shiny::req(length(anos_temporada) > 0)
      
      d <- serie_semanal()[, c("se", "casos", "casos_est")]
      d$se     <- as.integer(d$se)
      d$ano    <- d$se %/% 100L
      d$semana <- d$se %%  100L
      d$casos     <- as.numeric(d$casos)
      d$casos_est <- as.numeric(d$casos_est)
      
      d <- d[d$semana >= 1L & d$semana <= 53L, , drop = FALSE]
      d$ano_san <- ano_sanitario(d$ano, d$semana)
      d <- d[d$ano_san %in% anos_temporada, , drop = FALSE]
      
      dobrar_semana_53(d, c("casos", "casos_est"))
    })
    
    # -- montagem final: uma linha por semana, na ordem da temporada --------
    dados_grafico <- shiny::reactive({
      d <- serie()
      
      hist <- d[d$ano_san %in% anos_referencia(), , drop = FALSE] |>
        dplyr::group_by(semana) |>
        dplyr::summarise(
          casos_hist_total = sum(casos, na.rm = TRUE),
          mediana_hist      = stats::median(casos, na.rm = TRUE),
          .groups = "drop"
        )
      
      atual <- d[d$ano_san == ano_analise(), , drop = FALSE] |>
        dplyr::select(semana, casos_atual = casos, casos_nowcast = casos_est)
      
      data.frame(semana = NIVEIS_SEMANA_SANITARIA) |>
        dplyr::left_join(hist,  by = "semana") |>
        dplyr::left_join(atual, by = "semana") |>
        dplyr::mutate(
          dplyr::across(c(casos_hist_total, mediana_hist, casos_atual, casos_nowcast), ~ round(.x, 1)),
          # Rotulo categorico do eixo X, na ordem jul-jun (ja garantida por
          # NIVEIS_SEMANA_SANITARIA), nao na ordem numerica 1..53.
          semana_rotulo = factor(as.character(semana), levels = as.character(NIVEIS_SEMANA_SANITARIA))
        )
    })
    
    output$grafico <- echarts4r::renderEcharts4r({
      d <- dados_grafico()
      shiny::req(nrow(d) > 0)
      
      nome_atual   <- sprintf("Casos %s", rotulo_ano_sanitario(ano_analise()))
      nome_nowcast <- sprintf("Nowcast %s", rotulo_ano_sanitario(ano_analise()))
      
      max_hist <- suppressWarnings(max(d$casos_hist_total, na.rm = TRUE))
      if (!is.finite(max_hist) || max_hist <= 0) max_hist <- 1
      
      # Barra mais escura quanto mais perto do maximo historico - mesma
      # linguagem visual do mapa/KPIs (PALETA_CHOCOLATE, escuro = mais alto).
      js_cor_barra <- htmlwidgets::JS(sprintf(
        "function(p){
           var v = p.value, m = %s;
           if (v === null || v === undefined) return '%s';
           if (v > m * 0.8) return '%s';
           if (v > m * 0.5) return '%s';
           return '%s';
         }",
        max_hist,
        PALETA_CHOCOLATE[["baunilha"]],
        PALETA_CHOCOLATE[["amargo"]],
        PALETA_CHOCOLATE[["chocolate"]],
        PALETA_CHOCOLATE[["caramelo"]]
      ))
      
      # Posicao (indice 0-based) da ultima semana antes da virada de temporada
      # - mesmo calculo e mesmo efeito do diagrama de controle.
      idx_corte <- length(seq(SEMANA_CORTE_SANITARIA, 52L)) - 1L
      
      g <- d |>
        echarts4r::e_charts(semana_rotulo) |>
        
        # 1) soma historica
        echarts4r::e_bar(
          casos_hist_total,
          name      = "Histórico (soma)",
          itemStyle = list(color = js_cor_barra, borderRadius = list(2, 2, 0, 0))
        ) |>
        
        # 2) mediana historica
        echarts4r::e_line(
          mediana_hist,
          name      = "Mediana histórica",
          symbol    = "none",
          lineStyle = list(color = COR_CURVA_HIST_MEDIANA, width = 2, type = "solid")
        ) |>
        
        # 3) casos notificados da temporada em analise
        echarts4r::e_line(
          casos_atual,
          name       = nome_atual,
          symbol     = "circle",
          symbolSize = 4,
          lineStyle  = list(color = COR_CURVA_ATUAL, width = 2.5, type = "dashed")
        ) |>
        
        # linha vertical na virada da temporada (semana 52 -> semana 1)
        echarts4r::e_mark_line(
          serie     = nome_atual,
          data      = list(xAxis = idx_corte),
          title     = "",
          symbol    = "none",
          lineStyle = list(color = "gray", type = "solid", width = 1)
        )
      
      if (isTRUE(input$mostrar_nowcast)) {
        g <- g |>
          echarts4r::e_line(
            casos_nowcast,
            name      = nome_nowcast,
            symbol    = "none",
            lineStyle = list(color = COR_CURVA_NOWCAST, width = 2, type = "dotted")
          )
      }
      
      legenda <- c("Histórico (soma)", "Mediana histórica", nome_atual)
      if (isTRUE(input$mostrar_nowcast)) legenda <- c(legenda, nome_nowcast)
      
      g |>
        echarts4r::e_x_axis(
          name         = "Semana epidemiológica (temporada jul–jun)",
          nameLocation = "middle",
          nameGap      = 28,
          axisLabel    = list(interval = 3)
        ) |>
        echarts4r::e_y_axis(name = "casos", nameLocation = "end", nameGap = 12) |>
        echarts4r::e_legend(data = as.list(legenda), top = 0, type = "scroll") |>
        echarts4r::e_grid(left = 70, right = 24, top = 40, bottom = 60) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Salvar imagem")
    })
  })
}