# ==============================================================================
# Dengue RIDE | Modulo: Serie Temporal
#
# Linha do notificado contra a do nowcast, ao longo de data_ini_se.
# Dois controles: agrupamento (semana epidemiologica ou mes) e metrica
# (casos absolutos ou incidencia por 100 mil).
#
# A serie inteira e enviada ao cliente e a navegacao acontece no datazoom:
# ~850 pontos para 16 anos de historico nao justificam refazer a consulta a
# cada ajuste de janela.
# ==============================================================================

mod_serie_temporal_controles <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::radioButtons(
      ns("agrupamento"), "Agrupamento",
      choices  = c("Semana epidemiológica" = "semana", "Mensal" = "mes"),
      selected = "semana", inline = TRUE
    ),
    shiny::radioButtons(
      ns("metrica"), "Métrica",
      choices  = c("Casos" = "casos", "Incidência /100 mil" = "incidencia"),
      selected = "casos", inline = TRUE
    ),
    shiny::checkboxInput(ns("mostrar_est"), "Mostrar nowcast", value = TRUE)
  )
}

mod_serie_temporal_saida <- function(id, height = "420px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height)
}


mod_serie_temporal_server <- function(id, geo, serie_semanal) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    dados <- shiny::reactive({
      serie_semanal() |>
        reagrupar_periodo(input$agrupamento) |>
        calcular_indicadores(geo())
    })
    
    output$grafico <- echarts4r::renderEcharts4r({
      d <- dados()
      shiny::req(nrow(d) > 0)
      
      por_incidencia <- identical(input$metrica, "incidencia")
      
      d$obs <- if (por_incidencia) d$incidencia     else d$casos
      d$est <- if (por_incidencia) d$incidencia_est else d$casos_est
      
      # Forca eixo categorico (texto "AAAA-MM-DD"): se `periodo` for passado
      # como Date, o echarts4r pode auto-detectar eixo de tempo e entregar
      # timestamp numerico ao formatter, que espera texto com "-" pra separar
      # ano/mes/dia (ver js_formato_data). Sem isso o rotulo vira
      # "undefined/undefined/<numero>" e os ticks se sobrepoem.
      d$periodo <- as.character(d$periodo)
      
      nome_obs  <- if (por_incidencia) "Incidência notificada" else "Casos notificados"
      nome_est  <- if (por_incidencia) "Incidência (nowcast)"  else "Casos estimados (nowcast)"
      casas     <- if (por_incidencia) 1 else 0
      fmt_data  <- js_formato_data(if (identical(input$agrupamento, "mes")) "mes" else "dia")
      
      js_tooltip <- htmlwidgets::JS(sprintf(
        "function(params){
           if (!params || !params.length) return '';
           var v = params[0].axisValue;
           var txt = '<b>' + %s + '</b>';
           params.forEach(function(p){
             var val = (p.value === null || p.value === undefined)
               ? '—'
               : p.value.toLocaleString('pt-BR', {maximumFractionDigits: %d});
             txt += '<br/>' + p.marker + p.seriesName + ': <b>' + val + '</b>';
           });
           return txt;
         }",
        fmt_data, casas
      ))
      
      js_eixo <- htmlwidgets::JS(sprintf("function(v){ return %s; }", fmt_data))
      
      g <- d |>
        echarts4r::e_charts(periodo) |>
        echarts4r::e_line(
          obs,
          name       = nome_obs,
          symbol     = "none",
          smooth     = FALSE,
          lineStyle  = list(color = PALETA_DENGUE[["dengue"]], width = 2),
          itemStyle  = list(color = PALETA_DENGUE[["dengue"]])
        )
      
      if (isTRUE(input$mostrar_est)) {
        g <- g |>
          echarts4r::e_line(
            est,
            name      = nome_est,
            symbol    = "none",
            lineStyle = list(color = SERIES_CENARIOS[[3]], width = 2, type = "dashed"),
            itemStyle = list(color = SERIES_CENARIOS[[3]])
          )
      }
      
      g |>
        echarts4r::e_x_axis(axisLabel = list(formatter = js_eixo)) |>
        echarts4r::e_y_axis(
          name         = if (por_incidencia) "/100 mil" else "casos",
          nameLocation = "end",
          nameGap      = 12
        ) |>
        echarts4r::e_datazoom(x_index = 0, type = "slider", bottom = 4) |>
        echarts4r::e_legend(top = 0, type = "scroll") |>
        echarts4r::e_grid(left = 70, right = 24, top = 40, bottom = 78) |>
        echarts4r::e_tooltip(trigger = "axis", formatter = js_tooltip) |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Salvar imagem") |>
        echarts4r::e_text_style(fontFamily = FONTE_GRAFICO)
    })
  })
}