# ==============================================================================
# Dengue RIDE | Modulo: Heatmap de sazonalidade
#
# Eixo X = semana epidemiologica (1-53), eixo Y = ano, cor = incidencia.
# A leitura util e vertical: em que semanas do ano a transmissao sobe, e como
# essa janela se desloca (ou nao) entre os anos.
#
# INCIDENCIA - mesma restricao das fases 2 e 4:
#   `p_inc100k` e uma razao por municipio/semana. Media de razoes entre
#   municipios ignora o peso populacional. Entao no nivel municipal a celula
#   e o proprio p_inc100k (equivalente a casos/pop*1e5), e acima disso e
#   sum(casos)/sum(pop)*1e5 recalculado para a area inteira.
#
# Nao consulta o banco: consome a serie semanal ja carregada pelo server.R.
# ==============================================================================

mod_heatmap_controles <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::sliderInput(
      ns("anos"), "Anos exibidos",
      min   = ANO_MIN,
      max   = ANO_MAX,
      value = c(max(ANO_MIN, ANO_MAX - 9), ANO_MAX),
      step  = 1,
      sep   = ""
    ),
    shiny::radioButtons(
      ns("escala"), "Escala de cor",
      choices  = c("Faixas do MS" = "faixas", "Contínua" = "continua"),
      selected = "faixas", inline = TRUE
    ),
    shiny::helpText(
      shiny::tags$small(
        "As faixas do MS são as mesmas do mapa coroplético, o que permite ",
        "comparar as duas visões."
      )
    )
  )
}

mod_heatmap_saida <- function(id, height = "480px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height)
}


mod_heatmap_server <- function(id, geo, serie_semanal) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    dados <- shiny::reactive({
      shiny::req(input$anos)
      
      serie_semanal() |>
        calcular_indicadores(geo()) |>
        dplyr::mutate(
          ano_num    = se %/% 100L,
          semana_num = se %%  100L
        ) |>
        dplyr::filter(
          !is.na(se),
          semana_num >= 1L, semana_num <= 53L,
          ano_num >= input$anos[1], ano_num <= input$anos[2]
        ) |>
        # A ordem das categorias no ECharts vem da ordem de aparicao nos dados.
        # Ordenar aqui garante semanas 1..53 no eixo X e anos crescentes no Y.
        dplyr::arrange(semana_num, ano_num) |>
        dplyr::transmute(
          semana = as.character(semana_num),
          ano    = as.character(ano_num),
          valor  = round(incidencia, 1),
          casos  = casos
        )
    })
    
    output$grafico <- echarts4r::renderEcharts4r({
      d <- dados()
      shiny::req(nrow(d) > 0)
      shiny::validate(shiny::need(
        any(!is.na(d$valor)),
        "Sem incidência calculável neste recorte (população ausente ou zerada)."
      ))
      
      # Casos por celula, para o tooltip. A serie desenhada so carrega a
      # incidencia; o numero absoluto vem deste dicionario.
      chave  <- paste(d$ano, d$semana, sep = "-")
      casos_js <- jsonlite::toJSON(
        stats::setNames(as.list(d$casos), chave),
        auto_unbox = TRUE, na = "null"
      )
      
      js_tooltip <- htmlwidgets::JS(sprintf(
        "function(p){
           var casos = %s;
           if (!p.value) return '';
           var semana = p.value[0], ano = p.value[1], inc = p.value[2];
           var c = casos[ano + '-' + semana];
           function f(x, d){
             return (x === null || x === undefined)
               ? '—' : x.toLocaleString('pt-BR', {maximumFractionDigits: d});
           }
           return '<b>SE ' + semana + ' / ' + ano + '</b><br/>' +
                  'Incidência: <b>' + f(inc, 1) + '</b> /100 mil<br/>' +
                  'Casos: ' + f(c, 0);
         }",
        casos_js
      ))
      
      g <- d |>
        echarts4r::e_charts(semana) |>
        echarts4r::e_heatmap(
          ano, valor,
          name      = "Incidência /100 mil",
          itemStyle = list(borderColor = "#FFFFFF", borderWidth = 0.5)
        )
      
      g <- if (identical(input$escala, "faixas")) {
        g |>
          echarts4r::e_visual_map(
            valor,
            type   = "piecewise",
            pieces = PECAS_INCIDENCIA,
            orient = "horizontal",
            left   = "center",
            bottom = 0,
            itemWidth = 14, itemHeight = 12
          )
      } else {
        g |>
          echarts4r::e_visual_map(
            valor,
            type    = "continuous",
            orient  = "horizontal",
            left    = "center",
            bottom  = 0,
            inRange = list(color = RAMPA_CHOCOLATE),
            formatter = htmlwidgets::JS(
              "function(v){return v.toLocaleString('pt-BR',{maximumFractionDigits:0});}"
            )
          )
      }
      
      g |>
        echarts4r::e_x_axis(
          type         = "category",
          name         = "Semana epidemiológica",
          nameLocation = "middle",
          nameGap      = 28,
          axisLabel    = list(interval = 3),
          splitArea    = list(show = TRUE)
        ) |>
        echarts4r::e_y_axis(
          type      = "category",
          splitArea = list(show = TRUE)
        ) |>
        echarts4r::e_grid(left = 60, right = 24, top = 20, bottom = 90) |>
        echarts4r::e_tooltip(trigger = "item", formatter = js_tooltip) |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Salvar imagem")
    })
  })
}