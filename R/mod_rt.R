# ==============================================================================
# Dengue RIDE | Modulo: Numero de Reproducao Efetivo (Rt)
#
# Linha de Rt com limiar horizontal em 1,0: acima de 1 cada caso gera mais de
# um caso novo e a transmissao esta em expansao; abaixo de 1, em retracao.
#
# AGREGACAO - o ponto delicado deste modulo:
#   `rt` e estimado POR MUNICIPIO. Nao existe operacao aritmetica que combine
#   Rt de municipios diferentes num Rt do estado: seria preciso reestimar a
#   partir da serie agregada de casos, com o modelo de intervalo de geracao.
#   Entao:
#     - nivel municipio -> valor da coluna `rt`, sem transformacao.
#     - nivel UF/Brasil -> media de `rt` ponderada pelos casos de cada
#       municipio. E aproximacao, esta rotulada como tal no subtitulo, e o
#       tooltip mostra quantos municipios estao com Rt > 1 na semana - esse
#       segundo numero e bem definido em qualquer nivel e serve de contraprova.
#
# O limiar e desenhado como serie constante em vez de e_mark_line(): aparece na
# legenda e nao depende da assinatura da funcao de marcacao.
#
# Sempre semanal. Rt mensal misturaria estimativas de fases distintas do surto.
# ==============================================================================

# Rt tem cauda longa em municipios pequenos (poucos casos geram razoes altas).
# O corte evita que uma semana com Rt = 40 achate visualmente todo o resto.
RT_TETO_VISUAL <- 5

mod_rt_controles <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::checkboxInput(
      ns("limitar_escala"),
      sprintf("Limitar eixo a Rt = %d", RT_TETO_VISUAL),
      value = TRUE
    ),
    shiny::tags$hr(),
    shiny::uiOutput(ns("nota_metodo"))
  )
}

mod_rt_saida <- function(id, height = "420px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height)
}


mod_rt_server <- function(id, geo, serie_semanal) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    dados <- shiny::reactive({
      serie_semanal() |>
        reagrupar_periodo("semana") |>
        calcular_indicadores(geo()) |>
        dplyr::mutate(limiar = 1)
    })
    
    agregado <- shiny::reactive(!identical(geo()$nivel, "municipio"))
    
    output$nota_metodo <- shiny::renderUI({
      shiny::tags$div(
        class = "small text-muted",
        if (agregado()) {
          shiny::tagList(
            shiny::tags$strong("Rt agregado."),
            shiny::tags$br(),
            "Média ponderada pelos casos de cada município. Aproximação: ",
            "Rt não é aditivo entre áreas."
          )
        } else {
          shiny::tagList(
            shiny::tags$strong("Rt municipal."),
            shiny::tags$br(),
            "Valor direto da coluna ", shiny::tags$code("rt"), "."
          )
        }
      )
    })
    
    output$grafico <- echarts4r::renderEcharts4r({
      d <- dados()
      shiny::req(nrow(d) > 0)
      shiny::validate(shiny::need(
        any(!is.na(d$rt)),
        "Sem estimativas de Rt disponíveis para este recorte."
      ))
      
      # Forca eixo categorico (texto "AAAA-MM-DD"): se `periodo` for passado
      # como Date, o echarts4r pode auto-detectar eixo de tempo e entregar
      # timestamp numerico ao formatter, que espera texto com "-" pra separar
      # ano/mes/dia (ver js_formato_data). Sem isso o rotulo vira
      # "undefined/undefined/<numero>" e os ticks se sobrepoem.
      d$periodo <- as.character(d$periodo)
      
      fmt_data <- js_formato_data("dia")
      js_eixo  <- htmlwidgets::JS(sprintf("function(v){ return %s; }", fmt_data))
      
      # Tabela propria para o tooltip: precisa de n_mun/n_mun_rt1, que nao
      # estao em nenhuma das series desenhadas.
      tabela_js <- jsonlite::toJSON(
        data.frame(
          rt        = round(d$rt, 2),
          n_mun     = d$n_mun,
          n_mun_rt1 = d$n_mun_rt1,
          casos     = d$casos
        ),
        dataframe = "rows", na = "null", auto_unbox = TRUE, digits = 2
      )
      
      js_tooltip <- htmlwidgets::JS(sprintf(
        "function(params){
           if (!params || !params.length) return '';
           var tab = %s;
           var r = tab[params[0].dataIndex];
           var v = params[0].axisValue;
           if (!r) return '';
           function f(x, d){
             return (x === null || x === undefined)
               ? '—' : x.toLocaleString('pt-BR', {maximumFractionDigits: d});
           }
           var cor = (r.rt !== null && r.rt > 1) ? '%s' : '%s';
           var txt = '<b>' + %s + '</b><br/>' +
                     'Rt: <b style=\"color:' + cor + '\">' + f(r.rt, 2) + '</b><br/>' +
                     'Casos na semana: ' + f(r.casos, 0);
           if (%s) {
             txt += '<br/><span style=\"opacity:.7\">' + f(r.n_mun_rt1, 0) +
                    ' de ' + f(r.n_mun, 0) + ' municípios com Rt > 1</span>';
           }
           return txt;
         }",
        tabela_js,
        PALETA_CHOCOLATE[["amargo"]], PALETA_CHOCOLATE[["taupe"]],
        fmt_data, if (agregado()) "true" else "false"
      ))
      
      teto <- if (isTRUE(input$limitar_escala)) {
        RT_TETO_VISUAL
      } else {
        max(c(d$rt, 1.5), na.rm = TRUE)
      }
      
      d |>
        echarts4r::e_charts(periodo) |>
        echarts4r::e_line(
          rt,
          name      = "Rt",
          symbol    = "none",
          lineStyle = list(color = PALETA_CHOCOLATE[["chocolate"]], width = 2),
          itemStyle = list(color = PALETA_CHOCOLATE[["chocolate"]])
        ) |>
        echarts4r::e_line(
          limiar,
          name      = "Limiar (Rt = 1)",
          symbol    = "none",
          silent    = TRUE,
          lineStyle = list(color = PALETA_CHOCOLATE[["ambar"]], width = 1.5, type = "dashed"),
          itemStyle = list(color = PALETA_CHOCOLATE[["ambar"]]),
          tooltip   = list(show = FALSE)
        ) |>
        echarts4r::e_x_axis(axisLabel = list(formatter = js_eixo)) |>
        echarts4r::e_y_axis(
          name         = "Rt",
          nameLocation = "end",
          nameGap      = 12,
          min          = 0,
          max          = teto
        ) |>
        echarts4r::e_datazoom(x_index = 0, type = "slider", bottom = 4) |>
        echarts4r::e_legend(top = 0, type = "scroll") |>
        echarts4r::e_grid(left = 60, right = 24, top = 40, bottom = 78) |>
        echarts4r::e_tooltip(trigger = "axis", formatter = js_tooltip) |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Salvar imagem")
    })
  })
}