# ==============================================================================
# Dengue RIDE | Modulo: Analises (Epi Scanner) - UI
#
# Aba de analise de longo prazo, inspirada no Epi Scanner do InfoDengue: mapa
# coropletico + tabela lado a lado, comparando os municipios da RIDE-DF entre
# si (Rt e casos acumulados desde 2015). Por design, NAO participa do filtro
# geografico global (geo()) - mesmo raciocinio do mapa de tendencia
# (R/mod_mapa_rt.R): o valor esta em comparar os municipios entre si; um
# recorte para 1 municipio so esvaziaria a comparacao.
#
# Arquivo separado de mod_epi_scanner_server.R por pedido explicito - os
# demais modulos do painel usam um unico arquivo por modulo (ver
# R/mod_mapa.R, por exemplo); aqui a convencao foi trocada de proposito.
# ==============================================================================

mod_epi_scanner_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    bslib::card(
      class = "mb-3",
      bslib::card_body(
        shiny::radioButtons(
          ns("metrica"), "Métrica do mapa",
          choices = c(
            "Semanas com Rt > 1 (desde 2015)"      = "semanas_rt_alto",
            "Rt máximo (temporada atual)"          = "rt_max_atual",
            "Total acumulado de casos (desde 2015)" = "casos_totais"
          ),
          selected = "casos_totais",
          inline   = TRUE
        )
      )
    ),
    
    bslib::layout_columns(
      col_widths = c(6, 6),
      
      cartao_grafico(
        titulo = "Mapa por município",
        saida  = echarts4r::echarts4rOutput(ns("mapa"), height = "520px")
      ),
      
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-primary text-white",
          shiny::tags$span(class = "fw-semibold", "Tabela por município")
        ),
        bslib::card_body(
          DT::DTOutput(ns("tabela"))
        )
      )
    )
  )
}
