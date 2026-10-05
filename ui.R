# ==============================================================================
# Dengue RIDE | ui.R
#
# Duas abas no navbar principal:
#   "Painel"    - o painel executivo original, inalterado (layout de pagina
#                 unica de antes, so que agora dentro de um nav_panel).
#   "Análises"  - novo: Epi Scanner (R/mod_epi_scanner_ui.R). Mapa + tabela
#                 por municipio, escopo GLOBAL (nao usa o filtro geografico
#                 da aba Painel - ver nota no topo de mod_epi_scanner_ui.R).
#
# Estrutura da aba "Painel":
#   0. cabecalho
#   1. filtros geograficos (card, tres seletores lado a lado)
#   2. KPIs
#   3. mapa (7)  |  evolucao temporal (5)
#   4. mapa de tendencia por municipio (largura total)
#   5. diagrama de controle (6)  |  heatmap (6)
#   6. dados brutos e SQL
#
# O card "Rt atual" dos KPIs usa a coluna `rt` real (numero de reproducao
# efetivo, do modelo InfoDengue). O card da secao 4 usa OUTRA metrica
# (tendencia/anomalia sazonal por municipio) e nao deve ser confundido com o
# Rt verdadeiro - ver o comentario no topo de R/mod_mapa_rt.R.
#
# TEMA_DENGUE3D e cartao_grafico() vivem em R/utils_ui.R.
# ==============================================================================

bslib::page_navbar(
  title        = "Dengue RIDE",
  theme        = TEMA_DENGUE3D,
  window_title = "Dengue RIDE — Painel Epidemiológico",
  padding      = "1rem",
  fillable     = FALSE,
  
  # --------------------------------------------------------------- Painel --
  bslib::nav_panel(
    "Painel",

    shiny::tagList(
      # z-index do dropdown dos seletores + popover que fecha ao clicar fora
      # (R/utils_ui.R). singleton(): entra so uma vez, mesmo se referenciado
      # de novo.
      dengue3d_ui_assets(),

      # ----------------------------------------------------------- cabecalho --
      shiny::tags$div(
        class = "d-flex justify-content-between align-items-end mb-3",
        shiny::tags$div(
          shiny::tags$h4(class = "mb-0 fw-semibold", "Dengue RIDE"),
          shiny::tags$span(class = "text-muted small",
                           "Painel de vigilância epidemiológica · fonte InfoDengue")
        ),
        shiny::tags$span(
          class = "text-muted small",
          sprintf("%s municípios · %d–%d", fmt_int(nrow(GEO_DICT)), ANO_MIN, ANO_MAX)
        )
      ),
      
      # ---------------------------------------------------- 1. filtros no topo --
      bslib::card(
        class = "mb-3",
        bslib::card_header(
          class = "bg-primary text-white d-flex justify-content-between align-items-center",
          shiny::tags$span(class = "fw-semibold", "Recorte geográfico"),
          shiny::tags$div(
            class = "d-flex align-items-center gap-3",
            mod_geo_filter_resumo("geo"),
            # Limpa o local selecionado (clique no mapa ou seletores) e
            # devolve o painel a visao geral. Ligado a
            # clear_selected_location() em server.R.
            shiny::actionButton(
              "limpar_selecao", "Visão geral",
              class = "btn btn-sm btn-light"
            )
          )
        ),
        bslib::card_body(
          mod_geo_filter_ui("geo")
        )
      ),
      
      # ------------------------------------------------------------- 2. KPIs --
      mod_kpi_ui("kpi"),
      
      # ----------------------------------------- 3. mapa | evolucao temporal (7/5) --
      bslib::layout_columns(
        col_widths = c(7, 5),
        row_heights = "auto",
        
        cartao_grafico(
          titulo    = "Distribuição espacial",
          saida     = mod_mapa_saida("mapa", height = "540px"),
          controles = mod_mapa_controles("mapa")
        ),
        
        # Serie temporal e curva epidemica sao leituras alternativas do mesmo
        # recorte: dividem o espaco em abas em vez de competir por altura.
        bslib::navset_card_tab(
          full_screen = TRUE,
          title = shiny::tags$span(class = "fw-semibold", "Evolução temporal"),
          
          bslib::nav_panel(
            "Série",
            bslib::card_body(
              class = "p-0",
              mod_serie_temporal_saida("serie", height = "230px")
            )
          ),
          bslib::nav_panel(
            "Curva epidêmica",
            bslib::card_body(
              class = "p-0",
              mod_curva_epidemica_saida("curva", height = "230px")
            )
          ),
          # Rotulado "(InfoDengue)" para nao se confundir com o card de tendencia
          # abaixo: este usa a coluna `rt` real (numero de reproducao efetivo,
          # estimado pelo modelo do InfoDengue); o outro e um indicador diferente.
          bslib::nav_panel(
            "Rt (InfoDengue)",
            bslib::card_body(
              class = "p-0",
              mod_rt_saida("rt", height = "230px")
            )
          ),
          
          bslib::nav_spacer(),
          bslib::nav_item(
            bslib::popover(
              gatilho_opcoes(),
              shiny::tags$h6(class = "mt-0", "Série temporal"),
              mod_serie_temporal_controles("serie"),
              shiny::tags$hr(),
              shiny::tags$h6("Curva epidêmica"),
              mod_curva_epidemica_controles("curva"),
              shiny::tags$hr(),
              shiny::tags$h6("Rt (InfoDengue)"),
              mod_rt_controles("rt"),
              title     = "Opções",
              placement = "left"
            )
          )
        )
      ),
      
      # ------------------------- 4. mapa de tendencia por municipio (largura total) --
      # Nao e o Rt real (ver nota no topo do arquivo e em R/mod_mapa_rt.R).
      cartao_grafico(
        titulo    = "Rt — Tendência por município (quadrantes)",
        saida     = mod_mapa_rt_saida("tendencia", height = "560px"),
        controles = mod_mapa_rt_controles("tendencia")
      ),
      
      # -------------- 5a. diagrama de controle + previsão (largura total) -----
      # Layout de duas colunas: gráfico à esquerda, painel de opções (controles
      # da temporada + camada de previsão: toggle de nowcasting, métricas dos
      # modelos e seleção de linhas) à direita.
      bslib::card(
        class = "mb-3",
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-primary text-white",
          shiny::tags$span(
            class = "fw-semibold",
            "Diagrama de controle (canal endêmico) & previsão de casos"
          )
        ),
        bslib::card_body(
          bslib::layout_columns(
            col_widths  = c(9, 3),
            row_heights = "520px",
            class = "g-3",
            mod_diagrama_controle_saida("diagrama", height = "100%"),
            shiny::tags$div(
              class = "overflow-auto h-100 ps-2 border-start",
              mod_diagrama_controle_painel("diagrama")
            )
          )
        )
      ),

      # ----------------------- 5b. sazonalidade (largura total) --------------
      cartao_grafico(
        titulo    = "Sazonalidade — incidência por ano e semana",
        saida     = mod_heatmap_saida("heatmap", height = "400px"),
        controles = mod_heatmap_controles("heatmap")
      ),
      
      # ------------------------------------------------ 6. dados brutos / SQL --
      bslib::navset_card_tab(
        title = shiny::tags$span(class = "fw-semibold",
                                 shiny::textOutput("titulo_recorte", inline = TRUE)),
        
        bslib::nav_panel(
          "Últimas semanas",
          bslib::card_body(
            shiny::tableOutput("tab_serie"),
            shiny::tags$p(
              class = "small text-muted mb-0",
              "Agregação feita no PostgreSQL; apenas as linhas exibidas trafegam para o R."
            )
          )
        ),
        
        bslib::nav_panel(
          "SQL gerado",
          bslib::card_body(
            shiny::tags$p(
              class = "small text-muted",
              "Consulta que o dbplyr envia ao banco para o recorte atual."
            ),
            shiny::verbatimTextOutput("sql_gerado")
          )
        )
      )
    )
  ),
  
  # ------------------------------------------------------------- Análises --
  # Epi Scanner: mapa + tabela por municipio, escopo GLOBAL (RIDE-DF inteira,
  # nao usa geo() da aba Painel). Ver R/mod_epi_scanner_ui.R.
  bslib::nav_panel(
    "Análises",
    mod_epi_scanner_ui("epi_scanner")
  )
)