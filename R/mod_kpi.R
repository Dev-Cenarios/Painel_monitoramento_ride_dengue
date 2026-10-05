# ==============================================================================
# Dengue RIDE | Modulo: Painel de KPIs
#
# Quatro value_box do bslib no topo do painel:
#   1. casos acumulados no ano mais recente com dados (total historico na nota)
#   2. incidencia da ultima semana disponivel
#   3. Rt da ultima semana com estimativa
#   4. abrangencia do recorte
#
# Os boxes sao renderizados via uiOutput em vez de fixados na UI porque a COR
# carrega informacao: a linha inteira escurece conforme a situacao se agrava,
# seguindo a rampa creme -> chocolate. `theme` do value_box e definido na
# construcao, entao os boxes precisam ser reconstruidos a cada mudanca de
# recorte.
#
# Nao consulta o banco: consome a serie semanal ja carregada pelo server.R.
# ==============================================================================

mod_kpi_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("boxes"))
}


mod_kpi_server <- function(id, geo, serie_semanal) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    indicadores <- shiny::reactive({
      d <- serie_semanal() |> calcular_indicadores(geo())
      shiny::req(nrow(d) > 0)
      
      d <- d[order(d$data_ini_se), , drop = FALSE]
      ultima <- d[nrow(d), ]
      
      ano_corrente <- as.integer(format(ultima$data_ini_se, "%Y"))
      no_ano <- as.integer(format(d$data_ini_se, "%Y")) == ano_corrente
      
      # O Rt da ultima semana pode ser NA (municipio sem estimativa naquela
      # semana). Recua ate a semana mais recente que tenha valor, e informa
      # a data para que o numero nao seja lido como se fosse de hoje.
      com_rt <- d[!is.na(d$rt), , drop = FALSE]
      
      list(
        ano           = ano_corrente,
        casos_ano     = sum(d$casos[no_ano], na.rm = TRUE),
        casos_total   = sum(d$casos, na.rm = TRUE),
        inc_ultima    = ultima$incidencia,
        casos_ultima  = ultima$casos,
        data_ultima   = ultima$data_ini_se,
        rt            = if (nrow(com_rt)) com_rt$rt[nrow(com_rt)] else NA_real_,
        data_rt       = if (nrow(com_rt)) com_rt$data_ini_se[nrow(com_rt)] else as.Date(NA),
        n_semanas     = nrow(d)
      )
    })
    
    # value_box_theme() em vez dos temas nomeados do Bootstrap: a rampa
    # chocolate precisa de controle exato do par fundo/texto, porque nas
    # tonalidades claras o texto tem de ser escuro para permanecer legivel.
    tom <- function(nome, claro = FALSE) {
      bslib::value_box_theme(
        bg = PALETA_CHOCOLATE[[nome]],
        fg = if (claro) PALETA_CHOCOLATE[["amargo"]] else "#FFFFFF"
      )
    }
    
    # Faixas do MS traduzidas em intensidade de cor: quanto mais escuro,
    # maior a incidencia. Mesma logica do mapa e do heatmap.
    tema_incidencia <- function(inc) {
      if (is.na(inc)) return(tom("baunilha", claro = TRUE))
      if (inc >= 300) return(tom("amargo"))
      if (inc >= 100) return(tom("cacau"))
      tom("areia", claro = TRUE)
    }
    
    tema_rt <- function(rt) {
      if (is.na(rt)) return(tom("baunilha", claro = TRUE))
      if (rt > 1)    return(tom("amargo"))
      tom("areia", claro = TRUE)
    }
    
    output$boxes <- shiny::renderUI({
      k <- indicadores()
      g <- geo()
      
      n_mun <- switch(
        g$nivel,
        brasil    = nrow(GEO_DICT),
        uf        = sum(GEO_DICT$uf_ibge == g$uf_ibge),
        municipio = 1L
      )
      
      bslib::layout_columns(
        fill  = FALSE,
        class = "mb-1",
        
        bslib::value_box(
          title = sprintf("Casos acumulados em %d", k$ano),
          value = fmt_int(k$casos_ano),
          theme = tom("chocolate"),
          shiny::tags$span(
            class = "small",
            sprintf("%s no histórico completo", fmt_int(k$casos_total))
          )
        ),
        
        bslib::value_box(
          title = "Incidência na última semana",
          value = fmt_num(k$inc_ultima, 1),
          theme = tema_incidencia(k$inc_ultima),
          shiny::tags$span(
            class = "small",
            sprintf("por 100 mil · %s casos · SE de %s",
                    fmt_int(k$casos_ultima),
                    format(k$data_ultima, "%d/%m/%Y"))
          )
        ),
        
        bslib::value_box(
          title = "Rt atual",
          value = if (is.na(k$rt)) "—" else fmt_num(k$rt, 2),
          theme = tema_rt(k$rt),
          shiny::tags$span(
            class = "small",
            if (is.na(k$rt)) {
              "sem estimativa disponível"
            } else if (k$rt > 1) {
              sprintf("acima do limiar · %s", format(k$data_rt, "%d/%m/%Y"))
            } else {
              sprintf("abaixo do limiar · %s", format(k$data_rt, "%d/%m/%Y"))
            }
          )
        ),
        
        bslib::value_box(
          title = "Recorte",
          value = g$rotulo,
          theme = tom("taupe"),
          shiny::tags$span(
            class = "small",
            sprintf("%s município(s) · %s semanas",
                    fmt_int(n_mun), fmt_int(k$n_semanas))
          )
        )
      )
    })
  })
}