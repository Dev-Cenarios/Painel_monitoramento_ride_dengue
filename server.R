# ==============================================================================
# Dengue RIDE | server.R
# Orquestra os modulos e monta o pipeline lazy de consulta.
# ==============================================================================

function(input, output, session) {
  
  # -- estado global: local selecionado -----------------------------------
  # `selected_location` e a unica fonte de verdade da "area em foco" do
  # painel. NULL = visao geral (nada destacado); caso contrario, uma lista
  # list(nivel, uf_ibge, geocodigo) — nivel = "municipio", vinda de um clique
  # no mapa ou da escolha no seletor de municipio. Desacopla quem PEDE o foco (mapa, e futuros
  # componentes) de quem o APLICA (modulo geografico), evitando dependencia
  # circular entre os modulos.
  selected_location <- shiny::reactiveVal(NULL)

  # `setSelectedLocation`: registra uma area como selecao corrente. O
  # modulo geografico observa `selected_location` via `foco_externo` e
  # reposiciona os seletores; a partir dai geo() se propaga para todos os
  # graficos (KPIs, serie, curva, Rt, heatmap, diagrama).
  set_selected_location <- function(loc) {
    selected_location(loc)
  }

  # Limpa a selecao e volta a visao geral (nivel agregado da base). O campo
  # `.nonce` garante que acionar o botao repetidamente sempre dispare a
  # cascata de reset em mod_geo_filter, mesmo que o estado anterior ja
  # fosse "brasil" (reactiveVal so notifica quando o valor muda).
  .reset_nonce <- 0L
  clear_selected_location <- function() {
    .reset_nonce <<- .reset_nonce + 1L
    selected_location(list(
      nivel     = "brasil",
      uf_ibge   = NA_integer_,
      geocodigo = NA_integer_,
      .nonce    = .reset_nonce
    ))
  }

  # -- modulo de filtro geografico ------------------------------------------
  # Retorna reactive com list(nivel, uf_ibge, geocodigo, rotulo).
  geo <- mod_geo_filter_server(
    "geo",
    foco_externo = shiny::reactive(selected_location())
  )
  
  # -- serie semanal compartilhada -------------------------------------------
  # UMA consulta por recorte, seis modulos consumindo: KPIs, diagrama de
  # controle, serie temporal, curva epidemica, Rt e heatmap. Sem isso cada um
  # dispararia a mesma agregacao no Postgres.
  serie_semanal <- shiny::reactive({
    shiny::withProgress(message = "Carregando série temporal…", value = 0.5, {
      consultar_serie_semanal(geo())
    })
  }) |>
    shiny::bindCache(geo())
  
  # -- KPIs ------------------------------------------------------------------
  mod_kpi_server("kpi", geo = geo, serie_semanal = serie_semanal)
  
  # -- mapa coropletico ------------------------------------------------------
  # Desenha SEMPRE a base RIDE-DF inteira (nao recorta por UF). Devolve
  # reactive com o pedido de foco gerado por clique numa area e recebe
  # `selected_location` para destacar o municipio escolhido (estado 'select'
  # do ECharts, sem re-render). Nao recebe geo(): o mapa nao filtra.
  clique_mapa <- mod_mapa_server(
    "mapa",
    selected_location = selected_location
  )

  # clique_mapa() traz nivel = "municipio" (clique numa area) ou
  # nivel = "brasil" (botao "Visao geral" sobreposto ao mapa).
  shiny::observeEvent(clique_mapa(), {
    pedido <- clique_mapa()
    if (identical(pedido$nivel, "brasil")) {
      clear_selected_location()
    } else {
      set_selected_location(pedido)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Botao "Visao geral" no cabecalho do card de recorte (ui.R) - mesmo efeito.
  shiny::observeEvent(input$limpar_selecao, {
    clear_selected_location()
  }, ignoreInit = TRUE)

  # -- seletor de recorte -> estado global ------------------------------------
  # Caminho inverso do clique no mapa: quando o municipio muda pelos seletores
  # do card "Recorte geografico", geo() ja move KPIs, serie, curva, Rt, heatmap
  # e diagrama; aqui devolvemos o mesmo local a `selected_location` para que o
  # mapa e a tendencia o destaquem, exatamente como num clique. Sem isso so os
  # graficos ligados a geo() reagiam ao seletor.
  # Nao ha laco: so escrevemos se o local DIFERE do atual, e o modulo geografico
  # ignora um foco que ja e o seu recorte (ver mod_geo_filter). Abrangencia
  # Estado / RIDE-DF nao destaca municipio: limpa o destaque (NULL nao aciona o
  # modulo geografico - ignoreNULL).
  shiny::observeEvent(geo(), {
    g     <- geo()
    atual <- shiny::isolate(selected_location())
    cod_atual <- if (!is.null(atual) && identical(atual$nivel, "municipio")) {
      as.integer(atual$geocodigo)
    } else {
      NA_integer_
    }

    if (identical(g$nivel, "municipio")) {
      if (!identical(cod_atual, as.integer(g$geocodigo))) {
        set_selected_location(list(
          nivel     = "municipio",
          uf_ibge   = as.integer(g$uf_ibge),
          geocodigo = as.integer(g$geocodigo)
        ))
      }
    } else if (!is.na(cod_atual)) {
      selected_location(NULL)
    }
  }, ignoreInit = TRUE)
  
  # -- diagrama de controle + previsao -------------------------------------
  # Canal endemico por quartis (Q1/Q2/Q3) + incidencia da temporada. Reaproveita
  # serie_semanal() para o historico; a camada de previsao chama
  # get_dengue_forecast() (backend em forecasting/) com bindCache por recorte,
  # por isso tambem recebe geo().
  mod_diagrama_controle_server("diagrama", serie_semanal = serie_semanal, geo = geo)
  
  # -- series temporais ------------------------------------------------------
  mod_serie_temporal_server("serie", geo = geo, serie_semanal = serie_semanal)
  mod_rt_server("rt",                geo = geo, serie_semanal = serie_semanal)
  
  # -- curva epidemica ---------------------------------------------------------
  # Por temporada, igual ao diagrama de controle: reaproveita serie_semanal(),
  # nao dispara consulta propria ao trocar temporada/temporadas de referencia.
  mod_curva_epidemica_server("curva", serie_semanal = serie_semanal)
  
  # -- sazonalidade ----------------------------------------------------------
  mod_heatmap_server("heatmap", geo = geo, serie_semanal = serie_semanal)
  
  
  # -- tendencia por municipio (mapa de quadrantes) ---------------------------
  # Nao recebe geo() nem serie_semanal(): sempre compara os municipios entre
  # si (ver nota no topo de R/mod_mapa_rt.R). Le SERIE_MUNICIPIOS direto do
  # global.R, sem consultar o DuckDB de novo.
  # Recebe `selected_location` apenas para DESTACAR o municipio clicado
  # (formatacao condicional) - o conjunto de pontos continua completo.
  mod_mapa_rt_server("tendencia", selected_location = selected_location)
  
  # -- aba Analises (Epi Scanner) ---------------------------------------------
  # Tambem nao recebe geo(): mapa+tabela global por municipio, de proposito
  # (ver nota no topo de R/mod_epi_scanner_ui.R).
  mod_epi_scanner_server("epi_scanner")
  
  # -- pipeline lazy ---------------------------------------------------------
  # Ainda NAO e uma consulta executada: e a receita do SELECT.
  dados_geo <- shiny::reactive({
    aplicar_filtro_geo(tbl_dengue(), geo())
  })
  
  output$titulo_recorte <- shiny::renderText({
    geo()$rotulo
  })
  
  # -- tabela das ultimas semanas (agregada no banco) ------------------------
  serie <- shiny::reactive({
    dados_geo() |>
      dplyr::group_by(data_ini_se, se) |>
      dplyr::summarise(
        casos     = sum(casos, na.rm = TRUE),
        casos_est = sum(casos_est, na.rm = TRUE),
        pop       = sum(pop, na.rm = TRUE),
        .groups   = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(data_ini_se)) |>
      head(12) |>
      dplyr::collect()
  }) |>
    shiny::bindCache(geo())
  
  output$tab_serie <- shiny::renderTable({
    serie() |>
      dplyr::mutate(
        `Semana`        = format(as.Date(data_ini_se), "%d/%m/%Y"),
        `SE`            = as.character(se),
        `Casos`         = fmt_int(casos),
        `Casos est.`    = fmt_num(casos_est, 0),
        `Inc. /100k`    = fmt_num(dplyr::if_else(pop > 0, casos / pop * 1e5, NA_real_), 1),
        .keep = "none"
      )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "lrrrr")
  
  # -- SQL gerado (transparencia do lazy eval) -------------------------------
  output$sql_gerado <- shiny::renderText({
    q <- dados_geo() |>
      dplyr::group_by(data_ini_se, se) |>
      dplyr::summarise(
        casos     = sum(casos, na.rm = TRUE),
        casos_est = sum(casos_est, na.rm = TRUE),
        pop       = sum(pop, na.rm = TRUE),
        .groups   = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(data_ini_se)) |>
      head(12)
    
    paste(as.character(dbplyr::sql_render(q)), collapse = "\n")
  })
}