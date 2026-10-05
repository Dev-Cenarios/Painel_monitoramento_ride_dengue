# ==============================================================================
# Dengue RIDE | Modulo: Analises (Epi Scanner) - Server
#
# Agregado GLOBAL por municipio (toda a RIDE-DF, ignora geo()) com tres
# metricas de longo prazo: semanas com Rt > 1, Rt maximo da temporada atual e
# casos acumulados - tudo desde 2015 (ANO_MIN da base).
#
# FONTE: SERIE_MUNICIPIOS (global.R) - a mesma tabela em memoria que o mapa de
# tendencia (R/mod_mapa_rt.R) usa, uma linha por municipio/semana, ja com
# `ano`, `casos` e `rt` agregados. Este modulo NAO consulta o DuckDB: as 3
# metricas sao aritmetica de grupo sobre ~25 mil linhas.
#
#   Antes, dados_municipio() disparava uma segunda agregacao da base inteira
#   no banco (group_by(municipio, se) + collect()) a CADA primeira abertura
#   da aba "Analises" em cada sessao nova. Como o resultado e constante (so
#   muda se o Parquet mudar), agora ele sai de SERIE_MUNICIPIOS e um
#   bindCache() com chave fixa o calcula uma unica vez por processo,
#   compartilhado entre todas as sessoes.
#
# Por que a etapa final fica em R e nao em SQL: as metricas indexam um vetor
# por posicao dentro do grupo (ex.: `max(rt[ano == ANO_MAX])`), sintaxe que o
# dbplyr nao traduz ("not available in this SQL variant"). Com a base ja em
# memoria isso deixa de importar.
# ==============================================================================

mod_epi_scanner_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    dados_municipio <- shiny::reactive({
      SERIE_MUNICIPIOS |>
        dplyr::group_by(municipio_geocodigo) |>
        dplyr::summarise(
          semanas_rt_alto = sum(rt > 1.0 & ano >= 2015L, na.rm = TRUE),
          # max() de um municipio sem nenhuma semana no ano atual devolve
          # -Inf com aviso (nao NA) - por isso o if(length()) e o guard abaixo.
          rt_max_atual = {
            v <- rt[ano == ANO_MAX]
            if (length(v)) suppressWarnings(max(v, na.rm = TRUE)) else NA_real_
          },
          casos_totais = sum(casos[ano >= 2015L], na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          rt_max_atual = dplyr::if_else(is.infinite(rt_max_atual), NA_real_, rt_max_atual)
        ) |>
        dplyr::left_join(
          dplyr::distinct(GEO_DICT, municipio_geocodigo, municipio_nome, uf_sigla),
          by = "municipio_geocodigo"
        ) |>
        dplyr::arrange(municipio_nome)
    }) |>
      # Chave fixa: o resultado nao depende de nenhum input, so de
      # SERIE_MUNICIPIOS (constante no processo). Calculado uma vez, servido
      # do cache app-level para todas as sessoes seguintes.
      shiny::bindCache("epi_scanner_municipio")
    
    rotulo_metrica <- shiny::reactive({
      switch(input$metrica,
             semanas_rt_alto = "Semanas com Rt > 1",
             rt_max_atual     = sprintf("Rt máximo (%d)", ANO_MAX),
             casos_totais     = "Casos acumulados",
             "Valor"
      )
    })
    
    # -- mapa -----------------------------------------------------------------
    output$mapa <- echarts4r::renderEcharts4r({
      d <- dados_municipio()
      shiny::req(nrow(d) > 0, input$metrica)
      
      malha <- carregar_geojson("BASE_MUN")
      shiny::validate(shiny::need(!is.null(malha), "Malha indisponível."))
      
      d$chave <- as.character(d$municipio_geocodigo)
      d$valor <- round(d[[input$metrica]], 2)
      
      # Mesmo padrao de tooltip codigo->nome do mapa principal (R/mod_mapa.R):
      # a juncao do mapa e por codigo IBGE, o nome so aparece na exibicao.
      nomes_js <- jsonlite::toJSON(
        stats::setNames(as.list(d$municipio_nome), d$chave),
        auto_unbox = TRUE
      )
      
      js_tooltip <- htmlwidgets::JS(sprintf(
        "function(p){
           var nomes = %s;
           var n = nomes[p.name] || p.name;
           if (p.value === undefined || p.value === null || isNaN(p.value)) {
             return '<b>' + n + '</b><br/>sem dado';
           }
           return '<b>' + n + '</b><br/>%s: ' +
                  p.value.toLocaleString('pt-BR', {maximumFractionDigits: 2});
         }",
        nomes_js, rotulo_metrica()
      ))
      
      d |>
        echarts4r::e_charts(chave) |>
        echarts4r::e_map_register("BASE_MUN", malha) |>
        echarts4r::e_map(
          valor,
          map          = "BASE_MUN",
          name         = rotulo_metrica(),
          nameProperty = GEO_NAME_PROPERTY,
          roam         = TRUE,
          itemStyle    = list(borderColor = "#FFFFFF", borderWidth = 0.4),
          # Ver nota identica em R/mod_mapa.R: sem isso o ECharts mostra o
          # codigo IBGE cru como rotulo ao passar o mouse (emphasis.label).
          label        = list(show = FALSE),
          emphasis     = list(
            itemStyle = list(areaColor = PALETA_CHOCOLATE[["ambar"]]),
            label     = list(show = FALSE)
          )
        ) |>
        echarts4r::e_visual_map(
          min       = 0,
          max       = max(d$valor, na.rm = TRUE),
          orient    = "vertical",
          left      = "left",
          bottom    = "6%",
          inRange   = list(color = RAMPA_CHOCOLATE),
          formatter = htmlwidgets::JS(
            "function(v){return v.toLocaleString('pt-BR',{maximumFractionDigits:1});}"
          )
        ) |>
        echarts4r::e_tooltip(trigger = "item", formatter = js_tooltip) |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Salvar imagem")
    })
    
    # -- tabela: Top 10 pela metrica selecionada -------------------------------
    # A mesma metrica que colore o mapa (input$metrica) define qual coluna
    # ordena o ranking - municipios sem valor valido pra ela (ex.: sem semana
    # nenhuma no ano atual, no caso de "Rt maximo") ficam de fora do Top 10
    # em vez de aparecerem com "—" ocupando uma posicao do ranking.
    dados_top10 <- shiny::reactive({
      shiny::req(input$metrica)
      dados_municipio() |>
        dplyr::filter(!is.na(.data[[input$metrica]])) |>
        dplyr::arrange(dplyr::desc(.data[[input$metrica]])) |>
        utils::head(10)
    })
    
    titulo_tabela <- shiny::reactive({
      switch(input$metrica,
             semanas_rt_alto = "Top 10 — Semanas com Rt > 1 (desde 2015)",
             rt_max_atual     = sprintf("Top 10 — Rt máximo (temporada %d)", ANO_MAX),
             casos_totais     = "Top 10 — Casos acumulados (desde 2015)",
             "Top 10"
      )
    })
    
    output$tabela <- DT::renderDT({
      d <- dados_top10()
      shiny::req(nrow(d) > 0)
      
      tabela <- d |>
        dplyr::transmute(
          `#`                 = seq_len(dplyr::n()),
          Município          = municipio_nome,
          UF                 = uf_sigla,
          `Semanas Rt > 1`   = semanas_rt_alto,
          `Rt máximo`        = round(rt_max_atual, 2),
          `Casos acumulados` = casos_totais
        )
      
      # Coluna que corresponde a metrica selecionada - destacada em negrito e
      # usada como ordenacao padrao da tabela (0-based, convencao do DataTables/JS).
      nome_coluna <- switch(input$metrica,
                            semanas_rt_alto = "Semanas Rt > 1",
                            rt_max_atual     = "Rt máximo",
                            casos_totais     = "Casos acumulados"
      )
      idx_coluna <- match(nome_coluna, names(tabela)) - 1L
      
      DT::datatable(
        tabela,
        rownames = FALSE,
        caption  = titulo_tabela(),
        options  = list(
          # dom = "t": so a tabela, sem busca/paginacao/menu - sao sempre
          # exatamente 10 linhas, essas opcoes so poluiriam.
          dom      = "t",
          order    = list(list(idx_coluna, "desc")),
          language = list(zeroRecords = "Nenhum município com dado suficiente")
        )
      ) |>
        DT::formatRound("Rt máximo", digits = 2) |>
        DT::formatCurrency("Casos acumulados", currency = "", interval = 3, mark = ".", digits = 0) |>
        DT::formatStyle(nome_coluna, fontWeight = "bold")
    })
  })
}