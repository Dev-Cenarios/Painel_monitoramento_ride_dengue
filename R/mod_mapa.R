# ==============================================================================
# Dengue RIDE | Modulo: mapa coropletico (echarts4r)
#
# GRANULARIDADE: sempre municipal, e SEMPRE a base inteira.
#   A base cobre a RIDE-DF (34 municipios em DF, GO e MG). O mapa desenha
#   TODOS eles o tempo todo (malha BASE_MUN) - clicar num municipio NAO
#   recorta o mapa para a UF dele (isso escondia Brasilia e os de MG ao
#   clicar num de GO). O clique so DESTACA o municipio (estado 'select' do
#   ECharts, ver FEEDBACK VISUAL abaixo); quem filtra os demais graficos e o
#   geo() global, nao o mapa.
#
# As malhas ja chegam recortadas ao escopo: preparar_malhas_base(), em
# global.R, filtra os GeoJSON do IBGE para os geocodigos presentes no Parquet e
# pre-popula o cache (BASE_MUN). Sem isso o mapa desenharia os 246 municipios
# de Goias quando a base tem 29, e os ausentes ficariam indistinguiveis de
# municipios com zero casos.
#
# Metrica: incidencia = sum(casos) / pop * 100.000.
#   Nao usamos media de p_inc100k entre municipios: incidencia e razao, e a
#   media aritmetica de razoes ignora o peso populacional de cada municipio.
#   max(pop) e nao sum(pop) no periodo - somar populacao ao longo das semanas
#   multiplicaria o denominador pelo numero de semanas.
#
# Contrato de saida (reactive): NULL ou lista com $nivel/$uf_ibge/$geocodigo,
# consumida pelo modulo geografico para reposicionar o filtro global.
#
# FEEDBACK VISUAL (entrada `selected_location`): reactive com o estado global
# de local selecionado (server.R). Um observer traduz esse estado numa
# mensagem para o handler JS 'dengue_map_highlight' (mod_mapa_saida), que
# marca o poligono escolhido com o estado 'select' do ECharts (areaColor
# azul) e reduz a opacidade do restante da serie - SEM re-renderizar o mapa.
# O reset (botao "Visao geral" -> clear_selected_location()) chega como
# nivel != "municipio": o handler faz unselect e devolve a opacidade cheia.
# ==============================================================================

# PECAS_INCIDENCIA vive em global.R: a mesma escala de cor e usada aqui e no
# heatmap de sazonalidade, para que a leitura seja comparavel entre os dois.


mod_mapa_controles <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::radioButtons(
      ns("metrica"), "Métrica",
      choices = c("Incidência /100 mil" = "incidencia",
                  "Casos notificados"   = "casos"),
      selected = "incidencia"
    ),
    shiny::sliderInput(
      ns("anos"), "Período (ano)",
      min   = ANO_MIN,
      max   = ANO_MAX,
      value = c(max(ANO_MIN, ANO_MAX - 1), ANO_MAX),
      step  = 1,
      sep   = ""
    ),
    shiny::helpText(
      shiny::tags$small("Clique em uma área do mapa para focar o painel nela.")
    )
  )
}

mod_mapa_saida <- function(id, height = "540px") {
  ns <- shiny::NS(id)

  shiny::tagList(
    # Script unico da pagina (singleton): (a) handler de destaque do mapa e
    # (b) "wake up" do canal do input de clique.
    #
    # (a) DESTAQUE. Mecanismo: estado 'select' do ECharts (persistente,
    #     casado por nome = geocodigo) para o poligono escolhido + setOption
    #     reduzindo a opacidade do resto da serie. NAO usa highlight/downplay
    #     (emphasis): num map series com coordinateSystem 'geo' o dispatch de
    #     'highlight' por nome nao aplica estado de forma confiavel; 'select'
    #     aplica. Handler proprio (e nao e_dispatch_action_p direto) porque na
    #     troca de UF o mapa e RE-RENDERIZADO e a mensagem chega junto do
    #     novo widget, sem ordem garantida - a aplicacao e re-afirmada 4x
    #     numa janela de ~480 ms para cobrir essa corrida.
    #
    # (b) WAKE UP. O primeiro Shiny.setInputValue disparado de dentro do
    #     handler de clique do ECharts as vezes fica so no cache do cliente e
    #     nao chega ao servidor - o pipeline de inputs do Shiny ainda nao
    #     "acordou" para aquele input. Ao conectar, registramos a chave do
    #     input de clique com "" (SEM priority:'event', entao nada dispara no
    #     servidor: o eventReactive de input$clique cai no req(!is.na(cod))).
    #     Assim o primeiro clique real ja encontra o canal pronto.
    shiny::singleton(shiny::tags$script(shiny::HTML(sprintf("
      Shiny.addCustomMessageHandler('dengue_map_highlight', function(m){
        var n = 0;
        function apply(){
          n++;
          var el = document.getElementById(m.id);
          var chart = null;
          if (el && window.echarts) {
            chart = echarts.getInstanceByDom(el) ||
                    (el.firstElementChild && echarts.getInstanceByDom(el.firstElementChild));
          }
          if (chart) {
            var opt = chart.getOption();
            var nData = (opt.series && opt.series[0] && opt.series[0].data) ? opt.series[0].data.length : 0;
            var hasSel = (m.name !== null && m.name !== undefined && m.name !== '');
            // opacidade do restante da serie: reduzida quando ha selecao
            chart.setOption({ series: [{
              selectedMode: 'single',
              itemStyle: { opacity: hasSel ? 0.3 : 1 },
              select: { itemStyle: { opacity: 1 } }
            }]});
            // limpa qualquer selecao anterior (unselect sem alvo nao zera tudo)
            if (nData > 0) {
              var all = [];
              for (var i = 0; i < nData; i++) all.push(i);
              chart.dispatchAction({ type: 'unselect', seriesIndex: 0, dataIndex: all });
            }
            if (hasSel) {
              chart.dispatchAction({ type: 'select', seriesIndex: 0, name: String(m.name) });
            }
          }
          if (n < 4) setTimeout(apply, 120);
        }
        apply();
      });

      (function(){
        var cliqueId = '%s';
        function wake(){ try { Shiny.setInputValue(cliqueId, ''); } catch (e) {} }
        if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected &&
            Shiny.shinyapp.isConnected()) {
          wake();
        } else {
          document.addEventListener('shiny:connected', wake);
        }
      })();
    ", ns("clique"))))),

    # Botao "Visao geral" sobreposto ao canto do mapa: limpa a selecao e
    # volta a visualizacao padrao (toda a base, sem destaque). Fica sempre
    # visivel - quando nada esta selecionado, e um no-op inofensivo. Canto
    # superior esquerdo: o toolbox (salvar imagem) fica no direito e o
    # visual map no inferior esquerdo, entao esse canto esta livre.
    shiny::div(
      class = "position-relative",
      shiny::actionButton(
        ns("limpar"),
        label = "Visão geral",
        icon  = shiny::icon("rotate-left"),
        class = "btn btn-sm btn-light shadow-sm",
        style = paste(
          "position:absolute; top:8px; left:8px; z-index:5;",
          "padding:.15rem .5rem; font-size:.8rem;"
        )
      ),
      echarts4r::echarts4rOutput(ns("mapa"), height = height)
    )
  )
}


mod_mapa_server <- function(id, selected_location = shiny::reactive(NULL)) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Malha unica: a base RIDE-DF inteira, sempre. Nao depende de geo() - o
    # mapa nunca recorta para uma UF, so destaca o municipio selecionado.
    # Constante (nao reactive): o render so re-executa por metrica/periodo.
    MALHA <- "BASE_MUN"

    # -- consulta agregada (tudo no DuckDB) ---------------------------------
    dados <- shiny::reactive({
      shiny::req(input$anos)

      d1 <- as.Date(sprintf("%d-01-01", input$anos[1]))
      d2 <- as.Date(sprintf("%d-12-31", input$anos[2]))

      tbl_dengue() |>
        dplyr::filter(data_ini_se >= !!d1, data_ini_se <= !!d2) |>
        dplyr::group_by(uf_ibge, municipio_geocodigo) |>
        dplyr::summarise(
          casos   = sum(casos, na.rm = TRUE),
          pop     = max(pop, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          uf_ibge = as.integer(uf_ibge),
          casos   = as.numeric(casos),
          pop     = as.numeric(pop)
        )
    }) |>
      shiny::bindCache(input$anos)

    # -- junta com a lista completa de areas (sem dado != ausente) ----------
    # Sem isso, um municipio sem notificacao no periodo sumiria do mapa;
    # queremos o poligono desenhado como "Sem casos". `areas` e SEMPRE a
    # base inteira (GEO_DICT), nunca recortada por UF.
    dados_mapa <- shiny::reactive({
      d <- dados()

      areas <- GEO_DICT |>
        dplyr::transmute(
          chave  = as.character(municipio_geocodigo),
          rotulo = municipio_nome,
          uf_ibge
        )
      
      chave_dados <- dplyr::transmute(
        d, chave = as.character(municipio_geocodigo), casos, pop
      )
      
      areas |>
        dplyr::left_join(chave_dados, by = "chave") |>
        dplyr::mutate(
          casos = dplyr::coalesce(casos, 0),
          valor = if (identical(input$metrica, "incidencia")) {
            dplyr::if_else(!is.na(pop) & pop > 0, casos / pop * 1e5, NA_real_)
          } else {
            casos
          },
          valor = round(valor, 1)
        )
    })
    
    rotulo_metrica <- shiny::reactive({
      if (identical(input$metrica, "incidencia")) "Incidência /100 mil" else "Casos"
    })
    
    # -- render --------------------------------------------------------------
    # Depende so de dados_mapa() (metrica + periodo). NAO depende de geo() /
    # do municipio clicado - clicar no mapa nao redesenha nada, so dispara o
    # 'select' pelo handler JS.
    output$mapa <- echarts4r::renderEcharts4r({
      chave <- MALHA
      malha <- carregar_geojson(chave)
      
      shiny::validate(shiny::need(
        !is.null(malha),
        sprintf(
          "Malha '%s' indisponível. Verifique os arquivos em '%s' e a mensagem
           'Malhas recortadas' no console de inicialização.",
          chave, GEO_DIR
        )
      ))
      
      d <- dados_mapa()
      shiny::req(nrow(d) > 0)
      
      # Dicionario codigo -> nome legivel, injetado no tooltip.
      # A juncao do mapa e feita por codigo (robusta a acento/caixa); o nome
      # so aparece na exibicao.
      nomes_js <- jsonlite::toJSON(
        stats::setNames(as.list(d$rotulo), d$chave),
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
                  p.value.toLocaleString('pt-BR', {maximumFractionDigits: 1});
         }",
        nomes_js, rotulo_metrica()
      ))
      
      # setTimeout minimo: tira o setInputValue de dentro do stack do evento
      # de clique do zrender/ECharts e o entrega num macrotask limpo, com o
      # pipeline de inputs do Shiny ja ocioso. Junto com o "wake up" em
      # mod_mapa_saida, garante que ate o PRIMEIRO clique da sessao registre.
      js_click <- htmlwidgets::JS(sprintf(
        "function(p){
           if (p && p.name) {
             var alvo = p.name;
             setTimeout(function(){
               Shiny.setInputValue('%s', alvo, {priority: 'event'});
             }, 60);
           }
         }",
        ns("clique")
      ))
      
      grafico <- d |>
        echarts4r::e_charts(chave) |>
        echarts4r::e_map_register(chave, malha) |>
        echarts4r::e_map(
          valor,
          map          = chave,
          name         = rotulo_metrica(),
          nameProperty = GEO_NAME_PROPERTY,
          roam         = TRUE,
          itemStyle    = list(borderColor = "#FFFFFF", borderWidth = 0.4),
          # O "nome" interno de cada area, pro ECharts, e o codigo IBGE
          # (nameProperty = codarea - e o que permite juntar os dados por
          # codigo). Por padrao o ECharts desenha um rotulo com esse "nome"
          # sobre a area ao passar o mouse (emphasis.label), entao sem
          # desliga-lo explicitamente aparece o codigo cru ali, mesmo com o
          # tooltip customizado (abaixo) ja resolvendo o nome legivel certo.
          label        = list(show = FALSE),
          emphasis     = list(
            itemStyle = list(areaColor = TONS_AZUL[["300"]]),
            label     = list(show = FALSE)
          ),
          # Estado "select": e por ele que o destaque persistente do local
          # escolhido e aplicado (dispatchAction 'select' pelo handler JS em
          # mod_mapa_saida). areaColor azul (fora da rampa terracota) + opacidade cheia; o restante da
          # serie recebe opacity reduzida via setOption no mesmo handler.
          # selectedMode = "single": um novo 'select' substitui o anterior.
          selectedMode = "single",
          select       = list(
            itemStyle = list(areaColor = PALETA_DENGUE[["azul_acao"]], opacity = 1),
            label     = list(show = FALSE)
          )
        )
      
      grafico <- if (identical(input$metrica, "incidencia")) {
        grafico |>
          echarts4r::e_visual_map(
            type   = "piecewise",
            pieces = PECAS_INCIDENCIA,
            orient = "vertical",
            left   = "left",
            bottom = "6%",
            itemWidth = 14, itemHeight = 12
          )
      } else {
        grafico |>
          echarts4r::e_visual_map(
            min    = 0,
            max    = max(d$valor, na.rm = TRUE),
            orient = "vertical",
            left   = "left",
            bottom = "6%",
            inRange = list(color = RAMPA_DENGUE),
            formatter = htmlwidgets::JS(
              "function(v){return v.toLocaleString('pt-BR',{maximumFractionDigits:0});}"
            )
          )
      }
      
      grafico |>
        echarts4r::e_tooltip(trigger = "item", formatter = js_tooltip) |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Salvar imagem") |>
        echarts4r::e_text_style(fontFamily = FONTE_GRAFICO) |>
        echarts4r::e_on(query = NULL, handler = js_click, event = "click")
    })
    
    # -- feedback visual: destaque do local selecionado -------------------
    # Traduz `selected_location` (estado global) numa mensagem para o handler
    # 'dengue_map_highlight' (ver mod_mapa_saida), que marca o poligono com o
    # estado 'select' do ECharts e reduz a opacidade do resto - SEM re-
    # renderizar o widget. Depende tambem de dados_mapa(): quando metrica ou
    # periodo re-renderizam o mapa, o observer re-roda e re-emite a mensagem.
    # O handler ainda re-afirma o destaque 4x numa janela curta, cobrindo a
    # corrida com a re-renderizacao.
    shiny::observe({
      sl     <- selected_location()
      chaves <- tryCatch(dados_mapa()$chave, error = function(e) character(0))

      cod <- NULL
      if (!is.null(sl) && identical(sl$nivel, "municipio") &&
          !is.null(sl$geocodigo) && !is.na(sl$geocodigo)) {
        c0 <- as.character(sl$geocodigo)
        # O mapa desenha a base inteira, entao o municipio sempre esta la;
        # o `%in%` e so guarda contra um dado_mapa() transitorio vazio.
        if (c0 %in% chaves) cod <- c0
      }

      session$sendCustomMessage("dengue_map_highlight", list(
        id   = session$ns("mapa"),
        name = cod
      ))
    })

    # -- pedido de foco para o filtro global ------------------------------
    # Dois gatilhos alimentam o mesmo canal de saida:
    #   - clique num municipio -> nivel = "municipio" (geocodigo de 7 digitos,
    #     os 2 primeiros sao a UF);
    #   - botao "Visao geral" do mapa -> nivel = "brasil" (reset). O campo
    #     .ts garante que cliques repetidos no botao sempre disparem o
    #     observeEvent la em server.R.
    pedido_foco <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$clique, {
      cod <- suppressWarnings(as.integer(input$clique))
      shiny::req(!is.na(cod))
      pedido_foco(list(
        nivel = "municipio", uf_ibge = cod %/% 100000L, geocodigo = cod
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$limpar, {
      pedido_foco(list(
        nivel = "brasil", uf_ibge = NA_integer_, geocodigo = NA_integer_,
        .ts = as.numeric(Sys.time())
      ))
    }, ignoreInit = TRUE)

    pedido_foco
  })
}