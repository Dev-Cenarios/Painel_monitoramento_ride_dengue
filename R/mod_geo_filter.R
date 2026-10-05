# ==============================================================================
# Dengue RIDE | Modulo: filtro geografico hierarquico
# RIDE-DF -> Estado -> Municipio. Unica fonte de verdade do recorte espacial.
#
# ESCOPO DINAMICO: uf_choices vem de UF_DISPONIVEIS e a cascata de municipios
# de GEO_DICT - ambos derivados de um DISTINCT sobre o Parquet. Nao existe
# lista de UFs ou de municipios escrita neste arquivo. Trocar a base troca os
# seletores, sem tocar em codigo.
#
# Contrato de saida (reactive): lista com
#   $nivel     "brasil" | "uf" | "municipio"
#   $uf_ibge   integer ou NA
#   $geocodigo integer ou NA
#   $rotulo    texto para titulos/legendas
#
# A chave "brasil" significa "toda a base" - o nome permanece por compatibilidade
# com aplicar_filtro_geo(); o rotulo exibido vem de ROTULO_BASE.
#
# Entrada opcional `foco_externo`: reactive que devolve NULL ou uma lista
# list(nivel, uf_ibge, geocodigo). Permite que outros componentes (o mapa,
# por exemplo) reposicionem o filtro sem duplicar estado. E o canal por onde
# server.R propaga o estado global `selected_location`: um clique no mapa
# chega como nivel = "municipio"; o botao "Visao geral" chega como
# nivel = "brasil" (com um campo `.nonce` ignorado aqui, so para forcar a
# notificacao do reactiveVal em cliques repetidos).
#
# O caminho inverso (seletor -> resto do painel) e simetrico: escolher um
# municipio no seletor leva a Abrangencia para "Municipio" (ver
# observeEvent(input$municipio)) e server.R devolve o local a
# `selected_location`, para que o mapa e a tendencia o destaquem como num clique.
# ==============================================================================

mod_geo_filter_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_column_wrap(
    width = 1/3,
    gap   = "1rem",
    
    shiny::radioButtons(
      inputId  = ns("nivel"),
      label    = "Abrangência",
      # O nivel agregado nao e "Brasil": a base cobre a RIDE-DF. A chave
      # interna continua "brasil" para nao mexer em aplicar_filtro_geo().
      choices  = stats::setNames(
        c("brasil", "uf", "municipio"),
        c(ROTULO_BASE, "Estado", "Município")
      ),
      selected = "brasil",
      inline   = TRUE
    ),
    
    # choices = NULL: preenchido no server a partir da base, nunca na UI.
    # selectizeInput (nao selectInput) para aceitar `options`: dropdownParent
    # = "body" tira o menu de dentro deste card - que tem overflow: hidden e
    # cortava a lista. O z-index do menu vem de dengue3d_ui_assets().
    shiny::selectizeInput(
      inputId = ns("uf"),
      label   = "Estado",
      choices = NULL,
      width   = "100%",
      options = list(dropdownParent = "body")
    ),

    shiny::selectizeInput(
      inputId = ns("municipio"),
      label   = "Município",
      choices = NULL,
      width   = "100%",
      options = list(
        placeholder    = "Digite para buscar...",
        dropdownParent = "body"
      )
    )
  )
}

#' Resumo compacto do recorte, para o cabecalho do card de filtros.
mod_geo_filter_resumo <- function(id) {
  shiny::uiOutput(shiny::NS(id)("resumo_selecao"), inline = TRUE)
}


mod_geo_filter_server <- function(id,
                                  geo_dict     = GEO_DICT,
                                  ufs          = UF_DISPONIVEIS,
                                  uf_padrao    = UF_PADRAO,
                                  foco_externo = shiny::reactive(NULL)) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    # Municipio que o proximo repovoamento da lista deve deixar selecionado.
    # Necessario porque trocar a UF dispara a cascata, que por padrao escolhe
    # o primeiro municipio - sobrescrevendo o alvo vindo de um clique no mapa.
    mun_pendente <- shiny::reactiveVal(NULL)

    # Valor que o SERVIDOR acabou de empurrar para o seletor de municipio
    # (cascata, clique no mapa, reset). O browser devolve esse valor como
    # input$municipio; sem esta marca nao ha como distinguir esse eco de uma
    # escolha feita pelo usuario no seletor (ver observeEvent(input$municipio)).
    mun_esperado <- shiny::reactiveVal(NULL)

    # -- seletor de UF, populado da base ------------------------------------
    uf_choices <- stats::setNames(
      as.character(ufs$uf_ibge),
      sprintf("%s (%s)", ufs$uf_nome, ufs$uf_sigla)
    )
    
    selecao_inicial <- if (uf_padrao %in% ufs$uf_ibge) {
      as.character(uf_padrao)
    } else {
      uf_choices[[1]]
    }
    
    shiny::updateSelectizeInput(
      session, "uf",
      choices  = uf_choices,
      selected = selecao_inicial
    )
    
    # -- repovoa a lista de municipios de uma UF ----------------------------
    # Resolvido em memoria (GEO_DICT), sem consulta ao Parquet.
    repovoar_municipios <- function(uf_i) {
      mun <- geo_dict[geo_dict$uf_ibge == uf_i, , drop = FALSE]
      mun <- mun[order(mun$municipio_nome), , drop = FALSE]
      if (nrow(mun) == 0) return(invisible(NULL))
      
      alvo <- mun_pendente()
      sel <- if (identical(alvo, "__limpar__")) {
        # Reset "Visao geral": deixa o seletor de municipio vazio (mostra o
        # placeholder), em vez de escolher o primeiro da lista.
        ""
      } else if (!is.null(alvo) && alvo %in% mun$municipio_geocodigo) {
        as.character(alvo)
      } else {
        as.character(mun$municipio_geocodigo[1])
      }
      if (!is.null(alvo)) mun_pendente(NULL)
      mun_esperado(sel)

      shiny::updateSelectizeInput(
        session, "municipio",
        choices  = stats::setNames(
          as.character(mun$municipio_geocodigo),
          mun$municipio_nome
        ),
        selected = sel,
        server   = TRUE
      )
    }
    
    # -- cascata: trocar a UF repovoa a lista de municipios -----------------
    shiny::observeEvent(input$uf, {
      uf_i <- suppressWarnings(as.integer(input$uf))
      shiny::req(!is.na(uf_i))
      repovoar_municipios(uf_i)
    }, ignoreInit = FALSE)
    
    # -- foco externo (ex.: clique no mapa) ---------------------------------
    shiny::observeEvent(foco_externo(), {
      f <- foco_externo()
      shiny::req(!is.null(f), !is.null(f$nivel))
      
      if (identical(f$nivel, "brasil")) {
        shiny::updateRadioButtons(session, "nivel", selected = "brasil")

        # Reset "Visao geral": alem do radio, devolve os seletores de Estado
        # e Municipio ao estado inicial. geo() ja ignora input$uf / input$
        # municipio quando nivel == "brasil", entao isto e puramente
        # cosmetico - impede que os campos continuem exibindo o rotulo do
        # ultimo clique. Estado volta ao default (selectInput base nao aceita
        # selecao vazia); Municipio e limpado ("" -> placeholder) via o
        # sentinela "__limpar__" que repovoar_municipios() reconhece.
        mun_pendente("__limpar__")
        if (!identical(input$uf, selecao_inicial)) {
          # A cascata em observeEvent(input$uf) consome mun_pendente() e
          # limpa o seletor de municipio.
          shiny::updateSelectizeInput(session, "uf", selected = selecao_inicial)
        } else {
          # UF ja estava no default: aplica a limpeza do municipio direto.
          repovoar_municipios(as.integer(selecao_inicial))
        }
        return(invisible(NULL))
      }
      
      # Ja e o recorte em vigor (ex.: o usuario escolheu o municipio no proprio
      # seletor e server.R devolveu o mesmo local so para destacar o mapa e a
      # tendencia): nada a reposicionar, evita um repovoamento redundante.
      if (identical(f$nivel, "municipio") && !is.na(f$geocodigo)) {
        atual <- tryCatch(shiny::isolate(geo()), error = function(e) NULL)
        if (!is.null(atual) && identical(atual$nivel, "municipio") &&
            identical(as.integer(atual$geocodigo), as.integer(f$geocodigo))) {
          return(invisible(NULL))
        }
      }

      shiny::req(!is.na(f$uf_ibge))
      uf_alvo <- as.character(as.integer(f$uf_ibge))

      if (identical(f$nivel, "municipio") && !is.na(f$geocodigo)) {
        mun_pendente(as.integer(f$geocodigo))
      }
      
      if (!identical(input$uf, uf_alvo)) {
        # A cascata em observeEvent(input$uf) consome mun_pendente().
        shiny::updateSelectizeInput(session, "uf", selected = uf_alvo)
      } else if (!is.null(mun_pendente())) {
        # UF ja era a selecionada: a cascata nao dispara, aplica direto.
        repovoar_municipios(as.integer(uf_alvo))
      }
      
      shiny::updateRadioButtons(session, "nivel", selected = f$nivel)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # -- escolha do usuario no seletor de municipio -------------------------
    # Mesmo efeito do clique no mapa: leva a Abrangencia para "Municipio", de
    # modo que geo() passe a apontar para o municipio escolhido (server.R
    # propaga o resto: graficos via geo(), destaque do mapa/tendencia via
    # selected_location). Sem isto, escolher um municipio com a Abrangencia
    # em RIDE-DF ou Estado nao mudava nada.
    # O eco de um valor empurrado pelo servidor (mun_esperado) e ignorado - do
    # contrario a cascata de "trocar a UF" (que seleciona o 1o municipio da
    # lista) forcaria a Abrangencia para Municipio sozinha.
    shiny::observeEvent(input$municipio, {
      v <- input$municipio
      shiny::req(!is.null(v), nzchar(v))

      if (identical(v, mun_esperado())) {
        mun_esperado(NULL)
        return(invisible(NULL))
      }

      mun_esperado(NULL)
      if (!identical(input$nivel, "municipio")) {
        shiny::updateRadioButtons(session, "nivel", selected = "municipio")
      }
    }, ignoreInit = TRUE)

    # -- contrato de saida --------------------------------------------------
    geo <- shiny::reactive({
      nivel <- input$nivel
      shiny::req(nivel)
      
      if (identical(nivel, "brasil")) {
        return(list(
          nivel     = "brasil",
          uf_ibge   = NA_integer_,
          geocodigo = NA_integer_,
          rotulo    = ROTULO_BASE
        ))
      }
      
      shiny::req(input$uf)
      uf_i <- suppressWarnings(as.integer(input$uf))
      shiny::req(!is.na(uf_i))
      info_uf <- ufs[ufs$uf_ibge == uf_i, , drop = FALSE]
      shiny::req(nrow(info_uf) == 1)
      
      if (identical(nivel, "uf")) {
        return(list(
          nivel     = "uf",
          uf_ibge   = uf_i,
          geocodigo = NA_integer_,
          rotulo    = sprintf("%s (%s)", info_uf$uf_nome, info_uf$uf_sigla)
        ))
      }
      
      shiny::req(input$municipio)
      cod <- suppressWarnings(as.integer(input$municipio))
      shiny::req(!is.na(cod))
      
      # Guarda contra estado transitorio: entre trocar a UF e o updateSelectize
      # chegar ao browser, input$municipio ainda aponta para a UF anterior.
      # Os 2 primeiros digitos do geocodigo sao o codigo da UF.
      shiny::req(cod %/% 100000L == uf_i)
      
      nome_mun <- geo_dict$municipio_nome[geo_dict$municipio_geocodigo == cod][1]
      
      list(
        nivel     = "municipio",
        uf_ibge   = uf_i,
        geocodigo = cod,
        rotulo    = sprintf("%s / %s", nome_mun, info_uf$uf_sigla)
      )
    })
    
    # -- feedback visual da selecao corrente --------------------------------
    output$resumo_selecao <- shiny::renderUI({
      g <- geo()
      n_mun <- switch(
        g$nivel,
        brasil    = nrow(geo_dict),
        uf        = sum(geo_dict$uf_ibge == g$uf_ibge),
        municipio = 1L
      )
      shiny::tags$span(
        class = "small",
        shiny::tags$strong(g$rotulo),
        shiny::tags$span(
          class = "opacity-75",
          sprintf(" · %s município(s)", fmt_int(n_mun))
        )
      )
    })
    
    geo
  })
}