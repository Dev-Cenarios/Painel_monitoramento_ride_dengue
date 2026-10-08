# ==============================================================================
# Dengue RIDE | Modulo: Mapa de tendencia (quadrantes) por municipio
#
# Eixo X = casos do municipio na janela atual / mediana historica do
#          municipio NESSA MESMA janela de semanas, em anos anteriores.
# Eixo Y = casos da janela atual / casos da janela imediatamente anterior
#          (mesmo ano corrente).
# Referencia 1 em ambos os eixos: "igual ao normal para a epoca" (X) e
# "estavel" (Y). Forma = incidencia acumulada no ano corrente ate a semana
# de referencia (limiares fixos, independentes da janela escolhida).
#
# ATENCAO A NOMENCLATURA: este grafico e chamado de "Rt" no titulo por pedido
# explicito, mas NAO e o numero de reproducao efetivo. A base ja tem uma
# coluna `rt` real (estimada pelo modelo do InfoDengue), usada em R/mod_rt.R
# e no KPI "Rt atual" (R/mod_kpi.R) - nenhum dos dois foi alterado por este
# modulo. O que se ve aqui e um indicador de ANOMALIA SAZONAL + TENDENCIA
# recente, pensado para comparar os 29 municipios entre si, nao para estimar
# transmissibilidade.
#
# Fonte: SERIE_MUNICIPIOS (global.R) - uma linha por municipio/semana, ja em
# memoria. Este modulo NAO consulta o DuckDB: so faz aritmetica de janela em
# cima de uma tabela de ~25 mil linhas, entao bindCache() na janela escolhida
# e suficiente.
#
# Por design, IGNORA o filtro geografico global (geo()): o valor do grafico e
# comparar municipios entre si, e um recorte para 1 municipio o esvaziaria.
#
# DESTAQUE (entrada `selected_location`): quando ha um municipio selecionado
# (clique no mapa), o ponto correspondente e RECALCULADO com cor de destaque,
# simbolo maior, borda e rotulo persistente com o nome completo; os demais
# pontos ficam com opacidade reduzida. Aqui e recalculo da serie (nao proxy):
# sao ~29 pontos, entao o setOption e instantaneo e o dado pesado (janelas
# historicas em quadro()) continua cacheado por `input$janela`, intacto.
# ==============================================================================

# Limiares de incidencia ACUMULADA no ano ate a semana de referencia (nao e a
# mesma escala semanal de PECAS_INCIDENCIA). Definem forma e nao cor: cor fica
# reservada para o quadrante (posicao x/y), que e a leitura principal do grafico.
LIMIARES_FORMA <- list(
  list(id = "triangle", teto = 10,  simbolo = "triangle", rotulo = "▲ < 10 /100k"),
  list(id = "diamond",  teto = 50,  simbolo = "diamond",  rotulo = "◆ 10–50 /100k"),
  list(id = "circle",   teto = 300, simbolo = "circle",   rotulo = "● 50–300 /100k"),
  list(id = "rect",     teto = Inf, simbolo = "rect",     rotulo = "■ > 300 /100k")
)

# Um municipio precisa de pelo menos este numero de anos historicos com dado
# na janela para que a mediana entre no denominador do eixo X. Com 1 ano so, a
# "mediana" seria um unico valor arbitrario, nao uma referencia sazonal.
MIN_ANOS_HISTORICO_RT <- 2L

# Cores por quadrante, em escala divergente (mesma regra do dengue_df):
# abaixo do normal em azul, acima em terracota; dentro de cada lado, o tom
# mais forte e o pior cenario. Validada para daltonismo em todos os pares.
CORES_QUADRANTE <- c(
  abaixo_estavel      = unname(TONS_AZUL[["700"]]),      # abaixo do normal, caindo
  abaixo_crescendo    = unname(TONS_AZUL[["400"]]),      # abaixo do normal, subindo
  acima_desacelerando = unname(TONS_DENGUE[["400"]]),    # acima do normal, caindo
  acima_crescendo     = unname(TONS_DENGUE[["700"]]),    # acima do normal, subindo - pior caso
  sem_base            = unname(PALETA_DENGUE[["sub"]])   # sem historico/janela anterior p/ comparar
)

ROTULOS_QUADRANTE <- c(
  abaixo_estavel      = "Abaixo do normal · caindo",
  abaixo_crescendo    = "Abaixo do normal · subindo",
  acima_desacelerando = "Acima do normal · caindo",
  acima_crescendo     = "Acima do normal · subindo",
  sem_base            = "Sem histórico suficiente"
)


mod_mapa_rt_controles <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::sliderInput(
      ns("janela"), "Janela de comparação (semanas)",
      min = 1, max = 8, value = 4, step = 1
    ),
    shiny::helpText(
      shiny::tags$small(
        "Eixo X: casos da janela atual vs. mediana histórica da mesma janela ",
        "(outros anos). Eixo Y: janela atual vs. janela anterior (mesmo ano). ",
        "Cor = quadrante; forma = incidência acumulada no ano até a semana ",
        "mais recente."
      )
    )
  )
}

mod_mapa_rt_saida <- function(id, height = "520px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height)
}


mod_mapa_rt_server <- function(id, selected_location = shiny::reactive(NULL)) {

  shiny::moduleServer(id, function(input, output, session) {

    # -- calculo do quadro (janela atual/anterior/historico) -----------------
    # Datas, nao numero de semana, definem as janelas atual/anterior: assim a
    # virada de ano (semana 52/53 -> 1) nao exige tratamento especial nelas.
    # So o casamento com o HISTORICO usa numero de semana (para achar a "mesma
    # epoca do ano" em anos passados) - ali a virada de ano e uma limitacao
    # conhecida e documentada abaixo, nao resolvida por raridade do caso.
    quadro <- shiny::reactive({
      shiny::req(input$janela)
      calcular_quadro_tendencia(SERIE_MUNICIPIOS, janela = input$janela)
    }) |>
      shiny::bindCache(input$janela)
    
    output$grafico <- echarts4r::renderEcharts4r({
      q <- quadro()
      d <- q$dados
      shiny::validate(shiny::need(nrow(d) > 0, "Sem dados suficientes para montar o painel."))
      
      # -- rotulo curto (primeiro nome) para nao poluir o grafico -----------
      d$rotulo_curto <- sub("\\s.*$", "", d$municipio_nome)

      # -- municipio em foco (estado global selected_location) --------------
      # NA = nenhum municipio selecionado (visao geral). Quando preenchido,
      # o ponto correspondente ganha cor/tamanho/rotulo de destaque e os
      # demais tem a opacidade reduzida (formatacao condicional abaixo).
      sl <- selected_location()
      sel_cod <- if (!is.null(sl) && identical(sl$nivel, "municipio") &&
                     !is.null(sl$geocodigo) && !is.na(sl$geocodigo)) {
        as.integer(sl$geocodigo)
      } else {
        NA_integer_
      }
      has_sel      <- !is.na(sel_cod)
      cor_destaque <- unname(PALETA_DENGUE[["tinta"]])
      
      # -- tamanho da bolha: raiz de casos, reescalado para 14-46px ----------
      raiz <- sqrt(pmax(d$casos_atual, 0))
      amplitude <- diff(range(raiz, na.rm = TRUE))
      d$bolha <- if (is.na(amplitude) || amplitude == 0) {
        rep(24, nrow(d))
      } else {
        14 + (raiz - min(raiz, na.rm = TRUE)) / amplitude * 32
      }
      
      # -- uma serie por FORMA (echarts exige symbol no nivel da serie) -----
      series_forma <- lapply(LIMIARES_FORMA, function(f) {
        sub <- d[d$forma == f$id & !is.na(d$x_val) & !is.na(d$taxa_cresc), , drop = FALSE]
        if (nrow(sub) == 0) return(NULL)
        
        pontos <- lapply(seq_len(nrow(sub)), function(i) {
          sel_i <- has_sel && !is.na(sub$municipio_geocodigo[i]) &&
                   sub$municipio_geocodigo[i] == sel_cod
          list(
            value      = list(sub$x_val[i], sub$taxa_cresc[i]),
            symbolSize = if (sel_i) sub$bolha[i] * 1.7 else sub$bolha[i],
            name       = sub$municipio_nome[i],
            itemStyle  = list(
              color       = if (sel_i) cor_destaque else CORES_QUADRANTE[[sub$quadrante[i]]],
              opacity     = if (has_sel && !sel_i) 0.3 else 1,
              borderColor = PALETA_DENGUE[["superficie"]],
              borderWidth = if (sel_i) 2 else 0
            ),
            label      = list(
              show         = TRUE,
              formatter    = if (sel_i) sub$municipio_nome[i] else sub$rotulo_curto[i],
              position     = "top",
              fontSize     = if (sel_i) 13 else 9,
              fontWeight   = if (sel_i) "bold" else "normal",
              color        = PALETA_DENGUE[["tinta"]],
              opacity      = if (has_sel && !sel_i) 0.35 else 1,
              backgroundColor = if (sel_i) "rgba(255,255,255,0.92)" else "transparent",
              padding      = if (sel_i) c(2, 5, 2, 5) else 0,
              borderRadius = if (sel_i) 3 else 0
            )
          )
        })
        
        list(
          name   = f$rotulo,
          type   = "scatter",
          symbol = f$simbolo,
          data   = pontos,
          emphasis = list(itemStyle = list(shadowBlur = 10, shadowColor = cor_alpha(PALETA_DENGUE[["tinta"]], 0.35)))
        )
      })
      series_forma <- Filter(Negate(is.null), series_forma)
      
      # -- serie "sem base": municipios sem denominador valido --------------
      sem_base <- d[d$quadrante == "sem_base", , drop = FALSE]
      y_faixa_sem_base <- {
        ymax <- suppressWarnings(max(d$taxa_cresc, na.rm = TRUE))
        if (!is.finite(ymax)) ymax <- 1
        round(ymax * 1.15 + 0.2, 2)
      }
      serie_sem_base <- if (nrow(sem_base) > 0) {
        pontos <- lapply(seq_len(nrow(sem_base)), function(i) {
          sel_i <- has_sel && !is.na(sem_base$municipio_geocodigo[i]) &&
                   sem_base$municipio_geocodigo[i] == sel_cod
          list(
            value      = list(dplyr::coalesce(sem_base$x_val[i], 0), y_faixa_sem_base),
            symbolSize = if (sel_i) sem_base$bolha[i] * 1.7 else sem_base$bolha[i],
            name       = sem_base$municipio_nome[i],
            itemStyle  = list(
              color       = if (sel_i) cor_destaque else CORES_QUADRANTE[["sem_base"]],
              opacity     = if (sel_i) 1 else if (has_sel) 0.25 else 0.5,
              borderColor = if (sel_i) PALETA_DENGUE[["superficie"]] else CORES_QUADRANTE[["sem_base"]],
              borderWidth = if (sel_i) 2 else 1.5
            ),
            label      = list(
              show         = TRUE,
              formatter    = if (sel_i) sem_base$municipio_nome[i] else sub("\\s.*$", "", sem_base$municipio_nome[i]),
              position     = "top",
              fontSize     = if (sel_i) 13 else 9,
              fontWeight   = if (sel_i) "bold" else "normal",
              color        = if (sel_i) PALETA_DENGUE[["tinta"]] else PALETA_DENGUE[["sub"]],
              opacity      = if (has_sel && !sel_i) 0.35 else 1,
              backgroundColor = if (sel_i) "rgba(255,255,255,0.92)" else "transparent",
              padding      = if (sel_i) c(2, 5, 2, 5) else 0,
              borderRadius = if (sel_i) 3 else 0
            )
          )
        })
        list(list(
          name   = "Sem histórico suficiente",
          type   = "scatter",
          symbol = "circle",
          data   = pontos
        ))
      } else list()
      
      # -- linhas de referencia (x=1, y=1) via serie invisivel + markLine ----
      linhas_ref <- list(list(
        name = "__ref__", type = "scatter", data = list(),
        silent = TRUE, tooltip = list(show = FALSE),
        markLine = list(
          silent = TRUE, symbol = list("none", "none"), animation = FALSE,
          data = list(
            list(xAxis = 1, lineStyle = list(color = PALETA_DENGUE[["sub"]], type = "dashed", width = 1.2),
                 label  = list(formatter = "Mediana histórica", position = "insideEndTop",
                               color = PALETA_DENGUE[["sub"]], fontSize = 10)),
            list(yAxis = 1, lineStyle = list(color = PALETA_DENGUE[["tinta"]], type = "dashed", width = 1.5),
                 label  = list(formatter = "Estável (Y=1)", position = "end",
                               color = PALETA_DENGUE[["tinta"]], fontSize = 10))
          )
        )
      ))
      
      todas_series <- c(series_forma, serie_sem_base, linhas_ref)
      
      # -- tooltip: le de uma tabela JS por nome, nao pelos valores plotados -
      # A serie "sem base" usa y "artificial" (y_faixa_sem_base) so para nao
      # sobrepor pontos; o tooltip precisa mostrar o dado real, nao essa
      # posicao de desenho.
      tabela_tt <- jsonlite::toJSON(
        stats::setNames(
          lapply(seq_len(nrow(d)), function(i) list(
            x      = round(d$x_val[i], 2),
            y      = round(d$taxa_cresc[i], 2),
            quad   = unname(ROTULOS_QUADRANTE[d$quadrante[i]]),
            casos  = d$casos_atual[i],
            inc    = round(d$incidencia_acumulada[i], 1)
          )),
          d$municipio_nome
        ),
        auto_unbox = TRUE, na = "null"
      )
      
      js_tooltip <- htmlwidgets::JS(sprintf(
        "function(p){
           if (p.seriesName === '__ref__') return '';
           var tab = %s;
           var r = tab[p.name];
           if (!r) return '';
           function f(v, d){ return (v===null||v===undefined) ? '—' :
             v.toLocaleString('pt-BR', {maximumFractionDigits: d}); }
           return '<b>' + p.name + '</b><br/>' +
                  r.quad + '<br/>' +
                  'Vs. mediana histórica: <b>' + f(r.x, 2) + 'x</b><br/>' +
                  'Vs. janela anterior: <b>' + f(r.y, 2) + 'x</b><br/>' +
                  'Casos na janela: ' + f(r.casos, 0) + '<br/>' +
                  'Incidência acumulada: ' + f(r.inc, 1) + ' /100k';
         }",
        tabela_tt
      ))
      
      subtitulo <- sprintf(
        "Janela atual: %s–%s | Janela anterior: %s–%s | Incidência acumulada em %d até SE %02d",
        q$atual_ini, q$atual_fim, q$ant_ini, q$ant_fim, q$ano_recente, q$se_ref_semana
      )
      
      echarts4r::e_charts() |>
        echarts4r::e_list(list(
          backgroundColor = "transparent",
          title = list(
            text = sprintf("Tendência por município — %d semana(s)", input$janela),
            subtext = subtitulo,
            textStyle    = list(color = PALETA_DENGUE[["tinta"]], fontSize = 14, fontWeight = "bold"),
            subtextStyle = list(color = PALETA_DENGUE[["sub"]], fontSize = 10)
          ),
          legend = list(
            list(
              data = lapply(LIMIARES_FORMA, function(f) f$rotulo),
              top = 46, left = "center",
              textStyle = list(color = PALETA_DENGUE[["sub"]], fontSize = 10),
              itemWidth = 14, itemHeight = 12, itemGap = 14
            ),
            list(
              data = lapply(names(CORES_QUADRANTE), function(k) list(
                name = ROTULOS_QUADRANTE[[k]],
                itemStyle = list(color = CORES_QUADRANTE[[k]])
              )),
              bottom = 6, left = "center",
              textStyle = list(color = PALETA_DENGUE[["tinta"]], fontSize = 10),
              itemWidth = 14, itemHeight = 12, itemGap = 12
            )
          ),
          tooltip = list(
            trigger = "item", formatter = js_tooltip,
            backgroundColor = "#FFFFFF",
            borderColor = PALETA_DENGUE[["linha"]], borderWidth = 1,
            textStyle = list(color = PALETA_DENGUE[["tinta"]])
          ),
          xAxis = list(
            type = "value", name = "Vs. mediana histórica (mediana = 1x)",
            nameLocation = "middle", nameGap = 32, min = 0,
            max = htmlwidgets::JS("function(v){ return Math.max(v.max*1.15, 1.3).toFixed(1); }"),
            nameTextStyle = list(color = PALETA_DENGUE[["sub"]], fontSize = 11),
            axisLabel = list(color = PALETA_DENGUE[["sub"]], fontSize = 10,
                             formatter = htmlwidgets::JS("function(v){ return v.toFixed(1)+'x'; }")),
            splitLine = list(lineStyle = list(color = PALETA_DENGUE[["linha"]], type = "dashed"))
          ),
          yAxis = list(
            type = "value", name = "Vs. janela anterior (estável = 1x)",
            nameLocation = "middle", nameGap = 46, min = 0,
            max = htmlwidgets::JS(sprintf(
              "function(v){ return Math.max(v.max*1.1, %s, 1.3).toFixed(1); }",
              round(y_faixa_sem_base + 0.15, 2)
            )),
            nameTextStyle = list(color = PALETA_DENGUE[["sub"]], fontSize = 11),
            axisLabel = list(color = PALETA_DENGUE[["sub"]], fontSize = 10,
                             formatter = htmlwidgets::JS(
                               "function(v){ return v.toLocaleString('pt-BR',{minimumFractionDigits:1}); }"
                             )),
            splitLine = list(lineStyle = list(color = PALETA_DENGUE[["linha"]], type = "dashed"))
          ),
          series = todas_series,
          dataZoom = list(
            list(type = "inside", xAxisIndex = 0, filterMode = "none", zoomOnMouseWheel = TRUE),
            list(type = "inside", yAxisIndex = 0, filterMode = "none", zoomOnMouseWheel = TRUE)
          ),
          grid = list(left = "9%", right = "4%", top = "26%", bottom = "18%"),
          textStyle = list(fontFamily = FONTE_GRAFICO)
        ))
    })
  })
}


#' Monta o quadro de tendencia: uma linha por municipio, pronta para o grafico.
#'
#' @param serie_mun SERIE_MUNICIPIOS (ou equivalente): municipio_geocodigo,
#'   municipio_nome, data_ini_se, se, ano, semana, casos, pop.
#' @param janela    tamanho da janela de comparacao, em semanas.
#'
#' LIMITACAO CONHECIDA: o casamento com o historico usa numero de semana
#' (`semana`), agrupado por `ano`. Se a janela atual cruzar a virada do ano
#' (ex.: semanas 52-53-1-2), os anos historicos casados por esses numeros de
#' semana ficam levemente deslocados perto da fronteira do ano - caso raro
#' (so ocorre quando a semana de referencia esta nas primeiras semanas do ano
#' E a janela e larga o suficiente para alcancar o ano anterior) e nao tratado
#' aqui para nao complicar o calculo por um cenario de borda.
calcular_quadro_tendencia <- function(serie_mun, janela = 4L) {
  
  data_max    <- max(serie_mun$data_ini_se, na.rm = TRUE)
  ano_recente <- as.integer(format(data_max, "%Y"))
  se_ref_semana <- serie_mun$semana[serie_mun$data_ini_se == data_max][1]
  
  atual_fim   <- data_max
  atual_ini   <- atual_fim - (janela - 1L) * 7
  ant_fim     <- atual_ini - 7
  ant_ini     <- ant_fim - (janela - 1L) * 7
  
  # -- janela atual e anterior, por municipio -------------------------------
  janela_atual <- serie_mun |>
    dplyr::filter(data_ini_se >= atual_ini, data_ini_se <= atual_fim) |>
    dplyr::group_by(municipio_geocodigo, municipio_nome) |>
    dplyr::summarise(casos_atual = sum(casos, na.rm = TRUE),
                     pop = max(pop, na.rm = TRUE), .groups = "drop")
  
  janela_anterior <- serie_mun |>
    dplyr::filter(data_ini_se >= ant_ini, data_ini_se <= ant_fim) |>
    dplyr::group_by(municipio_geocodigo) |>
    dplyr::summarise(casos_anterior = sum(casos, na.rm = TRUE), .groups = "drop")
  
  # -- mediana historica: mesmas semanas-do-ano, anos != ano_recente --------
  semanas_da_janela <- unique(
    serie_mun$semana[serie_mun$data_ini_se >= atual_ini & serie_mun$data_ini_se <= atual_fim]
  )
  
  historico <- serie_mun |>
    dplyr::filter(semana %in% semanas_da_janela, ano != ano_recente) |>
    dplyr::group_by(municipio_geocodigo, ano) |>
    dplyr::summarise(casos_janela_ano = sum(casos, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(municipio_geocodigo) |>
    dplyr::summarise(
      mediana_historica  = stats::median(casos_janela_ano, na.rm = TRUE),
      n_anos_historico   = dplyr::n(),
      .groups = "drop"
    )
  
  # -- incidencia acumulada no ano corrente ate a semana de referencia ------
  acumulado <- serie_mun |>
    dplyr::filter(ano == ano_recente, semana <= se_ref_semana) |>
    dplyr::group_by(municipio_geocodigo) |>
    dplyr::summarise(
      casos_acumulados = sum(casos, na.rm = TRUE),
      pop_ref = max(pop, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      incidencia_acumulada = dplyr::if_else(pop_ref > 0, casos_acumulados / pop_ref * 1e5, NA_real_)
    )
  
  # -- monta o quadro final ---------------------------------------------------
  base_mun <- dplyr::distinct(serie_mun, municipio_geocodigo, municipio_nome)
  
  dados <- base_mun |>
    dplyr::left_join(dplyr::select(janela_atual, -municipio_nome), by = "municipio_geocodigo") |>
    dplyr::left_join(janela_anterior, by = "municipio_geocodigo") |>
    dplyr::left_join(historico, by = "municipio_geocodigo") |>
    dplyr::left_join(acumulado, by = "municipio_geocodigo") |>
    dplyr::mutate(
      casos_atual    = dplyr::coalesce(casos_atual, 0),
      casos_anterior = dplyr::coalesce(casos_anterior, 0),
      sem_base = casos_anterior == 0 |
        is.na(mediana_historica) | mediana_historica == 0 |
        dplyr::coalesce(n_anos_historico, 0L) < MIN_ANOS_HISTORICO_RT,
      taxa_cresc = dplyr::if_else(casos_anterior > 0, casos_atual / casos_anterior, NA_real_),
      x_val      = dplyr::if_else(
        !is.na(mediana_historica) & mediana_historica > 0,
        casos_atual / mediana_historica, NA_real_
      ),
      # Os limiares vem de LIMIARES_FORMA (unica fonte de verdade) - nao
      # duplicar os numeros aqui, senao editar a lista deixa de ter efeito.
      forma = dplyr::case_when(
        is.na(incidencia_acumulada)                    ~ LIMIARES_FORMA[[1]]$id,
        incidencia_acumulada < LIMIARES_FORMA[[1]]$teto ~ LIMIARES_FORMA[[1]]$id,
        incidencia_acumulada < LIMIARES_FORMA[[2]]$teto ~ LIMIARES_FORMA[[2]]$id,
        incidencia_acumulada < LIMIARES_FORMA[[3]]$teto ~ LIMIARES_FORMA[[3]]$id,
        TRUE                                             ~ LIMIARES_FORMA[[4]]$id
      ),
      quadrante = dplyr::case_when(
        sem_base                              ~ "sem_base",
        x_val < 1  & taxa_cresc < 1            ~ "abaixo_estavel",
        x_val < 1  & taxa_cresc >= 1           ~ "abaixo_crescendo",
        x_val >= 1 & taxa_cresc < 1            ~ "acima_desacelerando",
        TRUE                                   ~ "acima_crescendo"
      )
    )
  
  list(
    dados         = dados,
    ano_recente   = ano_recente,
    se_ref_semana = se_ref_semana,
    atual_ini     = format(atual_ini, "%d/%m"),
    atual_fim     = format(atual_fim, "%d/%m"),
    ant_ini       = format(ant_ini, "%d/%m"),
    ant_fim       = format(ant_fim, "%d/%m")
  )
}