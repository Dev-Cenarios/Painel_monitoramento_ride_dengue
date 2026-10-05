# ==============================================================================
# Dengue RIDE | utils_ui.R
# Tema e helpers de composicao de cards. Toda decisao de apresentacao mora
# aqui e no ui.R; os modulos so expoem controles e saidas.
# ==============================================================================

# ------------------------------------------------------------------- tema ---
# preset = "zephyr" exige bslib >= 0.6.0. Em versoes anteriores, troque por
# bootswatch = "zephyr" - o resultado visual e praticamente o mesmo.
#
# Paleta: primary e a moldura (cabecalhos de card, links), navy sobrio.
# O vermelho fica reservado para `danger`, onde carrega significado
# epidemiologico: Rt > 1, incidencia alta, alertas. Se primary fosse vermelho,
# o painel inteiro ficaria vermelho e o alerta deixaria de se destacar.
# preset = "zephyr" exige bslib >= 0.6.0. Em versoes anteriores, troque por
# bootswatch = "zephyr" - o resultado visual e praticamente o mesmo.
#
# Paleta chocolate (PALETA_CHOCOLATE, em global.R): fundo creme, cards brancos,
# cabecalhos em chocolate medio. `danger` e o extremo escuro da rampa, nao uma
# cor de fora dela - assim o alerta continua se destacando por CONTRASTE de
# luminosidade, sem quebrar a identidade visual.
TEMA_DENGUE3D <- bslib::bs_theme(
  version = 5,
  preset  = "zephyr",
  
  bg = "#FAF6F1",
  fg = "#3B2A1D",
  
  primary   = PALETA_CHOCOLATE[["chocolate"]],
  secondary = PALETA_CHOCOLATE[["taupe"]],
  success   = PALETA_CHOCOLATE[["cacau"]],
  warning   = PALETA_CHOCOLATE[["ambar"]],
  danger    = PALETA_CHOCOLATE[["amargo"]],
  info      = PALETA_CHOCOLATE[["caramelo"]],
  
  base_font = bslib::font_collection(
    bslib::font_google("Inter", local = FALSE),
    "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
  ),
  
  "border-radius"      = "0.65rem",
  "border-radius-sm"   = "0.45rem",
  "card-border-radius" = "0.65rem",
  "card-cap-padding-y" = "0.6rem",
  "card-border-color"  = "rgba(74,42,16,.12)",
  "card-bg"            = "#FFFFFF",
  "enable-shadows"     = TRUE
)


# --------------------------------------------------------------- helpers ----

#' Gatilho de popover para os controles de um card.
#'
#' Os controles saem do corpo do card e vao para o cabecalho. E o que permite
#' card_body(class = "p-0") entregar 100% da area ao grafico.
gatilho_opcoes <- function(rotulo = "Opções") {
  shiny::tags$span(
    class = "badge rounded-pill bg-light text-primary fw-normal",
    style = "cursor: pointer;",
    rotulo
  )
}

#' Card padrao de grafico.
#'
#' @param titulo    texto do cabecalho.
#' @param saida     output do grafico (mod_*_saida()).
#' @param controles UI de inputs; vai para um popover no cabecalho. NULL omite.
#' @param tom       classe de fundo do cabecalho.
cartao_grafico <- function(titulo, saida, controles = NULL, tom = "bg-primary") {
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = paste(tom, "text-white d-flex justify-content-between align-items-center"),
      shiny::tags$span(class = "fw-semibold", titulo),
      if (!is.null(controles)) {
        bslib::popover(
          gatilho_opcoes(),
          controles,
          title     = titulo,
          placement = "left"
        )
      }
    ),
    # p-0: sem respiro interno, o grafico encosta nas bordas do card.
    bslib::card_body(class = "p-0", saida)
  )
}


#' Ajustes de UI globais, injetados uma vez no topo do app (ver ui.R).
#'
#' 1. DROPDOWN DOS SELETORES por cima de tudo. Os selectize de Estado/Municipio
#'    usam `dropdownParent = "body"` (R/mod_geo_filter.R) para o menu escapar
#'    do card - que tem `overflow: hidden`. Este z-index garante que o menu,
#'    ja no <body>, fique acima de cards, KPIs e popovers.
#'
#' 2. POPOVER DE "OPCOES" FECHA AO CLICAR FORA (ou Esc). O Bootstrap, sozinho,
#'    so fecha um popover de gatilho "click" no proprio gatilho ou no X. O
#'    handler abaixo fecha qualquer `.popover.show` quando o clique cai fora
#'    dele e fora do gatilho - ignorando cliques no dropdown de um selectize
#'    (que o `dropdownParent = "body"` joga para fora do popover).
dengue3d_ui_assets <- function() {
  shiny::tagList(
    shiny::singleton(shiny::tags$style(shiny::HTML(
      ".selectize-dropdown { z-index: 1080 !important; }"
    ))),
    shiny::singleton(shiny::tags$script(shiny::HTML(
"(function() {
  function insidePicker(t) {
    return t && t.closest && t.closest('.selectize-dropdown, .selectize-control');
  }
  function triggerOf(tip) {
    return tip.id ? document.querySelector('[aria-describedby=\"' + tip.id + '\"]') : null;
  }
  function hide(tip) {
    var trg  = triggerOf(tip);
    var inst = (trg && window.bootstrap && bootstrap.Popover)
      ? bootstrap.Popover.getInstance(trg) : null;
    var host = (trg && trg.closest) ? trg.closest('bslib-popover') : null;
    if (inst) inst.hide();
    else if (host && typeof host.hide === 'function') host.hide();
    else if (trg) trg.click();
  }
  document.addEventListener('click', function(e) {
    var open = document.querySelectorAll('.popover.show');
    if (!open.length || insidePicker(e.target)) return;
    open.forEach(function(tip) {
      var trg = triggerOf(tip);
      if (tip.contains(e.target)) return;
      if (trg && (trg === e.target || trg.contains(e.target))) return;
      hide(tip);
    });
  });
  document.addEventListener('keydown', function(e) {
    if (e.key !== 'Escape') return;
    document.querySelectorAll('.popover.show').forEach(hide);
  });
})();"
    )))
  )
}