# ==============================================================================
# Dengue RIDE | utils_ui.R
# Tema e helpers de composicao de cards. Toda decisao de apresentacao mora
# aqui e no ui.R; os modulos so expoem controles e saidas.
# ==============================================================================

# ------------------------------------------------------------------- tema ---
# preset = "zephyr" exige bslib >= 0.6.0. Em versoes anteriores, troque por
# bootswatch = "zephyr" - o resultado visual e praticamente o mesmo.
#
# Paleta (PALETA_DENGUE, em global.R): interface em azul Cenarios (moldura,
# cabecalhos, links), fundo cinza-azulado, cards brancos. A terracota fica
# para os dados de dengue e o vermelho `danger` para alertas (obito, Rt alto)
# - se a moldura tivesse a cor do dado, o alerta deixaria de se destacar.
# Tipografia do guia Cenarios+: Poppins nos titulos, DM Sans no corpo.
TEMA_DENGUE3D <- bslib::bs_theme(
  version = 5,
  preset  = "zephyr",

  bg = PALETA_DENGUE[["fundo"]],
  fg = PALETA_DENGUE[["texto"]],

  primary   = PALETA_DENGUE[["azul_acao"]],
  secondary = PALETA_DENGUE[["sub"]],
  success   = PALETA_DENGUE[["ok"]],
  warning   = PALETA_DENGUE[["atencao"]],
  danger    = PALETA_DENGUE[["critico"]],
  info      = PALETA_DENGUE[["azul"]],

  base_font = bslib::font_collection(
    bslib::font_google("DM Sans", wght = c(400, 500, 700), local = FALSE),
    "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
  ),
  heading_font = bslib::font_collection(
    bslib::font_google("Poppins", wght = c(600, 700), local = FALSE),
    "system-ui", "sans-serif"
  ),

  "headings-color"     = PALETA_DENGUE[["azul_titulo"]],
  "body-color"         = PALETA_DENGUE[["texto"]],
  "link-color"         = PALETA_DENGUE[["azul_acao"]],
  "border-radius"      = "0.65rem",
  "border-radius-sm"   = "0.45rem",
  "card-border-radius" = "0.65rem",
  "card-cap-padding-y" = "0.6rem",
  "card-border-color"  = PALETA_DENGUE[["linha"]],
  "card-bg"            = PALETA_DENGUE[["superficie"]],
  "enable-shadows"     = TRUE
) |>
  bslib::bs_add_rules(sprintf("
    .card-header.bg-primary { background-color: %s !important; }
    .card-header, .card-header .fw-semibold, .navbar-brand {
      font-family: Poppins, system-ui, sans-serif;
    }
    .value-box-value, .bslib-value-box .value-box-value, table td {
      font-variant-numeric: tabular-nums;
    }
    .value-box-value { font-weight: 700; }
  ", PALETA_DENGUE[["azul_titulo"]]))


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