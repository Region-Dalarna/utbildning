# ============================================================
#  func_diagram.R
#  Diagramhjälpare. Interaktiva via ggiraph/girafe (hover + klick).
#  Färger och interaktiv css definieras centralt i def_farger.R.
# ============================================================

# Tunna ut årsetiketter när de blir för många (varannan).
.ar_breaks <- function(ar) {
  u <- sort(unique(ar))
  if (length(u) <= 8) u else u[seq(1, length(u), by = 2)]
}

# Liten platshållarplot.
.tom_plot <- function(msg = "") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, color = RD_TEXT_MUTED, size = 3) +
    ggplot2::theme_void()
}

# Gemensamt, avskalat tema.
.rd_tema <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.title.y       = ggplot2::element_blank(),
      axis.title.x       = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      plot.margin        = ggplot2::margin(5, 12, 5, 5)
    )
}

# Standardiserad girafe. selection = TRUE ger klickbar korsfiltrering.
# Lasso (markera/avmarkera) är alltid dolt.
.girafe_std <- function(g, width_svg = 9, height_svg = 6, selection = FALSE) {
  opts <- list(
    ggiraph::opts_hover(css = RD_HOVER_CSS),
    ggiraph::opts_tooltip(css = RD_TOOLTIP_CSS),
    ggiraph::opts_toolbar(saveaspng = FALSE,
                          hidden = c("lasso_select", "lasso_deselect"))
  )
  if (selection) {
    opts <- c(opts, list(ggiraph::opts_selection(
      type = "single", only_shiny = TRUE, css = RD_SELECT_CSS)))
  }
  ggiraph::girafe(ggobj = g, width_svg = width_svg, height_svg = height_svg,
                  options = opts)
}

# Etikett för tooltip ("Antal antagna 2025").
.metrik_ar <- function(metrik_label, ar) {
  if (is.null(ar)) metrik_label else paste0(metrik_label, " ", ar)
}

# ---- Stapel: total metrik efter program (klickbar) ------------------------
skapa_diagram_bar <- function(df, metrik, metrik_label, ar = NULL) {
  d <- df |>
    dplyr::group_by(program) |>
    dplyr::summarise(antal = sum(.data[[metrik]], na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(antal > 0) |>
    dplyr::mutate(
      program = forcats::fct_reorder(program, antal),
      tooltip = paste0("<b>", program, "</b><br/>", .metrik_ar(metrik_label, ar), ": ", antal),
      data_id = as.character(program)
    )

  g <- ggplot2::ggplot(d, ggplot2::aes(x = antal, y = program)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = data_id),
      fill = RD_PRIMARY, width = 0.74) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = metrik_label, y = NULL) +
    .rd_tema()

  .girafe_std(g, width_svg = 6.8, height_svg = 6.6, selection = TRUE)
}

# ---- Stapel: könsuppdelad (staplad), klickbar väljer programmet -----------
skapa_diagram_bar_kon <- function(df, metrik_kv, metrik_man, metrik_label, ar = NULL) {
  d <- df |>
    dplyr::group_by(program) |>
    dplyr::summarise(
      Kvinnor = sum(.data[[metrik_kv]],  na.rm = TRUE),
      Män     = sum(.data[[metrik_man]], na.rm = TRUE),
      .groups = "drop") |>
    dplyr::mutate(tot = Kvinnor + Män) |>
    dplyr::filter(tot > 0) |>
    tidyr::pivot_longer(c(Kvinnor, Män), names_to = "kon", values_to = "antal") |>
    dplyr::mutate(
      program = forcats::fct_reorder(program, tot),
      tooltip = paste0("<b>", program, "</b><br/>", kon, " · ",
                       .metrik_ar(metrik_label, ar), ": ", antal),
      data_id = as.character(program))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = antal, y = program, fill = kon)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = data_id), width = 0.72) +
    ggplot2::scale_fill_manual(values = KON_FARGER, name = NULL) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = metrik_label, y = NULL) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top")

  .girafe_std(g, width_svg = 6.8, height_svg = 6.6, selection = TRUE)
}

# ---- Linje: utveckling över tid -------------------------------------------
skapa_diagram_trend <- function(df, metrik, metrik_label, program_sel = NULL) {
  rubrik <- if (is.null(program_sel)) "Alla program" else program_sel
  if (!is.null(program_sel)) df <- dplyr::filter(df, program == program_sel)

  d <- df |>
    dplyr::group_by(ar) |>
    dplyr::summarise(antal = sum(.data[[metrik]], na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(tooltip = paste0(metrik_label, " ", ar, ": ", antal))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = antal)) +
    ggplot2::geom_line(color = RD_PRIMARY, linewidth = 0.9) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = ar), color = RD_PRIMARY, size = 2.4) +
    ggplot2::scale_x_continuous(breaks = .ar_breaks(d$ar)) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = rubrik) +
    .rd_tema() +
    ggplot2::theme(
      plot.subtitle      = ggplot2::element_text(size = 10, color = RD_TEXT_MUTED),
      panel.grid.major.y = ggplot2::element_line(color = "#eef2f5")
    )

  .girafe_std(g, width_svg = 5, height_svg = 2.7, selection = FALSE)
}

# ---- Linje: utveckling över tid, könsuppdelad (två linjer) ----------------
skapa_diagram_trend_kon <- function(df, metrik_kv, metrik_man, metrik_label, program_sel = NULL) {
  rubrik <- if (is.null(program_sel)) "Alla program" else program_sel
  if (!is.null(program_sel)) df <- dplyr::filter(df, program == program_sel)

  d <- df |>
    dplyr::group_by(ar) |>
    dplyr::summarise(
      Kvinnor = sum(.data[[metrik_kv]],  na.rm = TRUE),
      Män     = sum(.data[[metrik_man]], na.rm = TRUE),
      .groups = "drop") |>
    tidyr::pivot_longer(c(Kvinnor, Män), names_to = "kon", values_to = "antal") |>
    dplyr::mutate(tooltip = paste0(kon, " · ", metrik_label, " ", ar, ": ", antal))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = antal, color = kon, group = kon)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = paste(ar, kon)), size = 2.2) +
    ggplot2::scale_color_manual(values = KON_FARGER, name = NULL) +
    ggplot2::scale_x_continuous(breaks = .ar_breaks(d$ar)) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = rubrik) +
    .rd_tema() +
    ggplot2::theme(
      legend.position    = "top",
      plot.subtitle      = ggplot2::element_text(size = 10, color = RD_TEXT_MUTED),
      panel.grid.major.y = ggplot2::element_line(color = "#eef2f5")
    )

  .girafe_std(g, width_svg = 5, height_svg = 2.9, selection = FALSE)
}

# ---- 100%-staplar: andel efter programtyp och år --------------------------
# Ritar en färdig data.frame (kolumner: ar, programtyp, grp, markerad, andel,
# lbl, tooltip). andel summerar alltid till 1 per år -> stapeln blir 100%.
.rita_programtyp <- function(d, subtitle = NULL) {
  farger <- programtyp_farger(d$programtyp)
  g <- ggplot2::ggplot(d, ggplot2::aes(x = factor(ar), y = andel, fill = programtyp,
                                       alpha = markerad, group = grp)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = grp), position = "stack", width = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = lbl),
                       position = ggplot2::position_stack(vjust = 0.5),
                       size = 2.4, color = "#ffffff") +
    ggplot2::scale_fill_manual(values = farger, name = NULL) +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.3), guide = "none") +
    ggplot2::scale_x_discrete(breaks = as.character(.ar_breaks(d$ar))) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = subtitle) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top",
                   panel.grid.major.x = ggplot2::element_blank(),
                   plot.subtitle = ggplot2::element_text(size = 9, color = RD_TEXT_MUTED))
  .girafe_std(g, width_svg = 5, height_svg = 3.2, selection = FALSE)
}

# program_sel = NULL  -> översikt, andel per programtyp.
# program_sel angivet -> hela 100%-stapeln, valt program markerat, resten nedtonat.
skapa_diagram_programtyp <- function(df, metrik, metrik_label, program_sel = NULL) {
  df <- dplyr::filter(df, !is.na(programtyp))
  df$programtyp <- trimws(as.character(df$programtyp))
  df <- dplyr::filter(
    df, nzchar(programtyp),
    !grepl("^(totalt|total|samtliga|alla|samtliga program)$", programtyp, ignore.case = TRUE)
  )
  if (nrow(df) == 0) return(.girafe_std(.tom_plot("Inga data"), width_svg = 5, height_svg = 3))

  if (is.null(program_sel)) {
    d <- df |>
      dplyr::group_by(ar, programtyp) |>
      dplyr::summarise(antal = sum(.data[[metrik]], na.rm = TRUE), .groups = "drop") |>
      dplyr::group_by(ar) |>
      dplyr::mutate(andel = ifelse(sum(antal) > 0, antal / sum(antal), 0)) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        markerad = TRUE,
        grp      = as.character(programtyp),
        lbl      = ifelse(andel >= 0.04, scales::percent(andel, accuracy = 1), ""),
        tooltip  = paste0("<b>", programtyp, "</b><br/>", metrik_label, " ", ar, ": ",
                          scales::percent(andel, accuracy = 1)))
    return(.rita_programtyp(d))
  }

  # Valt program: dela upp dess programtyp i "valt" (markerat) + "övrigt" (nedtonat),
  # och behåll övriga programtyper (nedtonade). Allt normaliseras till 100%.
  sel_typ <- df$programtyp[match(program_sel, df$program)]
  if (is.na(sel_typ)) return(.girafe_std(.tom_plot("Okänd programtyp"), width_svg = 5, height_svg = 3))

  per_typ <- df |>
    dplyr::group_by(ar, programtyp) |>
    dplyr::summarise(typ_total = sum(.data[[metrik]], na.rm = TRUE), .groups = "drop")
  sel_prog <- df |>
    dplyr::filter(program == program_sel) |>
    dplyr::group_by(ar) |>
    dplyr::summarise(sel_antal = sum(.data[[metrik]], na.rm = TRUE), .groups = "drop")

  ovr <- per_typ |>
    dplyr::left_join(sel_prog, by = "ar") |>
    dplyr::mutate(
      sel_antal = dplyr::coalesce(sel_antal, 0),
      antal     = ifelse(programtyp == sel_typ, pmax(typ_total - sel_antal, 0), typ_total),
      grp       = paste0(programtyp, "_ovrigt"),
      markerad  = FALSE) |>
    dplyr::select(ar, programtyp, antal, grp, markerad)

  valt <- sel_prog |>
    dplyr::transmute(ar, programtyp = sel_typ, antal = sel_antal,
                     grp = paste0(sel_typ, "_valt"), markerad = TRUE)

  d <- dplyr::bind_rows(ovr, valt) |>
    dplyr::group_by(ar) |>
    dplyr::mutate(andel = ifelse(sum(antal) > 0, antal / sum(antal), 0)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      lbl     = ifelse(markerad & andel >= 0.03, scales::percent(andel, accuracy = 1), ""),
      tooltip = ifelse(
        markerad,
        paste0("<b>", program_sel, "</b><br/>", ar, ": ",
               scales::percent(andel, accuracy = 1), " av totalen"),
        paste0("<b>", programtyp, "</b><br/>", ar, ": ",
               scales::percent(andel, accuracy = 1))))

  .rita_programtyp(d, subtitle = paste0(program_sel, " markerat"))
}
