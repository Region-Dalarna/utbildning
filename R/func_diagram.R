# ============================================================
#  func_diagram.R
#  Diagramhjälpare. Interaktiva via ggiraph/girafe (hover + klick).
#  Färger och interaktiv css definieras centralt i def_farger.R.
#
#  Alla publika diagramfunktioner tar rubrik/underrubrik/kalla så att
#  varje diagram är självbärande (titel, filtrering och källa bakas in
#  i själva SVG:n och följer med vid nedladdning).
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

# Källtext -> caption (med "Källa: "-prefix). NULL ger ingen caption.
.kalltext <- function(kalla) {
  if (is.null(kalla) || !nzchar(kalla)) NULL else paste0("Källa: ", kalla)
}

# Kortare etiketter för programtypslegenden (lång text klipps annars).
.programtyp_kort <- function(x) {
  map <- c(
    "Högskoleförberedande program" = "Högskoleförb.",
    "Yrkesprogram"                  = "Yrkesprogram",
    "Övriga utbildningar"           = "Övriga",
    "Övriga utbildning"             = "Övriga",
    "Introduktionsprogram"          = "Introduktion"
  )
  out <- unname(map[x]); ifelse(is.na(out), x, out)
}

# Gemensamt, avskalat tema. Rubrik/underrubrik/caption stylas här så att
# alla diagram ser likadana ut.
.rd_tema <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y    = ggplot2::element_blank(),
      panel.grid.minor      = ggplot2::element_blank(),
      axis.title.y          = ggplot2::element_blank(),
      axis.title.x          = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = ggplot2::element_text(face = "bold", size = 12.5, color = RD_TEXT,
                                            margin = ggplot2::margin(b = 1)),
      plot.subtitle = ggplot2::element_text(size = 9.5, color = RD_TEXT_MUTED,
                                            margin = ggplot2::margin(b = 6)),
      plot.caption  = ggplot2::element_text(size = 7.5, color = RD_TEXT_MUTED, hjust = 0,
                                            margin = ggplot2::margin(t = 8)),
      plot.margin   = ggplot2::margin(6, 12, 5, 5)
    )
}

# Standardiserad girafe. selection = TRUE ger klickbar korsfiltrering.
# Lasso är dolt; PNG-nedladdning (saveaspng) är på för alla diagram.
.girafe_std <- function(g, width_svg = 9, height_svg = 6, selection = FALSE) {
  opts <- list(
    ggiraph::opts_hover(css = RD_HOVER_CSS),
    ggiraph::opts_tooltip(css = RD_TOOLTIP_CSS),
    # Fyll hela containerbredden (annars centreras SVG:n med vita ytor på sidorna)
    ggiraph::opts_sizing(rescale = TRUE, width = 1),
    ggiraph::opts_toolbar(saveaspng = TRUE,
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
skapa_diagram_bar <- function(df, metrik, metrik_label, ar = NULL,
                              rubrik = NULL, underrubrik = NULL, kalla = NULL) {
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
    ggplot2::labs(x = metrik_label, y = NULL,
                  title = rubrik, subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema()

  .girafe_std(g, width_svg = 6.8, height_svg = 7.0, selection = TRUE)
}

# ---- Stapel: könsuppdelad (staplad), klickbar väljer programmet -----------
skapa_diagram_bar_kon <- function(df, metrik_kv, metrik_man, metrik_label, ar = NULL,
                                  rubrik = NULL, underrubrik = NULL, kalla = NULL) {
  d <- df |>
    dplyr::group_by(program) |>
    dplyr::summarise(
      Kvinnor = sum(.data[[metrik_kv]],  na.rm = TRUE),
      `Män`   = sum(.data[[metrik_man]], na.rm = TRUE),
      .groups = "drop") |>
    dplyr::mutate(tot = Kvinnor + `Män`) |>
    dplyr::filter(tot > 0) |>
    tidyr::pivot_longer(c(Kvinnor, `Män`), names_to = "kon", values_to = "antal") |>
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
    ggplot2::labs(x = metrik_label, y = NULL,
                  title = rubrik, subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top")

  .girafe_std(g, width_svg = 6.8, height_svg = 7.0, selection = TRUE)
}

# ---- Linje: utveckling över tid -------------------------------------------
skapa_diagram_trend <- function(df, metrik, metrik_label, program_sel = NULL,
                                rubrik = NULL, underrubrik = NULL, kalla = NULL) {
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
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))

  .girafe_std(g, width_svg = 5, height_svg = 3.1, selection = FALSE)
}

# ---- Linje: utveckling över tid, könsuppdelad (två linjer) ----------------
skapa_diagram_trend_kon <- function(df, metrik_kv, metrik_man, metrik_label, program_sel = NULL,
                                    rubrik = NULL, underrubrik = NULL, kalla = NULL) {
  if (!is.null(program_sel)) df <- dplyr::filter(df, program == program_sel)

  d <- df |>
    dplyr::group_by(ar) |>
    dplyr::summarise(
      Kvinnor = sum(.data[[metrik_kv]],  na.rm = TRUE),
      `Män`   = sum(.data[[metrik_man]], na.rm = TRUE),
      .groups = "drop") |>
    tidyr::pivot_longer(c(Kvinnor, `Män`), names_to = "kon", values_to = "antal") |>
    dplyr::mutate(tooltip = paste0(kon, " · ", metrik_label, " ", ar, ": ", antal))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = antal, color = kon, group = kon)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = paste(ar, kon)), size = 2.2) +
    ggplot2::scale_color_manual(values = KON_FARGER, name = NULL) +
    ggplot2::scale_x_continuous(breaks = .ar_breaks(d$ar)) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(
      legend.position    = "top",
      panel.grid.major.y = ggplot2::element_line(color = "#eef2f5")
    )

  .girafe_std(g, width_svg = 5, height_svg = 3.3, selection = FALSE)
}

# ---- 100%-staplar: andel efter programtyp och år --------------------------
# Staplarna ritas från ANDEL (y = andel) som summerar till 1 per år, så att
# stapelhöjd och hover-värde alltid är exakt samma siffra. Ingen etikett –
# informationen finns i hover.
.rita_programtyp <- function(d, rubrik = NULL, underrubrik = NULL, kalla = NULL) {
  farger <- programtyp_farger(d$programtyp)

  g <- ggplot2::ggplot(d, ggplot2::aes(x = factor(ar), y = andel, fill = programtyp,
                                       alpha = markerad, group = grp)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = paste0(grp, "_", ar)),
      position = "stack", width = 0.8) +
    ggplot2::scale_fill_manual(values = farger, name = NULL, labels = .programtyp_kort) +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.3), guide = "none") +
    ggplot2::scale_x_discrete(breaks = as.character(.ar_breaks(d$ar))) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1), nrow = 1)) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(
      legend.position    = "top",
      legend.key.size    = ggplot2::unit(9, "pt"),
      legend.text        = ggplot2::element_text(size = 7.5),
      legend.box.margin  = ggplot2::margin(0, 0, 0, 0),
      panel.grid.major.x = ggplot2::element_blank()
    )
  .girafe_std(g, width_svg = 5, height_svg = 3.5, selection = FALSE)
}

# program_sel = NULL  -> översikt, andel per programtyp.
# program_sel angivet -> hela 100%-stapeln, valt program markerat, resten nedtonat.
skapa_diagram_programtyp <- function(df, metrik, metrik_label, program_sel = NULL,
                                     rubrik = NULL, underrubrik = NULL, kalla = NULL) {
  df <- dplyr::filter(df, !is.na(programtyp))
  df$programtyp <- trimws(as.character(df$programtyp))
  df <- dplyr::filter(
    df, nzchar(programtyp),
    !grepl("^(totalt|total|samtliga|alla|samtliga program)$", programtyp, ignore.case = TRUE)
  )
  if (nrow(df) == 0) return(.girafe_std(.tom_plot("Inga data"), width_svg = 5, height_svg = 3))

  if (is.null(program_sel)) {
    seg <- df |>
      dplyr::group_by(ar, programtyp) |>
      dplyr::summarise(antal = sum(.data[[metrik]], na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(antal = pmax(antal, 0))
    tot <- seg |>
      dplyr::group_by(ar) |>
      dplyr::summarise(ar_total = sum(antal), .groups = "drop")
    d <- seg |>
      dplyr::left_join(tot, by = "ar") |>
      dplyr::mutate(
        andel    = dplyr::if_else(ar_total > 0, antal / ar_total, 0),
        markerad = TRUE,
        grp      = as.character(programtyp),
        tooltip  = paste0("<b>", programtyp, "</b><br/>", metrik_label, " ", ar, ": ",
                          scales::percent(andel, accuracy = 1)))
    return(.rita_programtyp(d, rubrik = rubrik, underrubrik = underrubrik, kalla = kalla))
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
      antal     = ifelse(programtyp == sel_typ, pmax(typ_total - sel_antal, 0), pmax(typ_total, 0)),
      grp       = paste0(programtyp, "_ovrigt"),
      markerad  = FALSE) |>
    dplyr::select(ar, programtyp, antal, grp, markerad)

  valt <- sel_prog |>
    dplyr::transmute(ar, programtyp = sel_typ, antal = pmax(sel_antal, 0),
                     grp = paste0(sel_typ, "_valt"), markerad = TRUE)

  combined <- dplyr::bind_rows(ovr, valt) |>
    dplyr::mutate(antal = pmax(antal, 0))
  tot <- combined |>
    dplyr::group_by(ar) |>
    dplyr::summarise(ar_total = sum(antal), .groups = "drop")
  d <- combined |>
    dplyr::left_join(tot, by = "ar") |>
    dplyr::mutate(
      andel = dplyr::if_else(ar_total > 0, antal / ar_total, 0),
      tooltip = ifelse(
        markerad,
        paste0("<b>", program_sel, "</b><br/>", ar, ": ",
               scales::percent(andel, accuracy = 1), " av totalen"),
        paste0("<b>", programtyp, "</b><br/>", ar, ": ",
               scales::percent(andel, accuracy = 1))))

  .rita_programtyp(d, rubrik = rubrik, underrubrik = underrubrik, kalla = kalla)
}

# ============================================================
#  Elever: diagram för årskursindelning och viktade andelar
# ============================================================

# ---- Staplad liggande stapel: elever per program och årskurs --------------
skapa_diagram_arskurs <- function(df, ar = NULL,
                                  rubrik = NULL, underrubrik = NULL, kalla = NULL) {
  d <- df |>
    dplyr::group_by(program) |>
    dplyr::summarise(
      `Årskurs 1` = sum(elever_ak1, na.rm = TRUE),
      `Årskurs 2` = sum(elever_ak2, na.rm = TRUE),
      `Årskurs 3` = sum(elever_ak3, na.rm = TRUE),
      .groups = "drop") |>
    dplyr::mutate(tot = `Årskurs 1` + `Årskurs 2` + `Årskurs 3`) |>
    dplyr::filter(tot > 0) |>
    tidyr::pivot_longer(c(`Årskurs 1`, `Årskurs 2`, `Årskurs 3`),
                        names_to = "arskurs", values_to = "antal") |>
    dplyr::mutate(
      program = forcats::fct_reorder(program, tot),
      tooltip = paste0("<b>", program, "</b><br/>", arskurs, ": ", antal),
      data_id = as.character(program))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = antal, y = program, fill = arskurs)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = data_id), width = 0.72) +
    ggplot2::scale_fill_manual(values = ARSKURS_FARGER, name = NULL) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = "Antal elever", y = NULL,
                  title = rubrik, subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top")

  .girafe_std(g, width_svg = 6.8, height_svg = 7.0, selection = TRUE)
}

# ---- Trend: elever per årskurs över tid (tre linjer) ----------------------
skapa_diagram_trend_arskurs <- function(df, program_sel = NULL,
                                        rubrik = NULL, underrubrik = NULL, kalla = NULL) {
  if (!is.null(program_sel)) df <- dplyr::filter(df, program == program_sel)

  d <- df |>
    dplyr::group_by(ar) |>
    dplyr::summarise(
      `Årskurs 1` = sum(elever_ak1, na.rm = TRUE),
      `Årskurs 2` = sum(elever_ak2, na.rm = TRUE),
      `Årskurs 3` = sum(elever_ak3, na.rm = TRUE),
      .groups = "drop") |>
    tidyr::pivot_longer(c(`Årskurs 1`, `Årskurs 2`, `Årskurs 3`),
                        names_to = "arskurs", values_to = "antal") |>
    dplyr::mutate(tooltip = paste0(arskurs, " ", ar, ": ", antal))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = antal, color = arskurs, group = arskurs)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = paste(ar, arskurs)), size = 2.2) +
    ggplot2::scale_color_manual(values = ARSKURS_FARGER, name = NULL) +
    ggplot2::scale_x_continuous(breaks = .ar_breaks(d$ar)) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top",
                   panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))

  .girafe_std(g, width_svg = 5, height_svg = 3.3, selection = FALSE)
}

# ---- Viktad andel (%) per program – liggande stapel -----------------------
# andel_kol är ett procenttal (0-100), vikt_kol är antal elever. Andelen
# aggregeras som viktat medel: sum(andel * vikt) / sum(vikt).
skapa_diagram_bar_andel <- function(df, andel_kol, vikt_kol, metrik_label, ar = NULL,
                                    rubrik = NULL, underrubrik = NULL, kalla = NULL) {
  d <- df |>
    dplyr::group_by(program) |>
    dplyr::summarise(
      vikt  = sum(.data[[vikt_kol]], na.rm = TRUE),
      andel = ifelse(vikt > 0,
                     sum(.data[[andel_kol]] * .data[[vikt_kol]], na.rm = TRUE) / vikt,
                     NA_real_),
      .groups = "drop") |>
    dplyr::filter(!is.na(andel), vikt > 0) |>
    dplyr::mutate(
      program = forcats::fct_reorder(program, andel),
      tooltip = paste0("<b>", program, "</b><br/>", .metrik_ar(metrik_label, ar), ": ",
                       scales::number(andel, accuracy = 0.1), " %"),
      data_id = as.character(program))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = andel, y = program)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = data_id), fill = RD_PRIMARY, width = 0.74) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, " %"),
                                expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = metrik_label, y = NULL,
                  title = rubrik, subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema()

  .girafe_std(g, width_svg = 6.8, height_svg = 7.0, selection = TRUE)
}

# ---- Viktad andel (%) över tid – linje ------------------------------------
skapa_diagram_trend_andel <- function(df, andel_kol, vikt_kol, metrik_label, program_sel = NULL,
                                      rubrik = NULL, underrubrik = NULL, kalla = NULL) {
  if (!is.null(program_sel)) df <- dplyr::filter(df, program == program_sel)

  d <- df |>
    dplyr::group_by(ar) |>
    dplyr::summarise(
      vikt  = sum(.data[[vikt_kol]], na.rm = TRUE),
      andel = ifelse(vikt > 0,
                     sum(.data[[andel_kol]] * .data[[vikt_kol]], na.rm = TRUE) / vikt,
                     NA_real_),
      .groups = "drop") |>
    dplyr::filter(!is.na(andel)) |>
    dplyr::mutate(tooltip = paste0(metrik_label, " ", ar, ": ",
                                   scales::number(andel, accuracy = 0.1), " %"))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = andel)) +
    ggplot2::geom_line(color = RD_PRIMARY, linewidth = 0.9) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = ar), color = RD_PRIMARY, size = 2.4) +
    ggplot2::scale_x_continuous(breaks = .ar_breaks(d$ar)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, " %")) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))

  .girafe_std(g, width_svg = 5, height_svg = 3.3, selection = FALSE)
}

# ============================================================
#  Genomströmning: trend med Dalarna + Riket som jämförelse
# ============================================================

# Trendlinje för genomströmning. df_dalarna är redan filtrerat till Dalarna
# (kommunrader summerade), df_riket är rikets rad (geo_niva == "riket").
# program_sel = NULL -> alla program sammanslagen (viktat medel med antal
# elever är okänt -> enkelt medel per år, ange i underrubrik).
skapa_diagram_genomstromning_trend <- function(df_dalarna, df_riket,
                                               program_sel = NULL,
                                               rubrik = NULL,
                                               underrubrik = NULL,
                                               kalla = NULL) {
  # Dalarna: medel per år (datan är redan en andel per program/kommunkombination).
  d_dal <- df_dalarna |>
    dplyr::group_by(ar) |>
    dplyr::summarise(andel = mean(andel, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(serie = "Dalarna",
                  tooltip = paste0("Dalarna ", ar, ": ",
                                   scales::number(andel, accuracy = 0.1), " %"))

  d_rik <- df_riket |>
    dplyr::rename(andel = andel_riket) |>
    dplyr::mutate(serie = "Riket",
                  tooltip = paste0("Riket ", ar, ": ",
                                   scales::number(andel, accuracy = 0.1), " %"))

  d <- dplyr::bind_rows(d_dal, d_rik) |>
    dplyr::filter(!is.na(andel))

  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 5, 3))

  serie_farger <- c("Dalarna" = RD_PRIMARY, "Riket" = RD_TEXT_MUTED)

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = andel,
                                       color = serie, group = serie)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = paste(serie, ar)), size = 2.4) +
    ggplot2::scale_color_manual(values = serie_farger, name = NULL) +
    ggplot2::scale_x_continuous(breaks = .ar_breaks(d$ar)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, " %"),
                                limits = c(0, NA)) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position  = "top",
                   panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))

  .girafe_std(g, width_svg = 5, height_svg = 3.3, selection = FALSE)
}

# Stapel per program – genomströmningsandel för valt år i Dalarna.
skapa_diagram_genomstromning_bar <- function(df, ar = NULL,
                                             rubrik = NULL,
                                             underrubrik = NULL,
                                             kalla = NULL) {
  d <- df |>
    dplyr::group_by(program) |>
    dplyr::summarise(andel = mean(andel, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(!is.na(andel)) |>
    dplyr::mutate(
      program = forcats::fct_reorder(program, andel),
      tooltip = paste0("<b>", program, "</b><br/>",
                       .metrik_ar("Andel med examen", ar), ": ",
                       scales::number(andel, accuracy = 0.1), " %"),
      data_id = as.character(program))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = andel, y = program)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = data_id),
      fill = RD_PRIMARY, width = 0.74) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(x, " %"),
      expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = "Andel med examen (%)", y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla)) +
    .rd_tema()

  .girafe_std(g, width_svg = 6.8, height_svg = 7.0, selection = TRUE)
}

# ============================================================
#  Etablering: grupperad stapel (antal år) + Dalarna vs Riket-trend
# ============================================================

# Etiketter för antal år efter examen
.ar_efter_etiketter <- c("1" = "1 år", "3" = "3 år", "5" = "5 år", "7" = "7 år")
ANTAL_AR_FARGER <- c(
  "1 år" = rd_farg("rd-blue-light", "#8edded"),
  "3 år" = rd_farg("rd-accent",     "#54a1bd"),
  "5 år" = rd_farg("rd-primary",    "#158daf"),
  "7 år" = rd_farg("rd-blue-deep",  "#0074a2")
)

# Liggande stapel. antal_ar_val = 1/3/5/7 -> en stapel per program för det
# valda uppföljningsåret (+ Riket-referenslinje). antal_ar_val = "alla" ->
# grupperade staplar med en stapel per uppföljningsår (1/3/5/7).
skapa_diagram_etablering_bar <- function(df, metrik, metrik_label,
                                         antal_ar_val = 3,
                                         riket_andel = NULL,
                                         rubrik = NULL, underrubrik = NULL,
                                         kalla = NULL) {

  # ---- Läge: alla uppföljningsår som grupperade staplar -------------------
  if (identical(as.character(antal_ar_val), "alla")) {
    d <- df |>
      dplyr::mutate(ar_etikett = .ar_efter_etiketter[as.character(antal_ar)]) |>
      dplyr::filter(!is.na(ar_etikett)) |>
      dplyr::group_by(namn, ar_etikett) |>
      dplyr::summarise(status_sum = sum(.data[[metrik]], na.rm = TRUE),
                       antal_sum  = sum(antal, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::mutate(andel = dplyr::if_else(antal_sum > 0,
                                           status_sum / antal_sum, NA_real_)) |>
      dplyr::filter(!is.na(andel))

    if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 6.8, 7))

    # Sortera program på medel över alla år
    namn_order <- d |>
      dplyr::group_by(namn) |>
      dplyr::summarise(m = mean(andel, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(m) |>
      dplyr::pull(namn)

    d <- d |>
      dplyr::mutate(
        namn       = factor(namn, levels = namn_order),
        ar_etikett = factor(ar_etikett, levels = c("1 år", "3 år", "5 år", "7 år")),
        tooltip    = paste0("<b>", namn, "</b><br/>", ar_etikett, ": ",
                            scales::percent(andel, accuracy = 0.1)))

    g <- ggplot2::ggplot(d, ggplot2::aes(x = andel, y = namn, fill = ar_etikett)) +
      ggiraph::geom_col_interactive(
        ggplot2::aes(tooltip = tooltip, data_id = paste(namn, ar_etikett)),
        position = ggplot2::position_dodge(width = 0.8), width = 0.72) +
      ggplot2::scale_fill_manual(values = ANTAL_AR_FARGER, name = "År efter examen") +
      ggplot2::scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = ggplot2::expansion(mult = c(0, 0.04)), limits = c(0, 1)) +
      ggplot2::labs(x = metrik_label, y = NULL,
                    title = rubrik, subtitle = underrubrik,
                    caption = .kalltext(kalla)) +
      .rd_tema() +
      ggplot2::theme(legend.position = "top")

    return(.girafe_std(g, width_svg = 11,
                       height_svg = max(6.5, dplyr::n_distinct(d$namn) * 0.6 + 1.5),
                       selection = TRUE))
  }

  # ---- Läge: ett uppföljningsår (+ Riket-referenslinje) -------------------
  d <- df |>
    dplyr::filter(antal_ar == as.integer(antal_ar_val)) |>
    dplyr::group_by(namn, program) |>
    dplyr::summarise(status_sum = sum(.data[[metrik]], na.rm = TRUE),
                     antal_sum  = sum(antal, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(andel = dplyr::if_else(antal_sum > 0,
                                         status_sum / antal_sum, NA_real_)) |>
    dplyr::filter(!is.na(andel))

  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 6.8, 7))

  # Sortera på andel, men håll inriktningar grupperade under sitt program.
  # program == namn när vi visar på programnivå.
  prog_order <- d |>
    dplyr::group_by(program) |>
    dplyr::summarise(prog_andel = sum(status_sum) / sum(antal_sum), .groups = "drop") |>
    dplyr::arrange(prog_andel) |>
    dplyr::pull(program)

  d <- d |>
    dplyr::mutate(
      program = factor(program, levels = prog_order),
      namn    = forcats::fct_reorder2(namn, program, andel,
                                      .fun = function(p, a) as.integer(p) + a * 0.01)
    ) |>
    dplyr::mutate(
      tooltip = paste0("<b>", namn, "</b><br/>",
                       antal_ar_val, " år efter examen: ",
                       scales::percent(andel, accuracy = 0.1))
    )

  g <- ggplot2::ggplot(d, ggplot2::aes(x = andel, y = namn)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = as.character(namn)),
      fill = RD_PRIMARY, width = 0.74) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.04)),
      limits = c(0, 1)) +
    ggplot2::labs(x = metrik_label, y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla)) +
    .rd_tema()

  # Riket-referenslinje (streckad) om tillgänglig
  if (!is.null(riket_andel) && !is.na(riket_andel)) {
    g <- g +
      ggplot2::geom_vline(xintercept = riket_andel, linetype = "dashed",
                          color = RD_TEXT_MUTED, linewidth = 0.7) +
      ggplot2::annotate("text", x = riket_andel, y = Inf,
                        label = paste0("Riket ", scales::percent(riket_andel, accuracy = 0.1)),
                        hjust = -0.08, vjust = 1.5, size = 2.8,
                        color = RD_TEXT_MUTED)
  }

  .girafe_std(g, width_svg = 11, height_svg = max(5, nrow(d) * 0.36 + 1.8),
              selection = TRUE)
}

# Trendlinje: Dalarna vs Riket, ett mått, ett antal_ar-värde per serie.
skapa_diagram_etablering_trend <- function(df_dalarna, df_riket,
                                           metrik, metrik_label,
                                           antal_ar_val = 3,
                                           rubrik = NULL, underrubrik = NULL,
                                           kalla = NULL) {
  d_dal <- df_dalarna |>
    dplyr::filter(antal_ar == antal_ar_val) |>
    dplyr::group_by(ar) |>
    dplyr::summarise(status_sum = sum(.data[[metrik]], na.rm = TRUE),
                     antal_sum  = sum(antal, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(
      andel   = dplyr::if_else(antal_sum > 0, status_sum / antal_sum, NA_real_),
      serie   = "Dalarna",
      tooltip = paste0("Dalarna ", ar, ": ", scales::percent(andel, accuracy = 0.1)))

  d_rik <- df_riket |>
    dplyr::filter(antal_ar == antal_ar_val) |>
    dplyr::rename(andel = andel_riket) |>
    dplyr::mutate(
      serie   = "Riket",
      tooltip = paste0("Riket ", ar, ": ", scales::percent(andel, accuracy = 0.1)))

  d <- dplyr::bind_rows(d_dal, d_rik) |> dplyr::filter(!is.na(andel))
  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 5, 3.3))

  # Mappa startår -> full examensperiod ("2014–2016") för x-axeletiketter.
  # Hämtas ur df_dalarna som har exam_ar_interval.
  ar_etik <- df_dalarna |>
    dplyr::distinct(ar, exam_ar_interval) |>
    dplyr::mutate(etikett = gsub("-", "\u2013", exam_ar_interval, fixed = TRUE))
  etik_vekt <- stats::setNames(ar_etik$etikett, as.character(ar_etik$ar))

  serie_farger <- c("Dalarna" = RD_PRIMARY, "Riket" = RD_TEXT_MUTED)

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = andel,
                                       color = serie, group = serie,
                                       linetype = serie)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = paste(serie, ar)), size = 2.4) +
    ggplot2::scale_color_manual(values = serie_farger, name = NULL) +
    ggplot2::scale_linetype_manual(
      values = c("Dalarna" = "solid", "Riket" = "dashed"), guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = sort(unique(d$ar)),
      labels = function(x) etik_vekt[as.character(x)]) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                limits = c(0, NA)) +
    ggplot2::labs(x = "Examensperiod", y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position    = "top",
                   axis.text.x        = ggplot2::element_text(size = 7.5),
                   panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))

  .girafe_std(g, width_svg = 5, height_svg = 3.3, selection = FALSE)
}

# ============================================================
#  UI-hjälpare: gråa ut + inaktivera specifika knappar i en
#  shinyWidgets radioGroupButtons. Manuell rekursion (ingen
#  tagQuery) för full förutsägbarhet oavsett htmltools-version.
#
#  Hittar varje <label> som innehåller en <input> vars value
#  finns i disabled_vals, inaktiverar input:en (disabled) och
#  gråar labeln. Ett inaktiverat radio-input kan inte väljas.
# ============================================================
grada_radioknappar <- function(tag, disabled_vals) {
  if (length(disabled_vals) == 0) return(tag)

  walk <- function(node) {
    if (!inherits(node, "shiny.tag")) return(node)

    if (identical(node$name, "label") && length(node$children)) {
      # Finns en input med value i disabled_vals bland barnen?
      mal <- FALSE
      for (ch in node$children) {
        if (inherits(ch, "shiny.tag") && identical(ch$name, "input")) {
          v <- ch$attribs$value
          if (!is.null(v) && as.character(v) %in% disabled_vals) mal <- TRUE
        }
      }
      if (mal) {
        # Inaktivera input-barnen och gråa labeln
        node$children <- lapply(node$children, function(ch) {
          if (inherits(ch, "shiny.tag") && identical(ch$name, "input")) {
            ch$attribs$disabled <- "disabled"
          }
          ch
        })
        gammal <- node$attribs$style
        node$attribs$style <- paste0(
          if (!is.null(gammal)) paste0(gammal, ";") else "",
          "opacity:0.4;pointer-events:none;cursor:not-allowed;")
        return(node)
      }
    }

    if (length(node$children))
      node$children <- lapply(node$children, walk)
    node
  }
  walk(tag)
}
