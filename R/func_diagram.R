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
