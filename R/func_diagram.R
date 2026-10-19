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

# Snälla heltalsbrytpunkter för antalsaxlar (elever, studerande, personer -
# alltid heltal). ggplot2/scales pretty()-brytpunkter kan annars bli
# decimaler när spannet är litet, t.ex. en enda mätpunkt (22.95, 22.975, ...).
.heltal_skala <- function(n = 5) {
  function(x) {
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) rng <- rng + c(-2, 2)
    steg_kandidater <- c(1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500,
                         1000, 2000, 2500, 5000, 10000, 20000, 25000, 50000)
    steg <- steg_kandidater[match(TRUE, steg_kandidater >= diff(rng) / n)]
    if (is.na(steg)) steg <- ceiling(diff(rng) / n)
    seq(floor(rng[1] / steg) * steg, ceiling(rng[2] / steg) * steg, by = steg)
  }
}

# Liten platshållarplot.
.tom_plot <- function(msg = "") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, color = RD_TEXT_MUTED, size = 3) +
    ggplot2::theme_void()
}

# Mikrodata från SCB:s uppdrag Regionala utvecklingsdatabasen. Tabellnamnen
# följer registrens namn i leveransens variabellista.
KALLA_RUD <- "Regionala utvecklingsdatabasen (SCB)"
kalla_rud <- function(tabell) paste0(KALLA_RUD, ", ", tabell)

# ---- Röjandekontroll -----------------------------------------------------
# Värden som bygger på färre än min_antal personer visas inte (MIN_ANTAL i
# func_data.R). Antal döljs när de är 1 till min_antal - 1, andelar och
# medelvärden när deras underlag är under min_antal. min_antal = 0 stänger
# av kontrollen (publicerad statistik).
#
# Dolda ANTAL ritas som en grå, streckad platshållarstapel med det fasta
# värdet ROJ_PLATSHALLARE (tooltip "färre än 4") - man ser att gruppen
# finns men inte hur stor den är. Dolda ANDELAR får ingen stapel, bara
# texten "färre än 4", eftersom en fast längd vore missvisande där.
.dold <- function(n, min_antal) !is.na(n) & n > 0 & n < min_antal
.dold_text <- function(min_antal) paste("färre än", min_antal)
ROJ_PLATSHALLARE <- 1.5
RD_DOLD_FYLL <- "#e1e5e8"
.dold_fyll_namn <- function(min_antal) paste("Färre än", min_antal)

# Etikett "färre än 4" vid nollan för dolda andelsstaplar (liggande diagram).
.dold_etikett <- function(d, y, min_antal) {
  ggplot2::geom_text(data = d, ggplot2::aes(x = 0, y = .data[[y]]),
                     label = .dold_text(min_antal), hjust = -0.08, size = 2.7,
                     color = RD_TEXT_MUTED, inherit.aes = FALSE)
}

# Källtext -> caption (med "Källa: "-prefix). NULL ger ingen caption.
# dolda = TRUE lägger till en rad om att värden har dolts; platshallare =
# TRUE förklarar de gråa platshållarstaplarna i stället.
.kalltext <- function(kalla, dolda = FALSE, min_antal = MIN_ANTAL, platshallare = FALSE) {
  k <- if (is.null(kalla) || !nzchar(kalla)) NULL else paste0("Källa: ", kalla)
  if (!isTRUE(dolda)) return(k)
  rad <- if (isTRUE(platshallare))
    paste0("Grå streckade staplar: ", .dold_text(min_antal), " (exakt värde visas inte).")
  else paste0("Värden som bygger på ", .dold_text(min_antal), " personer visas inte.")
  paste(c(k, rad), collapse = "\n")
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

# Höjd (px) för en girafe-container så att den matchar diagrammets egna
# bredd/höjd-förhållande (width_svg/height_svg) i stället för en gissad, rund
# pixelhöjd - annars blir det antingen tomt utrymme (container för hög) eller
# beskuret innehåll (container för låg). kolumnbredd_px är en ungefärlig
# bredd för kolumnen (t.ex. column(7, ...) i en sidebarLayout-huvudpanel).
.girafe_hojd <- function(width_svg, height_svg, kolumnbredd_px) {
  paste0(round(kolumnbredd_px * height_svg / width_svg), "px")
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
                              rubrik = NULL, underrubrik = NULL, kalla = NULL,
                              min_antal = MIN_ANTAL) {
  d <- df |>
    dplyr::group_by(program) |>
    dplyr::summarise(antal = sum(.data[[metrik]], na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(antal > 0) |>
    dplyr::mutate(
      dold    = .dold(antal, min_antal),
      varde   = dplyr::if_else(dold, ROJ_PLATSHALLARE, as.numeric(antal)),
      # Sortera på det synliga värdet, så att dolda inte avslöjas av ordningen.
      program = forcats::fct_reorder(program, varde),
      fyll    = dplyr::if_else(dold, RD_DOLD_FYLL, RD_PRIMARY),
      kant    = dplyr::if_else(dold, RD_TEXT_MUTED, NA_character_),
      tooltip = paste0("<b>", program, "</b><br/>", .metrik_ar(metrik_label, ar), ": ",
                       dplyr::if_else(dold, .dold_text(min_antal), as.character(antal))),
      data_id = as.character(program)
    )

  g <- ggplot2::ggplot(d, ggplot2::aes(x = varde, y = program)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = data_id, fill = fyll, colour = kant),
      width = 0.74, linetype = "22", linewidth = 0.35) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(breaks = .heltal_skala(), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = metrik_label, y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, any(d$dold), min_antal, platshallare = TRUE)) +
    .rd_tema()

  .girafe_std(g, width_svg = 6.8, height_svg = 7.0, selection = TRUE)
}

# ---- Stapel: könsuppdelad (staplad), klickbar väljer programmet -----------
skapa_diagram_bar_kon <- function(df, metrik_kv, metrik_man, metrik_label, ar = NULL,
                                  rubrik = NULL, underrubrik = NULL, kalla = NULL,
                                  min_antal = MIN_ANTAL) {
  d <- df |>
    dplyr::group_by(program) |>
    dplyr::summarise(
      Kvinnor = sum(.data[[metrik_kv]],  na.rm = TRUE),
      `Män`   = sum(.data[[metrik_man]], na.rm = TRUE),
      .groups = "drop") |>
    dplyr::filter(Kvinnor + `Män` > 0) |>
    tidyr::pivot_longer(c(Kvinnor, `Män`), names_to = "kon", values_to = "antal") |>
    dplyr::mutate(dold  = .dold(antal, min_antal),
                  varde = dplyr::if_else(dold, ROJ_PLATSHALLARE, as.numeric(antal)),
                  fyll  = dplyr::if_else(dold, .dold_fyll_namn(min_antal), kon),
                  kant  = dplyr::if_else(dold, RD_TEXT_MUTED, NA_character_)) |>
    dplyr::group_by(program) |>
    # Sortera på det synliga värdet, så att dolda inte avslöjas av ordningen.
    dplyr::mutate(tot = sum(varde)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      program = forcats::fct_reorder(program, tot),
      tooltip = paste0("<b>", program, "</b><br/>", kon, " · ",
                       .metrik_ar(metrik_label, ar), ": ",
                       dplyr::if_else(dold, .dold_text(min_antal), as.character(antal))),
      data_id = as.character(program))

  dold_namn <- .dold_fyll_namn(min_antal)
  g <- ggplot2::ggplot(d, ggplot2::aes(x = varde, y = program, fill = fyll, group = kon)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = data_id, colour = kant),
      width = 0.72, linetype = "22", linewidth = 0.35) +
    ggplot2::scale_fill_manual(values = c(KON_FARGER, stats::setNames(RD_DOLD_FYLL, dold_namn)),
                               breaks = c(names(KON_FARGER), if (any(d$dold)) dold_namn),
                               name = NULL) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(breaks = .heltal_skala(), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = metrik_label, y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, any(d$dold), min_antal, platshallare = TRUE)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top")

  .girafe_std(g, width_svg = 6.8, height_svg = 7.0, selection = TRUE)
}

# ---- Linje: utveckling över tid -------------------------------------------
skapa_diagram_trend <- function(df, metrik, metrik_label, program_sel = NULL,
                                rubrik = NULL, underrubrik = NULL, kalla = NULL,
                                min_antal = MIN_ANTAL) {
  if (!is.null(program_sel)) df <- dplyr::filter(df, program == program_sel)

  d <- df |>
    dplyr::group_by(ar) |>
    dplyr::summarise(antal = sum(.data[[metrik]], na.rm = TRUE), .groups = "drop")
  dolda <- any(.dold(d$antal, min_antal))
  # Dolda punkter blir NA - linjen får ett avbrott där.
  d <- d |>
    dplyr::mutate(antal = dplyr::if_else(.dold(antal, min_antal), NA_real_, as.numeric(antal)),
                  tooltip = paste0(metrik_label, " ", ar, ": ", antal))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = antal)) +
    ggplot2::geom_line(color = RD_PRIMARY, linewidth = 0.9, na.rm = TRUE) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = ar), color = RD_PRIMARY, size = 2.4,
      na.rm = TRUE) +
    ggplot2::scale_x_continuous(breaks = .ar_breaks(d$ar)) +
    ggplot2::scale_y_continuous(breaks = .heltal_skala()) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, dolda, min_antal)) +
    .rd_tema() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))

  .girafe_std(g, width_svg = 5, height_svg = 3.1, selection = FALSE)
}

# ---- Linje: utveckling över tid, könsuppdelad (två linjer) ----------------
skapa_diagram_trend_kon <- function(df, metrik_kv, metrik_man, metrik_label, program_sel = NULL,
                                    rubrik = NULL, underrubrik = NULL, kalla = NULL,
                                    min_antal = MIN_ANTAL) {
  if (!is.null(program_sel)) df <- dplyr::filter(df, program == program_sel)

  d <- df |>
    dplyr::group_by(ar) |>
    dplyr::summarise(
      Kvinnor = sum(.data[[metrik_kv]],  na.rm = TRUE),
      `Män`   = sum(.data[[metrik_man]], na.rm = TRUE),
      .groups = "drop") |>
    tidyr::pivot_longer(c(Kvinnor, `Män`), names_to = "kon", values_to = "antal")
  dolda <- any(.dold(d$antal, min_antal))
  d <- d |>
    dplyr::mutate(antal = dplyr::if_else(.dold(antal, min_antal), NA_real_, as.numeric(antal)),
                  tooltip = paste0(kon, " · ", metrik_label, " ", ar, ": ", antal))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = antal, color = kon, group = kon)) +
    ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = paste(ar, kon)), size = 2.2, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = KON_FARGER, name = NULL) +
    ggplot2::scale_x_continuous(breaks = .ar_breaks(d$ar)) +
    ggplot2::scale_y_continuous(breaks = .heltal_skala()) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, dolda, min_antal)) +
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
# d har kolumnen fyll (teckenförklaringens namn, i önskad ordning) och
# markerad. Nedtonade delar får en blekare färg i stället för genomskinlighet,
# så att teckenförklaringen visar samma färg som staplarna.
.blek_farg <- function(farg, styrka = 0.3) {
  m <- grDevices::col2rgb(farg) / 255
  grDevices::rgb(t(m * styrka + (1 - styrka)))
}

.rita_programtyp <- function(d, rubrik = NULL, underrubrik = NULL, kalla = NULL) {
  typ_farg <- programtyp_farger(d$programtyp)
  fyll_farg <- d |>
    dplyr::distinct(fyll, programtyp, markerad) |>
    dplyr::mutate(farg = ifelse(markerad, typ_farg[programtyp], .blek_farg(typ_farg[programtyp])))
  farger <- stats::setNames(fyll_farg$farg, fyll_farg$fyll)
  # En rad räcker oftast; långa namn (valt program) får två rader.
  rader <- if (sum(nchar(names(farger))) > 60) 2 else 1

  g <- ggplot2::ggplot(d, ggplot2::aes(x = factor(ar), y = andel, fill = fyll, group = grp)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = paste0(grp, "_", ar)),
      position = "stack", width = 0.8) +
    ggplot2::scale_fill_manual(values = farger, breaks = levels(d$fyll), name = NULL) +
    ggplot2::scale_x_discrete(breaks = as.character(.ar_breaks(d$ar))) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = rader, byrow = TRUE)) +
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
# Alla andelar är andelar av helheten - det står en gång i underrubriken
# ("andel av alla antagna" o.s.v.), inte i varje tooltip.
skapa_diagram_programtyp <- function(df, metrik, metrik_label, program_sel = NULL,
                                     rubrik = NULL, underrubrik = NULL, kalla = NULL) {
  underrubrik <- paste0(underrubrik, " \u00b7 andel av alla ",
                        tolower(sub("^Antal ", "", metrik_label)))
  pct <- function(x) scales::percent(x, accuracy = 1)
  tt  <- function(namn, ar, andel) paste0("<b>", namn, " ", ar, ":</b> ", pct(andel))
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
        fyll     = factor(.programtyp_kort(programtyp),
                          levels = .programtyp_kort(sort(unique(programtyp)))),
        tooltip  = tt(programtyp, ar, andel))
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
  # Teckenförklaring: valt program först, sedan övriga i programtypsordning.
  # Resten av valda programtypen heter "Övr. <typ>" (kort) resp.
  # "Övriga <typ>" (tooltip), så att det framgår att valt program inte ingår.
  typer <- sort(unique(combined$programtyp))
  ovr_kort <- paste("Övr.", tolower(.programtyp_kort(sel_typ)))
  ovr_lang <- switch(sel_typ,
                     "Högskoleförberedande program" = "Övriga högskoleförb. program",
                     "Yrkesprogram"                 = "Övriga yrkesprogram",
                     "Introduktionsprogram"         = "Övriga introduktionsprogram",
                     paste("Övriga inom", tolower(sel_typ)))
  fyll_niv <- c(program_sel, ifelse(typer == sel_typ, ovr_kort, .programtyp_kort(typer)))
  d <- combined |>
    dplyr::left_join(tot, by = "ar") |>
    dplyr::group_by(ar, programtyp) |>
    dplyr::mutate(typ_andel = sum(antal) / dplyr::first(ar_total)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      andel = dplyr::if_else(ar_total > 0, antal / ar_total, 0),
      fyll  = factor(dplyr::case_when(markerad ~ program_sel,
                                      programtyp == sel_typ ~ ovr_kort,
                                      TRUE ~ .programtyp_kort(programtyp)),
                     levels = fyll_niv),
      tooltip = dplyr::case_when(
        markerad ~ paste0(tt(program_sel, ar, andel), "<br/>",
                          .programtyp_kort(programtyp), " totalt: ", pct(typ_andel)),
        programtyp == sel_typ ~ tt(ovr_lang, ar, andel),
        TRUE ~ tt(programtyp, ar, andel)))

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
    ggplot2::scale_x_continuous(breaks = .heltal_skala(), expand = ggplot2::expansion(mult = c(0, 0.02))) +
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
    ggplot2::scale_y_continuous(breaks = .heltal_skala()) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik, caption = .kalltext(kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top",
                   panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))

  .girafe_std(g, width_svg = 5, height_svg = 3.3, selection = FALSE)
}

# ---- Viktad andel (%) per program – liggande stapel -----------------------
# andel_kol är ett procenttal (0-100), vikt_kol är antal elever. Andelen
# aggregeras som viktat medel: sum(andel * vikt) / sum(vikt). Fungerar för
# alla viktade medelvärden (t.ex. meritvärde) - enhet styr suffixet.
skapa_diagram_bar_andel <- function(df, andel_kol, vikt_kol, metrik_label, ar = NULL,
                                    rubrik = NULL, underrubrik = NULL, kalla = NULL,
                                    enhet = " %", min_antal = MIN_ANTAL) {
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
      dold    = .dold(vikt, min_antal),            # för litet underlag
      varde   = dplyr::if_else(dold, 0, andel),
      program = forcats::fct_reorder(program, varde),
      tooltip = paste0("<b>", program, "</b><br/>", .metrik_ar(metrik_label, ar), ": ",
                       dplyr::if_else(dold, .dold_text(min_antal),
                                      paste0(scales::number(andel, accuracy = 0.1), enhet))),
      data_id = as.character(program))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = varde, y = program)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = data_id), fill = RD_PRIMARY, width = 0.74) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, enhet),
                                expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = metrik_label, y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, any(d$dold), min_antal)) +
    .rd_tema()
  if (any(d$dold)) g <- g + .dold_etikett(dplyr::filter(d, dold), "program", min_antal)

  .girafe_std(g, width_svg = 6.8, height_svg = 7.0, selection = TRUE)
}

# ---- Viktad andel (%) över tid – linje ------------------------------------
skapa_diagram_trend_andel <- function(df, andel_kol, vikt_kol, metrik_label, program_sel = NULL,
                                      rubrik = NULL, underrubrik = NULL, kalla = NULL,
                                      enhet = " %", min_antal = MIN_ANTAL) {
  if (!is.null(program_sel)) df <- dplyr::filter(df, program == program_sel)

  d <- df |>
    dplyr::group_by(ar) |>
    dplyr::summarise(
      vikt  = sum(.data[[vikt_kol]], na.rm = TRUE),
      andel = ifelse(vikt > 0,
                     sum(.data[[andel_kol]] * .data[[vikt_kol]], na.rm = TRUE) / vikt,
                     NA_real_),
      .groups = "drop")
  dolda <- any(.dold(d$vikt, min_antal) & !is.na(d$andel))
  # Dolda år blir NA - linjen får ett avbrott där.
  d <- d |>
    dplyr::mutate(andel = dplyr::if_else(.dold(vikt, min_antal), NA_real_, andel),
                  tooltip = paste0(metrik_label, " ", ar, ": ",
                                   scales::number(andel, accuracy = 0.1), enhet))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = andel)) +
    ggplot2::geom_line(color = RD_PRIMARY, linewidth = 0.9, na.rm = TRUE) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = ar), color = RD_PRIMARY, size = 2.4,
      na.rm = TRUE) +
    ggplot2::scale_x_continuous(breaks = .ar_breaks(d$ar)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, enhet)) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, dolda, min_antal)) +
    .rd_tema() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))

  .girafe_std(g, width_svg = 5, height_svg = 3.3, selection = FALSE)
}

# ---- Viktad andel per område och kön – grupperade liggande staplar --------
# En rad per område (Hela Dalarna + kommunerna) med en stapel per kön.
# df har geo_niva "lan"/"kommun", kommkod, kommun och program = kön
# ("Kvinna"/"Man"). Hela Dalarna ligger överst, kommunerna sorteras på
# andelen för båda könen tillsammans. Klick på en rad ger kommkod (länet
# = "20") i input$<id>_selected. vald_kommkod tonar ned övriga rader.
# kon_uppdelat = FALSE slår ihop könen till en stapel per område.
# summa = TRUE summerar andel_kol (antal) i stället för att väga ett medel.
# grupp_farger (namngiven färgvektor) delar i stället upp på andra grupper än
# kön: program ska då innehålla gruppnamnen (t.ex. "2 år", "3 år").
skapa_diagram_andel_omrade_kon <- function(df, andel_kol, vikt_kol, metrik_label, ar = NULL,
                                           vald_kommkod = NULL, kon_uppdelat = TRUE,
                                           rubrik = NULL, underrubrik = NULL, kalla = NULL,
                                           enhet = " %", summa = FALSE, grupp_farger = NULL,
                                           min_antal = MIN_ANTAL) {
  kon_etikett <- c("Kvinna" = "Kvinnor", "Man" = "Män")
  egna_grupper <- !is.null(grupp_farger)
  visa_grupp   <- egna_grupper || kon_uppdelat
  d <- df |>
    dplyr::filter(geo_niva %in% c("lan", "kommun"),
                  program %in% if (egna_grupper) names(grupp_farger) else names(kon_etikett)) |>
    dplyr::mutate(omrade = dplyr::if_else(geo_niva == "lan", "Hela Dalarna", kommun),
                  kon    = if (egna_grupper) program
                           else if (kon_uppdelat) unname(kon_etikett[program]) else "Totalt") |>
    dplyr::group_by(kommkod, omrade, kon) |>
    dplyr::summarise(
      vikt  = if (summa) 1 else sum(.data[[vikt_kol]], na.rm = TRUE),
      andel = if (summa) sum(.data[[andel_kol]], na.rm = TRUE)
              else ifelse(vikt > 0,
                          sum(.data[[andel_kol]] * .data[[vikt_kol]], na.rm = TRUE) / vikt,
                          NA_real_),
      .groups = "drop") |>
    dplyr::filter(!is.na(andel)) |>
    # Röjandekontroll: antal (summa) eller underlag (vikt) under min_antal.
    # Dolda antal får en grå platshållarstapel, dolda andelar ingen stapel.
    dplyr::mutate(dold  = if (summa) .dold(andel, min_antal) else .dold(vikt, min_antal),
                  varde = dplyr::if_else(dold, if (summa) ROJ_PLATSHALLARE else 0, andel))

  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 6.8, 7))

  # Antal: länets stapel skulle trycka ihop kommunerna, så länet visas som
  # total i underrubriken i stället.
  if (summa && any(d$omrade == "Hela Dalarna")) {
    lan_tot <- sum(d$andel[d$omrade == "Hela Dalarna"])
    underrubrik <- paste0(underrubrik, " \u00b7 Hela Dalarna: ",
                          scales::number(lan_tot, big.mark = " "))
    d <- dplyr::filter(d, omrade != "Hela Dalarna")
  }

  # Sortering: kommunerna efter värdet för båda könen (summa eller viktat
  # medel), länet överst.
  ordning <- d |>
    dplyr::filter(omrade != "Hela Dalarna") |>
    dplyr::group_by(omrade) |>
    # Synliga värden, så att dolda inte avslöjas av ordningen.
    dplyr::summarise(tot = if (summa) sum(varde[!dold])
                           else if (any(!dold)) sum((varde * vikt)[!dold]) / sum(vikt[!dold]) else 0,
                     .groups = "drop") |>
    dplyr::arrange(tot) |>
    dplyr::pull(omrade)

  vald_rad <- if (is.null(vald_kommkod) || vald_kommkod %in% c("_alla_", "20")) "20" else vald_kommkod

  d <- d |>
    dplyr::mutate(
      omrade  = factor(omrade, levels = c(ordning, "Hela Dalarna")),
      kon     = factor(kon, levels = if (egna_grupper) names(grupp_farger)
                                     else c("Kvinnor", "Män", "Totalt")),
      # Tona bara ned när det valda området har en egen stapel.
      alfa    = dplyr::if_else(kommkod == vald_rad | !vald_rad %in% kommkod, 1, 0.45),
      tooltip = paste0("<b>", omrade, "</b><br/>",
                       if (visa_grupp) paste0(kon, " \u00b7 ") else "",
                       .metrik_ar(metrik_label, ar), ": ",
                       dplyr::if_else(dold, .dold_text(min_antal),
                                      paste0(scales::number(andel, accuracy = if (summa) 1 else 0.1,
                                                            big.mark = " "), enhet))),
      data_id = kommkod,
      platshallare = summa & dold,
      fyll    = dplyr::if_else(platshallare, .dold_fyll_namn(min_antal), as.character(kon)),
      kant    = dplyr::if_else(platshallare, RD_TEXT_MUTED, NA_character_))

  dold_namn <- .dold_fyll_namn(min_antal)
  bas_farger <- if (egna_grupper) grupp_farger else c(KON_FARGER, "Totalt" = RD_PRIMARY)
  bas_breaks <- if (egna_grupper) names(grupp_farger) else if (visa_grupp) c("Kvinnor", "Män") else character(0)
  visa_dold  <- any(d$platshallare)
  # group = kon håller staplarnas plats i raden, fill kan bli grå för dolda.
  g <- ggplot2::ggplot(d, ggplot2::aes(x = varde, y = omrade, fill = fyll, group = kon)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = data_id, alpha = alfa, colour = kant),
      # reverse = TRUE: första gruppen överst i varje rad, samma ordning som legenden.
      # preserve = "single": lika breda staplar även där en grupp saknas.
      position = ggplot2::position_dodge2(preserve = "single", reverse = TRUE, padding = 0.1),
      width = 0.75,
      linetype = "22", linewidth = 0.35) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_manual(values = c(bas_farger, stats::setNames(RD_DOLD_FYLL, dold_namn)),
                               name = NULL,
                               breaks = c(bas_breaks, if (visa_dold) dold_namn),
                               guide = if (visa_grupp || visa_dold) "legend" else "none") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(scales::number(x, big.mark = " "), enhet),
                                expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = metrik_label, y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, any(d$dold), min_antal, platshallare = summa)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top")
  # Andelar: etikett när hela raden (alla grupper) är dold.
  helt_dolda <- d |> dplyr::group_by(omrade) |> dplyr::filter(all(dold) & !summa) |>
    dplyr::ungroup() |> dplyr::distinct(omrade)
  if (nrow(helt_dolda) > 0) g <- g + .dold_etikett(helt_dolda, "omrade", min_antal)

  .girafe_std(g, width_svg = 7, height_svg = 9, selection = TRUE)
}

# ---- Viktad andel över tid per kön – två linjer ---------------------------
# Som skapa_diagram_trend_andel(), men med en linje per kön (program =
# "Kvinna"/"Man"). summa = TRUE summerar andel_kol (antal) i stället.
# grupp_farger: en linje per egen grupp i stället för per kön (se
# skapa_diagram_andel_omrade_kon()).
skapa_diagram_trend_andel_kon <- function(df, andel_kol, vikt_kol, metrik_label,
                                          rubrik = NULL, underrubrik = NULL, kalla = NULL,
                                          enhet = " %", summa = FALSE, grupp_farger = NULL,
                                          min_antal = MIN_ANTAL) {
  kon_etikett <- c("Kvinna" = "Kvinnor", "Man" = "Män")
  farger <- if (is.null(grupp_farger)) KON_FARGER else grupp_farger
  d <- df |>
    dplyr::filter(program %in% if (is.null(grupp_farger)) names(kon_etikett) else names(grupp_farger)) |>
    dplyr::mutate(kon = if (is.null(grupp_farger)) unname(kon_etikett[program]) else program,
                  kon = factor(kon, levels = names(farger))) |>
    dplyr::group_by(ar, kon) |>
    dplyr::summarise(
      vikt  = if (summa) 1 else sum(.data[[vikt_kol]], na.rm = TRUE),
      andel = if (summa) sum(.data[[andel_kol]], na.rm = TRUE)
              else ifelse(vikt > 0,
                          sum(.data[[andel_kol]] * .data[[vikt_kol]], na.rm = TRUE) / vikt,
                          NA_real_),
      .groups = "drop") |>
    dplyr::filter(!is.na(andel))
  # Röjandekontroll: dolda punkter blir NA - linjen får ett avbrott där.
  dold  <- if (summa) .dold(d$andel, min_antal) else .dold(d$vikt, min_antal)
  dolda <- any(dold)
  d$andel[dold] <- NA_real_
  d <- d |>
    dplyr::mutate(tooltip = paste0(kon, " \u00b7 ", metrik_label, " ", ar, ": ",
                                   scales::number(andel, accuracy = if (summa) 1 else 0.1,
                                                  big.mark = " "), enhet))

  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 5, 3.3))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = andel, color = kon, group = kon)) +
    ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = paste(ar, kon)), size = 2.2, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = farger, name = NULL) +
    ggplot2::scale_x_continuous(breaks = .ar_breaks(d$ar)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, enhet)) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, dolda, min_antal)) +
    .rd_tema() +
    ggplot2::theme(legend.position    = "top",
                   panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))

  # Många grupper: legenden i två kolumner, så att den inte klipps i kanten.
  if (length(farger) > 3) g <- g + ggplot2::guides(color = ggplot2::guide_legend(ncol = 2, byrow = TRUE))

  .girafe_std(g, width_svg = 5, height_svg = 3.3, selection = FALSE)
}

# ---- Staplad fördelning per grupp (t.ex. examenstyp) -------------------------
# df: en rad per (program, kategori) med antal i metrik. farger: namngiven
# färgvektor i staplingsordning. Dolda antal (färre än min_antal) blir grå
# platshållarsegment. Tooltip visar antal och andel av gruppen.
skapa_diagram_bar_fordelning <- function(df, kategori_kol, metrik, farger, metrik_label,
                                         ar = NULL, rubrik = NULL, underrubrik = NULL,
                                         kalla = NULL, min_antal = MIN_ANTAL) {
  dold_namn <- .dold_fyll_namn(min_antal)
  d <- df |>
    dplyr::group_by(program, kategori = .data[[kategori_kol]]) |>
    dplyr::summarise(antal = sum(.data[[metrik]], na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(antal > 0, kategori %in% names(farger)) |>
    dplyr::group_by(program) |>
    dplyr::mutate(grupp_tot = sum(antal)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dold  = .dold(antal, min_antal),
      varde = dplyr::if_else(dold, ROJ_PLATSHALLARE, antal),
      fyll  = dplyr::if_else(dold, dold_namn, kategori),
      kant  = dplyr::if_else(dold, RD_TEXT_MUTED, NA_character_),
      kategori = factor(kategori, levels = rev(names(farger)))
    )
  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 6.8, 7))
  d <- d |>
    dplyr::group_by(program) |>
    dplyr::mutate(tot = sum(varde),       # synliga värden, så att dolda inte avslöjas
                  nagon_dold = any(dold)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      program = forcats::fct_reorder(program, tot),
      tooltip = paste0("<b>", program, "</b><br/>", kategori, " \u00b7 ",
                       .metrik_ar(metrik_label, ar), ": ",
                       # Andel bara när inget i stapeln är dolt - annars kunde det
                       # dolda värdet räknas ut ur totalen.
                       dplyr::case_when(
                         dold       ~ .dold_text(min_antal),
                         nagon_dold ~ as.character(antal),
                         TRUE       ~ paste0(antal, " (", scales::number(100 * antal / grupp_tot,
                                                                         accuracy = 0.1), " %)"))),
      data_id = as.character(program))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = varde, y = program, fill = fyll, group = kategori)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = data_id, colour = kant),
      width = 0.72, linetype = "22", linewidth = 0.35) +
    ggplot2::scale_fill_manual(values = c(farger, stats::setNames(RD_DOLD_FYLL, dold_namn)),
                               breaks = c(names(farger), if (any(d$dold)) dold_namn),
                               name = NULL) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(breaks = .heltal_skala(), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = metrik_label, y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, any(d$dold), min_antal, platshallare = TRUE)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top") +
    # Två kolumner, så att långa kategorinamn inte klipps i kanten.
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2, byrow = TRUE))

  .girafe_std(g, width_svg = 6.8, height_svg = max(4, dplyr::n_distinct(d$program) * 0.35 + 2),
              selection = TRUE)
}

# ---- Trend: valt område jämfört med Dalarna och riket ----------------------
# d: en rad per (ar, serie) med varde och underlag. serie i önskad ordning
# (faktor), t.ex. "Falun", "Dalarna", "Riket". Värden vars underlag är under
# min_antal blir avbrott i linjen.
# farger: egna färger per serie (namngiven vektor) i stället för standard.
skapa_diagram_trend_jmf <- function(d, metrik_label, rubrik = NULL, underrubrik = NULL,
                                    kalla = NULL, enhet = " %", min_antal = MIN_ANTAL,
                                    decimaler = 0.1, farger = NULL) {
  dolda <- any(.dold(d$underlag, min_antal) & !is.na(d$varde))
  d <- d |>
    dplyr::mutate(
      varde   = dplyr::if_else(.dold(underlag, min_antal), NA_real_, varde),
      tooltip = paste0(serie, " ", ar, ": ", scales::number(varde, accuracy = decimaler), enhet))
  if (all(is.na(d$varde))) return(.girafe_std(.tom_plot("Inga data"), 5, 3.3))

  serier <- levels(factor(d$serie))
  if (is.null(farger)) {
    farger <- stats::setNames(rep(RD_PRIMARY, length(serier)), serier)
    farger[serier == "Dalarna"] <- if (length(serier) > 2) RD_ACCENT else RD_PRIMARY
    farger[serier == "Riket"]   <- RD_TEXT_MUTED
  }
  linjer <- stats::setNames(ifelse(serier == "Riket", "dashed", "solid"), serier)

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = varde, color = serie, group = serie,
                                       linetype = serie)) +
    ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = paste(serie, ar)), size = 2.2, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = farger, name = NULL) +
    ggplot2::scale_linetype_manual(values = linjer, guide = "none") +
    ggplot2::scale_x_continuous(breaks = .ar_breaks(d$ar)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, enhet)) +
    ggplot2::labs(x = NULL, y = NULL, title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, dolda, min_antal)) +
    .rd_tema() +
    ggplot2::theme(legend.position    = "top",
                   panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))

  .girafe_std(g, width_svg = 5, height_svg = 3.3, selection = FALSE)
}

# ============================================================
#  Genomströmning: trend med Dalarna + Riket som jämförelse
# ============================================================

# Trendlinje för genomströmning. df_dalarna är redan filtrerat till Dalarna
# (kommunrader summerade), df_riket är rikets rad (geo_niva == "riket").
# program_sel = NULL -> alla program sammanslagen (viktat medel med antal
# elever är okänt -> enkelt medel per år, ange i underrubrik).
# Röjandekontroll när datan har ett underlag (kolumnen avslutade, YH);
# gymnasiets genomströmning (Skolverket) saknar den och visas som den är.
# Medel av andelar för flera kommuner: viktat med elevantal (kolumnen vikt,
# se hamta_elevantal_vikt()) när vikt finns för alla, annars ovägt.
# Saknade andelar (prickade av Skolverket) räknas inte med.
.viktat_medel_andel <- function(andel, vikt = NULL) {
  ok <- !is.na(andel)
  if (!any(ok)) return(NA_real_)
  if (is.null(vikt) || any(is.na(vikt[ok])) || sum(vikt[ok]) <= 0) return(mean(andel[ok]))
  sum(andel[ok] * vikt[ok]) / sum(vikt[ok])
}
.GS_SAKNAS <- "uppgift saknas"

skapa_diagram_genomstromning_trend <- function(df_dalarna, df_riket,
                                               program_sel = NULL,
                                               rubrik = NULL,
                                               underrubrik = NULL,
                                               kalla = NULL,
                                               min_antal = MIN_ANTAL) {
  if (!"avslutade" %in% names(df_dalarna)) df_dalarna$avslutade <- NA_real_
  har_vikt <- "vikt" %in% names(df_dalarna)
  # Dalarna: en andel per år - viktad över kommunerna om flera (se ovan).
  d_dal <- df_dalarna |>
    dplyr::group_by(ar) |>
    dplyr::summarise(andel = .viktat_medel_andel(andel, if (har_vikt) vikt),
                     underlag = sum(avslutade, na.rm = TRUE), .groups = "drop")
  dolda <- any(.dold(d_dal$underlag, min_antal))
  d_dal <- d_dal |>
    dplyr::mutate(andel = dplyr::if_else(.dold(underlag, min_antal), NA_real_, andel),
                  serie = "Dalarna",
                  tooltip = paste0("Dalarna ", ar, ": ",
                                   scales::number(andel, accuracy = 0.1), " %"))

  d_rik <- df_riket |>
    dplyr::rename(andel = andel_riket) |>
    dplyr::mutate(serie = "Riket",
                  tooltip = paste0("Riket ", ar, ": ",
                                   scales::number(andel, accuracy = 0.1), " %"))

  d <- dplyr::bind_rows(d_dal, d_rik)

  if (all(is.na(d$andel))) return(.girafe_std(.tom_plot("Inga data"), 5, 3))

  serie_farger <- c("Dalarna" = RD_PRIMARY, "Riket" = RD_TEXT_MUTED)

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = andel,
                                       color = serie, group = serie)) +
    ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = paste(serie, ar)), size = 2.4, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = serie_farger, name = NULL) +
    ggplot2::scale_x_continuous(breaks = .ar_breaks(d$ar)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, " %"),
                                limits = c(0, NA)) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, dolda, min_antal)) +
    .rd_tema() +
    ggplot2::theme(legend.position  = "top",
                   panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))

  .girafe_std(g, width_svg = 5, height_svg = 3.3, selection = FALSE)
}

# Stapel per program – genomströmningsandel för valt år i Dalarna.
skapa_diagram_genomstromning_bar <- function(df, ar = NULL,
                                             rubrik = NULL,
                                             underrubrik = NULL,
                                             kalla = NULL,
                                             min_antal = MIN_ANTAL) {
  if (!"avslutade" %in% names(df)) df$avslutade <- NA_real_
  har_vikt <- "vikt" %in% names(df)
  # Program utan andel (prickade av Skolverket) behålls med etiketten
  # "uppgift saknas", så att de inte försvinner ur diagrammet.
  d <- df |>
    dplyr::group_by(program) |>
    dplyr::summarise(delvis = any(is.na(andel)) && !all(is.na(andel)),
                     andel = .viktat_medel_andel(andel, if (har_vikt) vikt),
                     underlag = sum(avslutade, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      saknas  = is.na(andel),
      dold    = !saknas & .dold(underlag, min_antal),
      varde   = dplyr::if_else(dold | saknas, 0, andel),
      program = forcats::fct_reorder(program, varde),
      tooltip = paste0("<b>", program, "</b><br/>",
                       .metrik_ar("Andel med examen", ar), ": ",
                       dplyr::case_when(
                         saknas ~ paste0(.GS_SAKNAS, " (för få elever)"),
                         dold   ~ .dold_text(min_antal),
                         TRUE   ~ paste0(scales::number(andel, accuracy = 0.1), " %")),
                       dplyr::if_else(delvis, "<br/>Uppgift saknas för någon av kommunerna.", "")),
      data_id = as.character(program))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = varde, y = program)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = data_id),
      fill = RD_PRIMARY, width = 0.74) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(x, " %"),
      expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = "Andel med examen (%)", y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, any(d$dold), min_antal)) +
    .rd_tema()
  if (any(d$dold)) g <- g + .dold_etikett(dplyr::filter(d, dold), "program", min_antal)
  if (any(d$saknas))
    g <- g + ggplot2::geom_text(data = dplyr::filter(d, saknas), ggplot2::aes(x = 0, y = program),
                                label = .GS_SAKNAS, hjust = -0.05, size = 2.8,
                                colour = RD_TEXT_MUTED, fontface = "italic", inherit.aes = FALSE)

  .girafe_std(g, width_svg = 6.8, height_svg = 7.0, selection = TRUE)
}

# ============================================================
#  YH-genomströmning med ofullständiga startårgångar
# ============================================================
# Status (yh_arsstatus()): "komplett" (högst YH_PAGAENDE_VARNING % pågår),
# "varning" (upp till YH_PAGAENDE_DOLJ %), "ofullstandig" (mer). Ofullständiga
# visas bara med visa_ofullstandiga = TRUE, och då markerade.
.YH_STATUS_TEXT <- c(komplett = "Färdig årgång", varning = "Årgången inte färdig",
                     ofullstandig = "Ofullständig årgång")
.yh_pagaende_text <- function(p) paste0(scales::number(p, accuracy = 1), " % pågår")

# Stapel per program (område eller utbildning). d: en rad per program med
# examen, avslutade, paborjade, pagaende_kvar.
# per_kon = TRUE: en stapel per kön (kolumnen kon), ej färdiga årgångar ljusare.
skapa_diagram_yh_genomstromning_bar <- function(d, ar = NULL, visa_ofullstandiga = FALSE,
                                                riket_andel = NULL, rubrik = NULL,
                                                underrubrik = NULL, kalla = NULL,
                                                per_kon = FALSE, min_antal = MIN_ANTAL) {
  ar_valt <- ar   # kolumnen ar nedan är bara en hjälpkolumn för summeringen
  d <- d |>
    dplyr::mutate(ar = 0L) |>
    yh_genomstromning_summera(grupp = c("program", if (per_kon) "kon")) |>
    dplyr::filter(!is.na(andel) | status == "ofullstandig")
  if (per_kon) d <- dplyr::filter(d, kon %in% names(.YH_KON_ETIKETT))
  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 6.8, 7))
  fyll <- c(komplett = RD_PRIMARY, varning = rd_farg("rd-blue-light", "#8edded"))
  d <- d |>
    dplyr::mutate(
      dold   = .dold(avslutade, min_antal),
      gomd   = status == "ofullstandig" & !visa_ofullstandiga,
      varde  = dplyr::if_else(dold | gomd | is.na(andel), 0, andel),
      grupp  = dplyr::if_else(status == "komplett", "komplett", "varning"),
      kon_lab = if (per_kon) factor(unname(.YH_KON_ETIKETT[kon]), levels = names(KON_FARGER)) else NA,
      program = forcats::fct_reorder(program, varde, .fun = sum),
      tooltip = paste0("<b>", program, "</b><br/>", if (per_kon) paste0(kon_lab, " \u00b7 ") else "",
                       .metrik_ar("Andel med examen", ar_valt), ": ",
                       dplyr::case_when(
                         gomd ~ "visas inte",
                         dold ~ .dold_text(min_antal),
                         TRUE ~ paste0(scales::number(andel, accuracy = 0.1), " %")),
                       dplyr::if_else(status == "komplett", "",
                                      paste0("<br/>", .YH_STATUS_TEXT[status], ": ",
                                             .yh_pagaende_text(andel_pagaende)))),
      data_id = as.character(program))
  legend <- if (any(d$grupp == "varning" & !d$gomd))
    c(komplett = "Färdig årgång", varning = paste0("Ej färdig (över ", YH_PAGAENDE_VARNING, " % pågår)"))

  g <- if (per_kon) {
    ggplot2::ggplot(d, ggplot2::aes(x = varde, y = program)) +
      ggiraph::geom_col_interactive(
        ggplot2::aes(tooltip = tooltip, data_id = data_id, fill = kon_lab, group = kon_lab,
                     alpha = dplyr::if_else(grupp == "komplett", 1, 0.45)),
        position = ggplot2::position_dodge2(preserve = "single", reverse = TRUE, padding = 0.1),
        width = 0.74) +
      ggplot2::scale_alpha_identity() +
      ggplot2::scale_fill_manual(values = KON_FARGER, name = NULL)
  } else {
    ggplot2::ggplot(d, ggplot2::aes(x = varde, y = program)) +
      ggiraph::geom_col_interactive(
        ggplot2::aes(tooltip = tooltip, data_id = data_id, fill = grupp), width = 0.74) +
      ggplot2::scale_fill_manual(values = fyll, labels = legend, breaks = names(legend),
                                 name = NULL, guide = if (is.null(legend)) "none" else "legend")
  }
  if (per_kon && !is.null(legend))
    kalla <- paste0(kalla, "\nLjusare stapel: årgången inte färdig (över ", YH_PAGAENDE_VARNING, " % pågår).")
  g <- g +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, " %"),
                                expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = "Andel med examen (%)", y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, any(d$dold), min_antal)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top")
  # Etiketter bara där hela raden (alla kön) är dold resp. ofullständig.
  dolda_rad <- d |> dplyr::group_by(program) |> dplyr::filter(all(dold & !gomd)) |>
    dplyr::ungroup() |> dplyr::distinct(program)
  if (nrow(dolda_rad) > 0) g <- g + .dold_etikett(dolda_rad, "program", min_antal)
  gomda_rad <- d |> dplyr::group_by(program) |> dplyr::filter(all(gomd)) |>
    dplyr::ungroup() |> dplyr::distinct(program)
  if (nrow(gomda_rad) > 0)
    g <- g + ggplot2::geom_text(data = gomda_rad, ggplot2::aes(x = 0, y = program),
                                label = "ofullständig årgång", hjust = -0.05, size = 2.8,
                                colour = RD_TEXT_MUTED, fontface = "italic", inherit.aes = FALSE)
  if (!is.null(riket_andel) && !is.na(riket_andel))
    g <- g +
      ggplot2::geom_vline(xintercept = riket_andel, linetype = "dashed",
                          color = RD_TEXT_MUTED, linewidth = 0.7) +
      ggplot2::annotate("text", x = riket_andel, y = Inf,
                        label = paste0("Riket ", scales::number(riket_andel, accuracy = 0.1), " %"),
                        hjust = 1.08, vjust = 1.5, size = 2.8, color = RD_TEXT_MUTED)

  .girafe_std(g, width_svg = 6.8, height_svg = max(4, nrow(d) * 0.3 + 2), selection = TRUE)
}

# Linje per serie (urvalet och riket). d: en rad per (serie, ar) med andel,
# avslutade, andel_pagaende, status (se yh_genomstromning_summera()).
# Färdiga år ritas heldragna; ej färdiga streckade, med ihåliga punkter och
# skuggning (efter urvalets status). Ofullständiga år tas bara med när
# visa_ofullstandiga = TRUE.
# farger: namngivna färger per serie (standard: urvalet blått, riket grått).
# status_ar: (ar, status) som styr skuggningen, standard första seriens.
skapa_diagram_yh_genomstromning_trend <- function(d, visa_ofullstandiga = FALSE,
                                                  rubrik = NULL, underrubrik = NULL,
                                                  kalla = NULL, farger = NULL, status_ar = NULL,
                                                  min_antal = MIN_ANTAL) {
  dolda <- any(.dold(d$avslutade, min_antal) & d$serie != "Riket")
  d <- d |>
    dplyr::filter(!is.na(status), visa_ofullstandiga | status != "ofullstandig") |>
    dplyr::mutate(andel = dplyr::if_else(.dold(avslutade, min_antal) & serie != "Riket",
                                         NA_real_, andel)) |>
    dplyr::arrange(serie, ar) |>
    dplyr::group_by(serie) |>
    dplyr::mutate(
      fardig = status == "komplett",
      # Streckad del: ej färdiga år plus det sista färdiga, så att linjen hänger ihop.
      sist_fardig = ar == suppressWarnings(max(ar[fardig])),
      andel_hel   = dplyr::if_else(fardig, andel, NA_real_),
      andel_streck = dplyr::if_else(!fardig | sist_fardig, andel, NA_real_)) |>
    dplyr::ungroup() |>
    dplyr::mutate(tooltip = paste0(serie, " ", ar, ": ", scales::number(andel, accuracy = 0.1), " %",
                                   dplyr::if_else(fardig, "", paste0("<br/>", .YH_STATUS_TEXT[status], ", ",
                                                                     .yh_pagaende_text(andel_pagaende)))))
  if (all(is.na(d$andel))) return(.girafe_std(.tom_plot("Inga data"), 5, 3.3))

  serier <- unique(d$serie)
  if (is.null(farger)) farger <- stats::setNames(c(RD_PRIMARY, RD_TEXT_MUTED)[seq_along(serier)], serier)
  # Skuggning efter urvalets status (status_ar), annars första seriens.
  skugga <- if (is.null(status_ar)) dplyr::filter(d, serie == serier[1], !fardig)
            else dplyr::filter(status_ar, status != "komplett", ar %in% d$ar)

  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, colour = serie, group = serie))
  if (nrow(skugga) > 0)
    g <- g + ggplot2::geom_rect(data = skugga, ggplot2::aes(xmin = ar - 0.5, xmax = ar + 0.5),
                                ymin = -Inf, ymax = Inf, fill = "#eef2f5", colour = NA,
                                inherit.aes = FALSE)
  g <- g +
    ggplot2::geom_line(ggplot2::aes(y = andel_hel), linewidth = 0.9, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = andel_streck), linewidth = 0.9, linetype = "22", na.rm = TRUE) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = andel, tooltip = tooltip, data_id = paste(serie, ar),
                   shape = dplyr::if_else(fardig, "fardig", "ej")),
      size = 2.4, fill = "white", stroke = 1, na.rm = TRUE) +
    ggplot2::scale_shape_manual(values = c(fardig = 16, ej = 21), guide = "none") +
    ggplot2::scale_colour_manual(values = farger, name = NULL) +
    ggplot2::scale_x_continuous(breaks = .ar_breaks(d$ar)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, " %"), limits = c(0, NA)) +
    ggplot2::labs(x = "Startår", y = NULL, title = rubrik, subtitle = underrubrik,
                  caption = paste(c(.kalltext(kalla, dolda, min_antal),
                                    if (nrow(skugga) > 0)
                                      "Skuggat och streckat: årgången är inte färdig."),
                                  collapse = "\n")) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top",
                   panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))

  .girafe_std(g, width_svg = 5, height_svg = 3.3, selection = FALSE)
}

# YH-etablering över examensår: urvalet och riket, ett uppföljningsår.
# En lodrät streckad linje markerar var definitionen ändrades (första
# examensåret med uppföljningsår YH_DEFINITIONSBYTE).
# per_kon = TRUE: en linje per kön (kolumnen kon) i stället för urval + riket.
skapa_diagram_yh_etablering_trend <- function(df_urval, df_riket, metrik, metrik_label,
                                              antal_ar_val, serie_namn = "Dalarna",
                                              rubrik = NULL, underrubrik = NULL,
                                              kalla = NULL, per_kon = FALSE, min_antal = MIN_ANTAL) {
  if (per_kon) df_urval <- dplyr::filter(df_urval, kon %in% names(.YH_KON_ETIKETT))
  d_u <- df_urval |>
    dplyr::filter(antal_ar == antal_ar_val) |>
    dplyr::mutate(serie = if (per_kon) unname(.YH_KON_ETIKETT[kon]) else serie_namn) |>
    dplyr::group_by(ar, serie) |>
    dplyr::summarise(status_sum = sum(.data[[metrik]], na.rm = TRUE),
                     antal_sum  = sum(antal, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(andel = dplyr::if_else(antal_sum > 0 & !.dold(antal_sum, min_antal),
                                         status_sum / antal_sum, NA_real_))
  dolda <- any(.dold(d_u$antal_sum, min_antal))
  d_r <- if (per_kon) NULL else df_riket |>
    dplyr::filter(antal_ar == antal_ar_val) |>
    dplyr::transmute(ar, andel = andel_riket, serie = "Riket")
  d <- dplyr::bind_rows(d_u, d_r) |>
    dplyr::mutate(tooltip = paste0(serie, ", examensår ", ar, ": ", scales::percent(andel, accuracy = 0.1)))
  if (all(is.na(d$andel))) return(.girafe_std(.tom_plot("Inga data"), 5, 3.3))

  byte <- yh_forsta_nya_examensar(antal_ar_val)
  farger <- if (per_kon) KON_FARGER else stats::setNames(c(RD_PRIMARY, RD_TEXT_MUTED), c(serie_namn, "Riket"))
  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = andel, colour = serie, group = serie,
                                       linetype = serie)) +
    ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
    ggiraph::geom_point_interactive(ggplot2::aes(tooltip = tooltip, data_id = paste(serie, ar)),
                                    size = 2.2, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = farger, name = NULL) +
    ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", if (per_kon) "solid" else "dashed"),
                                                            names(farger)), guide = "none") +
    ggplot2::scale_x_continuous(breaks = .ar_breaks(d$ar)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
    ggplot2::labs(x = "Examensår", y = NULL, title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, dolda, min_antal)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top",
                   panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))
  if (byte > min(d$ar) && byte <= max(d$ar))
    g <- g +
      ggplot2::geom_vline(xintercept = byte - 0.5, linetype = "dotted", colour = RD_TEXT, linewidth = 0.5) +
      ggplot2::annotate("text", x = byte - 0.4, y = -Inf, label = "Ny definition", hjust = 0,
                        vjust = -0.6, size = 2.6, colour = RD_TEXT)

  .girafe_std(g, width_svg = 5, height_svg = 3.3, selection = FALSE)
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
                                         kalla = NULL, min_antal = MIN_ANTAL) {

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
      dplyr::filter(!is.na(andel)) |>
      dplyr::mutate(dold  = .dold(antal_sum, min_antal),
                    varde = dplyr::if_else(dold, 0, andel))

    if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 6.8, 7))

    # Sortera program på medel över alla år (synliga värden)
    namn_order <- d |>
      dplyr::group_by(namn) |>
      dplyr::summarise(m = mean(varde, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(m) |>
      dplyr::pull(namn)

    d <- d |>
      dplyr::mutate(
        namn       = factor(namn, levels = namn_order),
        ar_etikett = factor(ar_etikett, levels = c("1 år", "3 år", "5 år", "7 år")),
        tooltip    = paste0("<b>", namn, "</b><br/>", ar_etikett, ": ",
                            dplyr::if_else(dold, .dold_text(min_antal),
                                           scales::percent(andel, accuracy = 0.1))))

    g <- ggplot2::ggplot(d, ggplot2::aes(x = varde, y = namn, fill = ar_etikett)) +
      ggiraph::geom_col_interactive(
        ggplot2::aes(tooltip = tooltip, data_id = paste(namn, ar_etikett)),
        position = ggplot2::position_dodge2(preserve = "single", padding = 0.1), width = 0.72) +
      ggplot2::scale_fill_manual(values = ANTAL_AR_FARGER, name = "År efter examen") +
      ggplot2::scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = ggplot2::expansion(mult = c(0, 0.04)), limits = c(0, 1)) +
      ggplot2::labs(x = metrik_label, y = NULL,
                    title = rubrik, subtitle = underrubrik,
                    caption = .kalltext(kalla, any(d$dold), min_antal)) +
      .rd_tema() +
      ggplot2::theme(legend.position = "top")
    helt_dolda <- d |> dplyr::group_by(namn) |> dplyr::filter(all(dold)) |>
      dplyr::ungroup() |> dplyr::distinct(namn)
    if (nrow(helt_dolda) > 0) g <- g + .dold_etikett(helt_dolda, "namn", min_antal)

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
    dplyr::filter(!is.na(andel)) |>
    dplyr::mutate(dold  = .dold(antal_sum, min_antal),
                  varde = dplyr::if_else(dold, 0, andel))

  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 6.8, 7))

  # Sortera på andel, men håll inriktningar grupperade under sitt program.
  # program == namn när vi visar på programnivå. Bara synliga värden räknas.
  prog_order <- d |>
    dplyr::group_by(program) |>
    dplyr::summarise(prog_andel = if (any(!dold)) sum(status_sum[!dold]) / sum(antal_sum[!dold]) else 0,
                     .groups = "drop") |>
    dplyr::arrange(prog_andel) |>
    dplyr::pull(program)

  d <- d |>
    dplyr::mutate(
      program = factor(program, levels = prog_order),
      namn    = forcats::fct_reorder2(namn, program, varde,
                                      .fun = function(p, a) as.integer(p) + a * 0.01)
    ) |>
    dplyr::mutate(
      tooltip = paste0("<b>", namn, "</b><br/>",
                       antal_ar_val, " år efter examen: ",
                       dplyr::if_else(dold, .dold_text(min_antal),
                                      scales::percent(andel, accuracy = 0.1)))
    )

  g <- ggplot2::ggplot(d, ggplot2::aes(x = varde, y = namn)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = as.character(namn)),
      fill = RD_PRIMARY, width = 0.74) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.04)),
      limits = c(0, 1)) +
    ggplot2::labs(x = metrik_label, y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = .kalltext(kalla, any(d$dold), min_antal)) +
    .rd_tema()
  if (any(d$dold)) g <- g + .dold_etikett(dplyr::filter(d, dold), "namn", min_antal)

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
                                           kalla = NULL, min_antal = MIN_ANTAL) {
  d_dal <- df_dalarna |>
    dplyr::filter(antal_ar == antal_ar_val) |>
    dplyr::group_by(ar) |>
    dplyr::summarise(status_sum = sum(.data[[metrik]], na.rm = TRUE),
                     antal_sum  = sum(antal, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(
      andel   = dplyr::if_else(antal_sum > 0 & !.dold(antal_sum, min_antal),
                               status_sum / antal_sum, NA_real_),
      serie   = "Dalarna",
      tooltip = paste0("Dalarna ", ar, ": ", scales::percent(andel, accuracy = 0.1)))

  d_rik <- df_riket |>
    dplyr::filter(antal_ar == antal_ar_val) |>
    dplyr::rename(andel = andel_riket) |>
    dplyr::mutate(
      serie   = "Riket",
      tooltip = paste0("Riket ", ar, ": ", scales::percent(andel, accuracy = 0.1)))

  dolda <- any(.dold(d_dal$antal_sum, min_antal))
  d <- dplyr::bind_rows(d_dal, d_rik)
  if (all(is.na(d$andel))) return(.girafe_std(.tom_plot("Inga data"), 5, 3.3))

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
    ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = paste(serie, ar)), size = 2.4, na.rm = TRUE) +
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
                  caption = .kalltext(kalla, dolda, min_antal)) +
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
