# ============================================================
#  func_diagram_prognos.R
#  Diagram för fliken Prognos 2040 (RegTop2025, se func_data_regtop.R).
#  Prognoser markeras alltid: streckade linjer och skuggad prognosperiod.
# ============================================================

.REGTOP_PROGNOS_FYLL <- "#eef2f5"
.TILLGANG_FARGER <- c("Tillgång" = RD_PRIMARY, "Efterfrågan" = "#d9653b")
.fmt_antal <- function(x) scales::number(x, accuracy = 1, big.mark = " ")
.fmt_proc  <- function(x) paste0(scales::number(x, accuracy = 0.1, decimal.mark = ","), " %")

# ---- Bedömning per utbildningsgrupp -------------------------------------------
# d: en rad per utbildningsgrupp (bedomning_2040) med har_bedomning. Staplar
# med balanstalet, färgade efter bedömningen. Klick ger utbildningsgrupp_kod.
skapa_diagram_bedomning <- function(d, rubrik = NULL, underrubrik = NULL, kalla = KALLA_REGTOP) {
  d <- dplyr::filter(d, har_bedomning, !is.na(balanstal_procent))
  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Ingen bedömning för urvalet"), 6.8, 4))
  d <- d |>
    dplyr::mutate(
      grupp = forcats::fct_reorder(utbildningsgrupp, -as.integer(bedomning_2040) * 1e6 + balanstal_procent),
      tooltip = paste0(
        "<b>", utbildningsgrupp, "</b><br/>", bedomning_2040, " 2040 (balanstal ",
        .fmt_proc(balanstal_procent), ")",
        "<br/>Tillgång: ", .fmt_antal(tillgang_2023), " (2023) → ", .fmt_antal(tillgang_2040), " (2040)",
        "<br/>Efterfrågan: ", .fmt_antal(efterfragan_2023), " (2023) → ", .fmt_antal(efterfragan_2040), " (2040)",
        dplyr::if_else(hansyn_lage_start %in% "Ja" & !is.na(bedomning_start),
                       paste0("<br/>", bedomning_start), "")))
  g <- ggplot2::ggplot(d, ggplot2::aes(x = balanstal_procent, y = grupp, fill = bedomning_2040)) +
    ggplot2::geom_vline(xintercept = 0, colour = RD_TEXT_MUTED, linewidth = 0.4) +
    ggiraph::geom_col_interactive(ggplot2::aes(tooltip = tooltip, data_id = utbildningsgrupp_kod),
                                  width = 0.75) +
    ggplot2::scale_fill_manual(values = BEDOMNING_FARGER, name = NULL,
                               breaks = intersect(REGTOP_BEDOMNINGAR, as.character(d$bedomning_2040))) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, " %")) +
    ggplot2::labs(x = "Balanstal 2040: (tillgång - efterfrågan) / efterfrågan", y = NULL,
                  title = rubrik, subtitle = underrubrik,
                  caption = paste0("Källa: ", kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE))
  .girafe_std(g, width_svg = 7.5, height_svg = max(4, nrow(d) * 0.24 + 2.2), selection = TRUE)
}

# Antal utbildningsgrupper per bedömning: valt län jämfört med riket.
skapa_diagram_bedomning_antal <- function(d, region_namn, rubrik = NULL, kalla = KALLA_REGTOP) {
  d <- d |>
    dplyr::filter(har_bedomning) |>
    dplyr::mutate(serie = dplyr::if_else(regionkod == "00", "Riket", region_namn)) |>
    dplyr::count(serie, bedomning_2040, .drop = FALSE) |>
    dplyr::mutate(serie = factor(serie, levels = unique(c(region_namn, "Riket"))),
                  tooltip = paste0(serie, " · ", bedomning_2040, ": ", n, " utbildningsgrupper"))
  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 5, 3.3))
  farger <- stats::setNames(c(RD_PRIMARY, RD_TEXT_MUTED), c(region_namn, "Riket"))
  g <- ggplot2::ggplot(d, ggplot2::aes(x = bedomning_2040, y = n, fill = serie)) +
    ggiraph::geom_col_interactive(ggplot2::aes(tooltip = tooltip, data_id = paste(serie, bedomning_2040)),
                                  position = ggplot2::position_dodge2(preserve = "single", padding = 0.1),
                                  width = 0.75) +
    ggplot2::scale_fill_manual(values = farger[unique(c(region_namn, "Riket"))], name = NULL) +
    ggplot2::scale_x_discrete(drop = FALSE, labels = function(x) gsub(" ", "\n", x)) +
    ggplot2::scale_y_continuous(breaks = function(l) seq(0, ceiling(l[2]), by = max(1, ceiling(l[2] / 5)))) +
    ggplot2::labs(x = NULL, y = "Antal utbildningsgrupper", title = rubrik,
                  caption = paste0("Källa: ", kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top", axis.text.x = ggplot2::element_text(size = 7))
  .girafe_std(g, width_svg = 5, height_svg = 3.4, selection = FALSE)
}

# ---- Tillgång och efterfrågan över tid ---------------------------------------
# d: tillgang_efterfragan för en region och grupp (ar, variabel, typ, varde).
# Historik heldragen, prognos streckad (från sista historiska året).
skapa_diagram_tillgang_efterfragan <- function(d, rubrik = NULL, underrubrik = NULL,
                                               kalla = KALLA_REGTOP) {
  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 6.5, 3.6))
  d <- d |>
    dplyr::mutate(ar = as.integer(ar),
                  serie = factor(dplyr::if_else(variabel == "tillgang", "Tillgång", "Efterfrågan"),
                                 levels = names(.TILLGANG_FARGER)))
  sista_hist <- max(d$ar[d$typ == "historik"])
  # Prognoslinjen börjar i sista historiska punkten så att linjerna hänger ihop.
  prog <- dplyr::bind_rows(
    dplyr::filter(d, typ == "historik", ar == sista_hist) |> dplyr::mutate(typ = "prognos_start"),
    dplyr::filter(d, typ == "prognos"))
  d <- d |> dplyr::mutate(tooltip = paste0(serie, " ", ar, if_else_prognos(typ), ": ", .fmt_antal(varde)))
  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar, y = varde, colour = serie)) +
    ggplot2::annotate("rect", xmin = sista_hist + 0.5, xmax = Inf, ymin = -Inf, ymax = Inf,
                      fill = .REGTOP_PROGNOS_FYLL) +
    ggplot2::annotate("text", x = sista_hist + 1, y = Inf, label = "Prognos", hjust = 0, vjust = 1.5,
                      size = 2.8, colour = RD_TEXT_MUTED) +
    ggplot2::geom_line(data = dplyr::filter(d, typ == "historik"), linewidth = 0.9) +
    ggplot2::geom_line(data = prog, linewidth = 0.9, linetype = "22") +
    ggiraph::geom_point_interactive(ggplot2::aes(tooltip = tooltip, data_id = paste(serie, ar),
                                                 shape = typ), size = 2, fill = "white", stroke = 1) +
    ggplot2::scale_shape_manual(values = c(historik = 16, prognos = 21), guide = "none") +
    ggplot2::scale_colour_manual(values = .TILLGANG_FARGER, name = NULL) +
    ggplot2::scale_x_continuous(breaks = seq(2010, 2040, by = 5)) +
    ggplot2::scale_y_continuous(breaks = .heltal_skala(), labels = .fmt_antal, limits = c(0, NA)) +
    ggplot2::labs(x = NULL, y = "Antal personer", title = rubrik, subtitle = underrubrik,
                  caption = paste0("Källa: ", kalla, "\nStreckat: prognos. Efterfrågan prognostiseras för 2030, 2035 och 2040.")) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top",
                   panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))
  .girafe_std(g, width_svg = 6.5, height_svg = 3.6, selection = FALSE)
}
if_else_prognos <- function(typ) ifelse(typ == "prognos", " (prognos)", "")

# ---- Faktorer som förändrar tillgång eller efterfrågan --------------------------
# d: faktor_text, varde. Liggande staplar FRÅN NOLL (inte vattenfall/kumulativt
# - svårt att läsa av hur mycket varje stapel är värd utan att hovra), färgade
# efter riktning. Värdet skrivs också ut direkt vid stapeln.
# fmt: .fmt_antal (tillgång, antal personer) eller .fmt_proc (efterfrågan, procent).
skapa_diagram_faktorer <- function(d, fmt, x_label, rubrik = NULL, underrubrik = NULL,
                                   kalla = KALLA_REGTOP) {
  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 5, 3.2))
  d <- d |> dplyr::mutate(
    faktor_text = forcats::fct_rev(factor(faktor_text, levels = unique(faktor_text))),
    etikett = paste0(dplyr::if_else(varde >= 0, "+", ""), fmt(varde)),
    tooltip = paste0("<b>", faktor_text, "</b><br/>", etikett),
    riktning = dplyr::if_else(varde >= 0, "okning", "minskning"))
  g <- ggplot2::ggplot(d, ggplot2::aes(x = varde, y = faktor_text, fill = riktning)) +
    ggplot2::geom_vline(xintercept = 0, colour = RD_TEXT_MUTED, linewidth = 0.4) +
    ggiraph::geom_col_interactive(ggplot2::aes(tooltip = tooltip, data_id = faktor_text), width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = etikett, hjust = dplyr::if_else(varde >= 0, -0.15, 1.15)),
                       size = 3, colour = RD_TEXT) +
    ggplot2::scale_fill_manual(values = c(okning = "#459079", minskning = "#d9653b"), guide = "none") +
    ggplot2::scale_x_continuous(labels = fmt, expand = ggplot2::expansion(mult = 0.14)) +
    ggplot2::labs(x = x_label, y = NULL, title = rubrik, subtitle = underrubrik,
                  caption = paste0("Källa: ", kalla, "\nGrönt ökar och orange minskar.")) +
    .rd_tema()
  .girafe_std(g, width_svg = 5, height_svg = max(2.6, nrow(d) * 0.4 + 1), selection = FALSE)
}

# ---- Arbetskraften per kön 2023 och 2040 ------------------------------------------
skapa_diagram_arbetskraft_kon <- function(d, rubrik = NULL, underrubrik = NULL, kalla = KALLA_REGTOP) {
  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 5, 3.2))
  d <- d |> dplyr::mutate(
    ar_text = paste0(ar, if_else_prognos(typ)),
    ar_text = factor(ar_text, levels = unique(ar_text[order(ar)])),
    kon = factor(kon, levels = names(KON_FARGER)),
    tooltip = paste0(kon, " ", ar_text, ": ", .fmt_antal(varde)))
  g <- ggplot2::ggplot(d, ggplot2::aes(x = ar_text, y = varde, fill = kon)) +
    ggiraph::geom_col_interactive(ggplot2::aes(tooltip = tooltip, data_id = paste(kon, ar)),
                                  position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::scale_fill_manual(values = KON_FARGER, name = NULL) +
    ggplot2::scale_y_continuous(breaks = .heltal_skala(), labels = .fmt_antal) +
    ggplot2::labs(x = NULL, y = "Antal i arbetskraften", title = rubrik, subtitle = underrubrik,
                  caption = paste0("Källa: ", kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top")
  .girafe_std(g, width_svg = 5, height_svg = 3.2, selection = FALSE)
}

# ---- Samma utbildningsgrupp i alla län ---------------------------------------------
# d: bedömningsraderna för en grupp i alla län (och riket). Valt län markeras,
# riket som streckad linje.
skapa_diagram_bedomning_lan <- function(d, vald_regionkod, rubrik = NULL, underrubrik = NULL,
                                        kalla = KALLA_REGTOP) {
  riket <- d$balanstal_procent[d$regionkod == "00" & d$har_bedomning]
  d <- dplyr::filter(d, regionkod != "00", har_bedomning, !is.na(balanstal_procent))
  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Ingen bedömning i något län"), 5, 4))
  d <- d |> dplyr::mutate(
    lan = forcats::fct_reorder(region, balanstal_procent),
    alfa = dplyr::if_else(regionkod == vald_regionkod, 1, 0.5),
    tooltip = paste0("<b>", region, "</b><br/>", bedomning_2040, " (", .fmt_proc(balanstal_procent), ")"))
  g <- ggplot2::ggplot(d, ggplot2::aes(x = balanstal_procent, y = lan, fill = bedomning_2040)) +
    ggplot2::geom_vline(xintercept = 0, colour = RD_TEXT_MUTED, linewidth = 0.4) +
    ggiraph::geom_col_interactive(ggplot2::aes(tooltip = tooltip, data_id = regionkod, alpha = alfa),
                                  width = 0.75) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_fill_manual(values = BEDOMNING_FARGER, guide = "none") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, " %")) +
    ggplot2::labs(x = "Balanstal 2040", y = NULL, title = rubrik, subtitle = underrubrik,
                  caption = paste0("Källa: ", kalla, if (length(riket) == 1) "\nStreckat: riket." else "")) +
    .rd_tema()
  if (length(riket) == 1)
    g <- g + ggplot2::geom_vline(xintercept = riket, linetype = "dashed", colour = RD_TEXT_MUTED)
  .girafe_std(g, width_svg = 5, height_svg = max(3.5, nrow(d) * 0.2 + 1.6), selection = FALSE)
}

# ---- Karta: bedömningen i Sveriges län ----------------------------------------------
# geo: länens geometri (hamta_lan_geometri()), d: bedömningsraderna för en
# utbildningsgrupp i alla län. Län utan prognos grå. Klick ger regionkod.
REGTOP_SAKNAS_FARG <- "#e9ecef"

# Tre klasser som i SCB:s rapport: viss brist/brist/stor brist = Brist,
# visst överskott/överskott/stort överskott = Överskott.
REGTOP_KLASS3 <- c("Brist", "Balans", "Överskott")
KLASS3_FARGER <- c("Brist" = BEDOMNING_FARGER[["Brist"]], "Balans" = BEDOMNING_FARGER[["Balans"]],
                   "Överskott" = BEDOMNING_FARGER[["Överskott"]])
regtop_klass3 <- function(bedomning) {
  o <- match(as.character(bedomning), REGTOP_BEDOMNINGAR)
  dplyr::case_when(is.na(o) ~ NA_character_, o < 4 ~ "Brist", o == 4 ~ "Balans", TRUE ~ "Överskott")
}

# klasser = 3 (som SCB:s rapport) eller 7 (alla bedömningsnivåer).
skapa_karta_bedomning <- function(geo, d, vald_regionkod = NULL, rubrik = NULL, kalla = KALLA_REGTOP,
                                  klasser = 3) {
  d <- dplyr::filter(d, regionkod != "00") |>
    dplyr::transmute(regionkod, region, balanstal_procent, bedomning = as.character(bedomning_2040),
                     klass = dplyr::if_else(har_bedomning,
                                            if (klasser == 3) regtop_klass3(bedomning_2040)
                                            else as.character(bedomning_2040),
                                            "Prognos saknas"))
  k <- dplyr::left_join(geo, d, by = "regionkod") |>
    dplyr::filter(!is.na(region)) |>
    dplyr::mutate(
      klass = factor(dplyr::coalesce(klass, "Prognos saknas"),
                     levels = c(if (klasser == 3) REGTOP_KLASS3 else REGTOP_BEDOMNINGAR, "Prognos saknas")),
      vald  = regionkod %in% vald_regionkod,
      tooltip = paste0("<b>", region, "</b><br/>", dplyr::coalesce(bedomning, "Prognos saknas"),
                       dplyr::if_else(is.na(balanstal_procent), "",
                                      paste0(" (balanstal ", .fmt_proc(balanstal_procent), ")"))))
  if (nrow(k) == 0) return(.girafe_std(.tom_plot("Ingen karta"), 4, 6))
  farger <- c(if (klasser == 3) KLASS3_FARGER else BEDOMNING_FARGER, "Prognos saknas" = REGTOP_SAKNAS_FARG)
  g <- ggplot2::ggplot(k) +
    ggiraph::geom_sf_interactive(ggplot2::aes(fill = klass, tooltip = tooltip, data_id = regionkod),
                                 colour = "white", linewidth = 0.2) +
    ggplot2::geom_sf(data = dplyr::filter(k, vald), fill = NA, colour = RD_TEXT, linewidth = 0.7) +
    ggplot2::scale_fill_manual(values = farger, name = NULL,
                               breaks = intersect(names(farger), as.character(k$klass))) +
    ggplot2::labs(title = rubrik, caption = paste0("Källa: ", kalla)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11),
                   plot.caption = ggplot2::element_text(size = 6.5, colour = RD_TEXT_MUTED, hjust = 0),
                   legend.text = ggplot2::element_text(size = 8),
                   legend.position = "right")
  .girafe_std(g, width_svg = 4.6, height_svg = 6, selection = TRUE)
}

# ---- Historik och prognosperioder över tid ---------------------------------------
# d: regtop_period_summa() (ar_fran, ar_till, typ, varde), ev. kolumnen serie.
# Historik som linje per år; prognosperioder som streckade vågräta streck
# över periodens år (årligt genomsnitt), i skuggat prognosfält.
skapa_diagram_prognos_tid <- function(d, y_label = "Antal per år", rubrik = NULL, underrubrik = NULL,
                                      kalla = KALLA_REGTOP, farger = NULL) {
  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 6.5, 3.6))
  if (!"serie" %in% names(d)) d$serie <- "Totalt"
  d$serie <- factor(d$serie, levels = unique(d$serie))
  if (is.null(farger)) farger <- stats::setNames(c(RD_PRIMARY, RD_TEXT_MUTED, RD_ACCENT)[seq_along(levels(d$serie))],
                                                 levels(d$serie))
  hist <- dplyr::filter(d, typ == "historik") |>
    dplyr::mutate(tooltip = paste0(serie, " ", period, ": ", .fmt_antal(varde)))
  prog <- dplyr::filter(d, typ == "prognos") |>
    dplyr::mutate(mitt = (ar_fran + ar_till) / 2,
                  tooltip = paste0(serie, " ", period, " (prognos): ", .fmt_antal(varde), " per år"))
  g <- ggplot2::ggplot()
  if (nrow(prog) > 0)
    g <- g +
      ggplot2::annotate("rect", xmin = min(prog$ar_fran) - 0.5, xmax = max(prog$ar_till) + 0.5,
                        ymin = -Inf, ymax = Inf, fill = .REGTOP_PROGNOS_FYLL) +
      ggplot2::annotate("text", x = min(prog$ar_fran), y = Inf, label = "Prognos", hjust = 0, vjust = 1.5,
                        size = 2.8, colour = RD_TEXT_MUTED) +
      ggplot2::geom_segment(data = prog, ggplot2::aes(x = ar_fran - 0.5, xend = ar_till + 0.5, y = varde,
                                                      yend = varde, colour = serie),
                            linewidth = 1, linetype = "22") +
      ggiraph::geom_point_interactive(data = prog, ggplot2::aes(x = mitt, y = varde, colour = serie,
                                                                tooltip = tooltip, data_id = paste(serie, period)),
                                      shape = 21, fill = "white", size = 2.2, stroke = 1)
  if (nrow(hist) > 0)
    g <- g +
      ggplot2::geom_line(data = hist, ggplot2::aes(x = ar_fran, y = varde, colour = serie), linewidth = 0.9) +
      ggiraph::geom_point_interactive(data = hist, ggplot2::aes(x = ar_fran, y = varde, colour = serie,
                                                                tooltip = tooltip, data_id = paste(serie, period)),
                                      size = 1.8)
  alla_ar <- c(d$ar_fran, d$ar_till)
  g <- g +
    ggplot2::scale_colour_manual(values = farger, name = NULL,
                                 guide = if (length(farger) > 1) "legend" else "none") +
    ggplot2::scale_x_continuous(breaks = seq(5 * floor(min(alla_ar) / 5), max(alla_ar), by = 5)) +
    ggplot2::scale_y_continuous(breaks = .heltal_skala(), labels = .fmt_antal, limits = c(0, NA)) +
    ggplot2::labs(x = NULL, y = y_label, title = rubrik, subtitle = underrubrik,
                  caption = paste0("Källa: ", kalla, "\nPrognosperioderna visas som årligt genomsnitt (streckat).")) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top",
                   panel.grid.major.y = ggplot2::element_line(color = "#eef2f5"))
  .girafe_std(g, width_svg = 6.5, height_svg = 3.6, selection = FALSE)
}

# ---- Senaste historiska år jämfört med en prognosperiod -----------------------------
# d: kategori, hist, prog (årligt genomsnitt). Klick ger kategorin.
skapa_diagram_prognos_jmf <- function(d, hist_label, prog_label, x_label = "Antal per år",
                                      rubrik = NULL, underrubrik = NULL, kalla = KALLA_REGTOP) {
  d <- dplyr::filter(d, !is.na(hist) | !is.na(prog))
  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data"), 6.8, 5))
  ordning <- d$kategori[order(dplyr::coalesce(d$prog, d$hist))]
  lang <- d |>
    tidyr::pivot_longer(c(hist, prog), names_to = "serie", values_to = "varde") |>
    dplyr::filter(!is.na(varde)) |>
    dplyr::mutate(serie = factor(dplyr::if_else(serie == "hist", hist_label, prog_label),
                                 levels = c(hist_label, prog_label)),
                  kategori = factor(kategori, levels = ordning),
                  tooltip = paste0("<b>", kategori, "</b><br/>", serie, ": ", .fmt_antal(varde)))
  farger <- stats::setNames(c(RD_PRIMARY, "#8edded"), c(hist_label, prog_label))
  g <- ggplot2::ggplot(lang, ggplot2::aes(x = varde, y = kategori, fill = serie)) +
    ggiraph::geom_col_interactive(ggplot2::aes(tooltip = tooltip, data_id = as.character(kategori)),
                                  position = ggplot2::position_dodge2(preserve = "single", reverse = TRUE,
                                                                      padding = 0.1), width = 0.75) +
    ggplot2::scale_fill_manual(values = farger, name = NULL) +
    ggplot2::scale_x_continuous(labels = .fmt_antal, expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = x_label, y = NULL, title = rubrik, subtitle = underrubrik,
                  caption = paste0("Källa: ", kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top")
  .girafe_std(g, width_svg = 7, height_svg = max(4, nrow(d) * 0.4 + 2), selection = TRUE)
}

# ---- Vägar in i yrket: examination per utbildningsform -------------------------------
REGTOP_UTBFORM_FARGER <- c(
  "Gymnasieskola" = RD_PRIMARY, "Komvux" = "#54a1bd", "Arbetsmarknadsutbildning" = "#8edded",
  "Yrkeshögskola" = "#459079", "Högskola, generell examen" = "#0f5f86",
  "Högskola, yrkesexamen" = "#d9653b", "Övrig examination" = "#b9c3ca")

# d: period, form (visningstext), varde (årligt genomsnitt). Staplat per period.
skapa_diagram_vagar <- function(d, rubrik = NULL, underrubrik = NULL, kalla = KALLA_REGTOP) {
  d <- dplyr::filter(d, !is.na(varde), varde != 0)
  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Ingen prognos för gruppen"), 6.5, 4))
  d <- d |> dplyr::mutate(
    form = factor(form, levels = rev(names(REGTOP_UTBFORM_FARGER))),
    tooltip = paste0("<b>", form, "</b><br/>", period, ": ", .fmt_antal(varde), " per år"))
  g <- ggplot2::ggplot(d, ggplot2::aes(x = period, y = varde, fill = form)) +
    ggiraph::geom_col_interactive(ggplot2::aes(tooltip = tooltip, data_id = paste(period, form)), width = 0.6) +
    ggplot2::scale_fill_manual(values = REGTOP_UTBFORM_FARGER, name = NULL,
                               breaks = intersect(names(REGTOP_UTBFORM_FARGER), as.character(d$form))) +
    ggplot2::scale_y_continuous(breaks = .heltal_skala(), labels = .fmt_antal) +
    ggplot2::labs(x = "Prognosperiod", y = "Examinerade per år", title = rubrik, subtitle = underrubrik,
                  caption = paste0("Källa: ", kalla)) +
    .rd_tema() +
    ggplot2::theme(legend.position = "right")
  .girafe_std(g, width_svg = 7, height_svg = 4, selection = FALSE)
}

# d: serie (län/riket), form, varde. Andel per utbildningsform, 100 % per serie.
skapa_diagram_vagar_andel <- function(d, rubrik = NULL, underrubrik = NULL, kalla = KALLA_REGTOP) {
  d <- d |> dplyr::filter(!is.na(varde), varde > 0) |>
    dplyr::group_by(serie) |> dplyr::mutate(andel = varde / sum(varde)) |> dplyr::ungroup()
  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Ingen prognos för gruppen"), 5, 3))
  d <- d |> dplyr::mutate(
    form = factor(form, levels = rev(names(REGTOP_UTBFORM_FARGER))),
    # Valt län överst, riket under.
    serie = factor(serie, levels = c("Riket", setdiff(unique(serie), "Riket"))),
    tooltip = paste0("<b>", serie, "</b><br/>", form, ": ", scales::percent(andel, accuracy = 1)))
  g <- ggplot2::ggplot(d, ggplot2::aes(x = andel, y = serie, fill = form)) +
    ggiraph::geom_col_interactive(ggplot2::aes(tooltip = tooltip, data_id = paste(serie, form)), width = 0.6) +
    ggplot2::scale_fill_manual(values = REGTOP_UTBFORM_FARGER, guide = "none") +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::labs(x = NULL, y = NULL, title = rubrik, subtitle = underrubrik,
                  caption = paste0("Källa: ", kalla)) +
    .rd_tema()
  .girafe_std(g, width_svg = 5, height_svg = 2.6, selection = FALSE)
}

# ---- Mätare (barometer) som i SCB:s rapport ------------------------------------------
# d: serie (t.ex. "Dalarnas län", "Riket") och bedomning_2040. Halvcirkel med
# överskott till vänster, balans överst och brist till höger; visaren pekar
# på bedömningen (sju nivåer, så att t.ex. viss brist hamnar nära balans).
.MATARE_VINKEL <- c(165, 145, 125, 90, 55, 35, 15)   # grader, stort överskott ... stor brist
skapa_diagram_matare <- function(d, kalla = KALLA_REGTOP) {
  baag <- function(fran, till, klass) {
    v <- seq(fran, till, length.out = 40) * pi / 180
    tibble::tibble(x = c(cos(v), rev(0.62 * cos(v))), y = c(sin(v), rev(0.62 * sin(v))), klass = klass)
  }
  sektorer <- dplyr::bind_rows(baag(112, 180, "Överskott"), baag(68, 112, "Balans"), baag(0, 68, "Brist"))
  d <- d |> dplyr::mutate(
    serie = factor(serie, levels = unique(serie)),
    ordning = match(as.character(bedomning_2040), REGTOP_BEDOMNINGAR),
    vinkel = .MATARE_VINKEL[8 - ordning] * pi / 180,
    text = dplyr::coalesce(as.character(bedomning_2040), "Prognos saknas"))
  sekt <- tidyr::expand_grid(serie = d$serie, sektorer)
  visare <- d |> dplyr::filter(!is.na(ordning)) |>
    dplyr::reframe(x = c(-0.06 * sin(vinkel), 0.95 * cos(vinkel), 0.06 * sin(vinkel)),
                   y = c(0.06 * cos(vinkel), 0.95 * sin(vinkel), -0.06 * cos(vinkel)), .by = c(serie, text))
  g <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = sekt, ggplot2::aes(x, y, fill = klass, group = klass), colour = "white") +
    ggplot2::geom_polygon(data = visare, ggplot2::aes(x, y, group = serie), fill = RD_TEXT) +
    ggplot2::geom_text(data = d, ggplot2::aes(x = 0, y = -0.22, label = text), size = 3.2, fontface = "bold",
                       colour = RD_TEXT) +
    ggplot2::facet_wrap(~serie) +
    ggplot2::scale_fill_manual(values = KLASS3_FARGER, breaks = rev(REGTOP_KLASS3), name = NULL) +
    ggplot2::coord_equal(xlim = c(-1.05, 1.05), ylim = c(-0.35, 1.05)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom", legend.text = ggplot2::element_text(size = 8),
                   strip.text = ggplot2::element_text(face = "bold", size = 10, margin = ggplot2::margin(b = 4)))
  .girafe_std(g, width_svg = 5, height_svg = 2.3, selection = FALSE)
}

# ---- Rörlighet: inflöde, utflöde och netto per period --------------------------------------
# d: regtop_rorlighet() (flode_text, period, typ, variabel in/ut/netto, varde).
# In uppåt, ut nedåt, netto som romb. En panel per flöde (egna skalor).
skapa_diagram_rorlighet <- function(d, rubrik = NULL, underrubrik = NULL, kalla = KALLA_REGTOP) {
  if (nrow(d) == 0) return(.girafe_std(.tom_plot("Inga data om flytt och pendling"), 7, 3.6))
  d <- d |> dplyr::mutate(
    flode_text = factor(flode_text, levels = intersect(REGTOP_FLODE_TEXT, unique(flode_text))),
    enhet_text = dplyr::if_else(flode == "pendling", "", " per år"),
    tooltip = paste0("<b>", flode_text, " ", period, if_else_prognos(typ), "</b><br/>",
                     c(`in` = "In", ut = "Ut", netto = "Netto")[variabel], ": ",
                     dplyr::if_else(varde > 0 & variabel == "netto", "+", ""), .fmt_antal(varde), enhet_text))
  staplar <- dplyr::filter(d, variabel %in% c("in", "ut"))
  netto <- dplyr::filter(d, variabel == "netto")
  g <- ggplot2::ggplot(mapping = ggplot2::aes(x = period)) +
    ggplot2::geom_hline(yintercept = 0, colour = RD_TEXT_MUTED, linewidth = 0.4) +
    ggiraph::geom_col_interactive(data = staplar,
                                  ggplot2::aes(y = varde, fill = variabel, tooltip = tooltip,
                                               data_id = paste(flode, period, variabel)), width = 0.6) +
    ggiraph::geom_point_interactive(data = netto,
                                    ggplot2::aes(y = varde, tooltip = tooltip, data_id = paste(flode, period, "netto")),
                                    shape = 23, size = 3, fill = RD_TEXT, colour = "white") +
    ggplot2::facet_wrap(~flode_text, scales = "free", nrow = 1) +
    ggplot2::scale_fill_manual(values = c(`in` = "#459079", ut = "#d9653b"),
                               labels = c(`in` = "Inflöde", ut = "Utflöde"), name = NULL) +
    ggplot2::scale_y_continuous(breaks = .heltal_skala(), labels = .fmt_antal) +
    ggplot2::labs(x = NULL, y = NULL, title = rubrik, subtitle = underrubrik,
                  caption = paste0("Källa: ", kalla, "\nRomb = netto. Flytt och migration: per år under perioden; ",
                                   "pendling: antal personer 2023 och 2040 (prognos).")) +
    .rd_tema() +
    ggplot2::theme(legend.position = "top", axis.text.x = ggplot2::element_text(size = 7),
                   strip.text = ggplot2::element_text(face = "bold"))
  .girafe_std(g, width_svg = 8, height_svg = 3.6, selection = FALSE)
}
