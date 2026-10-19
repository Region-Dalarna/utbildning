# ============================================================
#  func_data_regtop.R
#  Dataåtkomst för SCB:s Regionala trender och prognoser 2025 (RegTop2025).
#
#  Källa: oppna_data.regtop (en tabell per innehåll, se instruktionen till
#  inläsningen). Län och riket, inga kommuner. regionkod "00" = riket.
#
#  - bedomning_2040        brett: en rad per region och utbildningsgrupp
#  - tillgang_efterfragan  långt: tillgång/efterfrågan per år, historik och prognos
#  - forandringsfaktorer   vad som driver tillgång och efterfrågan 2024-2040
#  - arbetskraft_utbildningsgrupp  arbetskraften per kön 2023 och 2040
#  - vanligaste_yrken      de tre vanligaste yrkena per grupp och län
#  - antaganden_utbildningsvaxling  SCB:s antaganden (text, ingen region)
#  - nyckel_utbildningsgrupper      aggregat (grövre gruppering) för filter
#
#  ENHETER: kolumnen enhet styr alltid jämförelser. Länen redovisar ibland
#  summa för perioden (antal_summa_period) där riket redovisar årligt
#  genomsnitt (antal_arligt_genomsnitt) - räkna om med regtop_arligt() eller
#  regtop_summa_period() innan län och riket jämförs.
#
#  Publicerad statistik: ingen röjandekontroll (min_antal = 0).
# ============================================================

KALLA_REGTOP <- "SCB, Regionala trender och prognoser 2025"

.regtop_cache <- new.env(parent = emptyenv())

hamta_regtop <- function(tabell, force = FALSE) {
  if (force || is.null(.regtop_cache[[tabell]])) {
    con <- shiny_uppkoppling_las("oppna_data")
    on.exit(DBI::dbDisconnect(con))
    d <- dplyr::tbl(con, dbplyr::in_schema("regtop", tabell)) |> dplyr::collect()
    if ("regionkod" %in% names(d)) d$regionkod <- as.character(d$regionkod)
    .regtop_cache[[tabell]] <- d
  }
  .regtop_cache[[tabell]]
}

# ---- Enheter ----------------------------------------------------------------
.regtop_ar_i_period <- function(d) as.integer(d$ar_till) - as.integer(d$ar_fran) + 1L

# Allt som årligt genomsnitt (summa för perioden / antal år).
regtop_arligt <- function(d) {
  summa <- d$enhet %in% "antal_summa_period"
  d$varde[summa] <- d$varde[summa] / .regtop_ar_i_period(d)[summa]
  d$enhet[summa] <- "antal_arligt_genomsnitt"
  d
}

# Allt som summa för perioden (årligt genomsnitt * antal år).
regtop_summa_period <- function(d) {
  arligt <- d$enhet %in% "antal_arligt_genomsnitt"
  d$varde[arligt] <- d$varde[arligt] * .regtop_ar_i_period(d)[arligt]
  d$enhet[arligt] <- "antal_summa_period"
  d
}

# ---- Bedömning 2040 ------------------------------------------------------------
# Med aggregat (grövre gruppering) ur utbildningsnyckeln, för filtret.
regtop_bedomning <- function() {
  if (is.null(.regtop_cache$bedomning_med_aggregat)) {
    nyckel <- hamta_regtop("nyckel_utbildningsgrupper") |>
      dplyr::filter(!is.na(sun2020grp_alt_reg)) |>
      dplyr::distinct(utbildningsgrupp_kod = as.character(sun2020grp_alt_reg), aggregat) |>
      dplyr::distinct(utbildningsgrupp_kod, .keep_all = TRUE)
    .regtop_cache$bedomning_med_aggregat <- hamta_regtop("bedomning_2040") |>
      dplyr::mutate(utbildningsgrupp_kod = as.character(utbildningsgrupp_kod)) |>
      dplyr::left_join(nyckel, by = "utbildningsgrupp_kod") |>
      dplyr::mutate(
        aggregat = dplyr::coalesce(aggregat, "Övriga"),
        bedomning_2040 = factor(bedomning_2040, levels = REGTOP_BEDOMNINGAR),
        har_bedomning = framskrivning_2040 %in% "Ja" & !is.na(bedomning_2040))
  }
  .regtop_cache$bedomning_med_aggregat
}

# Regionerna att välja mellan: riket först, sedan länen i namnordning.
regtop_regioner <- function() {
  r <- regtop_bedomning() |> dplyr::distinct(regionkod, region)
  r <- dplyr::bind_rows(dplyr::filter(r, regionkod == "00"),
                        dplyr::arrange(dplyr::filter(r, regionkod != "00"), region))
  stats::setNames(r$regionkod, r$region)
}

# ---- Förändringsfaktorer ----------------------------------------------------------
REGTOP_FAKTOR_TEXT <- c(
  examination            = "Examination",
  vidareutbildning       = "Vidareutbildning",
  inrikes_flyttnetto     = "Inrikes flyttnetto",
  migrationsnetto        = "Migrationsnetto",
  lamnar_arbetskraften   = "Lämnar arbetskraften (främst pension)",
  naringsgrensutveckling = "Näringsgrensutveckling",
  yrkesstruktur          = "Yrkesstruktur",
  utbildningsvaxling     = "Utbildningsväxling"
)

regtop_faktorer <- function(regionkod, kod) {
  hamta_regtop("forandringsfaktorer") |>
    dplyr::filter(.data$regionkod == .env$regionkod, utbildningsgrupp_kod == kod) |>
    dplyr::mutate(faktor_text = dplyr::coalesce(unname(REGTOP_FAKTOR_TEXT[faktor]), faktor))
}

# ---- Vanligaste yrken -----------------------------------------------------------------
# Kolumnnamnen för yrket varierar mellan tabellerna - ta den som finns.
regtop_vanligaste_yrken <- function(regionkod, kod) {
  d <- hamta_regtop("vanligaste_yrken")
  yrke_kol <- intersect(c("yrke", "yrke_namn", "yrkesbenamning", "ssyk_namn"), names(d))[1]
  if (is.na(yrke_kol)) return(character(0))
  d <- dplyr::filter(d, .data$regionkod == .env$regionkod, utbildningsgrupp_kod == kod)
  if ("rang" %in% names(d)) d <- dplyr::arrange(d, rang)
  # Andel (eller antal) inom parentes, när den finns.
  tillagg <- if (all(c("varde", "enhet") %in% names(d)))
    dplyr::case_when(is.na(d$varde)          ~ "",
                     d$enhet %in% "procent" ~ paste0(" (", scales::number(d$varde, accuracy = 1), " %)"),
                     TRUE                    ~ paste0(" (", scales::number(d$varde, big.mark = " "), ")"))
  else ""
  unique(paste0(d[[yrke_kol]], tillagg))
}

# ---- Perioder, kön och summering --------------------------------------------------
# Tabeller med både enskilda år och perioder: ar är text ("2019", "2024-2030")
# med ar_fran/ar_till. Fyller ar_fran/ar_till ur ar om de saknas.
.regtop_perioder <- function(d) {
  if (!"ar_fran" %in% names(d)) d$ar_fran <- NA_integer_
  if (!"ar_till" %in% names(d)) d$ar_till <- NA_integer_
  ar <- as.character(d$ar)
  d$ar_fran <- dplyr::coalesce(as.integer(d$ar_fran), as.integer(substr(ar, 1, 4)))
  d$ar_till <- dplyr::coalesce(as.integer(d$ar_till), as.integer(sub("^.*-", "", ar)))
  d
}

# Kön: finns en totalrad används den, annars summeras kvinnor och män nedströms.
REGTOP_KON_TOTAL <- c("Totalt", "Båda könen", "Kvinnor och män", "Samtliga")
regtop_kon_total <- function(d) {
  if (!"kon" %in% names(d) || !any(d$kon %in% REGTOP_KON_TOTAL)) return(d)
  dplyr::filter(d, kon %in% REGTOP_KON_TOTAL)
}

# Summerar till en rad per (grupp, period, typ) som årligt genomsnitt, med
# en läsbar periodtext ("2023", "2031–2035").
regtop_period_summa <- function(d, grupp = NULL) {
  d |>
    .regtop_perioder() |>
    regtop_arligt() |>
    regtop_kon_total() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(grupp, "ar_fran", "ar_till", "typ")))) |>
    dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(period = dplyr::if_else(ar_fran == ar_till, as.character(ar_fran),
                                          paste0(ar_fran, "–", ar_till)))
}

# ---- Utbildningsformer (examination_utbildningsform) ---------------------------
REGTOP_UTBFORM_TEXT <- c(
  gymnasieskola            = "Gymnasieskola",
  komvux                   = "Komvux",
  arbetsmarknadsutbildning = "Arbetsmarknadsutbildning",
  yrkeshogskola            = "Yrkeshögskola",
  generell_examen_hogskola = "Högskola, generell examen",
  yrkesexamen_hogskola     = "Högskola, yrkesexamen",
  ovrig_examination        = "Övrig examination"
)

# ---- Eftergymnasialt (eftergymnasial) ------------------------------------------------
# Indikatorerna: utbildningsform + mått. nyborjare finns bara för riket.
REGTOP_EFTERGYMN <- list(
  yh_antagna     = list(label = "YH, antagna", utbildningsform = "yrkeshogskola", matt = "antagna"),
  yh_examinerade = list(label = "YH, examinerade", utbildningsform = "yrkeshogskola", matt = "examinerade"),
  hs_yrkesexamen = list(label = "Högskola, yrkesexamen", utbildningsform = "hogskola_yrkesexamen",
                        matt = "examinerade"),
  hs_generell    = list(label = "Högskola, generell examen", utbildningsform = "hogskola_generell_examen",
                        matt = "examinerade"),
  hs_nyborjare   = list(label = "Högskola, nybörjare (riket)", utbildningsform = "hogskola_yrkesexamen",
                        matt = "nyborjare", bara_riket = TRUE),
  # nyborjare_larosate: nybörjare på yrkesexamensprogram per lärosäte (ingen region).
  hs_larosate    = list(label = "Högskola, nybörjare per lärosäte", tabell = "nyborjare_larosate",
                        matt = "nyborjare", larosate = TRUE)
)

# ---- Rörlighet (rorlighet) -------------------------------------------------------------
# flode: inrikes_flytt (bara län), migration (län och riket), pendling (bara
# län, 2023 och 2040, antal). variabel in/ut/netto, utflöden negativa.
REGTOP_FLODE_TEXT <- c(inrikes_flytt = "Inrikes flytt", migration = "Migration", pendling = "Arbetspendling")

# Rörligheten för en region och grupp: perioder som årligt genomsnitt,
# pendlingen som antal personer det året.
regtop_rorlighet <- function(regionkod, kod) {
  hamta_regtop("rorlighet") |>
    dplyr::filter(.data$regionkod == .env$regionkod, utbildningsgrupp_kod == kod) |>
    .regtop_perioder() |>
    regtop_arligt() |>
    dplyr::mutate(flode_text = dplyr::coalesce(unname(REGTOP_FLODE_TEXT[flode]), flode),
                  period = dplyr::if_else(ar_fran == ar_till, as.character(ar_fran),
                                          paste0(ar_fran, "–", ar_till)))
}
