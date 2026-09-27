# ============================================================
#  func_data_yh.R
#  Dataåtkomst för YH-statistik (Yrkeshögskola).
#
#  Källor (alla i oppna_data.mikro_db, skrivna av dataskript/yh_till_databas.R):
#  - yh_studerande        påbörjade/pågående/examen per år
#  - yh_genomstromning    andel examen bland avslutade, per startår
#  - yh_uppfoljning       RAKS-etablering 1/3/5 år efter examen (redan
#                         summerad per region, utan hem-/arbetskommun)
#
#  Tabellerna har redan en korrekt geo_niva (riket/lan/kommun) och en färdig
#  länsrad (regionkod "20"), så ingen egen viktning behövs för Hela Dalarna.
#
#  Kolumner döps om till samma kontrakt som func_diagram.R:s generiska
#  diagramfunktioner förväntar (program, namn, andel, kommkod, geo_niva, ar)
#  - så att diagramfunktionerna kan återanvändas OFÖRÄNDRADE, precis som för
#  gymnasiet. "program" = utbildningsomrade.
#
#  Förenklingar i denna första version:
#  - Kön och studieform summeras ihop (ingen kön/studieform-vy än).
#  - Ingen samverkansområde/karta - bara Kommun/Hela Dalarna/Riket.
# ============================================================

.yh_hamta_tabell <- function(tabell) {
  con <- shiny_uppkoppling_las("oppna_data")
  rad <- dplyr::tbl(con, dbplyr::in_schema("mikro_db", tabell)) |>
    dplyr::collect()
  DBI::dbDisconnect(con)
  rad
}

# ---- Studerande --------------------------------------------------------
.yh_studerande_cache <- new.env(parent = emptyenv())

rensa_yh_studerande <- function(rad) {
  rad |>
    dplyr::rename(
      kommkod = regionkod,
      kommun  = region,
      program = utbildningsomrade
    ) |>
    dplyr::mutate(ar = as.integer(ar), kommkod = as.character(kommkod)) |>
    dplyr::group_by(ar, kommkod, kommun, geo_niva, program) |>
    dplyr::summarise(
      dplyr::across(c(paborjad, pagaende, avslutad, examen, avslutad_utan_examen),
                    ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

hamta_yh_studerande <- function(force = FALSE) {
  if (force || is.null(.yh_studerande_cache$df)) {
    .yh_studerande_cache$df <- rensa_yh_studerande(.yh_hamta_tabell("yh_studerande"))
  }
  .yh_studerande_cache$df
}

# ---- Genomströmning -----------------------------------------------------
.yh_genomstromning_cache <- new.env(parent = emptyenv())

rensa_yh_genomstromning <- function(rad) {
  rad |>
    dplyr::rename(
      kommkod = regionkod,
      kommun  = region,
      ar      = startar,
      program = utbildningsomrade
    ) |>
    dplyr::mutate(ar = as.integer(ar), kommkod = as.character(kommkod)) |>
    dplyr::group_by(ar, kommkod, kommun, geo_niva, program) |>
    dplyr::summarise(
      dplyr::across(c(paborjade, avslutade, examen, avslutad_utan_examen),
                    ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(andel = dplyr::if_else(avslutade > 0, 100 * examen / avslutade, NA_real_))
}

hamta_yh_genomstromning <- function(force = FALSE) {
  if (force || is.null(.yh_genomstromning_cache$df)) {
    .yh_genomstromning_cache$df <- rensa_yh_genomstromning(.yh_hamta_tabell("yh_genomstromning"))
  }
  .yh_genomstromning_cache$df
}

# Slår ihop valda rader till EN viktad andel per år (summa examen / summa
# avslutade). Diagramfunktionerna för genomströmning tar ett ovägt medel av
# andel per år, så datan skickas in färdigaggregerad för att få en korrekt
# total över alla utbildningsområden.
yh_genomstromning_per_ar <- function(df) {
  df |>
    dplyr::group_by(ar) |>
    dplyr::summarise(avslutade = sum(avslutade, na.rm = TRUE),
                     examen    = sum(examen, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(andel = dplyr::if_else(avslutade > 0, 100 * examen / avslutade, NA_real_))
}

# Riket-serie för jämförelselinjen. program_val = NULL -> alla områden.
genomstromning_riket_yh <- function(df_full, program_val = NULL) {
  d <- dplyr::filter(df_full, geo_niva == "riket")
  if (!is.null(program_val)) d <- dplyr::filter(d, program == program_val)
  yh_genomstromning_per_ar(d) |>
    dplyr::select(ar, andel_riket = andel)
}

# ---- Etablering (SCB/RAKS) ------------------------------------------------
# Samma kolumnkontrakt/enheter som gymnasiets hamta_etablering():
# syss/stud/arblos/ovriga/etabl/antal är ANTAL (inte andelar), aggregering
# sker i modulen via summering. inkomst_summa är en SUMMA - medelinkomst =
# inkomst_summa / etabl_med_inkomst.
.yh_etablering_cache <- new.env(parent = emptyenv())

rensa_yh_etablering <- function(rad) {
  rad |>
    dplyr::rename(
      kommkod = regionkod,
      kommun  = region,
      program = utbildningsomrade
    ) |>
    dplyr::mutate(
      kommkod  = as.character(kommkod),
      ar       = as.integer(substr(exam_ar_interval, 1, 4)),
      antal_ar = as.integer(antal_ar)
    ) |>
    dplyr::group_by(ar, exam_ar_interval, uppf_ar_interval, antal_ar,
                    kommkod, kommun, geo_niva, program) |>
    dplyr::summarise(
      dplyr::across(c(syss, stud, arblos, ovriga, antal, etabl,
                      etabl_med_inkomst, inkomst_summa),
                    ~sum(as.numeric(.x), na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(namn = program)   # ingen inriktnings-underindelning för YH
}

hamta_yh_etablering <- function(force = FALSE) {
  if (force || is.null(.yh_etablering_cache$df)) {
    .yh_etablering_cache$df <- rensa_yh_etablering(.yh_hamta_tabell("yh_uppfoljning"))
  }
  .yh_etablering_cache$df
}

# Riket-andel per program och antal_ar (jämförelselinje i trend).
# program_val = NULL -> alla program sammanslagna (totalvy). Samma mönster
# som gymnasiets etablering_riket() i func_data.R.
yh_etablering_riket <- function(df_full, program_val = NULL, metrik) {
  d <- df_full |> dplyr::filter(geo_niva == "riket")
  if (!is.null(program_val)) d <- dplyr::filter(d, program == program_val)
  d |>
    dplyr::group_by(ar, antal_ar) |>
    dplyr::summarise(
      status_sum = sum(.data[[metrik]], na.rm = TRUE),
      antal_sum  = sum(antal, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(andel_riket = dplyr::if_else(
      antal_sum > 0, status_sum / antal_sum, NA_real_)) |>
    dplyr::select(ar, antal_ar, andel_riket)
}
