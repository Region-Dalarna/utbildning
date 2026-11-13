# ============================================================
#  func_data_gymnasiet_dimensionering.R
#  Dataåtkomst för Skolverkets bedömning av utbildningsutbud och
#  jämförelse mot gymnasieantagningens platsfördelning.
#
#  Källa: oppna_data.skolverket.dimensionering (manuellt inlagd ur
#  Skolverkets planeringsunderlag, se skriv_skolverket_dimensionering.R).
#  Jämförs mot hamta_gymnasiedata() (func_data.R, dkf.gymnasieantagna).
#
#  Jämförelsen görs bara för gymnasieskolans 18 nationella program - det är
#  den enda delen av Skolverkets bedömning som har en motsvarande platsandel
#  i gymnasieantagningens data. Komvux (andra mått, inga platser), IM/
#  introduktionsprogrammet (ingår inte i bedömningen) och total-/
#  inriktningsrader (ingen egen procentsats) räknas inte med.
# ============================================================

.dimensionering_cache <- new.env(parent = emptyenv())

hamta_skolverket_dimensionering <- function(force = FALSE) {
  if (force || is.null(.dimensionering_cache$df)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("skolverket", "dimensionering")) |>
      dplyr::collect()
    DBI::dbDisconnect(con)
    .dimensionering_cache$df <- rad
  }
  .dimensionering_cache$df
}

# De 18 nationella programmen i gymnasieskolan med Skolverkets bedömda andel
# (%) av platserna, läsåret 2026/27. Totalrader (t.ex. "Yrkesprogram
# totalt") och inriktningsrader (ingen egen bedömning) utesluts.
dimensionering_program <- function() {
  hamta_skolverket_dimensionering() |>
    dplyr::filter(utbildningsform == "Gymnasieskolan", !ar_totalrad,
                 !is.na(andel_bedomning_procent)) |>
    dplyr::transmute(program,
                     andel_erbjudna_riket = andel_erbjudna_platser_procent,
                     andel_bedomning      = andel_bedomning_procent,
                     forandringsriktning, lasar)
}

# Programnamn i Skolverkets bedömning som inte alls förekommer i
# gymnasieantagningens programlista (oavsett år/område) - tecken på en
# stavningsskillnad mellan de två källorna som bör rättas till.
dimensionering_okanda_program <- function() {
  setdiff(dimensionering_program()$program, unique(hamta_gymnasiedata()$program))
}

# Jämför platsandelen i ett redan filtrerat urval (år/område/driftsform, se
# hamta_gymnasiedata()) mot Skolverkets bedömning. Andelen räknas bara mot de
# 18 nationella programmens platser - IM och övriga program som saknar
# bedömning snedvrider annars nämnaren. Program utan platser i urvalet får
# andel_platser = 0 (de finns ändå med i jämförelsen).
dimensionering_jamforelse <- function(platser_df) {
  bed <- dimensionering_program()
  d <- platser_df |>
    dplyr::filter(program %in% bed$program) |>
    dplyr::group_by(program) |>
    dplyr::summarise(platser = sum(platser, na.rm = TRUE), .groups = "drop")
  tot <- sum(d$platser, na.rm = TRUE)
  d |>
    # tot > 0 är ett enda villkor för hela urvalet (inte radvis) - if_else()
    # storleksätter utfallet efter villkoret, inte efter true/false, och
    # kraschar därför här; ett vanligt if duger.
    dplyr::mutate(andel_platser = if (tot > 0) 100 * platser / tot else NA_real_) |>
    dplyr::right_join(bed, by = "program") |>
    dplyr::mutate(platser       = dplyr::coalesce(platser, 0),
                 andel_platser = dplyr::coalesce(andel_platser, 0),
                 avvikelse     = andel_platser - andel_bedomning) |>
    dplyr::arrange(dplyr::desc(avvikelse))
}
