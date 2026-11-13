# ============================================================
#  func_data_gymnasiet_dimensionering.R
#  Dataåtkomst för Skolverkets bedömning av utbildningsutbud och
#  jämförelse mot gymnasieantagningens plats-/antagningsfördelning, samt
#  utvecklingen sedan föregående år (rör sig programmet åt det håll
#  Skolverket bedömer, och räcker det?).
#
#  Källa: oppna_data.skolverket.dimensionering (manuellt inlagd ur
#  Skolverkets planeringsunderlag, se skriv_skolverket_dimensionering.R).
#  Jämförs mot hamta_gymnasiedata() (func_data.R, dkf.gymnasieantagna).
#
#  Jämförelsen görs bara för gymnasieskolans 18 nationella program - det är
#  den enda delen av Skolverkets bedömning som har en motsvarande plats-/
#  antagningsandel i gymnasieantagningens data. Komvux (andra mått), IM/
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

# Andel av ett mått (platser eller antagna) per program, för ETT år, bara
# bland de 18 nationella programmen (bed$program) - IM och övriga program
# utan bedömning snedvrider annars nämnaren.
.dimensionering_andel_ar <- function(platser_df, bed, ar_val, matt) {
  d <- platser_df |>
    dplyr::filter(ar == ar_val, program %in% bed$program) |>
    dplyr::group_by(program) |>
    dplyr::summarise(varde = sum(.data[[matt]], na.rm = TRUE), .groups = "drop")
  tot <- sum(d$varde, na.rm = TRUE)
  # tot > 0 är ett enda villkor för hela urvalet (inte radvis) - if_else()
  # storleksätter utfallet efter villkoret, inte efter true/false, och
  # kraschar därför här; ett vanligt if duger.
  dplyr::mutate(d, andel = if (tot > 0) 100 * varde / tot else NA_real_)
}

# Klassar ett programs utveckling mot Skolverkets bedömda riktning:
# - "oförändrad" hos Skolverket -> alltid "Stabilt", oavsett egen rörelse.
# - redan i linje med bedömningen (avvikelse < 0,5 pe) -> "I linje...",
#   oavsett riktning förra året.
# - annars: rätt tecken på förändringen (mot bedömningens riktning) men
#   avvikelsen kvarstår -> "Rätt riktning, inte i mål"; fel tecken, eller
#   ingen rörelse alls -> "Fel riktning" / "Står still, fel håll".
# forandring = NA (inget jämförelseår i data, t.ex. första året) -> okänt.
dimensionering_riktningsstatus <- function(forandring, avvikelse, riktning) {
  behover <- dplyr::case_when(riktning == "öka" ~ 1, riktning == "minska" ~ -1, TRUE ~ 0)
  dplyr::case_when(
    is.na(forandring)           ~ "Inget jämförelseår",
    riktning == "oförändrad"    ~ "Stabilt",
    abs(avvikelse) < 0.5        ~ "I linje med bedömningen",
    forandring == 0             ~ "Står still, fel håll",
    sign(forandring) != behover ~ "Fel riktning",
    TRUE                        ~ "Rätt riktning, inte i mål"
  )
}

# Jämför andelen av valt mått (platser eller antagna) i ett redan
# geografi-/driftsformsfiltrerat urval (platser_df, flera år - se
# hamta_gymnasiedata()) mot Skolverkets bedömning för valt år, samt
# förändringen sedan föregående år. Program utan värde i urvalet får
# andel/förändring 0 (de finns ändå med i jämförelsen).
dimensionering_jamforelse <- function(platser_df, ar_val, matt = "platser") {
  stopifnot(matt %in% c("platser", "antagna"))
  bed <- dimensionering_program()
  nu <- .dimensionering_andel_ar(platser_df, bed, ar_val, matt)
  fg_finns <- (ar_val - 1L) %in% unique(platser_df$ar)
  fg <- if (fg_finns) .dimensionering_andel_ar(platser_df, bed, ar_val - 1L, matt)
        else dplyr::tibble(program = character(0), varde = numeric(0), andel = numeric(0))

  nu |>
    dplyr::right_join(bed, by = "program") |>
    dplyr::rename(varde_nu = varde, andel_nu = andel) |>
    dplyr::left_join(dplyr::rename(fg, varde_fg = varde, andel_fg = andel), by = "program") |>
    dplyr::mutate(
      dplyr::across(c(varde_nu, andel_nu), ~ dplyr::coalesce(.x, 0)),
      dplyr::across(c(varde_fg, andel_fg), ~ if (fg_finns) dplyr::coalesce(.x, 0) else NA_real_),
      avvikelse       = andel_nu - andel_bedomning,
      forandring      = andel_nu - andel_fg,
      riktningsstatus = dimensionering_riktningsstatus(forandring, avvikelse, forandringsriktning)
    ) |>
    dplyr::arrange(dplyr::desc(avvikelse))
}
