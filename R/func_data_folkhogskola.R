# ============================================================
#  func_data_folkhogskola.R
#  Dataåtkomst för folkhögskolestatistik.
#
#  Källa: oppna_data.mikro_db.folkhogskola_elever
#
#  "program" i diagramfunktionernas mening = ämnesområde, härlett ur
#  kursinriktningskodens tiotal (2x = estetiska ämnen, 7x = språk osv.).
#  De 62 kursinriktningarna blir annars för många staplar. Kod 1 (Allmän,
#  bred ämnesinriktning) får en egen stapel eftersom det är där
#  behörigheterna finns. Kursinriktningen ligger kvar i "namn".
#  Huvudman (Region/Enskild) är ett eget filter i modulen.
#
#  OBS: deltagare är unika individer per kursinriktning och huvudman och
#  dubbelräknas när raderna summeras till ämnesområde/total - därför är
#  indikatorn Deltagare avstängd (klar = FALSE) i mod_folkhogskola.R.
#
#  Kommunraderna avser skolans kommun, så bara kommuner med folkhögskola
#  finns i datan.
# ============================================================

.fhsk_amnesomrade <- c(
  "1" = "Beteendevetenskap, humaniora",
  "2" = "Estetiska ämnen",
  "3" = "Företagsekonomi, handel och kontor",
  "4" = "Matematik, naturvetenskap",
  "5" = "Medicin, hälsa och sjukvård",
  "6" = "Samhällsvetenskap",
  "7" = "Språk",
  "8" = "Teknik",
  "9" = "Övriga ämnen"
)

.folkhogskola_cache <- new.env(parent = emptyenv())

rensa_folkhogskola_data <- function(rad) {
  rad |>
    dplyr::rename(
      kommkod          = regionkod,
      kommun           = region,
      organisationstyp = hman_namn,
      namn             = kursinr_namn
    ) |>
    dplyr::mutate(
      ar      = as.integer(ar),
      kommkod = as.character(kommkod),
      kursinr = as.integer(kursinr),
      program = dplyr::case_when(
        kursinr == 1L ~ "Allmän, bred ämnesinriktning",
        kursinr < 10L ~ "Uppgift saknas",
        TRUE ~ dplyr::coalesce(unname(.fhsk_amnesomrade[as.character(kursinr %/% 10L)]),
                               "Uppgift saknas")
      )
    ) |>
    dplyr::group_by(ar, kommkod, kommun, geo_niva, organisationstyp, program) |>
    dplyr::summarise(
      dplyr::across(c(kursdeltaganden, deltagare, avbrott, grundlbeh, eftergym_klar, yrkesbeh),
                    ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      andel_avbrott = dplyr::if_else(kursdeltaganden > 0, 100 * avbrott / kursdeltaganden, NA_real_)
    )
}

hamta_folkhogskola_data <- function(force = FALSE) {
  if (force || is.null(.folkhogskola_cache$df)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("mikro_db", "folkhogskola_elever")) |>
      dplyr::collect()
    DBI::dbDisconnect(con)
    .folkhogskola_cache$df <- rensa_folkhogskola_data(rad)
  }
  .folkhogskola_cache$df
}
