# ============================================================
#  func_data_folkhogskola.R
#  Dataåtkomst för folkhögskolestatistik.
#
#  Källa: oppna_data.mikro_db.folkhogskola_elever
#
#  Tabellen har fyra granulariteter (kolumnen granularitet):
#  - "KursInr":  år x kursinriktning x huvudman x kön x område. Används för
#                kursdeltaganden, avbrott och resultat (rena radräkningar,
#                som går att summera).
#  - "Huvudman": år x huvudman x kön x område - unika deltagare.
#  - "Kon":      år x kön x område - unika deltagare.
#  - "Ar":       år x område - unika deltagare (= summan av "Kon").
#  Deltagare (unika individer) får INTE summeras över kursinriktningar -
#  de hämtas därför från "Kon"/"Huvudman" (hamta_folkhogskola_deltagare()).
#
#  "program" i diagramfunktionernas mening = ämnesområde, härlett ur
#  kursinriktningskodens tiotal (2x = estetiska ämnen, 7x = språk osv.).
#  De 62 kursinriktningarna blir annars för många staplar. Kod 1 (Allmän,
#  bred ämnesinriktning) får en egen stapel eftersom det är där
#  behörigheterna finns. Huvudman (Region/Enskild) är ett filter i modulen.
#
#  Kommunraderna avser kurskommunen, så bara kommuner med folkhögskola
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

# Huvudman ska vara text ("Region"/"Enskild"). Ett uttag hade koderna 2/5
# i stället - de översätts här så att båda varianterna fungerar.
.fhsk_huvudman <- c("2" = "Region", "5" = "Enskild")

.folkhogskola_cache <- new.env(parent = emptyenv())

rensa_folkhogskola_data <- function(rad) {
  rad |>
    dplyr::rename(kommkod = regionkod, kommun = region, namn = kursinr) |>
    dplyr::mutate(
      ar               = as.integer(ar),
      kommkod          = as.character(kommkod),
      organisationstyp = dplyr::coalesce(unname(.fhsk_huvudman[as.character(huvudman)]),
                                         as.character(huvudman)),
      kursinr          = suppressWarnings(as.integer(kursinr_kod)),   # "001" -> 1
      program = dplyr::case_when(
        is.na(kursinr) ~ NA_character_,                               # ej KursInr-rad
        kursinr == 1L  ~ "Allmän, bred ämnesinriktning",
        kursinr < 10L  ~ "Uppgift saknas",
        TRUE ~ dplyr::coalesce(unname(.fhsk_amnesomrade[as.character(kursinr %/% 10L)]),
                               "Uppgift saknas")
      ),
      dplyr::across(c(kursdeltaganden, deltagare, avbrott, grundlbeh, eftergym_klar, yrkesbeh),
                    as.numeric)
    )
}

.folkhogskola_ra <- function(force = FALSE) {
  if (force || is.null(.folkhogskola_cache$ra)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("mikro_db", "folkhogskola_elever")) |>
      dplyr::collect()
    DBI::dbDisconnect(con)
    .folkhogskola_cache$ra <- rensa_folkhogskola_data(rad)
  }
  .folkhogskola_cache$ra
}

# Kursdeltaganden, avbrott och resultat per ämnesområde och huvudman
# (KursInr-raderna, summerade över kön).
hamta_folkhogskola_data <- function(force = FALSE) {
  if (force || is.null(.folkhogskola_cache$df)) {
    .folkhogskola_cache$df <- .folkhogskola_ra(force) |>
      dplyr::filter(granularitet == "KursInr") |>
      dplyr::group_by(ar, kommkod, kommun, geo_niva, organisationstyp, program) |>
      dplyr::summarise(
        dplyr::across(c(kursdeltaganden, avbrott, grundlbeh, eftergym_klar, yrkesbeh),
                      ~sum(.x, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        andel_avbrott = dplyr::if_else(kursdeltaganden > 0, 100 * avbrott / kursdeltaganden, NA_real_)
      )
  }
  .folkhogskola_cache$df
}

# Unika deltagare per år x område x kön ("program" = kön, som i Grundskola).
# huvudman = "_alla_" -> granulariteten "Kon", annars "Huvudman" för vald
# huvudman.
hamta_folkhogskola_deltagare <- function(huvudman = "_alla_") {
  ra <- .folkhogskola_ra()
  d <- if (is.null(huvudman) || huvudman == "_alla_") {
    dplyr::filter(ra, granularitet == "Kon")
  } else {
    dplyr::filter(ra, granularitet == "Huvudman", organisationstyp == .env$huvudman)
  }
  d |> dplyr::transmute(ar, kommkod, kommun, geo_niva, program = kon, deltagare)
}
