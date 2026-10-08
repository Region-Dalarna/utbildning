# ============================================================
#  func_data_gymnasiet_avgangna.R
#  Dataåtkomst för avgångna gymnasieelever (examen, studiebevis, betyg).
#
#  Källa: oppna_data.mikro_db.gymnasiet_avgangna (Gymnasiet – avgångna).
#  Avgångsår 2014 och framåt.
#
#  Tabellen är lång med kolumnen granularitet. Varje granularitet är en egen
#  uppdelning - filtrera ALLTID på en och summera aldrig över flera:
#    Typ (totaler), TypKon, TypBakgrund, TypNyanland, TypHuvudman,
#    TypStudievag (program/inriktning), TypBeh, TypBetygstyp.
#  avgangna = unika elever. Summering över elevtyp, kön och inriktning inom
#  program är säker (en elev har en av varje).
#
#  geografi: "skolkommun" (elever på skolor i området) eller "bokommun"
#  (områdets ungdomar, var de än går i skola).
# ============================================================

# "Visa fördelat på" -> granularitet och den kolumn som är gruppen.
AVGANGNA_UPPDELNING <- list(
  ingen      = list(label = "Ingen uppdelning", granularitet = "Typ",          kol = NULL),
  kon        = list(label = "Kön",              granularitet = "TypKon",       kol = "kon"),
  bakgrund   = list(label = "Bakgrund",         granularitet = "TypBakgrund",  kol = "utlsvbakg_namn"),
  nyanland   = list(label = "Nyanländ",         granularitet = "TypNyanland",  kol = "nyanland"),
  huvudman   = list(label = "Huvudman",         granularitet = "TypHuvudman",  kol = "huvudman"),
  program    = list(label = "Program",          granularitet = "TypStudievag", kol = "program_namn"),
  inriktning = list(label = "Inriktning",       granularitet = "TypStudievag", kol = "inriktning_namn")
)

.avgangna_cache <- new.env(parent = emptyenv())

rensa_gymnasiet_avgangna <- function(rad) {
  rad |>
    dplyr::rename(kommkod = regionkod, kommun = region) |>
    dplyr::mutate(
      ar      = as.integer(ar),
      kommkod = as.character(kommkod),
      program_namn    = dplyr::coalesce(gymnasieprogram, "Okänt program"),
      inriktning_namn = paste0(program_namn, " – ",
                               dplyr::coalesce(inriktning, "Ingen inriktning")),
      dplyr::across(c(avgangna, jmftal_summa, antal_med_jmftal), as.numeric)
    )
}

hamta_gymnasiet_avgangna <- function(force = FALSE) {
  if (force || is.null(.avgangna_cache$df)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("mikro_db", "gymnasiet_avgangna")) |>
      dplyr::collect()
    DBI::dbDisconnect(con)
    .avgangna_cache$df <- rensa_gymnasiet_avgangna(rad)
  }
  .avgangna_cache$df
}

# Summerar rader till en rad per (ar, kommkod, grupp) med examen,
# studiebevis, betygspoäng och andelar. "grupp" blir "program" i
# diagramfunktionernas mening. examen_grupp summeras bort men räknas ut.
avgangna_summera <- function(d, grupp_kol = NULL) {
  d <- dplyr::mutate(d, program = if (is.null(grupp_kol)) "Alla" else .data[[grupp_kol]])
  d |>
    dplyr::group_by(ar, kommkod, kommun, geo_niva, program) |>
    dplyr::summarise(
      examen      = sum(avgangna[examen_grupp == "Examen"], na.rm = TRUE),
      studiebevis = sum(avgangna[examen_grupp == "Studiebevis (minst 2500 poäng)"], na.rm = TRUE),
      avgangna    = sum(avgangna, na.rm = TRUE),
      jmftal_summa     = sum(jmftal_summa, na.rm = TRUE),
      antal_med_jmftal = sum(antal_med_jmftal, na.rm = TRUE),
      .groups = "drop") |>
    dplyr::mutate(
      andel_examen      = dplyr::if_else(avgangna > 0, 100 * examen / avgangna, NA_real_),
      andel_studiebevis = dplyr::if_else(avgangna > 0, 100 * studiebevis / avgangna, NA_real_),
      betyg = dplyr::if_else(antal_med_jmftal > 0, jmftal_summa / antal_med_jmftal, NA_real_)
    )
}
