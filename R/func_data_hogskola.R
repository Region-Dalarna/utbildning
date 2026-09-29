# ============================================================
#  func_data_hogskola.R
#  Dataåtkomst för högskolestatistik.
#
#  Källor (oppna_data.mikro_db):
#  - hogskola_aktivitet    kursregistreringar/deltagare per lärosäte och program
#  - hogskola_examen       antal examina per examenstyp och SUN 2020-inriktning
#  - hogskola_etablering   RAKS-etablering 1/3/5 år efter examen, summerad per
#                          region och ämnesområde av
#                          dataskript/hogskola_uppfoljning_till_databas.R
#                          (samma kolumner som yh_uppfoljning)
#
#  regionkod/region är studentens HEMKOMMUN, inte lärosätets ort.
#
#  "program" i diagramfunktionernas mening:
#  - Studerande: lärosäte (hskod_namn). De minsta slås ihop till "Övriga
#    lärosäten" i modulen.
#  - Examina/etablering: ämnesområde = SUN 2020:s bredaste inriktning
#    (förstasiffran i sun2020inr resp. huvomgrp).
# ============================================================

# SUN 2020, bredaste inriktningsnivå (förstasiffran i inriktningskoden).
.sun_bredast <- c(
  "0" = "Allmän utbildning",
  "1" = "Pedagogik och lärarutbildning",
  "2" = "Humaniora och konst",
  "3" = "Samhällsvetenskap, juridik, handel, administration",
  "4" = "Naturvetenskap, matematik och IKT",
  "5" = "Teknik och tillverkning",
  "6" = "Lant- och skogsbruk samt djursjukvård",
  "7" = "Hälso- och sjukvård samt social omsorg",
  "8" = "Tjänster",
  "9" = "Okänd inriktning"
)

# Examenstyp ur första bokstaven i tmgrp (G = generell, K = konstnärlig,
# Y/P = yrkesexamen).
.examenstyp <- c("G" = "Generell examen", "K" = "Konstnärlig examen",
                 "Y" = "Yrkesexamen", "P" = "Yrkesexamen")

# ---- Studerande (aktivitet) -----------------------------------------------
.hogskola_aktivitet_cache <- new.env(parent = emptyenv())

rensa_hogskola_aktivitet <- function(rad) {
  rad |>
    dplyr::rename(kommkod = regionkod, kommun = region, program = hskod_namn) |>
    dplyr::mutate(ar = as.integer(ar), kommkod = as.character(kommkod)) |>
    dplyr::group_by(ar, kommkod, kommun, geo_niva, program) |>
    dplyr::summarise(
      dplyr::across(c(kursregistreringar, deltagare), ~sum(.x, na.rm = TRUE)),
      .groups = "drop")
}

hamta_hogskola_aktivitet <- function(force = FALSE) {
  if (force || is.null(.hogskola_aktivitet_cache$df)) {
    .hogskola_aktivitet_cache$df <-
      rensa_hogskola_aktivitet(.yh_hamta_tabell("hogskola_aktivitet"))
  }
  .hogskola_aktivitet_cache$df
}

# ---- Examina ----------------------------------------------------------------
.hogskola_examen_cache <- new.env(parent = emptyenv())

rensa_hogskola_examen <- function(rad) {
  rad |>
    dplyr::rename(kommkod = regionkod, kommun = region) |>
    dplyr::mutate(
      ar         = as.integer(ar),
      kommkod    = as.character(kommkod),
      program    = dplyr::coalesce(unname(.sun_bredast[substr(sun2020inr, 1, 1)]),
                                   "Okänd inriktning"),
      examenstyp = dplyr::coalesce(unname(.examenstyp[substr(tmgrp, 1, 1)]),
                                   "Övrig examen")
    ) |>
    dplyr::group_by(ar, kommkod, kommun, geo_niva, examenstyp, program) |>
    dplyr::summarise(antal_examina = sum(antal_examina, na.rm = TRUE), .groups = "drop")
}

hamta_hogskola_examen <- function(force = FALSE) {
  if (force || is.null(.hogskola_examen_cache$df)) {
    .hogskola_examen_cache$df <- rensa_hogskola_examen(.yh_hamta_tabell("hogskola_examen"))
  }
  .hogskola_examen_cache$df
}

# ---- Etablering ---------------------------------------------------------------
# Den summerade tabellen har samma kolumner som yh_uppfoljning, så YH:s
# städning (rensa_yh_etablering) och Riket-serie (yh_etablering_riket)
# återanvänds rakt av.
.hogskola_etablering_cache <- new.env(parent = emptyenv())

hamta_hogskola_etablering <- function(force = FALSE) {
  if (force || is.null(.hogskola_etablering_cache$df)) {
    .hogskola_etablering_cache$df <- rensa_yh_etablering(.yh_hamta_tabell("hogskola_etablering"))
  }
  .hogskola_etablering_cache$df
}
