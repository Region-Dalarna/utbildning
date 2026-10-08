# ============================================================
#  func_data_hogskola.R
#  Dataåtkomst för högskolestatistik.
#
#  Källor (oppna_data.mikro_db), regionkod = personens BOENDEKOMMUN:
#  - hogskola_aktivitet   deltagare (unika studenter) och kursregistreringar.
#                         granularitet: Totalt, TotaltKon, Larosate,
#                         LarosateKon, Program (år x lärosäte x program x kön).
#  - hogskola_examen      antal examina per lärosäte, huvudområde, examenstyp
#                         och kön (räknar examina, går att summera).
#  - hogskola_etablering  RAKS-etablering 1/3/5 år efter examen.
#                         granularitet: Region (summerat över lärosäten) och
#                         Larosate (riket per lärosäte).
#
#  Två perspektiv i modulen:
#  - Lärosäte: regionkod "00" (alla studenter i landet) + valt hskod.
#  - Dalarnas invånare: regionkod "20" eller vald kommun.
#
#  DUBBELRÄKNING: deltagare får inte summeras över lärosäten eller program
#  (samma person kan finnas på flera). Totaler läses därför från
#  Totalt/TotaltKon resp. LarosateKon, aldrig som summa av finare rader.
#  Summering över kön är säker.
#
#  "program" i diagramfunktionernas mening: programnamn (Program-rader),
#  lärosätesnamn (Larosate-rader) eller ämnesområde (examen/etablering =
#  SUN 2020:s bredaste inriktning).
# ============================================================

# SUN 2020, bredaste inriktningsnivå (förstasiffran i inriktningskoden),
# benämningar enligt SCB:s värdemängd för SUN2020Inr_1.
.sun_bredast <- c(
  "0" = "Allmän utbildning",
  "1" = "Pedagogik och lärarutbildning",
  "2" = "Humaniora och konst",
  "3" = "Samhällsvetenskap, juridik, handel, administration",
  "4" = "Naturvetenskap, matematik och informations- och kommunikationsteknik (IKT)",
  "5" = "Teknik och tillverkning",
  "6" = "Lant- och skogsbruk samt djursjukvård",
  "7" = "Hälso- och sjukvård samt social omsorg",
  "8" = "Tjänster",
  "9" = "Okänd"
)

# Examenstyp ur första bokstaven i tmgrp (G = generell, K = konstnärlig,
# Y/P = yrkesexamen).
.examenstyp <- c("G" = "Generell examen", "K" = "Konstnärlig examen",
                 "Y" = "Yrkesexamen", "P" = "Yrkesexamen")

# Högskolan Dalarna - förvalt lärosäte.
HOGSKOLA_STANDARD_HSKOD <- "007"

.hogskola_cache <- new.env(parent = emptyenv())

.hogskola_tabell <- function(tabell) {
  if (is.null(.hogskola_cache[[tabell]])) .hogskola_cache[[tabell]] <- .yh_hamta_tabell(tabell)
  .hogskola_cache[[tabell]]
}

# ---- Aktivitet (studenter och kursregistreringar) -------------------------
rensa_hogskola_aktivitet <- function(rad) {
  rad |>
    dplyr::rename(kommkod = regionkod, kommun = region) |>
    dplyr::mutate(
      ar      = as.integer(ar),
      kommkod = as.character(kommkod),
      hskod   = as.character(hskod),
      program = dplyr::case_when(
        granularitet == "Program" & !is.na(lprogben) & lprogben != "" ~ lprogben,
        granularitet == "Program" & !is.na(lprog) & lprog != ""       ~ lprog,
        granularitet == "Program"                                     ~ "Fristående kurser",
        TRUE                                                          ~ hskod_namn
      ),
      dplyr::across(c(deltagare, kursregistreringar), as.numeric)
    )
}

hamta_hogskola_aktivitet <- function() {
  if (is.null(.hogskola_cache$aktivitet))
    .hogskola_cache$aktivitet <- rensa_hogskola_aktivitet(.hogskola_tabell("hogskola_aktivitet"))
  .hogskola_cache$aktivitet
}

# Lärosäten att välja mellan i lärosätesperspektivet, störst först.
hogskola_larosaten <- function() {
  d <- hamta_hogskola_aktivitet() |>
    dplyr::filter(granularitet == "Larosate", kommkod == "00")
  d |>
    dplyr::filter(ar == max(ar)) |>
    dplyr::arrange(dplyr::desc(deltagare)) |>
    dplyr::distinct(hskod, hskod_namn)
}

# ---- Examina ----------------------------------------------------------------
rensa_hogskola_examen <- function(rad) {
  rad |>
    dplyr::rename(kommkod = regionkod, kommun = region) |>
    dplyr::mutate(
      ar         = as.integer(ar),
      kommkod    = as.character(kommkod),
      hskod      = as.character(hskod),
      program    = dplyr::coalesce(unname(.sun_bredast[substr(sun2020inr, 1, 1)]), "Okänd"),
      examenstyp = dplyr::coalesce(unname(.examenstyp[substr(tmgrp, 1, 1)]), "Övrig examen")
    ) |>
    dplyr::group_by(ar, kommkod, kommun, geo_niva, hskod, hskod_namn, examenstyp, program, kon) |>
    dplyr::summarise(antal_examina = sum(antal_examina, na.rm = TRUE), .groups = "drop")
}

hamta_hogskola_examen <- function() {
  if (is.null(.hogskola_cache$examen))
    .hogskola_cache$examen <- rensa_hogskola_examen(.hogskola_tabell("hogskola_examen"))
  .hogskola_cache$examen
}

# ---- Etablering ---------------------------------------------------------------
# Samma kolumner som yh_uppfoljning, så YH:s städning (rensa_yh_etablering)
# och Riket-serie (yh_etablering_riket) återanvänds.
# hskod = NULL -> invånarperspektivet (Region-raderna), annars riksraderna
# för det lärosätet (Larosate-raderna).
hamta_hogskola_etablering <- function(hskod = NULL) {
  rad <- .hogskola_tabell("hogskola_etablering")
  if ("granularitet" %in% names(rad)) {
    rad <- if (is.null(hskod)) {
      dplyr::filter(rad, granularitet == "Region")
    } else {
      dplyr::filter(rad, granularitet == "Larosate", .data$hskod == .env$hskod)
    }
  } else if (!is.null(hskod)) {
    rad <- rad[0, ]
  }
  rensa_yh_etablering(rad)
}
