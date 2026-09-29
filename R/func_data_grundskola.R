# ============================================================
#  func_data_grundskola.R
#  Dataåtkomst för grundskolans slutbetyg åk 9 (gymnasiebehörighet,
#  ämnesbetyg, meritvärde).
#
#  Källa: oppna_data.mikro_db.grundskola_slutbetyg
#
#  "program" i diagramfunktionernas mening = kön, eftersom datan saknar en
#  program-/ämnesdimension. Andelarna räknas här som procenttal och vägs
#  i diagrammen med sitt underlag (skapa_diagram_bar_andel()/trend_andel()),
#  samma mönster som gymnasiets viktade andelar. Meritvärde vägs på samma
#  sätt med antal elever med meritvärde.
#
#  Rader där måttet saknas (NA) får underlag 0, annars räknas eleverna in
#  i nämnaren men inte i täljaren och drar ner snittet.
# ============================================================

.grundskola_cache <- new.env(parent = emptyenv())

# Andel i procent + underlag, med underlag 0 där andelen inte går att räkna.
.andel_och_underlag <- function(tal, ej) {
  underlag <- dplyr::coalesce(tal, 0) + dplyr::coalesce(ej, 0)
  underlag <- dplyr::if_else(is.na(tal) | is.na(ej), 0, underlag)
  list(andel = dplyr::if_else(underlag > 0, 100 * tal / underlag, NA_real_),
       underlag = underlag)
}

rensa_grundskola_data <- function(rad) {
  d <- rad |>
    dplyr::rename(kommkod = regionkod, kommun = region, program = kon) |>
    dplyr::mutate(ar = as.integer(ar), kommkod = as.character(kommkod))

  # En eventuell totalrad för kön (NA/"Totalt"/"Samtliga") skulle bli en egen
  # stapel och dubbelräknas i trenden - behåll bara könsraderna.
  d <- dplyr::filter(d, !is.na(program),
                     !grepl("^(tot|samtliga|alla)", program, ignore.case = TRUE))

  beh <- .andel_och_underlag(d$behorig,    d$ej_behorig)
  en  <- .andel_och_underlag(d$en_godkant,  d$en_icke_godkant)
  ma  <- .andel_och_underlag(d$ma_godkant,  d$ma_icke_godkant)
  sv  <- .andel_och_underlag(d$sv_godkant,  d$sv_icke_godkant)
  sva <- .andel_och_underlag(d$sva_godkant, d$sva_icke_godkant)

  d |>
    dplyr::mutate(
      andel_behorig     = beh$andel, behorig_underlag = beh$underlag,
      andel_en_godkant  = en$andel,  en_underlag      = en$underlag,
      andel_ma_godkant  = ma$andel,  ma_underlag      = ma$underlag,
      andel_sv_godkant  = sv$andel,  sv_underlag      = sv$underlag,
      andel_sva_godkant = sva$andel, sva_underlag     = sva$underlag,
      meritvarde_underlag = dplyr::if_else(is.na(meritvarde_medel), 0,
                                           as.numeric(dplyr::coalesce(antal_med_meritvarde, 0L)))
    )
}

hamta_grundskola_data <- function(force = FALSE) {
  if (force || is.null(.grundskola_cache$df)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("mikro_db", "grundskola_slutbetyg")) |>
      dplyr::collect()
    DBI::dbDisconnect(con)
    .grundskola_cache$df <- rensa_grundskola_data(rad)
  }
  .grundskola_cache$df
}
