# ============================================================
#  func_data_komvux.R
#  Dataåtkomst för Komvux/SFI-statistik.
#
#  Källa: oppna_data.mikro_db.komvux_sfi_studerande
#
#  "program" i diagramfunktionernas mening = Niva (nivåtext enligt SCB:s
#  värdemängd, se .niva_klartext nedan). SFI saknar Niva (NA) och får
#  "program" = "SFI". Utbildningstyp (Komvux/SFI) är ett eget filter i
#  modulen, inte en diagramdimension.
#
#  OBS: tabellen har en rad per termin (VT/HT) och nivå, som summeras till
#  helår här. Det är rätt för kursdeltaganden/avbrott/godkänt (räknas per
#  kurs), men deltagare (unika individer per termin och nivå) dubbelräknas
#  då för den som läser båda terminerna eller på flera nivåer - därför är
#  indikatorn Deltagare avstängd (klar = FALSE) i mod_komvux.R.
#  Kolumnen kon används inte: tabellen är inte uppdelad på kön.
#
#  Ingen etablerings-/uppföljningsdel finns än för Komvux/SFI (öppen fråga
#  om hur en "kohort" ska definieras för kursbaserad utbildning).
# ============================================================

.niva_klartext <- c(
  "0" = "Påbyggnadsnivå", "3" = "Påbyggnadsnivå",
  "1" = "Grundläggande nivå",
  "2" = "Gymnasial nivå",
  "4" = "Okänt värde",
  "5" = "Nordisk elev (SKOLFS 1993:14)",
  "6" = "Prövning"
)

.komvux_cache <- new.env(parent = emptyenv())

rensa_komvux_data <- function(rad) {
  rad |>
    dplyr::rename(
      kommkod = regionkod,
      kommun  = region
    ) |>
    dplyr::mutate(
      ar      = as.integer(ar),
      kommkod = as.character(kommkod),
      program = dplyr::case_when(
        utbildningstyp == "SFI" ~ "SFI",
        TRUE ~ dplyr::coalesce(unname(.niva_klartext[as.character(niva)]), "Okänt värde")
      )
    ) |>
    dplyr::group_by(ar, kommkod, kommun, geo_niva, utbildningstyp, program) |>
    dplyr::summarise(
      dplyr::across(c(kursdeltaganden, deltagare, avbrott, godkant, icke_godkant,
                      avslutad_kurs, pagaende_kurs),
                    ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      andel_godkant = dplyr::if_else(avslutad_kurs > 0, 100 * godkant / avslutad_kurs, NA_real_),
      andel_avbrott = dplyr::if_else(kursdeltaganden > 0, 100 * avbrott / kursdeltaganden, NA_real_)
    )
}

hamta_komvux_data <- function(force = FALSE) {
  if (force || is.null(.komvux_cache$df)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("mikro_db", "komvux_sfi_studerande")) |>
      dplyr::collect()
    DBI::dbDisconnect(con)
    .komvux_cache$df <- rensa_komvux_data(rad)
  }
  .komvux_cache$df
}
