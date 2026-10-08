# ============================================================
#  func_data_komvux.R
#  Dataåtkomst för Komvux/SFI-statistik.
#
#  Källa: oppna_data.mikro_db.komvux_sfi_studerande
#
#  Tabellen har flera granulariteter (kolumnen granularitet), bl.a.:
#  - "ArNivaKon": år x utbildningstyp x nivå x kön x område
#  - "ArKon":     år x utbildningstyp x kön x område (unika deltagare)
#  - "Kurs":      år x termin x utbildningstyp x kurs x nivå x kön x huvudman
#  Kursdeltaganden, avbrott och godkänt är rena radräkningar och summerar
#  lika i alla granulariteter. Deltagare (unika individer) får INTE summeras
#  över finare nivåer - de läses från "ArKon" (hamta_komvux_deltagare()).
#  Den som läser både Komvux och SFI räknas i båda när de slås ihop.
#
#  "program" i diagramfunktionernas mening = nivå (klartext enligt SCB:s
#  värdemängd, se .niva_klartext), "SFI" för SFI-raderna (som saknar nivå),
#  eller kurs (hamta_komvux_kurser()). Utbildningstyp (Komvux/SFI) är ett
#  filter i modulen.
#
#  Ingen etablerings-/uppföljningsdel finns än för Komvux/SFI.
# ============================================================

.niva_klartext <- c(
  "0" = "Påbyggnadsnivå", "3" = "Påbyggnadsnivå",
  "1" = "Grundläggande nivå",
  "2" = "Gymnasial nivå",
  "4" = "Okänt värde",
  "5" = "Nordisk elev (SKOLFS 1993:14)",
  "6" = "Prövning"
)

.komvux_matt <- c("kursdeltaganden", "avbrott", "godkant", "icke_godkant",
                  "avslutad_kurs", "pagaende_kurs")

.komvux_cache <- new.env(parent = emptyenv())

.komvux_andelar <- function(df) {
  dplyr::mutate(df,
    andel_godkant = dplyr::if_else(avslutad_kurs > 0, 100 * godkant / avslutad_kurs, NA_real_),
    andel_avbrott = dplyr::if_else(kursdeltaganden > 0, 100 * avbrott / kursdeltaganden, NA_real_))
}

rensa_komvux_data <- function(rad) {
  rad |>
    dplyr::distinct() |>   # uttaget har haft exakta dubbletter bland Kurs-raderna
    dplyr::rename(kommkod = regionkod, kommun = region) |>
    dplyr::mutate(
      ar      = as.integer(ar),
      kommkod = as.character(kommkod),
      niva_txt = as.character(niva),
      program = dplyr::case_when(
        utbildningstyp == "SFI" ~ "SFI",
        niva_txt %in% names(.niva_klartext) ~ unname(.niva_klartext[niva_txt]),
        !is.na(niva_txt) ~ niva_txt,                  # redan i klartext
        TRUE ~ "Okänt värde"
      )
    ) |>
    dplyr::select(-niva_txt)
}

.komvux_ra <- function(force = FALSE) {
  if (force || is.null(.komvux_cache$ra)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("mikro_db", "komvux_sfi_studerande")) |>
      dplyr::collect()
    DBI::dbDisconnect(con)
    .komvux_cache$ra <- rensa_komvux_data(rad)
  }
  .komvux_cache$ra
}

# Kursdeltaganden, avbrott och godkänt per år x område x utbildningstyp x nivå.
hamta_komvux_data <- function(force = FALSE) {
  if (force || is.null(.komvux_cache$df)) {
    ra <- .komvux_ra(force)
    # Äldre tabell utan granularitet: alla rader är redan på nivånivå.
    if ("granularitet" %in% names(ra)) ra <- dplyr::filter(ra, granularitet == "ArNivaKon")
    .komvux_cache$df <- ra |>
      dplyr::group_by(ar, kommkod, kommun, geo_niva, utbildningstyp, program) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(.komvux_matt), ~sum(.x, na.rm = TRUE)),
                       .groups = "drop") |>
      .komvux_andelar()
  }
  .komvux_cache$df
}

# Unika deltagare per år x område x kön ("program" = kön). utbildningstyp =
# "_alla_" summerar Komvux och SFI (en person i båda räknas två gånger).
hamta_komvux_deltagare <- function(utbildningstyp = "_alla_") {
  ra <- .komvux_ra()
  if (!"granularitet" %in% names(ra)) return(ra[0, ])
  d <- dplyr::filter(ra, granularitet == "ArKon")
  if (!is.null(utbildningstyp) && utbildningstyp != "_alla_")
    d <- dplyr::filter(d, .data$utbildningstyp == .env$utbildningstyp)
  d |>
    dplyr::group_by(ar, kommkod, kommun, geo_niva, program = kon) |>
    dplyr::summarise(deltagare = sum(deltagare, na.rm = TRUE), .groups = "drop")
}

# Kursdeltaganden, avbrott och godkänt per kurs ("program" = kursbeskrivning),
# summerat över termin, nivå, kön och huvudman.
hamta_komvux_kurser <- function() {
  if (is.null(.komvux_cache$kurser)) {
    ra <- .komvux_ra()
    if (!"granularitet" %in% names(ra)) return(ra[0, ])
    .komvux_cache$kurser <- ra |>
      dplyr::filter(granularitet == "Kurs") |>
      dplyr::mutate(program = dplyr::coalesce(kursbeskrivning, kurskod, "Okänd kurs")) |>
      dplyr::group_by(ar, kommkod, kommun, geo_niva, utbildningstyp, program) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(.komvux_matt), ~sum(.x, na.rm = TRUE)),
                       .groups = "drop") |>
      .komvux_andelar()
  }
  .komvux_cache$kurser
}
