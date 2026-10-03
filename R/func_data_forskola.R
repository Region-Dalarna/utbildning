# ============================================================
#  func_data_forskola.R
#  Dataåtkomst för förskolan: antal barn 2-5 år och hur många av dem som
#  är inskrivna i förskolan.
#
#  Källa: oppna_data.mikro_db.forskola
#  En rad per år x ålder (2-5) x utländsk bakgrund x kön x område.
#  antal_barn = alla barn i åldern, antal_forskola = inskrivna i förskolan.
#
#  "program" i diagramfunktionernas mening = kön (som i Grundskola).
#  Ålder och utländsk bakgrund är filter i modulen; forskola_summera()
#  summerar bort dem och räknar andelen inskrivna.
# ============================================================

.forskola_cache <- new.env(parent = emptyenv())

rensa_forskola_data <- function(rad) {
  rad |>
    dplyr::rename(kommkod = regionkod, kommun = region, program = kon) |>
    dplyr::mutate(
      ar       = as.integer(ar),
      alder    = as.integer(alder),
      kommkod  = as.character(kommkod),
      bakgrund = dplyr::coalesce(utlsvbakg_namn, "Uppgift saknas"),
      dplyr::across(c(antal_barn, antal_forskola), as.numeric)
    ) |>
    dplyr::select(ar, kommkod, kommun, geo_niva, program, alder, bakgrund,
                  antal_barn, antal_forskola)
}

hamta_forskola_data <- function(force = FALSE) {
  if (force || is.null(.forskola_cache$df)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("mikro_db", "forskola")) |>
      dplyr::collect()
    DBI::dbDisconnect(con)
    .forskola_cache$df <- rensa_forskola_data(rad)
  }
  .forskola_cache$df
}

# Filtrerar på ålder/bakgrund ("_alla_" = alla) och summerar till en rad per
# år x område x kön, med andel inskrivna i procent.
forskola_summera <- function(df, alder = "_alla_", bakgrund = "_alla_") {
  if (!is.null(alder) && alder != "_alla_") df <- dplyr::filter(df, alder == as.integer(.env$alder))
  if (!is.null(bakgrund) && bakgrund != "_alla_") df <- dplyr::filter(df, bakgrund == .env$bakgrund)
  df |>
    dplyr::group_by(ar, kommkod, kommun, geo_niva, program) |>
    dplyr::summarise(antal_barn     = sum(antal_barn, na.rm = TRUE),
                     antal_forskola = sum(antal_forskola, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(andel_inskrivna = dplyr::if_else(antal_barn > 0,
                                                   100 * antal_forskola / antal_barn, NA_real_))
}
