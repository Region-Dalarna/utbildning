# ============================================================
#  func_data.R
#  Dataåtkomst för gymnasiestatistik.
#
#  Källa: databasen "oppna_data", schema "dkf", tabell "gymnasieantagna"
#         (Gymnasieantagningen, Dalarnas kommunförbund).
#
#  hamta_gymnasiedata() returnerar RENSADE kolumnnamn – de icke-syntaktiska
#  DB-namnen (1a_tot, 1a_män, ...) hanteras EN gång i rensa_gymnasiedata().
#  Datan hämtas en gång per R-process och cachas (read-only referensdata).
# ============================================================

# Enkel processcache så tabellen bara hämtas en gång.
.data_cache <- new.env(parent = emptyenv())

# ---- Rensning: råtabell -> appens interna kolumnnamn -----------------------
# Här (och bara här) hanteras de icke-syntaktiska kolumnnamnen via backticks.
# Lägg till fler kolumner när nya indikatorer behöver dem (t.ex. ant_kv/ant_män,
# merit_lägst/merit_median, res_tot, ant_1:a_h).
rensa_gymnasiedata <- function(rad) {
  rad |>
    dplyr::transmute(
      ar                 = as.integer(ar),
      kommkod            = as.character(kommkod),
      kommun             = kommun,
      pr_kod             = pr_kod,
      program            = program,
      program_inriktning = program_inriktning,
      programtyp         = programtyp,
      organisationstyp   = organisationstyp,            # driftsform (kommun/fristående)
      platser            = org,
      sok_1a             = `1a_tot`,
      sok_1a_kv          = `1a_kv`,
      sok_1a_man         = `1a_män`,
      antagna            = ant_tot,
      antagna_kv         = ant_kv,
      antagna_man        = `ant_män`,
      lediga_platser     = led_pl,
      merit_medel        = merit_medel
    ) |>
    # Samverkansområde finns inte i tabellen – joinas på kommkod.
    dplyr::left_join(kommun_samverkan, by = "kommkod")
}

# ---- Publik läsfunktion ----------------------------------------------------
# force = TRUE läser om från databasen (annars används cachen).
hamta_gymnasiedata <- function(force = FALSE) {
  if (force || is.null(.data_cache$df)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("dkf", "gymnasieantagna")) |>
      dplyr::collect()
    # Om shiny_uppkoppling_las() ger en vanlig DBI-anslutning (inte en pool)
    # kan du frigöra den här:
    # DBI::dbDisconnect(con)
    .data_cache$df <- rensa_gymnasiedata(rad)
  }
  .data_cache$df
}

# ---- Excel-export ----------------------------------------------------------
# Enkel men snygg formatering: fet rubrikrad i petrolblått, autobredd på
# kolumner, fryst rubrikrad och autofilter. Kräver paketet openxlsx.
skriv_gymnasie_excel <- function(df, file, blad = "Gymnasiet") {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, blad)

  rubrikstil <- openxlsx::createStyle(
    fontColour = "#ffffff", fgFill = RD_PRIMARY, textDecoration = "bold",
    halign = "left", valign = "center", border = "bottom", borderColour = "#ffffff"
  )
  openxlsx::writeData(wb, blad, df, headerStyle = rubrikstil, withFilter = TRUE)
  openxlsx::setColWidths(wb, blad, cols = seq_along(df), widths = "auto")
  openxlsx::freezePane(wb, blad, firstActiveRow = 2)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}
