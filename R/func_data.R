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

# ============================================================
#  Elever (Skolverket) – långt format som görs brett
# ============================================================
.elever_cache <- new.env(parent = emptyenv())

# Klassar gymnasieprogram-värdet i nivå, eftersom Skolverkets kolumn blandar
# totaler, programtyper, enskilda program och introduktionsundertyper.
# Tvättar elevtabellen: behåller Dalarnas kommunrader, härleder år, gör bred,
# döper om mätkolumnerna, och sätter nivå + programtyp (programtyp tas ur
# datans egna aggregatrader, inte via join mot antagningsdatan).
rensa_elevdata <- function(rad) {
  byt <- c(
    "Antal elever"                 = "antal_elever",
    "Andel kvinnor (%)"            = "andel_kvinnor",
    "Andel m utl bakgr (%)"        = "andel_utl",
    "Andel m högutb föräldrar (%)" = "andel_hogutb",
    "Antal elever skolår 1"        = "elever_ak1",
    "Antal elever skolår 2"        = "elever_ak2",
    "Antal elever skolår 3"        = "elever_ak3"
  )

  # Steg 1: Rensa och filtrera rådata till Dalarnas kommunrader.
  # "Samtliga" är en förberäknad totalsumma och tas bort för att undvika
  # dubbelräkning när vi summerar Kommunal + Enskild + eventuella andra.
  rad_fil <- rad |>
    dplyr::rename(program = gymnasieprogram, kommkod = regionkod,
                  kommun = region, organisationstyp = huvudman) |>
    dplyr::mutate(
      kommkod = as.character(kommkod),
      ar      = as.integer(substr(as.character(lasar), 1, 4)),
      varde   = as.numeric(varde)
    ) |>
    dplyr::filter(
      nchar(kommkod) == 4,
      substr(kommkod, 1, 2) == "20",
      organisationstyp != "Samtliga"
    )

  # Steg 2: Summera per (ar, kommkod, program, organisationstyp, variabel).
  # organisationstyp behålls så att driftsformsfiltret i appen fungerar precis
  # som för antagningsdata (Kommunal / Enskild / Alla).
  antal_var  <- "Antal elever"
  andel_vars <- c("Andel kvinnor (%)", "Andel m utl bakgr (%)",
                  "Andel m högutb föräldrar (%)")
  skolar_vars <- c("Antal elever skolår 1", "Antal elever skolår 2",
                   "Antal elever skolår 3")

  # Antal och skolår summeras per driftsform.
  antal_df <- rad_fil |>
    dplyr::filter(variabel == antal_var) |>
    dplyr::group_by(ar, kommkod, kommun, program, organisationstyp) |>
    dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(variabel = antal_var)

  skolar_df <- rad_fil |>
    dplyr::filter(variabel %in% skolar_vars) |>
    dplyr::group_by(ar, kommkod, kommun, program, organisationstyp, variabel) |>
    dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop")

  # Andelar viktas med antal elever per driftsform.
  andel_df <- rad_fil |>
    dplyr::filter(variabel %in% andel_vars) |>
    dplyr::left_join(
      dplyr::select(antal_df, ar, kommkod, program, organisationstyp, antal = varde),
      by = c("ar", "kommkod", "program", "organisationstyp")) |>
    dplyr::group_by(ar, kommkod, kommun, program, organisationstyp, variabel) |>
    dplyr::summarise(
      varde = dplyr::if_else(
        sum(antal, na.rm = TRUE) > 0,
        sum(varde * antal, na.rm = TRUE) / sum(antal, na.rm = TRUE),
        NA_real_),
      .groups = "drop")

  df <- dplyr::bind_rows(antal_df, skolar_df, andel_df) |>
    tidyr::pivot_wider(names_from = variabel, values_from = varde)

  # Döp om mätkolumnerna till syntaktiska namn (de som finns).
  for (gammalt in names(byt)) {
    if (gammalt %in% names(df)) names(df)[names(df) == gammalt] <- byt[[gammalt]]
  }

  df |>
    dplyr::mutate(
      prog_niva = dplyr::case_when(
        program == "Nationella program"                                  ~ "total",
        program %in% c("Högskoleförberedande program", "Yrkesprogram")  ~ "programtyp",
        program %in% c("Introduktionsprogrammen",
                       "Riksrekryterande utbildningar")                  ~ "ovrigt_agg",
        grepl("^Introduktionsprogram,", program)                        ~ "introduktion_sub",
        TRUE                                                            ~ "program"
      ),
      programtyp = dplyr::case_when(
        program == "Högskoleförberedande program" ~ "Högskoleförberedande program",
        program == "Yrkesprogram"                 ~ "Yrkesprogram",
        program %in% c("Introduktionsprogrammen",
                       "Riksrekryterande utbildningar") ~ "Övriga utbildningar",
        TRUE                                      ~ NA_character_
      )
    ) |>
    dplyr::left_join(kommun_samverkan, by = "kommkod")
}

# OBS: byt ut tabellnamnet nedan mot den faktiska elevtabellen i databasen.
hamta_gymnasie_elever <- function(force = FALSE) {
  if (force || is.null(.elever_cache$df)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("skolverket", "gymnasiet_elever")) |>
      dplyr::collect()
    .elever_cache$df <- rensa_elevdata(rad)
  }
  .elever_cache$df
}

# Plockar ut de enskilda programmen (för stapel/trend), bort med aggregat-
# och totalrader. För antagningsdata (som saknar prog_niva) returneras df oförändrad.
elever_endast_program <- function(df) {
  if ("prog_niva" %in% names(df))
    dplyr::filter(df, prog_niva %in% c("program", "introduktion_sub"))
  else df
}

# ============================================================
#  Genomströmning (Skolverket) – andel med examen inom 4 år
#
#  Datan är i långt format med en kolumn per variabel:
#    läsår, regionkod, region, Gymnasieprogram, Typ av huvudman,
#    Genomströmning, andel
#  Kolumnen "Genomströmning" innehåller bara ett värde (beskrivning
#  av måttet) och läses inte in. "andel" är redan färdigberäknad %.
#
#  Riket (regionkod "00") behålls separat för jämförelselinje i trend.
# ============================================================
.genomstromning_cache <- new.env(parent = emptyenv())

rensa_genomstromning <- function(rad) {
  # Kolumnnamnen innehåller mellanslag och svenska tecken – hanteras med backticks.
  rad |>
    dplyr::rename(
      lasar            = `läsår`,
      kommkod          = regionkod,
      kommun           = region,
      program          = gymnasieprogram,
      organisationstyp = `typ av huvudman`
    ) |>
    dplyr::mutate(
      kommkod = as.character(kommkod),
      ar      = as.integer(substr(lasar, 1, 4)),
      andel   = as.numeric(andel)
    ) |>
    # Behåll Dalarnas kommuner (4-siffriga koder 20xx) + Riket (00) + Dalarna
    # som län (20) för ev. framtida bruk. Övriga län filtreras bort.
    dplyr::filter(
      (nchar(kommkod) == 4 & substr(kommkod, 1, 2) == "20") |
        kommkod %in% c("00", "20")
    ) |>
    # Samtliga = aggregerad rad av Kommunal + Enskild. Vi behåller den som
    # "Alla" för totalvyn men tar bort dubbletten när driftsform filtreras.
    dplyr::mutate(
      organisationstyp = dplyr::case_when(
        organisationstyp == "Samtliga" ~ "Alla",
        TRUE                           ~ organisationstyp
      ),
      geo_niva = dplyr::case_when(
        kommkod == "00" ~ "riket",
        kommkod == "20" ~ "lan",
        TRUE            ~ "kommun"
      ),
      prog_niva = dplyr::case_when(
        program %in% c("Gymnasieskolan totalt", "Nationella program",
                       "Riksrekryterande utbildningar")        ~ "total",
        program %in% c("Högskoleförberedande program",
                       "Yrkesprogram", "Introduktionsprogram") ~ "programtyp",
        TRUE                                                   ~ "program"
      ),
      programtyp = dplyr::case_when(
        program == "Högskoleförberedande program" ~ "Högskoleförberedande program",
        program == "Yrkesprogram"                 ~ "Yrkesprogram",
        program == "Introduktionsprogram"         ~ "Övriga utbildningar",
        TRUE                                      ~ NA_character_
      )
    ) |>
    dplyr::left_join(kommun_samverkan, by = "kommkod") |>
    dplyr::select(ar, lasar, kommkod, kommun, samverkansomrade,
                  program, organisationstyp, geo_niva, prog_niva,
                  programtyp, andel)
}

hamta_genomstromning <- function(force = FALSE) {
  if (force || is.null(.genomstromning_cache$df)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("skolverket", "gymnasiet_genomstromning")) |>
      dplyr::collect()
    .genomstromning_cache$df <- rensa_genomstromning(rad)
  }
  .genomstromning_cache$df
}

# Plockar ut enskilda program (för stapel/trend), bort med aggregat.
# geo_niva-filtret: bara kommunrader (inte riket/länet) används för
# Dalarna-beräkningar; riket hanteras separat i diagramfunktionen.
genomstromning_endast_program <- function(df) {
  dplyr::filter(df, prog_niva == "program", geo_niva == "kommun")
}

# Riket-andel för ett givet program och år (för jämförelselinje).
# Returnerar en data.frame med ar + andel_riket.
genomstromning_riket <- function(df_full, program_val = "Nationella program",
                                 org = "Alla") {
  df_full |>
    dplyr::filter(geo_niva == "riket", program == program_val,
                  organisationstyp == org) |>
    dplyr::select(ar, andel_riket = andel)
}


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
