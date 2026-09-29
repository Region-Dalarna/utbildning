# ============================================================
#  hogskola_uppfoljning_till_databas.R
#  Summerar högskoleuppföljningen (RAKS-etablering) så att den kan läsas
#  av Shiny-appen utan individnära rader.
#
#  Källtabellen oppna_data.mikro_db.hogskola_uppfoljning har rader per
#  hemkommun x lärosäte x huvudområdesgrupp - de flesta med 1-3 personer och
#  inkomst per person (forvink_etabl). Den summeras här till
#  region x ämnesområde x kön, med samma kolumner som yh_uppfoljning, och
#  skrivs till oppna_data.mikro_db.hogskola_etablering (läses av
#  R/func_data_hogskola.R).
#
#  Körs manuellt, inte av appen. Kräver att lösenordet för
#  skrivanvändaren finns sparat (shiny_set_password("shiny_skriv")).
#
#  Efteråt: ta bort eller flytta råtabellen från oppna_data (se sist).
# ============================================================

library(dplyr)
library(DBI)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R",
       encoding = "utf-8", echo = FALSE)

# ---- Inställningar ---------------------------------------------------------
db_namn      <- "oppna_data"
schema       <- "mikro_db"
kalltabell   <- "hogskola_uppfoljning"
maltabell    <- "hogskola_etablering"
skriv_till_db <- TRUE     # FALSE = bara summera och kontrollera

# SUN 2020, bredaste inriktning = förstasiffran i huvomgrp. Samma lista som
# .sun_bredast i R/func_data_hogskola.R.
sun_bredast <- c(
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

# ---- Läs och summera -------------------------------------------------------
con <- shiny_uppkoppling_las(db_namn)
ra <- dplyr::tbl(con, dbplyr::in_schema(schema, kalltabell)) |> dplyr::collect()
DBI::dbDisconnect(con)

hogskola_etablering <- ra |>
  dplyr::mutate(
    regionkod = as.character(regionkod),
    # Yrkesexamina saknar huvudområde (huvomgrp = NA).
    utbildningsomrade = dplyr::if_else(
      is.na(huvomgrp),
      "Utan huvudområde (främst yrkesexamina)",
      dplyr::coalesce(unname(sun_bredast[substr(as.character(huvomgrp), 1, 1)]),
                      "Okänd inriktning")),
    dplyr::across(c(antal_ar, syss, stud, arblos, ovriga, antal, etabl), as.integer),
    forvink_etabl = as.numeric(forvink_etabl),
    # Inkomst saknas för en del etablerade - håll reda på hur många som har
    # inkomst, så att medelinkomst = inkomst_summa / etabl_med_inkomst.
    etabl_med_inkomst = dplyr::if_else(is.na(forvink_etabl), 0L, etabl)
  ) |>
  dplyr::group_by(exam_ar_interval, uppf_ar_interval, antal_ar,
                  regionkod, region, geo_niva, utbildningsomrade, kon) |>
  dplyr::summarise(
    antal  = sum(antal),  syss   = sum(syss),   stud   = sum(stud),
    arblos = sum(arblos), ovriga = sum(ovriga), etabl  = sum(etabl),
    etabl_med_inkomst = sum(etabl_med_inkomst),
    inkomst_summa     = sum(forvink_etabl, na.rm = TRUE),   # summa, INTE medel
    .groups = "drop")

# ---- Kontroller ------------------------------------------------------------
# Kommunraderna ska summera till länsraden, annars dubbelräknas något.
kontroll <- hogskola_etablering |>
  dplyr::filter(geo_niva != "riket") |>
  dplyr::group_by(antal_ar, exam_ar_interval, geo_niva) |>
  dplyr::summarise(antal = sum(antal), .groups = "drop") |>
  tidyr::pivot_wider(names_from = geo_niva, values_from = antal)
print(kontroll)
if (any(kontroll$lan != kontroll$kommun))
  warning("Kommunraderna summerar inte till länet - kontrollera innan appen används.")

message("Rader: ", nrow(ra), " -> ", nrow(hogskola_etablering),
        ". Rader med högst 3 personer efter summering: ",
        sum(hogskola_etablering$antal <= 3))

# ---- Skriv till databasen --------------------------------------------------
# Finns tabellen töms den och fylls på igen (TRUNCATE + append), så att
# behörigheterna för läsanvändaren (shiny_las) ligger kvar.
if (skriv_till_db) {
  con <- shiny_uppkoppling_skriv(db_name = db_namn)
  if (is.null(con)) stop("Kunde inte koppla upp mot databasen.")
  id <- DBI::Id(schema = schema, table = maltabell)
  DBI::dbWithTransaction(con, {
    if (DBI::dbExistsTable(con, id)) {
      DBI::dbExecute(con, paste0("TRUNCATE TABLE ", schema, ".", maltabell, ";"))
      DBI::dbWriteTable(con, id, hogskola_etablering, append = TRUE)
    } else {
      DBI::dbWriteTable(con, id, hogskola_etablering)
      DBI::dbExecute(con, paste0("GRANT SELECT ON ", schema, ".", maltabell, " TO shiny_las;"))
    }
  })
  message(schema, ".", maltabell, ": ", nrow(hogskola_etablering), " rader skrivna.")

  # Råtabellen har individnära rader (inkomst per person) och bör inte ligga
  # i oppna_data. Flytta den till sekretess-databasen och ta sedan bort den här:
  # DBI::dbExecute(con, paste0("DROP TABLE ", schema, ".", kalltabell, ";"))
  DBI::dbDisconnect(con)
}
