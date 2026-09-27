# ============================================================
#  yh_till_databas.R
#  Läser in YH-filerna (csv), städar dem och skriver till databasen
#  så att Shiny-appen kan läsa dem därifrån (på samma sätt som
#  gymnasiedatan i dkf.gymnasieantagna).
#
#  Körs manuellt, inte av appen. Kräver att lösenordet för
#  skrivanvändaren finns sparat (shiny_set_password("shiny_skriv")).
#
#  Tabeller som skapas i oppna_data.mikro_db (läses av R/func_data_yh.R):
#    yh_studerande            studerande per år, område, studieform, kön
#    yh_uppfoljning           sysselsättning/etablering 1, 3 och 5 år efter
#                             examen, aggregerat per region (utan hem-/arbetskommun)
#    yh_uppfoljning_arbetsort samma uppföljning, men per arbetskommun
#                             (var de sysselsatta jobbar)
#    yh_genomstromning        genomströmning per startår. Första uttaget
#                             (2026-09) var trasigt (avslutade = påbörjade på
#                             alla rader) - skriptet stoppar om det ser ut så.
# ============================================================

library(dplyr)
library(readr)
library(DBI)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R",
       encoding = "utf-8", echo = FALSE)

# ---- Inställningar ---------------------------------------------------------
mapp                  <- "G:/Samhällsanalys/Statistik/YH"   # ändra till mappen med csv-filerna
db_namn               <- "oppna_data"
schema                <- "mikro_db"
skriv_till_db         <- TRUE     # FALSE = bara städa och kontrollera
ta_med_genomstromning <- TRUE     # stoppar med fel om filen ser trasig ut

las_csv <- function(fil) {
  readr::read_csv(file.path(mapp, fil),
                  col_types = readr::cols(.default = "c"),   # allt som text först,
                  na = c("", "NA"),                          # så att koder behåller
                  locale = readr::locale(encoding = "UTF-8"))# inledande nollor
}

# Gemensamma kolumner för alla tabeller: region (riket/län/kommun) och kön.
rensa_gemensamt <- function(df) {
  df |>
    dplyr::rename(utbildningsomrade = UtbildningsOmrade_Utb) |>
    dplyr::mutate(
      regionkod = as.character(regionkod),
      geo_niva  = dplyr::case_when(
        regionkod == "00"     ~ "riket",
        nchar(regionkod) == 2 ~ "lan",
        TRUE                  ~ "kommun"),
      kon = Kon_namn
    ) |>
    dplyr::select(-Kon, -Kon_namn)
}

# ---- Studerande ------------------------------------------------------------
yh_studerande <- las_csv("yh_studerande.csv") |>
  rensa_gemensamt() |>
  dplyr::transmute(
    ar = as.integer(ar), regionkod, region, geo_niva,
    utbildningsomrade, studieform = StudieForm_Utb, kon,
    pagaende             = as.integer(pagaende),
    paborjad             = as.integer(`pabörjad`),
    avslutad             = as.integer(avslutad),
    examen               = as.integer(examen),
    avslutad_utan_examen = as.integer(avslutad_utan_examen)
  )
# OBS: avslutad är ibland större än examen + avslutad_utan_examen (främst
# riksrader). Använd avslutad som nämnare när andel med examen räknas.

# ---- Uppföljning -----------------------------------------------------------
uppf_ra <- las_csv("yh_uppfoljning_raks.csv") |>
  rensa_gemensamt() |>
  dplyr::mutate(
    dplyr::across(c(antal_ar, syss, stud, arblos, ovriga, antal, etabl), as.integer),
    ForvInk_etabl = as.numeric(ForvInk_etabl),
    # Inkomsten saknas för en del etablerade. Håll reda på hur många som har
    # inkomst, så att medelinkomst = inkomst_summa / etabl_med_inkomst.
    etabl_med_inkomst = dplyr::if_else(is.na(ForvInk_etabl), 0L, etabl)
  )

uppf_grupp <- c("exam_ar_interval", "uppf_ar_interval", "antal_ar",
                "regionkod", "region", "geo_niva", "utbildningsomrade", "kon")

summera_uppf <- function(df) {
  df |>
    dplyr::summarise(
      antal  = sum(antal),  syss   = sum(syss),   stud   = sum(stud),
      arblos = sum(arblos), ovriga = sum(ovriga), etabl  = sum(etabl),
      etabl_med_inkomst = sum(etabl_med_inkomst),
      inkomst_summa     = sum(ForvInk_etabl, na.rm = TRUE),  # summa, INTE medel
      .groups = "drop")
}

# Raderna i filen är per hemkommun x arbetskommun (ofta antal = 1) – summeras
# bort här. Använd antal som nämnare: syss+stud+arblos+ovriga stämmer inte
# alltid med antal.
yh_uppfoljning <- uppf_ra |>
  dplyr::group_by(dplyr::across(dplyr::all_of(uppf_grupp))) |>
  summera_uppf()

# Per arbetskommun (bara Dalarna-raderna har kommunuppgifter).
yh_uppfoljning_arbetsort <- uppf_ra |>
  dplyr::filter(geo_niva != "riket") |>
  dplyr::mutate(
    ast_kommunkod = AstKommun,
    ast_kommun    = dplyr::coalesce(AstKommun_namn, "Ingen arbetsplats"),
    ast_i_dalarna = substr(AstKommun, 1, 2) == "20"
  ) |>
  dplyr::group_by(dplyr::across(dplyr::all_of(
    c(uppf_grupp, "ast_kommunkod", "ast_kommun", "ast_i_dalarna")))) |>
  summera_uppf()

# ---- Genomströmning --------------------------------------------------------
if (ta_med_genomstromning) {
  yh_genomstromning <- las_csv("yh_genomstromning.csv") |>
    rensa_gemensamt() |>
    dplyr::transmute(
      startar = as.integer(startar), regionkod, region, geo_niva,
      utbildningsomrade, kon,
      paborjade            = as.integer(paborjade),
      avslutade            = as.integer(avslutade),
      examen               = as.integer(examen),
      avslutad_utan_examen = as.integer(avslutad_utan_examen))

  if (all(yh_genomstromning$avslutade == yh_genomstromning$paborjade) ||
      sum(yh_genomstromning$examen[yh_genomstromning$geo_niva != "riket"]) == 0)
    stop("yh_genomstromning ser fortfarande trasig ut (avslutade = påbörjade ",
         "eller ingen examen i Dalarna). Kontrollera uttaget.")
}

# ---- Kontroller ------------------------------------------------------------
# Kommunraderna ska summera till länsraden, annars dubbelräknas något.
kontrollera_lan <- function(df, grupp, varde) {
  x <- df |>
    dplyr::filter(geo_niva != "riket") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(grupp, "geo_niva")))) |>
    dplyr::summarise(v = sum(.data[[varde]]), .groups = "drop") |>
    tidyr::pivot_wider(names_from = geo_niva, values_from = v)
  fel <- dplyr::filter(x, lan != kommun)
  if (nrow(fel) > 0) {
    print(fel)
    stop("Kommunraderna summerar inte till länet för ", varde, ".")
  }
  invisible(TRUE)
}
kontrollera_lan(yh_studerande,  "ar",       "pagaende")
kontrollera_lan(yh_uppfoljning, "antal_ar", "antal")

# ---- Skriv till databasen --------------------------------------------------
# Finns tabellen töms den och fylls på igen (TRUNCATE + append), så att
# behörigheterna för läsanvändaren (shiny_las) ligger kvar.
skriv_tabell <- function(con, df, tabell) {
  id <- DBI::Id(schema = schema, table = tabell)
  DBI::dbWithTransaction(con, {
    if (DBI::dbExistsTable(con, id)) {
      DBI::dbExecute(con, paste0("TRUNCATE TABLE ", schema, ".", tabell, ";"))
      DBI::dbWriteTable(con, id, df, append = TRUE)
    } else {
      DBI::dbWriteTable(con, id, df)
      DBI::dbExecute(con, paste0("GRANT SELECT ON ", schema, ".", tabell,
                                 " TO shiny_las;"))
    }
  })
  message(schema, ".", tabell, ": ", nrow(df), " rader skrivna.")
}

tabeller <- list(
  yh_studerande            = yh_studerande,
  yh_uppfoljning           = yh_uppfoljning,
  yh_uppfoljning_arbetsort = yh_uppfoljning_arbetsort)
if (ta_med_genomstromning) tabeller$yh_genomstromning <- yh_genomstromning

if (skriv_till_db) {
  con <- shiny_uppkoppling_skriv(db_name = db_namn)
  if (is.null(con)) stop("Kunde inte koppla upp mot databasen.")
  DBI::dbExecute(con, paste0("CREATE SCHEMA IF NOT EXISTS ", schema, ";"))
  DBI::dbExecute(con, paste0("GRANT USAGE ON SCHEMA ", schema, " TO shiny_las;"))
  for (t in names(tabeller)) skriv_tabell(con, tabeller[[t]], t)
  DBI::dbDisconnect(con)
} else {
  for (t in names(tabeller)) message(t, ": ", nrow(tabeller[[t]]), " rader (ej skrivna).")
}
