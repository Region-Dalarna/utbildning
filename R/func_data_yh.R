# ============================================================
#  func_data_yh.R
#  Dataåtkomst för YH-statistik (Yrkeshögskola).
#
#  Källor (alla i oppna_data.mikro_db, skrivna av ett separat inläsningsskript):
#  - yh_studerande        påbörjade/pågående/examen per år
#  - yh_genomstromning    utfall per startår: examen, utan examen, pågående
#  - yh_uppfoljning       RAKS-etablering 1/3/5 år efter examen, ett
#                         examensår i taget
#
#  GRANULARITET: tabellerna har två nivåer i samma tabell - "Omrade" (per
#  utbildningsområde) och "Utbildning" (per område och utbildningsnamn).
#  Filtrera ALLTID på en nivå (yh_niva()); summeras båda räknas allt dubbelt.
#  Områdets egna siffror tas från Omrade-raderna, inte som summan av
#  utbildningarna (små utbildningsceller döljs, då blir summan för låg).
#
#  Tabellerna har en korrekt geo_niva (riket/lan/kommun) och en färdig
#  länsrad (regionkod "20"). Kön behålls (kolumnen kon, Kvinna/Man) - det
#  som inte delas på kön summeras över den. Studieform och Match summeras ihop.
#  "program" = utbildningsområde (diagramfunktionernas kolumnkontrakt).
# ============================================================

# Genomströmning: andel pågående (pagaende_kvar / paborjade) som avgör om en
# startårgång räknas som färdig. Upp till VARNING visas normalt, upp till
# DOLJ med varning, över DOLJ döljs om inte användaren väljer att visa.
YH_PAGAENDE_VARNING <- 5
YH_PAGAENDE_DOLJ    <- 50

# Etablering: uppföljningsåret får inte vara senare än sista året med
# inkomstuppgifter. Från uppföljningsår 2020 bygger utfallet på en annan
# klassificering (HuvInkKalla/ArbInkNiva i stället för RAKS) - brott i serien.
YH_SISTA_UPPF_AR     <- 2023
YH_DEFINITIONSBYTE   <- 2020

.yh_hamta_tabell <- function(tabell) {
  con <- shiny_uppkoppling_las("oppna_data")
  rad <- dplyr::tbl(con, dbplyr::in_schema("mikro_db", tabell)) |>
    dplyr::collect()
  DBI::dbDisconnect(con)
  rad
}

# Äldre tabeller utan granularitet räknas som områdesnivå.
.yh_granularitet <- function(rad) {
  if (!"granularitet" %in% names(rad)) rad$granularitet <- "Omrade"
  if (!"utbildningsnamn" %in% names(rad)) rad$utbildningsnamn <- NA_character_
  rad
}

# Rader för en nivå: områdesnivån (omrade = NULL) eller utbildningarna i ett
# område, med "program" = utbildningsnamn så att diagramfunktionerna visar dem.
yh_niva <- function(d, omrade = NULL) {
  if (is.null(omrade)) return(dplyr::filter(d, granularitet == "Omrade"))
  d <- d |>
    dplyr::filter(granularitet == "Utbildning", program == omrade) |>
    dplyr::mutate(program = dplyr::coalesce(utbildningsnamn, "Okänd utbildning"))
  if ("namn" %in% names(d)) d$namn <- d$program
  d
}

# Kvinnor och män i var sin kolumn (m_kv, m_man) för de könsuppdelade
# diagramfunktionerna (skapa_diagram_bar_kon() m.fl.).
yh_kon_bred <- function(d, metrik) {
  dplyr::mutate(d,
    m_kv  = dplyr::if_else(kon %in% "Kvinna", as.numeric(.data[[metrik]]), 0),
    m_man = dplyr::if_else(kon %in% "Man",    as.numeric(.data[[metrik]]), 0))
}
.YH_KON_ETIKETT <- c("Kvinna" = "Kvinnor", "Man" = "Män")

# ---- Studerande --------------------------------------------------------
.yh_studerande_cache <- new.env(parent = emptyenv())

rensa_yh_studerande <- function(rad) {
  .yh_granularitet(rad) |>
    dplyr::rename(
      kommkod = regionkod,
      kommun  = region,
      program = utbildningsomrade
    ) |>
    dplyr::mutate(ar = as.integer(ar), kommkod = as.character(kommkod)) |>
    dplyr::group_by(dplyr::across(dplyr::any_of(c("ar", "kommkod", "kommun", "geo_niva", "granularitet",
                                                  "program", "utbildningsnamn", "kon")))) |>
    dplyr::summarise(
      dplyr::across(dplyr::any_of(c("paborjad", "pagaende", "avslutad", "examen",
                                    "avslutad_utan_examen", "resultat_saknas")),
                    ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

hamta_yh_studerande <- function(force = FALSE) {
  if (force || is.null(.yh_studerande_cache$df)) {
    .yh_studerande_cache$df <- rensa_yh_studerande(.yh_hamta_tabell("yh_studerande"))
  }
  .yh_studerande_cache$df
}

# ---- Genomströmning -----------------------------------------------------
.yh_genomstromning_cache <- new.env(parent = emptyenv())

rensa_yh_genomstromning <- function(rad) {
  rad <- .yh_granularitet(rad)
  for (k in setdiff(c("resultat_saknas", "pagaende_kvar"), names(rad))) rad[[k]] <- 0
  rad |>
    dplyr::rename(
      kommkod = regionkod,
      kommun  = region,
      ar      = startar,
      program = utbildningsomrade
    ) |>
    dplyr::mutate(ar = as.integer(ar), kommkod = as.character(kommkod)) |>
    dplyr::group_by(dplyr::across(dplyr::any_of(c("ar", "kommkod", "kommun", "geo_niva", "granularitet",
                                                  "program", "utbildningsnamn", "kon")))) |>
    dplyr::summarise(
      dplyr::across(c(paborjade, avslutade, examen, avslutad_utan_examen,
                      resultat_saknas, pagaende_kvar),
                    ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

hamta_yh_genomstromning <- function(force = FALSE) {
  if (force || is.null(.yh_genomstromning_cache$df)) {
    .yh_genomstromning_cache$df <- rensa_yh_genomstromning(.yh_hamta_tabell("yh_genomstromning"))
  }
  .yh_genomstromning_cache$df
}

# Hur färdig en startårgång är, utifrån andel pågående (%).
yh_arsstatus <- function(andel_pagaende) {
  dplyr::case_when(
    is.na(andel_pagaende)                  ~ NA_character_,
    andel_pagaende <= YH_PAGAENDE_VARNING  ~ "komplett",
    andel_pagaende <= YH_PAGAENDE_DOLJ     ~ "varning",
    TRUE                                   ~ "ofullstandig")
}

# Summerar valda rader per år (och ev. grupp) till andel med examen
# (examen / avslutade), andel pågående (pagaende_kvar / paborjade) och
# årgångsstatus. Komplettheten räknas alltså för just det urvalet.
yh_genomstromning_summera <- function(df, grupp = NULL) {
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("ar", grupp)))) |>
    dplyr::summarise(dplyr::across(c(paborjade, avslutade, examen, pagaende_kvar),
                                   ~sum(.x, na.rm = TRUE)), .groups = "drop") |>
    dplyr::mutate(
      andel          = dplyr::if_else(avslutade > 0, 100 * examen / avslutade, NA_real_),
      andel_pagaende = dplyr::if_else(paborjade > 0, 100 * pagaende_kvar / paborjade, NA_real_),
      status         = yh_arsstatus(andel_pagaende))
}

# Riksrader på samma nivå som urvalet: hela riket (områdesnivå), ett område
# (dess Omrade-rader) eller en utbildning i området.
yh_riket_rader <- function(df_full, omrade = NULL, utbildning = NULL) {
  d <- yh_niva(dplyr::filter(df_full, geo_niva == "riket"), if (!is.null(utbildning)) omrade)
  if (!is.null(utbildning)) dplyr::filter(d, program == utbildning)
  else if (!is.null(omrade)) dplyr::filter(d, program == omrade)
  else d
}

# Riket-serie för jämförelselinjen (se yh_riket_rader()).
genomstromning_riket_yh <- function(df_full, omrade = NULL, utbildning = NULL) {
  yh_genomstromning_summera(yh_riket_rader(df_full, omrade, utbildning))
}

# ---- Etablering (SCB/RAKS) ------------------------------------------------
# Samma kolumnkontrakt/enheter som gymnasiets hamta_etablering():
# syss/stud/arblos/ovriga/etabl/antal är ANTAL (inte andelar), aggregering
# sker i modulen via summering. inkomst_summa är en SUMMA - medelinkomst =
# inkomst_summa / etabl_med_inkomst.
.yh_etablering_cache <- new.env(parent = emptyenv())

# YH: ett examensår per rad (exam_ar_interval "2013-2013"); ar = examensåret.
# Summeras över kön och Match (yrkesmatchning, ingen indikator än).
# Används även för hogskola_etablering (samma kolumner).
rensa_yh_etablering <- function(rad) {
  .yh_granularitet(rad) |>
    dplyr::rename(
      kommkod = regionkod,
      kommun  = region,
      program = utbildningsomrade
    ) |>
    dplyr::mutate(
      kommkod  = as.character(kommkod),
      ar       = as.integer(substr(exam_ar_interval, 1, 4)),
      antal_ar = as.integer(antal_ar)
    ) |>
    dplyr::group_by(dplyr::across(dplyr::any_of(c(
      "ar", "exam_ar_interval", "uppf_ar_interval", "antal_ar", "kommkod", "kommun", "geo_niva",
      "granularitet", "program", "utbildningsnamn", "kon")))) |>
    dplyr::summarise(
      dplyr::across(c(syss, stud, arblos, ovriga, antal, etabl,
                      etabl_med_inkomst, inkomst_summa),
                    ~sum(as.numeric(.x), na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(namn = program)   # ingen inriktnings-underindelning för YH
}

hamta_yh_etablering <- function(force = FALSE) {
  if (force || is.null(.yh_etablering_cache$df)) {
    # Uppföljningsår efter YH_SISTA_UPPF_AR saknar inkomstuppgifter - tas bort.
    .yh_etablering_cache$df <- rensa_yh_etablering(.yh_hamta_tabell("yh_uppfoljning")) |>
      dplyr::filter(ar + antal_ar <= YH_SISTA_UPPF_AR)
  }
  .yh_etablering_cache$df
}

# ---- Arbetsort ------------------------------------------------------------------
# yh_uppfoljning_arbetsort: var de utexaminerade har sitt arbetsställe
# (ast_kommun, ast_i_dalarna), per studieort (regionkod), examensår,
# uppföljningstid, utbildningsområde och kön. Bara områdesnivå. Alla
# utexaminerade finns med (antal = samma som i yh_uppfoljning).
.yh_arbetsort_cache <- new.env(parent = emptyenv())

rensa_yh_arbetsort <- function(rad) {
  rad |>
    dplyr::rename(kommkod = regionkod, kommun = region, program = utbildningsomrade) |>
    dplyr::mutate(
      kommkod       = as.character(kommkod),
      ar            = as.integer(substr(exam_ar_interval, 1, 4)),
      antal_ar      = as.integer(antal_ar),
      ast_kommunkod = as.character(ast_kommunkod),
      ast_kommun    = dplyr::coalesce(ast_kommun, "Okänd arbetsort"),
      ast_i_dalarna = as.logical(ast_i_dalarna) %in% TRUE
    ) |>
    dplyr::filter(ar + antal_ar <= YH_SISTA_UPPF_AR) |>
    dplyr::group_by(ar, exam_ar_interval, antal_ar, kommkod, kommun, geo_niva, program, kon,
                    ast_kommunkod, ast_kommun, ast_i_dalarna) |>
    dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop")
}

hamta_yh_arbetsort <- function(force = FALSE) {
  if (force || is.null(.yh_arbetsort_cache$df))
    .yh_arbetsort_cache$df <- rensa_yh_arbetsort(.yh_hamta_tabell("yh_uppfoljning_arbetsort"))
  .yh_arbetsort_cache$df
}

# Första examensåret med den nya definitionen för en uppföljningstid.
yh_forsta_nya_examensar <- function(antal_ar) YH_DEFINITIONSBYTE - as.integer(antal_ar)

# Riket-andel per år och antal_ar (jämförelselinje). program_val = NULL ->
# alla program. YH skickar in riksrader på rätt nivå (yh_riket_rader());
# Högskolan sina Region-rader.
yh_etablering_riket <- function(df_full, program_val = NULL, metrik) {
  d <- df_full |> dplyr::filter(geo_niva == "riket")
  if (!is.null(program_val)) d <- dplyr::filter(d, program == program_val)
  d |>
    dplyr::group_by(ar, antal_ar) |>
    dplyr::summarise(
      status_sum = sum(.data[[metrik]], na.rm = TRUE),
      antal_sum  = sum(antal, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(andel_riket = dplyr::if_else(
      antal_sum > 0, status_sum / antal_sum, NA_real_)) |>
    dplyr::select(ar, antal_ar, andel_riket)
}
