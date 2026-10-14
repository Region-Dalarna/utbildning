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
      anskod             = if ("anskod" %in% names(rad)) anskod else NA_character_,
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
    dplyr::left_join(kommun_samverkan, by = "kommkod") |>
    tolka_anskod()
}

# ---- Inriktningar och IM-varianter ur anskod --------------------------------
# anskod = programkod (2 tecken) + inriktningskod (3 bokstäver) + ev. tillägg:
#   0L/00L = lärling, 0R/00R/S0R = riksrekrytering, SPR = spetsutbildning,
#   -XXX = lokal profil (räknas som programmet/inriktningen).
# "25" direkt efter programkoden = Gy25-versionen av programmet (samma program).
# Koder som BAHUD är inriktningar, BA/BA25/EK-EKIALP saknar inriktning.
# Introduktionsprogrammet (IM): IMPRO/IMV = programinriktat val, IMYRK/IMY =
# yrkesintroduktion, följt av målprogrammets kod (t.ex. IMYVO, IMPROBA).
# Rena "IM" = variant ej angiven (troligen individuellt alternativ och
# språkintroduktion, som inte söks via antagningen).
#
# Lägger till kolumnerna inriktning (nivå 1 under programmet) och mal (nivå 2,
# bara IM: programmet varianten riktar sig mot). Inriktningarnas namn tas ur
# fritexten (program_inriktning) och ANTAGNING_INRIKTNING_NAMN; koder utan
# namn visas som "Inriktning <kod>" (se antagning_inriktningar_utan_namn()).
ANTAGNING_EJ_VALD   <- "Inriktning väljs senare / ej vald"
ANTAGNING_IM_EJ_ANG <- "Variant ej angiven"

# Namn för inriktningskoder som saknas i fritexten. Fylls på efter kontroll.
ANTAGNING_INRIKTNING_NAMN <- c()

# De nationella programmens koder (för IM:s målprogram).
NATIONELLA_PROGRAM <- c(
  BA = "Bygg- och anläggningsprogrammet", BF = "Barn- och fritidsprogrammet",
  EE = "El- och energiprogrammet",        EK = "Ekonomiprogrammet",
  ES = "Estetiska programmet",            FS = "Försäljnings- och serviceprogrammet",
  FT = "Fordons- och transportprogrammet", HA = "Handels- och administrationsprogrammet",
  HT = "Hotell- och turismprogrammet",    HU = "Humanistiska programmet",
  HV = "Hantverksprogrammet",             IN = "Industritekniska programmet",
  "NA" = "Naturvetenskapsprogrammet",     NB = "Naturbruksprogrammet",
  RL = "Restaurang- och livsmedelsprogrammet", SA = "Samhällsvetenskapsprogrammet",
  TE = "Teknikprogrammet",                VF = "VVS- och fastighetsprogrammet",
  VO = "Vård- och omsorgsprogrammet"
)

.anskod_variant <- function(rest) {
  dplyr::case_when(
    grepl("SPR$", rest)          ~ "spetsutbildning",
    grepl("0R$", rest)           ~ "riksrekrytering",
    grepl("0L$", rest)           ~ "lärling",
    TRUE                         ~ NA_character_)
}

# Inriktningens namn ur fritexten: delen efter programnamnet, t.ex.
# "El- och energiprogrammet, elteknik, riksrekrytering" -> "Elteknik".
.inriktning_ur_text <- function(txt) {
  del <- sub("^[^,;]*[,;] *", "", txt)
  del <- ifelse(del == txt | is.na(txt), NA_character_, sub(" *[,;].*$", "", del))
  ifelse(is.na(del) | del == "", NA_character_,
         paste0(toupper(substr(del, 1, 1)), substr(del, 2, nchar(del))))
}

tolka_anskod <- function(d) {
  if (all(is.na(d$anskod))) {
    d$inriktning <- NA_character_; d$mal <- NA_character_
    return(d)
  }
  kod  <- toupper(trimws(d$anskod))
  prg  <- substr(kod, 1, 2)
  rest <- substr(kod, 3, nchar(kod))
  rest <- sub("^25", "", rest)                     # Gy25 = samma program
  ar_im <- prg == "IM"

  # Nationella program: inriktningskod = tre bokstäver direkt efter programkoden.
  inr_kod <- ifelse(!ar_im & grepl("^[A-ZÅÄÖ]{3}", rest), substr(rest, 1, 3), NA_character_)
  variant <- .anskod_variant(sub("-.*$", "", rest))

  # Namn per (program, inriktningskod): vanligaste namnet i fritexten.
  namn <- tibble::tibble(prg, inr_kod, txt = .inriktning_ur_text(d$program_inriktning)) |>
    dplyr::filter(!is.na(inr_kod), !is.na(txt)) |>
    dplyr::count(prg, inr_kod, txt) |>
    dplyr::group_by(prg, inr_kod) |>
    dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  inr_namn <- namn$txt[match(paste(prg, inr_kod), paste(namn$prg, namn$inr_kod))]
  inr_namn <- dplyr::coalesce(inr_namn, unname(ANTAGNING_INRIKTNING_NAMN[paste0(prg, inr_kod)]),
                              ifelse(is.na(inr_kod), NA_character_, paste("Inriktning", inr_kod)))
  inriktning <- dplyr::case_when(
    !is.na(inr_namn) & !is.na(variant) ~ paste0(inr_namn, " (", variant, ")"),
    !is.na(inr_namn)                   ~ inr_namn,
    !is.na(variant) ~ paste0(toupper(substr(variant, 1, 1)), substr(variant, 2, nchar(variant))),
    TRUE                               ~ ANTAGNING_EJ_VALD)

  # IM: variant och målprogram.
  im_var <- dplyr::case_when(
    grepl("^(PRO|V)", rest)  ~ "Programinriktat val",
    grepl("^(YRK|Y)", rest)  ~ "Yrkesintroduktion",
    grepl("^IND", rest)      ~ "Individuellt alternativ",
    grepl("^SPR", rest)      ~ "Språkintroduktion",
    TRUE                     ~ ANTAGNING_IM_EJ_ANG)
  mal_kod <- dplyr::case_when(
    grepl("^(PRO|YRK)", rest) ~ substr(rest, 4, 5),
    grepl("^(V|Y)", rest)     ~ substr(rest, 2, 3),
    TRUE                      ~ "")
  # Programnamn: fasta listan, annars namnet i datan.
  prog_namn <- d |> dplyr::filter(pr_kod != "IM") |> dplyr::distinct(pr_kod, program)
  mal <- dplyr::coalesce(unname(NATIONELLA_PROGRAM[mal_kod]),
                         prog_namn$program[match(mal_kod, prog_namn$pr_kod)])
  mal <- dplyr::case_when(!grepl("^[A-ZÅÄÖ]{2}$", mal_kod) ~ "Målprogram ej angivet",
                          is.na(mal)                        ~ paste("Programkod", mal_kod),
                          TRUE                              ~ mal)

  d$inriktning <- ifelse(ar_im, im_var, inriktning)
  d$mal        <- ifelse(ar_im & im_var %in% c("Programinriktat val", "Yrkesintroduktion"), mal, NA_character_)
  d
}

# Inriktningskoder som saknar namn (visas som "Inriktning <kod>" i appen).
# Lägg till dem i ANTAGNING_INRIKTNING_NAMN, t.ex. c("NBNAM" = "Naturturism").
antagning_inriktningar_utan_namn <- function() {
  hamta_gymnasiedata() |>
    dplyr::filter(grepl("^Inriktning [A-ZÅÄÖ]{3}", inriktning)) |>
    dplyr::mutate(kod = paste0(pr_kod, sub("^Inriktning ([A-ZÅÄÖ]{3}).*$", "\\1", inriktning))) |>
    dplyr::group_by(program, kod) |>
    dplyr::summarise(exempel_anskod = paste(utils::head(unique(anskod), 4), collapse = " "),
                     ar = paste(range(ar), collapse = "–"),
                     antagna = sum(antagna, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(program, kod)
}

# ---- Publik läsfunktion ----------------------------------------------------
# force = TRUE läser om från databasen (annars används cachen).
hamta_gymnasiedata <- function(force = FALSE) {
  if (force || is.null(.data_cache$df)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("dkf", "gymnasieantagna")) |>
      dplyr::collect()
    DBI::dbDisconnect(con)
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

  # Steg 1: Rensa och filtrera rådata till Dalarnas kommunrader + länsraden.
  # OBS: kommkod == "20" (Dalarnas län) behålls medvetet – Skolverket har
  # redan en färdigt (och korrekt) viktad länsrad i datasetet, så appen
  # slipper vikta ihop kommunraderna själv för "Hela Dalarna" (se geo_niva
  # nedan och hur den används i data_bas() i mod_gymnasiet.R).
  # "Samtliga" behålls också (döps om till "Alla" nedan) – det är
  # Skolverkets egen färdiga totalrad för Kommunal+Enskild, så appen
  # slipper summera ihop driftsformerna själv (samma mönster som redan
  # används i rensa_genomstromning()).
  rad_fil <- rad |>
    dplyr::rename(program = gymnasieprogram, kommkod = regionkod,
                  kommun = region, organisationstyp = huvudman) |>
    dplyr::mutate(
      kommkod = as.character(kommkod),
      ar      = as.integer(substr(as.character(lasar), 1, 4)),
      varde   = as.numeric(varde)
    ) |>
    dplyr::filter(
      (kommkod == "20") | (nchar(kommkod) == 4 & substr(kommkod, 1, 2) == "20")
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

  # Andelar viktas med antal elever per driftsform. Rader med okänd andel
  # (varde = NA, t.ex. Skolverkets cellsuppression för få elever) filtreras
  # bort INNAN viktsumman räknas – annars bidrar de sina elever till
  # nämnaren men noll till täljaren, vilket drar ner andelen konstlat.
  andel_df <- rad_fil |>
    dplyr::filter(variabel %in% andel_vars, !is.na(varde)) |>
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
      organisationstyp = dplyr::case_when(
        organisationstyp == "Samtliga" ~ "Alla",
        TRUE                           ~ organisationstyp
      ),
      geo_niva = dplyr::case_when(kommkod == "20" ~ "lan", TRUE ~ "kommun"),
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
    DBI::dbDisconnect(con)
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
#  Elevantal som viktningsunderlag för Genomströmning vid
#  samverkansområden. gymnasiet_genomstromning saknar en egen
#  antal-kolumn (bara färdigberäknad "andel"), så vi lånar
#  elevantal från gymnasiet_elever för att kunna vikta ihop flera
#  kommuner korrekt i stället för ett ovägt medelvärde.
#
#  Elevtabellen har en egen "Alla"-rad (Skolverkets "Samtliga",
#  omdöpt i rensa_elevdata()) för Kommunal+Enskild kombinerat, så
#  ingen egen summering behövs här.
#
#  OBS: programnamnen i genomströmnings- och elevtabellerna kommer
#  från olika Skolverket-uttag och matchas här på exakt strängvärde
#  (efter trimws()). Om ett program inte hittar en matchande vikt
#  faller viktningen tillbaka till ovägt medel för just det
#  programmet – se .viktat_medel_andel() i func_diagram.R. Värt att
#  stämma av mot källdata efter driftsättning.
# ============================================================
hamta_elevantal_vikt <- function(force = FALSE) {
  hamta_gymnasie_elever(force = force) |>
    dplyr::filter(geo_niva == "kommun") |>
    dplyr::mutate(program = trimws(program)) |>
    dplyr::select(ar, kommkod, program, organisationstyp, vikt = antal_elever)
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
    DBI::dbDisconnect(con)
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


# ============================================================
#  Etablering (SCB/RAKS) – uppföljning av gymnasieexaminerade
#
#  Kolumner: exam_ar_interval, uppf_ar_interval, antal_ar,
#    programkod, regionkod, syss, stud, arblos, etabl,
#    antal, region, gymnasieprogram, inriktning
#
#  antal_ar = 1/3/5/7 år efter examen.
#  Datan är på individ-/cellnivå (en rad per person × bokommun × astkommun).
#  Kolumnerna syss/stud/arblos/ovriga/etabl/antal är antal (inte andelar).
#  syss+stud+arblos+ovriga ≈ antal (källans egna total).
#  etabl är en delmängd (av syss) och summerar INTE med övriga till 100%.
#  forvink_etabl = summerad förvärvsinkomst bland etablerade (kr).
#  Aggregering sker i modulen via summering.
# ============================================================
.etablering_cache <- new.env(parent = emptyenv())

rensa_etablering <- function(rad) {
  rad |>
    dplyr::rename(
      bokommunkod  = kommun,
      bokommun     = kommun_namn,
      astkommunkod = astkommun,
      astkommun    = astkommun_namn,
      program          = gymnasieprogram,
      organisationstyp = hman_namn,
      kommkod      = regionkod,
      kommun       = region
    ) |>
    dplyr::mutate(
      kommkod = as.character(kommkod),
      ar      = as.integer(substr(exam_ar_interval, 1, 4)),
      syss    = as.numeric(syss),
      stud    = as.numeric(stud),
      arblos  = as.numeric(arblos),
      ovriga  = as.numeric(ovriga),
      etabl   = as.numeric(etabl),
      antal   = as.numeric(antal),
      forvink_etabl = as.numeric(forvink_etabl),
      geo_niva = dplyr::case_when(
        kommkod == "00" ~ "riket",
        kommkod == "20" ~ "lan",
        TRUE            ~ "kommun"
      ),
      prog_niva = dplyr::case_when(
        program %in% c("Gymnasieskolan totalt", "Nationella program",
                       "Yrkesprogram", "Högskoleförberedande program",
                       "Introduktionsprogram")                  ~ "aggregat",
        TRUE                                                    ~ "program"
      )
    ) |>
    dplyr::left_join(kommun_samverkan, by = "kommkod") |>
    dplyr::select(ar, exam_ar_interval, uppf_ar_interval, antal_ar,
                  kommkod, kommun, samverkansomrade, program, inriktning,
                  organisationstyp, bokommunkod, bokommun, astkommunkod, astkommun,
                  forvink_etabl, geo_niva, prog_niva,
                  syss, stud, arblos, ovriga, antal, etabl)
}

hamta_etablering <- function(force = FALSE) {
  if (force || is.null(.etablering_cache$df)) {
    con <- shiny_uppkoppling_las("sekretess", db_user = "shiny_las_sekretess")
    rad <- dplyr::tbl(con, dbplyr::in_schema("mikro_db",
                                             "gymnasiet_uppfoljning_raks")) |>
      dplyr::collect()
    DBI::dbDisconnect(con)
    .etablering_cache$df <- rensa_etablering(rad)
  }
  .etablering_cache$df
}

# Riket-andel per program och antal_ar (för jämförelselinje i trend).
# Om program_val = NULL returneras medel över alla program (för totalvyn).
# Aggregerar genom att summera antal och beräkna andel = status/total.
etablering_riket <- function(df_full, program_val = NULL, metrik) {
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

# Enkel men snygg formatering: fet rubrikrad i petrolblått, autobredd på
# kolumner, fryst rubrikrad och autofilter. Kräver paketet openxlsx.
# Röjandekontroll (MIN_ANTAL, se nedan) görs innan filen skrivs.
# min_antal = 0 stänger av den (publicerad statistik, t.ex. gymnasieantagningen).
skriv_gymnasie_excel <- function(df, file, blad = "Gymnasiet", min_antal = MIN_ANTAL) {
  df <- rojandekontroll_tabell(df, min_antal)
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

# ============================================================
#  Nedladdning av etableringsdata (Gymnasiet, YH, Högskola)
# ============================================================
# Etableringsdatan har rader med en eller ett par personer (och i
# gymnasiets fall bo-/arbetskommun och inkomst per rad). Nedladdningen
# summeras därför till samma nivå som diagrammen. Celler med färre än
# MIN_ANTAL personer får tomma värden, och medelinkomsten visas bara när
# minst så många etablerade har uppgift om inkomst.

summera_etablering_nedladdning <- function(df) {
  grupp <- intersect(c("exam_ar_interval", "uppf_ar_interval", "antal_ar", "geo_niva",
                       "kommkod", "kommun", "samverkansomrade", "program", "inriktning",
                       "organisationstyp"), names(df))
  status <- c("etabl", "syss", "stud", "arblos", "ovriga")

  # Inkomst som summa + antal etablerade med inkomst. YH/Högskola har dem
  # färdiga; gymnasiet har forvink_etabl (summa per rad, NA = saknas).
  if (!all(c("inkomst_summa", "etabl_med_inkomst") %in% names(df))) {
    df <- if ("forvink_etabl" %in% names(df)) {
      dplyr::mutate(df,
        inkomst_summa     = dplyr::coalesce(as.numeric(forvink_etabl), 0),
        etabl_med_inkomst = dplyr::if_else(is.na(forvink_etabl), 0, as.numeric(etabl)))
    } else {
      dplyr::mutate(df, inkomst_summa = 0, etabl_med_inkomst = 0)
    }
  }

  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grupp))) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(c("antal", status, "inkomst_summa", "etabl_med_inkomst")),
                                   ~sum(.x, na.rm = TRUE)),
                     .groups = "drop") |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(status),
                    ~dplyr::if_else(antal > 0, round(100 * .x / antal, 1), NA_real_),
                    .names = "andel_{.col}"),
      medelinkomst_etabl = dplyr::if_else(
        etabl_med_inkomst >= MIN_ANTAL & antal >= MIN_ANTAL,
        round(inkomst_summa / etabl_med_inkomst), NA_real_),
      dplyr::across(dplyr::all_of(c(status, paste0("andel_", status))),
                    ~dplyr::if_else(antal < MIN_ANTAL, NA_real_, as.numeric(.x)))
    ) |>
    dplyr::select(-inkomst_summa, -etabl_med_inkomst) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(grupp))) |>
    structure(rojandekontrollerad = TRUE)   # rojandekontroll_tabell() hoppar över
}

# ============================================================
#  Röjandekontroll
# ============================================================
# Värden som bygger på färre än MIN_ANTAL personer visas inte - varken i
# diagrammen (se func_diagram.R) eller i nedladdningen. Gäller mikrodata
# från Regionala utvecklingsdatabasen; publicerad statistik (Skolverket,
# gymnasieantagningen) undantas med min_antal = 0.
MIN_ANTAL <- 4

# Andelar/medelvärden och den kolumn som är deras underlag (nämnare).
.ROJ_UNDERLAG <- c(
  andel             = "avslutade",            # genomströmning
  andel_godkant     = "avslutad_kurs",
  andel_avbrott     = "kursdeltaganden",
  andel_inskrivna   = "antal_barn",
  andel_behorig     = "behorig_underlag",
  andel_en_godkant  = "en_underlag",
  andel_ma_godkant  = "ma_underlag",
  andel_sv_godkant  = "sv_underlag",
  andel_sva_godkant = "sva_underlag",
  meritvarde_m2_medel = "meritvarde_underlag",
  meritvarde_medel  = "antal_med_meritvarde"
)

# Kolumner som är tal men inte antal personer (år, koder m.m.).
.ROJ_EJ_ANTAL <- "^(ar|antal_ar|startar|kursinr|hman|huvudman|niva|kon)$|^andel|medel|inkomst|_underlag$"

# Döljer i en tabell för nedladdning: andelar vars underlag är under
# min_antal, och antal mellan 1 och min_antal - 1.
rojandekontroll_tabell <- function(df, min_antal = MIN_ANTAL) {
  if (min_antal <= 0 || isTRUE(attr(df, "rojandekontrollerad"))) return(df)
  # 1. Andelar efter sitt underlag (innan underlaget självt döljs).
  for (kol in intersect(names(.ROJ_UNDERLAG), names(df))) {
    und <- .ROJ_UNDERLAG[[kol]]
    if (und %in% names(df))
      df[[kol]] <- dplyr::if_else(df[[und]] < min_antal, NA_real_, as.numeric(df[[kol]]))
  }
  # 2. Små antal.
  antal_kol <- names(df)[vapply(df, is.numeric, logical(1)) & !grepl(.ROJ_EJ_ANTAL, names(df))]
  for (kol in antal_kol) {
    x <- df[[kol]]
    df[[kol]] <- dplyr::if_else(!is.na(x) & x > 0 & x < min_antal, NA_real_, as.numeric(x))
  }
  df
}
