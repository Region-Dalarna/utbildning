# ============================================================
#  func_data_gymnasiet_avgangna.R
#  Dataåtkomst för avgångna gymnasieelever (examen, studiebevis, betyg).
#
#  Källa: oppna_data.mikro_db.gymnasiet_avgangna (Gymnasiet – avgångna).
#  Avgångsår 2014 och framåt.
#
#  Tabellen är lång med kolumnen granularitet. Varje granularitet är en egen
#  uppdelning - filtrera ALLTID på en och summera aldrig över flera:
#    Typ (totaler), TypKon, TypBakgrund, TypNyanland, TypHuvudman,
#    TypStudievag (program/inriktning), TypBetygstyp (Avgb_Typ), TypBeh.
#  TypStudievagDetalj (beställd): program/inriktning kombinerat med kön,
#    bakgrund, nyanländ, huvudman, betygstyp och beh. Finns den används den för
#    programdiagrammet så att det kan delas upp och filtreras på huvudman.
#  avgangna = unika elever. Inom EN granularitet är summering över alla
#  dimensioner säker (en elev har ett värde av varje).
#
#  geografi: "skolkommun" (elever på skolor i området) eller "bokommun"
#  (områdets ungdomar, var de än går i skola).
# ============================================================

AVGANGNA_DETALJ <- "TypStudievagDetalj"

# Uppdelningarna (knapparna under diagrammet): granularitet för totaler och
# kolumnen med gruppens etikett.
AVGANGNA_GRUPPER <- list(
  kon      = list(label = "Kön",      granularitet = "TypKon",      kol = "grupp_kon"),
  bakgrund = list(label = "Bakgrund", granularitet = "TypBakgrund", kol = "grupp_bakgrund"),
  nyanland = list(label = "Nyanländ", granularitet = "TypNyanland", kol = "grupp_nyanland")
)

# Avgb_Typ (typ av betyg/betygsdokument) enligt SCB:s värdemängd.
.avgb_typ_klartext <- c(
  "B" = "Samlat betygsdokument",
  "C" = "Certificate",
  "D" = "Diploma",
  "E" = "Högskoleförberedande examen",
  "J" = "Studiebevis tekniskt fjärde år",
  "L" = "Yrkesförberedande examen",
  "S" = "Slutbetyg",
  "T" = "Examensbevis T4",
  "Z" = "Studiebevis"
)

.avgangna_cache <- new.env(parent = emptyenv())

rensa_gymnasiet_avgangna <- function(rad) {
  saknas <- setdiff(c("utrinrfodd_namn", "avgb_typ"), names(rad))
  for (k in saknas) rad[[k]] <- NA_character_
  rad |>
    dplyr::rename(kommkod = regionkod, kommun = region) |>
    dplyr::mutate(
      ar      = as.integer(ar),
      kommkod = as.character(kommkod),
      program_namn    = dplyr::coalesce(gymnasieprogram, "Okänt program"),
      inriktning_kort = dplyr::coalesce(inriktning, "Ingen inriktning"),
      avgb_typ_namn   = dplyr::coalesce(unname(.avgb_typ_klartext[avgb_typ]), avgb_typ,
                                        "Okänd betygstyp"),
      # Korta etiketter för uppdelningarna.
      grupp_kon      = dplyr::recode(kon, "Kvinna" = "Kvinnor", "Man" = "Män"),
      grupp_bakgrund = utrinrfodd_namn,
      grupp_nyanland = dplyr::recode(nyanland, "Nyanländ (högst 4 år i Sverige)" = "Nyanlända",
                                     "Ej nyanländ" = "Ej nyanlända"),
      dplyr::across(c(avgangna, jmftal_summa, antal_med_jmftal), as.numeric)
    )
}

hamta_gymnasiet_avgangna <- function(force = FALSE) {
  if (force || is.null(.avgangna_cache$df)) {
    con <- shiny_uppkoppling_las("oppna_data")
    rad <- dplyr::tbl(con, dbplyr::in_schema("mikro_db", "gymnasiet_avgangna")) |>
      dplyr::collect()
    DBI::dbDisconnect(con)
    .avgangna_cache$df <- rensa_gymnasiet_avgangna(rad)
  }
  .avgangna_cache$df
}

# examen_grupp för antalsindikatorerna Examen och Studiebevis.
AVGANGNA_EXAMEN_GRUPP <- c(examen = "Examen", studiebevis = "Studiebevis (minst 2500 poäng)")

# Summerar till en rad per (ar, område, program, delgrupp) med examen,
# studiebevis, betygspoäng, behörighet och andelar. "program" och "delgrupp"
# tas från angivna kolumner ("Alla" om NULL). Behörighet (beh: 1 = behörig,
# 0 = ej behörig, tomt räknas inte) blir NA när raderna saknar beh.
avgangna_summera <- function(d, grupp_kol = NULL, delgrupp_kol = NULL) {
  if (!"beh" %in% names(d)) d$beh <- NA
  d |>
    dplyr::mutate(program  = if (is.null(grupp_kol)) "Alla" else .data[[grupp_kol]],
                  delgrupp = if (is.null(delgrupp_kol)) "Alla" else .data[[delgrupp_kol]]) |>
    dplyr::group_by(ar, kommkod, kommun, geo_niva, program, delgrupp) |>
    dplyr::summarise(
      examen      = sum(avgangna[examen_grupp == AVGANGNA_EXAMEN_GRUPP[["examen"]]], na.rm = TRUE),
      studiebevis = sum(avgangna[examen_grupp == AVGANGNA_EXAMEN_GRUPP[["studiebevis"]]], na.rm = TRUE),
      behoriga     = sum(avgangna[beh %in% 1], na.rm = TRUE),
      beh_underlag = sum(avgangna[beh %in% c(0, 1)], na.rm = TRUE),
      avgangna    = sum(avgangna, na.rm = TRUE),
      jmftal_summa     = sum(jmftal_summa, na.rm = TRUE),
      antal_med_jmftal = sum(antal_med_jmftal, na.rm = TRUE),
      .groups = "drop") |>
    dplyr::mutate(
      andel_examen      = dplyr::if_else(avgangna > 0, 100 * examen / avgangna, NA_real_),
      andel_studiebevis = dplyr::if_else(avgangna > 0, 100 * studiebevis / avgangna, NA_real_),
      andel_beh = dplyr::if_else(beh_underlag > 0, 100 * behoriga / beh_underlag, NA_real_),
      betyg = dplyr::if_else(antal_med_jmftal > 0, jmftal_summa / antal_med_jmftal, NA_real_)
    )
}
