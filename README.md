# Utbildning i Dalarna

Shiny-app med utbildningsstatistik för Dalarna (Samhällsanalys, Region Dalarna):
förskola, grundskola, gymnasiet, komvux (inkl. SFI), folkhögskola,
yrkeshögskola (YH) och högskola. Datan läses direkt ur databasen.

## Filstruktur
```
global.R          library(), bl.a. rdshinyappar (uppkopplingar, df_till_sf) – körs först
ui.R, server.R    flikar (en modul per skolform), Gymnasiet förvald
R/                laddas AUTOMATISKT av Shiny (>= 1.5.0), bokstavsordning, efter global.R
  def_farger.R                färger, läses ur CSS-variablerna i www/
  def_geografi.R              kommuner + samverkansområden
  func_data*.R                databasläsning och rensning, en fil per skolform
                              (func_data.R = gymnasiet + gemensamma hjälpare)
  func_diagram.R              diagramfunktioner (ggiraph)
  func_karta.R                referenskarta över kommuner/samverkansområden (sf)
  mod_*.R                     en modul per skolform
www/              CSS, favicon, logga, tooltips
```

## Köra lokalt
```r
renv::restore()
shiny::runApp(".")
```
Lösenorden till databasanvändarna hanteras av `rdshinyappar`. `sf` ligger i
`_dependencies.R` eftersom `rdshinyappar` bara har det som valfritt beroende.

## Struktur (hierarki)
- **N1 Skolform** – yttre `tabsetPanel` i `ui.R`
- **N2 Statistikområde** – knapprad överst i varje modul
- **N3 Indikator** – knappar i sidopanelen (beskrivningen visas som tooltip)

Indikatorerna definieras i `<skolform>_struktur` överst i respektive modul.

## Datakällor
| Flik | Tabell | Kommun avser |
|------|--------|--------------|
| Förskola | `oppna_data.mikro_db.forskola` | hemkommun |
| Grundskola | `oppna_data.mikro_db.grundskola_slutbetyg` | skolans kommun |
| Gymnasiet | `oppna_data.dkf.gymnasieantagna` m.fl., avgångna i `oppna_data.mikro_db.gymnasiet_avgangna` (skolkommun eller bokommun), etablering i `sekretess.mikro_db.gymnasiet_uppfoljning_raks` | – |
| Komvux | `oppna_data.mikro_db.komvux_sfi_studerande` | skolans kommun |
| Folkhögskola | `oppna_data.mikro_db.folkhogskola_elever` | kurskommun |
| YH | `oppna_data.mikro_db.yh_studerande`, `yh_genomstromning`, `yh_uppfoljning` | studieort |
| Högskola | `oppna_data.mikro_db.hogskola_aktivitet`, `hogskola_examen`, `hogskola_etablering` | hemkommun |

Tabellerna hämtas en gång per R-process och cachas. Komvux och Folkhögskola har
en kolumn `granularitet`: deltagare (unika individer) får inte summeras över
finare nivåer, så de läses från den nivå som motsvarar diagrammet.

## Data in i databasen
Appen läser bara från databasen. Tabellerna skrivs med separata skript
utanför det här repot (t.ex. `hogskola_etablering`, som är en summering av
den individnära högskoleuppföljningen).

## Röjandekontroll
Värden som bygger på färre än `MIN_ANTAL` (4) personer visas inte, varken i
diagrammen eller i nedladdningen. Antal mellan 1 och 3 döljs, och andelar och
medelvärden döljs när deras underlag är färre än 4.

- **Antal** visas som en grå, streckad platshållarstapel med det fasta värdet
  `ROJ_PLATSHALLARE` (1,5) och tooltipen "färre än 4" – man ser att gruppen
  finns men inte hur stor den är.
- **Andelar och medelvärden** får ingen stapel, bara texten "färre än 4"
  (en fast längd vore missvisande).
- **Trendlinjer** får ett avbrott vid dolda år.
- Källtexten får en rad som förklarar det.

Gränsen sätts i `MIN_ANTAL` i `R/func_data.R`. Diagramfunktionerna i
`R/func_diagram.R` tar `min_antal` (standard `MIN_ANTAL`), och nedladdningen går
genom `rojandekontroll_tabell()`; underlaget för varje andel anges i
`.ROJ_UNDERLAG`. Publicerad statistik (gymnasieantagningen, Skolverket)
undantas med `min_antal = 0` – det gäller allt i Gymnasiet utom etableringen.

## Nedladdning
"Ladda ner aktuellt urval" och "Ladda ner hela datasetet" ger Excelfiler.
Etableringsdata (Gymnasiet, YH, Högskola) summeras först till diagrammens nivå,
med medelinkomst bland etablerade (tom när färre än 4 etablerade har uppgift om
inkomst), se `summera_etablering_nedladdning()` i `R/func_data.R`.

## Kartan
`R/func_karta.R` ritar Dalarnas kommuner färgade efter samverkansområde.
Geometrin hämtas ur `geodata.karta.kommun_scb`. Går den inte att läsa visas
felmeddelandet under kartan och i loggen.
