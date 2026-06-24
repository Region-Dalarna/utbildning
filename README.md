# Utbildning i Dalarna – Shiny-skelett

Utbildningsstatistik (Samhällsanalys, Region Dalarna). Startar med gymnasiet.
Läser gymnasieantagningsdata direkt ur databasen.

## Filstruktur
```
global.R          library() + delad func_shinyappar.R (körs först)
ui.R, server.R    rot
R/                laddas AUTOMATISKT av Shiny (>= 1.5.0), bokstavsordning, efter global.R
  def_geografi.R              kommuner + Gysam-områden
  func_data.R                 databasläsning (beror på def_geografi)
  func_diagram.R              diagramhjälpare (ggiraph)
  func_karta.R                referenskarta över samverkansområden (ggiraph + sf)
  mod_gymnasiet.R             modul: skolform Gymnasiet
  mod_skolform_placeholder.R  platshållare för övriga skolformer
www/              CSS, favicon, logga
```

## Köra lokalt
```r
shiny::runApp(".")
```
Krav: `shiny`, `shinyWidgets`, `dplyr`, `tidyr`, `tibble`, `forcats`,
`readr`, `ggplot2`, `ggiraph`, `dbplyr`, samt `sf` (för kartan – saknas den
visas en liten fallback i stället, appen kraschar inte). Lägg
`logo_liggande_fri_vit.png` i `www/`.

## Struktur (hierarki)
- **N1 Skolform** – yttre `tabsetPanel` (Gymnasiet, YH, Komvux, Högskola, …)
- **N2 Statistikområde** – `radioGroupButtons` (knapprad) i `R/mod_gymnasiet.R`
- **N3 Indikator** – `radioGroupButtons` (knappar i 2-kolumnersrutnät) i sidopanelen

Klara indikatorer: *Platser efter program* och *Sökande, första hand* (könsuppdelad).

## Kartan
`R/func_karta.R` ritar Dalarnas kommuner som polygoner färgade efter
samverkansområde (ingen bakgrundskarta). Visas alltid i sidopanelen och
markerar vald kommun (kommunläge) eller område (samverkansläge). Geometrin
hämtas ur geodata-databasen (`karta.kommun_scb`) via
`shiny_uppkoppling_las("geodata")`, filtreras på länskod 20 och joinas mot
`kommun_samverkan`. Saknas `sf` eller DB-åtkomst visas en liten fallback-text.

## Datakälla
Gymnasiedatan läses i `hamta_gymnasiedata()` (`R/func_data.R`) från
`oppna_data.dkf.gymnasieantagna` via `shiny_uppkoppling_las("oppna_data")`.
Tabellen hämtas en gång per R-process och cachas; `hamta_gymnasiedata(force = TRUE)`
läser om. Rensningen av de icke-syntaktiska kolumnnamnen sker EN gång i
`rensa_gymnasiedata()`:

| DB-kolumn   | Appens namn      |
|-------------|------------------|
| `org`       | `platser`        |
| `1a_tot`    | `sok_1a`         |
| `1a_kv`     | `sok_1a_kv`      |
| `1a_män`    | `sok_1a_man`     |
| `ant_tot`   | `antagna`        |
| `led_pl`    | `lediga_platser` |
| `merit_medel` | `merit_medel`  |

`samverkansomrade` finns inte i tabellen utan joinas på `kommkod` via
`kommun_samverkan` (`R/def_geografi.R`).

## Källor
Antagningsindikatorerna anger "Gymnasieantagningen, Dalarnas kommunförbund".
Källa sätts per indikator (`kalla`-fältet i `gymnasiet_struktur`).
