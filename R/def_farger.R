# ============================================================
#  def_farger.R
#  Enda källan för R-sidans färger.
#
#  ggplot/ggiraph renderar SVG i R och kan inte läsa CSS var() för
#  geom-färger – därför plockar vi ut :root-variablerna ur CSS-filerna
#  vid uppstart, så att CSS och de server-renderade graferna delar samma
#  definitioner. Hex-fallback används om en variabel/fil saknas.
#
#  Den INTERAKTIVA css:en (hover/tooltip/markering) ligger i DOM:en och
#  använder var(--rd-...) rakt av – se RD_*_CSS nedan.
# ============================================================

# Läs CSS-custom-properties (--namn: värde;) ur en eller flera filer.
# Senare fil vinner (app.css överstyr regiondalarna_ruf.css), precis som i CSS.
.las_css_variabler <- function(filer) {
  rader <- unlist(lapply(filer, function(f) {
    if (file.exists(f)) readLines(f, warn = FALSE, encoding = "UTF-8") else character(0)
  }))
  m <- regmatches(
    rader,
    regexec("--([A-Za-z0-9_-]+)[[:space:]]*:[[:space:]]*([^;]+);", rader)
  )
  m <- m[lengths(m) == 3]
  if (!length(m)) return(stats::setNames(character(0), character(0)))
  namn <- vapply(m, `[`, character(1), 2)
  vals <- trimws(vapply(m, `[`, character(1), 3))
  vals <- stats::setNames(vals, namn)
  vals[!duplicated(names(vals), fromLast = TRUE)]
}

.RD_CSS <- .las_css_variabler(c("www/regiondalarna_ruf.css", "www/app.css"))

# Hämta en CSS-variabel som R-värde (med fallback).
rd_farg <- function(namn, fallback = "#000000") {
  v <- .RD_CSS[[namn]]
  if (is.null(v) || !nzchar(v)) fallback else v
}

# ---- Basfärger -------------------------------------------------------------
RD_PRIMARY    <- rd_farg("rd-primary",    "#158daf")
RD_ACCENT     <- rd_farg("rd-accent",     "#54a1bd")
RD_TEXT_MUTED <- rd_farg("rd-text-muted", "#6c757d")

# ---- Kön (definieras i app.css) --------------------------------------------
KON_FARGER <- c(
  "Kvinnor" = rd_farg("rd-kon-kvinnor", "#e2a855"),
  "Män"     = rd_farg("rd-kon-man",     "#459079")
)

# ---- Programtyp: RD:s blå vektor (tydligt skild från könsfärgerna) ---------
PROGRAMTYP_STOPS <- c(
  rd_farg("rd-blue-deep",  "#0074a2"),
  rd_farg("rd-primary",    "#158daf"),
  rd_farg("rd-blue-light", "#8edded")
)
programtyp_farger <- function(typer) {
  typer <- sort(unique(typer))
  stats::setNames(grDevices::colorRampPalette(PROGRAMTYP_STOPS)(length(typer)), typer)
}

# ---- Samverkansområden (RD:s blå vektor) -----------------------------------
SAMVERKAN_FARGER <- c(
  "Gysam Västra"            = rd_farg("rd-blue-bright", "#00b4e4"),
  "Gysam Södra"             = rd_farg("rd-blue-deep",   "#0074a2"),
  "Gysam Siljan"            = rd_farg("rd-primary",     "#158daf"),
  "Avesta"                  = rd_farg("rd-blue-light",  "#8edded"),
  "Mora, Orsa och Älvdalen" = rd_farg("rd-accent",      "#54a1bd")
)

# ---- Interaktiv css (i DOM:en -> kan använda var(--rd-...)) -----------------
RD_TOOLTIP_CSS <- paste0(
  "background:var(--rd-text);color:#fff;padding:6px 9px;",
  "border-radius:4px;font-family:Poppins,Arial,sans-serif;font-size:12px;"
)
RD_HOVER_CSS        <- "fill:var(--rd-primary-dark);cursor:pointer;"
RD_SELECT_CSS       <- "fill:var(--rd-primary-dark);"
RD_KARTA_HOVER_CSS  <- "stroke:var(--rd-text);stroke-width:1;cursor:pointer;"
RD_KARTA_SELECT_CSS <- "stroke:var(--rd-text);stroke-width:1.4;"
