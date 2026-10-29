# ============================================================
#  def_lankning.R – djuplänkning (URL → flik/diagramval)
#
#  Query-parametern "flik" i URL:en väljer flik vid sidladdning, t.ex.
#  https://.../?flik=gymnasiet. Nycklarna här är korta, URL-vänliga
#  slugs; värdena är flikarnas riktiga namn (måste matcha tabPanel-
#  titlarna i ui.R ordagrant).
#
#  Varje skolformsmodul kan läsa fler query-parametrar för att
#  återskapa ett visst diagramval -- se "lank"-argumentet till
#  mod_*_server() i respektive modulfil.
# ============================================================

FLIK_SLUGS <- c(
  forskola     = 'Förskola',
  grundskola   = 'Grundskola',
  gymnasiet    = 'Gymnasiet',
  komvux       = 'Komvux',
  folkhogskola = 'Folkhögskola',
  yh           = 'Yrkeshögskola',
  hogskola     = 'Högskola',
  prognos      = 'Prognos 2040',
  om           = 'Om rapporten'
)

# Bygger en URL av `varden` (en named list - t.ex. flik, indikator, år,
# klickat element) som query-sträng och kopierar den till urklipp, samma
# knapp-/observeEvent-mönster i varje modul som stödjer diagramlänkning.
# NULL/tomma värden tas bort, flervärdiga val (t.ex. valda program) ska
# redan vara hopslagna till en kommaseparerad sträng av anroparen.
kopiera_lank_till_urklipp <- function(session, varden) {
  varden <- varden[!vapply(varden, function(x) is.null(x) || length(x) == 0 || !nzchar(x[1]),
                           logical(1))]
  query_sok <- paste(
    mapply(function(namn, varde) paste0(namn, '=', utils::URLencode(as.character(varde), reserved = TRUE)),
           names(varden), unlist(varden)),
    collapse = '&'
  )
  shinyjs::runjs(sprintf(
    "var url = window.location.origin + window.location.pathname + '?%s';
     if (navigator.clipboard && window.isSecureContext) {
       navigator.clipboard.writeText(url);
     } else {
       var ta = document.createElement('textarea');
       ta.value = url; ta.style.position = 'fixed'; ta.style.opacity = '0';
       document.body.appendChild(ta); ta.select();
       document.execCommand('copy'); document.body.removeChild(ta);
     }",
    query_sok
  ))
  shiny::showNotification('Länk till den här vyn kopierad till urklipp.',
                          type = 'message', duration = 4)
}
