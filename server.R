shinyServer(function(input, output, session) {

  telemetri_server(telemetry, navigation_id = 'skolform', forsta_flik = 'Gymnasiet')

  # ---- Djuplänkning: läs ev. query-parametrar i URL:en en gång vid start.
  # flik=<slug> väljer flik direkt (se R/def_lankning.R för slugs); övriga
  # parametrar tolkas av respektive modul.
  lank <- shiny::isolate(shiny::parseQueryString(session$clientData$url_search))

  if (!is.null(lank$flik) && lank$flik %in% names(FLIK_SLUGS)) {
    shiny::updateTabsetPanel(session, 'skolform', selected = FLIK_SLUGS[[lank$flik]])
  }

  # Håll adressfältet synkat med vald flik, så att den går att dela utan
  # någon extra "kopiera länk"-knapp (ignoreInit så vi inte skriver över
  # en fullständig diagramlänk direkt vid sidladdning).
  shiny::observeEvent(input$skolform, ignoreInit = TRUE, {
    slug <- names(FLIK_SLUGS)[match(input$skolform, FLIK_SLUGS)]
    if (!is.na(slug)) shiny::updateQueryString(paste0('?flik=', slug), mode = 'replace')
  })

  # En modul per skolform.
  mod_forskola_server('forskola', lank = lank)
  mod_gymnasiet_server('gym', lank = lank)
  mod_yh_server('yh', lank = lank)
  mod_komvux_server('komvux', lank = lank)
  mod_hogskola_server('hogskola', lank = lank)
  mod_grundskola_server('grundskola', lank = lank)
  mod_folkhogskola_server('folkhogskola', lank = lank)
  mod_prognos_server('prognos', lank = lank)

})
