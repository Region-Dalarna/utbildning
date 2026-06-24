source('global.R')

shinyUI(
  fluidPage(
    tags$head(
      tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'regiondalarna_ruf.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'app.css')
    ),

    # ---- Header (full bredd via app.css) ---------------------------------
    tags$div(
      class = 'rd-header',
      tags$div(class = 'rd-header__title', 'Utbildning i Dalarna'),
      tags$a(
        class  = 'rd-header__right',
        href   = 'https://www.regiondalarna.se',
        target = '_blank',
        tags$img(src = 'logo_liggande_fri_vit.png', alt = 'Region Dalarna'),
        tags$span('Samhällsanalys')
      )
    ),

    # ---- N1: Skolform (yttre tabsetPanel), hela bredden ------------------
    div(
      style = 'padding: 8px 24px 24px;',
      tabsetPanel(
        id = 'skolform',

        tabPanel('Gymnasiet',     mod_gymnasiet_ui('gym')),
        tabPanel('Yrkeshögskola', mod_skolform_placeholder_ui('yh',       'Yrkeshögskola')),
        tabPanel('Komvux',        mod_skolform_placeholder_ui('komvux',   'Komvux')),
        tabPanel('Högskola',      mod_skolform_placeholder_ui('hogskola', 'Högskola')),

        tabPanel(
          'Om rapporten',
          div(class = 'rd-card',
              h2('Om rapporten'),
              p('Den här applikationen visar utbildningsstatistik för Dalarna. ',
                'I nuvarande version är gymnasiet inlagt; YH, komvux och högskola ',
                'tillkommer efter hand.'),
              div(class = 'rd-info',
                  tags$strong('Källa: '),
                  'Gymnasieantagningen, Dalarnas kommunförbund.')
          )
        )
      )
    ),

    # ---- Footer (full bredd via app.css) ---------------------------------
    tags$div(
      class = 'rd-footer',
      'Samhällsanalys, Region Dalarna · ',
      tags$a(
        href = 'mailto:samhallsanalys@regiondalarna.se',
        'samhallsanalys@regiondalarna.se'
      )
    )
  )
)
