shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'regiondalarna_ruf.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'app.css'),
      # Hover-tooltips på knappar (t.ex. indikatorknapparna) via tippy.js,
      # se www/tooltips.js. Tippy + Popper laddas lokalt (självhostat, ingen
      # extern CDN). OBS: "tippy-bundle" bundlar INTE Popper trots namnet –
      # Popper måste laddas som en egen fil FÖRE tippy-bundle, annars blir
      # window.Popper odefinierad och tippy kraschar internt vid start.
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'tippy.css'),
      tags$script(src = 'popper.min.js'),
      tags$script(src = 'tippy-bundle.umd.min.js'),
      tags$script(src = 'tooltips.js')
    ),

    # ---- Header (full bredd via app.css) ---------------------------------
    tags$div(
      class = 'rd-header',
      tags$div(class = 'rd-header__title', 'Utbildning i Dalarna'),
      tags$a(
        class  = 'rd-header__right',
        href   = 'https://www.regiondalarna.se',
        target = '_blank',
        tags$img(src = 'logo_liggande_fri_vit.png', alt = 'Region Dalarna')
      )
    ),

    # ---- Startmeddelande: appen är under utveckling -----------------------
    # Dyker upp vid start, ligger kvar en lång stund så att alla hinner läsa
    # den, och går även att stänga direkt med krysset.
    tags$div(
      id = 'rd-dev-notice', class = 'rd-dev-notice',
      tags$div(
        class = 'rd-dev-notice__kort', role = 'status',
        tags$button(
          type = 'button', class = 'rd-dev-notice__stang',
          onclick = "document.getElementById('rd-dev-notice').classList.add('rd-dev-notice--dold')",
          `aria-label` = 'Stäng meddelandet', HTML('&times;')
        ),
        h3('Appen är under utveckling'),
        p('Den här applikationen byggs och utvecklas löpande. Det kommer att ',
          'ske en hel del förändringar framöver, så nästa gång du besöker ',
          'sidan kan den se annorlunda ut.'),
        p('Har du frågor eller funderingar? Hör gärna av dig till ',
          tags$a(href = 'mailto:samhallsanalys@regiondalarna.se',
                 'samhallsanalys@regiondalarna.se'), '.')
      ),
      tags$script(HTML(
        "setTimeout(function() {
           var el = document.getElementById('rd-dev-notice');
           if (el) el.classList.add('rd-dev-notice--dold');
         }, 25000);"
      ))
    ),

    # ---- N1: Skolform (yttre tabsetPanel), hela bredden ------------------
    div(
      style = 'padding: 8px 24px 24px;',
      tabsetPanel(
        id = 'skolform',
        selected = 'Gymnasiet',

        tabPanel('Förskola',      mod_forskola_ui('forskola')),
        tabPanel('Grundskola',    mod_grundskola_ui('grundskola')),
        tabPanel('Gymnasiet',     mod_gymnasiet_ui('gym')),
        tabPanel('Komvux',        mod_komvux_ui('komvux')),
        tabPanel('Folkhögskola',  mod_folkhogskola_ui('folkhogskola')),
        tabPanel('Yrkeshögskola', mod_yh_ui('yh')),
        tabPanel('Högskola',      mod_hogskola_ui('hogskola')),
        tabPanel('Prognos 2040',  mod_prognos_ui('prognos')),

        tabPanel(
          'Om rapporten',
          div(class = 'rd-card',
              h2('Om rapporten'),
              p('Den här applikationen visar utbildningsstatistik för Dalarna. ',
                'Appen omfattar förskolan, gymnasiet, yrkeshögskola (YH), komvux (inkl. SFI), ',
                'högskola, grundskola och folkhögskola.'),
              p('Appen är under utveckling och byggs vidare löpande, så utseende ',
                'och innehåll kan komma att ändras. Har du frågor eller funderingar ',
                'är du välkommen att höra av dig till ',
                tags$a(href = 'mailto:samhallsanalys@regiondalarna.se',
                       'samhallsanalys@regiondalarna.se'), '.'),
              div(class = 'rd-info',
                  tags$strong('Källa: '),
                  'Regionala utvecklingsdatabasen (SCB), Skolverket, ',
                  'Gymnasieantagningen (Dalarnas kommunförbund) och SCB:s ',
                  'Regionala trender och prognoser 2025 (RegTop).'),
              p(class = 'rd-hint',
                'Etablering efter examen bygger på Registerbaserad aktivitetsstatistik ',
                '(RAKS) och, från och med 2020, Befolkningens arbetsmarknadsstatus (BAS). ',
                'HReg är SCB:s register över studenter och examina i högskolan.')
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
