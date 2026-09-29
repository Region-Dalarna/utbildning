# ============================================================
#  mod_grundskola.R
#  Shiny-modul för skolformen Grundskola (slutbetyg åk 9).
#
#  Samma förenklade mönster som mod_komvux.R (inget program-/
#  inriktningsval, ingen samverkansområde/karta). "program" i
#  diagramfunktionernas mening = kön (se func_data_grundskola.R), så
#  stapel/trend visar Män vs Kvinnor snarare än en ämnesuppdelning -
#  grundskoladatan saknar en naturlig sådan dimension.
#
#  Alla indikatorer är viktade medelvärden (vy "andel"). Meritvärde har
#  enhet "" i stället för " %" (se skapa_diagram_bar_andel()).
# ============================================================

.KALLA_GRUNDSKOLA <- "SCB, Grundskolans slutbetyg åk 9"

grundskola_struktur <- list(
  resultat = list(
    label = "Resultat åk 9",
    indikatorer = list(
      behorighet = list(label = "Gymnasiebehörighet", klar = TRUE, vy = "andel", kon = FALSE,
                        amne = "Andel med gymnasiebehörighet", metrik = "andel_behorig", vikt = "behorig_underlag",
                        metrik_label = "Andel behöriga (%)", kalla = .KALLA_GRUNDSKOLA,
                        beskrivning = paste(
                          "För att en elev ska vara behörig till gymnasieskolans nationella",
                          "program krävs lägst betyget godkänd i ämnena svenska/svenska som",
                          "andraspråk, engelska och matematik.")),
      meritvarde = list(label = "Meritvärde", klar = TRUE, vy = "andel", kon = FALSE,
                        amne = "Genomsnittligt meritvärde", metrik = "meritvarde_medel",
                        vikt = "meritvarde_underlag", enhet = "",
                        metrik_label = "Genomsnittligt meritvärde", kalla = .KALLA_GRUNDSKOLA,
                        beskrivning = paste(
                          "Summan av betygsvärdena i elevens bästa ämnen, i genomsnitt bland",
                          "elever som har ett meritvärde.")),
      en = list(label = "Engelska", klar = TRUE, vy = "andel", kon = FALSE,
               amne = "Andel godkänt i Engelska", metrik = "andel_en_godkant", vikt = "en_underlag",
               metrik_label = "Andel godkänt (%)", kalla = .KALLA_GRUNDSKOLA,
               beskrivning = "Andel godkänt betyg i Engelska, bland elever med ett fastställt betygsresultat i ämnet."),
      ma = list(label = "Matematik", klar = TRUE, vy = "andel", kon = FALSE,
               amne = "Andel godkänt i Matematik", metrik = "andel_ma_godkant", vikt = "ma_underlag",
               metrik_label = "Andel godkänt (%)", kalla = .KALLA_GRUNDSKOLA,
               beskrivning = "Andel godkänt betyg i Matematik, bland elever med ett fastställt betygsresultat i ämnet."),
      sv = list(label = "Svenska", klar = TRUE, vy = "andel", kon = FALSE,
               amne = "Andel godkänt i Svenska", metrik = "andel_sv_godkant", vikt = "sv_underlag",
               metrik_label = "Andel godkänt (%)", kalla = .KALLA_GRUNDSKOLA,
               beskrivning = "Andel godkänt betyg i Svenska, bland elever med ett fastställt betygsresultat i ämnet."),
      sva = list(label = "Svenska som andraspråk", klar = TRUE, vy = "andel", kon = FALSE,
                amne = "Andel godkänt i Svenska som andraspråk", metrik = "andel_sva_godkant", vikt = "sva_underlag",
                metrik_label = "Andel godkänt (%)", kalla = .KALLA_GRUNDSKOLA,
                beskrivning = "Andel godkänt betyg i Svenska som andraspråk, bland elever med ett fastställt betygsresultat i ämnet.")
    )
  )
)

# ---- UI --------------------------------------------------------------------
mod_grundskola_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "rd-segmented",
      shinyWidgets::radioGroupButtons(
        inputId  = ns("omrade"), label = NULL,
        choices  = .choices_fran_lista(grundskola_struktur),
        selected = names(grundskola_struktur)[1]
      )
    ),

    sidebarLayout(
      sidebarPanel(
        width = 3, class = "rd-sidebar",

        uiOutput(ns("indikator_ui")),
        tags$hr(),

        shinyWidgets::pickerInput(
          inputId = ns("geo_val"), label = "Område",
          choices = c("Hela Dalarna" = "_alla_", geo_val_kommun),
          selected = "_alla_",
          options = shinyWidgets::pickerOptions(liveSearch = TRUE)
        ),
        uiOutput(ns("ar_ui")),

        tags$hr(),
        div(
          class = "rd-nedladdningar",
          downloadButton(ns("ladda_ner"), "Ladda ner aktuellt urval",
                         class = "rd-btn rd-btn--ghost"),
          downloadButton(ns("ladda_ner_alla"), "Ladda ner hela datasetet",
                         class = "rd-btn rd-btn--ghost")
        )
      ),

      mainPanel(
        width = 9,
        div(
          class = "rd-card",
          div(class = "rd-brodsmula", textOutput(ns("brodsmula"))),
          uiOutput(ns("vy"))
        )
      )
    )
  )
}

# ---- Server ----------------------------------------------------------------
mod_grundskola_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$indikator_ui <- renderUI({
      omr <- grundskola_struktur[[ req(input$omrade) ]]
      ch  <- .choices_fran_lista(omr$indikatorer)
      besk <- omr$indikatorer |> purrr::map("beskrivning") |> purrr::compact()

      knappar <- shinyWidgets::radioGroupButtons(
        inputId = ns("indikator"), label = "Indikator",
        choices = ch, selected = unname(ch)[1], individual = TRUE
      )

      tagList(
        div(class = "rd-indikator-knappar", knappar),
        if (length(besk) > 0)
          tags$script(
            type = "application/json",
            class = "rd-tooltip-data",
            `data-input-id`  = ns("indikator"),
            `data-tooltips`  = as.character(jsonlite::toJSON(besk, auto_unbox = TRUE))
          )
      )
    })

    aktuell_data <- reactive({ hamta_grundskola_data() })

    valt_indikator <- reactive({
      req(input$omrade)
      ind_list <- grundskola_struktur[[input$omrade]]$indikatorer
      req(input$indikator %in% names(ind_list))
      ind_list[[input$indikator]]
    })
    valt_vy <- reactive({
      v <- valt_indikator()$vy
      if (is.null(v)) "dashboard" else v
    })

    geo_label <- reactive({
      gv <- input$geo_val
      if (is.null(gv) || gv == "_alla_") return("Dalarna")
      dalarna_kommuner$kommun[match(gv, dalarna_kommuner$kommkod)]
    })
    filter_underrubrik <- function(med_ar = FALSE) {
      txt <- geo_label()
      if (med_ar) txt <- paste0(txt, " år ", req(input$ar))
      txt
    }

    output$ar_ui <- renderUI({
      d <- aktuell_data()
      tidigare_ar <- isolate(input$ar)
      ar <- sort(unique(d$ar), decreasing = TRUE)
      behall <- if (!is.null(tidigare_ar) && tidigare_ar %in% as.character(ar)) tidigare_ar else max(ar)
      selectInput(ns("ar"), "År", choices = ar, selected = behall)
    })

    # "Hela Dalarna" = den redan färdiga länsraden (geo_niva == "lan"),
    # ingen egen viktning behövs - samma mönster som gymnasiet/YH.
    data_bas <- reactive({
      d  <- aktuell_data()
      gv <- req(input$geo_val)
      if (gv == "_alla_") dplyr::filter(d, geo_niva == "lan")
      else dplyr::filter(d, geo_niva == "kommun", kommkod == gv)
    })
    data_ar <- reactive({
      req(input$ar)
      dplyr::filter(data_bas(), ar == as.integer(input$ar))
    })

    output$brodsmula <- renderText({
      omr <- grundskola_struktur[[req(input$omrade)]]$label
      paste0("Grundskola \u203a ", omr, " \u203a ", valt_indikator()$label)
    })

    output$vy <- renderUI({
      ind <- valt_indikator()
      if (!isTRUE(ind$klar)) return(div(class = "rd-info", "Den här vyn är inte inlagd än."))
      fluidRow(
        column(7, ggiraph::girafeOutput(ns("d_bar"), height = "420px")),
        column(5, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_trend"), height = "380px")))
      )
    })

    output$d_bar <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      df  <- data_ar()
      validate(need(nrow(df) > 0, "Inga data för valt urval."))
      sub <- filter_underrubrik(med_ar = TRUE)
      skapa_diagram_bar_andel(df, ind$metrik, ind$vikt, ind$metrik_label, input$ar,
                              rubrik = paste0(ind$amne, " efter kön"),
                              underrubrik = sub, kalla = ind$kalla,
                              enhet = if (is.null(ind$enhet)) " %" else ind$enhet)
    })

    output$d_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      df  <- data_bas()
      validate(need(nrow(df) > 0, "Inga data."))
      rub <- paste0(ind$amne, " \u2013 utveckling över tid")
      sub <- filter_underrubrik()
      skapa_diagram_trend_andel(df, ind$metrik, ind$vikt, ind$metrik_label, NULL,
                                rubrik = rub, underrubrik = sub, kalla = ind$kalla,
                                enhet = if (is.null(ind$enhet)) " %" else ind$enhet)
    })

    output$ladda_ner <- downloadHandler(
      filename = function() paste0("grundskola_", input$indikator, "_", input$ar, ".xlsx"),
      content  = function(file) skriv_gymnasie_excel(data_ar(), file, blad = "Grundskola")
    )
    output$ladda_ner_alla <- downloadHandler(
      filename = function() "grundskola_hela_datasetet.xlsx",
      content  = function(file) skriv_gymnasie_excel(aktuell_data(), file, blad = "Grundskola")
    )
  })
}
