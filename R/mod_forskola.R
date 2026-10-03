# ============================================================
#  mod_forskola.R
#  Shiny-modul för skolformen Förskola (barn 2-5 år).
#
#  Samma upplägg som Grundskola-modulen: stapeldiagrammet visar Hela
#  Dalarna och alla kommuner för valt år, könsuppdelat eller totalt.
#  Valt område (filtret eller klick på en stapel) styr trenden.
#  Under staplarna väljs könsuppdelat, totalt eller åldersgrupper (2-5 år).
#  Född i Sverige/utomlands är ett filter i sidopanelen.
# ============================================================

.KALLA_FORSKOLA <- "SCB, Förskolan"

forskola_struktur <- list(
  inskrivning = list(
    label = "Inskrivna barn",
    indikatorer = list(
      andel_inskrivna = list(label = "Andel inskrivna", klar = TRUE, vy = "andel", kon = FALSE,
                             amne = "Andel barn inskrivna i förskolan", metrik = "andel_inskrivna",
                             vikt = "antal_barn", metrik_label = "Andel inskrivna (%)",
                             kalla = .KALLA_FORSKOLA,
                             beskrivning = "Andel av alla barn 2-5 år som är inskrivna i förskolan.")
    )
  )
)

.forskola_bakgrund_val <- c(
  "Alla"          = "_alla_",
  "Inrikes födda" = "Inrikes födda",
  "Utrikes födda" = "Utrikes födda"
)

# Färger för åldersgrupperna: RD:s blå skala, ljust (yngst) till mörkt.
FORSKOLA_ALDER_FARGER <- c(
  "2 år" = rd_farg("rd-blue-light", "#8edded"),
  "3 år" = rd_farg("rd-accent",     "#54a1bd"),
  "4 år" = rd_farg("rd-primary",    "#158daf"),
  "5 år" = rd_farg("rd-blue-deep",  "#0074a2")
)

# ---- UI --------------------------------------------------------------------
mod_forskola_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "rd-segmented",
      shinyWidgets::radioGroupButtons(
        inputId  = ns("omrade"), label = NULL,
        choices  = .choices_fran_lista(forskola_struktur),
        selected = names(forskola_struktur)[1]
      )
    ),

    sidebarLayout(
      sidebarPanel(
        width = 3, class = "rd-sidebar",

        uiOutput(ns("indikator_ui")),
        tags$hr(),

        shinyWidgets::pickerInput(
          inputId = ns("geo_val"), label = "Område (hemkommun)",
          choices = c("Hela Dalarna" = "_alla_", geo_val_kommun),
          selected = "_alla_",
          options = shinyWidgets::pickerOptions(liveSearch = TRUE)
        ),
        uiOutput(ns("ar_ui")),
        shinyWidgets::pickerInput(
          inputId = ns("bakgrund"), label = "Födelseland",
          choices = .forskola_bakgrund_val, selected = "_alla_"
        ),

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
mod_forskola_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$indikator_ui <- renderUI({
      omr <- forskola_struktur[[ req(input$omrade) ]]
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

    # Könsuppdelat (förvalt), totalt eller åldersgrupper - styr både staplar
    # och trend.
    lage <- reactive({ if (is.null(input$kon_lage)) "kon" else input$kon_lage })
    kon_uppdelat <- reactive({ lage() != "total" })
    grupp_farger <- reactive({ if (lage() == "alder") FORSKOLA_ALDER_FARGER else NULL })

    # Filtrerat på bakgrund och summerat till år x område x kön/åldersgrupp.
    aktuell_data <- reactive({
      forskola_summera(hamta_forskola_data(), input$bakgrund,
                       grupp = if (lage() == "alder") "alder" else "kon")
    })

    valt_indikator <- reactive({
      req(input$omrade)
      ind_list <- forskola_struktur[[input$omrade]]$indikatorer
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
    # Valda filter som text, t.ex. "2-5 år · Utrikes födda".
    filter_text <- function() {
      b <- input$bakgrund
      paste(c("2-5 år", if (!is.null(b) && b != "_alla_") b), collapse = " \u00b7 ")
    }

    filter_underrubrik <- function(med_ar = FALSE) {
      txt <- paste0(geo_label(), " \u00b7 ", filter_text())
      if (med_ar) txt <- paste0(txt, " \u00b7 år ", req(input$ar))
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

    # Alla kommuner + länet för valt år (stapeldiagrammet).
    data_ar_alla <- reactive({
      req(input$ar)
      dplyr::filter(aktuell_data(), ar == as.integer(input$ar),
                    geo_niva %in% c("lan", "kommun"))
    })

    # Klick på en stapel väljer området i filtret (länet = Hela Dalarna).
    observeEvent(input$d_bar_selected, {
      sel <- input$d_bar_selected
      if (length(sel) != 1) return()
      shinyWidgets::updatePickerInput(session, "geo_val",
                                      selected = if (sel == "20") "_alla_" else sel)
    })

    output$brodsmula <- renderText({
      omr <- forskola_struktur[[req(input$omrade)]]$label
      paste0("Förskola \u203a ", omr, " \u203a ", valt_indikator()$label)
    })

    output$vy <- renderUI({
      ind <- valt_indikator()
      if (!isTRUE(ind$klar)) return(div(class = "rd-info", "Den här vyn är inte inlagd än."))
      hint <- tags$p(class = "rd-hint rd-hint--bar",
                     "Klicka på en stapel för att se utvecklingen över tid för kommunen.")
      fluidRow(
        column(7, ggiraph::girafeOutput(ns("d_bar"), height = "760px"), hint,
               div(class = "rd-kon-kontroll",
                   shinyWidgets::radioGroupButtons(
                     inputId  = ns("kon_lage"), label = NULL,
                     choices  = c("Könsuppdelat" = "kon", "Totalt" = "total",
                                  "Åldersgrupper" = "alder"),
                     selected = isolate(if (is.null(input$kon_lage)) "kon" else input$kon_lage),
                     size = "sm"))),
        column(5, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_trend"), height = "380px")))
      )
    })

    output$d_bar <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      df  <- data_ar_alla()
      validate(need(nrow(df) > 0, "Inga data för valt urval."))
      skapa_diagram_andel_omrade_kon(df, ind$metrik, ind$vikt, ind$metrik_label, input$ar,
                                     vald_kommkod = input$geo_val,
                                     kon_uppdelat = kon_uppdelat(),
                                     grupp_farger = grupp_farger(),
                                     rubrik = paste0(ind$amne, " efter kommun",
                                                     switch(lage(), kon = " och kön",
                                                            alder = " och ålder", "")),
                                     underrubrik = paste0("Hemkommun \u00b7 ", filter_text(),
                                                          " \u00b7 år ", input$ar),
                                     kalla = ind$kalla,
                                     enhet = if (is.null(ind$enhet)) " %" else ind$enhet)
    })

    output$d_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      df  <- data_bas()
      validate(need(nrow(df) > 0, "Inga data."))
      rub <- paste0(ind$amne, " \u2013 utveckling över tid")
      sub <- filter_underrubrik()
      enh <- if (is.null(ind$enhet)) " %" else ind$enhet
      if (kon_uppdelat()) {
        skapa_diagram_trend_andel_kon(df, ind$metrik, ind$vikt, ind$metrik_label,
                                      rubrik = rub, underrubrik = sub, kalla = ind$kalla,
                                      enhet = enh, grupp_farger = grupp_farger())
      } else {
        skapa_diagram_trend_andel(df, ind$metrik, ind$vikt, ind$metrik_label, NULL,
                                  rubrik = rub, underrubrik = sub, kalla = ind$kalla,
                                  enhet = enh)
      }
    })

    output$ladda_ner <- downloadHandler(
      filename = function() paste0("forskola_", input$indikator, "_", input$ar, ".xlsx"),
      content  = function(file) skriv_gymnasie_excel(data_ar(), file, blad = "Förskola")
    )
    output$ladda_ner_alla <- downloadHandler(
      filename = function() "forskola_hela_datasetet.xlsx",
      content  = function(file) skriv_gymnasie_excel(hamta_forskola_data(), file, blad = "Förskola")
    )
  })
}
