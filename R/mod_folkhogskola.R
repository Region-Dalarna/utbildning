# ============================================================
#  mod_folkhogskola.R
#  Shiny-modul för skolformen Folkhögskola.
#
#  Samma mönster som mod_komvux.R. "program" = ämnesområde (grupperade
#  kursinriktningar, se func_data_folkhogskola.R). Huvudman (Region/Enskild)
#  är ett eget filter, inte en diagramdimension.
#
#  Kommunvalet visar bara kommuner med folkhögskola (datan är per skolans
#  kommun).
# ============================================================

.KALLA_FOLKHOGSKOLA <- "SCB, Folkhögskolan"

folkhogskola_struktur <- list(
  deltagande = list(
    label = "Deltagande",
    indikatorer = list(
      kursdeltaganden = list(label = "Kursdeltaganden", klar = TRUE, vy = "dashboard", kon = FALSE,
                             amne = "Antal kursdeltaganden",
                             metrik = "kursdeltaganden", metrik_label = "Antal kursdeltaganden",
                             kalla = .KALLA_FOLKHOGSKOLA,
                             beskrivning = "Antal kursdeltaganden under året (en person kan gå flera kurser)."),
      # klar = FALSE: deltagare är unika per kursinriktning och huvudman och
      # dubbelräknas när de summeras (se func_data_folkhogskola.R).
      deltagare = list(label = "Deltagare", klar = FALSE, vy = "dashboard", kon = FALSE,
                       amne = "Antal deltagare",
                       metrik = "deltagare", metrik_label = "Antal deltagare (unika individer)",
                       kalla = .KALLA_FOLKHOGSKOLA,
                       beskrivning = "Antal unika individer som deltagit i en kurs under året."),
      andel_avbrott = list(label = "Andel avbrott", klar = TRUE, vy = "andel", kon = FALSE,
                           amne = "Andel avbrott", metrik = "andel_avbrott", vikt = "kursdeltaganden",
                           metrik_label = "Andel avbrott (%)", kalla = .KALLA_FOLKHOGSKOLA,
                           beskrivning = "Andel av samtliga kursdeltaganden som slutat med avbrott.")
    )
  ),
  resultat = list(
    label = "Resultat",
    indikatorer = list(
      grundlbeh = list(label = "Grundläggande behörighet", klar = TRUE, vy = "dashboard", kon = FALSE,
                       amne = "Antal med grundläggande behörighet",
                       metrik = "grundlbeh", metrik_label = "Antal med grundläggande behörighet",
                       kalla = .KALLA_FOLKHOGSKOLA,
                       beskrivning = "Antal deltagare som uppnått grundläggande behörighet för högskolestudier."),
      eftergym_klar = list(label = "Intyg eftergymnasial utbildning", klar = TRUE, vy = "dashboard", kon = FALSE,
                           amne = "Antal med intyg om eftergymnasial utbildning",
                           metrik = "eftergym_klar", metrik_label = "Antal med intyg",
                           kalla = .KALLA_FOLKHOGSKOLA,
                           beskrivning = "Antal deltagare som fått intyg om genomförd eftergymnasial utbildning."),
      yrkesbeh = list(label = "Behörighet till YH", klar = TRUE, vy = "dashboard", kon = FALSE,
                      amne = "Antal med behörighet till yrkeshögskolan",
                      metrik = "yrkesbeh", metrik_label = "Antal med behörighet till YH",
                      kalla = .KALLA_FOLKHOGSKOLA,
                      beskrivning = "Antal deltagare som uppnått behörighet till yrkeshögskolan.")
    )
  )
)

# ---- UI --------------------------------------------------------------------
mod_folkhogskola_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "rd-segmented",
      shinyWidgets::radioGroupButtons(
        inputId  = ns("omrade"), label = NULL,
        choices  = .choices_fran_lista(folkhogskola_struktur),
        selected = names(folkhogskola_struktur)[1]
      )
    ),

    sidebarLayout(
      sidebarPanel(
        width = 3, class = "rd-sidebar",

        uiOutput(ns("indikator_ui")),
        tags$hr(),

        shinyWidgets::radioGroupButtons(
          inputId = ns("huvudman"), label = "Huvudman",
          choices = c("Alla" = "_alla_", "Region" = "Region", "Enskild" = "Enskild"),
          selected = "_alla_"
        ),
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
mod_folkhogskola_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$indikator_ui <- renderUI({
      omr <- folkhogskola_struktur[[ req(input$omrade) ]]
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

    aktuell_data <- reactive({ hamta_folkhogskola_data() })

    valt_indikator <- reactive({
      req(input$omrade)
      ind_list <- folkhogskola_struktur[[input$omrade]]$indikatorer
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
      bitar <- geo_label()
      hm <- input$huvudman
      if (!is.null(hm) && hm != "_alla_") bitar <- c(bitar, paste("Huvudman:", hm))
      txt <- paste(bitar, collapse = " \u00b7 ")
      if (med_ar) txt <- paste0(txt, " år ", req(input$ar))
      txt
    }

    filtrera_huvudman <- function(d, hm) {
      if (!is.null(hm) && hm != "_alla_") dplyr::filter(d, organisationstyp == hm) else d
    }

    # Bara kommuner som har folkhögskola (rader) i datan.
    observe({
      d <- aktuell_data()
      finns <- geo_val_kommun[geo_val_kommun %in% d$kommkod[d$geo_niva == "kommun"]]
      shinyWidgets::updatePickerInput(session, "geo_val",
                                      choices = c("Hela Dalarna" = "_alla_", finns),
                                      selected = isolate(input$geo_val))
    })

    program_vald <- reactiveVal(NULL)
    observeEvent(input$d_bar_selected, {
      sel <- input$d_bar_selected
      program_vald(if (length(sel) >= 1) sel else NULL)
    }, ignoreNULL = FALSE)
    observeEvent(input$omrade, { program_vald(NULL) }, ignoreInit = TRUE)

    output$ar_ui <- renderUI({
      d <- aktuell_data()
      tidigare_ar <- isolate(input$ar)
      ar <- sort(unique(d$ar), decreasing = TRUE)
      behall <- if (!is.null(tidigare_ar) && tidigare_ar %in% as.character(ar)) tidigare_ar else max(ar)
      selectInput(ns("ar"), "År", choices = ar, selected = behall)
    })

    # Aggregerar bort kommunraderna till en Dalarna-totalrad per (ar, program)
    # när "Hela Dalarna" (geo_niva == "lan") väljs - datan har redan en
    # färdig länsrad från uttaget, ingen egen viktning behövs.
    data_bas <- reactive({
      d  <- aktuell_data()
      gv <- req(input$geo_val)
      d  <- if (gv == "_alla_") dplyr::filter(d, geo_niva == "lan")
      else dplyr::filter(d, geo_niva == "kommun", kommkod == gv)
      filtrera_huvudman(d, input$huvudman)
    })
    data_ar <- reactive({
      req(input$ar)
      dplyr::filter(data_bas(), ar == as.integer(input$ar))
    })

    output$brodsmula <- renderText({
      omr <- folkhogskola_struktur[[req(input$omrade)]]$label
      paste0("Folkhögskola \u203a ", omr, " \u203a ", valt_indikator()$label)
    })

    output$vy <- renderUI({
      ind <- valt_indikator()
      if (!isTRUE(ind$klar)) return(div(class = "rd-info", "Den här vyn är inte inlagd än."))
      hint <- tags$p(class = "rd-hint rd-hint--bar",
                     "Klicka på en stapel i diagrammet för att se statistik för ett specifikt ämnesområde.")
      fluidRow(
        column(7, ggiraph::girafeOutput(ns("d_bar"), height = "560px"), hint),
        column(5, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_trend"), height = "380px")))
      )
    })

    output$d_bar <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      df  <- data_ar()
      validate(need(nrow(df) > 0, "Inga data för valt urval."))
      sub <- filter_underrubrik(med_ar = TRUE)

      if (valt_vy() == "andel") {
        skapa_diagram_bar_andel(df, ind$metrik, ind$vikt, ind$metrik_label, input$ar,
                                rubrik = paste0(ind$amne, " efter ämnesområde"),
                                underrubrik = sub, kalla = ind$kalla)
      } else {
        skapa_diagram_bar(df, ind$metrik, ind$metrik_label, input$ar,
                          rubrik = paste0(ind$amne, " efter ämnesområde"),
                          underrubrik = sub, kalla = ind$kalla)
      }
    })

    output$d_trend <- ggiraph::renderGirafe({
      ind  <- valt_indikator(); req(isTRUE(ind$klar))
      df   <- data_bas()
      validate(need(nrow(df) > 0, "Inga data."))
      prog <- program_vald()
      rub  <- if (is.null(prog)) paste0(ind$amne, " \u2013 utveckling över tid") else paste0(ind$amne, " \u2013 ", prog)
      sub  <- filter_underrubrik()

      if (valt_vy() == "andel") {
        skapa_diagram_trend_andel(df, ind$metrik, ind$vikt, ind$metrik_label, prog,
                                  rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      } else {
        skapa_diagram_trend(df, ind$metrik, ind$metrik_label, prog,
                            rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      }
    })

    output$ladda_ner <- downloadHandler(
      filename = function() paste0("folkhogskola_", input$omrade, "_", input$indikator, "_", input$ar, ".xlsx"),
      content  = function(file) skriv_gymnasie_excel(data_ar(), file, blad = "Folkhögskola")
    )
    output$ladda_ner_alla <- downloadHandler(
      filename = function() "folkhogskola_hela_datasetet.xlsx",
      content  = function(file) skriv_gymnasie_excel(aktuell_data(), file, blad = "Folkhögskola")
    )
  })
}
