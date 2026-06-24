# ============================================================
#  mod_gymnasiet.R
#  Shiny-modul för skolformen Gymnasiet.
#
#  N2 Statistikområde -> radioGroupButtons (segmenterad knapprad)
#  N3 Indikator       -> radioGroupButtons (knappar i 2-kolumnersrutnät)
#
#  Klara indikatorer ritas som dashboard: stapel (klickbar) + utveckling
#  över tid + andel programtyp. Fältet "kon = TRUE" ger en kontroll under
#  stapeln (Könsuppdelat/Totalt); då används metrik_kv/metrik_man och både
#  stapel och trend blir könsuppdelade.
# ============================================================

.KALLA_ANTAGNING <- "Gymnasieantagningen, Dalarnas kommunförbund"

gymnasiet_struktur <- list(
  antagning = list(
    label = "Antagning",
    indikatorer = list(
      platser_program = list(label = "Platser efter program", klar = TRUE, kon = FALSE,
                             metrik = "platser", metrik_label = "Antal platser",
                             kalla = .KALLA_ANTAGNING),
      antagna_program = list(label = "Antagna efter program", klar = TRUE, kon = TRUE,
                             metrik = "antagna", metrik_kv = "antagna_kv", metrik_man = "antagna_man",
                             metrik_label = "Antal antagna", kalla = .KALLA_ANTAGNING),
      sokande_forsta  = list(label = "Sökande, första hand", klar = TRUE, kon = TRUE,
                             metrik = "sok_1a", metrik_kv = "sok_1a_kv", metrik_man = "sok_1a_man",
                             metrik_label = "Förstahandssökande", kalla = .KALLA_ANTAGNING),
      outnyttjade     = list(label = "Outnyttjade platser", klar = TRUE, kon = FALSE,
                             metrik = "lediga_platser", metrik_label = "Outnyttjade platser",
                             kalla = .KALLA_ANTAGNING),
      antagningspoang = list(label = "Antagningspoäng", klar = FALSE, kalla = .KALLA_ANTAGNING)
    )
  ),
  elever = list(
    label = "Elever",
    indikatorer = list(
      elever_program = list(label = "Elever per program", klar = FALSE, kalla = "Skolverket"),
      elever_skolar  = list(label = "Elever per skolår",  klar = FALSE, kalla = "Skolverket"),
      behoriga       = list(label = "Andel behöriga",     klar = FALSE, kalla = "Skolverket")
    )
  ),
  resultat = list(
    label = "Resultat & examen",
    indikatorer = list(
      examen = list(label = "Andel med examen", klar = FALSE, kalla = "Skolverket")
    )
  ),
  etablering = list(
    label = "Etablering efter gymnasiet",
    indikatorer = list(
      etablering = list(label = "Sysselsättning/studier efter examen", klar = FALSE, kalla = "SCB")
    )
  )
)

.choices_fran_lista <- function(x) {
  labels <- vapply(x, function(e) e$label, character(1))
  stats::setNames(names(x), unname(labels))
}

# ---- UI --------------------------------------------------------------------
mod_gymnasiet_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "rd-segmented",
      shinyWidgets::radioGroupButtons(
        inputId  = ns("omrade"), label = NULL,
        choices  = .choices_fran_lista(gymnasiet_struktur),
        selected = names(gymnasiet_struktur)[1]
      )
    ),

    sidebarLayout(
      sidebarPanel(
        width = 3, class = "rd-sidebar",

        uiOutput(ns("indikator_ui")),
        tags$hr(),

        shinyWidgets::radioGroupButtons(
          inputId = ns("geo_niva"), label = "Geografisk indelning",
          choices = c("Kommun" = "kommun", "Samverkansområde" = "samverkansomrade"),
          selected = "kommun"
        ),
        shinyWidgets::pickerInput(
          inputId = ns("geo_val"), label = "Område", choices = NULL,
          options = shinyWidgets::pickerOptions(liveSearch = TRUE)
        ),
        uiOutput(ns("org_ui")),
        uiOutput(ns("ar_ui")),

        tags$hr(),
        tags$div(class = "rd-label", "Samverkansområden"),
        ggiraph::girafeOutput(ns("karta"), height = "320px"),

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
          h2(textOutput(ns("titel"))),
          div(class = "rd-subtitle", textOutput(ns("brodsmula"))),
          uiOutput(ns("vy")),
          uiOutput(ns("kalla"))
        )
      )
    )
  )
}

# ---- Server ----------------------------------------------------------------
mod_gymnasiet_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$indikator_ui <- renderUI({
      omr <- gymnasiet_struktur[[ req(input$omrade) ]]
      ch  <- .choices_fran_lista(omr$indikatorer)
      div(
        class = "rd-indikator-knappar",
        shinyWidgets::radioGroupButtons(
          inputId = ns("indikator"), label = "Indikator",
          choices = ch, selected = unname(ch)[1], individual = TRUE
        )
      )
    })

    output$org_ui <- renderUI({
      typer <- sort(unique(stats::na.omit(hamta_gymnasiedata()$organisationstyp)))
      shinyWidgets::radioGroupButtons(
        inputId = ns("organisationstyp"), label = "Driftsform",
        choices = c("Alla" = "_alla_", stats::setNames(typer, typer)), selected = "_alla_"
      )
    })

    output$ar_ui <- renderUI({
      ar <- sort(unique(hamta_gymnasiedata()$ar), decreasing = TRUE)
      selectInput(ns("ar"), "År", choices = ar, selected = max(ar))
    })

    observeEvent(input$geo_niva, {
      val <- if (input$geo_niva == "kommun") geo_val_kommun else geo_val_samverkan
      shinyWidgets::updatePickerInput(
        session, "geo_val",
        choices = c("Hela Dalarna" = "_alla_", val), selected = "_alla_")
    }, ignoreInit = FALSE)

    # Kartklick styr Område-valet. Klick på redan vald yta avmarkerar
    # (ggiraph skickar tomt urval) -> tillbaka till Hela Dalarna.
    # Kartan ritas inte om på geo_val, så ingen återställningsloop uppstår.
    observeEvent(input$karta_selected, {
      sel <- input$karta_selected
      ny  <- if (length(sel) >= 1) sel else "_alla_"
      if (!identical(ny, input$geo_val)) {
        shinyWidgets::updatePickerInput(session, "geo_val", selected = ny)
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    valt_indikator <- reactive({
      req(input$omrade)
      ind_list <- gymnasiet_struktur[[input$omrade]]$indikatorer
      req(input$indikator %in% names(ind_list))
      ind_list[[input$indikator]]
    })

    kon_lage <- reactive({ if (is.null(input$kon_lage)) "kon" else input$kon_lage })

    geo_label <- reactive({
      gv <- input$geo_val
      if (is.null(gv) || gv == "_alla_") return("Hela Dalarna")
      if (input$geo_niva == "kommun")
        dalarna_kommuner$kommun[match(gv, dalarna_kommuner$kommkod)]
      else gv
    })

    data_bas <- reactive({
      d  <- hamta_gymnasiedata()
      gv <- req(input$geo_val)
      if (gv != "_alla_") {
        d <- if (input$geo_niva == "kommun")
          dplyr::filter(d, kommkod == gv) else dplyr::filter(d, samverkansomrade == gv)
      }
      org <- input$organisationstyp
      if (!is.null(org) && org != "_alla_") d <- dplyr::filter(d, organisationstyp == org)
      d
    })
    data_ar <- reactive({
      req(input$ar)
      dplyr::filter(data_bas(), ar == as.integer(input$ar))
    })

    program_vald <- reactiveVal(NULL)
    observeEvent(input$d_bar_selected, {
      sel <- input$d_bar_selected
      program_vald(if (length(sel) >= 1) sel else NULL)
    }, ignoreNULL = FALSE)
    observeEvent(list(input$omrade, input$indikator, input$geo_niva,
                      input$geo_val, input$organisationstyp, input$ar, input$kon_lage), {
                        program_vald(NULL)
                      }, ignoreInit = TRUE)

    output$titel     <- renderText({ valt_indikator()$label })
    output$brodsmula <- renderText({
      omr <- gymnasiet_struktur[[req(input$omrade)]]$label
      org <- input$organisationstyp
      org_txt <- if (!is.null(org) && org != "_alla_") paste0(" · ", org) else ""
      paste0("Gymnasiet › ", omr, " › ", valt_indikator()$label,
             " · ", geo_label(), org_txt, " · ", req(input$ar))
    })

    output$vy <- renderUI({
      ind <- valt_indikator()
      if (!isTRUE(ind$klar)) {
        return(div(class = "rd-info",
                   "Den här vyn är inte inlagd än – kommer i en senare version."))
      }
      fluidRow(
        column(
          7,
          tags$p(class = "rd-hint rd-hint--bar",
                 "Klicka på en stapel i diagrammet för att se statistik för ett specifikt program."),
          ggiraph::girafeOutput(ns("d_bar"), height = "470px"),
          uiOutput(ns("kon_kontroll"))
        ),
        column(
          5,
          div(class = "rd-subcard",
              tags$h4("Utveckling över tid"),
              ggiraph::girafeOutput(ns("d_trend"), height = "200px")),
          div(class = "rd-subcard",
              tags$h4("Andel efter programtyp och år"),
              ggiraph::girafeOutput(ns("d_programtyp"), height = "240px"))
        )
      )
    })

    output$kon_kontroll <- renderUI({
      if (!isTRUE(valt_indikator()$kon)) return(NULL)
      div(
        class = "rd-kon-kontroll",
        shinyWidgets::radioGroupButtons(
          inputId  = ns("kon_lage"), label = NULL,
          choices  = c("Könsuppdelat" = "kon", "Totalt" = "total"),
          selected = "kon", size = "sm"
        )
      )
    })

    output$d_bar <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      df <- data_ar()
      validate(need(nrow(df) > 0, "Inga data för valt urval."))
      if (isTRUE(ind$kon) && kon_lage() == "kon") {
        skapa_diagram_bar_kon(df, ind$metrik_kv, ind$metrik_man, ind$metrik_label, input$ar)
      } else {
        skapa_diagram_bar(df, ind$metrik, ind$metrik_label, input$ar)
      }
    })

    output$d_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      df <- data_bas()
      validate(need(nrow(df) > 0, "Inga data."))
      if (isTRUE(ind$kon) && kon_lage() == "kon") {
        skapa_diagram_trend_kon(df, ind$metrik_kv, ind$metrik_man, ind$metrik_label, program_vald())
      } else {
        skapa_diagram_trend(df, ind$metrik, ind$metrik_label, program_vald())
      }
    })

    output$d_programtyp <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      df <- data_bas()
      validate(need(nrow(df) > 0, "Inga data."))
      skapa_diagram_programtyp(df, ind$metrik, ind$metrik_label, program_vald())
    })

    output$karta <- ggiraph::renderGirafe({
      k <- skapa_karta_samverkan(input$geo_niva)
      validate(need(!is.null(k),
                    "Kartan kunde inte läsas (kräver sf och åtkomst till geodata-databasen)."))
      k
    })

    output$kalla <- renderUI({
      k <- valt_indikator()$kalla
      if (is.null(k)) return(NULL)
      div(class = "rd-caption", paste0("Källa: ", k))
    })

    output$ladda_ner <- downloadHandler(
      filename = function() paste0("gymnasiet_", input$indikator, "_", input$ar, ".xlsx"),
      content  = function(file) skriv_gymnasie_excel(data_ar(), file)
    )
    output$ladda_ner_alla <- downloadHandler(
      filename = function() "gymnasiet_hela_datasetet.xlsx",
      content  = function(file) skriv_gymnasie_excel(hamta_gymnasiedata(), file)
    )
  })
}
