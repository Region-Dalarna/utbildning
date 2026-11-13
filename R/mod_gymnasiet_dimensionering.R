# ============================================================
#  mod_gymnasiet_dimensionering.R
#  Statistikområdet "Dimensionering" i Gymnasiet-fliken: hur gymnasiets
#  platser fördelar sig per program, jämfört med Skolverkets bedömning av
#  hur de borde fördelas (planeringsunderlaget för Dalarnas län).
#
#  Inbäddad i mod_gymnasiet.R på samma sätt som Avgångna (mod_gymnasiet_
#  avgangna.R): en egen, enklare modul utanför gymnasiet_struktur, eftersom
#  den jämför mot en länsövergripande bedömning i stället för att borra ner
#  per skola/kommun.
#
#  Område väljs som ETT val (inte flera) - precis som i det ursprungliga
#  Excel-underlaget ("körningar" per samverkansområde) - och återanvänder
#  Gysam-indelningen i def_geografi.R (kommun_samverkan, GYSAM_HELA,
#  samverkan_i()). Skolverkets bedömning gäller hela Dalarna oavsett vilket
#  område som väljs - det förtydligas i en notis under diagrammet.
# ============================================================

.KALLA_DIMENSIONERING <- paste(
  "Gymnasieantagningen (Dalarnas kommunförbund) och Skolverkets",
  "planeringsunderlag för gymnasial utbildning, Dalarnas län")

dimensionering_struktur <- list(
  jamforelse = list(label = "Platser vs bedömning", klar = TRUE,
                    beskrivning = paste(
                      "Andelen av platserna per nationellt program i gymnasieskolan,",
                      "jämfört med Skolverkets bedömning av hur platserna borde",
                      "fördelas läsåret 2026/27.")),
  avvikelse  = list(label = "Avvikelse mot bedömning", klar = TRUE,
                    beskrivning = paste(
                      "Skillnaden, i procentenheter, mellan andelen platser och Skolverkets",
                      "bedömda andel per program. Positivt värde: fler platser än",
                      "bedömningen. Negativt värde: färre."))
)

# ---- UI --------------------------------------------------------------------
mod_gymnasiet_dimensionering_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3, class = "rd-sidebar",
      uiOutput(ns("indikator_ui")),
      tags$hr(),
      shinyWidgets::pickerInput(
        inputId = ns("geo_val"), label = "Område",
        choices = list(
          "Hela länet"     = c("Hela Dalarna" = "_alla_"),
          "Gysam"          = c("Hela Gysam" = GYSAM_HELA, geo_val_samverkan),
          "Enskild kommun" = geo_val_kommun
        ),
        selected = GYSAM_HELA,
        options = shinyWidgets::pickerOptions(liveSearch = TRUE)
      ),
      uiOutput(ns("org_ui")),
      uiOutput(ns("ar_ui")),
      tags$hr(),
      div(
        class = "rd-nedladdningar",
        downloadButton(ns("ladda_ner"), "Ladda ner aktuellt urval", class = "rd-btn rd-btn--ghost"),
        downloadButton(ns("ladda_ner_alla"), "Ladda ner Skolverkets bedömning", class = "rd-btn rd-btn--ghost")
      )
    ),
    mainPanel(
      width = 9,
      div(
        class = "rd-card",
        div(class = "rd-brodsmula", textOutput(ns("brodsmula"))),
        ggiraph::girafeOutput(ns("d_bar"), height = "620px"),
        uiOutput(ns("notiser"))
      )
    )
  )
}

# ---- Server ----------------------------------------------------------------
mod_gymnasiet_dimensionering_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$indikator_ui <- renderUI({
      ch   <- .choices_fran_lista(dimensionering_struktur)
      besk <- dimensionering_struktur |> purrr::map("beskrivning") |> purrr::compact()
      tagList(
        div(class = "rd-indikator-knappar",
            shinyWidgets::radioGroupButtons(
              inputId = ns("indikator"), label = "Indikator",
              choices = ch, selected = unname(ch)[1], individual = TRUE)),
        tags$script(type = "application/json", class = "rd-tooltip-data",
                    `data-input-id` = ns("indikator"),
                    `data-tooltips` = as.character(jsonlite::toJSON(besk, auto_unbox = TRUE)))
      )
    })
    valt_indikator <- reactive({
      req(input$indikator %in% names(dimensionering_struktur))
      dimensionering_struktur[[input$indikator]]
    })

    # Valt område -> kommunkoder. "_alla_" = hela Dalarna, GYSAM_HELA/ett
    # samverkansområde expanderas via samverkan_i(), annars är valet redan
    # en enskild kommunkod.
    omr_kod <- reactive(if (is.null(input$geo_val)) GYSAM_HELA else input$geo_val)
    omr_kommkoder <- reactive({
      v <- omr_kod()
      if (identical(v, "_alla_")) return(dalarna_kommuner$kommkod)
      if (identical(v, GYSAM_HELA) || v %in% geo_val_samverkan)
        return(kommun_samverkan$kommkod[samverkan_i(kommun_samverkan$samverkansomrade, v)])
      v
    })
    omr_namn <- reactive({
      v <- omr_kod()
      if (identical(v, "_alla_")) "Dalarna"
      else if (identical(v, GYSAM_HELA) || v %in% geo_val_samverkan) v
      else dalarna_kommuner$kommun[match(v, dalarna_kommuner$kommkod)]
    })

    platser_omr <- reactive(dplyr::filter(hamta_gymnasiedata(), kommkod %in% omr_kommkoder()))

    output$org_ui <- renderUI({
      typer <- sort(unique(stats::na.omit(platser_omr()$organisationstyp)))
      if (length(typer) <= 1) return(NULL)
      shinyWidgets::radioGroupButtons(
        inputId = ns("organisationstyp"), label = "Driftsform",
        choices = c("Alla" = "_alla_", stats::setNames(typer, typer)),
        selected = isolate(if (!is.null(input$organisationstyp) &&
                               input$organisationstyp %in% c("_alla_", typer))
          input$organisationstyp else "_alla_")
      )
    })
    org <- reactive(if (is.null(input$organisationstyp)) "_alla_" else input$organisationstyp)

    output$ar_ui <- renderUI({
      ar <- sort(unique(hamta_gymnasiedata()$ar), decreasing = TRUE)
      tidigare <- isolate(input$ar)
      selectInput(ns("ar"), "Antagningsår", choices = ar,
                  selected = if (!is.null(tidigare) && tidigare %in% ar) tidigare else max(ar))
    })

    platser_ar <- reactive({
      d <- dplyr::filter(platser_omr(), ar == as.integer(req(input$ar)))
      if (org() != "_alla_") d <- dplyr::filter(d, organisationstyp == org())
      d
    })

    jamforelse <- reactive({
      d <- dimensionering_jamforelse(platser_ar())
      validate(need(nrow(d) > 0, "Inga data för valt urval."))
      d
    })

    underrubrik <- reactive(paste0(
      omr_namn(), " · antagning ", input$ar,
      if (org() != "_alla_") paste0(" · driftsform: ", org()) else ""))

    output$brodsmula <- renderText(paste0("Gymnasiet › Dimensionering › ", valt_indikator()$label))

    output$d_bar <- ggiraph::renderGirafe({
      d <- jamforelse()
      if (identical(input$indikator, "avvikelse")) {
        skapa_diagram_dimensionering_avvikelse(
          d, rubrik = "Avvikelse mot Skolverkets bedömning, nationella program",
          underrubrik = underrubrik(), kalla = .KALLA_DIMENSIONERING)
      } else {
        skapa_diagram_dimensionering_jamforelse(
          d, rubrik = "Andel platser jämfört med Skolverkets bedömning",
          underrubrik = underrubrik(), kalla = .KALLA_DIMENSIONERING)
      }
    })

    output$notiser <- renderUI({
      n <- c(
        paste("Skolverkets bedömning avser läsåret 2026/27 och gäller hela Dalarna -",
              "jämförelsen görs mot den oavsett vilket område som är valt ovan."),
        paste("Bara gymnasieskolans 18 nationella program ingår. Introduktions-",
              "programmet, komvux och inriktningsnivå (som saknar egen bedömning",
              "i planeringsunderlaget) räknas inte med.")
      )
      saknas <- dimensionering_okanda_program()
      if (length(saknas) > 0) {
        n <- c(n, paste0("Hittar inga platser alls för: ", paste(saknas, collapse = ", "),
                         " - kontrollera att programnamnen stavas likadant i de två källorna."))
      }
      tags$ul(class = "rd-hint", lapply(n, tags$li))
    })

    # ---- Nedladdning --------------------------------------------------------
    output$ladda_ner <- downloadHandler(
      filename = function() paste0("gymnasiet_dimensionering_", input$ar, ".xlsx"),
      content  = function(file) {
        d <- dplyr::select(jamforelse(), program, platser, andel_platser,
                           andel_bedomning, avvikelse, forandringsriktning)
        skriv_gymnasie_excel(d, file, blad = "Dimensionering", min_antal = 0)
      }
    )
    output$ladda_ner_alla <- downloadHandler(
      filename = function() "skolverkets_bedomning_dimensionering.xlsx",
      content  = function(file) skriv_gymnasie_excel(
        hamta_skolverket_dimensionering(), file, blad = "Skolverkets bedömning", min_antal = 0)
    )
  })
}
