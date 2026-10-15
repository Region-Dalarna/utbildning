# ============================================================
#  mod_prognos.R
#  Fliken Prognos 2040: SCB:s Regionala trender och prognoser 2025 (RegTop).
#
#  Statistikområde Bedömning 2040: arbetsmarknadsläget 2040 per
#  utbildningsgrupp i valt län (eller riket). Klick på en grupp öppnar en
#  fördjupning: tillgång och efterfrågan över tid, vad som driver
#  förändringen, arbetskraften per kön, de vanligaste yrkena, SCB:s
#  antaganden och samma grupp i alla län.
#
#  Datan finns bara för län och riket. Publicerad statistik - ingen
#  röjandekontroll. Se func_data_regtop.R om enheter (summa för perioden
#  i länen, årligt genomsnitt i riket).
# ============================================================

prognos_struktur <- list(
  bedomning = list(label = "Bedömning 2040")
)

# ---- UI --------------------------------------------------------------------
mod_prognos_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "rd-segmented",
      shinyWidgets::radioGroupButtons(
        inputId = ns("omrade"), label = NULL,
        choices = .choices_fran_lista(prognos_struktur),
        selected = names(prognos_struktur)[1]
      )
    ),
    sidebarLayout(
      sidebarPanel(
        width = 3, class = "rd-sidebar",
        uiOutput(ns("region_ui")),
        uiOutput(ns("aggregat_ui")),
        tags$hr(),
        tags$p(class = "rd-hint",
               "Prognoserna bygger på SCB:s Regionala trender och prognoser 2025 (RegTop), ",
               "publicerad i maj 2026. De visar hur tillgång och efterfrågan på arbetskraft ",
               "med olika utbildningar kan utvecklas fram till 2040 om trenderna håller i sig - ",
               "inte vad som säkert kommer att hända."),
        tags$hr(),
        div(
          class = "rd-nedladdningar",
          downloadButton(ns("ladda_ner"), "Ladda ner aktuellt urval", class = "rd-btn rd-btn--ghost"),
          downloadButton(ns("ladda_ner_alla"), "Ladda ner hela datasetet", class = "rd-btn rd-btn--ghost")
        )
      ),
      mainPanel(
        width = 9,
        div(class = "rd-card",
            div(class = "rd-brodsmula", textOutput(ns("brodsmula"))),
            uiOutput(ns("vy")))
      )
    )
  )
}

# ---- Server ----------------------------------------------------------------
mod_prognos_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$region_ui <- renderUI({
      val <- regtop_regioner()
      shinyWidgets::pickerInput(ns("region"), "Län", choices = val,
                                selected = if ("20" %in% val) "20" else val[1],
                                options = shinyWidgets::pickerOptions(liveSearch = TRUE))
    })
    output$aggregat_ui <- renderUI({
      agg <- sort(unique(regtop_bedomning()$aggregat))
      selectInput(ns("aggregat"), "Utbildningsgrupper", choices = c("Alla" = "_alla_", agg))
    })

    region <- reactive(if (is.null(input$region)) "20" else input$region)
    region_namn <- reactive(names(regtop_regioner())[match(region(), regtop_regioner())])

    # Bedömningen i valt län, filtrerad på aggregat.
    bedomning_region <- reactive({
      d <- dplyr::filter(regtop_bedomning(), regionkod == region())
      a <- input$aggregat
      if (!is.null(a) && a != "_alla_") d <- dplyr::filter(d, aggregat == a)
      d
    })

    # ---- Fördjupning: vald utbildningsgrupp --------------------------------
    grupp_vald <- reactiveVal(NULL)
    observeEvent(input$d_bedomning_selected, {
      sel <- input$d_bedomning_selected
      if (length(sel) == 1) grupp_vald(sel)
    })
    observeEvent(input$tillbaka, grupp_vald(NULL))
    grupp_rad <- reactive({
      req(grupp_vald())
      dplyr::filter(regtop_bedomning(), regionkod == region(), utbildningsgrupp_kod == grupp_vald())
    })
    grupp_namn <- reactive({
      n <- regtop_bedomning()$utbildningsgrupp[regtop_bedomning()$utbildningsgrupp_kod == grupp_vald()]
      if (length(n) == 0) grupp_vald() else n[1]
    })

    output$brodsmula <- renderText({
      paste0("Prognos 2040 › Bedömning 2040",
             if (!is.null(grupp_vald())) paste0(" › ", grupp_namn()))
    })

    output$vy <- renderUI({
      if (is.null(grupp_vald())) {
        utan <- dplyr::filter(bedomning_region(), !har_bedomning)$utbildningsgrupp
        # Höjden följer antalet grupper (samma proportioner som diagrammet).
        n <- sum(bedomning_region()$har_bedomning)
        return(fluidRow(
          column(7,
                 ggiraph::girafeOutput(ns("d_bedomning"), height = paste0(max(400, n * 22 + 200), "px")),
                 tags$p(class = "rd-hint rd-hint--bar",
                        "Klicka på en utbildningsgrupp för att se vad som ligger bakom bedömningen."),
                 if (length(utan) > 0)
                   tags$p(class = "rd-hint", paste0("Ingen bedömning för 2040 (SCB gör ingen framskrivning): ",
                                                    paste(utan, collapse = ", "), "."))),
          column(5,
                 div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_antal"), height = "330px")),
                 div(class = "rd-subcard", .regtop_lasanvisning()))
        ))
      }
      tagList(
        actionLink(ns("tillbaka"), "← Alla utbildningsgrupper", class = "rd-tillbaka"),
        tags$h3(class = "rd-rubrik-grupp", grupp_namn(), tags$small(paste0(" · ", region_namn()))),
        fluidRow(
          column(7, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_tillgang"), height = "360px"))),
          column(5, div(class = "rd-subcard", uiOutput(ns("faktaruta"))))
        ),
        fluidRow(
          column(7, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_vattenfall"), height = "360px"))),
          column(5, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_efterfragan"), height = "330px")))
        ),
        fluidRow(
          column(7, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_lan"), height = "520px"))),
          column(5, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_karta"), height = "520px"),
                        tags$p(class = "rd-hint", "Klicka på ett län för att byta län.")))
        ),
        fluidRow(
          column(7, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_kon"), height = "330px")))
        )
      )
    })

    # ---- Översikt -------------------------------------------------------------
    output$d_bedomning <- ggiraph::renderGirafe({
      d <- bedomning_region()
      validate(need(nrow(d) > 0, "Inga data för valt urval."))
      a <- input$aggregat
      skapa_diagram_bedomning(
        d, rubrik = paste0("Arbetsmarknadsläget 2040 – ", region_namn()),
        underrubrik = paste0("Bedömning per utbildningsgrupp",
                             if (!is.null(a) && a != "_alla_") paste0(" · ", a)))
    })
    output$d_antal <- ggiraph::renderGirafe({
      d <- dplyr::filter(regtop_bedomning(), regionkod %in% unique(c(region(), "00")))
      a <- input$aggregat
      if (!is.null(a) && a != "_alla_") d <- dplyr::filter(d, aggregat == a)
      skapa_diagram_bedomning_antal(d, region_namn(),
                                    rubrik = paste0("Antal utbildningsgrupper per bedömning"))
    })

    # ---- Fördjupning ---------------------------------------------------------------
    output$d_tillgang <- ggiraph::renderGirafe({
      req(grupp_vald())
      d <- dplyr::filter(hamta_regtop("tillgang_efterfragan"), regionkod == region(),
                         utbildningsgrupp_kod == grupp_vald())
      skapa_diagram_tillgang_efterfragan(d, rubrik = "Tillgång och efterfrågan",
                                         underrubrik = paste0(region_namn(), " · 2010–2040"))
    })

    output$faktaruta <- renderUI({
      r <- grupp_rad()
      if (nrow(r) == 0) return(tags$p(class = "rd-hint", "Gruppen saknas för valt län."))
      r <- r[1, ]
      bed <- if (isTRUE(r$har_bedomning)) {
        farg <- BEDOMNING_FARGER[[as.character(r$bedomning_2040)]]
        tagList(
          tags$div(class = "rd-label", "Bedömning 2040"),
          tags$p(tags$span(style = paste0("display:inline-block;width:12px;height:12px;margin-right:6px;",
                                          "border-radius:2px;background:", farg)),
                 tags$strong(as.character(r$bedomning_2040)),
                 paste0(" (balanstal ", .fmt_proc(r$balanstal_procent), ")")),
          if (r$hansyn_lage_start %in% "Ja" && !is.na(r$bedomning_start))
            tags$p(class = "rd-hint", r$bedomning_start, " har vägts in i bedömningen."),
          tags$p(paste0("Tillgång: ", .fmt_antal(r$tillgang_2023), " (2023) → ",
                        .fmt_antal(r$tillgang_2040), " (2040)"), tags$br(),
                 paste0("Efterfrågan: ", .fmt_antal(r$efterfragan_2023), " (2023) → ",
                        .fmt_antal(r$efterfragan_2040), " (2040)")))
      } else tags$p(class = "rd-hint", "SCB gör ingen framskrivning till 2040 för gruppen i det här länet.")
      yrken <- tryCatch(regtop_vanligaste_yrken(region(), grupp_vald()), error = function(e) character(0))
      ant <- tryCatch(
        hamta_regtop("antaganden_utbildningsvaxling") |>
          dplyr::filter(utbildningsgrupp_kod == grupp_vald()) |>
          dplyr::distinct(yrke, antagande),
        error = function(e) NULL)
      tagList(
        bed,
        if (length(yrken) > 0) tagList(tags$div(class = "rd-label", "Vanligaste yrken"),
                                       tags$ol(lapply(utils::head(yrken, 3), tags$li))),
        if (!is.null(ant) && nrow(ant) > 0)
          tagList(tags$div(class = "rd-label", "SCB:s antaganden om utbildningsväxling"),
                  tags$ul(class = "rd-hint",
                          lapply(seq_len(min(nrow(ant), 5)), function(i)
                            tags$li(tags$strong(ant$yrke[i]), ": ", ant$antagande[i]))))
      )
    })

    output$d_vattenfall <- ggiraph::renderGirafe({
      r <- grupp_rad()
      f <- regtop_faktorer(region(), grupp_vald()) |>
        dplyr::filter(sida == "tillgang") |>
        regtop_summa_period()
      skapa_diagram_vattenfall(
        if (nrow(r) > 0) r$tillgang_2023[1] else NA, f, if (nrow(r) > 0) r$tillgang_2040[1] else NA,
        rubrik = "Vad förändrar tillgången till 2040?",
        underrubrik = paste0(region_namn(), " · summa för 2024–2040"))
    })

    output$d_efterfragan <- ggiraph::renderGirafe({
      f <- regtop_faktorer(region(), grupp_vald()) |> dplyr::filter(sida == "efterfragan")
      skapa_diagram_efterfragan_faktorer(f, rubrik = "Vad förändrar efterfrågan?",
                                         underrubrik = region_namn())
    })

    output$d_kon <- ggiraph::renderGirafe({
      d <- dplyr::filter(hamta_regtop("arbetskraft_utbildningsgrupp"), regionkod == region(),
                         utbildningsgrupp_kod == grupp_vald())
      skapa_diagram_arbetskraft_kon(d, rubrik = "Arbetskraften per kön", underrubrik = region_namn())
    })

    output$d_lan <- ggiraph::renderGirafe({
      d <- dplyr::filter(regtop_bedomning(), utbildningsgrupp_kod == grupp_vald())
      skapa_diagram_bedomning_lan(d, region(), rubrik = "Samma utbildningsgrupp i alla län",
                                  underrubrik = "Balanstal och bedömning 2040")
    })

    # Karta: bedömningen för gruppen i alla län. Klick byter län.
    output$d_karta <- ggiraph::renderGirafe({
      geo <- hamta_lan_geometri()
      validate(need(!is.null(geo), paste0("Kartan kunde inte läsas",
                                          if (!is.null(.geo_cache$fel_lan)) paste0(": ", .geo_cache$fel_lan) else ".")))
      d <- dplyr::filter(regtop_bedomning(), utbildningsgrupp_kod == grupp_vald())
      skapa_karta_bedomning(geo, d, region(), rubrik = "Bedömning 2040 per län")
    })
    observeEvent(input$d_karta_selected, {
      sel <- input$d_karta_selected
      if (length(sel) == 1 && sel %in% regtop_regioner())
        shinyWidgets::updatePickerInput(session, "region", selected = sel)
    })

    # ---- Nedladdning ------------------------------------------------------------------
    urval_data <- function() {
      if (is.null(grupp_vald())) return(dplyr::select(bedomning_region(), -har_bedomning))
      dplyr::filter(hamta_regtop("tillgang_efterfragan"), regionkod == region(),
                    utbildningsgrupp_kod == grupp_vald())
    }
    output$ladda_ner <- downloadHandler(
      filename = function() paste0("prognos2040_", region(), if (!is.null(grupp_vald())) paste0("_", grupp_vald()), ".xlsx"),
      content  = function(file) skriv_gymnasie_excel(urval_data(), file, blad = "Prognos 2040", min_antal = 0)
    )
    output$ladda_ner_alla <- downloadHandler(
      filename = function() "prognos2040_bedomning.xlsx",
      content  = function(file) skriv_gymnasie_excel(dplyr::select(regtop_bedomning(), -har_bedomning),
                                                     file, blad = "Prognos 2040", min_antal = 0)
    )
  })
}

# Kort läsanvisning för bedömningsskalan.
.regtop_lasanvisning <- function() {
  tagList(
    tags$div(class = "rd-label", "Så läser du bedömningen"),
    tags$p(class = "rd-hint",
           "Balanstalet jämför tillgången på personer med en viss utbildning 2040 med ",
           "efterfrågan. Negativt balanstal betyder att det kan bli brist, positivt att ",
           "det kan bli överskott. SCB:s bedömning tar också hänsyn till läget i dag."),
    tags$ul(class = "rd-hint",
            lapply(REGTOP_BEDOMNINGAR, function(b)
              tags$li(tags$span(style = paste0("display:inline-block;width:10px;height:10px;",
                                               "margin-right:6px;border-radius:2px;background:",
                                               BEDOMNING_FARGER[[b]])), b)))
  )
}
