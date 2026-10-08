# ============================================================
#  mod_gymnasiet_avgangna.R
#  Statistikområdet "Avgångna" i Gymnasiet-fliken: vilka som lämnar
#  gymnasiet och med vilket utfall (examen, studiebevis, betyg).
#
#  Inbäddad i mod_gymnasiet.R: när statistikområdet Avgångna väljs visas den
#  här modulens sidopanel och diagram i stället för Gymnasiets vanliga.
#
#  Data: func_data_gymnasiet_avgangna.R. Varje "Visa fördelat på" motsvarar
#  en granularitet i tabellen - totaler läses från "Typ", aldrig som summa
#  av en uppdelning.
# ============================================================

.KALLA_AVGANGNA <- kalla_rud("Gymnasiet – avgångna")
.AVGANGNA_ANTAL_STAPLAR <- 25

AVGANGNA_EXAMEN_FARGER <- c(
  "Examen"                         = RD_PRIMARY,
  "Studiebevis (minst 2500 poäng)" = rd_farg("rd-blue-light", "#8edded"),
  "Övrig/okänd typ"                = "#b9c3ca"
)

avgangna_struktur <- list(
  avgangna = list(label = "Avgångna", klar = TRUE, vy = "fordelning",
                  amne = "Avgångna gymnasieelever", metrik = "avgangna",
                  metrik_label = "Antal avgångna",
                  beskrivning = "Antal elever som lämnat gymnasiet under året, efter typ av examen eller studiebevis."),
  andel_examen = list(label = "Andel med examen", klar = TRUE, vy = "andel",
                      amne = "Andel med examen", metrik = "andel_examen", vikt = "avgangna",
                      metrik_label = "Andel med examen (%)",
                      beskrivning = paste(
                        "Andel av de avgångna som fått examen: högskoleförberedande examen,",
                        "yrkesexamen eller International Baccalaureate (IB).")),
  andel_studiebevis = list(label = "Andel med studiebevis", klar = TRUE, vy = "andel",
                           amne = "Andel med studiebevis (minst 2 500 poäng)",
                           metrik = "andel_studiebevis", vikt = "avgangna",
                           metrik_label = "Andel med studiebevis (%)",
                           beskrivning = "Andel av de avgångna som fått studiebevis om minst 2 500 poäng."),
  betyg = list(label = "Betygspoäng", klar = TRUE, vy = "andel",
               amne = "Genomsnittlig betygspoäng", metrik = "betyg", vikt = "antal_med_jmftal",
               enhet = "", metrik_label = "Genomsnittlig betygspoäng",
               beskrivning = "Genomsnittlig betygspoäng (jämförelsetal) bland avgångna med betygspoäng."),
  # Väntar på bekräftad kodlista för beh (0/1).
  behorighet = list(label = "Behörighet", klar = FALSE, vy = "andel",
                    amne = "Andel med grundläggande behörighet",
                    beskrivning = "Grundläggande behörighet till högskolan. Läggs in när kodlistan är bekräftad.")
)

# ---- UI --------------------------------------------------------------------
mod_gymnasiet_avgangna_ui <- function(id) {
  ns <- NS(id)
  uppd <- AVGANGNA_UPPDELNING
  sidebarLayout(
    sidebarPanel(
      width = 3, class = "rd-sidebar",
      uiOutput(ns("indikator_ui")),
      shinyWidgets::pickerInput(
        inputId = ns("uppdelning"), label = "Visa fördelat på",
        choices = stats::setNames(names(uppd), vapply(uppd, `[[`, "", "label")),
        selected = "ingen"
      ),
      tags$hr(),
      shinyWidgets::radioGroupButtons(
        inputId = ns("perspektiv"), label = "Perspektiv",
        choices = c("Skolkommun" = "skolkommun", "Bokommun" = "bokommun"),
        selected = "skolkommun"
      ),
      tags$p(class = "rd-hint",
             "Skolkommun: elever på skolor i området. ",
             "Bokommun: områdets ungdomar, oavsett var de går i skola."),
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
        downloadButton(ns("ladda_ner"), "Ladda ner aktuellt urval", class = "rd-btn rd-btn--ghost"),
        downloadButton(ns("ladda_ner_alla"), "Ladda ner hela datasetet", class = "rd-btn rd-btn--ghost")
      )
    ),
    mainPanel(
      width = 9,
      div(
        class = "rd-card",
        div(class = "rd-brodsmula", textOutput(ns("brodsmula"))),
        uiOutput(ns("vy")),
        uiOutput(ns("notiser"))
      )
    )
  )
}

# ---- Server ----------------------------------------------------------------
mod_gymnasiet_avgangna_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$indikator_ui <- renderUI({
      ch   <- .choices_fran_lista(avgangna_struktur)
      besk <- avgangna_struktur |> purrr::map("beskrivning") |> purrr::compact()
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
      req(input$indikator %in% names(avgangna_struktur))
      avgangna_struktur[[input$indikator]]
    })
    uppdelning <- reactive({
      AVGANGNA_UPPDELNING[[if (is.null(input$uppdelning)) "ingen" else input$uppdelning]]
    })
    omr_kod <- reactive({
      gv <- input$geo_val
      if (is.null(gv) || gv == "_alla_") "20" else gv
    })
    omr_namn <- reactive({
      if (omr_kod() == "20") "Dalarna" else dalarna_kommuner$kommun[match(omr_kod(), dalarna_kommuner$kommkod)]
    })

    # Rader för valt perspektiv och vald uppdelnings granularitet.
    data_gran <- reactive({
      persp <- if (is.null(input$perspektiv)) "skolkommun" else input$perspektiv
      hamta_gymnasiet_avgangna() |>
        dplyr::filter(geografi == persp, granularitet == uppdelning()$granularitet)
    })

    output$ar_ui <- renderUI({
      ar <- sort(unique(hamta_gymnasiet_avgangna()$ar), decreasing = TRUE)
      tidigare <- isolate(input$ar)
      selectInput(ns("ar"), "Avgångsår", choices = ar,
                  selected = if (!is.null(tidigare) && tidigare %in% ar) tidigare else max(ar))
    })

    program_vald <- reactiveVal(NULL)
    observeEvent(input$d_bar_selected, {
      sel <- input$d_bar_selected
      program_vald(if (length(sel) == 1 && !is.null(uppdelning()$kol)) sel else NULL)
    }, ignoreNULL = FALSE)
    observeEvent(list(input$uppdelning, input$perspektiv, input$geo_val), program_vald(NULL),
                 ignoreInit = TRUE)

    # Serienamn för jämförelser: valt område, Dalarna, Riket.
    serie_namn <- function(kod) dplyr::case_when(kod == "00" ~ "Riket", kod == "20" ~ "Dalarna",
                                                 TRUE ~ omr_namn())
    jmf_koder <- reactive(unique(c(omr_kod(), "20", "00")))

    # Summerat per (ar, område, grupp) för valt område och jämförelserna.
    summerat <- reactive({
      data_gran() |>
        dplyr::filter(kommkod %in% jmf_koder()) |>
        avgangna_summera(uppdelning()$kol)
    })

    # Staplar för valt år. Utan uppdelning: valt område, Dalarna och riket.
    stapel_data <- reactive({
      ar_val <- as.integer(req(input$ar))
      d <- dplyr::filter(summerat(), ar == ar_val)
      if (is.null(uppdelning()$kol)) {
        return(dplyr::mutate(d, program = serie_namn(kommkod)))
      }
      d <- dplyr::filter(d, kommkod == omr_kod())
      # Program/inriktning: de största grupperna (övriga utelämnas, de skulle
      # annars bli många små staplar).
      storst <- d |> dplyr::slice_max(avgangna, n = .AVGANGNA_ANTAL_STAPLAR, with_ties = FALSE) |>
        dplyr::pull(program)
      dplyr::filter(d, program %in% storst)
    })

    # Examenstyp per grupp (för fördelningsdiagrammet).
    fordelning_data <- reactive({
      ar_val <- as.integer(req(input$ar))
      kol <- uppdelning()$kol
      d <- data_gran() |> dplyr::filter(ar == ar_val, kommkod == omr_kod())
      d <- dplyr::mutate(d, program = if (is.null(kol)) omr_namn() else .data[[kol]])
      storst <- stapel_data()$program
      if (!is.null(kol)) d <- dplyr::filter(d, program %in% storst)
      d |> dplyr::group_by(program, examen_grupp) |>
        dplyr::summarise(avgangna = sum(avgangna, na.rm = TRUE), .groups = "drop")
    })

    underrubrik <- function(med_ar = FALSE) {
      persp <- if (identical(input$perspektiv, "bokommun")) "bokommun" else "skolkommun"
      txt <- paste0(omr_namn(), " (", persp, ")")
      if (med_ar) txt <- paste0(txt, " · avgångsår ", input$ar)
      txt
    }

    output$brodsmula <- renderText({
      paste0("Gymnasiet › Avgångna › ", valt_indikator()$label)
    })

    output$vy <- renderUI({
      ind <- valt_indikator()
      if (!isTRUE(ind$klar)) return(div(class = "rd-info", ind$beskrivning))
      hint <- if (!is.null(uppdelning()$kol))
        tags$p(class = "rd-hint rd-hint--bar",
               "Klicka på en stapel för att se utvecklingen över tid för gruppen.")
      fluidRow(
        column(7, ggiraph::girafeOutput(ns("d_bar"), height = "560px"), hint),
        column(5, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_trend"), height = "380px")))
      )
    })

    output$d_bar <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      rub <- paste0(ind$amne, if (!is.null(uppdelning()$kol)) paste0(" efter ", tolower(uppdelning()$label)))
      if (ind$vy == "fordelning") {
        d <- fordelning_data()
        validate(need(nrow(d) > 0, "Inga data för valt urval."))
        return(skapa_diagram_bar_fordelning(d, "examen_grupp", "avgangna", AVGANGNA_EXAMEN_FARGER,
                                            ind$metrik_label, input$ar, rubrik = rub,
                                            underrubrik = underrubrik(TRUE), kalla = .KALLA_AVGANGNA))
      }
      d <- stapel_data()
      validate(need(nrow(d) > 0, "Inga data för valt urval."))
      skapa_diagram_bar_andel(d, ind$metrik, ind$vikt, ind$metrik_label, input$ar,
                              rubrik = rub, underrubrik = underrubrik(TRUE), kalla = .KALLA_AVGANGNA,
                              enhet = if (is.null(ind$enhet)) " %" else ind$enhet)
    })

    output$d_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      prog <- program_vald()
      d <- summerat()
      if (!is.null(prog)) d <- dplyr::filter(d, program == prog)
      d <- d |>
        dplyr::group_by(ar, kommkod) |>
        dplyr::summarise(dplyr::across(c(avgangna, examen, studiebevis, jmftal_summa, antal_med_jmftal), sum),
                         .groups = "drop") |>
        dplyr::mutate(andel_examen      = 100 * examen / avgangna,
                      andel_studiebevis = 100 * studiebevis / avgangna,
                      betyg = dplyr::if_else(antal_med_jmftal > 0, jmftal_summa / antal_med_jmftal, NA_real_))
      validate(need(nrow(d) > 0, "Inga data."))
      rub <- paste0(ind$amne, " – ", if (is.null(prog)) "utveckling över tid" else prog)
      if (ind$vy == "fordelning") {
        d <- dplyr::filter(d, kommkod == omr_kod()) |> dplyr::mutate(program = "x")
        return(skapa_diagram_trend(d, "avgangna", ind$metrik_label, rubrik = rub,
                                   underrubrik = underrubrik(), kalla = .KALLA_AVGANGNA))
      }
      d <- d |>
        dplyr::mutate(serie = factor(serie_namn(kommkod), levels = unique(serie_namn(jmf_koder()))),
                      varde = .data[[ind$metrik]], underlag = .data[[ind$vikt]])
      skapa_diagram_trend_jmf(d, ind$metrik_label, rubrik = rub,
                              underrubrik = paste0(underrubrik(), " jämfört med Dalarna och riket"),
                              kalla = .KALLA_AVGANGNA,
                              enhet = if (is.null(ind$enhet)) " %" else ind$enhet)
    })

    # Kända begränsningar, beroende på val.
    output$notiser <- renderUI({
      n <- c("Data finns från avgångsår 2014 (typ av examen finns inte tidigare).",
             "Examen omfattar högskoleförberedande examen, yrkesexamen och International Baccalaureate (IB).")
      u <- input$uppdelning
      if (identical(u, "nyanland"))
        n <- c(n, paste("Nyanländ = högst 4 år i Sverige, räknat från senaste invandringsår.",
                        "Även svenskfödda som återinvandrat kan ingå."))
      if (isTRUE(u %in% c("program", "inriktning")))
        n <- c(n, paste("Inriktning kommer från studievägskoden. Varianter som idrottsutbildning,",
                        "lärling och riksrekryterande ingår i sitt basprogram. Vissa koder saknar",
                        "programnamn och visas som \"Okänt program\". De",
                        .AVGANGNA_ANTAL_STAPLAR, "största grupperna visas."))
      if (identical(input$perspektiv, "bokommun"))
        n <- c(n, "För senaste avgångsåret används bostadskommun och invandringsår från föregående år.")
      tags$ul(class = "rd-hint", lapply(n, tags$li))
    })

    output$ladda_ner <- downloadHandler(
      filename = function() paste0("gymnasiet_avgangna_", input$indikator, "_", input$uppdelning,
                                   "_", input$ar, ".xlsx"),
      content  = function(file) skriv_gymnasie_excel(stapel_data(), file, blad = "Avgångna")
    )
    output$ladda_ner_alla <- downloadHandler(
      filename = function() paste0("gymnasiet_avgangna_", input$uppdelning, ".xlsx"),
      content  = function(file) skriv_gymnasie_excel(summerat(), file, blad = "Avgångna")
    )
  })
}
