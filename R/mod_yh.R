# ============================================================
#  mod_yh.R
#  Shiny-modul för skolformen Yrkeshögskola (YH).
#
#  Samma grundmönster som mod_gymnasiet.R (N2 statistikområde ->
#  radioGroupButtons, N3 indikator -> radioGroupButtons), men FÖRENKLAD:
#  - Ingen samverkansområde/karta - bara Kommun eller Hela Dalarna
#    (geo_niva == "lan" i datan är redan en färdig Dalarna-totalrad,
#    ingen egen viktning behövs, se func_data_yh.R).
#  - Ingen driftsform (finns inte i YH-datan).
#  - Klick på ett utbildningsområde visar dess utbildningar (granularitet
#    "Utbildning"), klick på en utbildning visar den över tid. Områdets egna
#    siffror kommer alltid från "Omrade"-raderna (se func_data_yh.R).
#  - Genomströmning: ej färdiga startårgångar markeras, ofullständiga
#    (mer än YH_PAGAENDE_DOLJ % pågår) döljs om användaren inte väljer dem.
#  - Etablering: 1/3/5 år efter examen, ett eller flera examensår (andelar
#    ur summorna). Definitionsbytet vid uppföljningsår 2020 markeras.
#
#  Diagramfunktionerna återanvänds OFÖRÄNDRADE från func_diagram.R -
#  se func_data_yh.R för hur kolumnerna döps om för att passa kontraktet.
# ============================================================

.KALLA_YH <- kalla_rud("YH – studerande")
.KALLA_YH_ETABL <- kalla_rud("YH – studerande och RAKS/BAS")
.YH_ANTAL_UTBILDNINGAR <- 25

yh_struktur <- list(
  studerande = list(
    label = "Studerande",
    indikatorer = list(
      paborjad = list(label = "Påbörjade", klar = TRUE, vy = "dashboard", kon = FALSE,
                       amne = "Påbörjade YH-utbildningar",
                       metrik = "paborjad", metrik_label = "Antal påbörjade",
                       kalla = .KALLA_YH,
                       beskrivning = "Antal personer som påbörjat en YH-utbildning under året."),
      pagaende = list(label = "Pågående", klar = TRUE, vy = "dashboard", kon = FALSE,
                       amne = "Pågående YH-studier",
                       metrik = "pagaende", metrik_label = "Antal pågående",
                       kalla = .KALLA_YH,
                       beskrivning = "Antal personer med pågående YH-studier under året (oavsett påbörjandeår)."),
      examen = list(label = "Examen", klar = TRUE, vy = "dashboard", kon = FALSE,
                    amne = "YH-examen",
                    metrik = "examen", metrik_label = "Antal med examen",
                    kalla = .KALLA_YH,
                    beskrivning = "Antal personer som tagit YH-examen under året.")
    )
  ),
  resultat = list(
    label = "Genomströmning",
    indikatorer = list(
      genomstromning = list(
        label = "Andel med examen", klar = TRUE, vy = "genomstromning", kon = FALSE,
        amne = "Andel med examen bland avslutade YH-studier",
        metrik = "andel", metrik_label = "Andel med examen (%)",
        kalla = .KALLA_YH,
        beskrivning = paste(
          "Andel med examen bland dem som påbörjade en YH-utbildning ett visst år",
          "och vars utbildning är avslutad. De som fortfarande studerar räknas inte",
          "med, så för de senaste startåren - där få hunnit bli klara - visas hur",
          "stor del som fortfarande pågår.")
      )
    )
  ),
  etablering = list(
    label = "Etablering efter YH",
    indikatorer = list(
      etabl  = list(label = "Etablerade",  klar = TRUE, vy = "etablering", kon = FALSE,
                    amne = "Etablerade på arbetsmarknaden", metrik = "etabl",
                    metrik_label = "Andel etablerade", kalla = .KALLA_YH_ETABL,
                    beskrivning = paste(
                      "Samma RAKS-definition som gymnasiets etablering (se den fliken):",
                      "anställd med tillräcklig inkomst och utan arbetslöshetsersättning",
                      "under uppföljningsåret.")),
      syss   = list(label = "Sysselsatta", klar = TRUE, vy = "etablering", kon = FALSE,
                    amne = "Sysselsatta efter YH", metrik = "syss",
                    metrik_label = "Andel sysselsatta", kalla = .KALLA_YH_ETABL,
                    beskrivning = "Sysselsatt är den som har sin största inkomst från arbete."),
      stud   = list(label = "Studerande",  klar = TRUE, vy = "etablering", kon = FALSE,
                    amne = "Studerande efter YH", metrik = "stud",
                    metrik_label = "Andel studerande", kalla = .KALLA_YH_ETABL,
                    beskrivning = "Studerande är den som har sin största inkomst från studier."),
      arblos = list(label = "Arbetslösa",  klar = TRUE, vy = "etablering", kon = FALSE,
                    amne = "Arbetslösa efter YH", metrik = "arblos",
                    metrik_label = "Andel arbetslösa", kalla = .KALLA_YH_ETABL,
                    beskrivning = "Arbetslös är den som har sin största inkomst från arbetslöshetsersättningar.")
    )
  )
)

# ---- UI --------------------------------------------------------------------
mod_yh_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "rd-segmented",
      shinyWidgets::radioGroupButtons(
        inputId  = ns("omrade"), label = NULL,
        choices  = .choices_fran_lista(yh_struktur),
        selected = names(yh_struktur)[1]
      )
    ),

    sidebarLayout(
      sidebarPanel(
        width = 3, class = "rd-sidebar",

        uiOutput(ns("indikator_ui")),
        uiOutput(ns("prog_niva_ui")),
        tags$hr(),

        shinyWidgets::pickerInput(
          inputId = ns("geo_val"), label = "Område",
          choices = c("Hela Dalarna" = "_alla_", geo_val_kommun),
          selected = "_alla_",
          options = shinyWidgets::pickerOptions(liveSearch = TRUE)
        ),
        uiOutput(ns("ar_ui")),
        uiOutput(ns("ofullstandiga_ui")),

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
mod_yh_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$indikator_ui <- renderUI({
      omr <- yh_struktur[[ req(input$omrade) ]]
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

    aktuell_data <- reactive({
      omr <- req(input$omrade)
      if (omr == "resultat")        hamta_yh_genomstromning()
      else if (omr == "etablering") hamta_yh_etablering()
      else                          hamta_yh_studerande()
    })

    valt_indikator <- reactive({
      req(input$omrade)
      ind_list <- yh_struktur[[input$omrade]]$indikatorer
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

    # Valt område (kommun eller Hela Dalarna), alla nivåer.
    geo_filter <- function(d) {
      gv <- input$geo_val
      if (is.null(gv) || gv == "_alla_") dplyr::filter(d, geo_niva == "lan")
      else dplyr::filter(d, geo_niva == "kommun", kommkod == gv)
    }

    # ---- Nedborrning: område -> utbildningar -----------------------------------
    # omrade_drill = utbildningsområdet man klickat på (staplarna visar då dess
    # utbildningar), utb_vald = vald utbildning (för trenden).
    omrade_drill <- reactiveVal(NULL)
    utb_vald     <- reactiveVal(NULL)
    observeEvent(input$d_bar_selected, {
      sel <- input$d_bar_selected
      if (is.null(isolate(omrade_drill()))) {
        if (length(sel) == 1) { omrade_drill(sel); utb_vald(NULL) }
      } else {
        utb_vald(if (length(sel) == 1) sel else NULL)
      }
    }, ignoreNULL = FALSE)
    observeEvent(input$tillbaka, { omrade_drill(NULL); utb_vald(NULL) })
    observeEvent(input$omrade, { omrade_drill(NULL); utb_vald(NULL) }, ignoreInit = TRUE)

    # Staplarnas nivå: områdena, eller utbildningarna i valt område.
    niva <- function(d) yh_niva(d, omrade_drill())
    # Trendens rader: hela urvalet, valt område (Omrade-raderna) eller vald
    # utbildning - aldrig en summa av utbildningar.
    trend_niva <- function(d) {
      if (!is.null(utb_vald())) dplyr::filter(niva(d), program == utb_vald())
      else if (!is.null(omrade_drill())) dplyr::filter(yh_niva(d), program == omrade_drill())
      else yh_niva(d)
    }
    fokus_text <- function() {
      f <- c(omrade_drill(), utb_vald())
      if (length(f) == 0) NULL else paste(f, collapse = " \u203a ")
    }
    dim_text <- function() {
      if (is.null(omrade_drill())) " efter utbildningsområde"
      else paste0(" efter utbildning \u2013 ", omrade_drill())
    }
    # Högst .YH_ANTAL_UTBILDNINGAR utbildningar i stapeldiagrammet.
    storsta <- function(d, matt) {
      if (is.null(omrade_drill())) return(d)
      top <- d |> dplyr::group_by(program) |>
        dplyr::summarise(n = sum(.data[[matt]], na.rm = TRUE), .groups = "drop") |>
        dplyr::slice_max(n, n = .YH_ANTAL_UTBILDNINGAR, with_ties = FALSE) |>
        dplyr::pull(program)
      dplyr::filter(d, program %in% top)
    }

    # ---- Etablering: uppföljningstid och examensår ------------------------------
    antal_ar_vald <- reactiveVal("1")
    lapply(c("1", "3", "5", "alla"), function(v) {
      observeEvent(input[[paste0("arbtn_", v)]], { antal_ar_vald(v) }, ignoreInit = TRUE)
    })
    # Examensår som finns för vald uppföljningstid (Alla år: de som finns för
    # alla tre). Uppföljningsår efter YH_SISTA_UPPF_AR är redan bortfiltrerade.
    examensar_tillg <- reactive({
      d  <- dplyr::filter(hamta_yh_etablering(), geo_niva %in% c("lan", "kommun"))
      av <- antal_ar_vald()
      per <- split(d$ar, d$antal_ar)
      ar <- if (identical(av, "alla")) Reduce(intersect, per) else per[[av]]
      sort(unique(ar), decreasing = TRUE)
    })
    examensar_valda <- reactive({
      v <- intersect(as.integer(input$examensar), examensar_tillg())
      sort(if (length(v) == 0) utils::head(examensar_tillg(), 3) else v)
    })

    output$prog_niva_ui <- renderUI({
      req(valt_vy() == "etablering")
      vald <- antal_ar_vald()
      gor_knapp <- function(v, etikett) {
        actionButton(ns(paste0("arbtn_", v)), etikett,
                     class = paste("btn btn-sm rd-arbtn", if (identical(v, vald)) "rd-arbtn-aktiv" else ""))
      }
      tagList(
        div(
          class = "rd-arbtn-grupp",
          tags$label(class = "control-label", "År efter examen"),
          div(class = "rd-arbtn-rad",
              gor_knapp("1", "1 år"), gor_knapp("3", "3 år"),
              gor_knapp("5", "5 år"), gor_knapp("alla", "Alla år"))
        ),
        uiOutput(ns("program_filter_ui"))
      )
    })

    output$program_filter_ui <- renderUI({
      req(valt_vy() == "etablering", is.null(omrade_drill()))
      d_et <- yh_niva(hamta_yh_etablering())
      prg <- sort(unique(stats::na.omit(d_et$program[d_et$geo_niva %in% c("kommun", "lan")])))
      tidigare <- isolate(input$program_filter)
      vald <- if (!is.null(tidigare) && length(tidigare) > 0 && all(tidigare %in% prg)) tidigare else prg
      shinyWidgets::pickerInput(
        inputId = ns("program_filter"), label = "Filtrera utbildningsområde",
        choices = prg, selected = vald, multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE,
          selectedTextFormat = "count > 3", countSelectedText = "{0} områden valda",
          selectAllText = "Välj alla", deselectAllText = "Avmarkera alla")
      )
    })

    # ---- Genomströmning: ofullständiga årgångar ---------------------------------
    visa_ofullstandiga <- reactive(isTRUE(input$visa_ofullstandiga))
    output$ofullstandiga_ui <- renderUI({
      req(valt_vy() == "genomstromning")
      checkboxInput(ns("visa_ofullstandiga"), "Visa ofullständiga årgångar",
                    value = isolate(visa_ofullstandiga()))
    })
    # Urvalets årgångar (område/utbildning i vald kommun) med status.
    gs_urval_ar <- reactive({
      req(valt_vy() == "genomstromning")
      yh_genomstromning_summera(trend_niva(geo_filter(hamta_yh_genomstromning())))
    })

    output$ar_ui <- renderUI({
      vy <- tryCatch(valt_vy(), error = function(e) "dashboard")
      tidigare <- isolate(input$ar)
      if (vy == "etablering") {
        tillg <- examensar_tillg()
        valda <- isolate(input$examensar)
        valda <- if (!is.null(valda) && all(as.integer(valda) %in% tillg)) valda else utils::head(tillg, 3)
        return(shinyWidgets::pickerInput(
          ns("examensar"), "Examensår", choices = tillg, selected = valda, multiple = TRUE,
          options = shinyWidgets::pickerOptions(selectedTextFormat = "count > 4",
                                                countSelectedText = "{0} examensår valda")))
      }
      if (vy == "genomstromning") {
        s <- gs_urval_ar()
        ar <- sort(s$ar[visa_ofullstandiga() | s$status %in% c("komplett", "varning")], decreasing = TRUE)
        req(length(ar) > 0)
        return(selectInput(ns("ar"), "Startår", choices = ar,
                           selected = if (!is.null(tidigare) && tidigare %in% ar) tidigare else max(ar)))
      }
      ar <- sort(unique(aktuell_data()$ar), decreasing = TRUE)
      selectInput(ns("ar"), "År", choices = ar,
                  selected = if (!is.null(tidigare) && tidigare %in% ar) tidigare else max(ar))
    })

    data_bas <- reactive(geo_filter(aktuell_data()))
    data_ar <- reactive({
      req(input$ar)
      dplyr::filter(niva(data_bas()), ar == as.integer(input$ar))
    })

    # ---- Etablering: urvalets data ------------------------------------------------
    yh_etablering_urval <- reactive({
      req(valt_vy() == "etablering")
      d <- niva(geo_filter(hamta_yh_etablering()))
      pf <- input$program_filter
      if (is.null(omrade_drill()) && !is.null(pf) && length(pf) > 0) d <- dplyr::filter(d, program %in% pf)
      d
    })
    yh_etablering_trend_data <- reactive({
      req(valt_vy() == "etablering")
      trend_niva(geo_filter(hamta_yh_etablering()))
    })
    yh_etablering_riket_serie <- reactive({
      req(valt_vy() == "etablering")
      yh_etablering_riket(yh_riket_rader(hamta_yh_etablering(), omrade_drill(), utb_vald()),
                          NULL, valt_indikator()$metrik)
    })
    # Riket-andel för valda examensår (referenslinjen i stapeln).
    yh_etablering_riket_andel <- reactive({
      av <- antal_ar_vald()
      if (identical(av, "alla")) return(NA_real_)
      ind <- valt_indikator()
      d <- yh_niva(dplyr::filter(hamta_yh_etablering(), geo_niva == "riket"), omrade_drill()) |>
        dplyr::filter(antal_ar == as.integer(av), ar %in% examensar_valda())
      if (is.null(omrade_drill()) && !is.null(input$program_filter))
        d <- dplyr::filter(d, program %in% input$program_filter)
      if (sum(d$antal) == 0) NA_real_ else sum(d[[ind$metrik]]) / sum(d$antal)
    })
    examensar_text <- function() {
      v <- examensar_valda()
      if (length(v) > 1 && all(diff(v) == 1)) paste0(min(v), "\u2013", max(v)) else paste(v, collapse = ", ")
    }
    # Spänner valda examensår över definitionsbytet (för någon uppföljningstid)?
    over_bytet <- reactive({
      av <- antal_ar_vald()
      tider <- if (identical(av, "alla")) c(1L, 3L, 5L) else as.integer(av)
      any(vapply(tider, function(t) {
        uppf <- examensar_valda() + t
        any(uppf < YH_DEFINITIONSBYTE) && any(uppf >= YH_DEFINITIONSBYTE)
      }, logical(1)))
    })

    output$brodsmula <- renderText({
      omr <- yh_struktur[[req(input$omrade)]]$label
      paste0("Yrkeshögskola \u203a ", omr, " \u203a ", valt_indikator()$label)
    })

    output$vy <- renderUI({
      ind <- valt_indikator()
      if (!isTRUE(ind$klar)) {
        return(div(class = "rd-info", "Den här vyn är inte inlagd än."))
      }
      tillbaka <- if (!is.null(omrade_drill()))
        actionLink(ns("tillbaka"), "\u2190 Alla utbildningsområden", class = "rd-tillbaka")
      hint <- tags$p(class = "rd-hint rd-hint--bar",
                     if (is.null(omrade_drill()))
                       "Klicka på ett utbildningsområde för att se dess utbildningar."
                     else "Klicka på en utbildning för att se utvecklingen över tid.")
      trend_id <- switch(valt_vy(), etablering = "d_etablering_trend",
                         genomstromning = "d_genomstromning_trend", "d_trend")
      fluidRow(
        column(7, tillbaka, ggiraph::girafeOutput(ns("d_bar"), height = "560px"), hint,
               uiOutput(ns("kon_kontroll")), uiOutput(ns("notiser"))),
        column(5, div(class = "rd-subcard", ggiraph::girafeOutput(ns(trend_id), height = "380px")))
      )
    })

    # ---- Kön: Könsuppdelat (förvalt) eller Totalt ------------------------------
    # Inte för etablering med alla uppföljningstider (redan grupperat på år).
    kon_mojligt <- reactive(!(valt_vy() == "etablering" && identical(antal_ar_vald(), "alla")))
    kon_uppdelat <- reactive(kon_mojligt() && !identical(input$kon_lage, "total"))
    output$kon_kontroll <- renderUI({
      req(kon_mojligt())
      div(class = "rd-kon-kontroll",
          shinyWidgets::radioGroupButtons(
            inputId = ns("kon_lage"), label = NULL,
            choices = c("Könsuppdelat" = "kon", "Totalt" = "total"),
            selected = isolate(if (is.null(input$kon_lage)) "kon" else input$kon_lage),
            size = "sm"))
    })

    output$notiser <- renderUI({
      n <- character(0)
      if (!is.null(omrade_drill()))
        n <- c(n, paste0("Utbildningarna bygger ofta på få personer - värden under ", MIN_ANTAL,
                         " visas inte. Områdets egna siffror (trenden utan vald utbildning)",
                         " räknas på hela området. De ", .YH_ANTAL_UTBILDNINGAR,
                         " största utbildningarna visas."))
      if (valt_vy() == "genomstromning") {
        s <- dplyr::filter(gs_urval_ar(), ar == as.integer(req(input$ar)))
        if (nrow(s) == 1 && s$status %in% c("varning", "ofullstandig"))
          n <- c(n, paste0("Årgången är inte färdig: ", .yh_pagaende_text(s$andel_pagaende),
                           " av de påbörjade utbildningarna."))
        n <- c(n, paste0(
          "De senaste startåren är inte färdiga. De kortare utbildningarna blir klara först, så ",
          "andelen med examen för en ofullständig årgång kan se för hög eller för låg ut. ",
          "Årgångar där mer än ", YH_PAGAENDE_DOLJ, " % pågår visas bara om du väljer ",
          "\"Visa ofullständiga årgångar\"; årgångar där mer än ", YH_PAGAENDE_VARNING,
          " % pågår markeras."))
      }
      if (valt_vy() == "etablering") {
        if (over_bytet())
          n <- c(n, paste0("Valda examensår ligger både före och efter uppföljningsår ",
                           YH_DEFINITIONSBYTE, ", då definitionen av utfallet ändrades. ",
                           "Jämför inte nivåer rakt av över bytet."))
        n <- c(n, paste0("Från uppföljningsår ", YH_DEFINITIONSBYTE, " bygger utfallet ",
                         "(sysselsatt, studerande, arbetslös, etablerad) på en annan klassificering ",
                         "än tidigare - definitionen ändrades. Uppföljningen går till och med ",
                         YH_SISTA_UPPF_AR, ", så de senaste examensåren finns bara för kortare ",
                         "uppföljningstid."))
      }
      if (length(n) == 0) return(NULL)
      tags$ul(class = "rd-hint", lapply(n, tags$li))
    })

    output$d_bar <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      rub <- paste0(ind$amne, dim_text())

      if (valt_vy() == "etablering") {
        d <- dplyr::filter(yh_etablering_urval(), ar %in% examensar_valda())
        validate(need(nrow(d) > 0, "Inga data för valt urval."))
        d <- storsta(d, "antal")
        sub <- paste0(filter_underrubrik(), " \u00b7 examensår ", examensar_text())
        if (kon_uppdelat()) {
          # Grupperade staplar per kön (andel ur summorna, viktad på antal).
          s <- d |>
            dplyr::filter(antal_ar == as.integer(antal_ar_vald()), kon %in% c("Kvinna", "Man")) |>
            dplyr::group_by(namn, kon) |>
            dplyr::summarise(m = sum(.data[[ind$metrik]], na.rm = TRUE),
                             antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
            dplyr::mutate(andel = dplyr::if_else(antal > 0, 100 * m / antal, NA_real_),
                          geo_niva = "kommun", kommun = namn, kommkod = namn, program = kon)
          validate(need(nrow(s) > 0, "Inga data för valt urval."))
          return(skapa_diagram_andel_omrade_kon(
            s, "andel", "antal", ind$metrik_label, vald_kommkod = utb_vald(),
            rubrik = rub, underrubrik = paste0(sub, " \u00b7 ", antal_ar_vald(), " år efter examen"),
            kalla = ind$kalla))
        }
        skapa_diagram_etablering_bar(d, ind$metrik, ind$metrik_label,
                                     antal_ar_val = antal_ar_vald(),
                                     riket_andel = yh_etablering_riket_andel(),
                                     rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      } else if (valt_vy() == "genomstromning") {
        d <- data_ar()
        validate(need(nrow(d) > 0, "Inga data för valt urval."))
        d <- storsta(d, "paborjade")
        r <- genomstromning_riket_yh(hamta_yh_genomstromning(), omrade_drill()) |>
          dplyr::filter(ar == as.integer(input$ar))
        skapa_diagram_yh_genomstromning_bar(
          d, input$ar, visa_ofullstandiga = visa_ofullstandiga(),
          riket_andel = if (nrow(r) == 1 && (visa_ofullstandiga() || r$status != "ofullstandig")) r$andel,
          rubrik = rub, underrubrik = paste0(geo_label(), " \u00b7 startår ", input$ar),
          kalla = ind$kalla, per_kon = kon_uppdelat())
      } else {
        df <- storsta(data_ar(), ind$metrik)
        validate(need(nrow(df) > 0, "Inga data för valt urval."))
        if (kon_uppdelat())
          skapa_diagram_bar_kon(yh_kon_bred(df, ind$metrik), "m_kv", "m_man", ind$metrik_label, input$ar,
                                rubrik = rub, underrubrik = filter_underrubrik(med_ar = TRUE),
                                kalla = ind$kalla)
        else
          skapa_diagram_bar(df, ind$metrik, ind$metrik_label, input$ar,
                            rubrik = rub, underrubrik = filter_underrubrik(med_ar = TRUE),
                            kalla = ind$kalla)
      }
    })

    trend_rubrik <- function(ind) {
      f <- fokus_text()
      paste(strwrap(paste0(ind$amne, " \u2013 ", if (is.null(f)) "utveckling över tid" else f),
                    width = 55), collapse = "\n")
    }

    output$d_etablering_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar), valt_vy() == "etablering")
      df <- yh_etablering_trend_data()
      validate(need(nrow(df) > 0, "Inga data."))
      av <- antal_ar_vald()
      antal_ar_v <- if (identical(av, "alla")) 1L else as.integer(av)
      skapa_diagram_yh_etablering_trend(
        df, yh_etablering_riket_serie(), ind$metrik, ind$metrik_label, antal_ar_v,
        serie_namn = geo_label(), rubrik = trend_rubrik(ind),
        underrubrik = paste0(geo_label(), " \u00b7 ", antal_ar_v, " år efter examen"),
        kalla = ind$kalla, per_kon = kon_uppdelat())
    })

    output$d_genomstromning_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar), valt_vy() == "genomstromning")
      u <- gs_urval_ar()
      validate(need(nrow(u) > 0, "Inga data."))
      if (kon_uppdelat()) {
        # En linje per kön; skuggningen följer hela urvalets status.
        d <- trend_niva(geo_filter(hamta_yh_genomstromning())) |>
          dplyr::filter(kon %in% c("Kvinna", "Man")) |>
          yh_genomstromning_summera(grupp = "kon") |>
          dplyr::mutate(serie = factor(unname(.YH_KON_ETIKETT[kon]), levels = names(KON_FARGER)))
        return(skapa_diagram_yh_genomstromning_trend(
          d, visa_ofullstandiga(), rubrik = trend_rubrik(ind), underrubrik = geo_label(),
          kalla = ind$kalla, farger = KON_FARGER, status_ar = u))
      }
      r <- genomstromning_riket_yh(hamta_yh_genomstromning(), omrade_drill(), utb_vald())
      d <- dplyr::bind_rows(dplyr::mutate(u, serie = geo_label()), dplyr::mutate(r, serie = "Riket"))
      skapa_diagram_yh_genomstromning_trend(d, visa_ofullstandiga(), rubrik = trend_rubrik(ind),
                                            underrubrik = paste0(geo_label(), " jämfört med riket"),
                                            kalla = ind$kalla)
    })

    output$d_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      df <- trend_niva(data_bas())
      validate(need(nrow(df) > 0, "Inga data."))
      if (kon_uppdelat())
        return(skapa_diagram_trend_kon(yh_kon_bred(df, ind$metrik), "m_kv", "m_man", ind$metrik_label, NULL,
                                       rubrik = trend_rubrik(ind), underrubrik = filter_underrubrik(),
                                       kalla = ind$kalla))
      skapa_diagram_trend(df, ind$metrik, ind$metrik_label, NULL,
                          rubrik = trend_rubrik(ind), underrubrik = filter_underrubrik(),
                          kalla = ind$kalla)
    })

    # Nedladdning: aktuell nivå. Etableringen summeras (se
    # summera_etablering_nedladdning()), genomströmningen får andelarna.
    urval_data <- function() {
      if (valt_vy() == "etablering")
        return(summera_etablering_nedladdning(dplyr::filter(yh_etablering_urval(), ar %in% examensar_valda())))
      d <- data_ar()
      if (valt_vy() == "genomstromning") d <- yh_genomstromning_summera(d, grupp = "program")
      d
    }
    output$ladda_ner <- downloadHandler(
      filename = function() paste0("yh_", input$omrade, "_", input$indikator, ".xlsx"),
      content  = function(file) skriv_gymnasie_excel(urval_data(), file, blad = "YH")
    )
    output$ladda_ner_alla <- downloadHandler(
      filename = function() "yh_hela_datasetet.xlsx",
      content  = function(file) {
        d <- aktuell_data()
        if (identical(input$omrade, "etablering")) d <- summera_etablering_nedladdning(d)
        skriv_gymnasie_excel(d, file, blad = "YH")
      }
    )
  })
}
