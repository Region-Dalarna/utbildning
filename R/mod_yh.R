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
#  - Ingen program/inriktning-nivå - "program" = UtbildningsOmrade_Utb,
#    "namn" == "program" rakt av (ingen underindelning).
#  - Etablering har bara 1/3/5 år efter examen (inte 7, se
#    uttag_yh_uppf_raks.R som bara kör de tre kohorterna).
#
#  Diagramfunktionerna återanvänds OFÖRÄNDRADE från func_diagram.R -
#  se func_data_yh.R för hur kolumnerna döps om för att passa kontraktet.
# ============================================================

.KALLA_YH <- kalla_rud("YH – studerande")
.KALLA_YH_ETABL <- kalla_rud("YH – studerande och RAKS")

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
          "Andel av dem som PÅBÖRJADE en YH-utbildning ett visst år och vars",
          "utbildning sedan är avslutad (oavsett hur lång tid det tog) som",
          "tagit examen. De vars utbildning ännu inte är avslutad räknas inte",
          "med i nämnaren.")
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

    # ---- Etablering: uppföljningsår (1/3/5) --------------------------------
    antal_ar_vald <- reactiveVal("1")
    lapply(c("1", "3", "5", "alla"), function(v) {
      observeEvent(input[[paste0("arbtn_", v)]], { antal_ar_vald(v) }, ignoreInit = TRUE)
    })

    output$prog_niva_ui <- renderUI({
      req(valt_vy() == "etablering")
      d_et   <- hamta_yh_etablering()
      alla   <- c("1", "3", "5")
      ar_val <- tryCatch(as.integer(input$ar), error = function(e) NA_integer_)
      if (is.na(ar_val))
        ar_val <- max(d_et$ar[d_et$geo_niva %in% c("lan", "kommun")], na.rm = TRUE)

      har_data <- function(av) {
        r <- d_et[d_et$antal_ar == as.integer(av) &
                    d_et$geo_niva %in% c("lan", "kommun") &
                    d_et$ar == ar_val, , drop = FALSE]
        nrow(r) > 0 && any(!is.na(r$syss) | !is.na(r$stud) | !is.na(r$arblos) | !is.na(r$etabl))
      }
      tillg <- alla[vapply(alla, har_data, logical(1))]
      if (length(tillg) == 0) tillg <- alla

      vald <- antal_ar_vald()
      if (vald != "alla" && !vald %in% tillg) { vald <- tillg[1]; antal_ar_vald(vald) }

      gor_knapp <- function(v, etikett) {
        aktiv <- identical(v, vald)
        btn <- actionButton(
          ns(paste0("arbtn_", v)), etikett,
          class = paste("btn btn-sm rd-arbtn", if (aktiv) "rd-arbtn-aktiv" else ""))
        if (v != "alla" && !v %in% tillg) shinyjs::disabled(btn) else btn
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
      req(valt_vy() == "etablering")
      d_et <- hamta_yh_etablering()
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

    program_vald <- reactiveVal(NULL)
    observeEvent(input$d_bar_selected, {
      sel <- input$d_bar_selected
      program_vald(if (length(sel) >= 1) sel else NULL)
    }, ignoreNULL = FALSE)
    observeEvent(input$omrade, { program_vald(NULL) }, ignoreInit = TRUE)

    output$ar_ui <- renderUI({
      d  <- aktuell_data()
      vy <- tryCatch(valt_vy(), error = function(e) "dashboard")
      tidigare_ar <- isolate(input$ar)
      behall <- function(giltiga, standard) {
        if (!is.null(tidigare_ar) && tidigare_ar %in% as.character(giltiga)) tidigare_ar else standard
      }

      if (vy == "etablering") {
        ep <- d |>
          dplyr::filter(geo_niva %in% c("lan", "kommun")) |>
          dplyr::distinct(ar, exam_ar_interval) |>
          dplyr::arrange(dplyr::desc(ar))
        etiketter <- gsub("-", "\u2013", ep$exam_ar_interval, fixed = TRUE)
        choices <- stats::setNames(ep$ar, etiketter)
        selectInput(ns("ar"), "Examensperiod", choices = choices,
                    selected = behall(ep$ar, ep$ar[1]))
      } else {
        ar <- sort(unique(d$ar), decreasing = TRUE)
        selectInput(ns("ar"), "År", choices = ar, selected = behall(ar, max(ar)))
      }
    })

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

    # ---- Genomströmning -----------------------------------------------------
    yh_genomstromning_dalarna <- reactive({
      req(valt_vy() == "genomstromning")
      d  <- hamta_yh_genomstromning()
      gv <- input$geo_val
      if (is.null(gv) || gv == "_alla_") dplyr::filter(d, geo_niva == "lan")
      else dplyr::filter(d, geo_niva == "kommun", kommkod == gv)
    })
    yh_genomstromning_riket_serie <- reactive({
      req(valt_vy() == "genomstromning")
      genomstromning_riket_yh(hamta_yh_genomstromning(), program_vald())
    })

    # ---- Etablering: Dalarna-data --------------------------------------------
    yh_etablering_dalarna <- reactive({
      req(valt_vy() == "etablering")
      d  <- hamta_yh_etablering()
      gv <- input$geo_val
      d  <- if (is.null(gv) || gv == "_alla_") dplyr::filter(d, geo_niva == "lan")
      else dplyr::filter(d, geo_niva == "kommun", kommkod == gv)

      pf <- input$program_filter
      if (!is.null(pf) && length(pf) > 0) d <- dplyr::filter(d, program %in% pf)

      d |>
        dplyr::group_by(ar, exam_ar_interval, antal_ar, program, namn) |>
        dplyr::summarise(
          dplyr::across(c(syss, stud, arblos, ovriga, etabl, antal,
                          etabl_med_inkomst, inkomst_summa),
                        ~sum(.x, na.rm = TRUE)),
          .groups = "drop")
    })

    yh_etablering_riket_serie <- reactive({
      req(valt_vy() == "etablering")
      ind  <- valt_indikator()
      prog <- program_vald()
      yh_etablering_riket(hamta_yh_etablering(), prog, ind$metrik)
    })

    yh_etablering_riket_andel <- reactive({
      req(valt_vy() == "etablering")
      av <- antal_ar_vald()
      if (identical(av, "alla")) return(NA_real_)
      antal_ar_v <- as.integer(av)
      df <- yh_etablering_riket_serie() |>
        dplyr::filter(antal_ar == antal_ar_v, ar == as.integer(req(input$ar)))
      if (nrow(df) == 0) return(NA_real_)
      mean(df$andel_riket, na.rm = TRUE)
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
      hint <- tags$p(class = "rd-hint rd-hint--bar",
                     "Klicka på en stapel i diagrammet för att se statistik för ett specifikt utbildningsområde.")

      if (valt_vy() == "etablering") {
        fluidRow(
          column(7, ggiraph::girafeOutput(ns("d_bar"), height = "560px"), hint),
          column(5, div(class = "rd-subcard",
                        ggiraph::girafeOutput(ns("d_etablering_trend"), height = "380px")))
        )
      } else if (valt_vy() == "genomstromning") {
        fluidRow(
          column(7, ggiraph::girafeOutput(ns("d_bar"), height = "560px"), hint),
          column(5, div(class = "rd-subcard",
                        ggiraph::girafeOutput(ns("d_genomstromning_trend"), height = "380px")))
        )
      } else {
        fluidRow(
          column(7, ggiraph::girafeOutput(ns("d_bar"), height = "560px"), hint),
          column(5, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_trend"), height = "380px")))
        )
      }
    })

    output$d_bar <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      sub <- filter_underrubrik(med_ar = TRUE)

      if (valt_vy() == "etablering") {
        d <- yh_etablering_dalarna()
        validate(need(nrow(d) > 0, "Inga data för valt urval."))
        d <- dplyr::filter(d, ar == as.integer(req(input$ar)))
        skapa_diagram_etablering_bar(d, ind$metrik, ind$metrik_label,
                                     antal_ar_val = antal_ar_vald(),
                                     riket_andel = yh_etablering_riket_andel(),
                                     rubrik = ind$amne, underrubrik = sub, kalla = ind$kalla)
      } else if (valt_vy() == "genomstromning") {
        d <- dplyr::filter(yh_genomstromning_dalarna(), ar == as.integer(req(input$ar)))
        validate(need(nrow(d) > 0, "Inga data för valt urval."))
        skapa_diagram_genomstromning_bar(d, input$ar, rubrik = ind$amne,
                                         underrubrik = sub, kalla = ind$kalla)
      } else {
        df <- data_ar()
        validate(need(nrow(df) > 0, "Inga data för valt urval."))
        skapa_diagram_bar(df, ind$metrik, ind$metrik_label, input$ar,
                          rubrik = paste0(ind$amne, " efter utbildningsområde"),
                          underrubrik = sub, kalla = ind$kalla)
      }
    })

    output$d_etablering_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar), valt_vy() == "etablering")
      df_dal <- yh_etablering_dalarna()
      validate(need(nrow(df_dal) > 0, "Inga data."))
      df_rik <- yh_etablering_riket_serie()
      prog   <- program_vald()
      rub    <- if (is.null(prog)) ind$amne else prog
      antal_ar_in <- antal_ar_vald()
      antal_ar_v  <- if (identical(antal_ar_in, "alla")) 1L else as.integer(antal_ar_in)
      sub <- paste0(filter_underrubrik(), " \u00b7 ", antal_ar_v,
                    " år efter examen, per examensperiod")
      df_prog <- if (is.null(prog)) df_dal else dplyr::filter(df_dal, namn == prog)
      skapa_diagram_etablering_trend(df_prog, df_rik,
                                     ind$metrik, ind$metrik_label,
                                     antal_ar_val = antal_ar_v,
                                     rubrik = rub, underrubrik = sub, kalla = ind$kalla)
    })

    output$d_genomstromning_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar), valt_vy() == "genomstromning")
      df_dal <- yh_genomstromning_dalarna()
      validate(need(nrow(df_dal) > 0, "Inga data."))
      df_rik <- yh_genomstromning_riket_serie()
      prog   <- program_vald()
      rub    <- if (is.null(prog)) ind$amne else prog
      sub    <- filter_underrubrik()
      df_prog <- if (is.null(prog)) df_dal else dplyr::filter(df_dal, program == prog)
      df_prog <- yh_genomstromning_per_ar(df_prog)   # viktad andel per år
      skapa_diagram_genomstromning_trend(df_prog, df_rik, program_sel = prog,
                                         rubrik = rub, underrubrik = sub, kalla = ind$kalla)
    })

    output$d_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      df   <- data_bas()
      validate(need(nrow(df) > 0, "Inga data."))
      prog <- program_vald()
      rub  <- if (is.null(prog)) paste0(ind$amne, " \u2013 utveckling över tid") else paste0(ind$amne, " \u2013 ", prog)
      sub  <- filter_underrubrik()
      skapa_diagram_trend(df, ind$metrik, ind$metrik_label, prog,
                          rubrik = rub, underrubrik = sub, kalla = ind$kalla)
    })

    # Etableringsdata laddas ner summerad (se summera_etablering_nedladdning()).
    nedladdning <- function(d) {
      if (identical(input$omrade, "etablering")) summera_etablering_nedladdning(d) else d
    }

    output$ladda_ner <- downloadHandler(
      filename = function() paste0("yh_", input$omrade, "_", input$indikator, "_", input$ar, ".xlsx"),
      content  = function(file) skriv_gymnasie_excel(nedladdning(data_ar()), file, blad = "YH")
    )
    output$ladda_ner_alla <- downloadHandler(
      filename = function() "yh_hela_datasetet.xlsx",
      content  = function(file) skriv_gymnasie_excel(nedladdning(aktuell_data()), file, blad = "YH")
    )
  })
}
