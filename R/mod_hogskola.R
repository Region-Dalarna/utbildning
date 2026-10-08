# ============================================================
#  mod_hogskola.R
#  Shiny-modul för skolformen Högskola.
#
#  PERSPEKTIV (val högst upp):
#  - "larosate" (standard): studenter, kursregistreringar, examina och
#    etablering vid ett lärosäte, oavsett var studenterna bor
#    (regionkod "00" + hskod). Högskolan Dalarna är förvald; andra
#    lärosäten kan väljas längre ner i sidopanelen.
#  - "invanare": Dalarnas invånares studier var som helst i landet
#    (regionkod "20" eller vald kommun).
#
#  Statistikområden: Studerande (studenter, kursregistreringar), Examina
#  och Etablering efter examen. Studerande och Examina kan visas
#  könsuppdelat eller totalt.
#
#  Deltagare (studenter) summeras aldrig över lärosäten eller program - se
#  func_data_hogskola.R. Staplarna visar därför de största programmen resp.
#  lärosätena utan "Övriga", och totalen hämtas från egen granularitet.
# ============================================================

.KALLA_HOGSKOLA_REG   <- kalla_rud("HReg – registrerade")
.KALLA_HOGSKOLA_EXAM  <- kalla_rud("HReg – examen")
.KALLA_HOGSKOLA_ETABL <- kalla_rud("HReg – examen och RAKS")
.HOGSKOLA_ANTAL_STAPLAR <- 15

hogskola_struktur <- list(
  studerande = list(
    label = "Studerande",
    indikatorer = list(
      deltagare = list(label = "Studenter", klar = TRUE, vy = "dashboard", kon = TRUE,
                       amne = "Antal studenter",
                       metrik = "deltagare", metrik_label = "Antal studenter",
                       kalla = .KALLA_HOGSKOLA_REG,
                       beskrivning = paste(
                         "Antal unika studenter under året. En student kan läsa flera program",
                         "eller vid flera lärosäten, så staplarna summerar inte till totalen.")),
      kursregistreringar = list(label = "Kursregistreringar", klar = TRUE, vy = "dashboard", kon = TRUE,
                                amne = "Kursregistreringar",
                                metrik = "kursregistreringar", metrik_label = "Antal kursregistreringar",
                                kalla = .KALLA_HOGSKOLA_REG,
                                beskrivning = "Antal kursregistreringar under året (en student kan läsa flera kurser).")
    )
  ),
  examina = list(
    label = "Examina",
    indikatorer = list(
      antal_examina = list(label = "Examina", klar = TRUE, vy = "dashboard", kon = TRUE,
                           amne = "Examina",
                           metrik = "antal_examina", metrik_label = "Antal examina",
                           kalla = .KALLA_HOGSKOLA_EXAM,
                           beskrivning = paste(
                             "Antal utfärdade examina under året per ämnesområde (SUN 2020).",
                             "En person kan ta flera examina."))
    )
  ),
  etablering = list(
    label = "Etablering efter examen",
    indikatorer = list(
      etabl  = list(label = "Etablerade",  klar = TRUE, vy = "etablering", kon = FALSE,
                    amne = "Etablerade på arbetsmarknaden", metrik = "etabl",
                    metrik_label = "Andel etablerade", kalla = .KALLA_HOGSKOLA_ETABL,
                    beskrivning = paste(
                      "Samma RAKS-definition som gymnasiets etablering (se den fliken):",
                      "anställd med tillräcklig inkomst och utan arbetslöshetsersättning",
                      "under uppföljningsåret.")),
      syss   = list(label = "Sysselsatta", klar = TRUE, vy = "etablering", kon = FALSE,
                    amne = "Sysselsatta efter examen", metrik = "syss",
                    metrik_label = "Andel sysselsatta", kalla = .KALLA_HOGSKOLA_ETABL,
                    beskrivning = "Sysselsatt är den som har sin största inkomst från arbete."),
      stud   = list(label = "Studerande",  klar = TRUE, vy = "etablering", kon = FALSE,
                    amne = "Studerande efter examen", metrik = "stud",
                    metrik_label = "Andel studerande", kalla = .KALLA_HOGSKOLA_ETABL,
                    beskrivning = "Studerande är den som har sin största inkomst från studier."),
      arblos = list(label = "Arbetslösa",  klar = TRUE, vy = "etablering", kon = FALSE,
                    amne = "Arbetslösa efter examen", metrik = "arblos",
                    metrik_label = "Andel arbetslösa", kalla = .KALLA_HOGSKOLA_ETABL,
                    beskrivning = "Arbetslös är den som har sin största inkomst från arbetslöshetsersättningar.")
    )
  )
)

# Summerar ett mått per grupp och kön till kolumnerna kv/man (för
# skapa_diagram_bar_kon()/skapa_diagram_trend_kon()).
.kon_bred <- function(d, metrik, grupp) {
  d |>
    dplyr::filter(kon %in% c("Kvinna", "Man")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(grupp, "kon")))) |>
    dplyr::summarise(v = sum(.data[[metrik]], na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = kon, values_from = v, values_fill = 0) |>
    dplyr::rename(kv = dplyr::any_of("Kvinna"), man = dplyr::any_of("Man"))
}

# ---- UI --------------------------------------------------------------------
mod_hogskola_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "rd-segmented",
      shinyWidgets::radioGroupButtons(
        inputId  = ns("perspektiv"), label = NULL,
        choices  = c("Lärosäte" = "larosate", "Dalarnas invånare" = "invanare"),
        selected = "larosate"
      )
    ),
    div(
      class = "rd-segmented",
      shinyWidgets::radioGroupButtons(
        inputId  = ns("omrade"), label = NULL,
        choices  = .choices_fran_lista(hogskola_struktur),
        selected = names(hogskola_struktur)[1]
      )
    ),

    sidebarLayout(
      sidebarPanel(
        width = 3, class = "rd-sidebar",

        uiOutput(ns("indikator_ui")),
        uiOutput(ns("prog_niva_ui")),
        uiOutput(ns("examenstyp_ui")),
        tags$hr(),

        conditionalPanel(
          condition = "input.perspektiv == 'invanare'", ns = ns,
          shinyWidgets::pickerInput(
            inputId = ns("geo_val"), label = "Område (hemkommun)",
            choices = c("Hela Dalarna" = "_alla_", geo_val_kommun),
            selected = "_alla_",
            options = shinyWidgets::pickerOptions(liveSearch = TRUE)
          )
        ),
        uiOutput(ns("ar_ui")),
        conditionalPanel(
          condition = "input.perspektiv == 'larosate'", ns = ns,
          uiOutput(ns("larosate_ui"))
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
mod_hogskola_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    perspektiv <- reactive({ if (is.null(input$perspektiv)) "larosate" else input$perspektiv })
    ar_larosate <- reactive({ perspektiv() == "larosate" })
    hskod_val <- reactive({
      if (is.null(input$larosate)) HOGSKOLA_STANDARD_HSKOD else input$larosate
    })

    output$indikator_ui <- renderUI({
      omr <- hogskola_struktur[[ req(input$omrade) ]]
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

    output$larosate_ui <- renderUI({
      lar <- hogskola_larosaten()
      selectInput(ns("larosate"), "Lärosäte",
                  choices = stats::setNames(lar$hskod, lar$hskod_namn),
                  selected = isolate(hskod_val()))
    })

    valt_indikator <- reactive({
      req(input$omrade)
      ind_list <- hogskola_struktur[[input$omrade]]$indikatorer
      req(input$indikator %in% names(ind_list))
      ind_list[[input$indikator]]
    })

    valt_vy <- reactive({
      v <- valt_indikator()$vy
      if (is.null(v)) "dashboard" else v
    })

    # Valt lärosäte eller område, för rubriker.
    geo_label <- reactive({
      if (ar_larosate()) {
        lar <- hogskola_larosaten()
        return(lar$hskod_namn[match(hskod_val(), lar$hskod)])
      }
      gv <- input$geo_val
      if (is.null(gv) || gv == "_alla_") return("Dalarnas invånare")
      paste("Invånare i", dalarna_kommuner$kommun[match(gv, dalarna_kommuner$kommkod)])
    })

    # Vad staplarna visar i aktuellt statistikområde.
    dim_label <- reactive({
      if (identical(input$omrade, "studerande")) {
        if (ar_larosate()) "program" else "lärosäte"
      } else "ämnesområde"
    })

    filter_underrubrik <- function(med_ar = FALSE) {
      txt <- geo_label()
      et <- input$examenstyp
      if (identical(input$omrade, "examina") && !is.null(et) && et != "_alla_")
        txt <- paste0(txt, " · ", et)
      if (med_ar) txt <- paste0(txt, " · år ", req(input$ar))
      txt
    }

    # Filtrerar rader på perspektiv: lärosätet i riket, eller boendeområdet.
    filtrera_perspektiv <- function(d, med_hskod = TRUE) {
      if (ar_larosate()) {
        d <- dplyr::filter(d, kommkod == "00")
        if (med_hskod) d <- dplyr::filter(d, hskod == hskod_val())
        return(d)
      }
      gv <- input$geo_val
      if (is.null(gv) || gv == "_alla_") dplyr::filter(d, geo_niva == "lan")
      else dplyr::filter(d, geo_niva == "kommun", kommkod == gv)
    }

    # Könsuppdelat (förvalt) eller totalt - för Studerande och Examina.
    kon_uppdelat <- reactive({ !identical(input$kon_lage, "total") })

    program_vald <- reactiveVal(NULL)
    observeEvent(input$d_bar_selected, {
      sel <- input$d_bar_selected
      program_vald(if (length(sel) >= 1) sel else NULL)
    }, ignoreNULL = FALSE)
    observeEvent(list(input$omrade, input$perspektiv, input$larosate), {
      program_vald(NULL)
    }, ignoreInit = TRUE)

    # ---- Studerande och examina: stapel- och totaldata ----------------------
    # stapel: en rad per (ar, program, kon) - programmen (lärosätesperspektivet)
    #         resp. lärosätena (invånarperspektivet), eller ämnesområden (examina).
    # total:  en rad per (ar, kon) från egen granularitet (aldrig summa av stapel).
    stapel_data <- reactive({
      omr <- req(input$omrade)
      if (omr == "examina") {
        d <- filtrera_perspektiv(hamta_hogskola_examen())
        et <- input$examenstyp
        if (!is.null(et) && et != "_alla_") d <- dplyr::filter(d, examenstyp == et)
        return(d)
      }
      ak <- hamta_hogskola_aktivitet()
      if (ar_larosate()) filtrera_perspektiv(dplyr::filter(ak, granularitet == "Program"))
      else filtrera_perspektiv(dplyr::filter(ak, granularitet == "LarosateKon"), med_hskod = FALSE)
    })

    total_data <- reactive({
      if (identical(input$omrade, "examina")) return(stapel_data())   # examina går att summera
      ak <- hamta_hogskola_aktivitet()
      if (ar_larosate()) filtrera_perspektiv(dplyr::filter(ak, granularitet == "LarosateKon"))
      else filtrera_perspektiv(dplyr::filter(ak, granularitet == "TotaltKon"), med_hskod = FALSE)
    })

    # Staplar för valt år: de största programmen/lärosätena (ingen "Övriga",
    # eftersom deltagare inte får summeras över dem).
    stapel_ar <- reactive({
      ind <- valt_indikator()
      d <- dplyr::filter(stapel_data(), ar == as.integer(req(input$ar)))
      if (input$omrade == "examina" || nrow(d) == 0) return(d)
      storst <- d |>
        dplyr::group_by(program) |>
        dplyr::summarise(n = sum(.data[[ind$metrik]], na.rm = TRUE), .groups = "drop") |>
        dplyr::slice_max(n, n = .HOGSKOLA_ANTAL_STAPLAR, with_ties = FALSE) |>
        dplyr::pull(program)
      dplyr::filter(d, program %in% storst)
    })

    # ---- Etablering ------------------------------------------------------------
    etablering_alla <- reactive({
      if (ar_larosate()) hamta_hogskola_etablering(hskod_val()) else hamta_hogskola_etablering()
    })
    # Raderna för valt lärosäte (riket) eller valt område.
    etablering_omrade <- reactive({
      d <- etablering_alla()
      if (ar_larosate()) return(dplyr::filter(d, geo_niva == "riket"))
      gv <- input$geo_val
      if (is.null(gv) || gv == "_alla_") dplyr::filter(d, geo_niva == "lan")
      else dplyr::filter(d, geo_niva == "kommun", kommkod == gv)
    })

    antal_ar_vald <- reactiveVal("1")
    lapply(c("1", "3", "5", "alla"), function(v) {
      observeEvent(input[[paste0("arbtn_", v)]], { antal_ar_vald(v) }, ignoreInit = TRUE)
    })

    output$prog_niva_ui <- renderUI({
      req(valt_vy() == "etablering")
      d_et   <- etablering_omrade()
      alla   <- c("1", "3", "5")
      ar_val <- tryCatch(as.integer(input$ar), error = function(e) NA_integer_)
      if (is.na(ar_val) && nrow(d_et) > 0) ar_val <- max(d_et$ar, na.rm = TRUE)

      har_data <- function(av) {
        r <- d_et[d_et$antal_ar == as.integer(av) & d_et$ar %in% ar_val, , drop = FALSE]
        nrow(r) > 0 && any(!is.na(r$etabl))
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
      prg <- sort(unique(stats::na.omit(etablering_omrade()$program)))
      tidigare <- isolate(input$program_filter)
      vald <- if (!is.null(tidigare) && length(tidigare) > 0 && all(tidigare %in% prg)) tidigare else prg
      shinyWidgets::pickerInput(
        inputId = ns("program_filter"), label = "Filtrera ämnesområde",
        choices = prg, selected = vald, multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE,
          selectedTextFormat = "count > 3", countSelectedText = "{0} områden valda",
          selectAllText = "Välj alla", deselectAllText = "Avmarkera alla")
      )
    })

    etablering_dalarna <- reactive({
      req(valt_vy() == "etablering")
      d  <- etablering_omrade()
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

    # Jämförelse: riket, alla lärosäten (Region-raderna).
    etablering_riket_serie <- reactive({
      req(valt_vy() == "etablering")
      yh_etablering_riket(hamta_hogskola_etablering(), program_vald(), valt_indikator()$metrik)
    })

    etablering_riket_andel <- reactive({
      req(valt_vy() == "etablering")
      av <- antal_ar_vald()
      if (identical(av, "alla")) return(NA_real_)
      df <- etablering_riket_serie() |>
        dplyr::filter(antal_ar == as.integer(av), ar == as.integer(req(input$ar)))
      if (nrow(df) == 0) return(NA_real_)
      mean(df$andel_riket, na.rm = TRUE)
    })

    output$examenstyp_ui <- renderUI({
      req(identical(input$omrade, "examina"))
      shinyWidgets::radioGroupButtons(
        inputId = ns("examenstyp"), label = "Examenstyp",
        choices = c("Alla" = "_alla_", "Generell" = "Generell examen",
                    "Yrkes" = "Yrkesexamen", "Konstnärlig" = "Konstnärlig examen"),
        selected = if (is.null(isolate(input$examenstyp))) "_alla_" else isolate(input$examenstyp)
      )
    })

    output$ar_ui <- renderUI({
      vy <- tryCatch(valt_vy(), error = function(e) "dashboard")
      tidigare_ar <- isolate(input$ar)
      behall <- function(giltiga, standard) {
        if (!is.null(tidigare_ar) && tidigare_ar %in% as.character(giltiga)) tidigare_ar else standard
      }
      if (vy == "etablering") {
        ep <- etablering_omrade() |>
          dplyr::distinct(ar, exam_ar_interval) |>
          dplyr::arrange(dplyr::desc(ar))
        req(nrow(ep) > 0)
        etiketter <- gsub("-", "–", ep$exam_ar_interval, fixed = TRUE)
        selectInput(ns("ar"), "Examensperiod", choices = stats::setNames(ep$ar, etiketter),
                    selected = behall(ep$ar, ep$ar[1]))
      } else {
        ar <- sort(unique(stapel_data()$ar), decreasing = TRUE)
        req(length(ar) > 0)
        selectInput(ns("ar"), "År", choices = ar, selected = behall(ar, max(ar)))
      }
    })

    output$brodsmula <- renderText({
      omr <- hogskola_struktur[[req(input$omrade)]]$label
      persp <- if (ar_larosate()) "Lärosäte" else "Dalarnas invånare"
      paste0("Högskola › ", persp, " › ", omr, " › ", valt_indikator()$label)
    })

    output$vy <- renderUI({
      ind <- valt_indikator()
      if (!isTRUE(ind$klar)) return(div(class = "rd-info", "Den här vyn är inte inlagd än."))
      hint <- tags$p(class = "rd-hint rd-hint--bar",
                     paste0("Klicka på en stapel i diagrammet för att se utvecklingen för ett specifikt ",
                            dim_label(), "."))
      notis <- if (identical(ind$metrik, "deltagare"))
        tags$p(class = "rd-hint",
               paste0("En student kan läsa ", if (ar_larosate()) "flera program" else "vid flera lärosäten",
                      " samma år, så staplarna summerar inte till det totala antalet studenter."))
      kon_kontroll <- if (isTRUE(ind$kon))
        div(class = "rd-kon-kontroll",
            shinyWidgets::radioGroupButtons(
              inputId  = ns("kon_lage"), label = NULL,
              choices  = c("Könsuppdelat" = "kon", "Totalt" = "total"),
              selected = isolate(if (is.null(input$kon_lage)) "kon" else input$kon_lage),
              size = "sm"))
      trend_id <- if (valt_vy() == "etablering") "d_etablering_trend" else "d_trend"
      fluidRow(
        column(7, ggiraph::girafeOutput(ns("d_bar"), height = "560px"), kon_kontroll, hint, notis),
        column(5, div(class = "rd-subcard", ggiraph::girafeOutput(ns(trend_id), height = "380px")))
      )
    })

    output$d_bar <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      sub <- filter_underrubrik(med_ar = TRUE)

      if (valt_vy() == "etablering") {
        d <- etablering_dalarna()
        validate(need(nrow(d) > 0, "Inga data för valt urval."))
        d <- dplyr::filter(d, ar == as.integer(req(input$ar)))
        return(skapa_diagram_etablering_bar(d, ind$metrik, ind$metrik_label,
                                            antal_ar_val = antal_ar_vald(),
                                            riket_andel = etablering_riket_andel(),
                                            rubrik = ind$amne, underrubrik = sub, kalla = ind$kalla))
      }

      df <- stapel_ar()
      validate(need(nrow(df) > 0, "Inga data för valt urval."))
      # Studenter: totalen (unika) i underrubriken, eftersom staplarna inte summerar till den.
      if (identical(ind$metrik, "deltagare")) {
        tot <- sum(dplyr::filter(total_data(), ar == as.integer(input$ar))$deltagare, na.rm = TRUE)
        sub <- paste0(sub, " · Totalt ", scales::number(tot, big.mark = " "), " studenter")
      }
      rub <- paste0(ind$amne, " efter ", dim_label())
      if (kon_uppdelat()) {
        skapa_diagram_bar_kon(.kon_bred(df, ind$metrik, "program"), "kv", "man",
                              ind$metrik_label, input$ar,
                              rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      } else {
        skapa_diagram_bar(df, ind$metrik, ind$metrik_label, input$ar,
                          rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      }
    })

    output$d_etablering_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar), valt_vy() == "etablering")
      df_dal <- etablering_dalarna()
      validate(need(nrow(df_dal) > 0, "Inga data."))
      prog   <- program_vald()
      antal_ar_in <- antal_ar_vald()
      antal_ar_v  <- if (identical(antal_ar_in, "alla")) 1L else as.integer(antal_ar_in)
      sub <- paste0(filter_underrubrik(), " · ", antal_ar_v,
                    " år efter examen, per examensperiod")
      df_prog <- if (is.null(prog)) df_dal else dplyr::filter(df_dal, namn == prog)
      skapa_diagram_etablering_trend(df_prog, etablering_riket_serie(),
                                     ind$metrik, ind$metrik_label,
                                     antal_ar_val = antal_ar_v,
                                     rubrik = if (is.null(prog)) ind$amne else prog,
                                     underrubrik = sub, kalla = ind$kalla)
    })

    output$d_trend <- ggiraph::renderGirafe({
      # Etablering har egen trend (d_etablering_trend) - rita inte den här
      # med etableringsmåtten när vyn just bytts.
      ind <- valt_indikator(); req(isTRUE(ind$klar), valt_vy() != "etablering")
      prog <- program_vald()
      # Valt program/lärosäte: dess egen serie. Annars totalen.
      df <- if (is.null(prog)) total_data() else dplyr::filter(stapel_data(), program == prog)
      validate(need(nrow(df) > 0, "Inga data."))
      rub <- if (is.null(prog)) paste0(ind$amne, " – utveckling över tid")
             else paste0(ind$amne, " – ", prog)
      sub <- filter_underrubrik()
      if (kon_uppdelat()) {
        skapa_diagram_trend_kon(.kon_bred(df, ind$metrik, "ar"), "kv", "man", ind$metrik_label,
                                rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      } else {
        skapa_diagram_trend(df, ind$metrik, ind$metrik_label, NULL,
                            rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      }
    })

    # ---- Nedladdning -----------------------------------------------------------
    # Etableringsdata laddas ner summerad (se summera_etablering_nedladdning()).
    urval_data <- function() {
      if (valt_vy() == "etablering")
        return(summera_etablering_nedladdning(
          dplyr::filter(etablering_omrade(), ar == as.integer(req(input$ar)))))
      stapel_ar()
    }
    alla_data <- function() {
      if (valt_vy() == "etablering") return(summera_etablering_nedladdning(etablering_omrade()))
      stapel_data()
    }

    output$ladda_ner <- downloadHandler(
      filename = function() paste0("hogskola_", perspektiv(), "_", input$omrade, "_",
                                   input$indikator, "_", input$ar, ".xlsx"),
      content  = function(file) skriv_gymnasie_excel(urval_data(), file, blad = "Högskola")
    )
    output$ladda_ner_alla <- downloadHandler(
      filename = function() paste0("hogskola_", perspektiv(), "_", input$omrade, ".xlsx"),
      content  = function(file) skriv_gymnasie_excel(alla_data(), file, blad = "Högskola")
    )
  })
}
