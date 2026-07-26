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
      platser_program = list(label = "Platser", klar = TRUE, kon = FALSE,
                             amne = "Gymnasieplatser",
                             metrik = "platser", metrik_label = "Antal platser",
                             kalla = .KALLA_ANTAGNING),
      antagna_program = list(label = "Antagna", klar = TRUE, kon = TRUE,
                             amne = "Antagna gymnasieelever",
                             metrik = "antagna", metrik_kv = "antagna_kv", metrik_man = "antagna_man",
                             metrik_label = "Antal antagna", kalla = .KALLA_ANTAGNING),
      sokande_forsta  = list(label = "Förstahandssökande", klar = TRUE, kon = TRUE,
                             amne = "Förstahandssökande till gymnasiet",
                             metrik = "sok_1a", metrik_kv = "sok_1a_kv", metrik_man = "sok_1a_man",
                             metrik_label = "Förstahandssökande", kalla = .KALLA_ANTAGNING),
      outnyttjade     = list(label = "Outnyttjade platser", klar = TRUE, kon = FALSE,
                             amne = "Outnyttjade gymnasieplatser",
                             metrik = "lediga_platser", metrik_label = "Outnyttjade platser",
                             kalla = .KALLA_ANTAGNING),
      antagningspoang = list(label = "Antagningspoäng", klar = FALSE, kalla = .KALLA_ANTAGNING)
    )
  ),
  elever = list(
    label = "Elever",
    indikatorer = list(
      antal_elever  = list(label = "Antal elever", klar = TRUE, vy = "dashboard", kon = FALSE,
                           amne = "Antal gymnasieelever",
                           metrik = "antal_elever", metrik_label = "Antal elever",
                           kalla = "Skolverket"),
      elever_arskurs = list(label = "Elever per årskurs", klar = TRUE, vy = "arskurs", kon = FALSE,
                            amne = "Gymnasieelever per årskurs",
                            metrik = "antal_elever", metrik_label = "Antal elever",
                            kalla = "Skolverket"),
      andel_kvinnor = list(label = "Andel kvinnor", klar = TRUE, vy = "andel", kon = FALSE,
                           amne = "Andel kvinnor", metrik = "andel_kvinnor", vikt = "antal_elever",
                           metrik_label = "Andel kvinnor (%)", kalla = "Skolverket"),
      andel_utl     = list(label = "Utländsk bakgrund", klar = TRUE, vy = "andel", kon = FALSE,
                           amne = "Andel med utländsk bakgrund", metrik = "andel_utl", vikt = "antal_elever",
                           metrik_label = "Andel med utländsk bakgrund (%)", kalla = "Skolverket"),
      andel_hogutb  = list(label = "Högutbildade föräldrar", klar = TRUE, vy = "andel", kon = FALSE,
                           amne = "Andel med högutbildade föräldrar", metrik = "andel_hogutb", vikt = "antal_elever",
                           metrik_label = "Andel med högutbildade föräldrar (%)", kalla = "Skolverket")
    )
  ),
  resultat = list(
    label = "Resultat & examen",
    indikatorer = list(
      genomstromning = list(
        label        = "Andel med examen",
        klar         = TRUE,
        vy           = "genomstromning",
        kon          = FALSE,
        amne         = "Andel gymnasieelever med examen inom 4 år",
        metrik       = "andel",
        metrik_label = "Andel med examen (%)",
        kalla        = "Skolverket"
      )
    )
  ),
  etablering = list(
    label = "Etablering efter gymnasiet",
    indikatorer = list(
      etabl = list(label = "Etablerade", klar = TRUE, vy = "etablering", kon = FALSE,
                   amne = "Etablerade på arbetsmarknaden",
                   metrik = "etabl", metrik_label = "Andel etablerade",
                   kalla = "SCB/RAKS"),
      syss  = list(label = "Sysselsatta", klar = TRUE, vy = "etablering", kon = FALSE,
                   amne = "Sysselsatta efter gymnasiet",
                   metrik = "syss", metrik_label = "Andel sysselsatta",
                   kalla = "SCB/RAKS"),
      stud  = list(label = "Studerande", klar = TRUE, vy = "etablering", kon = FALSE,
                   amne = "Studerande efter gymnasiet",
                   metrik = "stud", metrik_label = "Andel studerande",
                   kalla = "SCB/RAKS"),
      arblos = list(label = "Arbetslösa", klar = TRUE, vy = "etablering", kon = FALSE,
                    amne = "Arbetslösa efter gymnasiet",
                    metrik = "arblos", metrik_label = "Andel arbetslösa",
                    kalla = "SCB/RAKS")
    )
  )
)

# Etiketter genereras rakt från label-fältet. CSS (white-space: normal +
# overflow-wrap) bryter texten automatiskt vid behov — \n i label-strängar
# behövs inte och ska inte användas.
.choices_fran_lista <- function(x) {
  labels <- vapply(x, function(e) e$label, character(1))
  stats::setNames(names(x), unname(labels))
}

# Null-koalesceringsoperator
`%||%` <- function(a, b) if (!is.null(a)) a else b

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
        uiOutput(ns("prog_niva_ui")),
        tags$hr(),

        shinyWidgets::radioGroupButtons(
          inputId = ns("geo_niva"), label = "Geografisk indelning",
          choices = c("Kommuner" = "kommun", "Samverkansområden" = "samverkansomrade"),
          selected = "kommun"
        ),
        shinyWidgets::pickerInput(
          inputId = ns("geo_val"), label = "Område", choices = NULL,
          options = shinyWidgets::pickerOptions(liveSearch = TRUE)
        ),
        uiOutput(ns("org_ui")),
        uiOutput(ns("ar_ui")),

        tags$hr(),
        uiOutput(ns("karta_rubrik")),
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
          div(class = "rd-brodsmula", textOutput(ns("brodsmula"))),
          uiOutput(ns("vy"))
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

    # Väljer datakälla utifrån statistikområde: Elever -> elevtabellen,
    # övriga områden -> antagningsdatan.
    aktuell_data <- reactive({
      omr <- input$omrade
      if (omr == "elever")      hamta_gymnasie_elever()
      else if (omr == "resultat")   hamta_genomstromning()
      else if (omr == "etablering") hamta_etablering()
      else                      hamta_gymnasiedata()
    })
    ar_elever <- reactive({ identical(input$omrade, "elever") })

    # Program/inriktning-väljare — visas bara för etablering
    output$prog_niva_ui <- renderUI({
      req(valt_vy() == "etablering")
      tagList(
        shinyWidgets::radioGroupButtons(
          inputId  = ns("prog_eller_inriktning"),
          label    = "Visa per",
          choices  = c("Program" = "program", "Inriktning" = "inriktning"),
          selected = isolate(input$prog_eller_inriktning) %||% "program",
          size = "sm"
        ),
        {
          # Vilka uppföljningsår har data för valt examensår? Datan saknar
          # rader helt för uppföljningsår som inte hunnit ske (t.ex. 3/5/7 år
          # efter examensperiod 2020), så vi gråar ut de knapparna.
          d_et   <- hamta_etablering()
          alla   <- c("1", "3", "5", "7")
          ar_val <- tryCatch(as.integer(input$ar), error = function(e) NA_integer_)
          if (is.na(ar_val))
            ar_val <- max(d_et$ar[d_et$geo_niva %in% c("lan", "kommun")], na.rm = TRUE)

          har_data <- function(av) {
            r <- d_et[d_et$antal_ar == as.integer(av) &
                        d_et$geo_niva %in% c("lan", "kommun") &
                        d_et$ar == ar_val, , drop = FALSE]
            nrow(r) > 0 && any(!is.na(r$syss) | !is.na(r$stud) |
                                 !is.na(r$arblos) | !is.na(r$etabl))
          }
          tillg <- alla[vapply(alla, har_data, logical(1))]
          if (length(tillg) == 0) tillg <- alla

          vald <- antal_ar_vald()
          # Om nuvarande val saknar data, hoppa till minsta tillgängliga
          if (vald != "alla" && !vald %in% tillg) {
            vald <- tillg[1]
            antal_ar_vald(vald)
          }

          # Bygg knappgrupp av actionButtons. En <button disabled> går garanterat
          # inte att klicka – ingen gissning om widget-intern HTML krävs.
          gor_knapp <- function(v, etikett) {
            aktiv <- identical(v, vald)
            btn <- actionButton(
              ns(paste0("arbtn_", v)), etikett,
              class = paste("btn btn-sm rd-arbtn", if (aktiv) "rd-arbtn-aktiv" else "")
            )
            if (v != "alla" && !v %in% tillg) shinyjs::disabled(btn) else btn
          }
          div(
            class = "rd-arbtn-grupp",
            tags$label(class = "control-label", "År efter examen"),
            div(class = "rd-arbtn-rad",
                gor_knapp("1", "1 år"),  gor_knapp("3", "3 år"),
                gor_knapp("5", "5 år"),  gor_knapp("7", "7 år"),
                gor_knapp("alla", "Alla år"))
          )
        },
        uiOutput(ns("program_filter_ui"))
      )
    })

    output$program_filter_ui <- renderUI({
      req(valt_vy() == "etablering")
      # Visa bara program som finns i Dalarna (kommun/län), inte de som
      # bara finns på riksnivå.
      d_et <- hamta_etablering()
      prg <- sort(unique(stats::na.omit(
        d_et$program[d_et$prog_niva == "program" &
                       d_et$geo_niva %in% c("kommun", "lan")]
      )))
      # Behåll tidigare urval om det finns – annars markera alla
      tidigare <- isolate(input$program_filter)
      vald <- if (!is.null(tidigare) && length(tidigare) > 0 &&
                  all(tidigare %in% prg)) tidigare else prg
      shinyWidgets::pickerInput(
        inputId  = ns("program_filter"),
        label    = "Filtrera program",
        choices  = prg,
        selected = vald,
        multiple = TRUE,
        options  = shinyWidgets::pickerOptions(
          actionsBox      = TRUE,
          liveSearch      = TRUE,
          selectedTextFormat = "count > 3",
          countSelectedText  = "{0} program valda",
          selectAllText      = "Välj alla",
          deselectAllText    = "Avmarkera alla"
        )
      )
    })

    # Etablering: Dalarna-data filtrerat på geo + exam_ar (startår)
    etablering_dalarna <- reactive({
      req(valt_vy() == "etablering")
      d <- aktuell_data() |> dplyr::filter(prog_niva == "program")
      gv  <- input$geo_val

      if (is.null(gv) || gv == "_alla_") {
        d_lan <- dplyr::filter(d, geo_niva == "lan")
        d <- if (nrow(d_lan) > 0) d_lan else dplyr::filter(d, geo_niva == "kommun")
      } else if (input$geo_niva == "kommun") {
        d <- dplyr::filter(d, geo_niva == "kommun", kommkod == gv)
      } else {
        d <- dplyr::filter(d, geo_niva == "kommun", samverkansomrade == gv)
      }

      # Driftsformsfilter
      org <- input$organisationstyp
      if (!is.null(org) && org != "_alla_" && "organisationstyp" %in% names(d))
        d <- dplyr::filter(d, organisationstyp == org)

      # Programfilter
      pf <- input$program_filter
      if (!is.null(pf) && length(pf) > 0) d <- dplyr::filter(d, program %in% pf)

      # Aggregera: summera antal över bo-/astkommun-celler till program-/
      # inriktningsnivå. Andelen beräknas nedströms i diagramfunktionerna.
      pi <- if (!is.null(input$prog_eller_inriktning) &&
                input$prog_eller_inriktning == "inriktning") "inriktning" else "program"

      grp <- c("ar", "exam_ar_interval", "antal_ar", "program")
      if (pi == "inriktning") grp <- c(grp, "inriktning")

      d <- d |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
        dplyr::summarise(
          dplyr::across(c(syss, stud, arblos, ovriga, etabl, antal, forvink_etabl),
                        ~sum(.x, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          namn = if (pi == "inriktning")
            paste0(program, " \u2013 ", inriktning)
          else
            program
        )
      d
    })

    etablering_riket_serie <- reactive({
      req(valt_vy() == "etablering")
      ind      <- valt_indikator()
      prog     <- program_vald()
      # NULL = medel över alla program i Riket (totalvy)
      etablering_riket(aktuell_data(), prog, ind$metrik)
    })

    # Riket-skalär för referenslinjen i stapeldiagrammet (valt antal_ar)
    etablering_riket_andel <- reactive({
      req(valt_vy() == "etablering")
      av <- antal_ar_vald()
      if (identical(av, "alla")) return(NA_real_)
      antal_ar_v <- as.integer(av)
      df <- etablering_riket_serie() |>
        dplyr::filter(antal_ar == antal_ar_v, ar == as.integer(req(input$ar)))
      if (nrow(df) == 0) return(NA_real_)
      mean(df$andel_riket, na.rm = TRUE)
    })

    output$org_ui <- renderUI({
      d <- aktuell_data()
      if (!"organisationstyp" %in% names(d)) return(NULL)
      # Visa bara organisationstyper som finns i Dalarna (kommun/län-nivå),
      # inte de som bara finns på riksnivå.
      d_dalarna <- if ("geo_niva" %in% names(d))
        dplyr::filter(d, geo_niva %in% c("kommun", "lan"))
      else d
      typer <- sort(unique(stats::na.omit(d_dalarna$organisationstyp)))
      typer <- typer[!typer %in% c("Alla", "Samtliga")]
      if (length(typer) <= 1) return(NULL)  # Dölj om bara en typ finns
      shinyWidgets::radioGroupButtons(
        inputId = ns("organisationstyp"), label = "Driftsform",
        choices = c("Alla" = "_alla_", stats::setNames(typer, typer)), selected = "_alla_"
      )
    })

    output$ar_ui <- renderUI({
      d  <- aktuell_data()
      vy <- tryCatch(valt_vy(), error = function(e) "dashboard")
      # Behåll tidigare valt år om det finns kvar bland de nya valen
      tidigare_ar <- isolate(input$ar)
      behall <- function(giltiga, standard) {
        if (!is.null(tidigare_ar) && tidigare_ar %in% as.character(giltiga))
          tidigare_ar else standard
      }

      if (vy == "genomstromning") {
        # Visa läsår (t.ex. "2021/22") men spara startåret som value för filtrering
        lasar_per_ar <- d |>
          dplyr::filter(geo_niva %in% c("lan", "kommun")) |>
          dplyr::distinct(ar, lasar) |>
          dplyr::arrange(dplyr::desc(ar))
        choices <- stats::setNames(lasar_per_ar$ar, lasar_per_ar$lasar)
        selectInput(ns("ar"), "Startläsår", choices = choices,
                    selected = behall(lasar_per_ar$ar, lasar_per_ar$ar[1]))
      } else if (vy == "etablering") {
        # Visa examensperiod (t.ex. "2020–2022") men spara startåret som value
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
        selectInput(ns("ar"), "År", choices = ar,
                    selected = behall(ar, max(ar)))
      }
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

    # Vy-typ för vald indikator (default = dashboard).
    valt_vy <- reactive({
      v <- valt_indikator()$vy
      if (is.null(v)) "dashboard" else v
    })

    kon_lage <- reactive({ if (is.null(input$kon_lage)) "kon" else input$kon_lage })

    geo_label <- reactive({
      gv <- input$geo_val
      if (is.null(gv) || gv == "_alla_") return("Dalarna")
      if (input$geo_niva == "kommun")
        dalarna_kommuner$kommun[match(gv, dalarna_kommuner$kommkod)]
      else gv
    })

    data_bas <- reactive({
      d  <- aktuell_data()
      gv <- req(input$geo_val)
      if (gv != "_alla_") {
        d <- if (input$geo_niva == "kommun")
          dplyr::filter(d, kommkod == gv) else dplyr::filter(d, samverkansomrade == gv)
      }
      org <- input$organisationstyp
      if (!is.null(org) && org != "_alla_" && "organisationstyp" %in% names(d))
        d <- dplyr::filter(d, organisationstyp == org)
      d
    })
    data_ar <- reactive({
      req(input$ar)
      dplyr::filter(data_bas(), ar == as.integer(input$ar))
    })

    # Enskilda program (för stapel/trend) – för elevdata filtreras aggregat-
    # och totalrader bort; för antagningsdata returneras allt oförändrat.
    data_bas_prog <- reactive({
      if (valt_vy() == "genomstromning")
        genomstromning_endast_program(data_bas())
      else
        elever_endast_program(data_bas())
    })
    data_ar_prog <- reactive({
      if (valt_vy() == "genomstromning")
        genomstromning_endast_program(data_ar())
      else
        elever_endast_program(data_ar())
    })

    # Genomströmning: Dalarna-data.
    # Vid "Hela Dalarna": använd länets aggregerade rad (geo_niva == "lan"),
    # vilket är Skolverkets korrekt viktade Dalarna-total.
    # Vid specifik kommun/område: filtrera kommunraderna som vanligt.
    genomstromning_dalarna <- reactive({
      req(valt_vy() == "genomstromning")
      d   <- aktuell_data()
      gv  <- input$geo_val
      org <- input$organisationstyp
      org_filter <- if (!is.null(org) && org != "_alla_") org else "Alla"

      if (is.null(gv) || gv == "_alla_") {
        # Hela Dalarna: Skolverkets länssummering (geo_niva == "lan")
        d |> dplyr::filter(geo_niva == "lan", organisationstyp == org_filter)
      } else if (input$geo_niva == "kommun") {
        d |> dplyr::filter(geo_niva == "kommun", kommkod == gv,
                           organisationstyp == org_filter)
      } else {
        d |> dplyr::filter(geo_niva == "kommun", samverkansomrade == gv,
                           organisationstyp == org_filter)
      }
    })

    # Riket-serien för trenddiagrammet. Programmet väljs baserat på valt
    # program (klickval) eller "Nationella program" som standard.
    genomstromning_riket_serie <- reactive({
      req(valt_vy() == "genomstromning")
      prog <- if (!is.null(program_vald())) program_vald() else "Nationella program"
      org  <- input$organisationstyp
      org_r <- if (is.null(org) || org == "_alla_") "Alla" else org
      genomstromning_riket(aktuell_data(), program_val = prog, org = org_r)
    })

    program_vald <- reactiveVal(NULL)

    # Vald uppföljningsår (1/3/5/7/alla) för etablering. Egen reactiveVal
    # eftersom årsknapparna är actionButtons (för pålitlig gråning).
    antal_ar_vald <- reactiveVal("1")
    lapply(c("1", "3", "5", "7", "alla"), function(v) {
      observeEvent(input[[paste0("arbtn_", v)]], {
        antal_ar_vald(v)
      }, ignoreInit = TRUE)
    })

    observeEvent(input$d_bar_selected, {
      sel <- input$d_bar_selected
      program_vald(if (length(sel) >= 1) sel else NULL)
    }, ignoreNULL = FALSE)
    # Klickval och filter ligger kvar så länge man är kvar i samma
    # statistikområde (Antagning/Elever/Resultat/Etablering). De nollställs
    # BARA när man byter statistikområde – inte vid indikatorbyte och inte
    # vid ändring av examensperiod, geografi, driftsform osv.
    observeEvent(input$omrade, {
      program_vald(NULL)
      # Nollställ programfiltret (visa alla) vid byte av statistikområde
      if (valt_vy() == "etablering") {
        d_et <- hamta_etablering()
        prg <- sort(unique(stats::na.omit(
          d_et$program[d_et$prog_niva == "program" &
                         d_et$geo_niva %in% c("kommun", "lan")]
        )))
        shinyWidgets::updatePickerInput(session, "program_filter", selected = prg)
      }
    }, ignoreInit = TRUE)

    output$brodsmula <- renderText({
      omr <- gymnasiet_struktur[[req(input$omrade)]]$label
      paste0("Gymnasiet › ", omr, " › ", valt_indikator()$label)
    })

    # Filtrering som ska synas i varje diagrams underrubrik. Driftsform tas bara
    # med när man filtrerat (inte "Alla"). med_ar lägger till valt år.
    filter_underrubrik <- function(med_ar = FALSE) {
      bitar <- geo_label()
      org <- input$organisationstyp
      if (!is.null(org) && org != "_alla_") bitar <- c(bitar, org)
      if (med_ar) bitar <- c(bitar, as.character(req(input$ar)))
      paste(bitar, collapse = " · ")
    }

    output$vy <- renderUI({
      ind <- valt_indikator()
      if (!isTRUE(ind$klar)) {
        return(div(class = "rd-info",
                   "Den här vyn är inte inlagd än – kommer i en senare version."))
      }
      hint <- tags$p(class = "rd-hint rd-hint--bar",
                     "Klicka på en stapel i diagrammet för att se statistik för ett specifikt program.")

      if (valt_vy() == "etablering") {
        fluidRow(
          column(7, hint,
                 ggiraph::girafeOutput(ns("d_bar"), height = "560px")),
          column(5,
                 div(class = "rd-subcard",
                     ggiraph::girafeOutput(ns("d_etablering_trend"), height = "380px")))
        )
      } else if (valt_vy() == "dashboard") {
        fluidRow(
          column(7, hint,
                 ggiraph::girafeOutput(ns("d_bar"), height = "470px"),
                 uiOutput(ns("kon_kontroll"))),
          column(5,
                 div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_trend"), height = "250px")),
                 div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_programtyp"), height = "300px")))
        )
      } else if (valt_vy() == "genomstromning") {
        # Stapel per program (vänster) + trendlinje Dalarna vs Riket (höger).
        fluidRow(
          column(7, hint,
                 ggiraph::girafeOutput(ns("d_bar"), height = "470px")),
          column(5,
                 div(class = "rd-subcard",
                     ggiraph::girafeOutput(ns("d_genomstromning_trend"), height = "300px")))
        )
      } else {
        # Årskurs och andel: stapel + en trend (ingen programtypsruta).
        fluidRow(
          column(7, hint,
                 ggiraph::girafeOutput(ns("d_bar"), height = "470px")),
          column(5,
                 div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_trend"), height = "300px")))
        )
      }
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
      sub <- filter_underrubrik(med_ar = TRUE)

      if (valt_vy() == "etablering") {
        df_all <- etablering_dalarna()
        # Om input$ar inte finns i etableringsdatan (t.ex. vid byte från Antagning),
        # välj det senaste tillgängliga året i stället.
        ar_val <- as.integer(req(input$ar))
        tillg  <- sort(unique(df_all$ar), decreasing = TRUE)
        if (!ar_val %in% tillg) ar_val <- tillg[1]
        df_et <- dplyr::filter(df_all, ar == ar_val)
        validate(need(nrow(df_et) > 0, "Inga data för valt urval."))
        antal_ar_in <- antal_ar_vald()
        exam_txt    <- gsub("-", "\u2013", unique(df_et$exam_ar_interval)[1], fixed = TRUE)
        if (identical(antal_ar_in, "alla")) {
          sub_et <- paste0(filter_underrubrik(), " · examensperiod ", exam_txt,
                           " · alla uppföljningsår")
          skapa_diagram_etablering_bar(
            df_et, ind$metrik, ind$metrik_label,
            antal_ar_val = "alla",
            riket_andel  = NULL,
            rubrik = ind$amne, underrubrik = sub_et, kalla = ind$kalla
          )
        } else {
          antal_ar_v <- as.integer(antal_ar_in)
          sub_et <- paste0(filter_underrubrik(), " · examensperiod ", exam_txt,
                           " · ", antal_ar_v, " år efter examen")
          skapa_diagram_etablering_bar(
            df_et, ind$metrik, ind$metrik_label,
            antal_ar_val  = antal_ar_v,
            riket_andel   = etablering_riket_andel(),
            rubrik        = ind$amne,
            underrubrik   = sub_et,
            kalla         = ind$kalla
          )
        }
      } else if (valt_vy() == "genomstromning") {
        df_gs <- genomstromning_dalarna() |>
          dplyr::filter(ar == as.integer(req(input$ar)),
                        prog_niva == "program")
        validate(need(nrow(df_gs) > 0, "Inga data för valt urval."))
        lasar_txt <- if (nrow(df_gs) > 0) df_gs$lasar[1] else as.character(input$ar)
        sub_gs <- paste0(filter_underrubrik(), " · startläsår ", lasar_txt)
        skapa_diagram_genomstromning_bar(df_gs, input$ar,
                                         rubrik = ind$amne,
                                         underrubrik = sub_gs, kalla = ind$kalla)
      } else {
        df <- data_ar_prog()
        validate(need(nrow(df) > 0, "Inga data för valt urval."))
        if (valt_vy() == "arskurs") {
          skapa_diagram_arskurs(df, input$ar, rubrik = ind$amne,
                                underrubrik = sub, kalla = ind$kalla)
        } else if (valt_vy() == "andel") {
          skapa_diagram_bar_andel(df, ind$metrik, ind$vikt, ind$metrik_label, input$ar,
                                  rubrik = paste0(ind$amne, " efter program"),
                                  underrubrik = sub, kalla = ind$kalla)
        } else if (isTRUE(ind$kon) && kon_lage() == "kon") {
          skapa_diagram_bar_kon(df, ind$metrik_kv, ind$metrik_man, ind$metrik_label, input$ar,
                                rubrik = paste0(ind$amne, " efter program"),
                                underrubrik = sub, kalla = ind$kalla)
        } else {
          skapa_diagram_bar(df, ind$metrik, ind$metrik_label, input$ar,
                            rubrik = paste0(ind$amne, " efter program"),
                            underrubrik = sub, kalla = ind$kalla)
        }
      }
    })

    output$d_etablering_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar), valt_vy() == "etablering")
      df_dal <- etablering_dalarna()
      validate(need(nrow(df_dal) > 0, "Inga data."))
      df_rik <- etablering_riket_serie()
      prog   <- program_vald()
      rub    <- if (is.null(prog)) ind$amne else prog
      # Vid "alla": trenden visar 1 år som standard (annars vore den oläslig).
      antal_ar_in <- antal_ar_vald()
      antal_ar_v  <- if (identical(antal_ar_in, "alla")) 1L else as.integer(antal_ar_in)
      # Underrubriken förklarar vad linjen visar: uppföljning efter X år,
      # ett värde per examensperiod (x-axeln).
      sub <- paste0(filter_underrubrik(), " · ", antal_ar_v,
                    " år efter examen, per examensperiod")
      df_prog <- if (is.null(prog)) df_dal else dplyr::filter(df_dal, namn == prog)
      skapa_diagram_etablering_trend(df_prog, df_rik,
                                     ind$metrik, ind$metrik_label,
                                     antal_ar_val = antal_ar_v,
                                     rubrik = rub, underrubrik = sub,
                                     kalla = ind$kalla)
    })

    output$d_genomstromning_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar), valt_vy() == "genomstromning")
      df_dal  <- genomstromning_dalarna()
      validate(need(nrow(df_dal) > 0, "Inga data."))
      df_rik  <- genomstromning_riket_serie()
      prog    <- program_vald()
      rub     <- if (is.null(prog)) ind$amne else prog
      # Senaste läsåret ur datan (t.ex. "2021/22") för underrubriken
      senaste_lasar <- df_dal$lasar[which.max(df_dal$ar)]
      sub     <- paste0(filter_underrubrik(), " · startläsår ", senaste_lasar)
      df_prog <- if (is.null(prog))
        dplyr::filter(df_dal, program == "Nationella program")
      else
        dplyr::filter(df_dal, program == prog)
      skapa_diagram_genomstromning_trend(df_prog, df_rik,
                                         rubrik = rub, underrubrik = sub,
                                         kalla = ind$kalla)
    })

    output$d_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      df <- data_bas_prog()
      validate(need(nrow(df) > 0, "Inga data."))
      prog <- program_vald()
      rub  <- if (is.null(prog)) paste0(ind$amne, " – utveckling över tid")
      else paste0(ind$amne, " – ", prog)
      # Underrubrik: filter + det senaste valda året (trenddiagrammet visar
      # alla år men filtret gäller driftsform och geografi).
      sub  <- paste0(filter_underrubrik(), " · t.o.m. ", req(input$ar))

      if (valt_vy() == "arskurs") {
        skapa_diagram_trend_arskurs(df, prog, rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      } else if (valt_vy() == "andel") {
        skapa_diagram_trend_andel(df, ind$metrik, ind$vikt, ind$metrik_label, prog,
                                  rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      } else if (isTRUE(ind$kon) && kon_lage() == "kon") {
        skapa_diagram_trend_kon(df, ind$metrik_kv, ind$metrik_man, ind$metrik_label, prog,
                                rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      } else {
        skapa_diagram_trend(df, ind$metrik, ind$metrik_label, prog,
                            rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      }
    })

    output$d_programtyp <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar), valt_vy() == "dashboard")
      df <- data_bas()
      validate(need(nrow(df) > 0, "Inga data."))
      skapa_diagram_programtyp(df, ind$metrik, ind$metrik_label, program_vald(),
                               rubrik = paste0(ind$amne, " – andel efter programtyp"),
                               underrubrik = paste0(filter_underrubrik(), " · t.o.m. ", req(input$ar)),
                               kalla = ind$kalla)
    })

    output$karta_rubrik <- renderUI({
      rubrik <- if (identical(input$geo_niva, "samverkansomrade")) "Samverkansområden" else "Kommuner"
      tags$div(class = "rd-label", rubrik)
    })

    output$karta <- ggiraph::renderGirafe({
      k <- skapa_karta_samverkan(input$geo_niva)
      validate(need(!is.null(k),
                    "Kartan kunde inte läsas (kräver sf och åtkomst till geodata-databasen)."))
      k
    })

    output$ladda_ner <- downloadHandler(
      filename = function() paste0(input$omrade, "_", input$indikator, "_", input$ar, ".xlsx"),
      content  = function(file) skriv_gymnasie_excel(data_ar(), file)
    )
    output$ladda_ner_alla <- downloadHandler(
      filename = function() paste0(input$omrade, "_hela_datasetet.xlsx"),
      content  = function(file) skriv_gymnasie_excel(aktuell_data(), file)
    )
  })
}
