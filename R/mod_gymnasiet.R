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
.KALLA_GYMN_ETABL <- kalla_rud("Gymnasiet – avgångna och RAKS/BAS")
# Röjandekontroll (MIN_ANTAL) gäller bara etableringen, som är mikrodata.
# Övriga diagram bygger på publicerad statistik och anropas med min_antal = 0.

gymnasiet_struktur <- list(
  antagning = list(
    label = "Antagning",
    indikatorer = list(
      antagna_program = list(label = "Antagna", klar = TRUE, kon = TRUE,
                             amne = "Antagna gymnasieelever",
                             metrik = "antagna", metrik_kv = "antagna_kv", metrik_man = "antagna_man",
                             metrik_label = "Antal antagna", kalla = .KALLA_ANTAGNING,
                             beskrivning = paste(
                               "Antalet antagna i början av september, när Gymnasieantagningen",
                               "på Dalarnas kommunförbund lämnar över kommande programbyten med",
                               "mera till skolorna.")),
      sokande_forsta  = list(label = "Förstahandssökande", klar = TRUE, kon = TRUE,
                             amne = "Förstahandssökande till gymnasiet",
                             metrik = "sok_1a", metrik_kv = "sok_1a_kv", metrik_man = "sok_1a_man",
                             metrik_label = "Förstahandssökande", kalla = .KALLA_ANTAGNING,
                             beskrivning = paste(
                               "Antal förstahandssökande. Precis som Antagna är detta",
                               "septembersiffror.")),
      platser_program = list(label = "Platser", klar = TRUE, kon = FALSE,
                             amne = "Gymnasieplatser",
                             metrik = "platser", metrik_label = "Antal platser",
                             kalla = .KALLA_ANTAGNING,
                             beskrivning = paste(
                               "Det antal platser som utbildningsanordnarna har angivit",
                               "när antagningsprocessen startar i januari.")),
      outnyttjade     = list(label = "Outnyttjade platser", klar = TRUE, kon = FALSE,
                             amne = "Outnyttjade gymnasieplatser",
                             metrik = "lediga_platser", metrik_label = "Outnyttjade platser",
                             kalla = .KALLA_ANTAGNING,
                             beskrivning = "Skillnaden mellan Platser och Antagna."),
      antagningspoang = list(label = "Antagningspoäng", klar = FALSE, kalla = .KALLA_ANTAGNING)
    )
  ),
  elever = list(
    label = "Elever",
    indikatorer = list(
      antal_elever  = list(label = "Antal elever", klar = TRUE, vy = "dashboard", kon = FALSE,
                           amne = "Antal gymnasieelever",
                           metrik = "antal_elever", metrik_label = "Antal elever",
                           kalla = "Skolverket",
                           beskrivning = paste(
                             "Rapporten visar antal elever i gymnasieskolan fördelat per",
                             "årskurs. Uppgifterna avser oktober det valda året. Redovisas",
                             "totalt och per program.")),
      elever_arskurs = list(label = "Elever per årskurs", klar = TRUE, vy = "arskurs", kon = FALSE,
                            amne = "Gymnasieelever per årskurs",
                            metrik = "antal_elever", metrik_label = "Antal elever",
                            kalla = "Skolverket",
                            beskrivning = "Antal elever per årskurs totalt och per program."),
      andel_kvinnor = list(label = "Andel kvinnor", klar = TRUE, vy = "andel", kon = FALSE,
                           amne = "Andel kvinnor", metrik = "andel_kvinnor", vikt = "antal_elever",
                           metrik_label = "Andel kvinnor (%)", kalla = "Skolverket",
                           beskrivning = "Andel kvinnor av alla elever totalt och per program."),
      andel_utl     = list(label = "Utländsk bakgrund", klar = TRUE, vy = "andel", kon = FALSE,
                           amne = "Andel med utländsk bakgrund", metrik = "andel_utl", vikt = "antal_elever",
                           metrik_label = "Andel med utländsk bakgrund (%)", kalla = "Skolverket",
                           beskrivning = paste(
                             "Med utländsk bakgrund avses att eleven är född utomlands eller",
                             "född i Sverige med två utlandsfödda föräldrar. Elever med okänd",
                             "bakgrund räknas i denna statistik till elever med utländsk",
                             "bakgrund. Med okänd bakgrund avses elever som inte var",
                             "folkbokförda per den 30 september innevarande läsår. Redovisas",
                             "totalt och per program.")),
      andel_hogutb  = list(label = "Högutbildade föräldrar", klar = TRUE, vy = "andel", kon = FALSE,
                           amne = "Andel med högutbildade föräldrar", metrik = "andel_hogutb", vikt = "antal_elever",
                           metrik_label = "Andel med högutbildade föräldrar (%)", kalla = "Skolverket",
                           beskrivning = paste(
                             "Högutbildade föräldrar innebär att eleven har minst en förälder",
                             "vars högsta utbildning är eftergymnasial."))
    )
  ),
  resultat = list(
    label = "Genomströmning",
    indikatorer = list(
      genomstromning = list(
        label        = "Andel med examen",
        klar         = TRUE,
        vy           = "genomstromning",
        kon          = FALSE,
        amne         = "Andel gymnasieelever med examen inom 4 år",
        metrik       = "andel",
        metrik_label = "Andel med examen (%)",
        kalla        = "Skolverket",
        beskrivning  = paste(
          "För att få slutbetyg från gymnasieskolan krävs att eleven fått",
          "betyg i alla kurser som ingår i programmet. Måttet för t.ex. år",
          "2019 avser andelen av nybörjarna hösten 2019 som erhållit",
          "slutbetyg t.o.m. läsåret 2021/2022.")
      )
    )
  ),
  etablering = list(
    label = "Etablering efter gymnasiet",
    indikatorer = list(
      etabl = list(label = "Etablerade", klar = TRUE, vy = "etablering", kon = FALSE,
                   amne = "Etablerade på arbetsmarknaden",
                   metrik = "etabl", metrik_label = "Andel etablerade",
                   kalla = .KALLA_GYMN_ETABL,
                   beskrivning = paste(
                     "Det är endast personer som är anställda (helårsanställd,",
                     "nyanställd, avgången eller delårsanställd) som kan definieras",
                     "som etablerade. Personer som är egna företagare eller har",
                     "fåmansaktiebolag kan i viss mån välja hur mycket ersättning de",
                     "ska ta ut ur företagen, det vill säga styra sin egen",
                     "företagarinkomst. Detta gör det i praktiken omöjligt att",
                     "beräkna etablering för denna grupp på basis av inkomsten.",
                     "Två definitioner har använts över tid: före år 2020 ett",
                     "gränsvärde beräknat som 60 % av medianinkomsten för personer",
                     "med kort förgymnasial utbildning, per åldersgrupp och kön; från",
                     "och med år 2020 en inkomst på minst 3 inkomstbasbelopp. I båda",
                     "fallen får personen heller inte ha haft arbetslöshetsersättning",
                     "under året för att klassas som etablerad.")),
      syss  = list(label = "Sysselsatta", klar = TRUE, vy = "etablering", kon = FALSE,
                   amne = "Sysselsatta efter gymnasiet",
                   metrik = "syss", metrik_label = "Andel sysselsatta",
                   kalla = .KALLA_GYMN_ETABL,
                   beskrivning = "Sysselsatt är den som har sin största inkomst från arbete."),
      stud  = list(label = "Studerande", klar = TRUE, vy = "etablering", kon = FALSE,
                   amne = "Studerande efter gymnasiet",
                   metrik = "stud", metrik_label = "Andel studerande",
                   kalla = .KALLA_GYMN_ETABL,
                   beskrivning = "Studerande är den som har sin största inkomst från studier."),
      arblos = list(label = "Arbetslösa", klar = TRUE, vy = "etablering", kon = FALSE,
                    amne = "Arbetslösa efter gymnasiet",
                    metrik = "arblos", metrik_label = "Andel arbetslösa",
                    kalla = .KALLA_GYMN_ETABL,
                    beskrivning = paste(
                      "Arbetslös är den som har sin största inkomst från",
                      "arbetslöshetsersättningar."))
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
        # Avgångna (egen modul, mod_gymnasiet_avgangna.R) före Etablering.
        choices  = append(.choices_fran_lista(gymnasiet_struktur),
                          c("Avgångna" = "avgangna"),
                          after = which(names(gymnasiet_struktur) == "resultat")),
        selected = names(gymnasiet_struktur)[1]
      )
    ),

    conditionalPanel(
      condition = "input.omrade == 'avgangna'", ns = ns,
      mod_gymnasiet_avgangna_ui(ns("avg"))
    ),

    conditionalPanel(
      condition = "input.omrade != 'avgangna'", ns = ns,
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
          inputId = ns("geo_val"), label = "Område", choices = NULL, multiple = TRUE,
          options = shinyWidgets::pickerOptions(liveSearch = TRUE, selectedTextFormat = "count > 3",
                                                countSelectedText = "{0} områden valda")
        ),
        tags$p(class = "rd-hint", "Välj flera med Ctrl-klick (Cmd på Mac) i listan eller kartan."),
        # Talar om för servern om klicket i listrutan gjordes med Ctrl/Cmd
        # (fångas före bootstrap-selects egen klickhantering).
        tags$script(HTML(sprintf(
          "document.addEventListener('click', function(e) {
             var bs = e.target.closest && e.target.closest('.bootstrap-select');
             if (bs && bs.querySelector('select#%s') && e.target.closest('.dropdown-menu'))
               Shiny.setInputValue('%s', e.ctrlKey || e.metaKey, {priority: 'event'});
           }, true);", ns("geo_val"), ns("geo_val_ctrl")))),
        uiOutput(ns("org_ui")),
        uiOutput(ns("ar_ui")),

        tags$hr(),
        uiOutput(ns("karta_rubrik")),
        ggiraph::girafeOutput(ns("karta"), height = "320px"),
        # Talar om för servern om kartklicket gjordes med Ctrl/Cmd nedtryckt
        # (fångas före ggiraphs egen klickhantering).
        tags$script(HTML(sprintf(
          "document.addEventListener('click', function(e) {
             if (e.target.closest && e.target.closest('#%s'))
               Shiny.setInputValue('%s', e.ctrlKey || e.metaKey, {priority: 'event'});
           }, true);", ns("karta"), ns("karta_ctrl")))),

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
  )
}

# ---- Server ----------------------------------------------------------------
mod_gymnasiet_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Statistikområdet Avgångna har en egen modul (mod_gymnasiet_avgangna.R).
    mod_gymnasiet_avgangna_server("avg")

    ns <- session$ns

    output$indikator_ui <- renderUI({
      # Avgångna har egen modul och finns inte i gymnasiet_struktur.
      req(input$omrade %in% names(gymnasiet_struktur))
      omr <- gymnasiet_struktur[[input$omrade]]
      ch   <- .choices_fran_lista(omr$indikatorer)
      # value -> beskrivning, bara för indikatorer som har en beskrivning satt.
      # OBS: radioGroupButtons() bygger sina <button>-taggar lat (via
      # htmltools::tagFunction) först vid rendering i webbläsaren, så vi kan
      # inte gå igenom R-taggträdet och sätta title= direkt här (de finns
      # inte som R-objekt än). I stället skickar vi med beskrivningarna som
      # JSON i ett data-attribut (INTE som text-innehåll i <script> – det
      # HTML-kodas av htmltools och webbläsaren avkodar det inte igen inuti
      # en <script>-tagg, så JSON.parse() skulle fallera tyst). Attribut
      # HTML-avkodas alltid korrekt. En liten JS-snutt (www/tooltips.js)
      # läser attributet och sätter title på rätt knapp i den färdiga DOM:en.
      besk <- omr$indikatorer |>
        purrr::map("beskrivning") |>
        purrr::compact()

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
      gv  <- geo_vald()

      if (identical(gv, "_alla_")) {
        d_lan <- dplyr::filter(d, geo_niva == "lan")
        d <- if (nrow(d_lan) > 0) d_lan else dplyr::filter(d, geo_niva == "kommun")
      } else if (input$geo_niva == "kommun") {
        d <- dplyr::filter(d, geo_niva == "kommun", kommkod %in% gv)
      } else {
        d <- dplyr::filter(d, geo_niva == "kommun", samverkan_i(samverkansomrade, gv))
      }

      # Driftsformsfilter
      d <- filtrera_driftsform(d, input$organisationstyp)

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

    # ---- Områdesval: ett eller flera områden ---------------------------------
    # geo_vald() = "_alla_" (Hela Dalarna) eller en vektor med kommunkoder
    # resp. samverkansområden. Flera kommuner räknas som ett samverkansområde:
    # kommunraderna summeras/viktas ihop.
    geo_vald_rv <- reactiveVal("_alla_")
    geo_vald <- reactive(geo_vald_rv())

    observeEvent(input$geo_niva, {
      val <- if (input$geo_niva == "kommun") geo_val_kommun
             else c(stats::setNames(GYSAM_HELA, GYSAM_HELA), geo_val_samverkan)   # Hela Gysam överst
      geo_vald_rv("_alla_")
      karta_forra(character(0))      # kartan ritas om utan urval
      shinyWidgets::updatePickerInput(
        session, "geo_val",
        choices = c("Hela Dalarna" = "_alla_", val), selected = "_alla_")
    }, ignoreInit = FALSE)

    # Kartans markering följer valet (ggiraph: meddelandet <id>_set).
    karta_ids <- function(gv) {
      if (identical(gv, "_alla_")) character(0)
      else if (identical(input$geo_niva, "samverkansomrade")) samverkan_expandera(gv)
      else gv
    }
    karta_forra <- reactiveVal(character(0))   # kartans senast kända urval
    synka_karta <- function(gv) {
      karta_forra(karta_ids(gv))
      session$sendCustomMessage(paste0(ns("karta"), "_set"), as.list(karta_ids(gv)))
    }

    # Listrutan fungerar som kartan: vanligt klick väljer bara det området
    # (klick på enda valda -> Hela Dalarna), Ctrl/Cmd-klick lägger till eller
    # tar bort. "Hela Dalarna" utesluter övriga val.
    observeEvent(input$geo_val, {
      sel   <- input$geo_val
      forra <- geo_vald_rv()
      klickad <- union(setdiff(sel, forra), setdiff(forra, sel))
      if (length(klickad) == 1 && !isTRUE(isolate(input$geo_val_ctrl))) {
        sel <- if (klickad %in% sel) klickad                     # valt: bara det
               else if (setequal(forra, klickad)) "_alla_"       # enda valda avmarkerat
               else klickad                                      # ett av flera: bara det
      }
      ny <- if (length(sel) == 0) "_alla_"
            else if ("_alla_" %in% sel && length(sel) > 1)
              (if ("_alla_" %in% forra) setdiff(sel, "_alla_") else "_alla_")
            else sel
      if (!setequal(ny, sel)) shinyWidgets::updatePickerInput(session, "geo_val", selected = ny)
      if (!setequal(ny, forra)) geo_vald_rv(ny)
      synka_karta(ny)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Kartklick: vanligt klick väljer bara det området (klick på enda valda
    # området -> Hela Dalarna), Ctrl/Cmd-klick lägger till eller tar bort.
    # Kartan har urvalstypen "multiple"; det klickade området är det som
    # skiljer nytt urval från förra. Servern synkar sedan kartan till valet.
    observeEvent(input$karta_selected, {
      sel   <- if (is.null(input$karta_selected)) character(0) else input$karta_selected
      forra <- karta_forra()
      karta_forra(sel)
      aktuell <- geo_vald_rv()
      if (setequal(sel, karta_ids(aktuell))) return()        # serverns egen synkning
      klickad <- union(setdiff(sel, forra), setdiff(forra, sel))
      if (length(klickad) != 1) return()
      valda <- setdiff(aktuell, "_alla_")
      ny <- if (isTRUE(isolate(input$karta_ctrl))) {
        if (klickad %in% valda) setdiff(valda, klickad) else c(valda, klickad)
      } else if (identical(valda, klickad)) character(0) else klickad
      if (length(ny) == 0) ny <- "_alla_"
      shinyWidgets::updatePickerInput(session, "geo_val", selected = ny)
      geo_vald_rv(ny)
      synka_karta(ny)
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
      gv <- geo_vald()
      if (identical(gv, "_alla_")) return("Dalarna")
      namn <- if (input$geo_niva == "kommun")
        dalarna_kommuner$kommun[match(gv, dalarna_kommuner$kommkod)] else gv
      if (length(namn) <= 3) paste(namn, collapse = ", ")
      else paste(length(namn), if (input$geo_niva == "kommun") "kommuner" else "samverkansområden")
    })

    # Driftsformsfilter, delad logik för alla statistikområden. Om en
    # specifik driftsform är vald filtreras till den. Annars ("Alla"): om
    # datan har en egen färdig totalrad (organisationstyp == "Alla" – t.ex.
    # Skolverkets "Samtliga") används DEN direkt i stället för att appen
    # själv summerar/väger ihop de enskilda driftsformerna. Saknar datan en
    # sådan rad (t.ex. antagningsdata från Dalarnas kommunförbund) lämnas
    # den orörd, så driftsformerna summeras ihop nedströms som tidigare.
    filtrera_driftsform <- function(d, org) {
      if (!"organisationstyp" %in% names(d)) return(d)
      if (!is.null(org) && org != "_alla_") {
        dplyr::filter(d, organisationstyp == org)
      } else if ("Alla" %in% d$organisationstyp) {
        dplyr::filter(d, organisationstyp == "Alla")
      } else {
        d
      }
    }

    data_bas <- reactive({
      d  <- aktuell_data()
      gv <- geo_vald()

      if ("geo_niva" %in% names(d)) {
        # Datan har egna färdigviktade läns-/kommunrader (elever). Använd
        # länsraden direkt för "Hela Dalarna" i stället för att vikta ihop
        # kommunraderna själva – ger Skolverkets egna, redan korrekt
        # viktade tal (samma mönster som genomströmning/etablering).
        if (identical(gv, "_alla_")) {
          d <- dplyr::filter(d, geo_niva == "lan")
        } else if (input$geo_niva == "kommun") {
          # En kommun: dess färdiga rader. Flera: kommunraderna viktas ihop
          # nedströms, som för samverkansområden.
          d <- dplyr::filter(d, geo_niva == "kommun", kommkod %in% gv)
        } else {
          # Samverkansområde: ingen färdig aggregatrad finns på den nivån,
          # så där vaktar vi ihop kommunraderna som tillhör området.
          d <- dplyr::filter(d, geo_niva == "kommun", samverkan_i(samverkansomrade, gv))
        }
      } else if (!identical(gv, "_alla_")) {
        d <- if (input$geo_niva == "kommun")
          dplyr::filter(d, kommkod %in% gv) else dplyr::filter(d, samverkan_i(samverkansomrade, gv))
      }

      d <- filtrera_driftsform(d, input$organisationstyp)
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
      gv  <- geo_vald()
      org <- input$organisationstyp
      org_filter <- if (!is.null(org) && org != "_alla_") org else "Alla"

      if (identical(gv, "_alla_")) {
        # Hela Dalarna: Skolverkets länssummering (geo_niva == "lan").
        # Redan en färdig rad per program – ingen viktning behövs.
        d |> dplyr::filter(geo_niva == "lan", organisationstyp == org_filter)
      } else if (input$geo_niva == "kommun" && length(gv) == 1) {
        # Enskild kommun: också redan en färdig rad per program.
        d |> dplyr::filter(geo_niva == "kommun", kommkod == gv,
                           organisationstyp == org_filter)
      } else {
        # Flera kommuner eller samverkansområde: viktas ihop (se nedan).
        # Samverkansområde: flera kommuners rader ska slås ihop. Skolverket
        # har ingen färdig aggregatrad på den här nivån (samverkansområden
        # är en lokal Region Dalarna-indelning), så vi joinar in elevantal
        # från elevtabellen som viktningsunderlag (se
        # hamta_elevantal_vikt() i func_data.R) i stället för ett ovägt
        # medel. Om en programrad saknar matchande vikt faller
        # .viktat_medel_andel() (func_diagram.R) tillbaka till ovägt medel
        # för just det programmet.
        i_omr <- if (input$geo_niva == "kommun") d$kommkod %in% gv
                 else samverkan_i(d$samverkansomrade, gv)
        d |> dplyr::filter(i_omr, geo_niva == "kommun", organisationstyp == org_filter) |>
          dplyr::mutate(program = trimws(program)) |>
          dplyr::left_join(
            hamta_elevantal_vikt(),
            by = c("kommkod", "program", "ar", "organisationstyp")
          )
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

    # ---- Nedborrning i Antagning ---------------------------------------------
    # Klick på ett program visar dess inriktningar (ur anskod, se
    # tolka_anskod()); för Introduktionsprogram varianterna och ett klick till
    # målprogrammen. ant_drill = vägen ned (program, ev. IM-variant),
    # ant_fokus = vald stapel på aktuell nivå (för trenden). program_vald
    # behåller programmet (programtypsdiagrammet).
    ant_drill <- reactiveVal(character(0))
    ant_fokus <- reactiveVal(NULL)
    ar_antagning <- reactive(identical(input$omrade, "antagning") && valt_vy() == "dashboard" &&
                               "inriktning" %in% names(aktuell_data()))
    .ant_kol <- c("program", "inriktning", "mal")
    ant_ar_im <- reactive({
      p <- ant_drill()
      length(p) > 0 && any(aktuell_data()$pr_kod[aktuell_data()$program == p[1]] %in% "IM")
    })
    # Rader på aktuell nivå, med "program" = det som staplarna visar.
    niva_data <- function(d) {
      p <- ant_drill()
      if (length(p) == 0 || !ar_antagning()) return(d)
      d <- dplyr::filter(d, program == p[1])
      if (length(p) == 2) d <- dplyr::filter(d, inriktning == p[2])
      d$program <- d[[.ant_kol[length(p) + 1]]]
      d
    }
    # Går det att borra vidare i stapeln? Inriktningar: minst två olika
    # (annars bara "ej vald"). IM-varianter: minst ett målprogram.
    kan_borra <- function(sel) {
      p <- ant_drill()
      if (length(p) >= 2) return(FALSE)
      d <- data_ar()
      d <- if (length(p) == 0) dplyr::filter(d, program == sel)
           else dplyr::filter(d, program == p[1], inriktning == sel)
      kol  <- .ant_kol[length(p) + 2]
      vard <- unique(stats::na.omit(d[[kol]]))
      if (kol == "mal") length(vard) >= 1 else length(vard) > 1
    }

    observeEvent(input$d_bar_selected, {
      sel <- input$d_bar_selected
      if (isolate(ar_antagning())) {
        p <- isolate(ant_drill())
        if (length(sel) != 1) {
          if (length(p) > 0) ant_fokus(NULL) else program_vald(NULL)
          return()
        }
        if (length(p) == 0) program_vald(sel)
        if (isolate(kan_borra(sel))) {
          ant_drill(c(p, sel)); ant_fokus(NULL)
        } else if (length(p) > 0) {
          ant_fokus(sel)
        }
        return()
      }
      program_vald(if (length(sel) >= 1) sel else NULL)
    }, ignoreNULL = FALSE)
    observeEvent(input$ant_tillbaka_alla, {
      ant_drill(character(0)); ant_fokus(NULL); program_vald(NULL)
    })
    observeEvent(input$ant_tillbaka_upp, { ant_drill(ant_drill()[1]); ant_fokus(NULL) })

    # Vad staplarna visar på aktuell nivå (till rubriken).
    ant_dim_text <- function() {
      p <- ant_drill()
      if (length(p) == 0 || !ar_antagning()) return(" efter program")
      if (length(p) == 1) paste0(if (ant_ar_im()) " efter variant – " else " efter inriktning – ", p[1])
      else paste0(" efter målprogram – ", p[2])
    }

    output$ant_nav <- renderUI({
      p <- ant_drill()
      if (length(p) == 0 || !ar_antagning()) return(NULL)
      tagList(
        actionLink(ns("ant_tillbaka_alla"), "\u2190 Alla program", class = "rd-tillbaka"),
        if (length(p) == 2)
          actionLink(ns("ant_tillbaka_upp"), paste0("\u2190 ", p[1]), class = "rd-tillbaka"))
    })

    output$bar_hint <- renderUI({
      txt <- if (!ar_antagning()) {
        "Klicka på en stapel i diagrammet för att se statistik för ett specifikt program."
      } else {
        p <- ant_drill()
        if (length(p) == 0) "Klicka på ett program för att se dess inriktningar (där de finns) och utvecklingen över tid."
        else if (length(p) == 1 && ant_ar_im()) "Klicka på en variant för att se vilka program den riktar sig mot."
        else if (length(p) == 1) "Klicka på en inriktning för att se utvecklingen över tid."
        else "Klicka på ett program för att se utvecklingen över tid."
      }
      notis <- if (ar_antagning() && length(ant_drill()) > 0) {
        if (ant_ar_im()) paste(
          "Individuellt alternativ och språkintroduktion söks oftast inte via gymnasieantagningen",
          "och kan inte skiljas ut här – de ingår i \"Variant ej angiven\".",
          "Under Elever finns antal elever på introduktionsprogrammets alla varianter.")
        else paste(
          "Inriktning syns bara när den väljs vid antagningen. På många program väljer eleverna",
          "inriktning först under utbildningen – de räknas till \"Inriktning väljs senare / ej vald\".",
          "Lärling, riksrekrytering och spetsutbildning anges inom parentes.")
      }
      tagList(tags$p(class = "rd-hint rd-hint--bar", txt),
              if (!is.null(notis)) tags$p(class = "rd-hint", notis))
    })
    # Klickval och filter ligger kvar så länge man är kvar i samma
    # statistikområde (Antagning/Elever/Resultat/Etablering). De nollställs
    # BARA när man byter statistikområde – inte vid indikatorbyte och inte
    # vid ändring av examensperiod, geografi, driftsform osv.
    observeEvent(input$omrade, {
      program_vald(NULL)
      ant_drill(character(0)); ant_fokus(NULL)
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
    # med när man filtrerat (inte "Alla"). med_ar lägger till valt år – utan
    # punkt-avgränsare, så det blir "Dalarna år 2025" i stället för
    # "Dalarna · år 2025".
    filter_underrubrik <- function(med_ar = FALSE) {
      bitar <- geo_label()
      org <- input$organisationstyp
      if (!is.null(org) && org != "_alla_") bitar <- c(bitar, org)
      txt <- paste(bitar, collapse = " · ")
      if (med_ar) txt <- paste0(txt, " år ", req(input$ar))
      txt
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
          column(7,
                 ggiraph::girafeOutput(ns("d_bar"), height = "560px"),
                 hint),
          column(5,
                 div(class = "rd-subcard",
                     ggiraph::girafeOutput(ns("d_etablering_trend"), height = "380px")))
        )
      } else if (valt_vy() == "dashboard") {
        fluidRow(
          column(7,
                 uiOutput(ns("ant_nav")),
                 ggiraph::girafeOutput(ns("d_bar"), height = "470px"),
                 uiOutput(ns("bar_hint")),
                 uiOutput(ns("kon_kontroll"))),
          column(5,
                 div(class = "rd-subcard",
                     ggiraph::girafeOutput(ns("d_trend"), height = "250px"),
                     uiOutput(ns("intro_kontroll"))),
                 div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_programtyp"), height = "300px")))
        )
      } else if (valt_vy() == "genomstromning") {
        # Stapel per program (vänster) + trendlinje Dalarna vs Riket (höger).
        fluidRow(
          column(7,
                 ggiraph::girafeOutput(ns("d_bar"), height = "470px"),
                 hint),
          column(5,
                 div(class = "rd-subcard",
                     ggiraph::girafeOutput(ns("d_genomstromning_trend"), height = "300px")))
        )
      } else {
        # Årskurs och andel: stapel + en trend (ingen programtypsruta).
        fluidRow(
          column(7,
                 ggiraph::girafeOutput(ns("d_bar"), height = "470px"),
                 hint),
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

    # Introduktionsprogrammen räknas som standard inte med i totalen för
    # "Antal elever" (utvecklingen över tid), så den matchar Skolverkets
    # egen "Nationella program"-summa. Kryssrutan låter användaren slå på
    # dem. Visas bara för totalvyn (inget program valt) av just den
    # indikatorn - för andra indikatorer/vyer är frågan inte relevant.
    output$intro_kontroll <- renderUI({
      ind <- valt_indikator()
      if (valt_vy() != "dashboard" || !identical(ind$metrik, "antal_elever") ||
          !is.null(program_vald())) return(NULL)
      div(
        class = "rd-hint",
        checkboxInput(ns("intro_med"), "Inkludera introduktionsprogrammen", value = FALSE)
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
        # "vikt" finns bara när flera kommuner vägs ihop för ett
        # samverkansområde (se genomstromning_dalarna()) – annars används
        # Skolverkets egen färdiga siffra rakt av, ingen viktning i appen.
        if ("vikt" %in% names(df_gs)) sub_gs <- paste0(sub_gs, " · viktat efter antal elever")
        skapa_diagram_genomstromning_bar(min_antal = 0, df_gs, input$ar,
                                         rubrik = ind$amne,
                                         underrubrik = sub_gs, kalla = ind$kalla)
      } else {
        df <- niva_data(data_ar_prog())
        validate(need(nrow(df) > 0, "Inga data för valt urval."))
        if (valt_vy() == "arskurs") {
          skapa_diagram_arskurs(df, input$ar, rubrik = ind$amne,
                                underrubrik = sub, kalla = ind$kalla)
        } else if (valt_vy() == "andel") {
          # Viktning sker bara i praktiken när flera kommuner slås ihop
          # (samverkansområde) - Hela Dalarna/enskild kommun är numera en
          # färdig rad (se data_bas()/"Alla"-raden), ingen viktning då.
          sub_andel <- if (dplyr::n_distinct(df$kommkod) > 1)
            paste0(sub, " · viktat efter antal elever") else sub
          skapa_diagram_bar_andel(min_antal = 0, df, ind$metrik, ind$vikt, ind$metrik_label, input$ar,
                                  rubrik = paste0(ind$amne, " efter program"),
                                  underrubrik = sub_andel,
                                  kalla = ind$kalla)
        } else if (isTRUE(ind$kon) && kon_lage() == "kon") {
          skapa_diagram_bar_kon(min_antal = 0, df, ind$metrik_kv, ind$metrik_man, ind$metrik_label, input$ar,
                                rubrik = paste0(ind$amne, ant_dim_text()),
                                underrubrik = sub, kalla = ind$kalla)
        } else {
          skapa_diagram_bar(min_antal = 0, df, ind$metrik, ind$metrik_label, input$ar,
                            rubrik = paste0(ind$amne, ant_dim_text()),
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
      # Se motsvarande kommentar i d_bar ovan: "vikt" finns bara vid
      # samverkansområde.
      if ("vikt" %in% names(df_prog)) sub <- paste0(sub, " · viktat efter antal elever")
      skapa_diagram_genomstromning_trend(min_antal = 0, df_prog, df_rik,
                                         rubrik = rub, underrubrik = sub,
                                         kalla = ind$kalla)
    })

    output$d_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      df <- niva_data(data_bas_prog())
      validate(need(nrow(df) > 0, "Inga data."))
      # Nedborrat i Antagning: hela vägen ned i rubriken (program › inriktning).
      drill <- if (ar_antagning()) ant_drill() else character(0)
      prog  <- if (length(drill) > 0) ant_fokus() else program_vald()
      fokus <- c(drill, ant_fokus())
      rub  <- if (length(drill) > 0) paste0(ind$amne, " – ", paste(fokus, collapse = " › "))
      else if (is.null(prog)) paste0(ind$amne, " – utveckling över tid")
      else paste0(ind$amne, " – ", prog)
      rub  <- paste(strwrap(rub, width = 55), collapse = "\n")
      # Underrubrik: bara filtret (driftsform + geografi). Trenddiagrammet
      # visar alla år i tidsserien, så inget enskilt år ska anges här.
      sub  <- filter_underrubrik()

      if (valt_vy() == "arskurs") {
        skapa_diagram_trend_arskurs(df, prog, rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      } else if (valt_vy() == "andel") {
        # Se motsvarande kommentar vid d_bar ovan.
        sub_andel <- if (dplyr::n_distinct(df$kommkod) > 1)
          paste0(sub, " · viktat efter antal elever") else sub
        skapa_diagram_trend_andel(min_antal = 0, df, ind$metrik, ind$vikt, ind$metrik_label, prog,
                                  rubrik = rub, underrubrik = sub_andel,
                                  kalla = ind$kalla)
      } else if (isTRUE(ind$kon) && kon_lage() == "kon") {
        skapa_diagram_trend_kon(min_antal = 0, df, ind$metrik_kv, ind$metrik_man, ind$metrik_label, prog,
                                rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      } else if (valt_vy() == "dashboard" && identical(ind$metrik, "antal_elever") && is.null(prog)) {
        # Totalen (inget program valt): använd Skolverkets egna aggregatrad
        # "Nationella program" i stället för att summera ihop de enskilda
        # programraderna själva - annars bidrar inte de elever som bara
        # syns i aggregatet (t.ex. riksrekryterande utbildningar) till
        # summan. Introduktionsprogrammen är en egen aggregatrad och
        # räknas bara med om kryssrutan under diagrammet är ikryssad,
        # så standardläget matchar Skolverkets "Nationella program"-tal.
        intro_med <- isTRUE(input$intro_med)
        df_total <- dplyr::filter(
          data_bas(),
          prog_niva == "total" | (intro_med & program == "Introduktionsprogrammen"))
        sub_total <- paste0(sub, if (intro_med) " · inkl. introduktionsprogrammen"
                            else " · exkl. introduktionsprogrammen")
        skapa_diagram_trend(min_antal = 0, df_total, ind$metrik, ind$metrik_label, NULL,
                            rubrik = rub, underrubrik = sub_total, kalla = ind$kalla)
      } else {
        skapa_diagram_trend(min_antal = 0, df, ind$metrik, ind$metrik_label, prog,
                            rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      }
    })

    output$d_programtyp <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar), valt_vy() == "dashboard")
      df <- data_bas()
      validate(need(nrow(df) > 0, "Inga data."))
      skapa_diagram_programtyp(df, ind$metrik, ind$metrik_label, program_vald(),
                               rubrik = paste0(ind$amne, " – andel efter programtyp"),
                               underrubrik = filter_underrubrik(),
                               kalla = ind$kalla)
    })

    output$karta_rubrik <- renderUI({
      rubrik <- if (identical(input$geo_niva, "samverkansomrade")) "Samverkansområden" else "Kommuner"
      tags$div(class = "rd-label", rubrik)
    })

    output$karta <- ggiraph::renderGirafe({
      k <- skapa_karta_samverkan(input$geo_niva)
      validate(need(!is.null(k), paste0(
        "Kartan kunde inte läsas (kräver sf och åtkomst till geodata-databasen).",
        if (!is.null(.geo_cache$fel)) paste0(" Fel: ", .geo_cache$fel) else "")))
      k
    })

    # Etableringsdata laddas ner summerad och röjandekontrollerad (se
    # summera_etablering_nedladdning()). Övrig data i fliken är publicerad
    # statistik (gymnasieantagningen, Skolverket) - därför min_antal = 0.
    nedladdning <- function(d) {
      if (identical(input$omrade, "etablering")) return(summera_etablering_nedladdning(d))
      # Genomströmning: prickade värden (NA från Skolverket) förklaras i en egen kolumn.
      if (identical(input$omrade, "resultat") && "andel" %in% names(d))
        d <- dplyr::mutate(d, kommentar = dplyr::if_else(is.na(andel), "uppgift saknas (för få elever)",
                                                         NA_character_))
      d
    }

    output$ladda_ner <- downloadHandler(
      filename = function() paste0(input$omrade, "_", input$indikator, "_", input$ar, ".xlsx"),
      content  = function(file) skriv_gymnasie_excel(nedladdning(data_ar()), file, min_antal = 0)
    )
    output$ladda_ner_alla <- downloadHandler(
      filename = function() paste0(input$omrade, "_hela_datasetet.xlsx"),
      content  = function(file) skriv_gymnasie_excel(nedladdning(aktuell_data()), file, min_antal = 0)
    )
  })
}
