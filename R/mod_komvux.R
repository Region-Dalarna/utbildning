# ============================================================
#  mod_komvux.R
#  Shiny-modul för skolformen Komvux (inkl. SFI).
#
#  Staplarna visar nivåerna ("program" = nivåtext, se func_data_komvux.R).
#  Klick på en nivå visar dess kurser (granularitet Kurs, de största, övriga
#  ihop), klick på en kurs visar den över tid. Nivåns egna siffror kommer
#  alltid från nivåraderna. Elever kan inte delas på kurs (samma elev kan
#  läsa en kurs flera terminer) - där visar klick bara nivån över tid.
#  Elever per kommun (vy "kommun_kon") visas som i Grundskola: en stapel
#  per kommun, könsuppdelat eller totalt, och klick väljer kommunen.
#
#  Begrepp i appen: "lästa kurser" = kursdeltaganden (en elev räknas en gång
#  per kurs), "elever" = deltagare (unika individer).
#  Utbildningstyp (Komvux/SFI) är ett eget filter, inte en diagramdimension.
# ============================================================

.KALLA_KOMVUX <- kalla_rud("Komvux och SFI – kursdeltagare")
.KOMVUX_ANTAL_KURSER <- 15

komvux_struktur <- list(
  kursdeltagande = list(
    label = "Nivåer och kurser",
    indikatorer = list(
      # Elever per nivå (ArNivaKon). Nivåerna summeras aldrig - totalen
      # (trend utan vald nivå, underrubrik) läses från ArKon.
      elever = list(label = "Elever", klar = TRUE, vy = "dashboard", kon = FALSE, unika = TRUE,
                    amne = "Antal elever", metrik = "deltagare", metrik_label = "Antal elever",
                    kalla = .KALLA_KOMVUX,
                    beskrivning = paste(
                      "Antal elever (unika personer) som läst minst en kurs under året.",
                      "En elev som läser på flera nivåer räknas på varje nivå, och med Alla",
                      "utbildningstyper räknas den som läser både Komvux och SFI i båda.")),
      kursdeltaganden = list(label = "Lästa kurser", klar = TRUE, vy = "dashboard", kon = FALSE,
                             amne = "Antal lästa kurser",
                             metrik = "kursdeltaganden", metrik_label = "Antal lästa kurser",
                             kalla = .KALLA_KOMVUX,
                             beskrivning = paste(
                               "Antal kurser som elever har läst under året, avslutade, avbrutna eller",
                               "pågående. En elev räknas en gång per kurs. Klicka på en nivå för att",
                               "se kurserna.")),
      # Unika elever från granulariteten ArKon (se func_data_komvux.R).
      deltagare = list(label = "Elever per kommun", klar = TRUE, vy = "kommun_kon", kon = FALSE,
                       amne = "Antal komvux-/SFI-elever",
                       metrik = "deltagare", metrik_label = "Antal elever",
                       kalla = .KALLA_KOMVUX,
                       beskrivning = paste(
                         "Antal elever (unika personer) per kommun som läst minst en kurs under året.",
                         "Med Alla utbildningstyper räknas den som läser både Komvux och SFI två gånger.")),
      andel_godkant = list(label = "Andel godkända", klar = TRUE, vy = "andel", kon = FALSE,
                          amne = "Andel godkända kurser", metrik = "andel_godkant", vikt = "avslutad_kurs",
                          metrik_label = "Andel godkända (%)", kalla = .KALLA_KOMVUX,
                          beskrivning = paste(
                            "Andel godkända bland de lästa kurser som fått ett betyg",
                            "(avslutade kurser - pågående och avbrutna räknas inte in).")),
      andel_avbrott = list(label = "Andel avbrott", klar = TRUE, vy = "andel", kon = FALSE,
                           amne = "Andel avbrott", metrik = "andel_avbrott", vikt = "kursdeltaganden",
                           metrik_label = "Andel avbrott (%)", kalla = .KALLA_KOMVUX,
                           beskrivning = "Andel av de lästa kurserna som avbrutits.")
    )
  )
)

# ---- UI --------------------------------------------------------------------
mod_komvux_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "rd-segmented",
      shinyWidgets::radioGroupButtons(
        inputId  = ns("omrade"), label = NULL,
        choices  = .choices_fran_lista(komvux_struktur),
        selected = names(komvux_struktur)[1]
      )
    ),

    sidebarLayout(
      sidebarPanel(
        width = 3, class = "rd-sidebar",

        uiOutput(ns("indikator_ui")),
        tags$hr(),

        shinyWidgets::radioGroupButtons(
          inputId = ns("utbildningstyp"), label = "Utbildningstyp",
          choices = c("Alla" = "_alla_", "Komvux" = "Komvux", "SFI" = "SFI"),
          selected = "_alla_"
        ),
        shinyWidgets::pickerInput(
          inputId = ns("geo_val"), label = "Område (skolans kommun)",
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
mod_komvux_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$indikator_ui <- renderUI({
      omr <- komvux_struktur[[ req(input$omrade) ]]
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

    # ---- Nedborrning: nivå -> kurser ------------------------------------------
    niva_drill <- reactiveVal(NULL)
    # Kurserna kan visas för allt utom elever (unika personer).
    kan_borra <- reactive(!isTRUE(valt_indikator()$unika) && valt_vy() != "kommun_kon")
    observeEvent(kan_borra(), { if (!kan_borra()) { niva_drill(NULL); program_vald(NULL) } })
    observeEvent(input$tillbaka, { niva_drill(NULL); program_vald(NULL) })

    aktuell_data <- reactive({
      if (is.null(niva_drill())) hamta_komvux_data()
      else dplyr::filter(hamta_komvux_kurser(), niva_namn == niva_drill())
    })

    # Vad staplarna visar.
    dim_label <- reactive({ if (is.null(niva_drill())) "nivå" else "kurs" })

    valt_indikator <- reactive({
      req(input$omrade)
      ind_list <- komvux_struktur[[input$omrade]]$indikatorer
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
      ut <- input$utbildningstyp
      if (!is.null(ut) && ut != "_alla_") bitar <- c(bitar, ut)
      txt <- paste(bitar, collapse = " \u00b7 ")
      if (med_ar) txt <- paste0(txt, " \u00b7 år ", req(input$ar))
      txt
    }

    filtrera_utbildningstyp <- function(d, ut) {
      if (!is.null(ut) && ut != "_alla_") dplyr::filter(d, utbildningstyp == ut) else d
    }

    program_vald <- reactiveVal(NULL)
    observeEvent(input$d_bar_selected, {
      sel <- input$d_bar_selected
      if (identical(isolate(valt_vy()), "kommun_kon")) {
        # Deltagare: klick på en kommunstapel väljer kommunen i filtret.
        if (length(sel) == 1)
          shinyWidgets::updatePickerInput(session, "geo_val",
                                          selected = if (sel == "20") "_alla_" else sel)
        return()
      }
      if (is.null(isolate(niva_drill())) && isolate(kan_borra()) && length(sel) == 1) {
        niva_drill(sel); program_vald(NULL)
        return()
      }
      program_vald(if (length(sel) >= 1) sel else NULL)
    }, ignoreNULL = FALSE)

    # Könsuppdelat (förvalt) eller totalt - bara för Deltagare.
    kon_uppdelat <- reactive({ !identical(input$kon_lage, "total") })

    # Deltagare per område och kön för vald utbildningstyp.
    deltagare_data <- reactive({ hamta_komvux_deltagare(input$utbildningstyp) })
    deltagare_bas <- reactive({
      d  <- deltagare_data()
      gv <- req(input$geo_val)
      if (gv == "_alla_") dplyr::filter(d, geo_niva == "lan")
      else dplyr::filter(d, geo_niva == "kommun", kommkod == gv)
    })
    observeEvent(input$omrade, { program_vald(NULL); niva_drill(NULL) }, ignoreInit = TRUE)

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
      d  <- filtrera_utbildningstyp(d, input$utbildningstyp)

      # Kurser: behåll de största kurserna (över alla år, så att samma kurser
      # visas oavsett år) och slå ihop resten.
      if (!is.null(niva_drill()) && nrow(d) > 0) {
        storst <- d |>
          dplyr::group_by(program) |>
          dplyr::summarise(n = sum(kursdeltaganden, na.rm = TRUE), .groups = "drop") |>
          dplyr::slice_max(n, n = .KOMVUX_ANTAL_KURSER, with_ties = FALSE) |>
          dplyr::pull(program)
        d <- d |>
          dplyr::mutate(program = dplyr::if_else(program %in% storst, program, "Övriga kurser")) |>
          dplyr::group_by(ar, kommkod, kommun, geo_niva, program) |>
          dplyr::summarise(dplyr::across(dplyr::all_of(.komvux_matt), ~sum(.x, na.rm = TRUE)),
                           .groups = "drop") |>
          .komvux_andelar()
      }
      d
    })
    data_ar <- reactive({
      req(input$ar)
      dplyr::filter(data_bas(), ar == as.integer(input$ar))
    })

    output$brodsmula <- renderText({
      omr <- komvux_struktur[[req(input$omrade)]]$label
      paste0("Komvux \u203a ", omr, " \u203a ", valt_indikator()$label)
    })

    output$vy <- renderUI({
      ind <- valt_indikator()
      if (!isTRUE(ind$klar)) return(div(class = "rd-info", "Den här vyn är inte inlagd än."))
      if (valt_vy() == "kommun_kon") {
        return(fluidRow(
          column(7, ggiraph::girafeOutput(ns("d_bar"), height = "560px"),
                 tags$p(class = "rd-hint rd-hint--bar",
                        "Klicka på en stapel för att se utvecklingen över tid för kommunen."),
                 div(class = "rd-kon-kontroll",
                     shinyWidgets::radioGroupButtons(
                       inputId  = ns("kon_lage"), label = NULL,
                       choices  = c("Könsuppdelat" = "kon", "Totalt" = "total"),
                       selected = isolate(if (is.null(input$kon_lage)) "kon" else input$kon_lage),
                       size = "sm"))),
          column(5, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_trend"), height = "380px")))
        ))
      }
      hint <- tags$p(class = "rd-hint rd-hint--bar",
                     if (is.null(niva_drill()) && kan_borra())
                       "Klicka på en nivå för att se kurserna."
                     else paste0("Klicka på en stapel för att se utvecklingen över tid för en ",
                                 dim_label(), "."))
      tillbaka <- if (!is.null(niva_drill()))
        actionLink(ns("tillbaka"), "\u2190 Alla nivåer", class = "rd-tillbaka")
      notis <- if (!is.null(niva_drill()))
        tags$p(class = "rd-hint", paste0("De ", .KOMVUX_ANTAL_KURSER, " mest lästa kurserna visas, ",
                                         "övriga slås ihop. Nivåns egna siffror (trenden utan vald kurs) ",
                                         "räknas på hela nivån."))
      fluidRow(
        column(7, tillbaka, ggiraph::girafeOutput(ns("d_bar"), height = "560px"), hint, notis),
        column(5, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_trend"), height = "380px")))
      )
    })

    output$d_bar <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      if (valt_vy() == "kommun_kon") {
        df <- dplyr::filter(deltagare_data(), ar == as.integer(req(input$ar)),
                            geo_niva %in% c("lan", "kommun"))
        validate(need(nrow(df) > 0, "Inga data för valt urval."))
        ut <- input$utbildningstyp
        sub <- paste0("Skolans kommun \u00b7 ",
                      if (!is.null(ut) && ut != "_alla_") ut else "Komvux och SFI",
                      " \u00b7 år ", input$ar)
        return(skapa_diagram_andel_omrade_kon(
          df, ind$metrik, ind$metrik, ind$metrik_label, input$ar,
          vald_kommkod = input$geo_val, kon_uppdelat = kon_uppdelat(),
          summa = TRUE, enhet = "",
          rubrik = paste0(ind$amne, " efter kommun", if (kon_uppdelat()) " och kön" else ""),
          underrubrik = sub, kalla = ind$kalla))
      }
      df  <- data_ar()
      validate(need(nrow(df) > 0, "Inga data för valt urval."))
      sub <- filter_underrubrik(med_ar = TRUE)
      if (isTRUE(ind$unika)) {
        validate(need(any(!is.na(df$deltagare)), "Elever per nivå finns inte i data."))
        # Totalen från ArKon - nivåerna kan inte summeras (samma elev på flera).
        tot <- sum(dplyr::filter(deltagare_bas(), ar == as.integer(input$ar))$deltagare)
        if (tot > 0) sub <- paste0(sub, " \u00b7 totalt ", scales::number(tot, big.mark = " "), " elever")
      }

      rub <- paste0(ind$amne, " efter ", dim_label(),
                    if (!is.null(niva_drill())) paste0(" \u2013 ", niva_drill()))
      if (valt_vy() == "andel") {
        skapa_diagram_bar_andel(df, ind$metrik, ind$vikt, ind$metrik_label, input$ar,
                                rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      } else {
        skapa_diagram_bar(df, ind$metrik, ind$metrik_label, input$ar,
                          rubrik = rub, underrubrik = sub, kalla = ind$kalla)
      }
    })

    output$d_trend <- ggiraph::renderGirafe({
      ind  <- valt_indikator(); req(isTRUE(ind$klar))
      if (valt_vy() == "kommun_kon") {
        df  <- deltagare_bas()
        validate(need(nrow(df) > 0, "Inga data."))
        rub <- paste0(ind$amne, " \u2013 utveckling över tid")
        sub <- filter_underrubrik()
        if (kon_uppdelat()) {
          return(skapa_diagram_trend_andel_kon(df, ind$metrik, ind$metrik, ind$metrik_label,
                                               rubrik = rub, underrubrik = sub, kalla = ind$kalla,
                                               enhet = "", summa = TRUE))
        }
        return(skapa_diagram_trend(df, ind$metrik, ind$metrik_label, NULL,
                                   rubrik = rub, underrubrik = sub, kalla = ind$kalla))
      }
      df   <- data_bas()
      validate(need(nrow(df) > 0, "Inga data."))
      prog <- program_vald()
      # Elever utan vald nivå: totalen från ArKon, inte summan av nivåerna.
      if (isTRUE(ind$unika) && is.null(prog)) df <- deltagare_bas()
      # Nedborrat utan vald kurs: nivåns egna rader (inte summan av kurserna).
      if (!is.null(niva_drill()) && is.null(prog)) {
        df <- hamta_komvux_data() |>
          dplyr::filter(if (input$geo_val == "_alla_") geo_niva == "lan" else geo_niva == "kommun" & kommkod == input$geo_val) |>
          filtrera_utbildningstyp(input$utbildningstyp)
        prog <- niva_drill()
      }
      fokus <- unique(c(niva_drill(), prog))
      rub  <- if (length(fokus) == 0) paste0(ind$amne, " \u2013 utveckling över tid")
              else paste0(ind$amne, " \u2013 ", paste(fokus, collapse = " \u203a "))
      rub  <- paste(strwrap(rub, width = 55), collapse = "\n")
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
      filename = function() paste0("komvux_", input$omrade, "_", input$indikator, "_", input$ar, ".xlsx"),
      content  = function(file) {
        d <- if (valt_vy() == "kommun_kon")
          dplyr::filter(deltagare_bas(), ar == as.integer(req(input$ar))) else data_ar()
        skriv_gymnasie_excel(d, file, blad = "Komvux")
      }
    )
    output$ladda_ner_alla <- downloadHandler(
      filename = function() "komvux_hela_datasetet.xlsx",
      content  = function(file) skriv_gymnasie_excel(aktuell_data(), file, blad = "Komvux")
    )
  })
}
