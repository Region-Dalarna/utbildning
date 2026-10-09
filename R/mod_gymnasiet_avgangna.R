# ============================================================
#  mod_gymnasiet_avgangna.R
#  Statistikområdet "Avgångna" i Gymnasiet-fliken: vilka som lämnar
#  gymnasiet och med vilket utfall (examen, studiebevis, betyg).
#
#  Inbäddad i mod_gymnasiet.R: när statistikområdet Avgångna väljs visas den
#  här modulens sidopanel och diagram i stället för Gymnasiets vanliga.
#
#  Standardvy: gymnasieprogrammen i valt område (Hela Dalarna förvalt), klick
#  på ett program visar dess inriktningar. Knappar under diagrammet delar upp
#  på kön, bakgrund (inrikes/utrikes födda) eller nyanländ; huvudman är ett
#  filter i sidopanelen.
#
#  Uppdelning och huvudmansfilter PER PROGRAM kräver granulariteten
#  TypStudievagDetalj (beställd). Saknas den visar uppdelningarna i stället
#  fördelningen för hela området, från respektive enskilda granularitet.
#  Totaler hämtas alltid från sin egen granularitet, aldrig som summa av
#  en annan.
# ============================================================

.KALLA_AVGANGNA <- kalla_rud("Gymnasiet – avgångna")
.AVGANGNA_ANTAL_STAPLAR <- 25

AVGANGNA_EXAMEN_FARGER <- c(
  "Examen"                         = RD_PRIMARY,
  "Studiebevis (minst 2500 poäng)" = rd_farg("rd-blue-light", "#8edded"),
  "Övrig/okänd typ"                = "#b9c3ca"
)

# Färger för grupper utan egen palett (bakgrund, nyanländ, betygstyp).
.avg_palett <- function(nivaer) {
  bas <- c(RD_PRIMARY, rd_farg("rd-blue-light", "#8edded"), rd_farg("rd-blue-deep", "#0074a2"),
           rd_farg("rd-accent", "#54a1bd"), "#b9c3ca", "#7d8a93", "#c9d6dd", "#3c5a6b", "#e2a855")
  nivaer <- sort(unique(stats::na.omit(nivaer)))
  stats::setNames(rep_len(bas, length(nivaer)), nivaer)
}

avgangna_struktur <- list(
  avgangna = list(label = "Avgångna", klar = TRUE, vy = "examen",
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
  betygstyp = list(label = "Betygstyp", klar = TRUE, vy = "betygstyp",
                   amne = "Avgångna efter typ av betyg", metrik = "avgangna",
                   metrik_label = "Antal avgångna",
                   beskrivning = "Vilken typ av betyg eller betygsdokument de avgångna har (SCB:s Avgb_Typ)."),
  # Beh: 1 = behörig, 0 = ej behörig, tomt = IB, Waldorf eller samlat
  # betygsdokument (räknas inte med). Finns bara som totaler (TypBeh).
  behorighet = list(label = "Behörighet", klar = TRUE, vy = "behorighet",
                    amne = "Andel med grundläggande behörighet till högskolan",
                    metrik = "andel_beh", vikt = "beh_underlag",
                    metrik_label = "Andel behöriga (%)",
                    beskrivning = paste(
                      "Andel av de avgångna vars utbildning ger grundläggande behörighet till",
                      "högskolestudier. Elever som läser IB eller Waldorf, eller som tagit ut",
                      "samlat betygsdokument, saknar uppgift och räknas inte med."))
)

# Andel behöriga per (ar, område) ur TypBeh-rader.
.avg_beh_summera <- function(d) {
  d |>
    dplyr::group_by(ar, kommkod, kommun, geo_niva) |>
    dplyr::summarise(behoriga     = sum(avgangna[beh %in% 1], na.rm = TRUE),
                     beh_underlag = sum(avgangna[beh %in% c(0, 1)], na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(andel_beh = dplyr::if_else(beh_underlag > 0, 100 * behoriga / beh_underlag, NA_real_))
}

# ---- UI --------------------------------------------------------------------
mod_gymnasiet_avgangna_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3, class = "rd-sidebar",
      uiOutput(ns("indikator_ui")),
      shinyWidgets::radioGroupButtons(
        inputId = ns("huvudman"), label = "Huvudman",
        choices = c("Alla" = "_alla_", "Kommun" = "Kommun", "Enskild" = "Enskild", "Region" = "Region"),
        selected = "_alla_"
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
    vy <- reactive(valt_indikator()$vy)

    # Uppdelning (knapparna under diagrammet): "total" eller en av AVGANGNA_GRUPPER.
    lage <- reactive({
      l <- input$uppdelning
      if (is.null(l) || !l %in% names(AVGANGNA_GRUPPER) || vy() %in% c("betygstyp", "behorighet")) "total" else l
    })
    grupp <- reactive(if (lage() == "total") NULL else AVGANGNA_GRUPPER[[lage()]])
    huvudman <- reactive(if (is.null(input$huvudman)) "_alla_" else input$huvudman)

    omr_kod <- reactive({
      gv <- input$geo_val
      if (is.null(gv) || gv == "_alla_") "20" else gv
    })
    omr_namn <- reactive({
      if (omr_kod() == "20") "Dalarna" else dalarna_kommuner$kommun[match(omr_kod(), dalarna_kommuner$kommkod)]
    })

    # Valt perspektiv, alla granulariteter.
    persp_data <- reactive({
      persp <- if (is.null(input$perspektiv)) "skolkommun" else input$perspektiv
      dplyr::filter(hamta_gymnasiet_avgangna(), geografi == persp)
    })
    har_detalj <- reactive(AVGANGNA_DETALJ %in% persp_data()$granularitet)
    gran <- function(g) dplyr::filter(persp_data(), granularitet == g)

    # Kan programdiagrammet visas med valda filter? Utan detaljgranulariteten
    # bara utan uppdelning och utan huvudmansfilter (och inte för betygstyp).
    prog_vy <- reactive({
      vy() != "behorighet" &&
        (har_detalj() || (lage() == "total" && huvudman() == "_alla_" && vy() != "betygstyp"))
    })

    output$ar_ui <- renderUI({
      ar <- sort(unique(hamta_gymnasiet_avgangna()$ar), decreasing = TRUE)
      tidigare <- isolate(input$ar)
      selectInput(ns("ar"), "Avgångsår", choices = ar,
                  selected = if (!is.null(tidigare) && tidigare %in% ar) tidigare else max(ar))
    })

    # ---- Program och nedborrning ---------------------------------------------
    program_drill <- reactiveVal(NULL)
    program_vald  <- reactiveVal(NULL)
    observeEvent(input$d_bar_selected, {
      sel <- input$d_bar_selected
      if (length(sel) != 1 || !prog_vy()) { program_vald(NULL); return() }
      if (is.null(program_drill())) { program_drill(sel); program_vald(NULL); return() }
      program_vald(sel)
    }, ignoreNULL = FALSE)
    observeEvent(input$tillbaka, { program_drill(NULL); program_vald(NULL) })
    observeEvent(list(input$perspektiv, input$geo_val, input$huvudman), {
      program_drill(NULL); program_vald(NULL)
    }, ignoreInit = TRUE)

    prog_kol <- reactive(if (is.null(program_drill())) "program_namn" else "inriktning_kort")

    # Rader för programdiagrammet: detaljgranulariteten (med huvudmansfilter)
    # om den finns, annars TypStudievag. Vid nedborrning bara valt program.
    prog_rader <- reactive({
      d <- if (har_detalj()) {
        dd <- gran(AVGANGNA_DETALJ)
        if (huvudman() != "_alla_") dd <- dplyr::filter(dd, .data$huvudman == .env$huvudman())
        dd
      } else gran("TypStudievag")
      if (!is.null(program_drill())) d <- dplyr::filter(d, program_namn == program_drill())
      d
    })

    # Rader för områdesvyn (utan detaljdata): den granularitet som motsvarar valet.
    omrade_rader <- reactive({
      if (vy() == "betygstyp") return(gran("TypBetygstyp"))
      if (!is.null(grupp())) return(gran(grupp()$granularitet))
      gran("TypHuvudman")
    })
    omrade_kol <- reactive({
      if (vy() == "betygstyp") NULL
      else if (!is.null(grupp())) grupp()$kol
      else "huvudman"
    })

    grupp_farger <- reactive({
      if (is.null(grupp())) return(NULL)
      if (lage() == "kon") KON_FARGER
      else .avg_palett(persp_data()[[grupp()$kol]])
    })

    # Valt år, valt område, de största programmen.
    prog_ar <- reactive({
      d <- dplyr::filter(prog_rader(), ar == as.integer(req(input$ar)), kommkod == omr_kod())
      storst <- d |>
        dplyr::group_by(p = .data[[prog_kol()]]) |>
        dplyr::summarise(n = sum(avgangna, na.rm = TRUE), .groups = "drop") |>
        dplyr::slice_max(n, n = .AVGANGNA_ANTAL_STAPLAR, with_ties = FALSE) |>
        dplyr::pull(p)
      dplyr::filter(d, .data[[prog_kol()]] %in% storst)
    })

    serie_namn <- function(k) dplyr::case_when(k == "00" ~ "Riket", k == "20" ~ "Dalarna", TRUE ~ omr_namn())
    jmf_koder  <- reactive(unique(c(omr_kod(), "20", "00")))

    underrubrik <- function(med_ar = FALSE) {
      persp <- if (identical(input$perspektiv, "bokommun")) "bokommun" else "skolkommun"
      txt <- paste0(omr_namn(), " (", persp, ")")
      if (huvudman() != "_alla_") txt <- paste0(txt, " · huvudman: ", tolower(huvudman()))
      if (med_ar) txt <- paste0(txt, " · avgångsår ", input$ar)
      txt
    }

    output$brodsmula <- renderText({
      paste0("Gymnasiet › Avgångna › ", valt_indikator()$label)
    })

    output$vy <- renderUI({
      ind <- valt_indikator()
      if (!isTRUE(ind$klar)) return(div(class = "rd-info", ind$beskrivning))
      drill <- program_drill()
      hint <- if (prog_vy())
        tags$p(class = "rd-hint rd-hint--bar",
               if (is.null(drill)) "Klicka på ett program för att se dess inriktningar."
               else "Klicka på en inriktning för att se utvecklingen över tid.")
      tillbaka <- if (!is.null(drill) && prog_vy())
        actionLink(ns("tillbaka"), "← Alla program", class = "rd-tillbaka")
      knappar <- if (!vy() %in% c("betygstyp", "behorighet"))
        div(class = "rd-kon-kontroll",
            shinyWidgets::radioGroupButtons(
              inputId = ns("uppdelning"), label = NULL,
              choices = c("Totalt" = "total", "Kön" = "kon", "Bakgrund" = "bakgrund",
                          "Nyanländ" = "nyanland"),
              selected = isolate(if (is.null(input$uppdelning)) "total" else input$uppdelning),
              size = "sm"))
      fluidRow(
        column(7, tillbaka, ggiraph::girafeOutput(ns("d_bar"), height = "560px"), knappar, hint),
        column(5, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_trend"), height = "380px")))
      )
    })

    # ---- Stapeldiagram ----------------------------------------------------------
    output$d_bar <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      sub <- underrubrik(TRUE)
      # Behörighet: valt område jämfört med Dalarna och riket (bara totaler).
      if (vy() == "behorighet") {
        d <- gran("TypBeh") |>
          dplyr::filter(ar == as.integer(req(input$ar)), kommkod %in% jmf_koder()) |>
          .avg_beh_summera() |>
          dplyr::mutate(program = serie_namn(kommkod))
        validate(need(nrow(d) > 0, "Inga data för valt urval."))
        return(skapa_diagram_bar_andel(d, ind$metrik, ind$vikt, ind$metrik_label, input$ar,
                                       rubrik = ind$amne, underrubrik = sub, kalla = .KALLA_AVGANGNA))
      }
      if (prog_vy()) {
        d <- prog_ar()
        validate(need(nrow(d) > 0, "Inga data för valt urval."))
        pk  <- prog_kol()
        rub <- paste0(ind$amne, if (is.null(program_drill())) " efter program"
                      else paste0(" efter inriktning – ", program_drill()))
        if (vy() == "betygstyp") {
          fd <- dplyr::mutate(d, program = .data[[pk]])
          return(skapa_diagram_bar_fordelning(fd, "avgb_typ_namn", "avgangna",
                                              .avg_palett(fd$avgb_typ_namn), ind$metrik_label, input$ar,
                                              rubrik = rub, underrubrik = sub, kalla = .KALLA_AVGANGNA))
        }
        if (vy() == "examen") {
          kat <- if (is.null(grupp())) "examen_grupp" else grupp()$kol
          far <- if (is.null(grupp())) AVGANGNA_EXAMEN_FARGER else grupp_farger()
          return(skapa_diagram_bar_fordelning(dplyr::mutate(d, program = .data[[pk]]), kat, "avgangna",
                                              far, ind$metrik_label, input$ar,
                                              rubrik = rub, underrubrik = sub, kalla = .KALLA_AVGANGNA))
        }
        enh <- if (is.null(ind$enhet)) " %" else ind$enhet
        if (is.null(grupp())) {
          return(skapa_diagram_bar_andel(avgangna_summera(d, pk), ind$metrik, ind$vikt, ind$metrik_label,
                                         input$ar, rubrik = rub, underrubrik = sub,
                                         kalla = .KALLA_AVGANGNA, enhet = enh))
        }
        # Program x grupp: grupperade staplar (kommun-diagrammets funktion,
        # med programmen som "område").
        s <- avgangna_summera(d, pk, grupp()$kol) |>
          dplyr::mutate(geo_niva = "kommun", kommun = program, kommkod = program, program = delgrupp)
        return(skapa_diagram_andel_omrade_kon(s, ind$metrik, ind$vikt, ind$metrik_label, input$ar,
                                              vald_kommkod = program_vald(), grupp_farger = grupp_farger(),
                                              rubrik = rub, underrubrik = sub, kalla = .KALLA_AVGANGNA,
                                              enhet = enh))
      }

      # Områdesvyn: fördelning för hela området (detaljdata saknas).
      d <- dplyr::filter(omrade_rader(), ar == as.integer(req(input$ar)), kommkod == omr_kod())
      validate(need(nrow(d) > 0, "Inga data för valt urval."))
      kol <- omrade_kol()
      rub <- paste0(ind$amne, if (!is.null(kol)) paste0(" efter ", if (is.null(grupp())) "huvudman" else tolower(grupp()$label)))
      if (vy() == "betygstyp") {
        fd <- dplyr::mutate(d, program = omr_namn())
        return(skapa_diagram_bar_fordelning(fd, "avgb_typ_namn", "avgangna", .avg_palett(fd$avgb_typ_namn),
                                            ind$metrik_label, input$ar, rubrik = rub, underrubrik = sub,
                                            kalla = .KALLA_AVGANGNA))
      }
      if (vy() == "examen") {
        return(skapa_diagram_bar_fordelning(dplyr::mutate(d, program = .data[[kol]]), "examen_grupp",
                                            "avgangna", AVGANGNA_EXAMEN_FARGER, ind$metrik_label, input$ar,
                                            rubrik = rub, underrubrik = sub, kalla = .KALLA_AVGANGNA))
      }
      skapa_diagram_bar_andel(avgangna_summera(d, kol), ind$metrik, ind$vikt, ind$metrik_label, input$ar,
                              rubrik = rub, underrubrik = sub, kalla = .KALLA_AVGANGNA,
                              enhet = if (is.null(ind$enhet)) " %" else ind$enhet)
    })

    # ---- Trend --------------------------------------------------------------------
    # Rader för trenden i valt område och jämförelseområdena: valt
    # program/inriktning om ett är i fokus, annars totalen (med huvudman).
    trend_rader <- function(koder, med_grupp = FALSE) {
      fokus_inr  <- program_vald()
      fokus_prog <- program_drill()
      if (prog_vy() && (!is.null(fokus_prog) || har_detalj())) {
        d <- if (har_detalj()) {
          dd <- gran(AVGANGNA_DETALJ)
          if (huvudman() != "_alla_") dd <- dplyr::filter(dd, .data$huvudman == .env$huvudman())
          dd
        } else gran("TypStudievag")
        if (!is.null(fokus_prog)) d <- dplyr::filter(d, program_namn == fokus_prog)
        if (!is.null(fokus_inr))  d <- dplyr::filter(d, inriktning_kort == fokus_inr)
      } else if (med_grupp && !is.null(grupp())) {
        d <- gran(grupp()$granularitet)
      } else if (huvudman() != "_alla_") {
        d <- dplyr::filter(gran("TypHuvudman"), .data$huvudman == .env$huvudman())
      } else {
        d <- gran(if (vy() == "betygstyp") "TypBetygstyp" else "Typ")
      }
      dplyr::filter(d, kommkod %in% koder)
    }

    output$d_trend <- ggiraph::renderGirafe({
      ind <- valt_indikator(); req(isTRUE(ind$klar))
      fokus <- if (!is.null(program_vald())) program_vald() else program_drill()
      rub <- paste0(ind$amne, " – ", if (is.null(fokus)) "utveckling över tid" else fokus)
      sub <- underrubrik()

      if (vy() == "behorighet") {
        d <- gran("TypBeh") |> dplyr::filter(kommkod %in% jmf_koder()) |> .avg_beh_summera() |>
          dplyr::mutate(serie = factor(serie_namn(kommkod), levels = unique(serie_namn(jmf_koder()))),
                        varde = andel_beh, underlag = beh_underlag)
        validate(need(nrow(d) > 0, "Inga data."))
        return(skapa_diagram_trend_jmf(d, ind$metrik_label, rubrik = rub,
                                       underrubrik = paste0(sub, " jämfört med Dalarna och riket"),
                                       kalla = .KALLA_AVGANGNA))
      }

      # Betygstyp: andel per betygstyp över tid i valt område.
      if (vy() == "betygstyp") {
        d <- trend_rader(omr_kod()) |>
          dplyr::group_by(ar, program = avgb_typ_namn) |>
          dplyr::summarise(n = sum(avgangna, na.rm = TRUE), .groups = "drop") |>
          dplyr::group_by(ar) |> dplyr::mutate(tot = sum(n), andel = 100 * n / tot) |> dplyr::ungroup()
        validate(need(nrow(d) > 0, "Inga data."))
        return(skapa_diagram_trend_andel_kon(d, "andel", "n", "Andel av avgångna (%)",
                                             rubrik = rub, underrubrik = sub, kalla = .KALLA_AVGANGNA,
                                             grupp_farger = .avg_palett(d$program)))
      }

      # Uppdelning: en linje per grupp i valt område.
      if (!is.null(grupp())) {
        d <- avgangna_summera(trend_rader(omr_kod(), med_grupp = TRUE), NULL, grupp()$kol) |>
          dplyr::mutate(program = delgrupp)
        validate(need(nrow(d) > 0, "Inga data."))
        if (vy() == "examen")
          return(skapa_diagram_trend_andel_kon(d, "avgangna", "avgangna", ind$metrik_label,
                                               rubrik = rub, underrubrik = sub, kalla = .KALLA_AVGANGNA,
                                               enhet = "", summa = TRUE, grupp_farger = grupp_farger()))
        return(skapa_diagram_trend_andel_kon(d, ind$metrik, ind$vikt, ind$metrik_label,
                                             rubrik = rub, underrubrik = sub, kalla = .KALLA_AVGANGNA,
                                             enhet = if (is.null(ind$enhet)) " %" else ind$enhet,
                                             grupp_farger = grupp_farger()))
      }

      # Totalt: antal i valt område, eller andel jämfört med Dalarna och riket.
      koder <- unique(c(omr_kod(), "20", "00"))
      d <- avgangna_summera(trend_rader(koder))
      validate(need(nrow(d) > 0, "Inga data."))
      if (vy() == "examen")
        return(skapa_diagram_trend(dplyr::filter(d, kommkod == omr_kod()), "avgangna", ind$metrik_label,
                                   rubrik = rub, underrubrik = sub, kalla = .KALLA_AVGANGNA))
      d <- dplyr::mutate(d, serie = factor(serie_namn(kommkod), levels = unique(serie_namn(koder))),
                         varde = .data[[ind$metrik]], underlag = .data[[ind$vikt]])
      skapa_diagram_trend_jmf(d, ind$metrik_label, rubrik = rub,
                              underrubrik = paste0(sub, " jämfört med Dalarna och riket"),
                              kalla = .KALLA_AVGANGNA, enhet = if (is.null(ind$enhet)) " %" else ind$enhet)
    })

    # Kända begränsningar, beroende på val.
    output$notiser <- renderUI({
      n <- "Examen omfattar högskoleförberedande examen, yrkesexamen och International Baccalaureate (IB)."
      if (vy() == "behorighet") {
        n <- c(n, paste("Behörighet finns inte per program, huvudman eller grupp i data -",
                        "diagrammen visar valt område jämfört med Dalarna och riket.",
                        "Elever som läser IB eller Waldorf, eller som tagit ut samlat",
                        "betygsdokument, saknar uppgift och räknas inte med."))
      } else if (!prog_vy()) {
        n <- c(n, paste("Uppdelning per program med valda inställningar finns inte i data än.",
                        "Diagrammet visar därför fördelningen för hela området."))
      }
      if (identical(lage(), "nyanland"))
        n <- c(n, paste("Nyanländ = högst 4 år i Sverige, räknat från senaste invandringsår.",
                        "Även svenskfödda som återinvandrat kan ingå."))
      if (prog_vy())
        n <- c(n, paste("Inriktning kommer från studievägskoden. Varianter som idrottsutbildning,",
                        "lärling och riksrekryterande ingår i sitt basprogram. Vissa koder saknar",
                        "programnamn och visas som \"Okänt program\". De",
                        .AVGANGNA_ANTAL_STAPLAR, "största programmen visas."))
      if (identical(input$perspektiv, "bokommun"))
        n <- c(n, "För senaste avgångsåret används bostadskommun och invandringsår från föregående år.")
      tags$ul(class = "rd-hint", lapply(n, tags$li))
    })

    # ---- Nedladdning --------------------------------------------------------------
    urval_data <- function() {
      if (vy() == "behorighet")
        return(gran("TypBeh") |> dplyr::filter(ar == as.integer(req(input$ar)), kommkod %in% jmf_koder()) |>
                 .avg_beh_summera())
      if (prog_vy()) {
        d <- prog_ar()
        return(avgangna_summera(d, prog_kol(), if (!is.null(grupp())) grupp()$kol))
      }
      d <- dplyr::filter(omrade_rader(), ar == as.integer(req(input$ar)), kommkod == omr_kod())
      avgangna_summera(d, omrade_kol())
    }
    output$ladda_ner <- downloadHandler(
      filename = function() paste0("gymnasiet_avgangna_", input$indikator, "_", input$ar, ".xlsx"),
      content  = function(file) skriv_gymnasie_excel(urval_data(), file, blad = "Avgångna")
    )
    output$ladda_ner_alla <- downloadHandler(
      filename = function() "gymnasiet_avgangna_program.xlsx",
      content  = function(file) skriv_gymnasie_excel(
        avgangna_summera(prog_rader(), "program_namn", "inriktning_kort"), file, blad = "Avgångna")
    )
  })
}
