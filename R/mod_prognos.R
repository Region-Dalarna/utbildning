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
#  Vägar in i yrket: prognostiserad examination per utbildningsform.
#  Gymnasiet och Eftergymnasialt: per program resp. utbildningsgrupp, senaste
#  historiska år jämfört med en prognosperiod (årligt genomsnitt), och
#  utvecklingen över tid med prognosperioderna streckade.
#
#  Datan finns bara för län och riket. Publicerad statistik - ingen
#  röjandekontroll. Se func_data_regtop.R om enheter (summa för perioden
#  i länen, årligt genomsnitt i riket).
# ============================================================

prognos_struktur <- list(
  bedomning       = list(label = "Bedömning 2040"),
  vagar           = list(label = "Vägar in i yrket"),
  gymnasiet       = list(label = "Gymnasiet"),
  eftergymnasialt = list(label = "Eftergymnasialt")
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
        uiOutput(ns("omrade_ui")),
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
    omrade <- reactive(if (is.null(input$omrade)) "bedomning" else input$omrade)
    output$aggregat_ui <- renderUI({
      req(omrade() == "bedomning")
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
      if (omrade() != "bedomning")
        return(paste0("Prognos 2040 \u203a ", prognos_struktur[[omrade()]]$label,
                      if (!is.null(kategori_vald())) paste0(" \u203a ", kategori_vald())))
      paste0("Prognos 2040 \u203a Bedömning 2040",
             if (!is.null(grupp_vald())) paste0(" \u203a ", grupp_namn()))
    })

    output$vy <- renderUI({
      if (omrade() == "vagar") return(vy_vagar())
      if (omrade() %in% c("gymnasiet", "eftergymnasialt")) return(vy_period())
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

    # ---- Vägar in i yrket ---------------------------------------------------------
    # Utbildningsgrupperna med examinationsprognos i valt län. Förvald: den
    # grupp man senast klickat på i Bedömning 2040.
    vagar_grupper <- reactive({
      d <- dplyr::filter(hamta_regtop("examination_utbildningsform"), regionkod == region()) |>
        dplyr::distinct(utbildningsgrupp_kod, utbildningsgrupp) |> dplyr::arrange(utbildningsgrupp)
      stats::setNames(d$utbildningsgrupp_kod, d$utbildningsgrupp)
    })
    vagar_grupp <- reactive({
      g <- input$vagar_grupp
      if (is.null(g) || !g %in% vagar_grupper()) unname(vagar_grupper()[1]) else g
    })
    vagar_data <- function(regionkoder) {
      dplyr::filter(hamta_regtop("examination_utbildningsform"), regionkod %in% regionkoder,
                    utbildningsgrupp_kod == vagar_grupp()) |>
        dplyr::mutate(form = dplyr::coalesce(unname(REGTOP_UTBFORM_TEXT[utbildningsform]), utbildningsform))
    }
    vy_vagar <- function() {
      fluidRow(
        column(7, ggiraph::girafeOutput(ns("d_vagar"), height = "420px"),
               tags$p(class = "rd-hint",
                      "Hur många som väntas ta examen inom utbildningsgruppen per år, efter utbildningsform. ",
                      "Länens summa för perioden har räknats om till årligt genomsnitt.")),
        column(5, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_vagar_andel"), height = "280px")))
      )
    }
    output$d_vagar <- ggiraph::renderGirafe({
      req(omrade() == "vagar")
      d <- regtop_period_summa(vagar_data(region()), grupp = "form")
      validate(need(nrow(d) > 0, "Ingen examinationsprognos för gruppen i valt län."))
      namn <- names(vagar_grupper())[match(vagar_grupp(), vagar_grupper())]
      skapa_diagram_vagar(d, rubrik = paste0("Vägar in i yrket – ", namn),
                          underrubrik = paste0(region_namn(), " · prognos, examinerade per år"))
    })
    output$d_vagar_andel <- ggiraph::renderGirafe({
      req(omrade() == "vagar")
      d <- vagar_data(unique(c(region(), "00"))) |>
        .regtop_perioder() |> regtop_summa_period() |>
        dplyr::group_by(regionkod, form) |>
        dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(serie = dplyr::if_else(regionkod == "00", "Riket", region_namn()))
      skapa_diagram_vagar_andel(d, rubrik = "Andel per utbildningsform",
                                underrubrik = "Hela prognosperioden 2024–2040")
    })

    # ---- Gymnasiet och eftergymnasialt -----------------------------------------------
    # Ett mått i taget; staplar per kategori (program resp. utbildningsgrupp):
    # senaste historiska år jämfört med vald prognosperiod (årligt genomsnitt).
    # Klick visar kategorin över tid.
    kategori_vald <- reactiveVal(NULL)
    observeEvent(list(input$omrade, input$gym_matt, input$eg_matt), kategori_vald(NULL), ignoreInit = TRUE)
    observeEvent(input$d_period_jmf_selected, {
      sel <- input$d_period_jmf_selected
      kategori_vald(if (length(sel) == 1) sel else NULL)
    }, ignoreNULL = FALSE)

    gym_matt <- reactive(if (is.null(input$gym_matt)) "elever_ak1" else input$gym_matt)
    eg_ind <- reactive(REGTOP_EFTERGYMN[[if (is.null(input$eg_matt)) "yh_antagna" else input$eg_matt]])
    # Nybörjare på högskolans yrkesexamensprogram finns bara för riket.
    period_region <- reactive(if (omrade() == "eftergymnasialt" && isTRUE(eg_ind()$bara_riket)) "00" else region())
    period_region_namn <- reactive(names(regtop_regioner())[match(period_region(), regtop_regioner())])

    # Rådata för valt mått och område, med kolumnen kategori.
    period_ra <- reactive({
      if (omrade() == "gymnasiet") {
        d <- dplyr::filter(hamta_regtop("gymnasiet"), regionkod == period_region(), matt == gym_matt())
        pt <- input$gym_programtyp
        if (!is.null(pt) && pt != "_alla_") d <- dplyr::filter(d, programtyp == pt)
        return(dplyr::mutate(d, kategori = program))
      }
      ind <- eg_ind()
      d <- dplyr::filter(hamta_regtop("eftergymnasial"), regionkod == period_region(),
                         utbildningsform == ind$utbildningsform, matt == ind$matt)
      dplyr::mutate(d, kategori = if (ind$matt == "nyborjare") dplyr::coalesce(examen, utbildningsgrupp)
                                  else utbildningsgrupp)
    })
    period_summa <- reactive(regtop_period_summa(period_ra(), grupp = "kategori"))
    prognosperioder <- reactive({
      d <- dplyr::filter(period_summa(), typ == "prognos") |> dplyr::distinct(ar_fran, period) |>
        dplyr::arrange(ar_fran)
      d$period
    })
    vald_period <- reactive({
      p <- input$period
      if (is.null(p) || !p %in% prognosperioder()) utils::tail(prognosperioder(), 1) else p
    })
    senaste_hist <- reactive({
      h <- dplyr::filter(period_summa(), typ == "historik")
      if (nrow(h) == 0) NA_character_ else h$period[which.max(h$ar_fran)]
    })

    output$omrade_ui <- renderUI({
      if (omrade() == "vagar")
        return(selectInput(ns("vagar_grupp"), "Utbildningsgrupp", choices = vagar_grupper(),
                           selected = if (!is.null(grupp_vald()) && grupp_vald() %in% vagar_grupper())
                                        grupp_vald() else isolate(vagar_grupp())))
      if (omrade() == "gymnasiet")
        return(tagList(
          shinyWidgets::radioGroupButtons(ns("gym_matt"), "Mått",
                                          choices = c("Elever i åk 1" = "elever_ak1", "Avgångna" = "avgangna"),
                                          selected = isolate(gym_matt())),
          selectInput(ns("gym_programtyp"), "Programtyp",
                      choices = c("Alla" = "_alla_", "Högskoleförberedande program", "Yrkesprogram",
                                  "Introduktionsprogram")),
          uiOutput(ns("period_ui"))))
      if (omrade() == "eftergymnasialt")
        return(tagList(
          shinyWidgets::radioGroupButtons(
            ns("eg_matt"), "Mått", direction = "vertical",
            choices = stats::setNames(names(REGTOP_EFTERGYMN), vapply(REGTOP_EFTERGYMN, `[[`, "", "label")),
            selected = isolate(if (is.null(input$eg_matt)) "yh_antagna" else input$eg_matt)),
          uiOutput(ns("period_ui"))))
      NULL
    })
    output$period_ui <- renderUI({
      req(length(prognosperioder()) > 0)
      selectInput(ns("period"), "Prognosperiod", choices = prognosperioder(), selected = isolate(vald_period()))
    })

    vy_period <- function() {
      fluidRow(
        column(7, ggiraph::girafeOutput(ns("d_period_jmf"), height = "560px"),
               tags$p(class = "rd-hint rd-hint--bar", "Klicka på en stapel för att se utvecklingen över tid."),
               if (omrade() == "eftergymnasialt" && isTRUE(eg_ind()$bara_riket))
                 tags$p(class = "rd-hint", "Nybörjare på högskolans yrkesexamensprogram finns bara för riket.")),
        column(5, div(class = "rd-subcard", ggiraph::girafeOutput(ns("d_period_tid"), height = "380px")),
               tags$p(class = "rd-hint",
                      if (omrade() == "gymnasiet")
                        "Län avser folkbokföringslän (där eleverna bor), inte skolans län."
                      else "Prognoserna gäller hela perioden och visas som årligt genomsnitt."))
      )
    }
    matt_label <- reactive({
      if (omrade() == "gymnasiet") c(elever_ak1 = "Elever i åk 1", avgangna = "Avgångna")[[gym_matt()]]
      else eg_ind()$label
    })
    output$d_period_jmf <- ggiraph::renderGirafe({
      req(omrade() %in% c("gymnasiet", "eftergymnasialt"))
      d <- period_summa()
      validate(need(nrow(d) > 0, "Inga data för valt urval."))
      jmf <- d |>
        dplyr::group_by(kategori) |>
        dplyr::summarise(hist = sum(varde[typ == "historik" & period %in% senaste_hist()]),
                         prog = sum(varde[typ == "prognos" & period %in% vald_period()]), .groups = "drop") |>
        dplyr::mutate(hist = dplyr::na_if(hist, 0), prog = dplyr::na_if(prog, 0))
      skapa_diagram_prognos_jmf(
        jmf, hist_label = paste0(senaste_hist(), " (historik)"),
        prog_label = paste0(vald_period(), " (prognos, per år)"),
        rubrik = paste0(matt_label(), " – ", period_region_namn()),
        underrubrik = paste0("Senaste år jämfört med prognosen ", vald_period()))
    })
    output$d_period_tid <- ggiraph::renderGirafe({
      req(omrade() %in% c("gymnasiet", "eftergymnasialt"))
      k <- kategori_vald()
      d <- period_ra()
      if (!is.null(k)) d <- dplyr::filter(d, kategori == k)
      validate(need(nrow(d) > 0, "Inga data."))
      skapa_diagram_prognos_tid(
        regtop_period_summa(d),
        rubrik = paste(strwrap(paste0(matt_label(), " – ", if (is.null(k)) "totalt" else k), 55),
                       collapse = "\n"),
        underrubrik = period_region_namn())
    })

    # ---- Nedladdning ------------------------------------------------------------------
    urval_data <- function() {
      if (omrade() == "vagar") return(vagar_data(unique(c(region(), "00"))))
      if (omrade() %in% c("gymnasiet", "eftergymnasialt")) return(period_ra())
      if (is.null(grupp_vald())) return(dplyr::select(bedomning_region(), -har_bedomning))
      dplyr::filter(hamta_regtop("tillgang_efterfragan"), regionkod == region(),
                    utbildningsgrupp_kod == grupp_vald())
    }
    output$ladda_ner <- downloadHandler(
      filename = function() paste0("prognos2040_", omrade(), "_", region(), if (!is.null(grupp_vald())) paste0("_", grupp_vald()), ".xlsx"),
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
