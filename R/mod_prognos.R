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
  rorlighet       = list(label = "Flytt och pendling"),
  gymnasiet       = list(label = "Gymnasiet"),
  eftergymnasialt = list(label = "Eftergymnasialt")
)

# Bedömning 2040: översikt (alla grupper, diagram/tabell) eller en vald grupp.
.BEDOMNING_VISNING <- c("Alla (diagram)" = "diagram", "Alla (tabell)" = "tabell",
                        "En utbildningsgrupp" = "grupp")

# Ungefärlig kolumnbredd (px) i sidebarLayout-huvudpanelen (width = 9), för
# att dimensionera girafe-diagrammen efter sitt eget bredd/höjd-förhållande
# (se .girafe_hojd() i func_diagram.R) - minimerar tomt utrymme i korten.
.PROGNOS_KOL7_PX <- 560
.PROGNOS_KOL6_PX <- 470
.PROGNOS_KOL5_PX <- 400

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
               "Prognoserna bygger på SCB:s Regionala trender och prognoser 2025, ",
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
    region <- reactive(if (is.null(input$region)) "20" else input$region)
    region_namn <- reactive(names(regtop_regioner())[match(region(), regtop_regioner())])

    # Alla utbildningsgrupper i valt län (oavsett aggregatfilter), för den
    # sökbara utbildningsgruppsväljaren i "En utbildningsgrupp".
    bedomning_alla_grupper <- reactive({
      d <- dplyr::filter(regtop_bedomning(), regionkod == region()) |>
        dplyr::distinct(utbildningsgrupp_kod, utbildningsgrupp) |> dplyr::arrange(utbildningsgrupp)
      stats::setNames(d$utbildningsgrupp_kod, d$utbildningsgrupp)
    })

    output$aggregat_ui <- renderUI({
      req(omrade() == "bedomning")
      # Får INTE isoleras: knappen "En utbildningsgrupp" ska flippa om den
      # här panelen direkt, både vid klick i panelen och via
      # updateRadioGroupButtons() (t.ex. klick i översiktsdiagrammet).
      visning_val <- if (is.null(input$visning)) "diagram" else input$visning
      if (identical(visning_val, "grupp")) {
        val <- bedomning_alla_grupper()
        return(tagList(
          shinyWidgets::radioGroupButtons(ns("visning"), "Visa", choices = .BEDOMNING_VISNING,
                                          selected = "grupp"),
          selectInput(ns("bedomning_grupp"), "Utbildningsgrupp (sökbar)", choices = val,
                      selected = isolate(if (!is.null(grupp_vald()) && grupp_vald() %in% val) grupp_vald()
                                         else unname(val[1])))))
      }
      agg <- sort(unique(regtop_bedomning()$aggregat))
      tagList(
        shinyWidgets::radioGroupButtons(ns("visning"), "Visa", choices = .BEDOMNING_VISNING,
                                        selected = visning_val),
        selectInput(ns("aggregat"), "Utbildningsgrupper",
                    choices = c("Alla" = "_alla_", agg),
                    selected = isolate(if (!is.null(input$aggregat) && input$aggregat %in% agg) input$aggregat)))
    })
    observeEvent(input$bedomning_grupp, {
      req(input$bedomning_grupp)
      grupp_vald(input$bedomning_grupp)
    })

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
      if (length(sel) == 1) {
        grupp_vald(sel)
        shinyWidgets::updateRadioGroupButtons(session, "visning", selected = "grupp")
      }
    })
    observeEvent(input$tillbaka, {
      grupp_vald(NULL)
      shinyWidgets::updateRadioGroupButtons(session, "visning", selected = "diagram")
    })
    # Detaljvyn visas antingen när väljaren står på "En utbildningsgrupp"
    # eller när en grupp redan är vald (klick från ett annat diagram) - de
    # kan skilja sig åt ett kort ögonblick innan input$bedomning_grupp hinner
    # fram till servern, se aggregat_ui.
    detaljvy <- reactive(identical(input$visning, "grupp") || !is.null(grupp_vald()))
    grupp_rad <- reactive({
      req(grupp_vald())
      dplyr::filter(regtop_bedomning(), regionkod == region(), utbildningsgrupp_kod == grupp_vald())
    })
    grupp_namn <- reactive({
      n <- regtop_bedomning()$utbildningsgrupp[regtop_bedomning()$utbildningsgrupp_kod == grupp_vald()]
      if (length(n) == 0) grupp_vald() else n[1]
    })

    # Faktorerna som ligger bakom tillgången resp. efterfrågan för vald grupp
    # - delade reactives så att både layouthöjden och diagrammen använder
    # exakt samma data (se output$vy och output$d_tillgang_faktorer/d_efterfragan).
    tillgang_faktorer <- reactive({
      req(grupp_vald())
      f <- regtop_faktorer(region(), grupp_vald()) |> dplyr::filter(sida == "tillgang") |>
        regtop_summa_period() |> dplyr::select(faktor_text, varde)
      r <- grupp_rad()
      start <- if (nrow(r) > 0) r$tillgang_2023[1] else NA
      slut  <- if (nrow(r) > 0) r$tillgang_2040[1] else NA
      if (is.na(start) || is.na(slut)) return(f)
      # Avrundning ger ofta någon enstaka person - bara märkbara skillnader
      # (minst 1 % av tillgången 2023) får en egen stapel.
      rest <- slut - start - sum(f$varde, na.rm = TRUE)
      if (abs(rest) >= max(1, 0.01 * abs(start)))
        f <- dplyr::bind_rows(f, tibble::tibble(faktor_text = "Övrigt och avrundning", varde = rest))
      f
    })
    efterfragan_faktorer <- reactive({
      req(grupp_vald())
      regtop_faktorer(region(), grupp_vald()) |> dplyr::filter(sida == "efterfragan") |>
        dplyr::select(faktor_text, varde)
    })

    output$brodsmula <- renderText({
      if (omrade() != "bedomning")
        return(paste0("Prognos 2040 › ", prognos_struktur[[omrade()]]$label,
                      if (!is.null(kategori_vald())) paste0(" › ", kategori_vald())))
      paste0("Prognos 2040 › Bedömning 2040",
             if (detaljvy() && !is.null(grupp_vald())) paste0(" › ", grupp_namn()))
    })

    output$vy <- renderUI({
      if (omrade() == "vagar") return(vy_vagar())
      if (omrade() %in% c("gymnasiet", "eftergymnasialt")) return(vy_period())
      if (omrade() == "rorlighet") return(vy_rorlighet())
      if (!detaljvy() && identical(input$visning, "tabell"))
        return(tagList(uiOutput(ns("tabell")),
                       tags$p(class = "rd-hint", "Klicka på en rad för att se vad som ligger bakom bedömningen.")))
      if (!detaljvy()) {
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
      # Höjderna räknas ut från varje diagrams eget bredd/höjd-förhållande så
      # att korten inte får onödigt tomt utrymme (se .girafe_hojd(), func_diagram.R).
      # grupp_vald() kan vara NULL ett kort ögonblick (se detaljvy() ovan) -
      # utbildningsgrupp_kod == NULL ger ett tomt urval, inte ett fel.
      n_lan <- if (is.null(grupp_vald())) 0 else
        nrow(dplyr::filter(regtop_bedomning(), utbildningsgrupp_kod == grupp_vald(),
                           regionkod != "00", har_bedomning))
      tagList(
        if (!is.null(grupp_vald()))
          tagList(actionLink(ns("tillbaka"), "← Alla utbildningsgrupper", class = "rd-tillbaka"),
                  tags$h3(class = "rd-rubrik-grupp", grupp_namn(), tags$small(paste0(" · ", region_namn())))),
        fluidRow(
          column(7, div(class = "rd-subcard",
                        ggiraph::girafeOutput(ns("d_tillgang"),
                                              height = .girafe_hojd(6.5, 3.6, .PROGNOS_KOL7_PX)))),
          column(5, div(class = "rd-subcard",
                        ggiraph::girafeOutput(ns("d_matare"), height = .girafe_hojd(5, 2.3, .PROGNOS_KOL5_PX)),
                        uiOutput(ns("faktaruta"))))
        ),
        fluidRow(
          column(6, div(class = "rd-subcard",
                        ggiraph::girafeOutput(
                          ns("d_tillgang_faktorer"),
                          height = .girafe_hojd(5, max(2.6, nrow(tillgang_faktorer()) * 0.4 + 1),
                                               .PROGNOS_KOL6_PX)))),
          column(6, div(class = "rd-subcard",
                        ggiraph::girafeOutput(
                          ns("d_efterfragan"),
                          height = .girafe_hojd(5, max(2.6, nrow(efterfragan_faktorer()) * 0.4 + 1),
                                               .PROGNOS_KOL6_PX))))
        ),
        fluidRow(
          column(7, div(class = "rd-subcard",
                        ggiraph::girafeOutput(ns("d_lan"),
                                              height = .girafe_hojd(5, max(3.5, n_lan * 0.2 + 1.6),
                                                                    .PROGNOS_KOL7_PX)))),
          column(5, div(class = "rd-subcard",
                        div(class = "rd-kon-kontroll",
                            shinyWidgets::radioGroupButtons(
                              ns("karta_klasser"), label = NULL, size = "sm",
                              choices = c("Tre klasser" = "3", "Sju klasser" = "7"),
                              selected = isolate(if (is.null(input$karta_klasser)) "3" else input$karta_klasser))),
                        ggiraph::girafeOutput(ns("d_karta"), height = .girafe_hojd(4.6, 6, .PROGNOS_KOL5_PX)),
                        tags$p(class = "rd-hint",
                               "Tre klasser som i SCB:s rapport (viss brist, brist och stor brist = brist). ",
                               "Klicka på ett län för att byta län.")))
        ),
        fluidRow(
          column(7, div(class = "rd-subcard",
                        ggiraph::girafeOutput(ns("d_rorlighet"), height = .girafe_hojd(8, 3.6, .PROGNOS_KOL7_PX)))),
          column(5, div(class = "rd-subcard",
                        ggiraph::girafeOutput(ns("d_kon"), height = .girafe_hojd(5, 3.2, .PROGNOS_KOL5_PX))))
        )
      )
    })

    # ---- Tabell: valt län och riket sida vid sida (som SCB:s sammanfattning) -----
    output$tabell <- renderUI({
      req(omrade() == "bedomning")
      d <- dplyr::filter(regtop_bedomning(), regionkod %in% unique(c(region(), "00")))
      a <- input$aggregat
      if (!is.null(a) && a != "_alla_") d <- dplyr::filter(d, aggregat == a)
      validate(need(nrow(d) > 0, "Inga data för valt urval."))
      koder <- unique(c(region(), "00"))
      kol_namn <- ifelse(koder == "00", "Riket", region_namn())
      cell <- function(r) {
        if (nrow(r) == 0 || !isTRUE(r$har_bedomning[1]))
          return(tags$td(class = "rd-bed-cell", style = paste0("background:", REGTOP_SAKNAS_FARG), "Prognos saknas"))
        b <- as.character(r$bedomning_2040[1])
        mork <- match(b, REGTOP_BEDOMNINGAR) %in% c(1, 6, 7)
        tags$td(class = "rd-bed-cell",
                style = paste0("background:", BEDOMNING_FARGER[[b]], if (mork) ";color:#fff"),
                title = paste0("Balanstal ", .fmt_proc(r$balanstal_procent[1])), b)
      }
      grupper <- d |> dplyr::distinct(aggregat, utbildningsgrupp_kod, utbildningsgrupp) |>
        dplyr::arrange(aggregat, utbildningsgrupp)
      rader <- lapply(split(grupper, grupper$aggregat), function(g) {
        tagList(
          tags$tr(class = "rd-bed-avsnitt", tags$td(colspan = length(koder) + 1, g$aggregat[1])),
          lapply(seq_len(nrow(g)), function(i) {
            kod <- g$utbildningsgrupp_kod[i]
            tags$tr(class = "rd-bed-rad",
                    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("tabell_grupp"), kod),
                    tags$td(g$utbildningsgrupp[i]),
                    lapply(koder, function(k) cell(dplyr::filter(d, regionkod == k, utbildningsgrupp_kod == kod))))
          }))
      })
      tags$table(class = "rd-tabell-bedomning",
                 tags$thead(tags$tr(tags$th("Utbildningsgrupp"), lapply(kol_namn, tags$th))),
                 tags$tbody(rader))
    })
    observeEvent(input$tabell_grupp, {
      grupp_vald(input$tabell_grupp)
      shinyWidgets::updateRadioGroupButtons(session, "visning", selected = "grupp")
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

    # SCB:s antaganden om utbildningsväxling tar en del plats (ofta en lista
    # med flera yrken), så de ligger i en modal bakom en info-länk i stället
    # för inline i faktarutan (se observeEvent(input$antaganden_info) nedan).
    antaganden_data <- reactive({
      req(grupp_vald())
      tryCatch(
        hamta_regtop("antaganden_utbildningsvaxling") |>
          dplyr::filter(utbildningsgrupp_kod == grupp_vald()) |>
          dplyr::distinct(yrke, antagande),
        error = function(e) NULL)
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
      ant <- antaganden_data()
      tagList(
        bed,
        if (length(yrken) > 0) tagList(tags$div(class = "rd-label", "Vanligaste yrken"),
                                       tags$ol(lapply(utils::head(yrken, 3), tags$li))),
        if (!is.null(ant) && nrow(ant) > 0)
          tags$p(class = "rd-hint",
                 actionLink(ns("antaganden_info"),
                            label = tagList(HTML("&#x24D8; "), "SCB:s antaganden om utbildningsväxling")))
      )
    })
    # SCB gör en bedömning per yrke som utbildningsgruppen förekommer i - det
    # kan bli över hundra yrken för en stor grupp. Fördelningen mellan
    # "minskar/samma/ökar" varierar mycket mellan grupper (för en del grupper
    # är "minskar" den stora kategorin, för andra "samma"), så det går inte
    # att gissa i förväg vilken som är störst. Modalen grupperar därför yrkena
    # per antagande och fäller bara ihop den STÖRSTA kategorin bakom en
    # <details> - de mindre, mer informativa kategorierna syns direkt.
    observeEvent(input$antaganden_info, {
      ant <- antaganden_data()
      req(!is.null(ant), nrow(ant) > 0)
      per_kategori <- ant |> dplyr::count(antagande) |> dplyr::arrange(dplyr::desc(n))
      storst <- per_kategori$antagande[1]
      avsnitt <- lapply(per_kategori$antagande, function(kat) {
        rader <- dplyr::filter(ant, antagande == kat) |> dplyr::arrange(yrke)
        etikett <- paste0(kat, " (", nrow(rader), " yrken)")
        lista <- tags$ul(lapply(rader$yrke, tags$li))
        if (identical(kat, storst)) tags$details(tags$summary(etikett), lista)
        else tagList(tags$div(class = "rd-label", etikett), lista)
      })
      showModal(modalDialog(
        title = "SCB:s antaganden om utbildningsväxling", easyClose = TRUE, size = "m",
        tags$p(class = "rd-hint",
               "Antagandena gäller hur stor andel av dem som arbetar inom respektive yrke som ",
               "SCB bedömer kommer att ha den här utbildningen 2040."),
        avsnitt
      ))
    })

    output$d_tillgang_faktorer <- ggiraph::renderGirafe({
      skapa_diagram_faktorer(
        tillgang_faktorer(), fmt = .fmt_antal, x_label = "Förändring 2024–2040, antal personer",
        rubrik = "Vad förändrar tillgången till 2040?",
        underrubrik = paste0(region_namn(), " · summa för 2024–2040"))
    })

    output$d_efterfragan <- ggiraph::renderGirafe({
      skapa_diagram_faktorer(
        efterfragan_faktorer(), fmt = .fmt_proc, x_label = "Påverkan på efterfrågan 2023–2040",
        rubrik = "Vad förändrar efterfrågan?", underrubrik = region_namn())
    })

    output$d_kon <- ggiraph::renderGirafe({
      req(grupp_vald())
      d <- dplyr::filter(hamta_regtop("arbetskraft_utbildningsgrupp"), regionkod == region(),
                         utbildningsgrupp_kod == grupp_vald())
      skapa_diagram_arbetskraft_kon(d, har_bedomning = isTRUE(grupp_rad()$har_bedomning[1]),
                                    rubrik = "Arbetskraften per kön", underrubrik = region_namn())
    })

    output$d_lan <- ggiraph::renderGirafe({
      req(grupp_vald())
      d <- dplyr::filter(regtop_bedomning(), utbildningsgrupp_kod == grupp_vald())
      skapa_diagram_bedomning_lan(d, region(), rubrik = "Samma utbildningsgrupp i alla län",
                                  underrubrik = "Balanstal och bedömning 2040")
    })

    # Karta: bedömningen för gruppen i alla län. Klick byter län.
    output$d_karta <- ggiraph::renderGirafe({
      req(grupp_vald())
      geo <- hamta_lan_geometri()
      validate(need(!is.null(geo), paste0("Kartan kunde inte läsas",
                                          if (!is.null(.geo_cache$fel_lan)) paste0(": ", .geo_cache$fel_lan) else ".")))
      d <- dplyr::filter(regtop_bedomning(), utbildningsgrupp_kod == grupp_vald())
      skapa_karta_bedomning(geo, d, region(), rubrik = "Bedömning 2040 per län",
                            klasser = as.integer(if (is.null(input$karta_klasser)) "3" else input$karta_klasser))
    })

    # Mätare: valt län och riket.
    output$d_matare <- ggiraph::renderGirafe({
      req(grupp_vald())
      d <- dplyr::filter(regtop_bedomning(), utbildningsgrupp_kod == grupp_vald(),
                         regionkod %in% unique(c(region(), "00"))) |>
        dplyr::mutate(bedomning_2040 = dplyr::if_else(har_bedomning, bedomning_2040, factor(NA)),
                      serie = dplyr::if_else(regionkod == "00", "Riket", region),
                      ordn = regionkod == "00") |>
        dplyr::arrange(ordn)
      validate(need(nrow(d) > 0, "Ingen bedömning."))
      skapa_diagram_matare(d)
    })
    observeEvent(input$d_karta_selected, {
      sel <- input$d_karta_selected
      if (length(sel) == 1 && sel %in% regtop_regioner())
        shinyWidgets::updatePickerInput(session, "region", selected = sel)
    })

    # ---- Rörlighet ---------------------------------------------------------------------
    # I fördjupningen: inflöde, utflöde och netto för vald grupp.
    output$d_rorlighet <- ggiraph::renderGirafe({
      req(grupp_vald())
      d <- regtop_rorlighet(region(), grupp_vald())
      skapa_diagram_rorlighet(d, har_bedomning = isTRUE(grupp_rad()$har_bedomning[1]),
                              rubrik = "Flytt, migration och pendling",
                              underrubrik = paste0(region_namn(), if (region() == "00")
                                " \u00b7 för riket finns bara migration" else ""))
    })

    # Statistikområdet Flytt och pendling: nettot per utbildningsgrupp för
    # valt flöde och period. Klick öppnar gruppens fördjupning.
    rorlighet_valt <- reactive(if (is.null(input$rorlighet_flode)) "inrikes_flytt" else input$rorlighet_flode)
    rorlighet_netto <- reactive({
      hamta_regtop("rorlighet") |>
        dplyr::filter(regionkod == region(), flode == rorlighet_valt(), variabel == "netto") |>
        .regtop_perioder() |> regtop_arligt() |>
        dplyr::mutate(period = dplyr::if_else(ar_fran == ar_till, as.character(ar_fran),
                                              paste0(ar_fran, "\u2013", ar_till)))
    })
    rorlighet_perioder <- reactive(unique(rorlighet_netto()$period[order(rorlighet_netto()$ar_fran)]))
    output$rorlighet_period_ui <- renderUI({
      req(length(rorlighet_perioder()) > 0)
      tidigare <- isolate(input$rorlighet_period)
      selectInput(ns("rorlighet_period"), "Period", choices = rorlighet_perioder(),
                  selected = if (!is.null(tidigare) && tidigare %in% rorlighet_perioder()) tidigare
                             else utils::tail(rorlighet_perioder(), 1))
    })
    vy_rorlighet <- function() {
      n <- dplyr::n_distinct(rorlighet_netto()$utbildningsgrupp_kod)
      tagList(
        ggiraph::girafeOutput(ns("d_rorlighet_netto"), height = paste0(max(400, n * 20 + 180), "px")),
        tags$p(class = "rd-hint",
               "Nettot är inflöde minus utflöde. Positivt netto ökar länets tillgång på personer med ",
               "utbildningen. Klicka på en grupp för att se hela bilden."))
    }
    output$d_rorlighet_netto <- ggiraph::renderGirafe({
      req(omrade() == "rorlighet")
      p <- if (is.null(input$rorlighet_period) || !input$rorlighet_period %in% rorlighet_perioder())
        utils::tail(rorlighet_perioder(), 1) else input$rorlighet_period
      d <- dplyr::filter(rorlighet_netto(), period %in% p)
      validate(need(nrow(d) > 0, if (region() == "00")
        "För riket finns bara migration - välj Migration eller ett län." else "Inga data för valt urval."))
      d <- d |> dplyr::mutate(
        grupp = forcats::fct_reorder(utbildningsgrupp, varde),
        riktning = dplyr::if_else(varde >= 0, "okning", "minskning"),
        tooltip = paste0("<b>", utbildningsgrupp, "</b><br/>Netto ", p, if_else_prognos(typ), ": ",
                         dplyr::if_else(varde > 0, "+", ""), .fmt_antal(varde),
                         if (rorlighet_valt() == "pendling") "" else " per år"))
      g <- ggplot2::ggplot(d, ggplot2::aes(x = varde, y = grupp, fill = riktning)) +
        ggplot2::geom_vline(xintercept = 0, colour = RD_TEXT_MUTED, linewidth = 0.4) +
        ggiraph::geom_col_interactive(ggplot2::aes(tooltip = tooltip, data_id = utbildningsgrupp_kod), width = 0.75) +
        ggplot2::scale_fill_manual(values = c(okning = "#459079", minskning = "#d9653b"), guide = "none") +
        ggplot2::scale_x_continuous(labels = .fmt_antal) +
        ggplot2::labs(x = if (rorlighet_valt() == "pendling") "Nettopendling, antal personer" else "Netto per år",
                      y = NULL,
                      title = paste0(REGTOP_FLODE_TEXT[[rorlighet_valt()]], " \u2013 ", region_namn()),
                      subtitle = paste0("Netto per utbildningsgrupp, ", p),
                      caption = paste0("Källa: ", KALLA_REGTOP)) +
        .rd_tema()
      .girafe_std(g, width_svg = 7.5, height_svg = max(4, nrow(d) * 0.22 + 1.8), selection = TRUE)
    })
    observeEvent(input$d_rorlighet_netto_selected, {
      sel <- input$d_rorlighet_netto_selected
      if (length(sel) != 1) return()
      grupp_vald(sel)
      shinyWidgets::updateRadioGroupButtons(session, "omrade", selected = "bedomning")
      shinyWidgets::updateRadioGroupButtons(session, "visning", selected = "grupp")
    })

    # ---- Vägar in i yrket ---------------------------------------------------------
    # Utbildningsgrupperna med examinationsprognos i valt län. Förvald: den
    # grupp man senast klickat på i Bedömning 2040.
    vagar_grupper <- reactive({
      d <- dplyr::filter(hamta_regtop("examination_utbildningsform"), regionkod == region()) |>
        dplyr::distinct(utbildningsgrupp_kod, utbildningsgrupp) |> dplyr::arrange(utbildningsgrupp)
      stats::setNames(d$utbildningsgrupp_kod, d$utbildningsgrupp)
    })
    # Ingen grupp vald sedan tidigare: slumpa vilken som visas, så att det
    # inte alltid är samma (alfabetiskt första) grupp som möter besökaren.
    vagar_grupp <- reactive({
      g <- input$vagar_grupp
      if (!is.null(g) && g %in% vagar_grupper()) return(g)
      unname(sample(vagar_grupper(), 1))
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
    period_region_namn <- reactive({
      if (omrade() == "eftergymnasialt" && isTRUE(eg_ind()$larosate)) return(valt_larosate())
      names(regtop_regioner())[match(period_region(), regtop_regioner())]
    })
    # Lärosäten i nyborjare_larosate; Högskolan Dalarna förvald.
    larosaten <- reactive(sort(unique(hamta_regtop("nyborjare_larosate")$larosate)))
    valt_larosate <- reactive({
      l <- input$larosate
      if (!is.null(l) && l %in% larosaten()) return(l)
      hd <- grep("Dalarna", larosaten(), value = TRUE)
      if (length(hd) > 0) hd[1] else larosaten()[1]
    })
    output$larosate_ui <- renderUI({
      req(omrade() == "eftergymnasialt", isTRUE(eg_ind()$larosate))
      shinyWidgets::pickerInput(ns("larosate"), "Lärosäte", choices = larosaten(),
                                selected = isolate(valt_larosate()),
                                options = shinyWidgets::pickerOptions(liveSearch = TRUE))
    })

    # Rådata för valt mått och område, med kolumnen kategori.
    period_ra <- reactive({
      if (omrade() == "gymnasiet") {
        d <- dplyr::filter(hamta_regtop("gymnasiet"), regionkod == period_region(), matt == gym_matt())
        pt <- input$gym_programtyp
        if (!is.null(pt) && pt != "_alla_") d <- dplyr::filter(d, programtyp == pt)
        return(dplyr::mutate(d, kategori = program))
      }
      ind <- eg_ind()
      if (isTRUE(ind$larosate)) {
        d <- hamta_regtop("nyborjare_larosate")
        d <- dplyr::filter(d, larosate == valt_larosate())
        return(dplyr::mutate(d, kategori = dplyr::coalesce(examen, utbildningsgrupp)))
      }
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
      if (omrade() == "rorlighet")
        return(tagList(
          shinyWidgets::radioGroupButtons(ns("rorlighet_flode"), "Flöde", direction = "vertical",
                                          choices = stats::setNames(names(REGTOP_FLODE_TEXT), REGTOP_FLODE_TEXT),
                                          selected = isolate(rorlighet_valt())),
          uiOutput(ns("rorlighet_period_ui"))))
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
          uiOutput(ns("larosate_ui")),
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
      har_prognos <- length(vald_period()) == 1
      skapa_diagram_prognos_jmf(
        jmf, hist_label = paste0(senaste_hist(), " (historik)"),
        prog_label = if (har_prognos) paste0(vald_period(), " (prognos, per år)") else "Prognos",
        rubrik = paste0(matt_label(), " \u2013 ", period_region_namn()),
        underrubrik = if (har_prognos) paste0("Senaste år jämfört med prognosen ", vald_period())
                      else paste0("Senaste år (", senaste_hist(), "), ingen prognos"))
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
