# ============================================================
#  func_karta.R
#  Liten referenskarta: Dalarnas kommuner som polygoner, färgade
#  efter samverkansområde. Ingen bakgrundskarta (bara polygoner).
#
#  Geometrin hämtas ur produktionsdatabasen (geodata.karta.kommun_scb)
#  via shiny_uppkoppling_las("geodata") och cachas per R-process.
#  Renderas via ggiraph (samma stack som diagrammen). Kräver paketet sf
#  och hjälparen df_till_sf(); saknas något, eller om läsningen misslyckas,
#  returneras NULL och appen visar en liten fallback-text i stället för att
#  krascha.
# ============================================================

# Färgerna SAMVERKAN_FARGER definieras centralt i def_farger.R (läses ur CSS).

# Enkel processcache så geometrin bara hämtas en gång.
.geo_cache <- new.env(parent = emptyenv())

.las_kommun_geometri <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) return(NULL)

  # Hämta kommunpolygonerna ur geodata-databasen. df_till_sf() är er egen
  # helper som gör om den hämtade tabellen till ett sf-objekt. Vi döper om
  # knkod/knnamn till app-namnen kommkod/kommun.
  geo <- tryCatch(
    dplyr::tbl(shiny_uppkoppling_las("geodata"),
               dbplyr::in_schema("karta", "kommun_scb")) |>
      dplyr::filter(str_sub(knkod, 1, 2) == "20") |>   # Dalarnas kommuner (länskod 20)
      dplyr::collect() |>
      df_till_sf() |>
      dplyr::rename(kommkod = knkod, kommun = knnamn) |>
      sf::st_transform(crs = 4326),
    error = function(e) NULL
  )
  if (is.null(geo)) return(NULL)

  geo$kommkod <- as.character(geo$kommkod)
  dplyr::left_join(geo, kommun_samverkan, by = "kommkod")
}

hamta_kommun_geometri <- function() {
  if (is.null(.geo_cache$geo)) .geo_cache$geo <- .las_kommun_geometri()
  .geo_cache$geo
}

# niva: "kommun" eller "samverkansomrade".
# Kommunläge: kommunerna var för sig (enfärgade, vita kommungränser), inga
#   samverkansområden syns. Klick väljer kommun.
# Samverkansläge: kommunerna inom samma område slås ihop (inre gränser tas bort)
#   och färgas per område. Klick väljer hela området.
# Markeringen sköts av ggiraphs urval (klick), så ett klick på en redan vald yta
# avmarkerar (-> hela Dalarna).
skapa_karta_samverkan <- function(niva = "kommun") {
  geo <- hamta_kommun_geometri()
  if (is.null(geo)) return(NULL)

  gemensam_girafe <- function(g) {
    ggiraph::girafe(
      ggobj = g, width_svg = 3.6, height_svg = 4.6,
      options = list(
        ggiraph::opts_hover(css = RD_KARTA_HOVER_CSS),
        ggiraph::opts_tooltip(css = RD_TOOLTIP_CSS),
        ggiraph::opts_selection(type = "single", only_shiny = TRUE,
                                css = RD_KARTA_SELECT_CSS),
        ggiraph::opts_toolbar(saveaspng = TRUE,
                              hidden = c("lasso_select", "lasso_deselect"))
      )
    )
  }

  if (niva == "samverkansomrade") {
    # Slå ihop kommunerna till ett område-polygon (inre gränser försvinner).
    omr <- geo |>
      dplyr::group_by(samverkansomrade) |>
      dplyr::summarise(.groups = "drop")

    # Tooltip på det interaktiva lagret: visa kommunnamn + område när man hovrar.
    geo$tooltip <- paste0("<b>", geo$kommun, "</b><br/>", geo$samverkansomrade)

    g <- ggplot2::ggplot() +
      # Lager 1: områden (interaktivt, klickbart, ger urval)
      ggiraph::geom_sf_interactive(
        data = omr,
        ggplot2::aes(fill = samverkansomrade, data_id = samverkansomrade,
                     tooltip = samverkansomrade),
        color = NA) +                                    # inga synliga gränser här …
      # Lager 2: kommungränser (svaga, ej interaktiva)
      ggplot2::geom_sf(
        data = geo, fill = NA, color = "white", linewidth = 0.25, alpha = 0.6) +
      # Lager 3: hover/tooltip på kommunnivå (transparent fyll, interaktivt)
      ggiraph::geom_sf_interactive(
        data = geo,
        ggplot2::aes(tooltip = tooltip, data_id = samverkansomrade),
        fill = NA, color = NA) +                         # syns inte, men fångar hover
      ggplot2::scale_fill_manual(values = SAMVERKAN_FARGER, name = NULL) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
      ggplot2::theme_void(base_size = 11) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.key.size = ggplot2::unit(10, "pt"),
        legend.text     = ggplot2::element_text(size = 8),
        plot.margin     = ggplot2::margin(2, 2, 2, 2)
      )
    return(gemensam_girafe(g))
  }

  # Kommunläge: enfärgade kommuner, vita gränser, ingen områdesfärg/legend.
  geo$tooltip <- paste0("<b>", geo$kommun, "</b><br/>", geo$samverkansomrade)

  g <- ggplot2::ggplot(geo) +
    ggiraph::geom_sf_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = kommkod),
      fill = KOMMUN_FYLL, color = "#ffffff", linewidth = 0.4) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(legend.position = "none",
                   plot.margin = ggplot2::margin(2, 2, 2, 2))
  gemensam_girafe(g)
}
