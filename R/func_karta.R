# ============================================================
#  func_karta.R
#  Liten referenskarta: Dalarnas kommuner/samverkansområden.
#
#  Markeringen vid hover/val sker genom att YTAN mörknar
#  (RD_KARTA_HOVER/SELECT_CSS = filter:brightness). Det undviker
#  kant-artefakter helt: en stroke täcks annars av grannpolygoner
#  som ritas senare, och i samverkansläge skulle varje kommun-form
#  få sin egen kant. Den yttre svarta områdesgränsen ritas i stället
#  som ett eget, permanent lager från den sammanslagna geometrin.
# ============================================================

.geo_cache <- new.env(parent = emptyenv())

.las_kommun_geometri <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) return(NULL)
  geo <- tryCatch(
    dplyr::tbl(shiny_uppkoppling_las("geodata"),
               dbplyr::in_schema("karta", "kommun_scb")) |>
      dplyr::filter(str_sub(knkod, 1, 2) == "20") |>
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
    # Sammanslagen geometri per område (för de yttre gränserna).
    omr <- geo |>
      dplyr::group_by(samverkansomrade) |>
      dplyr::summarise(.groups = "drop")

    # Tooltip: område fetstilt överst, kommun under.
    geo$tooltip <- paste0(
      "<b style='font-size:1.05em'>", geo$samverkansomrade, "</b><br/>",
      geo$kommun)

    g <- ggplot2::ggplot() +
      # Lager 1: kommun-ytor färgade per område (interaktiva). Hover/val mörknar
      # hela området eftersom alla kommuner delar data_id = samverkansomrade.
      ggiraph::geom_sf_interactive(
        data = geo,
        ggplot2::aes(fill = samverkansomrade, tooltip = tooltip,
                     data_id = samverkansomrade),
        color = NA) +
      # Lager 2: svaga vita kommungränser (inuti området), ej interaktiva.
      ggplot2::geom_sf(data = geo, fill = NA, color = "white", linewidth = 0.3) +
      # Lager 3: yttre områdesgränser, svarta, permanenta, ej interaktiva.
      ggplot2::geom_sf(data = omr, fill = NA, color = "#1a1a1a", linewidth = 0.7) +
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

  # Kommunläge: enfärgade kommuner, vita gränser. Hover/val mörknar kommunen.
  geo$tooltip <- paste0("<b>", geo$kommun, "</b>")

  g <- ggplot2::ggplot(geo) +
    ggiraph::geom_sf_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = kommkod),
      fill = KOMMUN_FYLL, color = "#ffffff", linewidth = 0.4) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(legend.position = "none",
                   plot.margin = ggplot2::margin(2, 2, 2, 2))
  gemensam_girafe(g)
}
