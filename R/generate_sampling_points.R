#' @title Sample EucFACE Cells with Maximum Distance and Stratified by Photosynthetic Pathway
#'
#' @description This function selects spatially distributed and photosynthetically balanced samples
#' from EucFACE vegetation data. It performs distance-constrained random sampling,
#' ensuring each plot gets an equal number of C3 and C4 species, and computes spatial
#' autocorrelation using Moran's I. Output includes plots, maps, and optionally a CSV file.
#'
#' @param gps_ref A data frame with columns: Ring, Plot, Latitude, Longitude, Species, and Photosynthetic_Pathway.
#' @param samples_per_plot Number of samples to select per plot (default: 6).
#' @param min_distance Minimum allowed distance (in meters) between samples (default: 0.4).
#' @param target_species Optional character vector of species names to filter (e.g., c("centella_asiatica")).
#' @param export_csv Logical. If TRUE, exports sampled data to CSV (default: FALSE).
#' @param filename Output filename for CSV if `export_csv = TRUE` (default: "eucface_samples.csv").
#' @param color_palette Optional named vector of colors to use for species. If NULL, a default palette is generated.
#'
#' @return A list with:
#' \describe{
#'   \item{samples}{A `data.frame` of sampled observations.}
#'   \item{plot}{A `ggplot2` object showing faceted sampling layout.}
#'   \item{map}{A `leaflet` map with sampling points.}
#'   \item{moran_test}{A list of Moran's I test results per plot.}
#' }
#'
#' @details
#' Assumes the input coordinate reference system is WGS84 (EPSG:4326) and transforms to UTM zone 56S (EPSG:32756).
#' Each plot is sampled to obtain half C3 and half C4 species, if available.
#'
#' @examples
#' data(synthetic_gps)
#' result <- generate_sampling_points(
#'   gps_ref = synthetic_gps,
#'   samples_per_plot = 6,
#'   min_distance = 0.4,
#'   target_species = c("centella_asiatica", "fimbristylis_dichotoma")
#' )
#' print(result$plot)
#' result$map
#' head(result$samples)
#'
#' @importFrom dplyr mutate filter group_by group_modify ungroup bind_rows bind_cols group_map
#' @importFrom sf st_as_sf st_transform st_coordinates st_drop_geometry
#' @importFrom ggplot2 ggplot aes geom_point facet_wrap coord_fixed scale_color_manual theme_minimal labs
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom spdep dnearneigh nb2listw moran.test
#' @importFrom grDevices rainbow hcl.colors
#' @importFrom utils write.csv
#' @export
generate_sampling_points <- function(
    gps_ref,
    samples_per_plot = 6,
    min_distance = 0.4,
    target_species = NULL,
    export_csv = FALSE,
    filename = "eucface_samples.csv",
    color_palette = NULL
) {
  gps_ref <- dplyr::mutate(
    gps_ref,
    Species = gsub(" ", "_", tolower(Species)),
    Photosynthetic_Pathway = tolower(Photosynthetic_Pathway)
  )

  if (!is.null(target_species)) {
    target_species <- gsub(" ", "_", tolower(target_species))
    gps_ref <- dplyr::filter(gps_ref, Species %in% target_species)
  }

  sf_utm <- sf::st_as_sf(gps_ref, coords = c("Longitude", "Latitude"), crs = 4326) |>
    sf::st_transform(crs = 32756)

  coords <- sf::st_coordinates(sf_utm)
  gps_ref <- sf::st_drop_geometry(sf_utm) |>
    dplyr::mutate(utm_x = coords[, 1], utm_y = coords[, 2])

  spatial_filter <- function(df, n, min_distance) {
    sampled <- data.frame()
    remaining <- df
    while (nrow(sampled) < n && nrow(remaining) > 0) {
      idx <- sample.int(nrow(remaining), 1)
      chosen <- remaining[idx, , drop = FALSE]
      sampled <- dplyr::bind_rows(sampled, chosen)
      remaining <- dplyr::filter(
        remaining,
        sqrt((utm_x - chosen$utm_x)^2 + (utm_y - chosen$utm_y)^2) > min_distance
      )
    }
    sampled
  }

  stratified_sample <- function(df, n) {
    c3 <- dplyr::filter(df, Photosynthetic_Pathway == "c3")
    c4 <- dplyr::filter(df, Photosynthetic_Pathway == "c4")
    if (nrow(c3) >= 3 && nrow(c4) >= 3) {
      dplyr::bind_rows(
        spatial_filter(c3, 3, min_distance),
        spatial_filter(c4, 3, min_distance)
      )
    } else {
      warning("Insufficient C3 or C4 species in plot. Sampling without stratification.")
      spatial_filter(df, n, min_distance)
    }
  }

  sampled <- gps_ref |>
    dplyr::group_by(Ring, Plot) |>
    dplyr::group_modify(~ stratified_sample(.x, samples_per_plot)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Ring_Plot = paste(Ring, Plot, sep = "_"))

  moran_result <- sampled |>
    dplyr::group_by(Plot) |>
    dplyr::group_map(~ {
      if (nrow(.x) < 3) return(NA)
      coords <- .x[, c("utm_x", "utm_y")]
      nb <- tryCatch(spdep::dnearneigh(coords, 0, min_distance * 3), error = function(e) NULL)
      if (is.null(nb) || all(sapply(nb, length) == 0)) return(NA)
      lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
      tryCatch(spdep::moran.test(.x$utm_x, lw, zero.policy = TRUE), error = function(e) NA)
    })

  names(moran_result) <- unique(sampled$Plot)

  color_map <- grDevices::rainbow(length(unique(sampled$Species)))
  names(color_map) <- unique(sampled$Species)

  # Construct dynamic labels
  title_text <- "EucFACE Sampling Grid by Ring & Plot"

  subtitle_text <- paste0(
    "Target species: ",
    if (!is.null(target_species)) {
      paste(target_species, collapse = ", ")
    } else {
      "All species"
    },
    " | ",
    samples_per_plot, " samples per plot",
    " (min distance: ", min_distance, " m)"
  )

  # Build plot
  n_facets <- length(unique(sampled$Ring_Plot))
  plot_cols <- 4
  plot_rows <- ceiling(n_facets / plot_cols)

  n_facets <- length(unique(sampled$Ring_Plot))

  # Dynamically determine layout
  plot_cols <- if (n_facets > 12) 6 else if (n_facets > 8) 4 else 3
  point_size <- if (n_facets > 20) 2 else if (n_facets > 12) 2.8 else 3.5
  strip_text_size <- if (n_facets > 12) 7 else 9
  title_size <- if (n_facets > 12) 12 else 14
  subtitle_size <- if (n_facets > 12) 9 else 11

  # Define species and color mapping
  species_levels <- sort(unique(sampled$Species))

  if (is.null(color_palette)) {
    color_map <- grDevices::hcl.colors(
      n = length(species_levels),
      palette = "Dynamic"
    )
  } else {
    if (length(color_palette) < length(species_levels)) {
      stop("Provided color_palette is shorter than the number of species.")
    }
    color_map <- color_palette
  }
  names(color_map) <- species_levels

  plot_out <- ggplot2::ggplot(sampled, ggplot2::aes(x = utm_x, y = utm_y, fill = Species)) +
    ggplot2::geom_point(shape = 21, color = "black", size = point_size, stroke = 0.3) +
    ggplot2::facet_wrap(~Ring_Plot, scales = "fixed", ncol = plot_cols) +
    ggplot2::scale_fill_manual(values = color_map) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = strip_text_size),
      plot.title = ggplot2::element_text(face = "bold", size = title_size, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = subtitle_size, hjust = 0.5),
      axis.title = ggplot2::element_text(size = 9, face = "bold"),
      axis.text = ggplot2::element_text(size = 8),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold", size = 9),
      legend.text = ggplot2::element_text(size = 8),
      plot.margin = ggplot2::margin(6, 6, 6, 6)
    ) +
    ggplot2::labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "UTM Easting",
      y = "UTM Northing",
      fill = "Species"
    )

  sampled_sf <- sf::st_as_sf(sampled, coords = c("utm_x", "utm_y"), crs = 32756)
  sampled_wgs <- sf::st_transform(sampled_sf, 4326)
  coords_wgs <- sf::st_coordinates(sampled_wgs)
  sampled$lon <- coords_wgs[, 1]
  sampled$lat <- coords_wgs[, 2]

  map_out <- leaflet::leaflet(sampled) |>
    leaflet::addTiles() |>
    leaflet::addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      color = ~unname(color_map[Species]),
      radius = 5,
      label = ~paste("Ring:", Ring, "<br>Plot:", Plot, "<br>Species:", Species)
    )

  if (export_csv) {
    utils::write.csv(sampled, filename, row.names = FALSE)
  }

  return(list(
    samples = sampled,
    plot = plot_out,
    map = map_out,
    moran_test = moran_result
  ))
}



# UsageExample
# Example synthetic data
# set.seed(42)
# example_gps <- expand.grid(
#   Ring = paste0("Ring", 1:6),
#   Plot = paste0("Plot", 1:4),
#   Cell = 1:25
# )
#
# example_gps$Latitude <- runif(nrow(example_gps), -33.6177, -33.6175)
# example_gps$Longitude <- runif(nrow(example_gps), 150.7413, 150.7416)
#
# species_pool <- data.frame(
#   Species = c("centella_asiatica", "fimbristylis_dichotoma"),
#   Photosynthetic_Pathway = c("C3", "C4")
# )
#
# gps_ref <- dplyr::bind_cols(
#   example_gps,
#   species_pool[sample(1:2, nrow(example_gps), replace = TRUE), ]
# )
#
# # Run
# result <- generate_sampling_points(
#   gps_ref = gps_ref,
#   samples_per_plot = 6,
#   min_distance = 0.4,
#   target_species = c("centella_asiatica", "fimbristylis_dichotoma"),
#   export_csv = F,
#   filename = "eucface_samples.csv"
# )
#
# # Outputs
# print(result$plot)
# result$map
# head(result$samples)
