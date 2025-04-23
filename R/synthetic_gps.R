#' Synthetic EucFACE GPS Reference Data
#'
#' A dataset containing synthetic EucFACE spatial and vegetation information used
#' to demonstrate sampling functionality in the `NicEuc` package.
#'
#' This includes plot metadata (Ring, Plot, Cell), geospatial coordinates,
#' and species with photosynthetic pathway annotations.
#'
#' @format A data frame with N rows and 7 variables:
#' \describe{
#'   \item{Ring}{Ring identifier (factor or character)}
#'   \item{Plot}{Plot identifier (factor or character)}
#'   \item{Cell}{Cell ID within plot (factor or character)}
#'   \item{Latitude}{Latitude in decimal degrees (numeric)}
#'   \item{Longitude}{Longitude in decimal degrees (numeric)}
#'   \item{Species}{Species name (character)}
#'   \item{Photosynthetic_Pathway}{Photosynthetic type, e.g., "C3" or "C4"}
#' }
#' @usage data(synthetic_gps)
#' @source Internally generated for NicEuc examples
"synthetic_gps"
