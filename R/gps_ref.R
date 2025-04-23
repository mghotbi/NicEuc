#' @title EucFACE Vegetation Sampling Reference Data
#'
#' @description A real-world subset of the EucFACE vegetation dataset including spatial coordinates and plant species with annotated photosynthetic pathways.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{Ring}{Ring ID (e.g., "Ring1", "Ring2")}
#'   \item{Plot}{Plot ID within each Ring (e.g., "Plot1")}
#'   \item{Cell}{Sampling cell identifier}
#'   \item{Latitude}{Latitude in decimal degrees}
#'   \item{Longitude}{Longitude in decimal degrees}
#'   \item{Species}{Plant species name (e.g., "centella_asiatica")}
#'   \item{Photosynthetic_Pathway}{Photosynthetic classification, either "C3" or "C4"}
#' }
#'
#' @usage data(gps_ref)
#' @keywords datasets
#' @examples
#' data(gps_ref)
#' head(gps_ref)
"gps_ref"
