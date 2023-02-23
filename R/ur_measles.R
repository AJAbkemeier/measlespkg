#' Weekly reported measles data for 1422 locations in the UK
#'
#' @format ## `ur_measles`
#' A list of 3 tibbles. Unit names ending in `.RD` are for rural areas; other
#' unit names are for urban areas. NOTE: not all units have coordinates.
#'
#' ## `ur_measles$measles`:
#' \describe{
#'   \item{unit}{City name}
#'   \item{data}{Date of observation}
#'   \item{cases}{Number of measles cases reported during the week}
#' }
#' ## `ur_measles$demog`:
#' \describe{
#'   \item{unit}{City name}
#'   \item{year}{Year that demography was recorded}
#'   \item{pop}{Population}
#'   \item{births}{Births}
#' }
#' ## `ur_measles$coord`:
#' \describe{
#'   \item{unit}{City name}
#'   \item{long}{Longitude of city}
#'   \item{lat}{Latitude of city}
#' }
#'
"ur_measles"
