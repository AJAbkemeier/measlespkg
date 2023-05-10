#' MLE's obtained by He after fitting the He10 model
#'
#' @format ## `He10_mles`
#' A 20 x 12 tibble. All columns after the first 2 are estimated parameters.
#'
#' \describe{
#'   \item{unit}{City name}
#'   \item{pop_1950}{Population size of the unit recorded for 1950, rounded}
#'   \item{alpha}{A mixing parameter}
#'   \item{sigmaSE}{Standard error of gamma white noise applied to rate of
#'   people transitioning from S to E}
#'   \item{R0}{Expected number of people infected by an individual in a
#'   population where everyone is susceptible}
#'   \item{sigma}{Rate at which people transition from E to I}
#'   \item{gamma}{Rate at which people transition from I to R}
#'   \item{amplitude}{Controls difference in rate at which people
#'   transition from S to E during school versus during vacations}
#'   \item{iota}{Mean number of infected individuals visiting the town at any
#'   time}
#'   \item{cohort}{Controls proportion of the year's susceptible individuals
#'   who are added on the 251st day of the year}
#'   \item{rho}{Probability that person transitioning from I to R is recorded}
#'   \item{psi}{Measurement overdispersion parameter}
#' }
#'
"He10_mles"
