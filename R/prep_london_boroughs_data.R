#' Title
#'
#' @param measles_filepath
#' @param demog_filepath
#' @param start_params_filepath
#'
#' @return
#' @export
#'
#' @examples
prep_london_boroughs_data = function(measles_filepath, demog_filepath,
start_params_filepath){
  london_measles = read.csv(measles_filepath) %>%
    select(borough, date, measles) %>%
    rename(town = borough, cases = measles) %>%
    mutate(date = as.Date(date)) %>%
    as.data.frame()
  # london_measles[(16097-3):(16097+3),]
  # town       date cases
  # 16094 Islington 1955-02-20    80
  # 16095 Islington 1955-02-27    97
  # 16096 Islington 1955-03-06    93
  # 16097 Islington 1955-03-13     0
  # 16098 Islington 1955-03-20   118
  # 16099 Islington 1955-03-27   187
  # 16100 Islington 1955-04-03   163
  london_measles[16097,"cases"] = NA

  london_demog = read.csv(demog_filepath) %>%
    rename(town = borough, pop = population) %>%
    select(town, year, pop, births) %>%
    as.data.frame()
  london_start_params = read.csv(start_params_filepath)

  list(measles = london_measles, demog = london_demog,
       mles = london_start_params)
}
