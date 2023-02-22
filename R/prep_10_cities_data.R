prep_10_cities_data = function(){
  measles = utils::read.csv("input/ten_cities_measles/measles.csv")
  demog = utils::read.csv("input/ten_cities_measles/birth_pop.csv")
  start_params = read.csv("input/ten_cities_measles/start_params.csv")

  # measles[(8358-3):(8358+3),]
  # A tibble: 7 × 3
  # town    date       cases
  # <chr>   <date>     <dbl>
  # 1 Hornsey 1957-01-27    51
  # 2 Hornsey 1957-02-03    70
  # 3 Hornsey 1957-02-10    88
  # 4 Hornsey 1957-02-17     8
  # 5 Hornsey 1957-02-24    99
  # 6 Hornsey 1957-03-03    64
  # 7 Hornsey 1957-03-10    87
  measles[8358,"cases"] = NA

  list(measles = measles, demog = demog, mles = start_params)
}
