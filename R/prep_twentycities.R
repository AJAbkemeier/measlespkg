#' Perform minor cleanup on `twentycities` data
#'
#' @return Returns `twentycities` but with 3 observations set to NA.
#' @export
#'
#' @examples
#' clean_twentycities()
clean_twentycities = function(){
  tc = twentycities
  # data used for He et al 2010, following their decision
  # to remove 3 data points

  # > measles[13769+1:5,]
  #            town       date cases
  # 13770 Liverpool 1955-11-04    10
  # 13771 Liverpool 1955-11-11    25
  # 13772 Liverpool 1955-11-18   116
  # 13773 Liverpool 1955-11-25    17
  # 13774 Liverpool 1955-12-02    18

  tc$measles[13772, "cases"] <- NA

  # > measles[13949+1:5,]
  #            town       date cases
  # 13950 Liverpool 1959-04-17   143
  # 13951 Liverpool 1959-04-24   115
  # 13952 Liverpool 1959-05-01   450
  # 13953 Liverpool 1959-05-08    96
  # 13954 Liverpool 1959-05-15   157

  tc$measles[13952, "cases"] <- NA

  # > measles[19551+1:5,]
  #             town       date cases
  # 19552 Nottingham 1961-08-18     6
  # 19553 Nottingham 1961-08-25     7
  # 19554 Nottingham 1961-09-01    66
  # 19555 Nottingham 1961-09-08     8
  # 19556 Nottingham 1961-09-15     7

  tc$measles[19554, "cases"] <- NA
  tc
}
