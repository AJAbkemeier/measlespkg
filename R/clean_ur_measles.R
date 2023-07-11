#' Clean the `ur_measles` data
#'
#' @return Returns `ur_measles` but with tweaked observations.
#' @export
#'
#' @examples
#' clean_ur_measles()
clean_ur_measles = function(){
  urm = measlespkg::ur_measles

  #            unit       date cases
  # 13770 Liverpool 1955-11-04    10
  # 13771 Liverpool 1955-11-11    25
  # 13772 Liverpool 1955-11-18   116
  # 13773 Liverpool 1955-11-25    17
  # 13774 Liverpool 1955-12-02    18

  urm$measles[
    urm$measles$unit == "Liverpool" &
      urm$measles$date == "1955-11-18",
    "cases"
  ] = NA

  # 13950 Liverpool 1959-04-17   143
  # 13951 Liverpool 1959-04-24   115
  # 13952 Liverpool 1959-05-01   450
  # 13953 Liverpool 1959-05-08    96
  # 13954 Liverpool 1959-05-15   157

  urm$measles[
    urm$measles$unit == "Liverpool" &
      urm$measles$date == "1959-05-01",
    "cases"
  ] = NA

  # 19552 Nottingham 1961-08-18     6
  # 19553 Nottingham 1961-08-25     7
  # 19554 Nottingham 1961-09-01    66
  # 19555 Nottingham 1961-09-08     8
  # 19556 Nottingham 1961-09-15     7

  urm$measles[
    urm$measles$unit == "Nottingham" &
      urm$measles$date == "1961-09-01",
    "cases"
  ] = NA

  # London 1955-08-12   124
  # London 1955-08-19    82
  # London 1955-08-26     0
  # London 1955-09-02    58
  # London 1955-09-09    38

  urm$measles[
    urm$measles$unit == "London" &
      urm$measles$date == "1955-08-26",
    "cases"
  ] = NA
  # The value 76 was used in He10, but it seems safer to use NA.

  # Sheffield 1961-05-05   266
  # Sheffield 1961-05-12   346
  # Sheffield 1961-05-19     0
  # Sheffield 1961-05-26   314
  # Sheffield 1961-06-02   297

  urm$measles[
    urm$measles$unit == "Sheffield" &
      urm$measles$date == "1961-05-19",
    "cases"
  ] = NA

  # 1 Hornsey 1957-01-27    51
  # 2 Hornsey 1957-02-03    70
  # 3 Hornsey 1957-02-10    88
  # 4 Hornsey 1957-02-17     8
  # 5 Hornsey 1957-02-24    99
  # 6 Hornsey 1957-03-03    64
  # 7 Hornsey 1957-03-10    87

  urm$measles[
    urm$measles$unit == "Hornsey" &
      urm$measles$date == "1957-02-17",
    "cases"
  ] = NA

  # 1 Worthing 1963-09-01     0
  # 2 Worthing 1963-09-08     0
  # 3 Worthing 1963-09-15     0
  # 4 Worthing 1963-09-22    28
  # 5 Worthing 1963-09-29     4
  # 6 Worthing 1963-10-06     2
  # 7 Worthing 1963-10-13     1

  urm$measles[
    urm$measles$unit == "Worthing" &
      urm$measles$date == "1963-09-22",
    "cases"
  ] = NA

  urm
}
