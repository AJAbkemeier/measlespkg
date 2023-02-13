calc_geo_cooling = function(cooling_fraction_50, n = 1, m = 1, N = 730){
  cooling_fraction_50^((n-1+(m-1)*N)/(50*N))
}
