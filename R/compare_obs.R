compare_obs = function(pomp_list_1, pomp_list_2){
  pomp_names = names(pomp_list_1)
  par(mfrow = c(2,1), mai = c(1.02/2, 0.82, 0.82/5, 0.42))
  for(i in seq_along(pomp_list_1)){
    plot(y = obs(pomp_list_1[[i]]), x = time(pomp_list_1[[i]]), type = "l", 
         xaxt ='n', ylab = "obs", xlab = "", main = pomp_names[[i]])
    plot(y = obs(pomp_list_2[[i]]), x = time(pomp_list_2[[i]]), type = "l", 
         ylab = "obs", xlab = "time", main = "")
  }
  par(mfrow = c(1,1))
}