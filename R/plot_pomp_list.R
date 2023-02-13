plot_pomp_list = function(pomp_list){
  pomp_names = names(pomp_list)
  par(mfrow = c(3,1))
  for(i in seq_along(pomp_list)){
    plot(y = obs(pomp_list[[i]]), x = time(pomp_list[[i]]), type = "l", 
         xaxt ='n', ylab = "obs", xlab = "", main = pomp_names[[i]])
    abline(v = time(pomp_list[[i]])[eff.sample.size(pomp_list[[i]]) == 0],
           col = "red", lwd = 1/4)
    plot(y = eff.sample.size(pomp_list[[i]]), x = time(pomp_list[[i]]),
         type = "l", xaxt ='n', ylab = "ESS", xlab = "")
    plot(y = cond.logLik(pomp_list[[i]]), x = time(pomp_list[[i]]), type = "l",
         ylab = "cond.logLik", xlab = "time")
  }
  par(mfrow = c(1,1))
}