get_cmd_args = function(){
  cmd_args = commandArgs(trailingOnly = TRUE)
  if(length(cmd_args)==0){
    print("No arguments supplied.")
  }else{
    for(i in 1:length(cmd_args)){
      eval(parse(text=cmd_args[[i]]))
    }
    rm(i)
    rm(cmd_args)
    sapply(ls(environment()), simplify = FALSE, USE.NAMES = TRUE, function(x)
      get(x)
    )
  }
}
