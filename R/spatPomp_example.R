spatPomp_example = function(){
  x = make_measlesPomp(
    model = model_mechanics_007(U = 20, shared_params = "g"),
    data = clean_twentycities()
  )
  pomp::coef(x) = mod_07_coef
  x
}
