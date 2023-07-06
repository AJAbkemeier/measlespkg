spatPomp_example = function(){
  x = make_spatMeaslesPomp(
    data = clean_twentycities(),
    model = model_mechanics_007(U = 20, shared_names = "g")
  )
  pomp::coef(x) = mod_07_coef
  x
}
