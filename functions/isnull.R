isnull = function(x,reemplazo,Nulos = c(NA,Inf,-Inf,NULL,NaN)){
  ifelse(x %in% Nulos,reemplazo,x)
}