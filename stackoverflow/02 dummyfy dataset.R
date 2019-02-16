source("init.R")

dataset = readRDS("data/clean/01_categorisado.rds")

listsToDummy = function(x){
  # print(x)
  aux = map(x,function(y) {
    y = str_remove_all(stry,"[[:punct:]]")
    make.names(y)
  })
  aux = map_dfr(aux,function(y){
    data.frame(matrix(1,ncol = length(y), nrow = 1,dimnames = list("1",y)) )
  })
  aux = map_dfc(aux,replace_na,0)
  
  return(aux)
}

cols_dummy = map( multiple_opt, function(name){
  print(name)
  col = dataset[,name]
  future({
    dummy = listsToDummy(col)
    colnames(dummy) = paste0(name,"_",colnames(dummy))
    dummy
  })
}) 
cols_dummy = map_dfc(cols_dummy,value)

dataset = select(dataset, -one_of(multiple_opt))
dataset = cbind(dataset,cols_dummy)

dummyfier = dummyVars( ~ ., dataset)
dataset_train = predict(dummyfier,dataset)
colnames(dataset_train) = make.names(colnames(dataset)) #corregir nombres de columnas

dataset_train = as.data.frame(dataset_train)

saveRDS(dataset,"data/clean/02_dummy.rds")
