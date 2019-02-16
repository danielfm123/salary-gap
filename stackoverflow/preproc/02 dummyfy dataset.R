source("init.R")

dataset = readRDS("data/clean/01_categorisado.rds")

listsToDummy = function(x, minimo = 0){
  # print(x)
  niveles = table(unnest(x))/nrow(x)
  niveles = niveles[niveles > minimo]

  dummyfied = map_dfc(names(niveles),function(n){
    mutate_all(x,~map_dbl(.,~n %in% .))
      
  }) %>% mutate_all(~replace_na(.,0))

  colnames(dummyfied) = make.names(str_remove_all(names(niveles),"[[:punct:]]"))
  colnames(dummyfied) = paste(colnames(x),colnames(dummyfied),sep='_')
  return(dummyfied)
}

cols_dummy = map( multiple_opt, function(name){
  print(name)
  col = dataset[,name]
  future({listsToDummy(col)})
}) 
cols_dummy = map_dfc(cols_dummy,value)

dataset = select(dataset, -one_of(multiple_opt))
dataset = bind_cols(dataset,cols_dummy)

saveRDS(dataset,"data/clean/02_dummy.rds")
