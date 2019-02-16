source("init.R")

dataset = readRDS("data/clean/03_ready.rds")

dataset %>% 
  group_by(Male) %>% 
  summarise(salario = mean(ConvertedSalary,na.rm=T),
            number = n())

aux = dataset %>% 
  group_by(Male,YearsCodingProf, OperatingSystem) %>% 
  summarise(salario = mean(ConvertedSalary,na.rm=T)) %>% 
  filter(!is.na(Male)) %>% 
  ungroup() %>% 
  spread(Male,salario)


# srv = h2o.init(nthreads = 0)
# h2o.aux = mutate_if(dataset,is.ordered,as.character)
# dataset.h2o = as.h2o(h2o.aux,destination_frame = "dataset_train")
# forest.h2o = h2o.randomForest(y = "ConvertedSalary",
#                               training_frame = dataset.h2o,
#                               model_id = "model",
#                               ntrees = 100)
# forest.h2o = h2o.randomForest(y = "Male",
#                               training_frame = dataset.h2o,
#                               model_id = "model",
#                               ntrees = 100)
# importance = as.data.frame(h2o.varimp(forest.h2o))
# View(importance)
# h2o.shutdown(prompt = F)

fitTree = rpart(ConvertedSalary ~ .,dataset)
fancyRpartPlot(fitTree)

fitTree = rpart(Male ~ .,dataset)
fancyRpartPlot(fitTree)

ranger(ConvertedSalary ~ .,dataset)

arbol = rpart(ConvertedSalary ~ ., dataset)
arbol = rpart(Male ~ ., dataset,cp=0.001)
fancyRpartPlot(arbol)
