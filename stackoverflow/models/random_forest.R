source("init.R")
safeLibrary(rpart)
safeLibrary(h2o)

dataset = readRDS("data/clean/03_ready.rds")

# Random Forest
srv = h2o.init(nthreads = -1)
h2o.aux = mutate_if(dataset,is.ordered,as.character) %>% 
  mutate_if(is.character, as.factor)
dataset.h2o = as.h2o(h2o.aux,destination_frame = "dataset_train")
salary_fit = h2o.randomForest(y = "ConvertedSalary",
                              x = setdiff(colnames(dataset),"ConvertedSalary"),
                              training_frame = dataset.h2o,
                              model_id = "salary_balanced",
                              ntrees = 1000)
#MSE 24418.3
h2o.saveModel(salary_fit,'data/models/',force = T)
saveRDS(data.frame(h2o.varimp(salary_fit)),'data/models/importance_salary.RDS')


h2o.shutdown(prompt = F)
