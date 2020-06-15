source("init.R")
safeLibrary(rpart)
safeLibrary(h2o)

dataset = readRDS("data/clean/03_ready.rds")

# Random Forest
srv = h2o.init(nthreads = -1)
h2o.aux = mutate_if(dataset,is.ordered,as.character) %>% 
  mutate_if(is.character, as.factor)
dataset.h2o = as.h2o(h2o.aux,destination_frame = "dataset_train")
salary_fit = h2o.automl(y = "ConvertedSalary",
                        x = setdiff(colnames(dataset),"ConvertedSalary"),
                        training_frame = dataset.h2o,
                        max_runtime_secs = 15*60,
                        verbosity = "info",
                        exclude_algos = "StackedEnsemble",
                        sort_metric = "RMSE")

h2o.shutdown(prompt = F)
