source("init.R")
safeLibrary(rpart)
safeLibrary(h2o)

dataset = readRDS("data/clean/03_ready.rds")
importance = readRDS("data/models/importance_salary.RDS") %>% 
  mutate(cum_percentage = cumsum(percentage)) %>% 
  filter(cum_percentage < 0.90)

# Random Forest
srv = h2o.init(nthreads = -1)
h2o.aux = mutate_if(dataset,is.ordered,as.character) %>% 
  mutate_if(is.character, as.factor)
dataset.h2o = as.h2o(h2o.aux,destination_frame = "dataset_train")
salary_fit = h2o.automl(y = "ConvertedSalary",
                        x = importance$variable,
                        training_frame = dataset.h2o,
                        max_runtime_secs = 10*60,
                        verbosity = "info",
                        exclude_algos = "StackedEnsemble",
                        sort_metric = "RMSE")
as.data.frame(salary_fit@leaderboard)
# RMSE 23063.14
h2o.shutdown(prompt = F)
