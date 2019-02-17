source("init.R")
safeLibrary(rpart)
safeLibrary(rattle)
safeLibrary(h2o)

dataset = readRDS("data/clean/03_ready.rds")

# Random Forest
srv = h2o.init(nthreads = -1)
h2o.aux = mutate_if(dataset,is.ordered,as.character)
dataset.h2o = as.h2o(h2o.aux,destination_frame = "dataset_train")
salary_fit = h2o.randomForest(y = "ConvertedSalary",
                              x = setdiff(colnames(dataset),"ConvertedSalary"),
                              training_frame = dataset.h2o,
                              model_id = "salary",
                              ntrees = 500)
h2o.saveModel(salary_fit,'data/models/')
saveRDS(data.frame(h2o.varimp(salary_fit)),'data/models/importance_salary.RDS')

gender_fit = h2o.randomForest(y = "Male",
                              x = setdiff(colnames(dataset),"Male"),
                              training_frame = dataset.h2o,
                              model_id = "gender",
                              ntrees = 500)
h2o.saveModel(gender_fit,'data/models/')
saveRDS(data.frame(h2o.varimp(gender_fit)),'data/models/importance_gender.RDS')

lasso_salary = h2o.glm(y = "ConvertedSalary",
                      x = setdiff(colnames(dataset),"ConvertedSalary"),
                      training_frame = dataset.h2o,
                      model_id = "lasso_salary",
                      lambda = 0,
                      remove_collinear_columns = TRUE,
                      compute_p_values = TRUE,
                      link = 'identity')
data.frame(h2o.varimp(lasso_salary))
p_value = arrange(data.frame(lasso_salary@model$coefficients_table),p_value)


lasso_gender = h2o.glm(y = "Male",
                       x = setdiff(colnames(dataset),"Male"),
                       training_frame = dataset.h2o,
                       model_id = "lasso_gender",
                       lambda = 0,
                       remove_collinear_columns = TRUE,
                       compute_p_values = TRUE,
                       family = 'binomial',
                       link = 'logit')
data.frame(h2o.varimp(lasso_gender))
data.frame(lasso_gender@model$coefficients_table)

h2o.shutdown(prompt = F)
