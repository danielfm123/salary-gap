library(pROC)
library(ROCR)


#Registro a considerar
dataset = readRDS("data/clean/03_ready.rds")


df <- dataset %>%
  filter(ConvertedSalary>10000, ConvertedSalary<300000,SalaryType=='Yearly',YearsCodingProf>0,Country=='United States',Employment =='Employed full-time') %>%
  select(-c(Country,Salary,Currency,Student,Employment,SalaryType,CurrencySymbol,OperatingSystem,AdBlocker,AdBlockerDisable,AdsActions, StackOverflowHasAccount,StackOverflowJobs,StackOverflowDevStory,EducationParents )) %>%
  mutate_if(is.character,as.factor) %>%
  filter(!is.na(.$Male))  %>%
  select(-one_of(names(which(colSums(is.na(.)) > 300)))) %>% mutate(Male = as.factor(Male))


df <- df[complete.cases(df),]
table(df[complete.cases(df),'Male'])


#A1: Mostrar diferencia entre sueldo mean y std agrupados entre hombres y mujeres por los años de experencia programando
A1 <- df %>% 
  group_by(Male,YearsCodingProf) %>%
  summarise(salario_promedio = mean(ConvertedSalary,na.rm=T),salario_std = sd(ConvertedSalary,na.rm=T))

A1 %>%
  ggplot(aes(x=YearsCodingProf, y=salario_promedio,group = Male)) +
  geom_line(aes(color=Male)) + geom_point(aes(color=Male),position=position_dodge(0.5)) 

A1 %>%
  ggplot(aes(x=YearsCodingProf, y=salario_promedio,group = Male)) +
  geom_line(aes(color=Male)) + geom_point(aes(color=Male),position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=salario_promedio-salario_std, ymax=salario_promedio+salario_std), colour="black", width=0.01, position=position_dodge(0.5))

#Crear el dataset para el training y testing de los Modelos
set.seed(29)
index <- createDataPartition(df$Male,p= 0.8, list = FALSE)
train_data <- df[index,]
test_data <- df[-index,]

table(train_data$Male)
table(test_data$Male)

sum(is.na(train_data))

## Buscar cuales son los atributos principales que determinan si un individio es Hombre o no

#Modelo 1: Aplicar un randomForest con parametros Default
set.seed(29)
modelo_1 <- randomForest(formula = Male ~ .,
                         data = train_data,
                         proximity=TRUE)
print(modelo_1)
varImpPlot(modelo_1 , sort=T, n.var=10, main='Top 10 Atributos')
partialPlot(modelo_1, train_data, YearsCoding, TRUE )
partialPlot(modelo_1, train_data, AssessJob9, TRUE )
partialPlot(modelo_1, train_data, HopeFiveYears, FALSE )
MDSplot(modelo_1,df$Male)

pred_modelo_1_prob <- predict(object = modelo_1, newdata = test_data, type='prob')
pred_modelo_1_class <- predict(object = modelo_1, newdata = test_data, type='class')

confusionMatrix(data = pred_modelo_1_class,
                reference = test_data$Male)

roc_obj <- roc(test_data$Male, pred_modelo_1_prob[,'TRUE'])
auc(roc_obj)


#Modelo 2: Strateficado
set.seed(29)
modelo_2 <- randomForest(formula = Male ~ .,
                         data=train_data,
                         ntree=4000,
                         sampsize=c(50,50),
                         strata=train_data$Male,
                         proximity=TRUE)
print(modelo_2)
varImpPlot(modelo_2 , sort=T, n.var=10, main='Top 10 Atributos')
partialPlot(modelo_2, train_data, YearsCoding, TRUE )
partialPlot(modelo_2, train_data, AssessJob9, TRUE )
partialPlot(modelo_2, train_data, HopeFiveYears, FALSE )
MDSplot(modelo_2,df$Male)

pred_modelo_2_prob <- predict(object = modelo_2, newdata = test_data,type='prob')
pred_modelo_2_class <- predict(object = modelo_2, newdata = test_data,type='class')

confusionMatrix(data = pred_modelo_2_class,
                reference = test_data$Male)

roc_obj <- roc(test_data$Male, pred_modelo_2_prob[,'TRUE'])
auc(roc_obj)


#ROC
preds_list <- list(pred_modelo_1_prob[,'TRUE'],pred_modelo_2_prob[,'TRUE'])
m <- length(preds_list)
actual_list <- rep(list(test_data$Male),m)
pred <- prediction(preds_list,actual_list)
rocs <- performance(pred,"tpr","fpr")

plot(rocs, col = as.list(1:m), main='ROC')
legend(x='bottomright',
       legend = c('Modelo 1','Modelo 2'),
       fill = 1:m)











