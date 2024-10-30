rm(list = ls())

library(dplyr)
library(caret)
library(skimr)
library(rio)
library(readr)
library(tidyr)
library(caret)
library(pROC)
library(factoextra)
library(fpc)
library(MLmetrics)
library(smotefamily)
library(xgboost)
library(adabag)
library(caret)   
# Data -----------------------------------------------------------

df_hogares_train <- read.csv("Datos/train_hogares.csv")
df_personas_train <- read.csv("Datos/train_personas.csv")

df_hogares_test <- read.csv("Datos/test_hogares.csv")
df_personas_test <- read.csv("Datos/test_personas.csv")


# Stepwise selection  -----------------------------------------------------

#Se va a hacer un dataset con todas las variables y seleccionar el mejor modelo con stepwise
#A partir de eso se va a aplicar CART. 

#Maximo grado educativo
df_personas_train_educ <- df_personas_train %>%
  group_by(id) %>% 
  filter(P6040 >= 10) %>%  #filtrar los niños
  reframe(maximo_grado_educ = as.factor(max(P6210, na.rm = TRUE))) 


#Mujeres madres de hojar
df_personas_train_mujeres_cab <- df_personas_train %>%
  group_by(id) %>% 
  mutate(hay_esposo = ifelse(P6050 == 2, 1, 0),
         esposo = cumsum(hay_esposo), 
         n_personas = cumsum(Orden), 
         mujer_soltera = ifelse(P6020 == 2 & P6050 == 1 & esposo == 0 & n_personas > 1, 1, 0)) %>% 
  filter(P6050 == 1) %>% 
  select(id, mujer_soltera)

#Ocupados
df_personas_train_ocup <- df_personas_train %>%
  mutate(cotiza = case_when(P6920 == 1 | P6920 == 3 ~ 1, TRUE ~ 0)) %>% 
  group_by(id) %>% 
  filter(Pet == 1) %>%  #filtrar los niños
  reframe(ocupados = sum(Oc, na.rm = TRUE), 
          pet = sum(Pet, na.rm = TRUE), 
          proporcion_ocupados = ocupados/pet,
          cotizantes = sum(cotiza), 
          proporcion_cotizantes = cotizantes/pet) %>% 
  mutate(proporcion_cotizantes = ifelse(proporcion_cotizantes > 1, 1, proporcion_cotizantes)) 


#Número de niños en el hogar
df_personas_train_niños <- df_personas_train %>%
  group_by(id) %>% 
  filter(P6040 <= 13) %>%  #filtrar los niños
  reframe(num_niños = n())


#Hacinamiento y tipo_vivienda
df_hogares_train_backward <- df_hogares_train %>% 
  left_join(df_personas_train_educ, by = "id") %>% 
  left_join(df_personas_train_ocup) %>%  
  left_join(df_personas_train_mujeres_cab) %>% 
  left_join(df_personas_train_niños) %>% 
  mutate(hacinamiento = Nper/P5010, 
         tipo_vivienda = as.factor(P5090), 
         num_niños = ifelse(is.na(num_niños), 0, num_niños), 
         proporcion_cotizantes = ifelse(is.na(proporcion_cotizantes), 0, proporcion_cotizantes),
         niños_madre = num_niños*mujer_soltera) %>% 
  select(Pobre, maximo_grado_educ, hacinamiento, proporcion_cotizantes,  num_niños, mujer_soltera, tipo_vivienda) 


backward_model <- lm(Pobre ~ ., data = df_hogares_train_backward)
backward_model1 <- step(backward_model, direction = "forward")

#En stepwise se decidieron todos los modelos




# Elastic net  ------------------------------------------------------------



df_hogares_train_elastic_net <-  df_hogares_train_backward %>% 
  mutate(Pobre =factor(Pobre,levels=c(0,1),labels=c("No","Yes")))
  
fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...))  ## Para 

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)


set.seed(123)

model_elastic_net1 <- train(Pobre~.,
                data=df_hogares_train_elastic_net,
                metric = "Accuracy",
                method = "glmnet",
                trControl = ctrl,
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.5),
                  lambda =10^seq(-1, -3, length = 10)
                )
                
)



## Test data ---------------------------------------------------------------


df_personas_test_educ <- df_personas_test %>%
  group_by(id) %>% 
  filter(P6040 >= 10) %>%  #filtrar los niños
  reframe(maximo_grado_educ = as.factor(max(P6210, na.rm = TRUE))) 


#Mujeres madres de hojar
df_personas_test_mujeres_cab <- df_personas_test %>%
  group_by(id) %>% 
  mutate(hay_esposo = ifelse(P6050 == 2, 1, 0),
         esposo = cumsum(hay_esposo), 
         n_personas = cumsum(Orden), 
         mujer_soltera = ifelse(P6020 == 2 & P6050 == 1 & esposo == 0 & n_personas > 1, 1, 0)) %>% 
  filter(P6050 == 1) %>% 
  select(id, mujer_soltera)

#Ocupados
df_personas_test_ocup <- df_personas_test %>%
  mutate(cotiza = case_when(P6920 == 1 | P6920 == 3 ~ 1, TRUE ~ 0)) %>% 
  group_by(id) %>% 
  filter(Pet == 1) %>%  #filtrar los niños
  reframe(ocupados = sum(Oc, na.rm = TRUE), 
          pet = sum(Pet, na.rm = TRUE), 
          proporcion_ocupados = ocupados/pet,
          cotizantes = sum(cotiza), 
          proporcion_cotizantes = cotizantes/pet) %>% 
  mutate(proporcion_cotizantes = ifelse(proporcion_cotizantes > 1, 1, proporcion_cotizantes)) 


#Número de niños en el hogar
df_personas_test_niños <- df_personas_test %>%
  group_by(id) %>% 
  filter(P6040 <= 13) %>%  #filtrar los niños
  reframe(num_niños = n())


#Hacinamiento y tipo_vivienda
df_hogares_test_variables <- df_hogares_test %>% 
  left_join(df_personas_test_educ, by = "id") %>% 
  left_join(df_personas_test_ocup) %>%  
  left_join(df_personas_test_mujeres_cab) %>% 
  left_join(df_personas_test_niños) %>% 
  mutate(hacinamiento = Nper/P5010, 
         tipo_vivienda = as.factor(P5090), 
         num_niños = ifelse(is.na(num_niños), 0, num_niños), 
         proporcion_cotizantes = ifelse(is.na(proporcion_cotizantes), 0, proporcion_cotizantes),
         niños_madre = num_niños*mujer_soltera) 


         
         
df_hogares_test_variables1 <- df_hogares_test_variables %>%     
  mutate(pobre =  predict(model_elastic_net1, newdata = df_hogares_test_variables1, type = "raw")) %>% 
  select(id, pobre) %>% 
  mutate(pobre = ifelse(pobre == "Yes", 1, 0)) 


write.table(df_hogares_test_variables1, file = "EN_accuracy_lambda_0.002_alpha_1.csv", row.names = FALSE, sep = ",")



# Tuning -------------------------------------------------------------

multiStats <- function(...) c(twoClassSummary(...), defaultSummary(...), prSummary(...))

ctrl_multiStats<- trainControl(method = "cv",
                               number = 5,
                               summaryFunction = multiStats,
                               classProbs = TRUE,
                               verbose=FALSE,
                               savePredictions = T)

lambda <- 10^seq(-1, -4, length = 50)
grid <- expand.grid("alpha" = seq(0,1,by=.2), lambda = lambda)


df_hogares_tuning <-  df_hogares_train_backward %>% 
  mutate(Pobre =factor(Pobre,levels=c(0,1),labels=c("No","Yes")))

set.seed(123)
model_tuning <- train(
  formula(Pobre ~ maximo_grado_educ + hacinamiento + proporcion_cotizantes +  num_niños + mujer_soltera + tipo_vivienda), 
  method = "glmnet",
  data = df_hogares_tuning,
  family = "binomial",
  tuneGrid = grid,
  preProcess = c("center", "scale"),
  trControl = ctrl_multiStats,
  metric = "Spec"
)


df_hogares_train_tuning <- df_hogares_tuning  %>% 
  mutate(prob = predict(model_tuning, newdata = df_hogares_tuning,
                        type = "raw"))

confusionMatrix(data = df_hogares_train_tuning$prob, reference = as.factor(df_hogares_train_tuning$Pobre))



df_hogares_test_variables2 <- df_hogares_test_variables %>%     
  mutate(pobre =  predict(model_tuning, newdata = df_hogares_test_variables, type = "raw")) %>% 
  select(id, pobre) %>% 
  mutate(pobre = ifelse(pobre == "Yes", 1, 0)) 


write.table(df_hogares_test_variables2, file = "mode_tuning_specificity.csv", row.names = FALSE, sep = ",")



# Smote  ------------------------------------------------------------------

df_hogares_train_smote <- df_hogares_train_backward %>% 
  mutate(maximo_grado_educ = as.numeric(maximo_grado_educ), 
         tipo_vivienda = as.numeric(tipo_vivienda), 
         Pobre =factor(Pobre,levels=c(0,1),labels=c("No","Yes")))
  
predictors<-colnames(df_hogares_train_smote  %>% select(maximo_grado_educ, hacinamiento, proporcion_cotizantes,  num_niños, mujer_soltera, tipo_vivienda))

smote_output <- SMOTE(X = df_hogares_train_smote[predictors],
                      target = df_hogares_train_smote$Pobre,
                      K=5)

X_train <- model.matrix(Pobre ~  maximo_grado_educ + hacinamiento + proporcion_cotizantes +  num_niños + mujer_soltera + tipo_vivienda, df_hogares_train_smote)
X_train <- X_train[, -1]
colnames(X_train)<-paste0("X",seq(1,6))
colnames(X_train)
X_train<-data.frame(X_train)

smote_data <- SMOTE(X = X_train, target = df_hogares_train_smote$Pobre, K = 5)

str(smote_data$data$class)

desempleado_logit_smote <- train(class ~.,
                                 data = smote_data$data, 
                                 method = "glm",
                                 family = "binomial",
                                 trControl = ctrl_multiStats,
                                 preProcess = c("center", "scale"), 
                                 metric = "Spec"
                        
)

df_hogares_test_variables_smote <- df_hogares_test_variables %>% 
  mutate(maximo_grado_educ = as.numeric(maximo_grado_educ), 
         tipo_vivienda = as.numeric(tipo_vivienda)) %>% 
  select(maximo_grado_educ, hacinamiento, proporcion_cotizantes,  num_niños, mujer_soltera,  tipo_vivienda)


colnames(df_hogares_test_variables_smote)<-paste0("X",seq(1,6))
colnames(df_hogares_test_variables_smote)
X_test <-data.frame(df_hogares_test_variables_smote)



df_hogares_test_variables3 <- df_hogares_test_variables  %>% 
  mutate(Pobre=predict(desempleado_logit_smote,newdata = X_test,
                                                           type = "raw")) %>% 
  mutate(Pobre = ifelse(Pobre == "Yes", 1, 0)) %>% 
  select(id, Pobre)



write.table(df_hogares_test_variables3, file = "smote.csv", row.names = FALSE, sep = ",")



# XG Boost ----------------------------------------------------------------



df_hogares_train_xgboostd <- df_hogares_train_backward %>% 
  mutate(Pobre =factor(Pobre,levels=c(0,1),labels=c("No","Yes")))

fitControl<-trainControl(method ="cv",
                         number=5)

grid_xbgoost <- expand.grid(nrounds = c(100,250),
                            max_depth = c(2,4), 
                            eta = c(0.01,0.05), 
                            gamma = c(0,1), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4,0.8))


set.seed(123)
Xgboost_tree <- train(Pobre ~ .,
                      data=df_hogares_train_xgboostd,
                      method = "xgbTree", 
                      trControl = fitControl,
                      tuneGrid=grid_xbgoost
)        



df_hogares_test_variables4 <- df_hogares_test_variables  %>% 
  mutate(Pobre=predict(Xgboost_tree,newdata = df_hogares_test_variables,
                       type = "raw")) %>% 
  mutate(Pobre = ifelse(Pobre == "Yes", 1, 0)) %>% 
  select(id, Pobre)



write.table(df_hogares_test_variables4, file = "xgboost.csv", row.names = FALSE, sep = ",")



# ADA boosting  -----------------------------------------------------------


df_hogares_train_adaboost <- df_hogares_train_backward %>% 
  mutate(Pobre =factor(Pobre,levels=c(0,1),labels=c("No","Yes"))) %>% 
  select(Pobre, maximo_grado_educ, num_niños, proporcion_cotizantes)

grid_gbm<-expand.grid(n.trees=c(200,300,500),
                      interaction.depth=c(4,6),
                      shrinkage=c(0.001,0.01),
                      n.minobsinnode = c(10,30))


model_gradient_boost <- train(Pobre ~., 
                        trControl = fitControl,
                        method = "gbm",
                        tuneGrid=grid_gbm,
                        data = df_hogares_train_xgboostd, 
                        verbose = FALSE
)  


df_hogares_test_variables5 <- df_hogares_test_variables  %>% 
  mutate(Pobre=predict(model_gradient_boost,newdata = df_hogares_test_variables,
                       type = "raw")) %>% 
  mutate(Pobre = ifelse(Pobre == "Yes", 1, 0)) %>% 
  select(id, Pobre)



write.table(df_hogares_test_variables5, file = "adaboost.csv", row.names = FALSE, sep = ",")




# Complex trees -----------------------------------------------------------

complex_tree <- rpart(Pobre ~ ., 
                      data    = df_hogares_train_adaboost,
                      method = "class",
                      cp = 0  
)



df_hogares_complex_tree <- df_hogares_train_adaboost  %>% 
  mutate(Pobre = ifelse(Pobre == "Yes", 1, 0)) 


pred_prob <- predict(complex_tree, newdata = df_hogares_complex_tree, type = "prob")    

aucval_arbol <- Metrics::auc(actual = df_hogares_complex_tree$Pobre,predicted = pred_prob[,2]) 
aucval_arbol



pred_test <- predict(complex_tree, newdata = df_hogares_test_variables, type = "prob")    
pred_test <- pred_test[,2]

df_hogares_test_variables5 <- df_hogares_test_variables  %>% 
  cbind(pred_test) %>% 
  mutate(Pobre = ifelse(pred_test > aucval_arbol, 1, 0)) %>% 
  select(id, Pobre)





write.table(df_hogares_test_variables5, file = "complex_tree.csv", row.names = FALSE, sep = ",")




# XG boosting y SMOTE -----------------------------------------------------

df_hogares_train_intento3 <- df_hogares_train %>% 
  left_join(df_personas_train_educ, by = "id") %>% 
  left_join(df_personas_train_ocup) %>%  
  left_join(df_personas_train_mujeres_cab) %>% 
  left_join(df_personas_train_niños) %>% 
  mutate(hacinamiento = Nper/P5010, 
         tipo_vivienda = as.factor(P5090), 
         num_niños = ifelse(is.na(num_niños), 0, num_niños), 
         proporcion_cotizantes = ifelse(is.na(proporcion_cotizantes), 0, proporcion_cotizantes),
         niños_madre = num_niños*mujer_soltera, 
         maximo_grado_educ = as.numeric(maximo_grado_educ)) %>% 
  select(Pobre, Nper, proporcion_cotizantes, maximo_grado_educ, num_niños) 



X_train <- model.matrix(Pobre ~  maximo_grado_educ + Nper + proporcion_cotizantes + num_niños + niños_madre + mujer_soltera, df_hogares_train_intento3)
X_train <- X_train[, -1]
colnames(X_train)<-paste0("X",seq(1,6))
colnames(X_train)
X_train<-data.frame(X_train)

smote_data <- SMOTE(X = X_train, target = df_hogares_train_smote$Pobre, K = 5)
smote_data_vars <- data.frame(smote_data$data)

df_hogares_test_variables_smote <- df_hogares_test_variables %>% 
  mutate(maximo_grado_educ = as.numeric(maximo_grado_educ), 
         niños_madre = num_niños*mujer_soltera, 
         tipo_vivienda = as.numeric(tipo_vivienda)) %>% 
  select(maximo_grado_educ, Nper, proporcion_cotizantes, num_niños, niños_madre, mujer_soltera)


colnames(df_hogares_test_variables_smote)<-paste0("X",seq(1,6))
colnames(df_hogares_test_variables_smote)
X_test <-data.frame(df_hogares_test_variables_smote)





fitControl<-trainControl(method ="cv",
                         number=5)

grid_xbgoost <- expand.grid(nrounds = c(100,200),
                            max_depth = c(2,4), 
                            eta = c(0.01,0.05), 
                            gamma = c(0,1), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4,0.8))


set.seed(123)
Xgboost_smote_tree <- train(class ~ .,
                      data=smote_data_vars,
                      method = "xgbTree", 
                      trControl = fitControl,
                      tuneGrid=grid_xbgoost
)        



df_hogares_test_variables9 <- df_hogares_test_variables  %>% 
  mutate(Pobre=predict(Xgboost_smote_tree, newdata = X_test,
                       type = "raw")) %>% 
  mutate(Pobre = ifelse(Pobre == "Yes", 1, 0)) %>% 
  select(id, Pobre)




write.table(df_hogares_test_variables8, file = "xgboost_SMOTE.csv", row.names = FALSE, sep = ",")




# LOOCV  ------------------------------------------------------------------
