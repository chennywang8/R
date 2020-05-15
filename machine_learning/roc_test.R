library(pROC)
library(randomForest)
library(ggplot2)
library(caret)

# ================= PREPARE DATA ===================
set.seed(420)
num_samples <- 100
weight <- sort(rnorm(n=num_samples, mean = 172, sd = 29))
obese  <- ifelse(test=(runif(n = num_samples) < (rank(weight)/100)),
                 yes = 1, no = 0)
data   <- data.frame(weight = weight, obese = obese)

# ================= GLM FIT =======================
glm_model<- glm(obese ~ weight, data = data, family = binomial)
glm_info <- roc(obese, glm_model$fitted.values, legacy.aexs = TRUE, auc = TRUE)

roc_df   <- data.frame(tpp=glm_info$sensitivities*100,
                       fpp=(1-glm_info$specificities)*100,
                       thresholds=glm_info$thresholds)

# ============= RANDOM FOREST FIT =================
rf_model <- randomForest(factor(obese) ~ weight)
rf_info  <- roc(obese, rf_model$votes[,1], legacy.axes = TRUE, auc = TRUE)


# ============ PLOT MULTI-ROC =====================
ggroc(list(glm_model=glm_info, random_forest=rf_info),
      alpha = 0.9, legacy.axes = TRUE, size = 2)

# ============ CARET ==============================
## Create trainControl object: myControl
myControl <- trainControl(
  method = "cv", ## cross validation
  number = 10,   ## 10-fold
  summaryFunction = twoClassSummary, ## NEW
  classProbs = TRUE, # IMPORTANT
  verboseIter = FALSE
)
## Train glm with custom trainControl: model
data$obese = factor(ifelse(data$obese, yes = "F", no = "S"))
cv_model <- train(obese ~ ., data,
               method = "glm", ## to use glm's logistic regression
               trControl = myControl)
