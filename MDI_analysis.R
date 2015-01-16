
# Toolkit -----------------------------------------------------------------
library(knitr)
library(caret)
library(foreign)
library(dplyr)
library(ROCR)
library(gbm)
library(e1071)

create_formulas <- function(y,x) {
  lapply(paste(y,"~",
               lapply(x,function(x) {paste(x,c(rep("+",length(x)-1),""),collapse = " ")}))
         ,as.formula)
}


# The Setup ---------------------------------------------------------------


# dt <- read.dta("AnalyticFileForNilay_090814.dta")
# save(dt,file = "mdi_data.RData")
# for (col in names(dt)[c(-1,-3,-9,-10,-21)]) {
#   dt[[col]] <- as.factor(dt[[col]])
# }
dt <- na.omit(dt)
dt <- dt[dt$sample == 1,]

load("mdi_data.RData")
set.seed(540)
trainIndex <- createDataPartition(dt$twoer, p = .8,list = FALSE)
training <- dt[trainIndex,]
testing <- dt[-trainIndex,]

outcomes <- c("twoer","inpat","totexpy1_dec")
baseline <- c("age","age_2","male","midwest","south","west","married","numkids",
              "numadults","pov100","pov138")
conditions <- c("anyheart","stroke","ulcer","cancer","diabetes","kidney","liver",
                "asthmaetal")
access <- c("mepspub","mepsuns")
HSRQOL <- c("halex")
utilization <- c("hospnt","ervisit","care10")
models <- list(baseline = baseline,
               baseline.conditions = c(baseline,conditions),
               baseline.access = c(baseline,access),
               baseline.HSRQOL = c(baseline,HSRQOL),
               baseline.utilization = c(baseline,utilization),
               baseline.conditions.HSRQOL = c(baseline,conditions,HSRQOL),
               baseline.conditions.utilization = c(baseline,conditions,utilization),
               baseline.HSRQOL.utilization = c(baseline,HSRQOL,utilization),
               baseline.conditions.HSRQOL.utilization = c(baseline,conditions,HSRQOL,utilization),
               baseline.conditions.HSRQOL.utilization.access = c(baseline,conditions,HSRQOL,utilization,access))


formulas <- unlist(lapply(outcomes,create_formulas,x = models))


# Logistic Regression -----------------------------------------------------


fit_models <- lapply(formulas,glm,family = "binomial",data = training)
names(fit_models) <- paste0(rep(outcomes,each = length(models)),'~',names(models))

for (i in 1:length(fit_models)) {
  pred <- prediction(predict(fit_models[[i]]),fit_models[[i]][['y']])
  results <- list(fit = fit_models[[i]],formula = names(fit_models)[i],
                  auc = performance(pred,'auc'),roc = performance(pred,'rch'))
  knit2html(input = 'C:/Users/m097845/Google Drive/Scripts/MDI Research/LR_Performance_Report.Rmd',
            output = paste0('Test/',results$formula,'.',Sys.Date(),'.html'))
}

#x <- sapply(fit_models,function(x) {performance(prediction(predict(x),x[['y']]),'auc')@y.values[[1]]})
#write.csv(as.matrix(x),'AUC_sample.csv')

# Random Forest -----------------------------------------------------------

for (i in 1:length(formulas)) {
    fit_rf <- train(formulas[[i]],data = training,method = 'rf')
}



names(fit_models)
pred <- prediction(predict(fit_models[[30]]),fit_models[[30]][['y']])
performance(pred,'auc')
summary(fit_models[[30]])



