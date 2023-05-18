## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
## load packages:
library(readr)
library(dplyr)
library(splitstackshape)
library(glmnet)
library(cases)

## -----------------------------------------------------------------------------
?data_wdbc
data <- data_wdbc

## -----------------------------------------------------------------------------
dim(data)

table(data$diagnosis) 

## Missing values?
colSums(is.na(data)) # -> no missing values

## -----------------------------------------------------------------------------
summary(data)

## -----------------------------------------------------------------------------
## define minimal acceptable criteria for specificity, sensitivity:
sp0 <- 0.7
se0 <- 0.9
benchmark <- c(sp0, se0)

## -----------------------------------------------------------------------------
pr <- seq(0,1, 0.1) 

quantile(data$area_peak, pr) # 500, 600, 700, 800, 900 ---> area
quantile(data$compactness_peak, pr) # 0.10, 0.15, 0.20, 0.25, 0.30 ---> compactness (perimeter^2 / area - 1.0)
quantile(data$concavity_peak, pr) # 0.10, 0.15, 0.20, 0.25, 0.30 ---> concavity (severity of concave portions of the contour)


## -----------------------------------------------------------------------------
cc <- c(500, 600, 700, 800, 900, 
        0.10, 0.15, 0.20, 0.25, 0.30,
        0.10, 0.15, 0.20, 0.25, 0.30) 

## -----------------------------------------------------------------------------
comp_bm <- data %>% 
  dplyr::select(area_peak, compactness_peak, concavity_peak) %>% 
  cases::categorize(cc, rep(1:3, each=5)) %>% 
  cases::compare(labels = as.numeric(as.character(data$diagnosis)))

results_bm <- cases::evaluate(comp_bm, benchmark = benchmark,
                              alternative = "greater", alpha = 0.025,
                              adj="boot", regu=1) 
results_bm

## -----------------------------------------------------------------------------
visualize(results_bm)

## -----------------------------------------------------------------------------
## data splitting:
set.seed(1337)
split <- stratified(data, c('diagnosis'), 1/3, bothSets = TRUE)
val <- split[[1]] %>% as.data.frame()
trn <- split[[2]] %>% as.data.frame()
dim(trn)
dim(val)
table(val$diagnosis)

## -----------------------------------------------------------------------------
## train example model
mod <- glmnet(x=trn[,-1], y=trn[,1], family="binomial", alpha=0.25)
str(mod, 0)

## -----------------------------------------------------------------------------
set.seed(1337)

## define hyperparameter values for L1/L2 penalty mixing parameter (alpha):
aa <- c(0, 0.25, 0.5, 0.75, 1)

## train models and create predictions:
pred_pm <- sapply(aa, function(alpha){
  mod_pm <- cv.glmnet(x = as.matrix(trn[,-1]), y=trn[,1],
                      family = "binomial",
                      type.measure = "class",
                      alpha = alpha)
  message(paste0("cv.glmnet (alpha = ", alpha, "):"))
  print(mod_pm)
  message("+++++")
  predict(mod_pm, as.matrix(val[,-1]), type="response")
})
colnames(pred_pm) <- paste0("en", aa*100)



## -----------------------------------------------------------------------------
head(pred_pm)

## -----------------------------------------------------------------------------
## define cutpoints (probability scale):
cc <- rep(seq(0.1, 0.5, 0.1), 5)
mm <- rep(1:5, each=5)

## create predictions: 
comp_pm <- pred_pm %>% 
  cases::categorize(cc, mm) %>% 
  cases::compare(labels = as.numeric(as.character(val$diagnosis)))
str(comp_pm, 1)

## -----------------------------------------------------------------------------
## conduct statistical analysis:
set.seed(1337)
results_pm <- cases::evaluate(comp_pm, benchmark = benchmark,
                              alternative = "greater", alpha = 0.025,
                              adj="boot", regu=1) 
str(results_pm, 1)

## -----------------------------------------------------------------------------
results_pm %>% lapply(filter, reject_all) 

## -----------------------------------------------------------------------------
visualize(results_pm)

