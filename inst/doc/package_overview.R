## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(cases)

## ----categorize1--------------------------------------------------------------
# real data example from publication here
set.seed(123)
M <- as.data.frame(mvtnorm::rmvnorm(10, mean=rep(0, 3), sigma=2*diag(3)))
M

## categorize at 0 by default
yhat <- categorize(M)
yhat

## define multiple cutpoints to define multiple decision rules per marker
C <- c(0, 1, 0, 1, 0, 1)
a <- c(1, 1, 2, 2, 3, 3)
categorize(M, C, a)


## this can even be used to do multi-class classification, like this:
C <- matrix(rep(c(-1, 0, 1, -2, 0, 2), 3), ncol=3, byrow = TRUE)
C
categorize(M, C, a)

## ----compare1-----------------------------------------------------------------
## consider binary prediction from 3 models from previous r chunk
names(yhat) <- paste0("rule", 1:ncol(yhat))
yhat

## assume true labels
y <- c(rep(1, 5), rep(0, 5))

## compare then results in 
compare(yhat, y)

## ----evaluate1----------------------------------------------------------------
evaluate(compare(yhat, y))

## ----draw_data1---------------------------------------------------------------
draw_data_lfc(n=20)

## ----draw_data2---------------------------------------------------------------
draw_data_roc(n=20)

## ----workflow1----------------------------------------------------------------
M %>%
  categorize() %>%
  compare(y) %>%
  evaluate()

## ----dtafun1, eval=FALSE------------------------------------------------------
#  ?evaluate

## -----------------------------------------------------------------------------
set.seed(1337)

data <- draw_data_roc(n=120, prev=c(0.25, 0.75), m=4,
                      delta=0.05, e=10, auc=seq(0.90, 0.95, 0.025), rho=c(0.25, 0.25))

head(data)

## ----viz_comp-----------------------------------------------------------------
## comparison regions
results_comp <- data %>% evaluate(alternative ="greater",
                                  alpha=0.025,
                                  benchmark = c(0.7, 0.8),
                                  analysis = "co-primary",
                                  regu = TRUE,
                                  adj = "maxt")
visualize(results_comp)

## -----------------------------------------------------------------------------
## confidence regions
results_conf <- data %>% evaluate(alternative = "greater",
                                  alpha = 0.025,
                                  benchmark = c(0.7, 0.8),
                                  analysis = "full",
                                  regu = TRUE,
                                  adj = "maxt")
visualize(results_conf)

## ----example_wdbc, eval=FALSE, echo=TRUE--------------------------------------
#  vignette("example_wdbc", "cases")

