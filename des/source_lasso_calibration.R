##################################################################
##### PROGRAM for LASSO CALIBRATION ##############################
##################################################################
#by PCA on 23 JAn. 2019

#### 1. Prepare varaibles for LASSO ####
 
prepare_lasso_vars <- function(svydata, y, indep_vars, dweight = NULL){      

indep_vars <- as.matrix(svydata[,indep_vars])
dep_var <- as.factor(as.matrix(svydata[,y]))
if(is.null(dweight)){
svydata$d <- 1
dwt <- as.vector(svydata$d)
} else {
dwt <- as.vector(svydata[,dweight])  
}

return_list <- list(indep_vars = indep_vars, dep_var = dep_var, dweight = dwt, svydata = svydata)

return(return_list)
}


#### 2. Compute CVs LASSO models to select alpha and lambda ####

compute_cv_lasso <- function(lasso_data, family, parallel = FALSE, alphas, n_folds = 10, type_measure = "deviance"){
  
require(glmnet)
require(doParallel)
require(tidyverse)  

indep_vars <- lasso_data$indep_vars
dep_var <- lasso_data$dep_var
dwt <- lasso_data$dweight
return_list <- lasso_data
i <- 1
  
if(parallel == T){
threads <- detectCores()					
cl <- makeCluster(threads-1)				
registerDoParallel(cl)	

  for(alpha in alphas){
    cv <- cv.glmnet(indep_vars, dep_var, family = family, weight = dwt, 
                    type.measure = type_measure, parallel = T, alpha = alpha,
                    nfolds = n_folds)
    return_list[[4+i]] <- cv
    i <- i + 1
      }
stopCluster(cl)
} else {
  for(alpha in alphas){
    cv <- cv.glmnet(indep_vars, dep_var, family = family, weight = dwt, 
                    type.measure = type_measure, parallel = F, alpha = alpha,
                    nfolds = n_folds)
    return_list[[4+i]] <- cv
    i <- i + 1
      }
}

cvs <- c("indep_vars", "dep_vars", "dweight", "svydata", paste0("cv", str_remove(as.character(alphas), "\\.")))
names(return_list) <- cvs
return_list$alphas <- alphas
return_list$cvname <- cvs[5:length(cvs)]

return(return_list) 
}

  
#### 3. Diagnostics to select appropiate levels of alpha and gamma ####

diags_lasso <- function(lasso_cvs, diag_plot = TRUE){
  
require(crayon)    
require(glmnet)
require(tidyverse)
require(ggpubr)
require(knitr)

cvname <- lasso_cvs$cvname
alphas <- lasso_cvs$alphas 
lalpha <- length(alphas) 

## diagnostics
alpha <- numeric()
lambda_min <- numeric()
error_lambda_min <- numeric()
coef_lambda_min <- numeric()
lambda_1se <- numeric()
error_lambda_1se <- numeric()
coef_lambda_1se <- numeric()

for(i in 1:lalpha){
ob <- lasso_cvs[[cvname[i]]]
errors <- data.frame(ob$lambda, ob$cvm, ob$nzero)
alpha <- c(alpha, alphas[i])
lambda_min <- c(lambda_min, ob$lambda.min)
error_lambda_min <- c(error_lambda_min, errors$ob.cvm[errors$ob.lambda == lambda_min[i]])
coef_lambda_min <- c(coef_lambda_min, errors$ob.nzero[errors$ob.lambda == lambda_min[i]])
lambda_1se <- c(lambda_1se, ob$lambda.1se)
error_lambda_1se <- c(error_lambda_1se, errors$ob.cvm[errors$ob.lambda == lambda_1se[i]])
coef_lambda_1se <- c(coef_lambda_1se, errors$ob.nzero[errors$ob.lambda == lambda_1se[i]])
if(diag_plot == T){
plot(ob, main = cvname[i])
}
}
par(mfrow=c(1,1))

summary <- data.frame(alpha, lambda_min, error_lambda_min, coef_lambda_min, lambda_1se, error_lambda_1se, coef_lambda_1se)
rownames(summary) <- cvname
print(kable(summary))

if(diag_plot == T){
summary_coef <- gather(summary[,c("alpha", "coef_lambda_min", "coef_lambda_1se")], l, value, coef_lambda_min:coef_lambda_1se)
p1 <- ggplot(data = summary_coef, aes(x = alpha, y = value, col = l)) +
  geom_line( ) +
  theme_minimal() + theme(legend.title = element_blank()) +
  labs(title = "Non-zero coef plot", y = "Non-zero coef.", x = "Alpha")
summary_error <- gather(summary[,c("alpha", "error_lambda_min", "error_lambda_1se")], l, value, error_lambda_min:error_lambda_1se)
p2 <- ggplot(data = summary_error, aes(x = alpha, y = value, col = l)) +
  geom_line( ) +
  theme_minimal() + theme(legend.title = element_blank()) +
  labs(title = "Error plot", y = "Error", x = "Alpha")
figure <- ggarrange(p1, p2, ncol = 1, nrow = 2)

print(figure)
}

return(summary)

}

#### 4. Predict model for population and sample ####
predict_lasso <- function(lasso_cvs, popdata, summary, model_selection = NULL, popweight = NULL, coef = FALSE, type_pred = "link"){
  
## check popdata
  popdata_cc <- popdata[complete.cases(popdata) == T,]
  if(nrow(popdata) != nrow(popdata_cc)){
    warning(paste(nrow(popdata) - nrow(popdata_cc), "rows have been dropped from the popdata due to NAs", sep = " "))
    popdata <- popdata_cc
  }
  
## check popweight  
  if(is.null(popweight)){
    popweight <- matrix(replicate(nrow(popdata), 1), ncol =1)
  } else {
    popweight <- matrix(popdata[[popweight]], ncol = 1)  
  }  

indep_vars <- lasso_cvs$indep_vars
indep_vars_names <- colnames(indep_vars)
indep_vars_pop <- as.matrix(popdata[,indep_vars_names])

## models to predict
if(is.null(model_selection)){
alpha <- readline(prompt = cat(green("Select a model (e.g 'cv0'): ")))
lambda <- as.numeric(readline(prompt = cat(green("Select lambda: "))))
} else if(model_selection == "min") {
error <- max(summary$error_lambda_min)
summary$cv <- rownames(summary)
summary <- filter(summary, error_lambda_min == error)
alpha <- summary$cv[1]
lambda <- summary$lambda_min[1]
} else if(model_selection == "1se") {
error <- max(summary$error_lambda_1se)
summary$cv <- rownames(summary)
summary <- filter(summary, error_lambda_1se == error)
alpha <- summary$cv[1]
lambda <- summary$lambda_1se[1]  
}

## adjust final models
lasso_model <- lasso_cvs[[alpha]]
if(coef == TRUE){print(coef(lasso_model, s = lambda, exact = F))}
svy_y_pred <- predict(lasso_model, newx = indep_vars, s = lambda, type = type_pred)
pop_y_pred <- predict(lasso_model, newx = indep_vars_pop, s = lambda, type = type_pred)
class(svy_y_pred) <- "numeric"
class(pop_y_pred) <- "numeric"
## return list
return_list <- list(model = lasso_model, svy_y_pred = svy_y_pred, pop_y_pred = pop_y_pred, 
                    svydata = lasso_cvs$svydata, dweight = lasso_cvs$dweight, popweight = popweight)
return(return_list)

}

#### 5. Execute LASSO calibration ####
calib_LASSO <- function(lasso_pred, calwt = "calwt"){
  
  svy_y_pred <- lasso_pred$svy_y_pred
  pop_y_pred <- lasso_pred$pop_y_pred
  dweight <- lasso_pred$dweight
  popweight <- lasso_pred$popweight
  nrows <- nrow(lasso_pred$svydata)
  svydata <- lasso_pred$svydata
    
  # compute matrix of coincidencies for y pred
  M <- matrix(c(replicate(nrows, 1), svy_y_pred), ncol = 2)
  
  # Diagonal of weights 
  D <- diag(x = dweight, nrow = nrows)
  
  # matrix of design weights
  d <- matrix(dweight, ncol = 1)
  
  # Total of sum
  TM <- matrix(c(sum(popweight), sum(pop_y_pred*popweight)), ncol = 2 )  
  
  # compute calwt 
  calibwt_lasso_y <- d + D %*% M %*% solve(t(M) %*% D %*% M) %*% t(TM - t(d) %*% M) 
  
  # consistency checks
  print(paste("CHECK 1. N is", round(sum(popweight),0), "and weighted sample total is", round(sum(calibwt_lasso_y))),0)
  print(paste("CHECK 2. Sum of predicted y in the population is", round(sum(pop_y_pred*popweight),1), "and in the weighted sample is", round(sum(calibwt_lasso_y*svy_y_pred))),1)
  
  #return
  svydata[, calwt] <- as.vector(calibwt_lasso_y)
  return(svydata)
}


#### 6. LASSO predict bundle function ####
get_lasso_predict <- function(svydata, y, indep_vars, popdata, dweight = NULL, popweight = NULL, family, n_folds = 10, type_measure = "deviance", parallel = FALSE, alphas, diag_plot = TRUE, model_selection = NULL, coef = FALSE, type_pred = "link"){
  

  
  lasso_data <- prepare_lasso_vars(svydata = svydata, y = y, indep_vars = indep_vars, dweight = dweight)
  lasso_cvs <- compute_cv_lasso(lasso_data = lasso_data, family = family, parallel = parallel, alphas = alphas, n_folds = n_folds, type_measure = type_measure)
  summary <- diags_lasso(lasso_cvs = lasso_cvs, diag_plot = diag_plot)  
  return_list <- predict_lasso(lasso_cvs = lasso_cvs, popdata = popdata, summary = summary, model_selection = model_selection, popweight = popweight, coef = coef, type_pred = type_pred)
  return(return_list)
  
}

#### 7. LASSO calibration bundle fucntion #####
lasso_calibrate <- function(svydata, y, indep_vars, popdata, dweight = NULL, popweight = NULL, family, n_folds = 10, type_measure = "deviance", parallel = FALSE, alphas, diag_plot = TRUE, coef = FALSE, model_selection = NULL, type_pred = "link", calwt = "calwt"){
  
  lasso_pred <- get_lasso_predict(svydata = svydata, y = y, indep_vars = indep_vars, popdata = popdata, dweight = dweight, popweight = popweight, family = family, 
                                n_folds = n_folds, type_measure = type_measure, parallel = parallel, model_selection = model_selection, 
                                alphas = alphas, diag_plot = diag_plot, coef = coef, type_pred = type_pred)
  return_df <- calib_LASSO(lasso_pred = lasso_pred, calwt = calwt)
  
  return_list <- list(model = lasso_pred,
                      wts = return_df)
  
  print("Finished calib")
  
  return_path <- paste0("data/temp/", calwt, ".RDS")
  write_rds(return_path, return_path)  
}
