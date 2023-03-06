
# Data Choice - can choose from Boston Dataset or external one I found
get_data <- function(choice = "Boston"){
   
  if (choice == "Boston"){
    d <- Boston %>% mutate(target = medv) %>% select(-medv) %>%
      mutate(chas = as.factor(chas)) %>% drop_na()
  } else {
    d <- fread("Data/housing.csv", sep = ',') %>% data.frame %>%
      mutate(target = median_house_value) %>%
      select(-c(latitude, longitude, median_house_value)) %>%
      mutate(across(where(is.character), as.factor)) %>%
      drop_na()
  } 
  
  return(d)
}

# Lightweight EDA for categorical and continuous variables
get_eda <- function(data = d){
  
  lapply(names(data), function(c_name){

    g <- ggplot(data, aes_string(x = c_name)) + theme_minimal() 
    
    if (is.numeric(data[,c_name])){
      out <- g + geom_boxplot() + theme(axis.title.y=element_blank(),
                                        axis.text.y=element_blank(),
                                        axis.ticks.y=element_blank())
    }
    
    else out <- g + geom_bar()
    
    return(out)
  })
  
}

# Fit a model against target, with supplied x variables. Assumes gaussian
regression_ModelFit <- function(x_variables, data = d){
  
  d_use <- data %>% select(target, all_of(x_variables))
  
  lm(target ~ ., data = d_use)
}

# Evaluate three quantitative regression assumptions
regression_Assumptions <- function(model, data = d){
  
  # linearity
  df_long <- data %>% select(where(is.numeric)) %>% pivot_longer(!target)
  l <- ggplot(df_long, aes(x = value, y = target, color = name)) +
    facet_wrap(~name, scales = "free_x") + geom_point() +
    theme_minimal() + theme(legend.position = "none")
  
  # Normality
  assum_df <- data.frame(resids = rstandard(model), fitted = model$fitted.values)
  
  n <- ggplot(assum_df, aes(sample = resids)) + stat_qq() +
    stat_qq_line(color = 'red') + theme_minimal() +
    xlab('Theoretical Quantiles') + ylab("Sample Quantiles")
  
  # Equal Variance
  ev <- ggplot(assum_df, aes(y = resids, x = fitted)) + geom_point() +
    geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
    theme_minimal() 
  
  full_list <- list(Linearity = l, Normality = n, Equal_Variance = ev)
  return(full_list)
}

# Returns a vector of x_variables selected for in the model by LASSO selection
regression_VarSelection <- function(data = d){
  
  X <- model.matrix(target ~ ., data = data)
  cv.fit <- cv.glmnet(x = X, y = data$target, family = "gaussian",
                      type.measure = "mse")
  
  retained <- coef(cv.fit, s = cv.fit$lambda.1se)
  
  orig_list <- retained@Dimnames[[1]][which(retained != 0)]
  problems <- orig_list[!(orig_list %in% names(data))]
  
  if (length(problems) > 0){
    
    for (i in 1:ncol(data)){
      # This is not efficient - try to think of a better way
      if (any(grepl(names(data)[i], problems))){
        orig_list <- c(orig_list, names(data)[i])
      }
    }
  }
  
  final_list <- orig_list[!(orig_list %in% problems)] %>% unique
  return(final_list)
}

# Filter off problematic points, check for multicollinearity (to adjust vars arguments)
regression_Diagnostics <- function(model, data = d){
  
  outliers <- attr(outlierTest(model)$p, "names") %>% as.numeric
  
  infl_lev <- influence.measures(model)$is.inf %>%
    data.frame %>% 
    mutate(ind = as.numeric(row.names(.))) %>%
    select(cook.d, hat, ind) %>% filter(hat | cook.d) %>%
    pull(ind)
  
  # Not sure the influence levels tbh - need to verify (didn't see in documentation)
  # can also do cooks.distance and define the hat values
  
  v <- vif(model)
  
  if (!is.matrix(v)){ # pure VIF, no categorical vars with levels > 2
    
    crit <- v>5
    
    if (any(crit)){
      v <- sort(v, decreasing = T)
      msg <- paste0(names(v)[crit], collapse = ", ")
      
      stop(paste0("Issues of multicollinearity (in order of largest issues) with ", msg))
    } 
  } else { # GVIF, at least one categorical variable with levels > 2
    
    crit <- v[,3] > 2.5
    
    if (any(crit)){
      dim_names <- attr(v, "dimnames")[[1]]
      probs <- dim_names[crit]
      msg <- paste0(probs[order(v[crit,3], decreasing = T)], collapse = ", ")
      
      stop(paste0("Issues of multicollinearity (in order of largest issues) with ", msg))
    }
  }
  
  filt_off <- c(outliers, infl_lev) %>% unique
  if (length(filt_off) > 0) d %>% slice(-filt_off)
  else d
}

# Evaluate the RMSE
regression_CV <- function(x_vars, data, kfolds = 10){
  
  inds <- sample(rep(1:kfolds, length= nrow(data)))
  cv.pred <- rep(NA, nrow(data))
  
  for (k in 1:kfolds){
    test_inds <- which(inds == k)
    train_dat <- data %>% slice(-test_inds)
    test_dat <- data %>% slice(test_inds)
    
    mod <- regression_ModelFit(x_vars, train_dat)
    cv.pred[test_inds] <- predict(mod, newdata = test_dat)
  }
  
  rmse <- sqrt(mean((cv.pred - data$target)^2))
  return(rmse)
}

support_RedefineVars <- function(elim, x = vars){
  x[x != elim]
}
