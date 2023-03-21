
# train option lacks final values so far
get_data <- function(type = "train"){
  d <- fread(paste0("Data/loan_", type, ".csv"), sep = ",", header = T,
             stringsAsFactors = F, na.strings = "") %>%
    data.frame
  
  return(d)
}

data_ImputeMissings <- function(df, target = "Status"){
  # Code my own "median" impute function

  wt_na <- apply(df, 2, function(x) any(is.na(x)))
  wt_na_names <- names(wt_na)[wt_na]
  
  if (target %in% wt_na_names) warning(paste0(target, " values will be imputed"))
    
  to_impute <- df %>% select(all_of(wt_na_names))
  
  imputed <- lapply(to_impute, function(col){

    if (class(col) %in% c("numeric", "integer")) col[is.na(col)] <- median(col, na.rm = T)
    
    else { # if decision needs to be made via voting
      tb <- table(col)
      replace <- names(tb)[which.max(tb)]
      col[is.na(col)] <- replace
    }

    return(col)
  }) %>% do.call(cbind.data.frame, .) %>% data.frame
  
  names(imputed) <- names(to_impute)
  
  d <- data.frame(imputed, select(df, all_of(names(wt_na)[!wt_na]))) %>%
    select(all_of(names(df))) # maintains orig order
  
  return(d)
}

data_AllInteractions <- function(df, target = "Status"){
  # artificially boost column space, though likely high correlations
  # high corrs dealt with with mtry in rfs
  
  d_small <- df %>% select(-all_of(target))
  ints <- model.matrix(~.^2, data = d_small) 
  
  nm <- attr(ints, "dimnames")[[2]]
  to_add <- ints[,grepl(":", nm)]
  
  final_to_add <- to_add %>% data.frame
  names(final_to_add) <- gsub(":", "X", attr(to_add, "dimnames")[[2]])
    
  out <- data.frame(select(df, all_of(target)), d_small, final_to_add)
  
  return(out)
}

analyze_TuneRF <- function(df, tunegrid = NULL, method_choice = "rf"){
  # method options are 'rf' and 'rfRules'
  
  ctrl <- trainControl(method = "cv", number = 10)
  # 10-fold cross-validation --> repeated prob better, but I don't understand it
  
  metric <- "Accuracy"
  
  mtry_center <- floor(sqrt(ncol(df)))
  mtry_vect <- (mtry_center-floor(mtry_center/2)):(mtry_center*2)
  
  # I made these up - better way? Centered around floor(sqrt(ncol(x))) for mtry (default)
  if (is.null(tunegrid) & method_choice == "rf") tunegrid <- expand.grid(.mtry = mtry_vect)
  else if (is.null(tunegrid) & method_choice == "rfRules") tunegrid <- expand.grid(.mtry = mtry_vect, .maxdepth = 10)
  
  ntrees_seq <- seq(from = 200, to = 600, by = 200)
  
  eval <- lapply(ntrees_seq, function(n){
    
    c_rf <- train(factor(Status) ~ ., data = df, method = method_choice,
                  metric = metric, tuneGrid = tunegrid, ntree = n,
                  trControl = ctrl)
    
    res <- c_rf$results %>% mutate(ntrees = n)
    
    return(res)
  }) %>% rbindlist %>% data.frame
  
  out <- eval[which.max(eval$Accuracy),]

  if (method_choice == "rf"){
    g <- ggplot(eval, aes(x = factor(mtry), y = Accuracy, group = factor(ntrees))) +
      geom_line(aes(color = factor(ntrees))) + theme_minimal() +
      labs(title = "Optimal Tuning Parameters", x= "Number of Columns at Split",
           y="Accuracy", color = "ntrees") +
      scale_y_continuous(labels = scales::percent)
    
    out <- list(best_tune = out, plot = g)
  }
  
  return(out)
}

# for future: Can also be tuning nodesize in 'rf'

fit_rf <- function(df, tuning_params, method_choice = "rf"){
  # Not implemented for rfRules yet (need a different tuning params arg)
  
  test_inds <- sample(1:nrow(df), floor(nrow(df)/3)) # take 1/3 as the test set
  
  test_df <- df[test_inds,] %>% select(-"Status")
  train_df <- df[-test_inds,]
  
  model <- train(factor(Status) ~ ., data = train_df,
                 method = "rf", metric = "Accuracy",
                 tuneGrid = data.frame(.mtry = tuning_params$mtry),
                 ntrees = tuning_params$ntrees,
                 importance = T)

  preds <- predict(model, newdata=test_df)
  
  acc_test <- length(which(preds == as.factor(df[test_inds, "Status"])))/length(test_inds)
  v_imp <- varImp(model)
  
  g <- plot(v_imp)
  
  out <- list(
    rf_model = model,
    accuracy_OOS = acc_test,
    importance = v_imp,
    imp_plot = g
  )
  
  return(out)
}
