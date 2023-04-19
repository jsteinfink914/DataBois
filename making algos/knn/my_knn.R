
library(tidyverse)

support.standardize <- function(d, target = 'Species', add_bin = T){
  
  if (any(is.na(d))) stop("Missing values in df")
  
  only_num <- function(x) !(is.factor(x) | is.character(x)) & (length(unique(x))>2)
  std <- function(x) (x-min(x))/(max(x) - min(x)) # if large row count, define at beginning of a loop
  
  out <- d %>% select(where(only_num)) %>%
    mutate(across(everything(), std)) %>%
    mutate(l = d[,target])
  
  names(out)[ncol(out)] <- target
  
  if (add_bin){
    
    bin <- function(x) length(unique(x)) == 2
    only_bin <- d %>% select(-all_of(target)) %>%
      select(where(bin)) %>%
      mutate(across(everything(), ~as.numeric(as.factor(.x))-1))
    
    out <- data.frame(out, only_bin)
  }
  
  return(out)
}

support.dist_metric <- function(a, b, choice){
  # a, b are input vectors
  
  if (choice == "Euclidean") c <- 2
  else if (choice == "Manhattan") c <- 1
  else stop(paste0(choice, " not yet implemented"))
  
  sum(abs(a-b)^c)^(1/c)
}

called.compute_dists <- function(train_dt, test_dt, ch){
  
  loop_over <- expand.grid(tr_ind = 1:nrow(train_dt), te_ind = 1:nrow(test_dt))
  
  # Make two "hashable" lists for slice lookups --> don't need to - already unique
  
  dists <- sapply(1:nrow(loop_over), function(i){
    sl <- loop_over[i,]
    
    a <- train_dt[sl$tr_ind,]
    b <- test_dt[sl$te_ind,]
    
    support.dist_metric(a, b, ch)
  })
  
  out <- data.frame(loop_over, dists = dists)
  return(out)
}

support.choose <- function(dt, me){
  if (me == "voting") names(which.max(summary(dt$outcome))) # could also do model.matrix 
  else if (me == "median") median(dt$outcome)
  else stop(paste0(me, " not yet implemented"))
}
  
called.decision <- function(dist_df, train_o, k, metric){

  train_o <- data.frame(tr_ind = 1:length(train_o), outcome = train_o)
  dist_df <- left_join(dist_df, train_o, by = "tr_ind") 
  
  out <- sapply(unique(dist_df$te_ind), function(i){
    
    # only spot that uses tidyverse
    dist_df %>% filter(te_ind == i) %>% arrange(dists) %>%
      slice(1:k) %>% support.choose(., metric) 
    
  })
  
  return(out)
}

# maybe make k_arg a vector that chooses optima
my_knn <- function(train, test, outcome, k){
  
  if (outcome %in% names(test)) te_dt <- test[,-which(names(test) == outcome)]
  else te_dt <- test
  
  tr_dt <- train[,-which(names(train) == outcome)]
  tr_o <- train[,outcome]
  
  dist_m <- "Euclidean"
  dec <- "voting"
  
  if (any(lapply(tr_dt, class) %in% c("character", "factor"))) stop("non-numeric columns")
  if (any(lapply(tr_dt, function(x) length(unique(x)))<3)) dist_m <- "Manhattan" # Whole df if one binary?
  if (!(class(tr_o) %in% c("character", "factor"))) dec <- "median"  
  
  all_dists <- called.compute_dists(tr_dt, te_dt, dist_m)
  test_outcomes <- called.decision(all_dists, tr_o, k, dec)
  
  return(test_outcomes)
}

create.train_test <- function(df, test_prop){
  test_inds <- sample(1:nrow(df), floor(nrow(df)*test_prop))
  
  test_df <- df[test_inds,]
  train_df <- df[-test_inds,]
  
  list(train = train_df, test = test_df)
}

# Not sure how to implement to do RMSE to not double work
# Have optional optimal k testing by giving a k-range to my_knn, will return graph as well and default response
test.optimal_k <- function(train, test, outcome, k_range){
  
  test_o <- test[,outcome]
  
  out_rg <- sapply(k_range, function(k){
    
    pred <- my_knn(train, test, outcome, k) # Can add in storage optimizations for distance matrix to not re-compute if selecting optimal
    res <- data.frame(test_o, pred)
    
    sum(diag(table(res$test, res$pred)))/length(res$pred)
  })
  
  res <- data.frame(k_range, out_rg)
  
  list(
    opt = k_range[which.max(out_rg)],
    g = ggplot(data = res, aes(x=k_range, y = out_rg)) +
      geom_point() + geom_line() +
      scale_x_continuous(breaks = res$k) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Neighbors", y="Accuracy") + theme_minimal()
  )
}

ir <- iris %>% support.standardize # Should I standardize with test data in?

l <- create.train_test(ir, 0.25)
tune <- test.optimal_k(l$train, l$test, outcome = "Species", k_range=2:7)

model <- my_knn(l$train, l$test, outcome = "Species", k = tune$opt)

out <- data.frame(l$test, prediction = model)
t <- table(out$Species, out$prediction)

# Can look to add: CV, feature selection, feature guardrails
