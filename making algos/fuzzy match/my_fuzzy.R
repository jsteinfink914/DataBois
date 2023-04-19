
# take a look at the source code for table
# Check out the switch function as well
# also vsetdiff

library(tidyverse)
library(data.table)
library(vecsets)
library(stringdist) # for using qgrams function to test outputs

setwd("~/R-Learning/Data Science/making algos/fuzzy match")

get_data <- function(only_forward = T){
  
  d <- fread("data.csv", sep = ",") %>% data.frame %>%
    select(-score) %>% mutate(ind = 1:nrow(.))
  
  if (only_forward) d <- d[(d$ind %% 4) == 1,]
  
  return(list(df1 = d$word1, df2 = d$word2))
}

# order doesn't matter, just q count
create.qgrams <- function(d, q){
  
  if (!is.vector(d)) stop("inputs must be vectors")
  if (!is.character(d)) d <- as.character(d)
  
  clean <- d %>% tolower %>% unique %>%
    sort %>% trimws 
  
  splits <- clean %>% strsplit(., "")
  
  q_breaks <- lapply(splits, function(i){ # two ways: have an empty vector and fill it, then check if its in; fill whole thing, then check agains 
  
    if (q >= length(i)) i
    # I think this makes sense for this case
    
    # can get counts with table
    sapply(1:(length(i)-q+1), function(j) paste0(i[j:(j+q-1)],collapse = "")) 
  })
  
  names(q_breaks) <- clean
  return(q_breaks) 
}

my_fuzzy <- function(match_1, match_2, max_dist = 2, q = 2, best_a = T){
  
  a <- create.qgrams(match_1, q = q)
  b <- create.qgrams(match_2, q = q)
  
  out <- data.frame(expand.grid(match_1 = names(a), match_2 = names(b)),
                    dist = NA)
  
  for (i in 1:nrow(out)){ # better way to fill? tried several, this is easiest
    u <- out %>% slice(i)
    
    a_val <- a[[u$match_1]]
    b_val <- b[[u$match_2]]
    
    # can't use setdiff, makes unique 
    out[i, "dist"] <- length(vsetdiff(a_val,b_val)) + length(vsetdiff(b_val,a_val)) # Which way for gram max value?
  }
  
  out <- out %>% filter(dist <= max_dist)
  if (best_a) out <- out %>% group_by(match_1) %>% slice_min(dist, n = 1)
  
  return(out)
}

d <- get_data()
see <- my_fuzzy(d$df1[1:100], d$df2[1:100])

# The function qgrams, when fed inputs of strings, returns unique letter combos
# of ngrams with 0, 1 --> can subtract these component-wise to get difs?

# Need to do larger scale testing on this
