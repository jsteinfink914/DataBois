
define_unavailable <- function(bundle){
  
  std_av <- matrix(rep(T, 9), nrow = 3)
  pos <- bundle$pos
  ac <- bundle$mat
  
  # Take all positions and move forward 
  new_bundle <- lapply(1:nrow(pos), function(c_row){
    r <- pos[c_row,"row"]
    co <- pos[c_row,"col"]
    
    ac[r,co] <- T # update position 
    
    av <- std_av
    av[which(ac == T)] <- F # all previously accessed, including current pos, are unavailable
    
    test_r <- (3:1)[r]
    test_co <- (3:1)[co]
    
    # horizontal and vertical conditions
    if (av[r,2] == T) av[r, test_co] <- F
    if (av[2,co] == T) av[test_r, co] <- F 
    
    # diagonal condition
    if (av[2,2] == T & !any(2 %in% c(r, co))) av[test_r, test_co] <- F 
    
    return(list(mat = ac, pos= which(av, arr.ind = T)))
  })
  
  return(new_bundle)
}

iterate <- function(start_arr, len){
  
  depth_list <- vector(mode = "list", length = len)
  depth_list[[1]] <- list(start_arr)
  
  if ((len-1)==0) return(depth_list[[len]])
  
  # for each depth, go through optional paths
  # Stop at len - 1 --> then can just count options
  for (depth in 1:(len-1)){ 
    
    target_list <- depth_list[[depth]]
    c_len <- length(target_list)
    
    for (i in 1:c_len){ # Move each unique option forward one tile
      depth_list[[depth+1]] <- append(depth_list[[depth+1]], define_unavailable(target_list[[i]]))
    }
    
  }
  
  return(depth_list[[len]])
}

count_patterns_from <- function(f, l){
  
  if (l == 9) l <- 8 # Only one option left, save an iteration
  
  std_av <- matrix(rep(T, 9), nrow = 3)
  accessed <- matrix(rep(F, 9), nrow = 3)
  
  letters <- matrix(LETTERS[1:9], nrow = 3, byrow = T)
  w <- which(letters == f, arr.ind = T)
  
  l_arr <- list(mat = accessed, pos = w)
  
  if (l == 0 | l > nrow(std_av)*ncol(std_av)) d <- 0 # I should prob make this work better
  else d <- sum(sapply(iterate(l_arr, l), function(c_bundle) nrow(c_bundle$pos)))
  
  return(d)
}

s <- expand.grid(LETTERS[1:9], 4:9)
s$count <- sapply(1:nrow(s), function(i) count_patterns_from(s[i,1], s[i,2]))
# This does it right (sum(s$all) == 389112), but a speed issue 
# There is symmetry wt/ corners + inner edges -> can this improve speed?
