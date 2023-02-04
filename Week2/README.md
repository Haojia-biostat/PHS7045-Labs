Week 2 lab
================
Haojia Li
2/4/23

``` r
rar_sim <- function(
    N             = 228, # total number of patients to be allocated in the trial
    interim       = 40, # size of each interim analysis when RAR is used
    trt_effect    = rep(0.35, 4), # treatment effect
    alpha         = 0.35, # alpha for all the arms in beta distribution
    beta          = 0.65, # beta for all the arms in beta distribution
    K             = 1000, # number of draws in posterior distribution
    rar           = T, # logical flag of whether to use RAR (T) or equal allocation (F)
    print_summary = T, # logical flag of whether print summary (T) or simply return max_pt_lt_p0 (F)
    delta         = ifelse(rar, 0.9892, 0.9912)
) {
  # function 1 - allocate participants
  # inputs: 
  #   1. n - total number of patients to be allocated
  #   2. alloc_prob - allocation probability
  # output:
  #   number of participants (nt) allocated to each arm in a vector
  allocation <- function(n, alloc_prob) {
    nt <- round(n*alloc_prob)
    # make sure the sum of allocated numbers equals to the input n
    if(sum(nt) != n) nt[1] <- n - sum(nt[-1])
    return(nt)
  }
  
  # function 2 - flip the coin and count the number of successes
  # inputs:
  #   1. nt - number of participants allocated to each arm in a vector
  #   2. pt - treatment effect for each arm
  # output:
  #   number of successes (sum_yt) in each arm
  rbinom_success <- function(nt, pt) {
    mapply(rbinom, nt, 1, pt, SIMPLIFY = F) |> sapply(sum)
  }
  
  # function 3 - draw 1000 times from the posterior distribution
  # inputs:
  #   1. cum_nt - cumulative number of participants allocated to each arm in a vector
  #   2. cum_sum_yt - cumulative number of successes in each arm
  # output:
  #   posterior probability in matrix with 1000 rows and 4 columns (post_mat)
  posterior_prob <- function(cum_nt, cum_sum_yt) {
    mapply(rbeta, K, alpha + cum_sum_yt, beta + cum_nt - cum_sum_yt)
  }
  
  # function 4 - calculate V0, V1, V2, and V3 and re-normalized to be the adaptive allocation prob
  # inputs:
  #   post_mat - posterior matrix
  #   nt - number of participants allocated to each arm in a vector
  # output:
  #   new allocation probability (alloc_prob), summing up to 1
  adapt_allocpr <- function(post_mat, nt) {
    vt <- sapply(2:4, \(x) mean(apply(post_mat, 1, which.max) == x))
    v0 <- min(c(sum(vt * (nt[-1]+1) / (nt[1]+1)), max(vt)))
    return(c(v0,vt)/sum(c(v0,vt)))
  }
  
  # function 5 - calculate the probability that each treatment arm is better than control
  # input:
  #   post_mat - posterior matrix
  # output:
  #   probability that each treatment arm is better than control in a vector (pt_lt_p0)
  prob_pt_lt_p0 <- function(post_mat) {
    sapply(2:4, \(i) mean(post_mat[,i] > post_mat[,1]))
  }
  
  res <- vector(mode = "list", length = 5)
  names(res) <- c(
    "Design",
    "Num. of patients allocated (nt)", "Num. of successes (sum_yt)", 
    "Pr(post_prob_t > post_prob_0)", "Conclusion"
  )
  res$`Num. of patients allocated (nt)` <- res$`Num. of successes (sum_yt)` <- matrix(ncol = 4, nrow = 0)
  
  alloc_prob <- rep(0.25,4) # initial allocation probability regardless of design
  while(sum(res$`Num. of patients allocated (nt)`) < N) {
    if(rar) n <- min(interim, N - sum(res$`Num. of patients allocated (nt)`)) 
    else n <- N
    # 1. simulate nt and save it to `Num. of patients allocated (nt)`
    nt <- allocation(n, alloc_prob)
    res$`Num. of patients allocated (nt)` <- rbind(res$`Num. of patients allocated (nt)`, nt)
    # 2. simulate sum_yt and save it to `Num. of successes (sum_yt)`
    sum_yt <- rbinom_success(nt, trt_effect)
    res$`Num. of successes (sum_yt)` <- rbind(res$`Num. of successes (sum_yt)`, sum_yt)
    # 3. simulate posterior probability matrix based on cum_nt and cum_sum_yt
    post_mat <- posterior_prob(colSums(res$`Num. of patients allocated (nt)`), colSums(res$`Num. of successes (sum_yt)`))
    
    # 4.1. if the allocation for the whole trial has not been completed, calculate the adaptive allocation probability
    if(sum(res$`Num. of patients allocated (nt)`) < N) alloc_prob <- adapt_allocpr(post_mat, nt)
    # 4.2. else, calculate the probability that each treatment arm is better than control
    else res$`Pr(post_prob_t > post_prob_0)` <- prob_pt_lt_p0(post_mat)
  }
  
  if(print_summary) {
    colnames(res$`Num. of patients allocated (nt)`) <- colnames(res$`Num. of successes (sum_yt)`) <- 
      c("ctrl", "arm1", "arm2", "arm3")
    names(res$`Pr(post_prob_t > post_prob_0)`) <- c("arm1", "arm2", "arm3")
    if(rar) {
      res$Design <- "Response-adaptive randomization (RAR)"
      rownames(res$`Num. of patients allocated (nt)`) <- rownames(res$`Num. of successes (sum_yt)`) <- 
        1:nrow(res$`Num. of patients allocated (nt)`)
      res$`Num. of patients allocated (nt)` <- addmargins(res$`Num. of patients allocated (nt)`)
      res$`Num. of successes (sum_yt)` <- addmargins(res$`Num. of successes (sum_yt)`)
    } else {
      res$Design <- "Equal allocation"
    }
    res$Conclusion <- paste0(
      "Comparing to the threshold of ", delta, ", the trial is a ", 
      ifelse(max(res$`Pr(post_prob_t > post_prob_0)`) > delta, "success.", "failure.")
    )
    print(res)
  }
  else return(max(res$`Pr(post_prob_t > post_prob_0)`))
}
```

``` r
set.seed(7045)
rar_sim()
```

    $Design
    [1] "Response-adaptive randomization (RAR)"

    $`Num. of patients allocated (nt)`
        ctrl arm1 arm2 arm3 Sum
    1     10   10   10   10  40
    2     17    3   17    3  40
    3     17    4   17    2  40
    4     16    4   16    4  40
    5     19    1   19    1  40
    6     12    1   12    3  28
    Sum   91   23   91   23 228

    $`Num. of successes (sum_yt)`
        ctrl arm1 arm2 arm3 Sum
    1      4    3    5    3  15
    2      2    1    7    0  10
    3      5    1    5    1  12
    4      8    0    7    0  15
    5      6    0    4    1  11
    6      7    1    5    1  14
    Sum   32    6   33    6  77

    $`Pr(post_prob_t > post_prob_0)`
     arm1  arm2  arm3 
    0.186 0.553 0.176 

    $Conclusion
    [1] "Comparing to the threshold of 0.9892, the trial is a failure."

``` r
rar_sim(rar = F)
```

    $Design
    [1] "Equal allocation"

    $`Num. of patients allocated (nt)`
       ctrl arm1 arm2 arm3
    nt   57   57   57   57

    $`Num. of successes (sum_yt)`
           ctrl arm1 arm2 arm3
    sum_yt   18   19   23   15

    $`Pr(post_prob_t > post_prob_0)`
     arm1  arm2  arm3 
    0.564 0.829 0.261 

    $Conclusion
    [1] "Comparing to the threshold of 0.9912, the trial is a failure."

``` r
set.seed(7045)
sim_equal1 <- replicate(10000, {rar_sim(rar = F, print_summary = F)})
sim_rar1 <- replicate(10000, {rar_sim(rar = T, print_summary = F)})
```
