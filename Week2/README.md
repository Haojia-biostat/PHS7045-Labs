Week 2 lab
================
Haojia Li
2/6/23

``` r
rar_sim <- function(
    N             = 228, # total number of patients to be allocated in the trial
    interim       = 40, # size of each interim analysis when RAR is used
    trt_effect    = rep(0.35, 4), # treatment effect
    alpha         = 0.35, # alpha for all the arms in beta distribution
    beta          = 0.65, # beta for all the arms in beta distribution
    K             = 1000, # number of draws in posterior distribution
    rar           = T, # logical flag of whether to use RAR (T) or equal allocation (F)
    delta         = ifelse(rar, 0.9892, 0.9912) # threshold to determine a successful trial
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
    if(sum(nt, na.rm = T) != n) nt[1] <- n - sum(nt[-1], na.rm = T)
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
    vt <- sapply(2:4, \(i) mean(apply(post_mat, 1, which.max) == i))
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
  
  # define result components
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
    
    # 4. if the allocation for the whole trial has not been completed, calculate the adaptive allocation probability
    if(sum(res$`Num. of patients allocated (nt)`) < N) alloc_prob <- adapt_allocpr(post_mat, nt)
    # 5. else, calculate the probability that each treatment arm is better than control
    else res$`Pr(post_prob_t > post_prob_0)` <- prob_pt_lt_p0(post_mat)
  }
  
  colnames(res$`Num. of patients allocated (nt)`) <- colnames(res$`Num. of successes (sum_yt)`) <- 
    c("Ctrl", "Arm1", "Arm2", "Arm3")
  rownames(res$`Num. of patients allocated (nt)`) <- rownames(res$`Num. of successes (sum_yt)`) <- 
    1:nrow(res$`Num. of patients allocated (nt)`)
  names(res$`Pr(post_prob_t > post_prob_0)`) <- c("Arm1", "Arm2", "Arm3")
  if(rar) {
    res$Design <- "Response-adaptive randomization (RAR)"
    res$`Num. of patients allocated (nt)` <- addmargins(res$`Num. of patients allocated (nt)`)
    res$`Num. of successes (sum_yt)` <- addmargins(res$`Num. of successes (sum_yt)`)
  } else {
    res$Design <- "Equal allocation"
    res$`Num. of patients allocated (nt)` <- addmargins(res$`Num. of patients allocated (nt)`, 2)
    res$`Num. of successes (sum_yt)` <- addmargins(res$`Num. of successes (sum_yt)`, 2)
  }
  res$Conclusion <- paste0(
    "Comparing to the threshold of ", delta, ", the trial is a ", 
    ifelse(max(res$`Pr(post_prob_t > post_prob_0)`) > delta, "success.", "failure.")
  )
  return(res)
}
```

``` r
set.seed(7045)
# simulation result of equal allocation
rar_sim(rar = F)
```

    $Design
    [1] "Equal allocation"

    $`Num. of patients allocated (nt)`
      Ctrl Arm1 Arm2 Arm3 Sum
    1   57   57   57   57 228

    $`Num. of successes (sum_yt)`
      Ctrl Arm1 Arm2 Arm3 Sum
    1   24   32   21   18  95

    $`Pr(post_prob_t > post_prob_0)`
     Arm1  Arm2  Arm3 
    0.943 0.270 0.115 

    $Conclusion
    [1] "Comparing to the threshold of 0.9912, the trial is a failure."

``` r
# simulation result of RAR
rar_sim()
```

    $Design
    [1] "Response-adaptive randomization (RAR)"

    $`Num. of patients allocated (nt)`
        Ctrl Arm1 Arm2 Arm3 Sum
    1     10   10   10   10  40
    2     15    3   15    7  40
    3     16    1   16    7  40
    4     14    2   14   10  40
    5     13    2   12   13  40
    6     12    2   11    3  28
    Sum   80   20   78   50 228

    $`Num. of successes (sum_yt)`
        Ctrl Arm1 Arm2 Arm3 Sum
    1      2    2    4    3  11
    2      6    0    5    2  13
    3      8    1    6    3  18
    4      3    0    3    3   9
    5      4    1    6    3  14
    6      3    2    7    0  12
    Sum   26    6   31   14  77

    $`Pr(post_prob_t > post_prob_0)`
     Arm1  Arm2  Arm3 
    0.400 0.835 0.278 

    $Conclusion
    [1] "Comparing to the threshold of 0.9892, the trial is a failure."

``` r
# parallel calculation
library(parallel)
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, c("rar_sim"))

# replicate the design 10000 times
set.seed(7045)
# equal allocation
sim_equal <- parLapply(cl, 1:10000, function(i, ...) rar_sim(rar = F))
# RAR
sim_rar <- parLapply(cl, 1:10000, function(i, ...) rar_sim(rar = T))

# stop clustering
stopCluster(cl)

# generate the largest prob that one arm has a higher posterior rate than control
max_pt_lt_p0_equal <- sapply(sim_equal, \(x) max(x$`Pr(post_prob_t > post_prob_0)`))
max_pt_lt_p0_rar <- sapply(sim_rar, \(x) max(x$`Pr(post_prob_t > post_prob_0)`))

# depict the distribution
par(mfrow = c(1,2))
hist(max_pt_lt_p0_equal, main = "Equal allocation", xlab = "")
hist(max_pt_lt_p0_rar, main = "RAR", xlab = "")
```

![](README_files/figure-commonmark/unnamed-chunk-3-1.png)

``` r
# calculate the Type I error
mean(max_pt_lt_p0_equal >= 0.9912)
```

    [1] 0.0235

``` r
mean(max_pt_lt_p0_rar >= 0.9892)
```

    [1] 0.0257

``` r
# find threshold
quantile(max_pt_lt_p0_equal, 0.975)
```

    97.5% 
    0.991 

``` r
quantile(max_pt_lt_p0_rar, 0.975)
```

    97.5% 
     0.99 

``` r
# replicate the study design assuming treatment effect is 0.35, 0.35, 0.35 and 0.65 for the arms, respectively
rar_sim(trt_effect = c(0.35, 0.35, 0.35, 0.65), rar = F)
```

    $Design
    [1] "Equal allocation"

    $`Num. of patients allocated (nt)`
      Ctrl Arm1 Arm2 Arm3 Sum
    1   57   57   57   57 228

    $`Num. of successes (sum_yt)`
      Ctrl Arm1 Arm2 Arm3 Sum
    1   24   32   21   39 116

    $`Pr(post_prob_t > post_prob_0)`
     Arm1  Arm2  Arm3 
    0.943 0.270 0.995 

    $Conclusion
    [1] "Comparing to the threshold of 0.9912, the trial is a success."

``` r
rar_sim(trt_effect = c(0.35, 0.35, 0.35, 0.65))
```

    $Design
    [1] "Response-adaptive randomization (RAR)"

    $`Num. of patients allocated (nt)`
        Ctrl Arm1 Arm2 Arm3 Sum
    1     10   10   10   10  40
    2     19    0    2   19  40
    3     20    0    1   19  40
    4     20    0    0   20  40
    5     20    0    0   20  40
    6     14    0    0   14  28
    Sum  103   10   13  102 228

    $`Num. of successes (sum_yt)`
        Ctrl Arm1 Arm2 Arm3 Sum
    1      2    2    4    7  15
    2      5    0    1   13  19
    3      5    0    0   15  20
    4      5    0    0   14  19
    5      7    0    0   11  18
    6      7    0    0   12  19
    Sum   31    2    5   72 110

    $`Pr(post_prob_t > post_prob_0)`
     Arm1  Arm2  Arm3 
    0.216 0.701 1.000 

    $Conclusion
    [1] "Comparing to the threshold of 0.9892, the trial is a success."
