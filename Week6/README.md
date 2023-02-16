Lab 06 - Parallel computing
================
Haojia Li
2/16/23

# Learning goals

- Learn from HW 1 and revisit vectorizing
- Parallel computing for simulations

# Lab task

**Part 1 (20 minutes)**:

Write an outline of [lab 2 / HW 1
solution](https://github.com/UofUEpiBio/PHS7045-advanced-programming-solutions).

``` r
N <- 228
arms <- 4
h0 <- 0.35
h1 <- 0.35
h2 <- 0.35
h3 <- 0.35



# Generate draws from posterior and calculate pmax
postDraws <- function(y,nMcmc,h0,h1,h2,h3,n0,n1,n2,n3){
  
  #------------------------------
  # Generate draws from posterior
  #------------------------------
  postDraws0 <- rbeta(n=nMcmc, shape1 = h0 + sum(y[1:n0,"0"]==1), shape2 = (1-h0) + n0 - sum(y[1:n0,"0"]==0))
  postDraws1 <- rbeta(n=nMcmc, shape1 = h1 + sum(y[1:n1,"1"]==1), shape2 = (1-h1) + n1 - sum(y[1:n1,"1"]==0))
  postDraws2 <- rbeta(n=nMcmc, shape1 = h2 + sum(y[1:n2,"2"]==1), shape2 = (1-h2) + n2 - sum(y[1:n2,"2"]==0))
  postDraws3 <- rbeta(n=nMcmc, shape1 = h3 + sum(y[1:n3,"3"]==1), shape2 = (1-h3) + n3 - sum(y[1:n3,"3"]==0))
  
  #-----------------------------------
  # Calculate allocation probabilities
  #-----------------------------------
  # v1-v3 probability each arm is best
  # v0 see RMatch Viele et al. 2020
  pBest <- pmax(postDraws0,postDraws1,postDraws2,postDraws3)
  
  v1 <- mean(pBest==postDraws1)
  v2 <- mean(pBest==postDraws2)
  v3 <- mean(pBest==postDraws3)
  
  v0 <- min(sum( c(v1,v2,v3) * (c( n1, n2, n3) + 1) / (n0 + 1), max(v1, v2, v3)) )
  
  # Standardize
  V0 <- v0 / (sum(v0,v1,v2,v3))
  V1 <- v1 / (sum(v0,v1,v2,v3))
  V2 <- v2 / (sum(v0,v1,v2,v3))
  V3 <- v3 / (sum(v0,v1,v2,v3))
  
  # Calculate probability each arm is greater than control
  p1 <- mean(postDraws1 > postDraws0)
  p2 <- mean(postDraws2 > postDraws0)
  p3 <- mean(postDraws3 > postDraws0)
  
  # Report maximum probablity an arm is greater than control
  pMax <- max(p1,p2,p3)
  # unname n0 objects for consistent object names in output
  n0 <- unname(n0)
  n1 <- unname(n1)
  n2 <- unname(n2)
  n3 <- unname(n3)
  out <- c(V0=V0,V1=V1,V2=V2,V3=V3,p1=p1,p2=p2,p3=p3,pMax=pMax,n0=n0,n1=n1,n2=n2,n3=n3)
  return(out)
  
}



design1 <- function(y, nMcmc=10000, h0, h1, h2, h3){
  
  # By the end of the study, each arm will have equal allocation
  postDraws(y=y,nMcmc=nMcmc,
            n0=nrow(y)/ncol(y),
            n1=nrow(y)/ncol(y),
            n2=nrow(y)/ncol(y),
            n3=nrow(y)/ncol(y),
            h0=h0, h1=h1, h2=h2, h3=h3)
  
}

design2 <- function(y, N, nInterim, h0, h1, h2, h3){
  
  #-----------------------------------------------
  # Set up parameters to inform interim monitoring
  #-----------------------------------------------

  # arms and looks as derived from y and look attributes
  arms         <- ncol(y)
  looks        <- floor(N / nInterim)
  
  # n at time t with n = 10 for each arm at t = 1
  nt           <- matrix(NA,nrow=looks,ncol=arms)
  colnames(nt) <- 0:(arms-1)
  nt[1,]       <- rep(nInterim / arms,arms)
  
  # Number of observations between each look
  # Account for possibility of no residual number of nInterim looks
  if(N %% nInterm != 0) {
    residual <- N %% nInterim
  } else {
    residual <- NULL
  }
  size         <- c(rep(nInterim,looks - 1),residual)
  
  
  
  #---------------------------------------------------------------
  # Update allocation probabilities and nt for each look iteration
  #---------------------------------------------------------------
  for(i in seq(looks-1)){
    
    alloProbs <- postDraws(y=y, nMcmc = 1000, h0=h0,h1=h1,h2=h2,h3=h3,
                           n0 = nt[i,"0"],
                           n1 = nt[i,"1"],
                           n2 = nt[i,"2"],
                           n3 = nt[i,"3"])
    
    nt[i+1,] <- nt[i,] + c(rmultinom(n = 1, size = size[i], 
                                    prob = alloProbs[c("V0","V1","V2","V3")]))
    
  }

  #--------------------------------------------------------
  # Report final probabilities of best arm and sample sizes
  #--------------------------------------------------------
  post <- postDraws(y=y, nMcmc = 1000, h0=h0,h1=h1,h2=h2,h3=h3,
                    n0 = nt[i,"0"],
                    n1 = nt[i,"1"],
                    n2 = nt[i,"2"],
                    n3 = nt[i,"3"])
  
  return(post)

}


# Generate outcomes under null
y0 <- rbinom(N,size=1,prob=h0)
y1 <- rbinom(N,size=1,prob=h1)
y2 <- rbinom(N,size=1,prob=h2)
y3 <- rbinom(N,size=1,prob=h3)

y <- cbind("0"=y0,"1"=y1,"2"=y2,"3"=y3)

design1(y,h0=h0,h1=h1,h2=h2,h3=h3)
design2(y,N=N,nInterim = 40,h0=h0,h1=h1,h2=h2,h3=h3)
```

**Part 2 (30 minutes)**:

Re-write design 1 and/or design 2 based upon what you’ve
learned/outlined from the solution. Compare efficiency between your
submission and your revision using the `bench` package.

**Part 3 (25 minutes)**:

Using either submitted design in HW 1 or revisions made in Part 2, use a
parallelization method to simulate 1000 replicates. Compare the timing
to simulating the same 1000 replicates without parallelization.

Extra time? Take any next steps on completing the simluation from the
original homework assignment:

- Assess the Type I error (increase the number of replicates)
- Refine $\delta$ as needed
- Simulate under the alternative hypothesis
