
# Make a permutation test function that shuffles the treatment groups and 
# returns the proportion of times that the difference in means between groups
# is greater than or equal to (or less than or equal to) the actual difference
# in means in the orginal treatment assignment.
# ----------------------------------------------------------------------------

perm.test <- function(rsp,trmt,iter,two.sided="yes",
                      side=NULL){ # response, treatment, iterations, 
                                  # two.sided, side
  require(doParallel)
  true.diff <- mean(rsp[trmt==1]-rsp[trmt==0])
  shuf.diff <- times(iter) %do% 
    with(data.frame(rsp=rsp,trmt=trmt[sample(1:length(trmt))]),
         mean(rsp[trmt==1]-rsp[trmt==0]))
  if(two.sided=="yes"){
    p <- (sum(abs(shuf.diff)>=abs(true.diff)))/iter
    p <- rbind(true.diff,p)
    colnames(p) <- "Two-sided Permutation Test"
    rownames(p) <- c("Difference in Means","p-value")
    return(round(p,5))
  }
  if(two.sided=="no"&side=="upper") {
    p <- (sum(shuf.diff>=true.diff))/iter
    p <- rbind(true.diff,p)
    colnames(p) <- "One-sided Permutation Test (Upper)"
    rownames(p) <- c("Difference in Means","p-value")
    return(round(p,5))
  }
  if(two.sided=="no"&side=="lower") {
    p <- (sum(shuf.diff<=true.diff))/iter
    p <- rbind(true.diff,p)
    colnames(p) <- "One-sided Permutation Test (Lower)"
    rownames(p) <- c("Difference in Means","p-value")
    return(round(p,5))
  }
}

# Generate some fake data
# -----------------------
X <- c(rep(1,len=50),rep(0,len=50)) # Treatment
Y <- rnorm(10,sd=7,mean=12) + X*3   # Response (if X=1, the conditional mean of the
                                    # response should increase by ~3)

# Take perm.test on a test run
# ----------------------------
perm.test(Y,X,1000) # Two-tailed "significance test"
perm.test(Y,X,1000,two.sided="no",side="upper") # One-tailed
perm.test(Y,X,1000,two.sided="no",side="lower") # One-tailed (should give big p-value)
perm.test(Y,X,1000,two.sided="no") # Should produce error

# Change in p-values as number of iterations increases
library(ggplot2)
library(dplyr)

p1 <- perm.test(Y,X,10)[2]
p2 <- perm.test(Y,X,100)[2]
p3 <- perm.test(Y,X,500)[2]
p4 <- perm.test(Y,X,1000)[2]
p5 <- perm.test(Y,X,5000)[2]
p6 <- perm.test(Y,X,10000)[2]
data.frame(n=c(10,100,500,1000,5000,10000),
           p=c(p1,p2,p3,p4,p5,p6)) %>%
  ggplot(aes(x=n,y=p)) + geom_point() +
  geom_line() + theme_bw()

p1 <- perm.test(Y,X,10,two.sided = "no", side="upper")[2]
p2 <- perm.test(Y,X,100,two.sided = "no", side="upper")[2]
p3 <- perm.test(Y,X,500,two.sided = "no", side="upper")[2]
p4 <- perm.test(Y,X,1000,two.sided = "no", side="upper")[2]
p5 <- perm.test(Y,X,5000,two.sided = "no", side="upper")[2]
p6 <- perm.test(Y,X,10000,two.sided = "no", side="upper")[2]
data.frame(n=c(10,100,500,1000,5000,10000),
           p=c(p1,p2,p3,p4,p5,p6)) %>%
  ggplot(aes(x=n,y=p)) + geom_point() +
  geom_line() + theme_bw()