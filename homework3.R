# Excercise 1-1
# (1)

# sample = 100
# step 1 ¥Íp1, ..., pn
set.seed(181020)
sample.100 <- rbeta(100, 15, 11)
sample.10000 <- rbeta(10000, 15, 11)


# step 2 
n = 100
num <- sum(ifelse(sample.100 > 0.5, 1, 0)) # numbers of (pi > 0.5)
p.hat <- num/n 
p.hat # 0.83

# step 3
var.p.hat <- p.hat*(1-p.hat)/n
var.p.hat # 0.001411

# sample = 10000
# step 1 ¥Íp1, ..., p10000
sample.10000 <- rbeta(10000, 15, 11)

# step 2 
n = 10000
num <- sum(ifelse(sample.10000 > 0.5, 1, 0)) # numbers of (pi > 0.5)
p.hat <- num/n 
p.hat # 0.7845

# step 3
var.p.hat <- p.hat*(1-p.hat)/n
var.p.hat # 1.714319*10^(-5)


# Exercise 1-2
# step 1 ¥Ílogit p1, ..., p100 or p10000
logit <- function(n){
  logit.sample <- c()
  for(i in 1:length(n)){
    logit.sample[i] = log(n[i]/(1-n[i]))
  }
  return(logit.sample)
}

logit(sample.100)
logit(sample.10000)

# step 2
mean(logit(sample.100))    # 0.3052098
mean(logit(sample.10000))  # 0.3242664

# step 3
var(logit(sample.100))/100     # 0.001249733
var(logit(sample.10000))/10000   #  1.650752*10^(-5)


# Excercise 2 
#theta <- function(n){
#  return(runif(n, 0, 1))
#}

theta.100 <- runif(100, 0, 1)
theta.10000 <- runif(10000, 0, 1)

importance.sampling <- function(n, g, x){
  beta <- dbeta(x, 15, 11) # beta(15, 11)
  q <- dunif(x, 0, 1) # important function uniform(0, 1)
  return(cat("mean =",mean(beta*g/q), 
             "variance =", var(beta*g/q)/n)) #mean(beta*g/q)*(1-mean(beta*g/q))/n
}


importance.sampling(100, ifelse(theta.100 > 0.5, 1, 0), theta.100) 
importance.sampling(10000, ifelse(theta.10000  > 0.5, 1, 0), theta.10000)

importance.sampling(100, logit(theta.100), theta.100)
importance.sampling(10000, logit(theta.10000), theta.10000)

