condition <- c(rep(letters[1:2], 2), rep(letters[2:1], 2))
item <- rep(1:4, 2)
subj <- rep(1:2, each = 4)
df_example <- data.frame(subj, item, condition)
df_example

xtabs(~ subj + item, df_example)
xtabs(~ subj + condition, df_example)
xtabs(~ item + condition, df_example)


## sample size
n <- 1000
## independent and identically distributed sample:
y <- rnorm(n, mean = 500, sd = 100)
## histogram of data:
hist(y, freq = FALSE)
## true value of mean
abline(v = 500, lwd = 2)


#### experiment repetition
mu <- 500
sigma <- 100
## repetition
k <- 2000
y_matrix <- matrix(rep(NA, n * k), ncol = k)

for (i in 1:k) {
  y_matrix[, i] <- rnorm(n, mean = mu, sd = sigma)
}

y_means <- colMeans(y_matrix)
c(mean(y_means), sd(y_means))


## figures
op<-par(mfrow=c(1,2),pty="s")

for(i in 1:k){
  if(i==1){
plot(density(y_matrix[,i]),main="Samples from a \n Normal distribution",
     xlab="")
  } else {
lines(density(y_matrix[,i]),lty=2)
  }
}

hist(y_means,freq=FALSE,main="Sampling distribution of means",
     xlab="means under repeated sampling",
     ylim=c(0,0.15))
abline(v=mean(y_means),lty=2)
lines(seq(9,11,by=0.01),
      dnorm(seq(9,11,by=0.01),mean(y_means),sd(y_means)))
arrows(x0=mean(y_means)-sd(y_means),
      y0=0.04,
      x1=mean(y_means)+sd(y_means),
      y1=0.04,angle = 90,code=3)
lines(seq(490,510,by=0.01),
      dnorm(seq(490,510,by=0.01),
            mean(y_means),sd(y_means)))


## sampling distribution
#### 1. exponential distribution
k<-2000
y_matrix<-matrix(rep(NA,n*k),ncol=k)
for(i in 1:k){
  y_matrix[,i]<-rexp(n,rate=1/10)
}

op<-par(mfrow=c(1,2),pty="s")
for(i in 1:k){
  if(i==1){
plot(density(y_matrix[,i]),main="Samples from an \n  Exponential distribution",
     xlab="",ylim=c(0,0.15))
  } else {
lines(density(y_matrix[,i]),lty=2)
  }
}

## compute means from each replication:
y_means<-colMeans(y_matrix)
hist(y_means,freq=FALSE,main="Sampling distribution of means \n (Data sampled \n from Exponential)",
     xlab="means under repeated sampling",
     ylim=c(0,2))
abline(v=mean(y_means),lty=2)
lines(seq(9,11,by=0.01),
      dnorm(seq(9,11,by=0.01),mean(y_means),sd(y_means)))

#### 2. gamma distribution
k<-2000
y_matrix<-matrix(rep(NA,n*k),ncol=k)
for(i in 1:k){
  y_matrix[,i]<-rgamma(n,shape=1,rate=1)
}

op<-par(mfrow=c(1,2),pty="s")
for(i in 1:k){
  if(i==1){
plot(density(y_matrix[,i]),main="Samples from a \n Gamma distribution",
     xlab="")
  } else {
lines(density(y_matrix[,i]),lty=2)
  }
}
## compute means from each replication:
y_means<-colMeans(y_matrix)
hist(y_means,freq=FALSE,main="Sampling distribution of means \n (Data sampled from Gamma)",
     xlab="means under repeated sampling")
abline(v=mean(y_means),lty=2)
lines(seq(.9,1.1,by=0.01),
      dnorm(seq(.9,1.1,by=0.01),mean(y_means),sd(y_means)))


#### 3. cauchy distribution
k<-2000
y_matrix<-matrix(rep(NA,n*k),ncol=k)
for(i in 1:k){
  y_matrix[,i]<-rcauchy(n)
}

op<-par(mfrow=c(1,2),pty="s")
for(i in 1:k){
  if(i==1){
plot(density(y_matrix[,i]),main="Samples from a \n Cauchy distribution",
     xlab="")
  } else {
lines(density(y_matrix[,i]),lty=2)
  }
}
## compute means from each replication:
y_means<-colMeans(y_matrix)
hist(y_means,freq=FALSE,main="Sampling distribution of means \n (Data sampled from Cauchy)",
     xlab="means under repeated sampling")
abline(v=median(y_means),lty=2)
lines(seq(.9,1.1,by=0.01),
      dnorm(seq(.9,1.1,by=0.01),mean(y_means),sd(y_means)))



### Power calculation
sds <- seq(10, 300, by = 1)
lower <- power.t.test(
  delta = 15 - 5,
  sd = sds, n = 10,
  strict = TRUE
)$power
upper <- power.t.test(
  delta = 15 + 5,
  sd = sds, n = 10,
  strict = TRUE
)$power
meanval <- power.t.test(
  delta = 15,
  sd = sds, n = 10,
  strict = TRUE
)$power

plot(sds, meanval,
  type = "l",
  main = "Power curve (n=10)\n
     using power.t.test",
  xlab = "standard deviation",
  ylab = "power",
  yaxs = "i",
  ylim = c(0.05, 0.8)
)
lines(sds, lower, lty = 2)
lines(sds, upper, lty = 2)
text(40, 0.07, "10")
text(50, 0.09, "15")
text(60, 0.11, "20")

library(furrr)
compute_power <- function(nsim = 100000, n = 10,
                          effect = NULL,
                          stddev = NULL) {
  crit_t <- abs(qt(0.025, df = n - 1))
  temp_power <- future_map_dbl(rep(0, nsim), function(i) {
                                 y <- rnorm(n, mean = effect, sd = stddev)
                                 ifelse(abs(t.test(y)$statistic) > crit_t, 1, 0)
                          },
                          .options = furrr_options(seed = TRUE)
  )
  ## return power calculation:
  mean(temp_power)
}


sds <- seq(10, 300, by = 5)
nsim <- 100000
n <- 10
power_meanval <- future_map_dbl(sds,
                                ~compute_power(nsim = nsim, n = n,
                                               effect = 15, .))
power_lowerval <- future_map_dbl(sds,
                                ~compute_power(nsim = nsim, n = n,
                                               effect = 10, .))

power_upperval <- future_map_dbl(sds,
                                ~compute_power(nsim = nsim, n = n,
                                               effect = 20, .))
plot(sds, power_upperval,
  type = "l", lty = 2,
  xlab = "standard deviation",
  ylab = "power",
  main = "Power curve (n=30) \n using simulation",
  ylim = c(0.05, 0.8),
  yaxs = "i"
)
lines(sds, power_lowerval, lty = 2)
lines(sds, power_meanval)

