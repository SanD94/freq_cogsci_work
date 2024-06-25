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
