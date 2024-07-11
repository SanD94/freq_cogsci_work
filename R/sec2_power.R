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
    ~compute_power(nsim = nsim, n = n, effect = 15, .))
power_lowerval <- future_map_dbl(sds,
  ~compute_power(nsim = nsim, n = n, effect = 10, .))

power_upperval <- future_map_dbl(sds,
  ~compute_power(nsim = nsim, n = n, effect = 20, .))
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


### Type S and M error
## probable effect size, derived from past studies:
D <- 15
## SE from the study of interest
se <- 46
N <- 37
sd <- se * sqrt(N)
nsim <- 10000
drep <- future_map_dbl(rep(0, nsim),
  function(x) {
    samp <- rnorm(N, mean = D, sd = sd)
    mean(samp)
  },
  .options = furrr_options(seed = TRUE)
)

pow <- mean(ifelse(abs(drep / se) > 2, 1, 0))

signif <- which(abs(drep / se) > 2)

## type S error rate / signif:
types_sig <- mean(drep[signif] < 0)

## type M error rate / signif
typem_sig <- mean(abs(drep[signif]) / D)


## funnel plot
truemu <- 15

sampsize <- seq(10, 10000, by = 10)
elem <- future_map(sampsize, function(n) {
  y <- rnorm(n, mean = truemu, sd = 250)
  sig <- ifelse(t.test(y)$p.value < 0.05, 1, 0)
  means <- mean(y)
  power <- power.t.test(d = truemu, sd = 250, n = n)$power
  data.frame(sig, means, power)
}, .options = furrr_options(seed = TRUE)) %>% list_rbind()


plot(jitter(elem$means), jitter(elem$power),
  main = "Funnel plot",
  xlim = range(c(min(elem$means), max(elem$means))),
  xlab = "effect", ylab = "power",
  cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.5
)
abline(v = 15)
elem_sig <- elem %>% dplyr::filter(sig == 1)
points(elem_sig$means, elem_sig$power, pch = 21, bg = "#CACACA")

# searching for significane
n <- 15
nsim <- 10000
stddev <- 250
mn <- 0
res <- future_map(1:nsim, function(i) {
  samp <- rnorm(n, mean = mn, sd = stddev)
  test <- t.test(samp)
  pvals <- test$p.value
  t_stat <- test$statistic
  data.frame(pvals, t_stat)
}, .options = furrr_options(seed = TRUE)) %>% list_rbind()

mean(res$pvals < 0.05) %>% round(2)


## how many subjects can I run?
upper_bound <- 6 * n
res_inc <- future_map(1:nsim, function(i) {
  sig <- FALSE
  x <- rnorm(n, mean = mn, sd = stddev)
  while (!sig && length(x) < upper_bound) {
    if (t.test(x)$p.value > 0.05)
    x <- append(x, rnorm(n, mean = mn, sd = stddev))
    else
    sig <- TRUE
  }
  test <- t.test(x)
  pvals <- test$p.value
  t_stat <- test$statistic
  data.frame(pvals, t_stat)
}, .options = furrr_options(seed = TRUE)) %>% list_rbind()

mean(res_inc$pvals < 0.05) %>% round(2)

plot(density(res$t_stat), main = "", xlab = "t-value")
lines(density(res_inc$t_stat), lty = 2)

