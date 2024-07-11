library(httpgd)
library(showtext)

showtext_auto()
font_add_google("Lato", "lato")


hgd()

normal <- function(y = NULL, mu = 500, sigma = 100) {
  normalization <- 1 / sqrt(2 * pi * sigma ^ 2)
  exponent <- exp(- (y - mu) ^ 2 / (2 * sigma ^ 2))

  normalization * exponent
}


y <- seq(100, 900, by = 0.01)
plot(y, normal(y), type = "l")


library(bivariate)
library(lingpsych)

data("df_discreteagrmt")
rating_0 <- table(subset(df_discreteagrmt, accuracy == 0)$rating)
rating_1 <- table(subset(df_discreteagrmt, accuracy == 1)$rating)


ratingsbivar <- data.frame(
   rating0 = rating_0,
   rating1 = rating_1
)

ratingsbivar <- ratingsbivar[, c(2,4)]
colnames(ratingsbivar) <- c(0,1)

library(MASS)

f <- bivariate::gbvpmf(as.matrix(ratingsbivar))
plot(f, TRUE, arrows = FALSE)


probs <- attr(f, "p")
t(probs)

library(magrittr)
probs %>% t() %>% rowSums() %>% sum()
probs %>% t() %>% colSums() %>% sum()


std_x <- 5
std_y <- 10
corr <- 0.6
sigma <- matrix(c(std_x ^ 2, std_x * std_y * corr, 
                  std_x * std_y * corr, std_y ^ 2),
                byrow = FALSE, ncol = 2)
u <- mvrnorm(n = 100, mu = c(0, 0), Sigma = sigma)
head(u)

plot(u[,1], u[,2])


### sds

sds <- c(std_x, std_y)
sd_diag <- diag(sds)
corr_matrix <- matrix(c(1.0, corr, corr, 1.0), ncol = 2)

sd_diag %*% corr_matrix %*% sd_diag


# maximum likelihood
x <- rbinom(1, size = 2, prob = 0.5)
theta <- seq(0, 1, by = 0.01)
plot(theta, dbinom(x, size = 2, prob = theta),
  type = "l",
  ylab = "likelihood"
)
abline(v = x / 2)
sum(0.01 * dbinom(x, size = 2, prob = theta))


x1 <- rnorm(1)
x2 <- rnorm(1)

mu <- 1
dnorm(x1, mean = mu) * dnorm(x2, mean = mu)

normal_likelihood <- function(mu = NULL) {
  dnorm(x1, mean = mu) * dnorm(x2, mean = mu)
}

mu <- seq(-3, 3, by = 0.01)
plot(mu, normal_likelihood(mu), type = "l")
abline(v = mean(c(x1, x2)))
