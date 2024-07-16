library(here)
source(here("R", "loader.R"))

data("lexdec")

m <- lm(RT ~ Frequency, lexdec)
summary(m)$coefficients


plot(RT ~ Frequency, lexdec)
abline(m)

y <- lexdec$RT
x <- lexdec$Frequency
m0 <- lm(y ~ x)

## design matrix
X <- model.matrix(m0)
head(X, n = 4)

lexdec %>% names()


m <- lm(RT ~ Frequency + FamilySize + Sex, lexdec)
summary(m)


vif(m)

## design matrix
X <- model.matrix(m0)
head(X, n = 4)

inv_xtx <- solve(t(X) %*% X)
beta_hat <- inv_xtx %*% t(X) %*% y
sigma_hat <- summary(m0)$sigma
var_hat <- sigma_hat^2 * inv_xtx
rho_hat <- var_hat[1, 2] / (sqrt(var_hat[1, 1]) * sqrt(var_hat[2, 2]))

x_c <- scale(x, scale = FALSE)
m <- lm(y ~ x_c)
## design matrix
X <- model.matrix(m)

inv_xtx <- solve(t(X) %*% X)
beta_hat <- inv_xtx %*% t(X) %*% y
sigma_hat <- summary(m)$sigma
var_hat <- sigma_hat^2 * inv_xtx
rho_hat <- var_hat[1, 2] / (sqrt(var_hat[1, 1]) * sqrt(var_hat[2, 2]))
rho_hat %>% round()

## hypothesis testing with ANOVA
m1 <- lm(y ~ x_c)
m0 <- lm(y ~ 1)
anova(m0, m1)

# vif of greater than 5 is a cause for worry.
### Checking model assumptions
qqPlot(residuals(m))
acf(residuals(m))

m <- lmer(RT ~ FreqSingular + FreqPlural + (1 | Subject), lexdec)
acf(residuals(m))

op <- par(mfrow = c(2, 2), pty = "s")
plot(m)

## TODO: check influence.ME package
library(dobson)

data(beetle)
colnames(beetle) <- c("dose", "number", "killed")
beetle

beetle <- beetle %>%
  mutate(propn.dead = killed / number)

beetle %>% with(plot(dose, propn.dead))

### Generalized Linear Model
fm1 <- glm(propn.dead ~ dose,
  family = binomial(logit),
  weights = number,
  data = beetle
)
summary(fm1)

plot(propn.dead ~ dose, beetle)
points(fm1$fitted ~ dose, beetle, pch = 4)


## compute log odds of death for
## concentration 1.7552:
x <- as.matrix(c(1, 1.7552))
# log odds:
log.odds <- t(x) %*% coef(fm1)
### compute CI for log odds:
## Get vcov matrix:
vcovmat <- vcov(fm1)
var.log.odds <- t(x) %*% vcovmat %*% x
lower <- log.odds - 1.96 * sqrt(var.log.odds)
upper <- log.odds + 1.96 * sqrt(var.log.odds)


meanprob <- exp(log.odds) / (1 + exp(log.odds))
lowerprob <- exp(lower) / (1 + exp(lower))
upperprob <- exp(upper) / (1 + exp(upper))

# Multiple Linear Regression
data("df_hindi")
hindi10 <- df_hindi
skip <- ifelse(hindi10$TFT == 0, 1, 0)
hindi10$skip <- skip
## display relevant columns:
head(hindi10[, c(1, 2, 3, 24, 33, 34)])

fm_skip <- glm(skip ~ word_complex + SC, family = binomial(), hindi10)
summary(fm_skip)

### Hypothesis testing in glm
null.glm <- glm(propn.dead ~ 1, binomial(logit),
  weights = number, data = beetle
)
summary(null.glm)

dose.glm <- glm(propn.dead ~ dose, binomial(logit),
  weights = number, data = beetle
)
summary(dose.glm)

anova(null.glm, dose.glm)

anova(dose.glm)

### Assessing goodness of fit of a fitted model
deviance(null.glm)
## critical value:
qchisq(0.95, df = 7) # deviance is bigger

deviance(dose.glm)
qchisq(0.95, df = 6) # deviance is less so the model is fit


op <- par(mfrow = c(2, 2), pty = "s")
plot(dose.glm)
