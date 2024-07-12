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
