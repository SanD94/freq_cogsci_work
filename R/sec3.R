# section 3
library(here)
source(here("R", "loader.R"))
source(here("R", "utils.R"))


data("df_gg05e1")
tb_gg05e1 <- df_gg05e1 %>%
  tibble()

means <- tb_gg05e1 %>%
  group_by(condition) %>%
  summarise(rawRT = round(mean(rawRT))) %>%
  ungroup()
means

bysubj <- tb_gg05e1 %>%
  group_by(subject, condition) %>%
  summarise(rawRT = mean(rawRT)) %>%
  ungroup()

byitem <- tb_gg05e1 %>%
  group_by(item, condition) %>%
  summarise(rawRT = mean(rawRT)) %>%
  ungroup()

# sanity check
t(xtabs(~ subject + condition, bysubj))
t(xtabs(~ item + condition, byitem))

bysubj %>% t_test(rawRT ~ condition, paired = TRUE, detailed = TRUE)
byitem %>% t_test(rawRT ~ condition, paired = TRUE, detailed = TRUE)
## aggregate t-test

# lmer -- contrast coding
m0rawsubj <- lmer(rawRT ~ condition + (1 | subject), bysubj)
summary(m0rawsubj)$coefficients

m0rawitem <- lmer(rawRT ~ condition + (1 | item), byitem)
summary(m0rawitem)$coefficients
## same with t-tests above

m0raw <- lm(rawRT ~ condition, bysubj)
summary(m0raw)$coefficients

bysubj <- bysubj %>% mutate(
  cond = if_else(condition == "objgap", 1, -1)
)
m1raw <- lm(rawRT ~ cond, bysubj)
round(coef(m1raw))
summary(m1raw)$coefficients

## extract residuals:
res_m1raw <- residuals(m1raw)
qqnorm(res_m1raw)

### log transform gives median not norm if you transfer back to raw values
### https://statproofbook.github.io/P/lognorm-med.html
m1log <- lm(log(rawRT) ~ cond, bysubj)
res_m1log <- residuals(m1log)
qqnorm(res_m1log)

round(summary(m0raw)$coefficients, 2)[, c(1:3)]
bysubj %>% t_test(rawRT ~ condition, paired = FALSE, detailed = TRUE)


m0raw_lmer <- lmer(rawRT ~ condition + (1 | subject), bysubj)
summary(m0raw_lmer)$coefficients


## Linear Mixed Models
tb_gg05e1 <- tb_gg05e1 %>%
  mutate(
    so = if_else(condition == "objgap", 1, -1),
    logrt = log(rawRT)
  )

t(xtabs(~ subject + condition, tb_gg05e1))


gg_xyplot(
  so, logrt, ~subject,
  shape = 1, size = 3,
  data = tb_gg05e1
)

t(xtabs(~ subject + condition, tb_gg05e1))

## seperate linear models for each subject
lmlist_m1 <- lmList(logrt ~ so | subject, tb_gg05e1)
lmlist_m1$`1`$coefficients

plot(tb_gg05e1$so,
  tb_gg05e1$logrt,
  axes = FALSE,
  xlab = "condition",
  ylab = "log rt"
)
axis(1,
  at = c(-1, 1),
  labels = c("SR", "OR")
)
axis(2)

subjects <- 1:42

for (i in subjects) {
  abline(lmlist_m1[[i]])
}

abline(lm(tb_gg05e1$logrt ~ tb_gg05e1$so), lwd = 5, col = "blue")

### same t-values in different models
summary_ttest(t.test(coef(lmlist_m1)[2]))
bysubj <- tb_gg05e1 %>%
  group_by(condition, subject) %>%
  summarise(logrt = mean(log(rawRT))) %>%
  ungroup()
bysubj %>% t_test(logrt ~ condition, paired = TRUE, detailed = TRUE)

summary(lmer(
  logrt ~ condition + (1 | subject),
  bysubj
))$coefficients[2, ]
###

### Estimates are same but std is different in two models
m0_lmer <- lmer(logrt ~ so + (1 | subject), gg05e1)
summary(m0_lmer)

m0_lm <- lm(logrt ~ so, gg05e1)
summary(m0_lm)

## first 10 subjects' intercept adjustments:
ranef(m0_lmer)$subject[, 1][1:10]

dotplot(ranef(m0_lmer, condVar = TRUE))

ranefu0 <- ranef(m0_lmer, condVar = TRUE) %>% as_tibble()

ranefu0_se <- ranefu0 %>% mutate(
  lwr = condval - 1.96 * condsd,
  upr = condval + 1.96 * condsd
)

u0 <- ranefu0_se %>% pull(condval)
u0_sd <- ranefu0_se %>% pull(condsd) # standard deviation

b0 <- summary_m0_lmer$coefficients[1, 1]
b0_se <- summary_m0_lmer$coefficients[1, 2] # standard error

b0_u0 <- b0 + u0 # expectation mean
b0_u0_sd <- sqrt(b0_se^2 + u0_sd^2)
b0_u0_lower <- b0_u0 - 2 * b0_u0_sd
b0_u0_upper <- b0_u0 + 2 * b0_u0_sd

bysubj_intercept <- tibble(
  b = b0_u0,
  lower = b0_u0_lower,
  upper = b0_u0_upper,
  subj = factor(1:42)
) %>%
  arrange(b) %>%
  mutate(
    subj = factor(subj, levels = as.numeric(subj))
  )


bysubj_intercept %>%
  ggplot(aes(x = b, y = subj)) +
  geom_errorbar(width = .1, aes(xmin = lower, xmax = upper)) +
  geom_point(shape = 21, size = 3, fill = "white") +
  ylab("subject id") +
  xlab("intercept estimate (log ms)") +
  theme_bw()

## m0_lm has one intercept b_0 for all subjects
## m0_lmer has a different intercept b_0 for each subject i

a <- fixef(m0_lmer)[1]
newa <- a + ranef(m0_lmer)$subj

ab <- data.frame(newa = newa, b = fixef(m0_lmer)[2])

plot(gg05e1$so, gg05e1$logrt, xlab = "condition", ylab = "log rt", axes = F)
axis(1, at = c(-1, 1), labels = c("SR", "OR"))
axis(2)

for (i in 1:42) {
  abline(a = ab[i, 1], b = ab[i, 2])
}

abline(lm(logrt ~ so, gg05e1), lwd = 3, col = "blue")


## Model Type 2 : varying intercepsts and varying slopes
## without a correlation
m1_lmer <- lmer(logrt ~ so + (1 + so || subject), gg05e1)
summary(m1_lmer)

a <- fixef(m1_lmer)[1]
b <- fixef(m1_lmer)[2]

newa <- a + ranef(m1_lmer)$subject[1]
newb <- b + ranef(m1_lmer)$subject[2]

ab <- data.frame(newa = newa, b = newb)

plot(gg05e1$so, gg05e1$logrt,
  xlab = "condition",
  ylab = "log rt", axes = F,
  main = "varying intercepts and slopes for each subject"
)
axis(1, at = c(-1, 1), labels = c("SR", "OR"))
axis(2)

for (i in 1:42) {
  abline(a = ab[i, 1], b = ab[i, 2])
}

abline(lm(logrt ~ so, gg05e1), lwd = 3, col = "blue")

## Comparing lmList model with varying intercepts model
op <- par(mfrow = c(1, 2), pty = "s")

plot(gg05e1$so, gg05e1$logrt,
  axes = FALSE, xlab = "condition",
  ylab = "log rt", main = "ordinary linear model"
)
axis(1, at = c(-1, 1), labels = c("SR", "OR"))
axis(2)

subjects <- 1:42

lmlistcoef <- coef(lmlist_m1)
a_lmlist <- lmlistcoef$`(Intercept)`
b_lmlist <- lmlistcoef$so

for (i in subjects) {
  abline(a = a_lmlist[i], b = b_lmlist[i])
}

abline(lm(logrt ~ so, gg05e1), lwd = 3, col = "blue")

a <- fixef(m1_lmer)[1]
b <- fixef(m1_lmer)[2]

newa <- a + ranef(m1_lmer)$subj[1]
newb <- b + ranef(m1_lmer)$subj[2]

ab <- data.frame(newa = newa, b = newb)

plot(gg05e1$so, gg05e1$logrt,
  axes = FALSE,
  main = "varying intercepts and slopes",
  xlab = "condition", ylab = "log rt"
)
axis(1, at = c(-1, 1), labels = c("SR", "OR"))
axis(2)

for (i in 1:42) {
  abline(a = ab[i, 1], b = ab[i, 2])
}

abline(lm(logrt ~ so, gg05e1), lwd = 3, col = "blue")

### subject best fit line in the linear mixed model
### is gravitating towards the grand fit in the simple linear model

### Visualizing random effects of Model 2
dotplot(ranef(m1_lmer, condVar = TRUE))$subject


## Crossed random effects for subjects and for items
head(xtabs(~ subject + item, gg05e1))
m2_lmer <- lmer(logrt ~ so + (1 + so || subject) +
  (1 + so || item), gg05e1)

dotplot(ranef(m2_lmer, condVar = TRUE), layout = c(2, 1))

### Model type 3: Varying intercepts and varying slopes, with correlation
m3_lmer <- lmer(
  logrt ~ so + (1 + so | subject) + (1 + so | item),
  gg05e1
)

summary(m3_lmer)
dotplot(ranef(m3_lmer, condVar = TRUE), layout = c(2, 1))

# corr btw slope and intercept for subject var
plot(ranef(m3_lmer)$subject[, 1], ranef(m3_lmer)$subject[, 2],
  xlab = "Intercept adjustments (subjects)",
  ylab = "Slope adjustments (subjects)"
)

m4_lmer <- lmer(
  logrt ~ so + (1 + so | subject) + (1 + so || item),
  gg05e1
)
summary(m4_lmer)
