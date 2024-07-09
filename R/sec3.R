# section 3
source(here("R", "loader.R"))
source(here("R", "utils.R"))


data("df_gg05e1")
gg05e1 <- df_gg05e1
means <- round(with(gg05e1, tapply(rawRT,
    IND = condition,
    mean
)))

means


bysubj <- aggregate(rawRT ~ subject +
                    condition,
                  mean,
                  data = gg05e1
)
byitem <- aggregate(rawRT ~ item +
                    condition,
                  mean,
                  data = gg05e1
)

bysubj %>% t_test(rawRT ~ condition, paired = TRUE, detailed = TRUE)
byitem %>% t_test(rawRT ~ condition, paired = TRUE, detailed = TRUE)

bysubj$cond <- ifelse(bysubj$condition == "objgap", 1, -1)

summary(m0raw <- lm(rawRT ~ condition, bysubj))$coefficients
m1raw <- lm(rawRT ~ cond, bysubj)
round(coef(m1raw))

## extract residuals:
res_m1raw <- residuals(m1raw)
qqnorm(res_m1raw)

round(summary(m0raw)$coefficients, 2)[, c(1:3)]
bysubj %>% t_test(rawRT ~ condition, paired = FALSE, detailed = TRUE)


m0raw_lmer <- lmer(rawRT ~ condition + (1 | subject), bysubj)
summary(m0raw_lmer)$coefficients


## Linear Mixed Models
gg05e1$so <- ifelse(gg05e1$condition == "objgap", 1, -1)
gg05e1$logrt <- log(gg05e1$rawRT)

t(xtabs(~ subject + condition, gg05e1))

lmlist_m1 <- lmList(logrt ~ so | subject, gg05e1)
lmlist_m1$`1`$coefficients


### same t-values
summary_ttest(t.test(coef(lmlist_m1)[2]))

bysubj <- aggregate(log(rawRT) ~ subject + condition,
  mean,
  data = gg05e1
)

colnames(bysubj)[3] <- "logrt"
summary_ttest(t.test(logrt ~ condition, bysubj, 
                     paired = TRUE))

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
