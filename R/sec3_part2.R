data("df_gg05e1")

gg05e1 <- df_gg05e1 %>% tibble()
gg05e1 <- gg05e1 %>% mutate(
  so = if_else(condition == "objgap", 1, -1),
  logrt = log(rawRT)
)

m0_lmer <- lmer(logrt ~ so + (1 | subject), gg05e1)
summary_m0_lmer <- summary(m0_lmer)

m0_lm <- lm(logrt ~ so, gg05e1)
summary(m0_lm)

dotplot(ranef(m0_lmer, condVar = TRUE))

ranefu0 <- ranef(m0_lmer, condVar = TRUE) %>% as_tibble()

ranefu0_se <- ranefu0 %>% mutate(
  lwr = condval - 1.96 * condsd,
  upr = condval + 1.96 * condsd
)

u0 <- ranefu0_se %>% pull(condval)
u0_sd <- ranefu0_se %>% pull(condsd)

b0 <- summary_m0_lmer$coefficients[1,1]
b0_sd <- summrary_m0_lmer$coefficients[1,2]

b0_u0 <- b0 + u0 # expectation mean
b0_u0_sd <- sqrt(b0_sd^2 + u0_sd^2)
