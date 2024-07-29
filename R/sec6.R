library(here)
source(here("R", "loader.R"))

data("df_contrasts1")
df_contrasts1

table1 <- df_contrasts1 %>%
  group_by(F) %>% # Table for main effect F
  dplyr::summarize(N = n(), M = mean(DV), SD = sd(DV), SE = round(SD / sqrt(N), 1))
table1a <- as.data.frame(table1)
names(table1a) <- c("Factor", "N data", "Est. means", "Std. dev.", "Std. errors")
GM <- mean(table1$M) # Grand Mean

plot1 <- qplot(x = F, y = M, group = 1, data = table1, geom = c("point", "line")) +
  geom_errorbar(aes(max = M + SE, min = M - SE), width = 0) +
  # scale_y_continuous(breaks=c(250,275,300)) +
  scale_y_continuous(breaks = seq(0, 1, .2)) + coord_cartesian(ylim = c(0, 1)) +
  labs(y = "Mean Response Time [sec]", x = "Factor F") +
  theme_bw()


fit_F_brm <- brm(DV ~ 1 + F, data = df_contrasts1)
summary(fit_F_brm)

fit_F <- lm(DV ~ 1 + F, data = df_contrasts1)
round(summary(fit_F)$coefficients, 3)


contrasts(df_contrasts1$F)


df_contrasts1$Fb <- factor(df_contrasts1$F,
  levels = c("F2", "F1")
)
contrasts(df_contrasts1$Fb)

fit_Fb <- lm(DV ~ 1 + Fb,
  data = df_contrasts1
)

round(summary(fit_Fb)$coefficients, 3)


contrasts(df_contrasts1$F) <- c(-0.5, +0.5)
fit_mSum <- lm(DV ~ 1 + F,
  data = df_contrasts1
)
round(summary(fit_mSum)$coefficients, 3)


## Hypothesis Matrix
data("df_contrasts2")
head(df_contrasts2)

## Defining a custom contrast matrix involves four steps:

### Write down the estimated comparisons or hypotheses
### Extract the weights and write them into what we will call a hypothesis matrix
### Apply the generalized matrix inverse to the hypothesis matrix to create the contrast matrix
### Assign the contrast matrix to the factor and run the linear (mixed) model


hc_sum <- rbind(
  cH00 = c(adjectives = 1 / 3, nouns = 1 / 3, verbs = 1 / 3),
  cH01 = c(adjectives = +2 / 3, nouns = -1 / 3, verbs = -1 / 3),
  cH02 = c(adjecctives = -1 / 3, nouns = +2 / 3, verbs = -1 / 3)
)
fractions(t(hc_sum))

# define a function to make the output nicer
ginv2 <- function(x) {
  fractions(provideDimnames(ginv(x),
    base = dimnames(x)[2:1]
  ))
}

xc_sum <- ginv2(hc_sum)

fractions(cbind(1, contr.sum(3)))
