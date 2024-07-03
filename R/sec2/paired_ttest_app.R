# section 2
library(lingpsych)
library(tibble)
library(magrittr)
library(dplyr)
library(ggplot2)
library(here)
source(here("R", "utils.R"))

conditions <- letters[1:4]

dat <- rep(c("subordinate", "main"), each = 2)
adj <- rep(c("subordinate", "main"), 2)
exp_design <- data.frame(conditions, dat, adj)

data("df_levykeller13E1")
head(df_levykeller13E1)

tb_levykeller13e1 <- tibble(df_levykeller13E1)
tb_levykeller13e1 <- tb_levykeller13e1 %>% mutate(
  cond = case_when(
    dat == "sub" & adj == "sub" ~ "a",
    dat == "sub" & adj == "main" ~ "b",
    dat == "main" & adj == "sub" ~ "c",
    dat == "main" & adj == "main" ~ "d"
  )
)

# sanity check
xtabs(~ cond + dat, tb_levykeller13e1)
tb_levykeller13e1 %>%
  xtabs(~ subj + cond, .) %>%
  t()

tb_levykeller13e1 %>%
  dplyr::filter(cond == "a") %>%
  pull(TFT) %>%
  length()

bysubj <- tb_levykeller13e1 %>%
  group_by(subj, cond) %>%
  summarise(TFT = mean(TFT))

bysubj %>%
  xtabs(~ subj + cond, .) %>%
  t()
ggplot(bysubj, aes(x = cond, y = TFT)) +
  geom_boxplot(outlier.colour = NA, width = .4) +
  geom_dotplot(binaxis = "y", stackdir = "center", fill = NA) +
  xlab("The four conditions in Levy and Keller 2013") +
  ylab("Total fixation time (ms)") +
  theme_bw()


conds <- bysubj %>%
  group_by(cond) %>%
  group_split() %>%
  future_map(function(x) x$TFT)


mean_ab <- (conds[[1]] + conds[[2]]) / 2
mean_cd <- (conds[[3]] + conds[[4]]) / 2

me_dat_res <- t.test(mean_ab, mean_cd, paired = TRUE)
summary_ttest(me_dat_res)

mean_ac <- (conds[[1]] + conds[[3]]) / 2
mean_bd <- (conds[[2]] + conds[[4]]) / 2

me_adj_res <- t.test(mean_ac, mean_bd, paired = TRUE)
summary_ttest(me_adj_res)
