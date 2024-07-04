# section 2
source(here("R", "loader.R"))
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


# main effect of dative
mean_ab <- (conds[[1]] + conds[[2]]) / 2
mean_cd <- (conds[[3]] + conds[[4]]) / 2

me_dat_res <- t.test(mean_ab, mean_cd, paired = TRUE)
summary_ttest(me_dat_res)

# main effect of adjunct
mean_ac <- (conds[[1]] + conds[[3]]) / 2
mean_bd <- (conds[[2]] + conds[[4]]) / 2

me_adj_res <- t.test(mean_ac, mean_bd, paired = TRUE)
summary_ttest(me_adj_res)

diff_ab <- conds[[1]] - conds[[2]]
diff_cd <- conds[[3]] - conds[[4]]

int_dat_adj_res <- t.test(diff_ab, diff_cd, paired = TRUE)
summary_ttest(int_dat_adj_res)

# repeated measure anova
bysubj2 <- tb_levykeller13e1 %>%
  group_by(subj, adj, dat) %>%
  summarise(TFT = mean(TFT)) %>%
  ungroup()
subj_anova <- bysubj2 %>% anova_test(
    dv = TFT,
    wid = subj,
    within = c(adj, dat)
)

get_anova_table(subj_anova)

# analyzing 2x2x2 repeated measure design with paired t-tests
data("df_fedorenko06")
head(df_fedorenko06)


fedorenko06_conds <- df_fedorenko06 %>%
  tibble() %>%
  group_by(rctype, nountype, load, subj) %>%
  summarise(RT = mean(RT)) %>%
  mutate(id = cur_group_id(), id_name = list(cur_group()))

id_names <- fedorenko06_conds %>% pull(id_name) %>% unique()

fedorenko06_conds <- fedorenko06_conds %>%
  group_split() %>%
  future_map(function(x) x$RT)

conds <- fedorenko06_conds
cond_obj_occ_hard <- get_cond(
    conds,
    id_names,
    tibble(rctype = "obj", nountype = "occ", load = "hard")
)
cond_subj_occ_hard <- get_cond(
    conds,
    id_names,
    tibble(rctype = "subj", nountype = "occ", load = "hard")
)

diff_occ_hard <- cond_obj_occ_hard - cond_subj_occ_hard

cond_obj_name_hard <- get_cond(
    conds,
    id_names,
    tibble(rctype = "obj", nountype = "name", load = "hard")
)
cond_subj_name_hard <- get_cond(
    conds,
    id_names,
    tibble(rctype = "subj", nountype = "name", load = "hard")
)
diff_name_hard <- cond_obj_name_hard - cond_subj_name_hard

hard_int_res <- t.test(diff_name_hard, diff_occ_hard, paired = TRUE)
summary_ttest(hard_int_res)





# compute difference between OR and SR in Noun Type occ

