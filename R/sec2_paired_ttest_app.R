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


# to test whether there is an interaction between rc and noun in hard condition
tb_fedorenko <- df_fedorenko06 %>%
  tibble() %>%
  group_by(rctype, nountype, load, subj) %>%
  summarise(RT = mean(RT)) %>%
  ungroup()

cond_obj_occ_hard <- tb_fedorenko %>%
  filter(rctype == "obj" & nountype == "occ" & load == "hard") %>%
  pull(RT)

cond_subj_occ_hard <- tb_fedorenko %>%
  filter(rctype == "subj" & nountype == "occ" & load == "hard") %>%
  pull(RT)

diff_occ_hard <- cond_obj_occ_hard - cond_subj_occ_hard

cond_obj_name_hard <- tb_fedorenko %>%
  filter(rctype == "obj" & nountype == "name" & load == "hard") %>%
  pull(RT)

cond_subj_name_hard <- tb_fedorenko %>%
  filter(rctype == "subj" & nountype == "name" & load == "hard") %>%
  pull(RT)

diff_name_hard <- cond_obj_name_hard - cond_subj_name_hard

hard_int_res <- t.test(diff_name_hard, diff_occ_hard, paired = TRUE)
summary_ttest(hard_int_res)

# to test whether there is an interaction between rc and noun in easy condition
cond_obj_occ_easy <- tb_fedorenko %>%
  filter(rctype == "obj" & nountype == "occ" & load == "easy") %>%
  pull(RT)

cond_subj_occ_easy <- tb_fedorenko %>%
  filter(rctype == "subj" & nountype == "occ" & load == "easy") %>%
  pull(RT)

diff_occ_easy <- cond_obj_occ_easy - cond_subj_occ_easy

cond_obj_name_easy <- tb_fedorenko %>%
  filter(rctype == "obj" & nountype == "name" & load == "easy") %>%
  pull(RT)

cond_subj_name_easy <- tb_fedorenko %>%
  filter(rctype == "subj" & nountype == "name" & load == "easy") %>%
  pull(RT)

diff_name_easy <- cond_obj_name_easy - cond_subj_name_easy

easy_int_res <- t.test(diff_name_easy, diff_occ_easy, paired = TRUE)
summary_ttest(easy_int_res)

# paired t-test and correspondence with anova
fedorenko_anova <- tb_fedorenko %>% anova_test(
   dv = RT,
   wid = subj,
   within = c(rctype, nountype, load)
)

get_anova_table(fedorenko_anova)


# main effects

## rctype
rc_fedorenko <- tb_fedorenko %>%
  group_by(rctype, subj) %>%
  summarise(RT = mean(RT)) %>%
  ungroup()

rc_obj_fedorenko <- rc_fedorenko %>%
  filter(rctype == "obj") %>%
  pull(RT)
rc_subj_fedorenko <- rc_fedorenko %>%
  filter(rctype == "subj") %>%
  pull(RT)

rc_main_res <- t.test(rc_obj_fedorenko, rc_subj_fedorenko, paired = TRUE)
summary_ttest(rc_main_res)

## nountype
noun_fedorenko <- tb_fedorenko %>%
  group_by(nountype, subj) %>%
  summarise(RT = mean(RT)) %>%
  ungroup()

noun_occ_fedorenko <- noun_fedorenko %>%
  filter(nountype == "occ") %>%
  pull(RT)
noun_name_fedorenko <- noun_fedorenko %>%
  filter(nountype == "name") %>%
  pull(RT)

noun_main_res <- t.test(noun_occ_fedorenko, noun_name_fedorenko, paired = TRUE)
summary_ttest(noun_main_res)

## load
load_fedorenko <- tb_fedorenko %>%
  group_by(load, subj) %>%
  summarise(RT = mean(RT)) %>%
  ungroup()

load_easy_fedorenko <- load_fedorenko %>%
  filter(load == "easy") %>%
  pull(RT)
load_hard_fedorenko <- load_fedorenko %>%
  filter(load == "hard") %>%
  pull(RT)

load_main_res <- t.test(load_easy_fedorenko, load_hard_fedorenko, paired = TRUE)
summary_ttest(load_main_res)

# interactions
## rctype x nountype
rc_noun_fedorenko <- tb_fedorenko %>%
  group_by(rctype, nountype, subj) %>%
  summarise(RT = mean(RT)) %>%
  ungroup()

obj_occ_fedorenko <- rc_noun_fedorenko %>%
  filter(rctype == "obj" & nountype == "occ") %>%
  pull(RT)
obj_name_fedorenko <- rc_noun_fedorenko %>%
  filter(rctype == "obj" & nountype == "name") %>%
  pull(RT)
subj_occ_fedorenko <- rc_noun_fedorenko %>%
  filter(rctype == "subj" & nountype == "occ") %>%
  pull(RT)
subj_name_fedorenko <- rc_noun_fedorenko %>%
  filter(rctype == "subj" & nountype == "name") %>%
  pull(RT)

diff_obj_noun_fedorenko <- obj_occ_fedorenko - obj_name_fedorenko
diff_subj_noun_fedorenko <- subj_occ_fedorenko - subj_name_fedorenko
int_rc_noun_res <- t.test(
  diff_obj_noun_fedorenko,
  diff_subj_noun_fedorenko,
  paired = TRUE
)
summary_ttest(int_rc_noun_res)


## rctype x load
rc_load_fedorenko <- tb_fedorenko %>%
  group_by(rctype, load, subj) %>%
  summarise(RT = mean(RT)) %>%
  ungroup()

obj_easy_fedorenko <- rc_load_fedorenko %>%
  filter(rctype == "obj" & load == "easy") %>%
  pull(RT)
obj_hard_fedorenko <- rc_load_fedorenko %>%
  filter(rctype == "obj" & load == "hard") %>%
  pull(RT)
subj_easy_fedorenko <- rc_load_fedorenko %>%
  filter(rctype == "subj" & load == "easy") %>%
  pull(RT)
subj_hard_fedorenko <- rc_load_fedorenko %>%
  filter(rctype == "subj" & load == "hard") %>%
  pull(RT)

diff_obj_load_fedorenko <- obj_easy_fedorenko - obj_hard_fedorenko
diff_subj_load_fedorenko <- subj_easy_fedorenko - subj_hard_fedorenko
int_rc_load_res <- t.test(
  diff_obj_load_fedorenko,
  diff_subj_load_fedorenko,
  paired = TRUE
)
summary_ttest(int_rc_load_res)

## nountype x load
noun_load_fedorenko <- tb_fedorenko %>%
  group_by(nountype, load, subj) %>%
  summarise(RT = mean(RT)) %>%
  ungroup()

occ_easy_fedorenko <- noun_load_fedorenko %>%
  filter(nountype == "occ" & load == "easy") %>%
  pull(RT)
occ_hard_fedorenko <- noun_load_fedorenko %>%
  filter(nountype == "occ" & load == "hard") %>%
  pull(RT)
name_easy_fedorenko <- noun_load_fedorenko %>%
  filter(nountype == "name" & load == "easy") %>%
  pull(RT)
name_hard_fedorenko <- noun_load_fedorenko %>%
  filter(nountype == "name" & load == "hard") %>%
  pull(RT)

diff_occ_load_fedorenko <- occ_easy_fedorenko - occ_hard_fedorenko
diff_name_load_fedorenko <- name_easy_fedorenko - name_hard_fedorenko
int_noun_load_res <- t.test(
  diff_occ_load_fedorenko,
  diff_name_load_fedorenko,
  paired = TRUE
)
summary_ttest(int_noun_load_res)

## rctype x nountype x load
### rctype  "obj"
obj_occ_easy_fd <- tb_fedorenko %>%
  filter(rctype == "obj" & nountype == "occ" & load == "easy") %>%
  pull(RT)
obj_occ_hard_fd <- tb_fedorenko %>%
  filter(rctype == "obj" & nountype == "occ" & load == "hard") %>%
  pull(RT)
obj_name_easy_fd <- tb_fedorenko %>%
  filter(rctype == "obj" & nountype == "name" & load == "easy") %>%
  pull(RT)
obj_name_hard_fd <- tb_fedorenko %>%
  filter(rctype == "obj" & nountype == "name" & load == "hard") %>%
  pull(RT)

diff_obj_int_noun_load <- (obj_occ_easy_fd - obj_occ_hard_fd) -
  (obj_name_easy_fd - obj_name_hard_fd)


### rctype  "subj"
subj_occ_easy_fd <- tb_fedorenko %>%
  filter(rctype == "subj" & nountype == "occ" & load == "easy") %>%
  pull(RT)
subj_occ_hard_fd <- tb_fedorenko %>%
  filter(rctype == "subj" & nountype == "occ" & load == "hard") %>%
  pull(RT)
subj_name_easy_fd <- tb_fedorenko %>%
  filter(rctype == "subj" & nountype == "name" & load == "easy") %>%
  pull(RT)
subj_name_hard_fd <- tb_fedorenko %>%
  filter(rctype == "subj" & nountype == "name" & load == "hard") %>%
  pull(RT)

diff_subj_int_noun_load <- (subj_occ_easy_fd - subj_occ_hard_fd) -
  (subj_name_easy_fd - subj_name_hard_fd)


int_rc_noun_load_res <- t.test(
  diff_obj_int_noun_load,
  diff_subj_int_noun_load,
  paired = TRUE
)
summary_ttest(int_rc_noun_load_res)
