# parallelization is needed for simulations
# main message : don't use foreach whatsoever! it's unbearibly slow.
library(future)
plan(multisession)

tic <- function() {
  tic_start <<- base::Sys.time()
}

toc <- function() {
  dt <- base::difftime(base::Sys.time(), tic_start)
  dt <- round(dt, digits = 1L)
  message(paste(format(dt), "since tic()"))
}


summary_ttest <- function(res, paired = TRUE, units = "ms") {
  obs_t <- round(res$statistic, 2)
  dfs <- round(res$parameter)
  pval <- round(res$p.value, 3)
  ci <- round(res$conf.int, 2)
  est <- round(res$estimate, 2)

  res <- list(
    obs_t = obs_t, dfs = dfs,
    pval = pval, ci = ci,
    est = est, units = units,
    paired = paired
  )
  print_ttest(res)
}

print_ttest <- function(res) {
  cat(paste(
    paste("t(", res$dfs, ") = ", res$obs_t, sep = ""),
    paste("p = ", res$pval, sep = ""), "\n"
  ))
  if (res$paired == TRUE) {
    print_ci_paired(res$est, res$ci, res$units)
  } else {
    print_ci(res$est, res$ci, res$units)
  }
}

print_ci_paired <- function(est, ci, units) {
  cat(paste("est. : ", est,
    " [", ci[1], ", ", ci[2], "] ",
    units,
    sep = ""
  ))
  cat("\n")
}

print_ci <- function(est, ci, units) {
  cat(paste(
    paste("est. 1:", est[1], units),
    paste("est. 2:", est[2], units),
    paste("CI of diff. in means: [", ci[1], ", ", ci[2], "] ",
      units,
      sep = ""
    ),
    sep = "\n"
  ))
  cat("\n")
}

gg_xyplot <- function(x, y, formula, shape, size, data) {
  data %>% ggplot(aes(
    x = {{ x }},
    y = {{ y }}
  )) +
    facet_wrap(formula) +
    geom_smooth(method = "lm") +
    geom_point(color = "blue", shape = shape, size = size) +
    theme(panel.grid.minor = element_blank()) +
    theme_bw() +
    #     scale_x_discrete(breaks=c("-1","0.5","0","0.5","1"),labels=c("OR","", "", "", "SR"))+
    scale_x_continuous(breaks = round(seq(-1, 1, by = 2), 1)) +
    ylab(quo({{ y }})) +
    xlab("condition (-1: SR, +1: OR)")
}
