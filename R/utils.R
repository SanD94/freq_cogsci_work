# parallelization is needed for simulations
library(future)
library(doFuture)
plan(multisession)

tic <- function() {
  tic_start <<- base::Sys.time()
}

toc <- function() {
  dt <- base::difftime(base::Sys.time(), tic_start)
  dt <- round(dt, digits = 1L)
  message(paste(format(dt), "since tic()"))
}


slow_sum <- function(x) {
  sum <- 0
  for (value in x) {
    Sys.sleep(1.0)
    sum <- sum + value
  }
  sum
}


tic()
fa %<-% slow_sum(1:5)
toc()
fb %<-% slow_sum(6:10)
toc()
y <- fa + fb
toc()
y
toc()

# create tasks with foreach par loop
tic()
xs <- list(1:5, 6:10, 11:15, 16:20)
ys <- list()
ys <- foreach(ii = seq_along(xs)) %dofuture% {
  message(paste0("Iteration ", ii))
  slow_sum(xs[[ii]])
}
message("Done")
print(ys)

ys <- unlist(ys)
ys

y <- sum(ys)
y
toc()
