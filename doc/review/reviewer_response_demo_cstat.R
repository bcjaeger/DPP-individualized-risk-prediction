

set.seed(123)

n <- 10000

data_common <- tibble(time = round(runif(n) * 100),
                      x = runif(n, min = 0.5, max = 1),
                      status = rbinom(n, size = 1, prob = x))

data_rare <- tibble(time = round(runif(n) * 100),
                      x = runif(n, min = 0, max = 0.5),
                      status = rbinom(n, size = 1, prob = x))

# outcome is more common => lower C of 0.562
summary(coxph(Surv(time, status) ~ x, data = data_common))$concordance

# outcome is more rare => Higher C of 0.656
summary(coxph(Surv(time, status) ~ x, data = data_rare))$concordance
