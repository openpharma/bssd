
library(bssd)
set.seed(2021)
x1 <- stan_map_summary_data(events = c(1, 0, 5),
                            n = c(10, 5, 55),
                            time = log(c(1, 0.5, 1.2)))
set.seed(2021)
x2 <- stan_map_summary_data(events = c(1, 0, 5),
                            n = c(10, 5, 55),
                            time = log(c(1, 0.5, 1.2)))
set.seed(123)
x3 <- stan_map_summary_data(events = c(1, 0, 5),
                            n = c(10, 5, 55),
                            time = log(c(1, 0.5, 1.2)))
x1$mix
x2$mix
# Same
x3$mix
# Different
