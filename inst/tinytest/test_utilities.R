library(tinytest)

####################
# search = bisection
####################

# csm_get_at_t()
# t on time interval
# t_ind follows daily time step
t <- seq(0, 100, length.out = 11)
t_ind <- 0:100
dummy_variable <- t_ind*2

actual <-
  lapply(t, \(.t){
    csmbuilder::csm_get_at_t(dummy_variable, t_ind, t = .t,
                               method = "linear",
                               search = "bisection")
  }) |>
  unlist()

target <- t*2

expect_equal(actual, target,
             info = "csm_get_at_t(\"linear\") daily time step")

# csm_get_at_t()
# t between time interval
# t_ind follows daily time step
t <- c(0, seq(0.5, 99.5, length.out = 9), 100)
t_ind <- 0:100
dummy_variable <- t_ind*2

actual <-
  lapply(t, \(.t){
    csmbuilder::csm_get_at_t(dummy_variable, t_ind, t = .t,
                               method = "linear",
                               search = "bisection")
  }) |>
  unlist()

target <- t*2

expect_equal(actual, target,
             info = "csm_get_at_t(\"linear\") daily time step between interval")

# csm_get_at_t()
# t on time interval
# t_ind follows hourly time step (with t in day units)
t <- seq(0, 100, length.out = 11)
t_ind <- seq(0, 100, length.out = 100*24+1)
dummy_variable <- t_ind*2

actual <-
  lapply(t, \(.t){
    csmbuilder::csm_get_at_t(dummy_variable, t_ind, t = .t,
                               method = "linear",
                               search = "bisection")
  }) |>
  unlist()

target <- t*2

expect_equal(actual, target,
             info = "csm_get_at_t(\"linear\") hourly time step")

# csm_get_at_t()
# t between time interval
# t_ind follows hourly time step (with t in day units)
t <- c(0, seq(0.5, 99.5, length.out = 9), 100)
t_ind <- seq(0, 100, length.out = 100*24+1)
dummy_variable <- t_ind*2

actual <-
  lapply(t, \(.t){
    csmbuilder::csm_get_at_t(dummy_variable, t_ind, t = .t,
                               method = "linear",
                               search = "bisection")
  }) |>
  unlist()

target <- t*2

expect_equal(actual, target,
             info = "csm_get_at_t(\"linear\") hourly time step")

########################
# search = interpolation
########################

# csm_get_at_t()
# t on time interval
# t_ind follows daily time step
t <- seq(0, 100, length.out = 11)
t_ind <- 0:100
dummy_variable <- t_ind*2

actual <-
  lapply(t, \(.t){
    csmbuilder::csm_get_at_t(dummy_variable, t_ind, t = .t,
                               method = "linear",
                               search = "interpolation")
  }) |>
  unlist()

target <- t*2

expect_equal(actual, target,
             info = "csm_get_at_t(\"linear\") daily time step")

# csm_get_at_t()
# t between time interval
# t_ind follows daily time step
t <- c(0, seq(0.5, 99.5, length.out = 9), 100)
t_ind <- 0:100
dummy_variable <- t_ind*2

actual <-
  lapply(t, \(.t){
    csmbuilder::csm_get_at_t(dummy_variable, t_ind, t = .t,
                               method = "linear",
                               search = "interpolation")
  }) |>
  unlist()

target <- t*2

expect_equal(actual, target,
             info = "csm_get_at_t(\"linear\") daily time step between interval")

# csm_get_at_t()
# t on time interval
# t_ind follows hourly time step (with t in day units)
t <- seq(0, 100, length.out = 11)
t_ind <- seq(0, 100, length.out = 100*24+1)
dummy_variable <- t_ind*2

actual <-
  lapply(t, \(.t){
    csmbuilder::csm_get_at_t(dummy_variable, t_ind, t = .t,
                               method = "linear",
                               search = "interpolation")
  }) |>
  unlist()

target <- t*2

expect_equal(actual, target,
             info = "csm_get_at_t(\"linear\") hourly time step")

# csm_get_at_t()
# t between time interval
# t_ind follows hourly time step (with t in day units)
t <- c(0, seq(0.5, 99.5, length.out = 9), 100)
t_ind <- seq(0, 100, length.out = 100*24+1)
dummy_variable <- t_ind*2

actual <-
  lapply(t, \(.t){
    csmbuilder::csm_get_at_t(dummy_variable, t_ind, t = .t,
                               method = "linear",
                               search = "interpolation")
  }) |>
  unlist()

target <- t*2

expect_equal(actual, target,
             info = "csm_get_at_t(\"linear\") hourly time step")


####################
# search = bruteforce
####################

# csm_get_at_t()
# t on time interval
# t_ind follows daily time step
t <- seq(0, 100, length.out = 11)
t_ind <- 0:100
dummy_variable <- t_ind*2

actual <-
  lapply(t, \(.t){
    csmbuilder::csm_get_at_t(dummy_variable, t_ind, t = .t,
                               method = "linear",
                               search = "bruteforce")
  }) |>
  unlist()

target <- t*2

expect_equal(actual, target,
             info = "csm_get_at_t(\"linear\") daily time step")

# csm_get_at_t()
# t between time interval
# t_ind follows daily time step
t <- c(0, seq(0.5, 99.5, length.out = 9), 100)
t_ind <- 0:100
dummy_variable <- t_ind*2

actual <-
  lapply(t, \(.t){
    csmbuilder::csm_get_at_t(dummy_variable, t_ind, t = .t,
                               method = "linear",
                               search = "bruteforce")
  }) |>
  unlist()

target <- t*2

expect_equal(actual, target,
             info = "csm_get_at_t(\"linear\") daily time step between interval")

# csm_get_at_t()
# t on time interval
# t_ind follows hourly time step (with t in day units)
t <- seq(0, 100, length.out = 11)
t_ind <- seq(0, 100, length.out = 100*24+1)
dummy_variable <- t_ind*2

actual <-
  lapply(t, \(.t){
    csmbuilder::csm_get_at_t(dummy_variable, t_ind, t = .t,
                               method = "linear",
                               search = "bruteforce")
  }) |>
  unlist()

target <- t*2

expect_equal(actual, target,
             info = "csm_get_at_t(\"linear\") hourly time step")

# csm_get_at_t()
# t between time interval
# t_ind follows hourly time step (with t in day units)
t <- c(0, seq(0.5, 99.5, length.out = 9), 100)
t_ind <- seq(0, 100, length.out = 100*24+1)
dummy_variable <- t_ind*2

actual <-
  lapply(t, \(.t){
    csmbuilder::csm_get_at_t(dummy_variable, t_ind, t = .t,
                               method = "linear",
                               search = "bruteforce")
  }) |>
  unlist()

target <- t*2

expect_equal(actual, target,
             info = "csm_get_at_t(\"linear\") hourly time step")

