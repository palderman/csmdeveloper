library(tinytest)
library(deSolve)

###########################################
# Phenology with Vernalization Model Test Example
###########################################

generate_Tair <- function(doy = NULL){
  if(is.null(doy)) doy <- 1:365
  seq(0, 2*pi, length.out = length(doy))
  plot(doy, cos((doy-200)/365*2*pi))
}
generate_Tair()

get_at_t_linear <- function(x, t){
  i = floor(t) + 1  # plus 1 because t=0 at index=1
  j = ceiling(t) + 1  # plus 1 because t=0 at index=1
  t_i = floor(t)
  t_j = ceiling(t)
  if(i == j){
    return(x[i])
  }else{
    return(x[i]*(t_j-t) + x[j]*(t-t_i))/(t_j - t_i)
  }
}

mod_arr <- function(Tt, ko, H, E, To){
  R <- 8.314
  ko*(H*exp(E/R*(1/(To+273.15)-1/(Tt + 273.15))))/(H - E*(1-exp(H/R*(1/(To+273.15)-1/(Tt + 273.15)))))
}

# Initial conditions
state <- c(du = 0,
           cum_vrn = 0)

# parameters
parameters <-
  c(ko = 1,
    H = 136000,
    E = 95000,
    To = 20,
    vreq = 42,
    kv = 1,
    Hv = 830000,
    Ev = 700000,
    Vo = 4
  )

# Rate equation function
pheno_tt_ref_dt <- function(t, state, parms, wth){
  with(as.list(c(state, parms)), {
    Tt <- get_at_t_linear(wth[,1], t)
    fv <- min(c(cum_vrn/vreq, 1))
    list(c(
      fv*mod_arr(Tt, ko, H, E, To),
      mod_arr(Tt, kv, Hv, Ev, Vo),
    ))
  })
}

# Generate air temperature forcings
wth <-
  matrix(rnorm(100, mean = parameters["To"], sd = 5),
         ncol = 1)

# Check mod_arr()
expect_equal(
  with(as.list(parameters), {
    mod_arr(To, ko, H, E, To)
  }),
  parameters["ko"],
  info = "mod_arr",
  check.attributes = FALSE
)

# Check pheno_tt_ref_dt()
expect_equal(
  pheno_tt_ref_dt(0,
                      state = state,
                      parms = parameters,
                      wth = wth),
  with(as.list(parameters), {
    list(mod_arr(wth[1,1], ko, H, E, To))
  }),
  info = "pheno_tt_ref_dt(); t=0"
)

expect_equal(
  pheno_tt_ref_dt(0.5,
                      state = state,
                      parms = parameters,
                      wth = wth),
  with(as.list(parameters), {
    list(mod_arr(mean(wth[1:2,1]), ko, H, E, To))
  }),
  info = "pheno_tt_ref_dt(); t=0.5"
)

# Specify times at which to report output
times <- seq(0, 100, by = 0.01)

# Create list of integration methods to test:
integ_list <- c("euler", "rk4")

# Run integration
pheno_tt_ref_out <-
  integ_list |>
  (\(.x) setNames(.x, .x))() |>
  lapply(\(.method){
    ode(
      y = state,
      times = times,
      func = pheno_tt_ref_dt,
      parms = parameters,
      method = .method,
      wth = wth
    )
  })

#######################################################
# Create Phenology Thermal Time Model with csmdeveloper
#######################################################

# Define state variables
sp_state <- csmdeveloper::csm_create_state(
  c("du"),
  definition = c("development units"),
  units = c("physiological days"),
  expression(~mod_arr(Tair, ko, H, E, To)))

# Define parameters
sp_parameters <- csmdeveloper::csm_create_parameter(
  c("ko", "H", "E", "To"),
  definition = c("relative reaction rate",
                 "deactivation energy",
                 "activation energy",
                 "optimum temperature"),
  units = c("unitless", "Joules per mole",
            "Joules  per mole", "degrees Celcius"))

# Define weather inputs
sp_wth <- csmdeveloper::csm_create_transform(
  c("Tair"),
  definition = c("air temperature"),
  units = c("degrees Celcius"),
  equation = ~get_at_t_linear(wth[,1], t))

# Create function for calculating rates
pheno_tt_dydt <-
  csmdeveloper::csm_create_dydt(
    name = "pheno_tt",
    state = sp_state,
    parameters = sp_parameters,
    wth = sp_wth,
    arg_names = c(state = "state",
                  parameters = "parms",
                  wth = "wth"),
    output_type = "deSolve")

# Run integration
pheno_tt_dydt_out <-
  integ_list |>
  (\(.x) setNames(.x, .x))() |>
  lapply(\(.method){
    ode(
      y = state,
      times = times,
      func = pheno_tt_dydt,
      parms = parameters,
      wth = wth,
      method = .method
    )
  })

for(integ_method in integ_list){
  expect_equal(
    current = pheno_tt_dydt_out[[integ_method]],
    target = pheno_tt_ref_out[[integ_method]],
    info = integ_method
  )
}
