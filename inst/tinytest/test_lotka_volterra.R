library(tinytest)
library(deSolve)

#############################
# Lotka-Volterra Test Example
#############################

# Initial conditions
state <- c(x = 10,
           y = 10)

# parameters
parameters <-
  c(alpha = 1.1,
    beta = 0.4,
    gamma = 0.1,
    delta = 0.4)

# Rate equation function
lotka_volt_ref_dt <- function(t, state, parms){
  with(as.list(c(state, parms)), {
    list(
      c(
        alpha*x-beta*x*y,
        delta*x*y-gamma*y
      )
    )
    })
}

# Specify times at which to report output
times <- seq(0, 100, by = 0.01)

# Create list of integration methods to test:
integ_list <- c("euler", "rk4", "ode23", "ode45")

# Run integration
lotka_volt_ref_out <-
  integ_list |>
  (\(.x) setNames(.x, .x))() |>
  lapply(\(.method){
    ode(
      y = state,
      times = times,
      func = lotka_volt_ref_dt,
      parms = parameters,
      method = .method
    )
  })

#########################################
# Create Lotka-Volterra with csmdeveloper
#########################################

# Define state variables
lv_state <- csmdeveloper::csm_create_state(
  c("x", "y"),
  definition = c("prey", "predator"),
  units = c("rabbits per square km", "foxes per square km"),
  expression(~alpha*x-beta*x*y, ~delta*x*y-gamma*y))

# Define parameters
lv_parameters <- csmdeveloper::csm_create_parameter(
  c("alpha", "beta", "gamma", "delta"),
  definition = c("maximum prey per capita growth rate",
                 "effect of predator population on prey death rate",
                 "predator per capita death rate",
                 "effect of prey population on predator growth rate"),
  units = c("rabbits per rabbit", "per fox",
            "foxes per fox", "foxes per rabbit"))

# Define model
lotka_volt_model <-
  csmdeveloper::csm_create_model(
    state = lv_state,
    parms = lv_parameters)

# Create function for calculating rates
lotka_volt_dydt <- csmdeveloper::csm_render_model(lotka_volt_model,
                                                  arg_alias = c(parameters = "parms"),
                                                  output_type = "function",
                                                  language = "R")

# Run integration
lotka_volt_dydt_out <-
  integ_list |>
  (\(.x) setNames(.x, .x))() |>
  lapply(\(.method){
    ode(
      y = state,
      times = times,
      func = lotka_volt_dydt,
      parms = parameters,
      method = .method
    )
  })

for(integ_method in integ_list){
  expect_equal(
    current = lotka_volt_dydt_out[[integ_method]],
    target = lotka_volt_ref_out[[integ_method]],
    info = integ_method
  )
}
