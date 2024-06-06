library(tinytest)
library(deSolve)

#############################
# Lorenz Test Example
#############################

# Initial conditions
state <- c(X = 1,
           Y = 1,
           Z = 1)

# parameters
parameters <- c(a = -8/3,
                b = -10,
                c = 28)

# Rate equation function
Lorenz_ref_dt <- function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    # rate of change
      dX <- a*X + Y*Z
      dY <- b * (Y-Z)
      dZ <- -X*Y + c*Y - Z
        # return the rate of change
        list(c(dX, dY, dZ))
      }) # end with(as.list ...
  }

# Specify times at which to report output
times <- seq(0, 100, by = 0.01)

# Create list of integration methods to test:
integ_list <- c("euler", "rk4", "ode23", "ode45")

# Run integration
Lorenz_ref_out <-
  integ_list |>
  (\(.x) setNames(.x, .x))() |>
  lapply(\(.method){
    ode(
      y = state,
      times = times,
      func = Lorenz_ref_dt,
      parms = parameters,
      method = .method
    )
  })

#################################
# Create Lorenz with csmdeveloper
#################################

# Define state variables
lorenz_state <- csmdeveloper::csm_create_state(
  c("X", "Y", "Z"),
  definition = c("Lorenz X", "Lorenz Y", "Lorenz Z"),
  units = c("unitless", "unitless", "unitless"),
  expression(~a*X+Y*Z, ~b*(Y-Z), ~-X*Y+c*Y-Z))

# Define parameters
lorenz_parameters <- csmdeveloper::csm_create_parameter(
  c("a", "b", "c"),
  definition = c("Lorenz a",
                 "Lorenz b",
                 "Lorenz c"),
  units = c("unitless", "unitless", "unitless"))

# Create function for calculating rates
Lorenz_dydt <-
  csmdeveloper::csm_create_dydt(
    name = "lorenz",
    state = lorenz_state,
    parameters = lorenz_parameters,
    arg_names = c(state = "state", parameters = "parms"),
    output_type = "deSolve"
  )

# Run integration
Lorenz_dydt_out <-
  integ_list |>
  (\(.x) setNames(.x, .x))() |>
  lapply(\(.method){
    ode(
      y = state,
      times = times,
      func = Lorenz_dydt,
      parms = parameters,
      method = .method
    )
  })

for(integ_method in integ_list){
  expect_equal(
    current = Lorenz_dydt_out[[integ_method]],
    target = Lorenz_ref_out[[integ_method]],
    info = integ_method
    )
}
