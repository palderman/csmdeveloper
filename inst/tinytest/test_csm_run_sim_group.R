library(tinytest)

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

#################################
# Create Lorenz with csmbuilder
#################################

# Define state variables
lorenz_state <- csmbuilder::csm_create_state(
  c("X", "Y", "Z"),
  definition = c("Lorenz X", "Lorenz Y", "Lorenz Z"),
  units = c("unitless", "unitless", "unitless"),
  expression(~a*X+Y*Z, ~b*(Y-Z), ~-X*Y+c*Y-Z))

# Define parameters
lorenz_parameters <- csmbuilder::csm_create_parameter(
  c("a", "b", "c"),
  definition = c("Lorenz a",
                 "Lorenz b",
                 "Lorenz c"),
  units = c("unitless", "unitless", "unitless"))

# Create model
lorenz_model <- csmbuilder::csm_create_model(state = lorenz_state,
                                               parameters = lorenz_parameters,
                                               name = "lorenz")

# Create function for calculating rates
Lorenz_dydt <-
  csmbuilder::csm_render_model(
    model = lorenz_model,
    arg_alias = c(parameters = "parms"),
    output_type = "function",
    language = "R")

# Specify times at which to report output
times <- csmbuilder::csm_time_vector(0, 100, dt = 0.01)

# Run integration
Lorenz_dydt_out <-
  csmbuilder::csm_run_sim_group(
      model_function = Lorenz_dydt,
      y_init = list(state),
      t = list(times),
      parms = list(parameters),
      method = 'euler',
      return_df = TRUE)

expect_true("data.frame" %in% class(Lorenz_dydt_out),
            info = "Check data frame output")

Lorenz_dydt_out <-
  csmbuilder::csm_run_sim_group(
    model_function = Lorenz_dydt,
    y_init = list(state),
    t = list(times),
    parms = list(parameters,
                 parameters*0.9),
    method = 'euler',
    return_df = FALSE)

expect_true("list" %in% class(Lorenz_dydt_out),
            info = "Check list output")

expect_error(
    csmbuilder::csm_run_sim_group(
      model_function = Lorenz_dydt,
      y_init = state,
      t = list(times),
      parms = list(parameters),
      method = 'euler'),
    pattern = "not of list type",
    info = "Check for list inputs"
)

expect_error(
  csmbuilder::csm_run_sim_group(
    model_function = Lorenz_dydt,
    y_init = list(state),
    t = list(times, times, times),
    parms = list(parameters, parameters*0.9),
    method = 'euler'),
  pattern = "differing lengths",
  info = "Check for matching list lengths"
)
