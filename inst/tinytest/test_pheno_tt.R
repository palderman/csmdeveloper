library(tinytest)

###########################################
# Phenology Thermal Time Model Test Example
###########################################

generate_Tair <- function(doy = NULL){
  if(is.null(doy)) doy <- 1:365
  15*cos((doy-200)/365*2*pi)+15
}

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

csmdeveloper::load_to_global_env(
  c("get_at_t_linear",
    "mod_arr")
)

# Initial conditions
state <- c(du = 0)

# parameters
parameters <-
  c(ko = 1,
    H = 136000,
    E = 95000,
    To = 20)

# Rate equation function
pheno_tt_ref_dt <- function(t, state, parms, wth){
  with(as.list(c(state, parms)), {
    Tt <- get_at_t_linear(wth[,2], t)
    list(
      mod_arr(Tt, ko, H, E, To)
    )
  })
}

# Generate air temperature forcings
wth <-
  matrix(
    c(seq_along(c(300:365,1:180)) - 1,
      generate_Tair(c(300:365,1:180))),
         ncol = 2)

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
    list(mod_arr(wth[1,2], ko, H, E, To))
  }),
  info = "pheno_tt_ref_dt(); t=0"
)

expect_equal(
  pheno_tt_ref_dt(0.5,
                      state = state,
                      parms = parameters,
                      wth = wth),
  with(as.list(parameters), {
    list(mod_arr(mean(wth[1:2,2]), ko, H, E, To))
  }),
  info = "pheno_tt_ref_dt(); t=0.5"
)

# Specify times at which to report output
times <- seq(0, nrow(wth)-1, by = 0.01)

# Create list of integration methods to test:
integ_list <- c("euler", "rk4")

# Run integration
pheno_tt_ref_out <-
  integ_list |>
  (\(.x) setNames(.x, .x))() |>
  lapply(\(.method){
    deSolve::ode(
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
sp_wth_inp <- csmdeveloper::csm_create_transform(
  c("wtime", "Tair"),
  definition = c("time of weather observation",
                 "air temperature"),
  units = c("days after planting", "degrees Celcius"),
  equation = c(~wth[,1],
               ~get_at_t_linear(wth[,2], wtime, t)))

sp_wth <- csmdeveloper::csm_create_data_structure(
  name = "wth",
  definition = "weather data",
  variables = c(sp_wth_inp)
)

# Define model
pheno_tt_model <-
  csmdeveloper::csm_create_model(
    name = "pheno_tt",
    state = sp_state,
    parms = sp_parameters,
    wth = sp_wth)

# Create function for calculating rates
pheno_tt_dydt <-
  csmdeveloper::csm_render_model(
    model = pheno_tt_model,
    arg_alias = c(parameters = "parms"),
    output_type = "deSolve")

# Run integration
pheno_tt_dydt_out <-
  integ_list |>
  (\(.x) setNames(.x, .x))() |>
  lapply(\(.method){
    deSolve::ode(
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

csmdeveloper::cleanup_global_env()
