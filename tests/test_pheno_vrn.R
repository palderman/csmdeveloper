library(tinytest)

###########################################
# Phenology with Vernalization Model Test Example
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
state <- c(du = 0,
           cum_vrn = 0)

# parameters
parameters <-
  c(ko = 1,
    H = 136000,
    E = 95000,
    To = 20,
    kv = 1,
    Hv = 830000,
    Ev = 700000,
    Vo = 4,
    vreq = 42
  )

# Rate equation function
pheno_vrn_ref_dt <- function(t, state, parms, wth){
  with(as.list(c(state, parms)), {
    Tt <- get_at_t_linear(wth[,1], t)
    fv <- min(c(cum_vrn/vreq, 1))
    list(c(
      fv*mod_arr(Tt, ko, H, E, To),
      mod_arr(Tt, kv, Hv, Ev, Vo)
    ))
  })
}

# Generate air temperature forcings
wth <-
  matrix(generate_Tair(doy = c(300:365, 1:180)),
         ncol = 1)

# Check mod_arr()
expect_equal(
  with(as.list(parameters), {
    mod_arr(Vo, kv, Hv, Ev, Vo)
  }),
  parameters["kv"],
  info = "mod_arr",
  check.attributes = FALSE
)

# Check pheno_vrn_ref_dt()
expect_equal(
  pheno_vrn_ref_dt(0,
                      state = state,
                      parms = parameters,
                      wth = wth),
  with(as.list(parameters), {
    list(c(0,
         mod_arr(wth[1,1], kv, Hv, Ev, Vo)
    ))
  }),
  info = "pheno_vrn_ref_dt(); t=0"
)

expect_equal(
  pheno_vrn_ref_dt(0.5,
                      state = state,
                      parms = parameters,
                      wth = wth),
  with(as.list(parameters), {
    list(c(0,
         mod_arr(mean(wth[1:2,1]), kv, Hv, Ev, Vo)
    ))
  }),
  info = "pheno_vrn_ref_dt(); t=0.5"
)

# Specify times at which to report output
times <- seq(0, nrow(wth)-1, by = 0.01)

# Create list of integration methods to test:
integ_list <- c("euler", "rk4")

# Run integration
pheno_vrn_ref_out <-
  integ_list |>
  (\(.x) setNames(.x, .x))() |>
  lapply(\(.method){
    deSolve::ode(
      y = state,
      times = times,
      func = pheno_vrn_ref_dt,
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
  c("du", "cum_vrn"),
  definition = c("development units",
                 "cumulative vernalization"),
  units = c("physiological days",
            "vernalization days"),
  expression(~fv*mod_arr(Tair, ko, H, E, To),
             ~mod_arr(Tair, kv, Hv, Ev, Vo)))

# Define parameters
sp_parameters <- csmdeveloper::csm_create_parameter(
  c("ko", "H", "E", "To", "kv", "Hv", "Ev", "Vo", "vreq"),
  definition = c("relative reaction rate at optimum temperature for primary temperature response",
                 "deactivation energy for primary temperature response",
                 "activation energy for primary temperature response",
                 "optimum temperature for primary temperature response",
                 "relative reaction rate at optimum temperature for vernalization temperature response",
                 "deactivation energy for vernalization temperature response",
                 "activation energy for vernalization temperature response",
                 "optimum temperature for vernalization temperature response",
                 "vernalization requirement"
  ),
  units = c("unitless", "Joules per mole",
            "Joules  per mole", "degrees Celcius",
            "unitless", "Joules per mole",
            "Joules  per mole", "degrees Celcius",
            "vernalization days"))

# Define weather inputs
sp_wth <- csmdeveloper::csm_create_transform(
  c("Tair"),
  definition = c("air temperature"),
  units = c("degrees Celcius"),
  equation = ~get_at_t_linear(wth[,1], t))

# Define intermediate factors
sp_factors <- csmdeveloper::csm_create_transform(
  c("fv"),
  definition = c("vernalization factor"),
  units = c("relative progress towards complete vernalization (0-1)"),
  equation = ~min(c(cum_vrn/vreq, 1)))

# Create function for calculating rates
pheno_vrn_dydt <-
  csmdeveloper::csm_create_dydt(
    name = "pheno_vrn",
    state = sp_state,
    parms = sp_parameters,
    wth = sp_wth,
    intermediate_factors = sp_factors,
    arg_names = c("state",
                  "parms",
                  "wth"),
    output_type = "deSolve")

# Run integration
pheno_vrn_dydt_out <-
  integ_list |>
  (\(.x) setNames(.x, .x))() |>
  lapply(\(.method){
    deSolve::ode(
      y = state,
      times = times,
      func = pheno_vrn_dydt,
      parms = parameters,
      wth = wth,
      method = .method
    )
  })

for(integ_method in integ_list){
  expect_equal(
    current = pheno_vrn_dydt_out[[integ_method]],
    target = pheno_vrn_ref_out[[integ_method]],
    info = integ_method
  )
}
