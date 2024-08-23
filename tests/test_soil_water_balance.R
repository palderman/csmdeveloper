library(tinytest)

###########################################
# Bare Soil Water Balance Model Test Example
###########################################

# Generate rain and ETo forcings
fib_rain <- function(n, event = 10){
  rain <- numeric(n)
  rain[] <- 0
  fib_buf <- c(0, 1, 1, 2)
  ind <- 0
  while(TRUE){
    fib_buf[1:3] <- fib_buf[2:4]
    fib_buf[4] <- sum(fib_buf[2:3])
    ind <- ind + fib_buf[1]
    if(ind <= n){
      rain[ind] = event
    }else{
      break
    }
  }
  return(rain)
}

doy <- c(300:365, 1:180)

fib_rain(length(doy))

wth <-
  matrix(c(
    csmdeveloper::generate_ETo(doy = doy,
                               lat = 36.145,
                               method = "Hargreaves",
                               K = 0.0023),
    fib_rain(length(doy), 5)),
    byrow = FALSE,
    ncol = 2)

# Generate soil input data
profile_depth_mm <- 100
soil <-
  c(# soil water at field capacity
    0.36*profile_depth_mm,
    # soil water at permanent wilting point
    0.22*profile_depth_mm,
    # soil water at limit of readily extractable water
    (0.36 + 0.22*3)/4*profile_depth_mm,
    # soil water at limit of total extractable water
    0.22/2*profile_depth_mm)

# Initial conditions
state <- c(sw = soil[1],
           et_cum = 0)

# parameters
parameters <-
  c(Ke = 0.15)

# Rate equation function
dsw_dt <- function(t, state, parms, wth, soil, delta_t){
  with(as.list(c(state, parms)), {
    ETo_t <- csmdeveloper::get_at_t_linear(wth[, 1], t)*delta_t
    P_t <- csmdeveloper::get_at_t_linear(wth[, 2], t)*delta_t
    fi <- 1 - 1/(1+exp(-log(99)/(soil[1] - (soil[1]*3 + soil[2])/4)*(sw - (soil[1]*7/8+soil[2]/8))))
    fe <- 1/(1+exp(-log(99)/(soil[3] - soil[4])*(sw - soil[3])))
    list(c(
      P_t*fi-ETo_t*Ke*fe,
      ETo_t*Ke*fe
    ))
  })
}

# Check dsw_dt()
expect_equal(
  dsw_dt(0,
         state = state,
         parms = parameters,
         wth = wth,
         soil = soil,
         delta_t = 1),
  with(as.list(parameters), {
    fi <- unname(1 - 1/(1+exp(-log(99)/(soil[1] - (soil[1]*3 + soil[2])/4)*(state[1] - (soil[1]*7/8+soil[2]/8)))))
    fe <- unname(1/(1+exp(-log(99)/(soil[3] - soil[4])*(state[1] - soil[3]))))
    list(c(
        wth[1,2]*fi-wth[1,1]*Ke*fe,
        wth[1,1]*Ke*fe
    ))
  }),
  info = "dsw_dt(); t=0"
)

expect_equal(
  dsw_dt(0.5,
         state = state,
         parms = parameters,
         wth = wth,
         soil = soil,
         delta_t = 0.5),
  with(as.list(parameters), {
    fi <- unname(1 - 1/(1+exp(-log(99)/(soil[1] - (soil[1]*3 + soil[2])/4)*(state[1] - (soil[1]*7/8+soil[2]/8)))))
    fe <- unname(1/(1+exp(-log(99)/(soil[3] - soil[4])*(state[1] - soil[3]))))
    list(c(
      mean(wth[1:2,2])*0.5*fi-mean(wth[1:2,1])*0.5*Ke*fe,
      mean(wth[1:2,1])*0.5*Ke*fe
    ))
  }),
  info = "dsw_dt(); t=0.5"
)

# Specify times at which to report output
delta_t <- 0.01
times <- seq(0, nrow(wth)-1, by = delta_t)

# Create list of integration methods to test:
integ_list <- c("euler", "rk4")

# Run integration
sw_ref_out <-
  integ_list |>
  (\(.x) setNames(.x, .x))() |>
  lapply(\(.method){
    deSolve::ode(
      y = state,
      times = times,
      func = dsw_dt,
      parms = parameters,
      method = .method,
      wth = wth,
      soil = soil,
      delta_t = delta_t
    )
  })

plot(sw_ref_out[["rk4"]][,3], typ = "l")

##########################################################
# Create Simple Soil Water Balance Model with csmdeveloper
##########################################################

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
