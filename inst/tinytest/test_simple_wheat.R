library(tinytest)

#################################
# Simple Wheat Model Test Example
#################################

generate_Tair <- function(doy = NULL){
  if(is.null(doy)) doy <- 1:365
  15*cos((doy-200)/365*2*pi)+15
}

sig <<- function(x, y){
  1/(1 + exp(-100*(x/y - 1)))
}

# Initial conditions
state <- c(TT = 0, LAI = 0.01, biomass = 0.1)

# parameters
parameters <-
  c(Tbase = -1.16,
    alpha = 0.007,
    K = 0.39,
    TTL = 1378,
    senrate = 0.0015,
    RUE = 1.82/20,
    TTM = 2162)

# Rate equation function
swheat_ref_dt <- function(t, state, parms, wth){
  with(as.list(c(state, parms)), {
    Tavg <- csmdeveloper::csm_get_at_t(wth[,2], wth[,1], t, "linear")
    SRAD <- csmdeveloper::csm_get_at_t(wth[,3], wth[,1], t, "linear")
    c(
      # dTT:
      (Tavg - Tbase)*sig(Tavg+273.15, Tbase+273.15),
      # dLAI:
      (Tavg - Tbase)*sig(Tavg+273.15, Tbase+273.15)*alpha*LAI*exp(-K*LAI)*(1-sig(TT, TTL)) -
        LAI*senrate*(Tavg - Tbase)*sig(Tavg+273.15, Tbase+273.15)*sig(TT, TTL),
      # dbiomass:
      RUE*SRAD*(1-exp(-K*LAI))*(Tavg - Tbase)*sig(Tavg+273.15, Tbase+273.15)*(1 - sig(TT, TTM))
    )
  }) |>
    list()
}

# Generate air temperature forcings
doy <- c(300:365,1:180)
wth <-
  matrix(
    c(seq_along(doy) - 1,
      generate_Tair(doy),
      csmdeveloper:::solar_radiation(latitude = 36, doy = doy)),
    ncol = 3)

colnames(wth) <- c("time", "Tair", "SRAD")

# Check swheat_ref_dt()
expect_equal(
  swheat_ref_dt(0,
                  state = state,
                  parms = parameters,
                  wth = wth),
  with(as.list(c(state, parameters)), {
    c(
      # dTT:
      (wth[1, 2] - Tbase)*sig(wth[1, 2]+273.15, Tbase+273.15),
      # dLAI:
      (wth[1, 2] - Tbase)*sig(wth[1, 2]+273.15, Tbase+273.15)*alpha*LAI*exp(-K*LAI)*(1-sig(TT, TTL)) -
        LAI*senrate*(wth[1, 2] - Tbase)*sig(wth[1, 2]+273.15, Tbase+273.15)*sig(TT, TTL),
      # dbiomass:
      RUE*wth[1, 3]*(1-exp(-K*LAI))*(wth[1, 2] - Tbase)*sig(wth[1, 2]+273.15, Tbase+273.15)*(1 - sig(TT, TTM))
    ) |>
      unname() |>
      list()
  }),
  info = "swheat_ref_dt(); t=0"
)

expect_equal(
  swheat_ref_dt(0.5,
                  state = state,
                  parms = parameters,
                  wth = wth),
  with(c(as.list(state), as.list(parameters)), {
    c(
      # dTT:
      (mean(wth[1:2, "Tair"]) - Tbase)*sig(mean(wth[1:2, "Tair"])+273.15, Tbase+273.15),
      # dLAI:
      (mean(wth[1:2, "Tair"]) - Tbase)*sig(mean(wth[1:2, "Tair"])+273.15, Tbase+273.15)*alpha*LAI*exp(-K*LAI)*(1-sig(TT, TTL)) -
        LAI*senrate*(mean(wth[1:2, "Tair"]) - Tbase)*sig(mean(wth[1:2, "Tair"])+273.15, Tbase+273.15)*sig(TT, TTL),
      # dbiomass:
      RUE*mean(wth[1:2, "SRAD"])*(1-exp(-K*LAI))*(mean(wth[1:2, "Tair"]) - Tbase)*sig(mean(wth[1:2, "Tair"])+273.15, Tbase+273.15)*(1 - sig(TT, TTM))
    ) |>
    unname() |>
    list()
  }),
  info = "swheat_ref_dt(); t=0.5"
)

# Specify times at which to report output
times <- seq(0, nrow(wth)-1, by = 0.01)

# Create list of integration methods to test:
integ_list <- c("euler", "rk4")

# Run integration
swheat_ref_out <-
  integ_list |>
  (\(.x) setNames(.x, .x))() |>
  lapply(\(.method){
    deSolve::ode(
      y = state,
      times = times,
      func = swheat_ref_dt,
      parms = parameters,
      method = .method,
      wth = wth
    )
  })

#############################################
# Create Simple Wheat Model with csmdeveloper
#############################################

# Define parameters
swheat_parameters <- c(

  csmdeveloper::csm_create_parameter(
    name = "Tbase",
    definition = "base temperature",
    units = "degree C",
    lower_bound = -273.15),

  csmdeveloper::csm_create_parameter(
    name = "alpha",
    definition = "relative rate of leaf area expansion",
    units = "per degree C",
    lower_bound = 0),

  csmdeveloper::csm_create_parameter(
    name = "K",
    definition = "light extinction coefficient",
    units = "unitless",
    lower_bound = 0),

  csmdeveloper::csm_create_parameter(
    name = "TTL",
    definition = "thermal time to end of leaf expansion",
    units = "degree C",
    lower_bound = 0),

  csmdeveloper::csm_create_parameter(
    name = "senrate",
    definition = "relative rate of leaf senescence",
    units = "per degree C",
    lower_bound = 0),

  csmdeveloper::csm_create_parameter(
    name = "RUE",
    definition = "radiation use efficiency",
    units = "g per MJ",
    lower_bound = 0),

  csmdeveloper::csm_create_parameter(
    name = "TTM",
    definition = "thermal time to physiological maturity",
    units = "degree C",
    lower_bound = 0)
)

# Define weather inputs
wth_in <- c(
  csmdeveloper::csm_create_transform(
    name = "wtime",
    definition = "time of weather observation",
    units = "days after planting",
    ~wth[, 1]),

  csmdeveloper::csm_create_transform(
    name = "Tair",
    definition = "air temperature at time t",
    units = "degree C",
    ~csmdeveloper::csm_get_at_t(wth[, 2], wth[,1], t, "linear")),

  csmdeveloper::csm_create_transform(
    name = "SRAD",
    definition = "solar radiation",
    units = "MJ m^{-2} d^{-1}",
    ~csmdeveloper::csm_get_at_t(wth[, 3], wth[,1], t, "linear"))

)

swheat_wth <- csmdeveloper::csm_create_data_structure(
  "wth",
  definition = "weather data",
  variables = wth_in
)

# Define intermediate factors
swheat_intermediate <- c(

  csmdeveloper::csm_create_transform(
    name = "f_tt",
    definition = "thermal time factor",
    units = "degree C",
    ~(Tair - Tbase)/(1+exp(-100*((Tair + 273.15)/(Tbase + 273.15)-1))))
)

# Define state variables
swheat_state <- c(

  csmdeveloper::csm_create_state(
    name = "TT",
    definition = "cumulative thermal time",
    units = "degree C",
    ~f_tt),

  csmdeveloper::csm_create_state(
    name = "LAI",
    definition = "leaf area index",
    units = "leaf area m^{2} ground area m^{-2}",
    ~f_tt*alpha*LAI*exp(-K*LAI)*(1-1/(1+exp(-100*(TT/TTL-1)))) -
      LAI*senrate*f_tt*(1/(1+exp(-100*(TT/TTL-1))))),

  csmdeveloper::csm_create_state(
    name = "biomass",
    definition = "biomass",
    units = "g m^{-2}",
    ~RUE*SRAD*(1-exp(-K*LAI))*f_tt*(1-1/(1+exp(-100*(TT/TTM-1)))))

)

swheat_model <-
  csmdeveloper::csm_create_model(
    name = "swheat_dydt",
    state = swheat_state,
    prm = swheat_parameters,
    wth = swheat_wth,
    f_inter = swheat_intermediate)

swheat_dydt <- csmdeveloper::csm_render_model(
    model = swheat_model,
    arg_alias = c(state_variables = "state", parameters = "parms"),
    output_type = "deSolveRFunction"
  )

# Run integration
swheat_dydt_out <-
  integ_list |>
  (\(.x) setNames(.x, .x))() |>
  lapply(\(.method){
    deSolve::ode(
      y = state,
      times = times,
      func = swheat_dydt,
      parms = parameters,
      wth = wth,
      method = .method
    )
  })

for(integ_method in integ_list){
  expect_equal(
    current = swheat_dydt_out[[integ_method]],
    target = swheat_ref_out[[integ_method]],
    info = integ_method
  )
}

rm("sig", envir = .GlobalEnv)
