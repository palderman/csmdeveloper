library(tinytest)

###########################################
# Bare Soil Water Balance Model Test Example
###########################################

# Rain and ETo forcings derived from NASA POWER data for 2020-09-01 to 2021-08-31
ETo <- c(9.4974498, 8.7488478, 14.3757234, 13.76894025, 14.2566156,
         14.5291725, 13.872384, 10.5480225, 2.6586549, 4.2959646, 3.928311,
         9.4031928, 10.87665165, 7.941888, 9.0801999, 11.550276, 10.3571325,
         8.60881095, 9.270612, 9.5191524, 10.3721472, 3.0451869, 6.2345052,
         10.6671384, 10.3474665, 10.9330722, 2.29770405, 9.2637, 8.93646,
         10.5232203, 9.520038, 8.866773, 6.3407799, 8.3558952, 9.2137824,
         8.3862243, 9.6835743, 9.418032, 7.3353843, 7.18265475, 10.1679057,
         9.2375397, 9.2385414, 9.4345182, 7.3101555, 7.0712946, 8.02905075,
         1.3342644, 2.00933055, 2.32925895, 6.314679, 8.9583975, 2.46699945,
         2.4019254, 1.5285348, 0.5253255, 0.5941188, 0.578664, 4.314141,
         5.5606041, 5.739552, 6.0035985, 5.27877405, 6.61921875, 5.08605075,
         6.3830025, 6.3343215, 6.380451, 5.24585025, 3.7125297, 4.71982275,
         4.9949865, 5.153085, 3.2174415, 4.36689225, 5.2881012, 5.3133975,
         5.413716, 5.438637, 5.849496, 3.87735255, 1.06374465, 2.5337232,
         2.5450443, 0.588033, 3.76529445, 4.1406174, 4.05330345, 2.4521103,
         1.5598791, 3.2430672, 3.5451108, 0.51533415, 0.85169475, 3.2351022,
         3.3356502, 3.488751, 3.558762, 3.8234268, 4.1745375, 4.2744753,
         1.8696906, 1.40797035, 0.6775191, 2.2370904, 0.64118655, 1.29448935,
         2.8501875, 2.9580714, 3.396222, 3.4659495, 3.5732745, 3.7638216,
         3.60229545, 2.0335536, 2.906064, 3.8495709, 3.8478564, 1.642113,
         1.06731, 1.3578084, 1.248912, 0.897777, 1.4824269, 2.5912197,
         3.3654258, 3.3797547, 0.77362425, 0.8013168, 1.5080904, 2.4696711,
         1.7364375, 3.15141435, 3.26806515, 3.379644, 3.6391761, 1.7243928,
         2.89603755, 3.7200681, 2.0856528, 2.19607875, 1.432809, 2.3390694,
         3.9650796, 2.0590308, 1.17585, 1.27413, 0.97548165, 1.5540525,
         3.07725075, 3.1432401, 2.1549969, 2.22507, 3.751839, 4.7844513,
         4.8935124, 4.28141025, 4.0134042, 1.3414302, 0.821205, 0.71772075,
         0.591273, 0.649512, 0.661581, 0.8499087, 1.682694, 0.3191346,
         0.3645972, 0.7452, 1.7549136, 2.7069984, 3.4281225, 4.41663435,
         4.1763384, 6.12888255, 7.0395885, 6.37117245, 4.50289125, 2.1120075,
         4.3775721, 3.9670452, 6.32784285, 6.9159285, 5.9859486, 7.51829175,
         3.46518, 6.9812685, 7.8985179, 7.7438025, 7.37933085, 8.5621536,
         2.5750332, 1.9699875, 3.4180272, 5.23375425, 8.1117504, 7.0178724,
         2.0018502, 3.9236886, 8.01595575, 8.6635359, 8.4498417, 2.2010103,
         2.9370816, 5.8949748, 3.5130132, 9.7547085, 8.6699808, 9.7684542,
         11.1700539, 8.3928474, 8.83218735, 9.3686544, 9.147114, 10.8554985,
         11.13912, 10.8806571, 5.6310876, 5.3757594, 11.8212507, 5.8080861,
         8.6037066, 11.5956441, 7.5685077, 8.9971479, 6.06620475, 4.7986992,
         2.3699871, 4.509189, 8.7417063, 11.537883, 4.70000475, 8.9617725,
         5.3824176, 3.863403, 10.4963148, 13.14301275, 10.4765859, 5.29308,
         5.62197375, 11.024964, 14.46786495, 7.8588387, 5.8285548, 8.0631774,
         8.3391363, 11.56248, 13.57763175, 10.9188135, 13.6791828, 7.1709975,
         7.6381191, 3.5690868, 9.5276412, 13.21624215, 10.7424009, 8.4150927,
         5.3104329, 8.8988328, 6.9227784, 5.3697573, 3.6170118, 8.34337395,
         5.9805135, 4.7754252, 5.20040925, 7.07090715, 11.8728315, 3.95082765,
         12.974094, 12.089493, 8.588349, 2.6772768, 5.29195005, 8.4237165,
         14.08645485, 15.3659916, 11.9078208, 10.647774, 10.6170318, 12.9541734,
         13.1839245, 16.69537575, 14.05755, 16.0327512, 16.9199685, 18.3867138,
         18.592119, 18.4959558, 18.0592632, 18.32748525, 17.8471755, 18.078309,
         9.4400208, 16.9857621, 15.8388048, 17.215335, 15.9527394, 11.0113938,
         7.0428501, 6.15845835, 8.4061989, 9.7281675, 5.217534, 13.6407456,
         16.0090371, 14.60971485, 16.3940112, 16.0579746, 11.2431564,
         17.4784176, 16.357221, 11.66319, 11.3187942, 16.0283826, 16.6098357,
         15.93108, 9.8532315, 13.8172797, 12.1522356, 11.40831, 15.1159338,
         13.5246456, 13.3892946, 14.599305, 16.5191508, 17.44472565, 15.18477435,
         13.5944055, 16.8596802, 17.15459715, 17.34196095, 16.8826599,
         16.8710985, 10.92615075, 13.3638417, 14.060196, 13.50351405,
         8.33555475, 14.73787845, 15.6401658, 11.214801, 15.7549293, 17.0717544,
         14.6874546, 16.024635, 9.2452725, 13.5340308, 13.7106594, 11.13837885,
         11.9559726, 12.5817624, 8.625528, 12.996126, 11.725938, 15.04051875,
         16.8381504, 16.6380723, 16.7152302, 16.2983259, 15.4354059, 14.85610875,
         13.3843833, 13.051233, 14.4270504)

rain <- c(16.92, 5.77, 0.14, 0, 0, 0, 0, 9.47, 25.85, 4.45, 5.1, 0.04,
          0.01, 0, 0, 0.01, 0, 0, 0.03, 0, 0.13, 13.81, 0.1, 0, 0.02, 0,
          3.9, 0.01, 0, 0, 0, 0, 0, 0, 0, 0, 0.05, 0, 0, 0.1, 0.07, 0,
          0, 0, 0, 0, 0, 0.48, 0.05, 0.04, 0, 0.11, 0.43, 0, 0.82, 36.84,
          20.46, 18.02, 9.89, 0, 0.01, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.45,
          0, 0, 0.44, 0.83, 0, 0, 0, 0, 0, 0, 3.73, 3.35, 0.04, 16.91,
          0.09, 0, 0, 0.09, 1.41, 0, 0.01, 10.71, 0.57, 0, 0.11, 0.04,
          0, 0.02, 0, 0, 1.06, 0, 8.67, 0.85, 1.1, 1.11, 2.87, 0.38, 0.11,
          0, 0, 0, 0, 0, 0, 0, 0, 0.02, 17.81, 16.04, 11.63, 11.86, 2.24,
          2.11, 0, 0.02, 5, 0.49, 0.01, 0, 0.04, 0, 0.01, 0.01, 0, 0, 0,
          0, 0, 0, 0.06, 0.19, 0, 0, 0.99, 26.16, 0, 0.01, 0, 0.18, 11.23,
          0.01, 0.26, 0, 0, 0, 0, 0.66, 0.09, 0.64, 0, 0.02, 0, 0, 0.06,
          1.36, 0.02, 0.95, 1.39, 0.01, 0, 0, 0, 0, 0, 0, 0.8, 0.06, 0.27,
          0.08, 0, 0, 0, 2.42, 5.36, 0, 0, 0, 0, 0.07, 0.59, 7.39, 5.01,
          6.91, 0, 0.49, 15.62, 0.01, 0, 0, 0, 32.69, 0.84, 2.04, 0.42,
          0, 0, 0, 0, 0, 0, 0, 0.01, 0, 0, 0, 0.03, 0.06, 0, 7.59, 0, 0,
          0.16, 9.8, 10.45, 1.66, 10.9, 0.01, 0, 0, 0.03, 0, 0.49, 18.17,
          14.16, 0, 0, 6.51, 16.31, 0.04, 0, 0, 0, 12.05, 4.98, 0.76, 0,
          0.04, 0.1, 0.05, 0.36, 9.3, 0.06, 0, 0.08, 0.26, 15.98, 1.67,
          3.46, 4.99, 9, 3.34, 1.8, 2.88, 9.91, 1.49, 0.1, 30.48, 0, 0.01,
          0.31, 20.17, 0.71, 0.72, 0, 0, 0.45, 1.97, 6.86, 7.49, 0, 0,
          15.75, 4.15, 0.29, 0, 0, 0, 0, 0, 0, 0.67, 7.23, 0, 0.01, 0,
          23.02, 59.37, 30.18, 23.39, 16.31, 3.3, 22.27, 3.07, 0.01, 0,
          0, 0, 2.68, 0.62, 0.04, 19.24, 1.16, 0, 0, 0, 0.09, 1.02, 1.6,
          0.72, 0.04, 0, 0, 0, 0.12, 0.55, 0.41, 9.11, 2.68, 0.01, 0, 0,
          2.79, 10.57, 0.03, 0, 0, 1.78, 0.16, 3.73, 7.04, 0.1, 0.08, 0.03,
          0, 6.78, 2.05, 0.23, 3.26, 0.74, 1.39, 1.23, 1.91, 14.31, 1.34,
          0.04, 0, 0, 0.01, 0, 0, 0.03, 1.95, 0.07)

time <- (1:length(ETo)) - 1

wth <-
  matrix(c(
    time,
    ETo,
    rain),
    byrow = FALSE,
    ncol = 3)

colnames(wth) <- c("time", "ETo", "P")

# Generate soil input data
profile_depth_mm <- 100
soil <-
  c(# soil water at field capacity
    sw_fc = 0.36*profile_depth_mm,
    # soil water at permanent wilting point
    sw_pwp = 0.22*profile_depth_mm,
    # soil water at limit of readily extractable water
    sw_rew = (0.36 + 0.22*3)/4*profile_depth_mm,
    # soil water at limit of total extractable water
    sw_tew = 0.22/2*profile_depth_mm)

# Initial conditions
state <- c(sw = unname(soil[1]))

# parameters
parameters <-
  c(Ke = 0.15)

# Rate equation function
dsw_dt <- function(t, state, parms, wth, soil){
  with(as.list(c(state, parms)), {
    ETo_t <- csmbuilder::csm_get_at_t(wth[, "ETo"], wth[, "time"], t, "linear")
    P_t <- csmbuilder::csm_get_at_t(wth[, "P"], wth[, "time"], t, "linear")
    fi <- 1 - 1/(1+exp(-log(99)/(soil["sw_fc"] - (soil["sw_fc"]*3 + soil["sw_pwp"])/4)*(sw - (soil["sw_fc"]*7/8+soil["sw_pwp"]/8))))
    fe <- 1/(1+exp(-log(99)/(soil["sw_rew"] - soil["sw_tew"])*(sw - soil["sw_rew"])))
    list(
      unname(P_t*fi-ETo_t*Ke*fe)
    )
  })
}

# Check dsw_dt()
expect_equal(
  dsw_dt(0,
         state = state,
         parms = parameters,
         wth = wth,
         soil = soil),
  with(as.list(parameters), {
    fi <- unname(1 - 1/(1+exp(-log(99)/(soil["sw_fc"] - (soil["sw_fc"]*3 + soil["sw_pwp"])/4)*(state["sw"] - (soil["sw_fc"]*7/8+soil["sw_pwp"]/8)))))
    fe <- unname(1/(1+exp(-log(99)/(soil["sw_rew"] - soil["sw_tew"])*(state["sw"] - soil["sw_rew"]))))
    list(
        unname(wth[1,"P"]*fi-wth[1,"ETo"]*Ke*fe)
    )
  }),
  info = "dsw_dt(); t=0"
)

# Specify times at which to report output
times <- csmbuilder::csm_time_vector(0, nrow(wth)-1, dt = 0.01)

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
      soil = soil
    )
  })

##########################################################
# Create Simple Soil Water Balance Model with csmbuilder
##########################################################

# Define state variables
sw_state <- csmbuilder::csm_create_state(
  "sw",
  definition = "soil water",
  units = "mm",
  expression(~P*fi-ETo*Ke*fe))

# Define parameters
sw_parameters <- csmbuilder::csm_create_parameter(
  "Ke",
  definition = "soil evaporation coefficient",
  units = "unitless")

# Define soil inputs
sw_soil_vars <- c(
  csmbuilder::csm_create_transform(
    "sw_fc",
    definition = "soil water at field capacity",
    units = "mm",
    equation = ~soil["sw_fc"]
  ),
  csmbuilder::csm_create_transform(
    "sw_pwp",
    definition = "soil water at permanent wilting point",
    units = "mm",
    equation = ~soil["sw_pwp"]
  ),
  csmbuilder::csm_create_transform(
    "sw_rew",
    definition = "soil water at limit of readily extractable water",
    units = "mm",
    equation = ~soil["sw_rew"]
  ),
  csmbuilder::csm_create_transform(
    "sw_tew",
    definition = "soil water at limit of total extractable water",
    units = "mm",
    equation = ~soil["sw_tew"]
  )
)

# Define weather variables
sw_wth_vars <- c(
  csmbuilder::csm_create_transform(
    "wtime",
    definition = "time of observation",
    units = "days after planting",
    equation = ~wth[,"time"]),
  csmbuilder::csm_create_transform(
    "ETo",
    definition = "reference evapotranspiration",
    units = "mm",
    equation = ~csm_get_at_t(wth[,"ETo"], wtime, t, "linear")),
  csmbuilder::csm_create_transform(
    "P",
    definition = "precipitation",
    units = "mm",
    equation = ~csm_get_at_t(wth[,"P"], wtime, t, "linear"))
)

# Define input data structures
sw_inputs <- c(
  csmbuilder::csm_create_data_structure(
    "soil_data",
    definition = "soil data",
    variables = sw_soil_vars
  ),
  csmbuilder::csm_create_data_structure(
    "wth_data",
    definition = "weather data",
    variables = sw_wth_vars
  )
)

# Define intermediate factors
sw_factors <- c(
  csmbuilder::csm_create_transform(
    "fi",
    definition = "infiltration factor",
  units = "unitless",
  equation = ~1 - 1/(1+exp(-log(99)/(sw_fc - (sw_fc*3 + sw_pwp)/4)*(sw - (sw_fc*7/8+sw_pwp/8))))),
  csmbuilder::csm_create_transform(
    "fe",
    definition = "evaporation factor",
    units = "unitless",
    equation = ~1/(1+exp(-log(99)/(sw_rew - sw_tew)*(sw - sw_rew))))
)

# Create model object
sw_model <-
  csmbuilder::csm_create_model(sw_state,
                                 sw_parameters,
                                 sw_factors,
                                 sw_inputs)

# Create function for calculating rates
sw_dydt <-
  csmbuilder::csm_render_model(
    name = "sw",
    model = sw_model,
    arg_alias = c(state_variables = "state",
                  soil_data = "soil",
                  wth_data = "wth"),
    output_type = "function",
    language = "R")

# Run integration
sw_dydt_out <-
  integ_list |>
  (\(.x) setNames(.x, .x))() |>
  lapply(\(.method){
    csmbuilder::csm_run_sim(
      model_function = sw_dydt,
      y_init = state,
      t = times,
      parms = parameters,
      wth = wth,
      soil = soil,
      method = .method
    )
  })

for(integ_method in integ_list){
  expect_equal(
    current = sw_dydt_out[[integ_method]],
    target = sw_ref_out[[integ_method]],
    info = integ_method
  )
}

