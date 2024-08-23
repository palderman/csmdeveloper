generate_T_avg <- function(lat){
  # Assume 25 C average at equator and -20 at poles
  cos(lat/180*pi)*45-20
}

generate_T_amp <- function(lat){
  # Assume amplitude of monthly temperature is approx 45 at 60 degrees
  # latitude N/S and about 25 C at 20 degrees latitude N/S
  55*(1-exp(-0.03*abs(lat)))/2
}

generate_Tair <- function(doy = 1:365, lat){
  T_amp = generate_T_amp(lat)
  T_avg = generate_T_avg(lat)
  if(lat > 0){
    offset <- 204
  }else{
    offset <- 22
  }
  T_amp*cos((doy-offset)/365*2*pi)+T_avg
}

generate_T_max <- function(T_avg){
  T_avg + 5
}

generate_T_min <- function(T_avg){
  T_avg - 5
}
generate_ppd <- function(latitude, doy = 1:365, p = 6){
  # Using the CBM model from Forsythe et al (1995)
  # p is the daylength coefficient in degrees:
  # p = 6 for civil twilight from Table 1 of Forsythe et al (1995)
  theta <- 0.2163108 + 2*atan(0.9671396*tan(0.00860*(doy - 186)))
  psi <- asin(0.39795*cos(theta))
  24 - 24/pi*acos((sin(p*pi/180) + sin(latitude*pi/180)*sin(psi))/(cos(latitude*pi/180)*cos(psi)))
}

calc_pressure <- function(elevation){
  # Eq 7 from Allen et al (1998) (i.e. FAO-56)
  101.3*(293-0.0065*elevation/293)^5.26
}

psychrometric_constant <- function(pressure,
                                   lambda = 2.45,
                                   c_p = 1.103e-3,
                                   epsilon = 0.622){
  # Eq 8 from Allen et al (1998) (i.e. FAO-56)
  c_p*pressure/epsilon/lambda
}

saturation_vapor_pressure <- function(temperature){
  # Eq 11 from Allen et al (1998) (i.e. FAO-56)
  0.6108*exp(17.27*temperature/(temperature + 237.3))
}

slope_saturation_vapor_pressure <- function(temperature){
  # Eq 13 from Allen et al (1998) (i.e. FAO-56)
  4098*saturation_vapor_pressure(temperature)/(temperature + 237.3)
}

actual_vapor_pressure <- function(Tmin){
  # Eq 48 from Allen et al (1998) (i.e. FAO-56)
  saturation_vapor_pressure(Tmin)
}

solar_declination <- function(doy){
  0.409*sin(2*pi*doy/365-1.39)
}

inverse_relative_distance <- function(doy){
  1 + 0.033*cos(2*pi*doy/365)
}

calc_omega_s <- function(latitude, doy){
  psi <- latitude*pi/180
  # Eq 24 from Allen et al (1998) (i.e. FAO-56)
  del <- solar_declination(doy)
  # Eq 25 from Allen et al (1998) (i.e. FAO-56)
  acos(-tan(psi)*tan(del))
}

daily_extraterrestrial_solar_radiation <- function(latitude, doy = 1:365){
  # G_sc = solar constant
  G_sc <- 0.0820
  psi <- latitude*pi/180
  # Eq 23 from Allen et al (1998) (i.e. FAO-56)
  d_r <- inverse_relative_distance(doy)
  # Eq 24 from Allen et al (1998) (i.e. FAO-56)
  del <- solar_declination(doy)
  # Eq 25 from Allen et al (1998) (i.e. FAO-56)
  omega_s <- acos(-tan(psi)*tan(del))
  # Eq 21 from Allen et al (1998) (i.e. FAO-56)
  24*60/pi*G_sc*d_r*(omega_s*sin(psi)*sin(del)+cos(psi)*cos(del)*sin(omega_s))
}

time_zone_longitude <- function(longitude){
  round(longitude / 15)*15
}

subdaily_extraterrestrial_solar_radiation <- function(latitude, longitude,
                                                      doy = rep(1:365, each = 24),
                                                      t = rep(1:24, 365),
                                                      t_l = 1){
  psi <- latitude*pi/180
  # Eq 24 from Allen et al (1998) (i.e. FAO-56)
  del <- solar_declination(doy)
  # Eq 25 from Allen et al (1998) (i.e. FAO-56)
  omega_s <- acos(-tan(psi)*tan(del))
  # Eq 32 from Allen et al (1998) (i.e. FAO-56)
  S_c <- 0.1645*sin(2*b) - 0.1255*cos(b) - 0.025*sin(b)
  # Eq 31 from Allen et al (1998) (i.e. FAO-56)
  L_z <- time_zone_longitude(longitude)
  omega <- pi/12*((t+2/30*(L_z - longitude) + S_c)-12)
  # Return 0 if sun is below horizon:
  if(omega < -omega_s | omega > omega_s) return(0)
  # t_l length of calculation period in hours
  # G_sc = solar constant
  G_sc <- 0.0820
  # Eq 23 from Allen et al (1998) (i.e. FAO-56)
  d_r <- inverse_relative_distance(doy)
  # Eq 24 from Allen et al (1998) (i.e. FAO-56)
  del <- solar_declination(doy)
  # Eq 33 from Allen et al (1998) (i.e. FAO-56)
  b <- 2*pi*(doy - 81)/364
  # Eq 29 from Allen et al (1998) (i.e. FAO-56)
  omega_1 <- omega - pi*t_l/24
  # Eq 30 from Allen et al (1998) (i.e. FAO-56)
  omega_2 <- omega + pi*t_l/24
  # Eq 28 from Allen et al (1998) (i.e. FAO-56)
  12*60/pi*G_sc*d_r*((omega_2 - omega_1)*sin(psi)*sin(del)+cos(psi)*cos(del)*sin(omega_s))
}

daylight_hours <- function(latitude, doy){
  # Eq 34 from Allen et al (1998) (i.e. FAO-56)
  24/pi*calc_omega_s(latitude, doy)
}

solar_radiation <- function(latitude,
                            doy = 1:365,
                            Ra = daily_extraterrestrial_solar_radiation(latitude, doy),
                            n = NULL,
                            a_s = 0.25, b_s = 0.50){
  N <- daylight_hours(latitude, doy)
  # Assume clear sky (i.e. n == N) if n = NULL
  if(is.null(n)) n <- N
  # Eq 35 from Allen et al (1998) (i.e. FAO-56)
  (a_s + b_s*n/N)*Ra
}

clear_sky_radiation <- function(Ra, a_s, b_s, elevation){
  # Eq 37 from Allen et al (1998) (i.e. FAO-56)
  if(missing(a_s) | missing(b_s)){
    if(missing(elevation)){
      stop("Either elevation or both a_s and b_s must be provided.")
    }else{
      (0.75 + elevation*2e-5)*Ra
    }
  }else{
    (a_s + b_s)*Ra
  }
}

#' @export
#' @noRd
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

#' @export
#' @noRd
generate_ETo <- function(doy, lat, method = "Hargreaves", ...){
  dot_args <- list(...)
  if(tolower(method) == "hargreaves"){
    Tavg <- generate_Tair(doy, lat)
    Tmax <- generate_T_max(Tavg)
    Tmin <- generate_T_min(Tavg)
    K <- dot_args[["K"]]
    if(is.null(K)){
      warning("K not specified so defaulting to K = 0.0023 for Hargreaves method")
      K <- 0.0023
    }
    Ra <- daily_extraterrestrial_solar_radiation(lat, doy)
    ETo <- K*Ra*(Tavg + 17.8)*sqrt(Tmax - Tmin)
  }else{
    warning("Implemented ETo methods currently only inlude Hargreaves")
    ETo <- rep(NA_real_, length(doy))
  }
  return(ETo)
}
