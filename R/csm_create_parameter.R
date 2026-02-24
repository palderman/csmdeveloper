#' Create a cropping systems model (CSM) parameter variable
#'
#' @inheritParams csm_create_variable
#'
#' @param lower_bound a numerical value providing the lower
#'   bound for the parameter
#' @param upper_bound a numerical value providing the upper
#'   bound for the parameter
#'
#' @returns
#' a list of csm_parameter objects
#'
#' @examples
#' # Define Lotka-Voterra parameters with single call
#'
#' lv_parameters <- csm_create_parameter(
#'   name = c("alpha", "beta", "gamma", "delta"),
#'   definition = c("maximum prey per capita growth rate",
#'                  "effect of predator population on prey death rate",
#'                  "predator per capita death rate",
#'                  "effect of prey population on predator growth rate"),
#'   units = c("rabbits per rabbit", "per fox",
#'             "foxes per fox", "foxes per rabbit"))
#'
#' # Define Lotka-Volterra parameters with multiple calls
#'
#' lv_parameters <-
#'   c(
#'     csm_create_parameter(
#'       name = "alpha",
#'       definition = "maximum prey per capita growth rate",
#'       units = "rabbits per rabbit"),
#'     csm_create_parameter(
#'       name = "beta",
#'       definition = "effect of predator population on prey death rate",
#'       units = "per fox"),
#'     csm_create_parameter(
#'       name = "gamma",
#'       definition = "predator per capita death rate",
#'       units = "foxes per fox"),
#'     csm_create_parameter(
#'       name = "delta",
#'       definition = "effect of prey population on predator growth rate",
#'       units = "foxes per rabbit"))
#'
#' @export
#'
csm_create_parameter <- function(name, definition, units,
                                 lower_bound = NULL,
                                 upper_bound = NULL){

  stopifnot(length(name) == length(definition))
  stopifnot(length(name) == length(units))

  if(is.null(lower_bound)){
    lower_bound <- rep(-Inf, length(name))
  }else{
    stopifnot(length(name) == length(lower_bound))
  }

  if(is.null(upper_bound)){
    upper_bound <- rep(Inf, length(name))
  }else{
    stopifnot(length(name) == length(upper_bound))
  }

  parameter_variable <- csm_create_variable(
    name = name,
    definition = definition,
    units = units
  )

  parameter_variable <-
    mapply(\(.par, .lb, .ub){
      class(.par) <- c(class(.par), "csm_parameter")
      attr(.par, "lower_bound") <- .lb
      attr(.par, "upper_bound") <- .ub
      .par
    },
    .par = parameter_variable,
    .lb = lower_bound,
    .ub = upper_bound,
    SIMPLIFY = FALSE)

  return(parameter_variable)
}
