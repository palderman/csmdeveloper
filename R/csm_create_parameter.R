#' Create a cropping systems model (CSM) parameter variable
#'
#' @inheritParams csm_create_variable
#'
#' @param lower_bound a numerical value providing the lower
#'   bound for the parameter
#' @param upper_bound a numerical value providing the upper
#'   bound for the parameter
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
