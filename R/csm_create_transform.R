#' Create a cropping systems model (CSM) transformed variable
#'
#' @inheritParams csm_create_variable
#'
#' @param equation an R expression with the equation
#'  for the value of the transformed CSM state variable
#'
#' @examples
#'
#' # Define intermediate factor
#'
#' sp_factors <- csmbuilder::csm_create_transform(
#'   name = "fv",
#'   definition = "vernalization factor",
#'   units = "relative progress towards complete vernalization (0-1)",
#'   equation = ~min(c(cum_vrn/vreq, 1)))
#'
#' @export
#'
csm_create_transform <- function(name, definition, units, equation){

  if(inherits(equation, "formula")) equation <- c(equation)

  stopifnot(length(name) == length(definition))
  stopifnot(length(name) == length(units))
  stopifnot(length(name) == length(equation))

  transform_variable <- csm_create_variable(
    name = name,
    definition = definition,
    units = units
    )

  transform_variable <- mapply(\(.s, .eq){
    attr(.s, "equation") <- .eq
    class(.s) <- c(class(.s), "csm_transform")
    return(.s)
  },
  .s = transform_variable,
  .eq = equation,
  SIMPLIFY = FALSE)

  return(transform_variable)
}
