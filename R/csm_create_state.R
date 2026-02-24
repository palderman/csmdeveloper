#' Create a cropping systems model (CSM) state variable
#'
#' @inheritParams csm_create_transform
#'
#' @param equation an R expression with the equation
#'  for the rate of change of the CSM state variable
#'
#' @returns
#' a list of csm_state objects
#'
#' @export
#'
#' @examples
#'
#' # Define state variables with single call
#'
#' lv_state <- csm_create_state(
#'   c("x", "y"),
#'   definition = c("prey", "predator"),
#'   units = c("rabbits per square km", "foxes per square km"),
#'   expression(~alpha*x-beta*x*y, ~delta*x*y-gamma*y))
#'
#' # Define state variables with multiple calls
#'
#' lv_state <-
#' c(
#'   csm_create_state(
#'     name = "x",
#'     definition = "prey",
#'     units = "rabbits per square km",
#'     equation = ~alpha*x-beta*x*y),
#'   csm_create_state(
#'     name = "y",
#'     definition = "predator",
#'     units = "foxes per square km",
#'     equation = ~delta*x*y-gamma*y)
#'   )
#'
csm_create_state <- function(name, definition, units, equation){

  state_variable <- csm_create_transform(
    name = name,
    definition = definition,
    units = units,
    equation = equation
    )

  state_variable <- mapply(\(.s){
    class(.s) <- c(class(.s), "csm_state")
    return(.s)
  },
  .s = state_variable,
  SIMPLIFY = FALSE)

  return(state_variable)
}
