#' Create a cropping systems model (CSM) state variable
#'
#' @inheritParams csm_create_variable
#'
#' @param equation an R expression with the equation
#'  for the rate of change of the CSM state variable
#'
#' @export
#'
csm_create_state <- function(name, definition, units, equation){

  stopifnot(length(name) == length(definition))
  stopifnot(length(name) == length(units))
  stopifnot(length(name) == length(equation))

  state_variable <- csm_create_variable(
    name = name,
    definition = definition,
    units = units
    )

  state_variable <- mapply(\(.s, .eq){
    attr(.s, "equation") <- .eq
    class(.s) <- c(class(.s), "csm_state")
    return(.s)
  },
  .s = state_variable,
  .eq = equation,
  SIMPLIFY = FALSE)

  return(state_variable)
}
