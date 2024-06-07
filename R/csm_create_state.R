#' Create a cropping systems model (CSM) state variable
#'
#' @inheritParams csm_create_transform
#'
#' @param equation an R expression with the equation
#'  for the rate of change of the CSM state variable
#'
#' @export
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
