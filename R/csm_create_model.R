#' Create a model definition for a Cropping System Model (CSM)
#'
#' @export
#'
#' @md
#'
#' @param state a list vector containing CSM state
#'   variables defined using [csmbuilder::csm_create_state()]
#'   in the intended order
#'
#' @param ... optional arguments of list vectors containing
#'   CSM parameters defined using [csmbuilder::csm_create_parameter()]
#'   or CSM variables defined using [csmbuilder::csm_create_variable()]
#'
#' @param name a character string containing a name for the models
#'
#' @returns
#' a list which defines all components of a model including state variables,
#' input variables, parameters, transformed variables and data structures.
#'
#' @examples
#'
#' # Define state variables
#' lv_state <- csm_create_state(
#'   c("x", "y"),
#'   definition = c("prey", "predator"),
#'   units = c("rabbits per square km", "foxes per square km"),
#'   expression(~alpha*x-beta*x*y, ~delta*x*y-gamma*y))
#'
#' # Define parameters
#' lv_parameters <- csm_create_parameter(
#'   c("alpha", "beta", "gamma", "delta"),
#'   definition = c("maximum prey per capita growth rate",
#'                  "effect of predator population on prey death rate",
#'                  "predator per capita death rate",
#'                  "effect of prey population on predator growth rate"),
#'   units = c("rabbits per rabbit", "per fox",
#'             "foxes per fox", "foxes per rabbit"))
#'
#' # Define model
#' lotka_volterra_model <-
#'   csm_create_model(
#'     state = lv_state,
#'     parms = lv_parameters)
#'
csm_create_model <- function(state, ..., name = "model"){

  # All models must have state variables so start with those
  model <-
    list(
      state_variables = state
    )

  # Combine ... into dots object
  dots <-
    list(...) |>
    do.call(c, args = _)

  model$input_variables <-
    subset_with(dots, is_input)

  model$parameters <-
    subset_with(dots, is_parameter)

  model$transformed_variables <-
    subset_with(dots, is_transform)

  model$data_structures <-
    subset_with(dots, is_data_structure)

  model
}
