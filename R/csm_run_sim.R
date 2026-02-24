#' Run a Cropping System Model (CSM) simulation
#'
#' @export
#'
#' @md
#'
#' @param model_function a rendered model produced by
#'   [csmbuilder::csm_render_model()]
#'
#' @param y_init a vector of initial values for the model state variables
#'
#' @param t an optional vector of time points for which simulated model outputs
#'  are desired
#'
#' @param ... additional arguments to pass to model_function for simulation
#'
#' @param method numerical integration method to be used. See [deSolve::ode()]
#'  for more details
#'
#' @returns
#' a data frame with one row for each time point specified by `t`
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
#' # Render model into a callable R function
#' lotka_volterra_fun <-
#'   csm_render_model(lotka_volterra_model,
#'                    output_type = "function",
#'                    language = "R")
#'
#' # Run model simulation
#' lotka_volterra_out <-
#'   csm_run_sim(model_function = lotka_volterra_fun,
#'               y_init = c(x = 10,
#'                          y = 10),
#'               t = csm_time_vector(0, 100, 0.01),
#'               parms = c(alpha = 1.1,
#'                         beta = 0.4,
#'                         gamma = 0.1,
#'                         delta = 0.4))
#'
csm_run_sim <- function(model_function,
                        y_init,
                        t,
                        ...,
                        method = "euler"){
  if(!requireNamespace('deSolve', quietly = TRUE)){
    "The csm_run_sim() function requires the deSolve package," |>
      paste0(" which is not currently installed.\n") |>
      paste0("Please install with install.packages('deSolve') and try again.") |>
      stop()
  }
  deSolve::ode(y = y_init,
               times = t,
               func = model_function,
               ...,
               method = method)
}
