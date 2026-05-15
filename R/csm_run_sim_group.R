#' Run a group of Cropping System Model (CSM) simulations
#'
#' @export
#'
#' @md
#'
#' @param model_function a rendered model produced by
#'   [csmbuilder::csm_render_model()]
#'
#' @param y_init a list of vectors of initial values for the model state
#'  variables. To use the same initial conditions for all simulation group
#'  members, the list should be of length one. Otherwise, the length of the list
#'  should correspond to the number of simulation group members.
#'
#' @param t an optional list of vectors of time points for which simulated model
#'  outputs are desired. To use the same time points for all simulation group
#'  members, the list should be of length one. Otherwise, the length of the list
#'  should correspond to the number of simulation group members.
#'
#' @param ... additional arguments to pass to model_function for simulation.
#'  Each argument should be supplied as a list. To use the same argument value
#'  for all simulation group members, the list should be of length one.
#'  Otherwise, the length of the list should correspond to the number of
#'  simulation group members.
#'
#' @param method numerical integration method to be used. See [deSolve::ode()]
#'  for more details
#'
#' @param return_df a logical value with a default value of `TRUE` that indicates
#'  whether to return output as a data frame (if set to `TRUE`) or as a list
#'  (if set to `FALSE`).
#'
#' @returns
#' If `return_df` is set to `TRUE`, the function returns a data frame with one
#' row for each time point specified by `t` within each simulation group member.
#' The simulation group member is indicated by a column within the data frame
#' (sim_no). If `return_df` is set to `FALSE`, the function returns a list of
#' data frames, each of which is the output of a simulation group member and
#' includes one row for each time point specified by `t`
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
#' # Run model simulations for two parameter vectors
#' lotka_volterra_out <-
#'   csm_run_sim_group(model_function = lotka_volterra_fun,
#'               y_init = list(c(x = 10,
#'                               y = 10)),
#'               t = list(csm_time_vector(0, 100, 0.01)),
#'               parms = list(c(alpha = 1.1,
#'                              beta = 0.4,
#'                              gamma = 0.1,
#'                              delta = 0.4),
#'                            c(alpha = 1.2,
#'                              beta = 0.5,
#'                              gamma = 0.15,
#'                              delta = 0.35))
#'
csm_run_sim_group <- function(model_function,
                              y_init,
                              t,
                              ...,
                              method = "euler",
                              return_df = TRUE){
  if(!requireNamespace('deSolve', quietly = TRUE)){
    "The csm_run_sim_group() function requires the deSolve package," |>
      paste0(" which is not currently installed.\n") |>
      paste0("Please install with install.packages('deSolve') and try again.") |>
      stop()
  }

  .args <- list(
    y_init = y_init,
    t = t,
    ...)

  # Check length/type of arguments
  .arg_lengths <- lapply(.args, length) |> unlist()
  .arg_is_list <- lapply(.args,
                         \(.x) "list" %in% class(.x)) |> unlist()

  if(!all(.arg_is_list)){
    not_list <- names(.args)[!.arg_is_list]
    not_list |>
      paste0(collapse = ", ") |>
      gsub(", ([^, ])$", " and \\1", x = _) |>
      paste0(if(length(not_list) > 1) " are " else " is ", "not of list type.") |>
      stop()
  }

  if(!all(.arg_lengths == 1 | .arg_lengths == max(.arg_lengths))){
    {names(.args)[.arg_lengths != max(.arg_lengths) & .arg_lengths != 1]} |>
      paste0(collapse = ", ") |>
      gsub(", ([^, ])$", " and \\1", x = _) |>
      paste0(" have differing lengths. They must be of the same length or have length 1.") |>
      stop()
  }

  sim_out <- .mapply(csm_run_sim,
                     dots = .args,
                     MoreArgs = list(
                       model_function = model_function,
                       method = method
                     ))

  if(return_df){
    n_rows <- sim_out |>
      lapply(nrow) |>
      unlist()
    sim_out <-
      do.call(rbind, args = sim_out) |>
      as.data.frame() |>
      rev() |>
      c(sim_no = seq_along(sim_out) |>
          rep(times = n_rows) |>
          list()) |>
      rev() |>
      data.frame(check.names = FALSE)
  }

  sim_out
}
