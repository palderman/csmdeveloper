#' Generate vector of times for simulation
#'
#' @export
#'
#' @param t_init a numerical value providing the initial time to
#'  use for simulation
#'
#' @param t_max a numerical value providing the final time to
#'  use for simulation
#'
#' @param dt a numerical value providing the size of the time step to
#'  use for simulation
#'
#' @returns
#' a numeric vector
#'
#' @examples
#'
#' csm_time_vector(0, 100, dt = 0.01)
#'
#'
csm_time_vector <- function(t_init, t_max, dt = 1){
  seq(t_init, t_max, by = dt)
}
