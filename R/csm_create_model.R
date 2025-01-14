#' Create a model definition for a Cropping System Model (CSM)
#'
#' @export
#'
#' @md
#'
#' @param state a list vector containing CSM state
#'   variables defined using [csmdeveloper::csm_create_state()]
#'   in the intended order
#' @param ... optional arguments of list vectors containing
#'   CSM parameters defined using [csmdeveloper::csm_create_parameter()]
#'   or CSM variables defined using [csmdeveloper::csm_create_variable()]
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
