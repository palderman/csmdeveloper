#' Create a cropping systems model (CSM) variable
#'
#' @param name a length-one character vector name of a variable
#'
#' @param definition a length-one character vector that defines
#'   the CSM variable
#'
#' @param units a length-one character vector of the units of
#'   the CSM variable
#'
#' @returns
#' a list of csm_variable objects
#'
#' @examples
#'
#' Tair <- csm_create_variable(name = "Tair",
#'                             definition = "air temperature",
#'                             units = "Celsius")
#'
#' @export
#'
csm_create_variable <- function(name, definition, units){

  stopifnot(length(name) == length(definition))
  stopifnot(length(name) == length(units))

  variable <- mapply(\(.name, .def, .unit){
      .var <- .def
      attr(.var, "name") <- .name
      attr(.var, "units") <- .unit
      class(.var) <- c(class(.var), "csm_variable")
      return(.var)
    },
    .name = name,
    .def = definition,
    .unit = units,
    SIMPLIFY = FALSE
  )

  names(variable) <- name

  return(variable)
}

# experimental, maybe need csm_variable_list type?
# units.csm_variable <- function(.var){
#   lapply(.var,
#          \(.x) attr(.x, "units")) |>
#     unlist()
# }

