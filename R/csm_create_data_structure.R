#' Create a cropping systems model (CSM) data structure
#'
#' @inheritParams csm_create_variable
#'
#' @param definition the definition of the data structure
#'
#' @param variables a list of the CSM variables (as defined with
#'  [csmdeveloper::csm_create_variable()]) that are contained within the data
#'   structure
#'
#' @examples
#'
#' # Create variables:
#' wth_variables <- c(
#'   csm_create_variable("Tair",
#'                       "air temperature",
#'                       "Celsius"),
#'   csm_create_variable("SRAD",
#'                       "solar radiation",
#'                       "MJ/m2/d"))
#'
#' # Create weather data structure:
#' weather <- csm_create_data_structure("weather",
#'                                      "weather data",
#'                                      wth_variables)
#'
#' @export
#'
csm_create_data_structure <- function(name, definition, variables){

  stopifnot(length(name) == length(definition))

  if(length(name) > 1){
    stopifnot(length(name) == length(variables))
  }else{
    variables <- list(variables)
  }

  data_structure <- mapply(\(.def, .name, .var, .dim){
    .ds <- .def
    attr(.ds, "name") <- .name
    attr(.ds, "variables") <- .var
    class(.ds) <- "csm_data_structure"
    return(.ds)
  },
  .def = definition,
  .name = name,
  .var = variables,
  SIMPLIFY = FALSE)

  names(data_structure) <- name

  return(data_structure)
}
