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
#' @param dimensions a named numeric vector of indices for dimensions
#'  of the dataset. It must contain an element named "variables" to
#'  denote the dimension across which the variables of the dataset
#'  are distributed
#'
#' @export
#'
csm_create_data_structure <- function(name, definition, variables,
                                      dimensions = c(variables = 1)){

  stopifnot(length(name) == length(definition))
  stopifnot("variables" %in% names(dimensions))

  if(length(name) > 1){
    stopifnot(length(name) == length(variables))
    stopifnot(length(name) == length(dimensions))
  }else{
    if(length(variables) > 1) variables <- list(variables)
    if(length(dimensions) > 1) dimensions <- list(dimensions)
  }

  data_structure <- mapply(\(.def, .name, .var, .dim){
    .ds <- .def
    attr(.ds, "name") <- .name
    attr(.ds, "variables") <- .var
    attr(.ds, "dimensions") <- .dim
    class(.ds) <- "csm_data_structure"
    return(.ds)
  },
  .def = definition,
  .name = name,
  .var = variables,
  .dim = dimensions,
  SIMPLIFY = FALSE)

  names(data_structure) <- name

  return(data_structure)
}
