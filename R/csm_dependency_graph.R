#' Create a dependency graph for a Cropping System Model (CSM)
#'
#' @noRd
#'
#' @md
#'
#' @param model a model as defined using [csmbuilder::csm_create_model()]
#'
csm_dependency_graph <- function(model){

  state_vars <-
    model[["state_variables"]] |>
    names()

  if("parameters" %in% names(model)){
    prms <-
      model[["parameters"]] |>
      names()
  }else{
    prms <- NULL
  }

  if("transformed_variables" %in% names(model)){
    tvars <-
      model[["tvars"]] |>
      names()
  }else{
    tvars <- NULL
  }


}
