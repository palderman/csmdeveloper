#' Generate documentation for a defined Cropping System Model (CSM)
#'
#' @md
#'
#' @noRd
#'
#' @param model a list vector containing a CSM as created by
#'  [csmbuilder::csm_create_model()]
#'
#' @param output_type a character value indicating the type of output to produce
#'
#' @param aliases an optional named character vector whose names indicate
#'   variables for which to use an alias within the generated documentation and whose
#'   elements provide the corresponding alias
#'
#' @param ... additional variables to use when generating figures
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
#' # Generate LaTeX equations for model
#' lotka_volterra_dydt <-
#'   csm_document_model(lotka_volterra_model,
#'                      output_type = "LaTeX_equations")
#'
#' # Generate typst equations for model
#' lotka_volterra_dydt <-
#'   csm_document_model(lotka_volterra_model,
#'                      output_type = "typst_equations")
#'
#' # Generate table of variable definitions for model
#' lotka_volterra_code <-
#'   csm_document_model(lotka_volterra_model,
#'                      output_type = "table")
#'
#' # Generate a Forrester diagram for model
#' lotka_volterra_code <-
#'   csm_document_model(lotka_volterra_model,
#'                      output_type = "forrester")
#'
#' # Generate a figure of processes for model
#' lotka_volterra_code <-
#'   csm_document_model(lotka_volterra_model,
#'                      output_type = "figure")
#'
csm_document_model <- function(
    model,
    output_type = c("LaTeX_equations", "typst_equations",
                    "table", "figure", "Forrester"),
    aliases = NULL,
    ...){

  output_type <- match.arg(output_type)

}

doc_var_type <- function(.x){
  if("csm_state" %in% class(.x)){
    "State"
  }else if("csm_transform" %in% class(.x)){
    "Intermediate Factor"
  }else if("csm_parameter" %in% class(.x)){
    "Parameter"
  }
}

doc_tbl_fmt <- function(.x){
  if(is.list(.x)){
    lapply(.x, doc_tbl_fmt)
  }else if("csm_data_structure" %in% class(.x)){
    attr(.x, "variables") |>
      doc_tbl_fmt() |>
      do.call(rbind, args = _) |>
      within({
        type = "Input data"
      })
  }else{
    data.frame(name = attr(.x, "name"),
               units = attr(.x, "units"),
               type = doc_var_type(.x),
               description = .x)
  }
}

stack_df_list <- function(.x){
  if(!is.data.frame(.x) & is.list(.x)){
    df_out <-
      lapply(.x, stack_df_list) |>
      do.call(rbind, args = _)
    row.names(df_out) <- NULL
    df_out
  }else{
    .x
  }
}

to_typst_eq <- function(eq, inline = FALSE){
  eq_out <-
    eq |>
    gsub("([A-z]+)", "\"\\1\"", x = _)
  if(inline){
    paste0("$", eq_out, "$")
  }else{
    paste0("$ ", eq_out, " $")
  }
}
