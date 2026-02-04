#' Generate documentation for a defined Cropping System Model (CSM)
#'
#' @md
#'
#' @noRd
#'
#' @param model a list vector containing a CSM as created by
#'  [csmdeveloper::csm_create_model()]
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

  model_df <-
    model |>
    doc_tbl_fmt() |>
    stack_df_list()

  alias_regex <- names(aliases)

  if(grepl("table", output_type)){
    output <- model_df
    for(i in seq_along(aliases)){
      for(.c in c("name", "description")){
        output[[.c]] <- gsub(alias_regex[i], aliases[i], output[[.c]])
      }
    }
    if(output_type == "typst_table"){
      output[["name"]] <- output[["name"]]
    }else if(output_type == "LaTeX_table"){

    }
    return(output)
  }else{
    paste0("output type \"", output_type, "\" not implemented yet") |>
      stop()
  }

  if(grepl("RcppFunction", output_type) &
     !requireNamespace("Rcpp", quietly = TRUE)){
    paste0(
      "Rcpp must be installed for output_type ",
      output_type, ".") |>
      stop()
  }

  if(output_type %in% c("Rfunction", "Rcode", "deSolve")){
    comment_char <- "#"
    line_end <- ""
    assign_op <- "<-"
    l_bracket <- "["
    r_bracket <- "]"
    vector_base_adj <- 0
    v_types = c(scalar = "",
                array = "")
  }else if(grepl("Rcpp", output_type)){
    comment_char <- "//"
    line_end <- ";"
    assign_op <- "="
    l_bracket <- "["
    r_bracket <- "]"
    vector_base_adj <- 0
    v_types = c(scalar = "double",
                array = "NumericVector")
  }else if(grepl("Fortran", output_type)){
    comment_char <- "!"
    line_end <- ""
    assign_op <- "="
    l_bracket <- "("
    r_bracket <- ")"
    vector_base_adj <- 0
    v_types = c(scalar = "",
                vector = "")
  }

  arg_names <- c(
    state_variables = "state_variables",
    parameters = if(!is.null(model$parameters)) "parameters",
    setNames(nm = names(model$input_variables)),
    setNames(nm = names(model$data_structures))
  )

  arg_regex <- paste0("^", names(arg_alias), "$")
  for(i in seq_along(arg_alias)){
    arg_names <- gsub(arg_regex[i], arg_alias[i], arg_names)
  }

  # State variables

  state <- model$state_variables

  state_type <- get_v_type(state, v_types = v_types)

  state_names <- names(model$state_variables)

  if(output_type %in% c("RFunction", "RCode", "deSolveFunction")){
    dstate_dt <- paste0("d", arg_names["state_variables"], "_dt ", assign_op, " ",
                        "vector(\"numeric\", length(", arg_names["state_variables"], "))",
                        line_end)
    state_arg <- arg_names["state_variables"]
  }else if(grepl("Rcpp", output_type)){
    dstate_dt <- paste0(state_type, " d", arg_names["state_variables"], "_dt(",
                        arg_names["state_variables"], ".size())",
                        line_end)
    state_arg <- arg_names["state_variables"]
  }else{
    if(state_type %in% c("array", "vector")){
      dstate_dt <- paste0(state_type, l_bracket, length(state), r_bracket, " d", state_names, "_dt", line_end)
    }else{
      dstate_dt <- paste0(state_type," d", state_names, "_dt", line_end)
    }
    state_arg <- paste0(state_type, " ", state_names, line_end)
  }

  # Other arguments

  dots_list <- c(
    parameters = list(model$parameters),
    input_variables = list(model$input_variables),
    model$data_structures,
    transformed_variables = list(model$transformed_variables)
  )
  dots_list <- dots_list[!sapply(dots_list, is.null)]

  if(length(dots_list) == 0){
    dydt_dots <- NULL
  }else{
    dydt_dots <- get_dydt_dots(dots_list, arg_names, comment_char,
                               v_types, line_end, output_type)
  }

  if(grepl("Rcpp", output_type)){
    dydt_args <-
      ifelse(arg_names %in% names(model$data_structures),
             "DataFrame",
             "NumericVector") |>
      paste0(" ", arg_names) |>
      paste0(collapse = ", ")
  }else{
    dydt_args <- paste0(arg_names, collapse = ", ")
  }

  if(grepl("deSolve", output_type)){
    if(grepl("Rcpp", output_type)){
      dydt_signature <- paste0("List ", name, "(NumericVector t, ", dydt_args, ")")
    }else{
      dydt_signature <- paste0("function(t, ", dydt_args, ")")
    }
  }else{
    if(grepl("Rcpp", output_type)){
      dydt_signature <- paste0("NumericVector ", name,
                               "(", dydt_args, ")")
    }else{
      dydt_signature <- paste0("function(", dydt_args, ")")
    }
  }

  if(grepl("Rcpp", output_type)){
    state_indices <- seq_along(state) - 1
  }else{
    state_indices <- seq_along(state)
  }

  dydt_state <-
    c(code_comment("State variables:", comment_char),
      mapply(dydt_declaration,
             .i = state_indices,
             .name = names(state),
             .var = state,
             .var_type = v_types["scalar"],
             .in = arg_names["state_variables"],
             .in_type = state_type,
             .comment_char = comment_char,
             .line_end = line_end),
      ""
    )

  dydt_rates <-
    c(
      code_comment("Calculation of rate of change for state variables:",
                   comment_char),
      mapply(dydt_rate_eqs,
             .i = state_indices,
             .state = state,
             .state_name = arg_names["state_variables"],
             .line_end = line_end),
      ""
    )

  dydt_body <- c(
    dstate_dt,
    "",
    dydt_state,
    dydt_dots,
    dydt_rates
  )

  dydt_output <- c(
    paste0(dydt_signature, "{"),
    "",
    paste0("  ", dydt_body),
    paste0("  ",
           code_return(paste0("d", arg_names["state_variables"], "_dt"),
                       output_type),
           line_end),
    "}",
    ""
  )

  if(grepl("Rcpp", output_type)){
    dydt_output <-
      c("#include <Rcpp.h>",
        "using namespace Rcpp;",
        "",
        "// [[Rcpp::export]]",
        dydt_output)
  }

  if(output_type %in% c("RFunction", "deSolveFunction")){
    dydt_output <-
      parse(text = dydt_output) |>
      eval()
  }else if(output_type == "deSolveRcppFunction"){
    Rcpp::sourceCpp(code = dydt_output)
  }

  return(dydt_output)

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
