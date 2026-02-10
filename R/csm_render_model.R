#' Render a defined Cropping System Model (CSM)
#'
#' @export
#'
#' @md
#'
#' @param model a list vector containing a CSM as created by
#'  [csmbuilder::csm_create_model()]
#'
#' @param output_type a character value indicating the type of output to produce;
#'   one of: "function" (a callable function) or "code" (computer code for the
#'   model)
#'
#' @param language a character value indicating which programming language into
#'  which to render the model
#'
#' @param name name of the resulting function
#'
#' @param arg_alias an optional named character vector whose names indicate
#'   variables for which to use an alias within the generated function and whose
#'   elements provide the corresponding alias
#'
#' @param insert_functions an optional list of functions to add to rendered code
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
#' # Render model into raw R code
#' lotka_volterra_code <-
#'   csm_render_model(lotka_volterra_model,
#'                    output_type = "code",
#'                    language = "R")
#'
#' # Render model into a callable R function
#' lotka_volterra_fun <-
#'   csm_render_model(lotka_volterra_model,
#'                    output_type = "function",
#'                    language = "R")
#'
csm_render_model <- function(
    model,
    name = "dy_dt",
    output_type = c("function", "code"),
    language = c("R", "Rcpp"),
    arg_alias = NULL,
    insert_functions = NULL){

  output_type <- match.arg(output_type)
  language <- match.arg(language)

  if(language == "Rcpp" &
     !requireNamespace("Rcpp", quietly = TRUE)){
    paste0(
      "Rcpp package must be installed for language=",
      language, ".") |>
      stop()
  }

  if(language == "R"){
    comment_char <- "#"
    line_end <- ""
    assign_op <- "<-"
    l_bracket <- "["
    r_bracket <- "]"
    vector_base_adj <- 0
    declare_v_type <- FALSE
    v_types = c(scalar = "scalar",
                array = "vector")
  }else if(language == "Rcpp"){
    comment_char <- "//"
    line_end <- ";"
    assign_op <- "="
    l_bracket <- "["
    r_bracket <- "]"
    vector_base_adj <- -1
    declare_v_type <- TRUE
    v_types = c(scalar = "double",
                array = "NumericVector")
  }else if(language == "Fortran"){
    comment_char <- "!"
    line_end <- ""
    assign_op <- "="
    l_bracket <- "("
    r_bracket <- ")"
    vector_base_adj <- 0
    declare_v_type <- FALSE
    v_types = c(scalar = "",
                vector = "")
  }

  arg_names <- c(
    state_variables = "state_variables",
    parameters = if(!is.null(model$parameters)) "parms",
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

  if(language == "R"){
    dstate_dt <- paste0("d", arg_names["state_variables"], "_dt ", assign_op, " ",
                        "vector(\"numeric\", length(", arg_names["state_variables"], "))",
                        line_end)
    state_arg <- arg_names["state_variables"]
  }else if(language == "Rcpp"){
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
                               v_types, line_end, vector_base_adj,
                               declare_v_type)
  }

  if(language == "Rcpp"){
    dydt_args <-
      ifelse(arg_names %in% names(model$data_structures),
             "DataFrame",
             "NumericVector") |>
      paste0(" ", arg_names) |>
      paste0(collapse = ", ")
  }else{
    dydt_args <- paste0(arg_names, collapse = ", ")
  }

  if(language == "Rcpp"){
    dydt_signature <- paste0("List ", name, "(NumericVector t, ", dydt_args, ")")
  }else{
    dydt_signature <- paste0("function(t, ", dydt_args, ")")
  }

  state_indices <- seq_along(state) + vector_base_adj

  dydt_state <-
    c(code_comment("State variables:", comment_char),
      mapply(dydt_declaration,
             .i = state_indices,
             .name = names(state),
             .var = state,
             .var_type = c(scalar = v_types["scalar"]),
             .in = arg_names["state_variables"],
             .in_type = state_type,
             .comment_char = comment_char,
             declare_v_type = declare_v_type,
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
                       language),
           line_end),
    "}",
    ""
  )

  if(language == "Rcpp"){
    dydt_output <-
      c("#include <Rcpp.h>",
        "using namespace Rcpp;",
        "",
        "// [[Rcpp::export]]",
        dydt_output)
  }

  if(output_type == "function" &
     language == "R"){
    dydt_output <-
      parse(text = dydt_output) |>
      eval()
  }else if(output_type == "function" &
           language == "Rcpp"){
    Rcpp::sourceCpp(code = dydt_output)
  }

  return(dydt_output)

}

dydt_declaration <- function(.i, .name,
                             .var, .var_type = "",
                             .in, .in_type = "vector",
                             .comment_char = "#",
                             .line_end  = "",
                             lbracket = "[",
                             rbracket = "]",
                             declare_v_type = FALSE){
  if(.in_type %in% c("array", "vector", "NumericVector")){
    .in_ind <- paste0(lbracket, .i, rbracket)
  }else{
    .in_ind <- ""
  }
  label <-
    code_comment(paste0(.name, ": ",
                        .var,
                        " (", attr(.var, "units"), ")"),
                 .comment_char)
  if(! is.null(attr(.var, "equation")) &
     ! "csm_state" %in% class(.var)){
    definition <-
      paste0(.name, " = ",
             attr(.var, "equation")[2], .line_end)
  }else{
    definition <-
      paste0(.name, " = ",
             .in, .in_ind, .line_end)
  }
  if(declare_v_type) definition <- paste0(.var_type, " ", definition)
  c(label, definition)
}

dydt_rate_eqs <- function(.i, .state, .state_name, .line_end = ""){
  paste0("d", .state_name, "_dt[", .i, "] = ",
         attr(.state, "equation")[2], .line_end)
}

get_dots_args <- function(dots_list, arg_names, v_types = NULL){

  dots_type <-
    dots_list |>
    lapply(get_v_type, v_types = v_types) |>
    unlist()

  dots_name <-
    1:length(dots_list) |>
    lapply(get_arg_name,
           .v_list = dots_list,
           arg_names = arg_names) |>
    unlist()

  dots_type <- dots_type[names(dots_list) %in% arg_names]
  dots_name <- dots_name[names(dots_list) %in% arg_names]

  dots_args <-
    paste0(dots_type, " ", dots_name)

  return(dots_args)
}

get_v_type <- function(.v,
                       v_types = c(scalar = "scalar",
                                   array = "array")){
  if(is.null(v_types)){
    return("")
  }else if(length(.v) == 1){
    return(v_types["scalar"])
  }else{
    return(v_types["array"])
  }
}

get_arg_name <- function(.i, .v_list, arg_names){
  if(names(.v_list)[.i] %in% names(arg_names)){
    .arg_name <- arg_names[names(.v_list)[.i]]
  }else{
    # .arg_name <- paste0("arg_", .i)
    # Switch to using name as given in call to csm_create_dydt()
    .arg_name <- names(.v_list)[.i]
  }
  return(.arg_name)
}

get_declaration <- function(.d, dots_list, arg_names, v_types,
                            line_end, comment_char, v_base_adj,
                            declare_v_type){

  if(is_data_structure(dots_list[[.d]])){

    .v <- attr(dots_list[[.d]], "variables")

    v_indices <- seq_along(.v) + v_base_adj

    mapply(dydt_declaration,
           .i = v_indices,
           .name = names(.v),
           .var = .v,
           .var_type = v_types["scalar"],
           .in = "",
           .in_type = v_types["scalar"],
           .comment_char = comment_char,
           .line_end = line_end,
           declare_v_type = declare_v_type,
           SIMPLIFY = FALSE) |>
      lapply(paste0, collapse = "\n")

  }else{

    .d_name <- get_arg_name(.d, dots_list, arg_names)

    .d_type <- get_v_type(dots_list[[.d]], v_types)

    d_indices <- seq_along(dots_list[[.d]]) + v_base_adj

    mapply(dydt_declaration,
           .i = d_indices,
           .name = names(dots_list[[.d]]),
           .var = dots_list[[.d]],
           .var_type = v_types["scalar"],
           .in = .d_name,
           .in_type = .d_type,
           .comment_char = comment_char,
           .line_end = line_end,
           declare_v_type = declare_v_type,
           SIMPLIFY = FALSE) |>
      lapply(paste0, collapse = "\n")

  }
}

is_csm_parameter <- function(.v){
  "csm_parameter" %in% class(.v)
}

get_dydt_dots <- function(dots_list, arg_names, comment_char = "#",
                          v_types, line_end, v_base_adj,
                          declare_v_type){

  dots_declaration <-
    1:length(dots_list) |>
    lapply(get_declaration,
           dots_list = dots_list,
           arg_names = arg_names,
           v_types = v_types,
           comment_char = comment_char,
           line_end = line_end,
           v_base_adj = v_base_adj,
           declare_v_type = declare_v_type)

  dots_is_parameter <-
    dots_list |>
    lapply(\(.x) any(sapply(.x, is_csm_parameter))) |>
    unlist()

  if(!any(dots_is_parameter)){
    dydt_parameters <- NULL
  }else{
    dydt_parameters <-
      {dots_declaration[dots_is_parameter]} |>
      unlist() |>
      c(code_comment("Parameters:", comment_char), x = _) |>
      strsplit(split = "\n") |>
      unlist()
  }

  dots_is_data_structure <-
    dots_list |>
    lapply(\(.x) is_data_structure(.x)) |>
    unlist()

  if(!any(dots_is_data_structure)){
    dydt_data_structures <- NULL
  }else{
    dydt_data_structures <-
      {dots_declaration[dots_is_data_structure]} |>
      unlist() |>
      c(code_comment("Data Structures:", comment_char), x = _) |>
      strsplit(split = "\n") |>
      unlist()
  }

  if(all(dots_is_parameter | dots_is_data_structure)){
    dydt_other_variables <- NULL
  }else{
    dydt_other_variables <-
      dots_declaration[!(dots_is_parameter | dots_is_data_structure)] |>
      unlist() |>
      c(code_comment("Other Variables:", comment_char), x = _) |>
      strsplit(split = "\n") |>
      unlist()
  }

  dydt_dots <- c(dydt_parameters,
                 "",
                 dydt_data_structures,
                 "",
                 dydt_other_variables,
                 "")

  return(dydt_dots)
}

code_comment <- function(the_comment, comment_char = "#"){
  paste0(comment_char, " ", the_comment)
}

code_return <- function(return_variable, language){
  if(language == "R"){
    return_code <- paste0("return(list(", return_variable, "))")
  }else if(language == "Rcpp"){
    return_code <- paste0("return List::create(", return_variable, ")")
  }else{
    return_code <- return_variable
  }
  return(return_code)
}

