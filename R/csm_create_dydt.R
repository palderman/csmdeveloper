#' Create a function for rate of change of
#' Cropping System Model (CSM) state variables
#'
#' @md
#'
#' @param name name of the resulting function
#' @param state a list vector containing CSM state
#'   variables defined using [csmdeveloper::csm_create_state()]
#'   in the intended order
#' @param ... optional arguments of list vectors containing
#'   CSM parameters defined using [csmdeveloper::csm_create_parameter()]
#'   or CSM variables defined using [csmdeveloper::csm_create_variable()]
#' @param arg_names an optional character vector whose
#'   elements give the names of variables in `...` for which
#'   to add an argument to the generated function. By default
#'   arguments will be added for all variables in `...`.
#' @param output_type a character value indicating the
#'   type of output to produce
#' @param comment_char a character value indicating
#'   which character to use as a prefix to comments
#'   in the generated code
#' @param line_end a character value indicating which
#'   character(s) to indicate the end of a line of
#'   source code
#' @param v_types a named character vector indicating
#'   the names of the variable types for scalars and
#'   vectors in the target output language
#'
#' @export
#'
csm_create_dydt <- function(
    name,
    state,
    ...,
    arg_names = NULL,
    output_type = c("Rfunction", "Rcode", "deSolve"),
    comment_char = "#",
    line_end = "",
    v_types = c(scalar = "",
                vector = "")){

  output_type <- match.arg(output_type)

  dots_list <- list(...)

  state_type <- get_v_type(state)

  if(is.null(arg_names)){
    arg_names <- c("state",
                   names(dots_list))
  }else if(! "state" %in% arg_names){
    arg_names <- c("state",
                   arg_names)
  }

  if(is.null(names(arg_names))){
    arg_names <- setNames(arg_names, arg_names)
  }

  if(output_type %in% c("Rfunction", "Rcode", "deSolve")){
    dstate_dt <- paste0("d", arg_names["state"], "_dt = ",
                        "vector(\"numeric\", length(", arg_names["state"], "))",
                        line_end)
    state_arg <- arg_names["state"]
  }else{
    if(state_type %in% c("array", "vector")){
      dstate_dt <- paste0(state_type,"[",length(state),"] d", arg_names["state"], "_dt", line_end)
    }else{
      dstate_dt <- paste0(state_type," d", arg_names["state"], "_dt", line_end)
    }
    state_arg <- paste0(state_type, " ", arg_names["state"], line_end)
  }

  if(length(dots_list) > 0){
    dots_args <- get_dots_args(dots_list, arg_names)
  }else{
    dots_args <- NULL
  }

  dydt_args <- paste0(c(state_arg, dots_args),
                      collapse = ", ")

  if(output_type == "deSolve"){
    dydt_signature <- paste0("function(t, ", dydt_args, ")")
  }else{
    dydt_signature <- paste0("function(", dydt_args, ")")
  }

  dydt_state <-
    c(code_comment("State variables:", comment_char),
      mapply(dydt_declaration,
             .i = 1:length(state),
             .name = names(state),
             .var = state,
             .var_type = v_types["scalar"],
             .in = arg_names["state"],
             .in_type = state_type,
             .comment_char = comment_char,
             .line_end = line_end),
      ""
    )

  if(length(dots_list) == 0){
    dydt_dots <- NULL
  }else{
    dydt_dots <- get_dydt_dots(dots_list, arg_names, comment_char)
  }

  dydt_rates <-
    c(
      code_comment("Calculation of rate of change for state variables:",
                   comment_char),
      mapply(dydt_rate_eqs,
             .i = 1:length(state),
             .state = state,
             .state_name = arg_names["state"]),
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
           code_return(paste0("d", arg_names["state"], "_dt"),
                       output_type),
           line_end),
    "}",
    ""
  )

  if(output_type %in% c("Rfunction", "deSolve")){
    dydt_output <-
      parse(text = dydt_output) |>
      eval()
  }

  return(dydt_output)
}

dydt_declaration <- function(.i, .name,
                             .var, .var_type = "",
                             .in, .in_type = "vector",
                             .comment_char = "#",
                             .line_end  = ""){
  if(.in_type %in% c("array", "vector")){
    .in_ind <- paste0("[", .i, "]")
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
      paste0(.var_type, " ", .name, " = ",
             attr(.var, "equation")[2], .line_end)
  }else{
    definition <-
      paste0(.var_type, " ", .name, " = ",
             .in, .in_ind, .line_end)
  }
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

get_declaration <- function(.d, dots_list, arg_names){

  .d_name <- get_arg_name(.d, dots_list, arg_names)

  .d_type <- get_v_type(dots_list[[.d]])

  mapply(dydt_declaration,
         .i = 1:length(dots_list[[.d]]),
         .name = names(dots_list[[.d]]),
         .var = dots_list[[.d]],
         .in = .d_name,
         .in_type = .d_type,
         SIMPLIFY = FALSE) |>
    lapply(paste0, collapse = "\n")
}

is_csm_parameter <- function(.v){
  "csm_parameter" %in% class(.v)
}

get_dydt_dots <- function(dots_list, arg_names, .comment_char = "#"){

  dots_declaration <-
    1:length(dots_list) |>
    lapply(get_declaration,
           dots_list = dots_list,
           arg_names = arg_names) |>
    unlist()

  dots_is_parameter <-
    dots_list |>
    lapply(\(.x) sapply(.x, is_csm_parameter)) |>
    unlist()

  if(!any(dots_is_parameter)){
    dydt_parameters <- NULL
  }else{
    dydt_parameters <-
      dots_is_parameter |>
      (\(.x) dots_declaration[.x])() |>
      (\(.x) c(code_comment("Parameters:", .comment_char),
               .x))() |>
      strsplit(split = "\n") |>
      unlist()
  }

  if(all(dots_is_parameter)){
    dydt_other_variables <- NULL
  }else{
    dydt_other_variables <-
      dots_is_parameter |>
      (\(.x) dots_declaration[!.x])() |>
      (\(.x) c(code_comment("Other Variables:", .comment_char),
               .x))() |>
      strsplit(split = "\n") |>
      unlist()
  }

  dydt_dots <- c(dydt_parameters,
                 "",
                 dydt_other_variables,
                 "")

  return(dydt_dots)
}

code_comment <- function(the_comment, comment_char = "#"){
  paste0(comment_char, " ", the_comment)
}

code_return <- function(return_variable,
                        output_type){
  if(output_type %in% c("Rfunction", "Rcode")){
    return_code <- paste0("return(", return_variable, ")")
  }else if(output_type == "deSolve"){
    return_code <- paste0("return(list(", return_variable, "))")
  }else{
    return_code <- return_variable
  }
  return(return_code)
}
