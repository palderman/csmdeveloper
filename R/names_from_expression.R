
names_from_expression <- function(expr){
  if(is.call(expr)){
    expr |>
      as.list() |>
      tail(-1) |>
      lapply(names_from_expression) |>
      unlist()
  }else if(is.symbol(expr)){
    as.character(expr)
  }
}
