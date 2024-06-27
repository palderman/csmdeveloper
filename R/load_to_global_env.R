#' @export
#' @noRd
load_to_global_env <- function(fun_names){
  backup_list <- NULL
  rm_list <- NULL
  for(fun in fun_names){
    if(!exists(fun, envir = .GlobalEnv)){
      rm_list <- c(rm_list, fun)
    }else{
      backup_list <- c(
        backup_list,
        setNames(
          list(get(fun, envir = .GlobalEnv)),
          fun
        )
      )
    }
    assign(fun,
           get(fun, envir = parent.frame()),
           envir = .GlobalEnv)
  }
  assign("rm_list",
         rm_list,
         envir = parent.frame())
  assign("backup_list",
         backup_list,
         envir = parent.frame())
}

#' @export
#' @noRd
cleanup_global_env <- function(){
  if(exists("rm_list", envir = parent.frame())){
    rm(list = get("rm_list", envir = parent.frame()),
       envir = .GlobalEnv)
  }
  if(exists("backup_list", envir = parent.frame())){
    backup_list <- get("backup_list", envir = parent.frame())
    for(fun in names(backup_list)){
      assign(fun, backup_list[[fun]], envir = .GlobalEnv)
    }
  }
}
