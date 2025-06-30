#' Save an `rjuliabugs` Object and Its Julia State
#'
#' Serializes the Julia object contained in an `rjuliabugs` object and saves
#' the entire object as an `.rds` file. The Julia object is saved separately
#' using Julia's `Serialization.serialize`. The file path can be passed manually,
#' or retrieved from the `chains_file` slot in the object.
#'
#' @param rjuliabugs_model An object of class `rjuliabugs`, containing at least
#'   the fields `name` (Julia object name as a string) and `chains_file`.
#' @param file A character string giving the base name or path for saving the `.rds` file.
#'   If the extension `.rds` is missing, it will be appended automatically.
#' @param chains_file Optional character string giving the path where the Julia
#'   object should be serialized. If `NULL`, uses the `chains_file` field from
#'   `rjuliabugs_model`.
#'
#' @return Returns `invisible(NULL)`. Used for its side effects: saving both the
#'   Julia object and the R `rjuliabugs` object to disk.
#'
#' @examples
#' \dontrun{
#' save_rjuliaBUGS(my_model, file = "my_model", chains_file = "chains.jls")
#' }
#'
#' @export
save_rjuliaBUGS <- function(rjuliabugs_model,
                            file,
                            chains_file = NULL){

  if(is.null(rjuliabugs_model$name)){
    stop("No valid name for the the Chains object from rjuliabugs object.")
  }

  if(is.null(chains_file)){

    if(is.null(rjuliabugs_model$chains_file)){
        stop("No valid path for the 'chains_file' is defined in rjuliabugs_model object.")
    } else {
      chains_file_path <- rjuliabugs_model$chains_file
    }

  } else {

    chains_file_path <- chains_file
    rjuliabugs_model$chains_file <- chains_file_path
  }

  if (!endsWith(chains_file_path, ".jls")) {
    stop(sprintf("Invalid `chains_file`: '%s' does not end with '.jls'", chains_file_path))
  }



  JuliaCall::julia_eval(paste0(cmd = 'Serialization.serialize("',chains_file_path,'",',rjuliabugs_model$name,')'),need_return = "Julia")

  if (!endsWith(tolower(file), ".rds")) {
    file <- paste0(file, ".rds")
  }

  saveRDS(rjuliabugs_model, file = file)

  return(invisible(NULL))

}

#' Load an `rjuliabugs` Object and Restore the Julia State
#'
#' Loads an object of class `rjuliabugs` from an `.rds` file and restores the
#' associated Julia object using Julia’s `Serialization.deserialize`. Both
#' `name` and `chains_file` fields must be present in the stored object.
#'
#' @param file A character string giving the path to the `.rds` file.
#'
#' @return An object of class `rjuliabugs`, with its Julia object restored in
#'   the current Julia session.
#'
#' @examples
#' \dontrun{
#' model <- load_rjuliaBUGS("my_model.rds")
#' }
#'
#' @export
load_rjuliaBUGS <- function(file){

  rjuliabugs_obj <- readRDS(file = file)

  if(is.null(rjuliabugs_obj$name) || is.null(rjuliabugs_obj$chains_file)){
    stop("Both chain 'name' and 'chains_file' from  the rjuliasampler object must be defined.")
  } else {
    JuliaCall::julia_eval(paste0(rjuliabugs_obj$name,' = Serialization.deserialize("',rjuliabugs_obj$chains_file,'")'))
  }

  return(rjuliabugs_obj)

}


