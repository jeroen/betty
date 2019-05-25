#' Cleanup Docs
#'
#' Delete docs for removed packages
#'
#' @export
remove_deleted_docs <- function(){
  packages <- jsonlite::fromJSON("https://ropensci.github.io/roregistry/registry.json")$packages
  docs <- list_all_docs()
  pkgs <- docs[!(docs %in% c(packages$name, 'ropensci-docs.github.io'))]
  cat("Removed packages: ", paste(pkgs, collapse = ', '), "\n")
  if(askYesNo("are you sure you want to delete these?")){
    lapply(pkgs, function(name){
      message("Deleting: ", name)
      gh::gh(paste0('/repos/ropensci-docs/', name), .method = 'DELETE')
    })
  }
  invisible()
}

list_all_docs <- function(){
  page <- 1
  repos <- NULL
  repeat {
    out <- gh::gh(paste0('/orgs/ropensci-docs/repos?per_page=100&page=', page))
    names <- unlist(lapply(out, `[[`, 'name'))
    repos <- c(repos, names)
    if(length(names) < 100)
      return(repos)
    page <- page + 1
  }
}
