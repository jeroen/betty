#' Update Universe
#'
#' Update git reference in the meta repository.
#'
#' @export
#' @param remote URL of the upstream git repository to add
#' @param package name of the package to add
#' @param ref which branch or commit to checkout for this submodule
#' @param dest root of the data drive
update_universe <- function(remote, package = basename(remote), ref = 'master', dest = "."){
  dest <- normalizePath(dest, mustWork = TRUE)
  universe <- file.path(dest, "universe")
  repo <- tryCatch({
    gert::git_pull(repo = gert::git_open(universe))
  }, error = function(e){
    gert::git_clone("https://github.com/r-universe/ropensci", universe)
  })

  # Copied from deploy, todo: factor out
  pwd <- getwd()
  on.exit(setwd(pwd))
  setwd(universe)
  git_user <- Sys.getenv("GIT_USER", "ropenscibot")
  git_email <- Sys.getenv("GIT_EMAIL", "myrmecocystus+ropenscibot@gmail.com")
  gert::git_config_set('user.name', git_user)
  gert::git_config_set('user.email', git_email)

  if(file.exists(package)){
    sys::exec_internal("git", c("submodule", "update", "--init", "--remote", package))
  } else {
    sys::exec_internal("git", c("submodule", "add", remote, package))
  }
  setwd(package)
  sys::exec_internal("git", c("checkout", ref))
  setwd("..")
  gert::git_add(package)
  gert::git_add(".")
  if(nrow(gert::git_status()) == 0){
    cat(sprintf("Submodule %s already seems up-to-date\n", package), file = stderr())
  } else {
    gert::git_commit(paste('Update submodule:', package))
    gert::git_push()
  }
}
