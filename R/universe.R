#' Update Universe
#'
#' Update git reference in the meta repository.
#'
#' @export
#' @param remote URL of the upstream git repository to add
#' @param dirname name of the submodule to add
#' @param ref which branch or commit to checkout for this submodule
#' @param dest root of the data drive
update_universe <- function(remote, dirname = basename(remote), ref = 'master', dest = "."){
  dest <- normalizePath(dest, mustWork = TRUE)
  universe <- file.path(dest, "universe")
  repo <- tryCatch({
    gert::git_open(universe)
  }, error = function(e){
    gert::git_clone("https://github.com/r-universe/ropensci", universe)
  })

  # Copied from deploy, todo: factor out
  pwd <- getwd()
  on.exit(setwd(pwd))
  setwd(universe)

  # Sync with upstream universe
  gert::git_fetch('origin')
  gert::git_reset_hard('origin/master')

  # Initiate or update the package submodule
  if(sys::exec_wait("git", c("submodule", "status", dirname), std_err = FALSE) == 0){
    sys::exec_wait("git", c("submodule", "update", "--init", "--remote", dirname))
  } else {
    sys::exec_wait("git", c("submodule", "add", remote, dirname))
  }
  #setwd(dirname)
  #sys::exec_internal("git", c("checkout", ref))
  #setwd("..")

  # In case a concurrent package just pushed a commit
  gert::git_pull()

  # Update the submodule and commit
  gert::git_add(dirname)
  if(!any(gert::git_status()$staged)){
    cat(sprintf("Submodule %s already seems up-to-date\n", dirname), file = stderr())
  } else {
    package <- read.dcf(file.path(dirname, 'DESCRIPTION'))[[1,'Package']]
    version <- read.dcf(file.path(dirname, 'DESCRIPTION'))[[1,'Version']]
    subrepo <- gert::git_open(dirname)
    stopifnot(basename(gert::git_info(repo = subrepo)$path) == dirname)
    commit <- gert::git_log(repo = subrepo, max = 1)
    commit_for_ropensci(message = paste(package, version), commit$author, commit$time)
    gert::git_push()
  }
}
