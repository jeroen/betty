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

  # Set author signature
  author_email <- Sys.getenv("GIT_EMAIL", NA)
  author_sig <- if(is.na(author_email)){
    git_signature_default()
  } else {
    author_name <- Sys.getenv('GIT_USER', 'rOpenSci user')
    git_signature(name = name, email = author_email)
  }

  # Set the user signature
  commit_sig <- git_signature(name = 'rOpenSci', email = 'myrmecocystus+ropenscibot@gmail.com')

  # Copied from deploy, todo: factor out
  pwd <- getwd()
  on.exit(setwd(pwd))
  setwd(universe)

  # Force sync with upstream: TODO: port to gert
  sys::exec_wait("git", c("fetch", '--all'))
  sys::exec_wait("git", c('reset', '--hard', 'origin/master'))

  # Initiate or update the package submodule
  if(sys::exec_wait("git", c("submodule", "status", dirname), std_err = FALSE) == 0){
    sys::exec_wait("git", c("submodule", "update", "--init", "--remote", dirname))
  } else {
    sys::exec_wait("git", c("submodule", "add", remote, dirname))
  }
  setwd(dirname)
  sys::exec_internal("git", c("checkout", ref))
  setwd("..")
  gert::git_add(dirname)
  gert::git_add(".")
  if(nrow(gert::git_status()) == 0){
    cat(sprintf("Submodule %s already seems up-to-date\n", dirname), file = stderr())
  } else {
    package <- read.dcf(file.path(dirname, 'DESCRIPTION'))[[1,'Package']]
    version <- read.dcf(file.path(dirname, 'DESCRIPTION'))[[1,'Version']]
    commit_msg <- paste(package, version, "\n\n\non-behalf-of: @ropensci <support@ropensci.org>")
    gert::git_commit(message = commit_msg, author = author_sig, committer = commit_sig)
    gert::git_push()
  }
}
