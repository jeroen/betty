.onLoad <- function(libname, pkgname){
  options(menu.graphics = FALSE, pkgdown.timeout = 3600)
  dir.create("~/.R", showWarnings = FALSE)
  file.create("~/.R/Makevars", showWarnings = FALSE)

  # Fall back on anonymous PAT for rate limits
  if(is.na(Sys.getenv('GITHUB_PAT', NA))){
    default_pat <- utils::getFromNamespace('github_pat', 'remotes')
    Sys.setenv(CI = 1)
    Sys.setenv(GITHUB_PAT = default_pat())
  }
}
