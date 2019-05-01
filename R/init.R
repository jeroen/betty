.onLoad <- function(libname, pkgname){
  options(menu.graphics = FALSE, pkgdown.timeout = 3600)
  dir.create("~/.R", showWarnings = FALSE)
  file.create("~/.R/Makevars", showWarnings = FALSE)

  # Fall back on anonymous PAT for rate limits
  if(is.na(Sys.getenv('GITHUB_PAT', NA))){
    Sys.setenv(CI = 1)
    Sys.setenv(GITHUB_PAT = remotes:::github_pat())
  }
}
