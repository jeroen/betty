.onLoad <- function(libname, pkgname){
  options(menu.graphics = FALSE)
  dir.create("~/.R", showWarnings = FALSE)
  file.create("~/.R/Makevars", showWarnings = FALSE)
}
