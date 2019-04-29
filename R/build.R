#' Betty Build
#'
#' Builds pkgdown site and source package from  a git remote and optionally
#' publish it directly to \url{https://docs.ropensci.org}.
#'
#' @rdname build
#' @inheritParams deploy_site
#' @param remote url of the git remote
#' @param dest path of volume to save docs and src output
#' @export
#' @examples \dontrun{
#' build_site('https://github.com/ropensci/magick')
#' }
build_site <- function(remote, dest = ".", deploy_url = 'https://docs.ropensci.org'){
  doc_dir <- paste0(dest, "/docs/")
  src_dir <- paste0(dest, "/src/")

  # Clone the URL and change dir
  src <- tempfile()
  gert::git_clone(remote, src, verbose = TRUE)
  pwd <- getwd()
  on.exit(setwd(pwd))
  setwd(src)
  if(!file.exists('DESCRIPTION'))
    stop("Remote does not contain an R package")

  if(!("readme.md" %in% tolower(list.files())))
    stop("Package does not contain a readme.md file")

  # Install package locally
  remotes::install_deps(dependencies = TRUE, upgrade = TRUE)
  pkgfile <- pkgbuild::build(dest_path = tempdir())
  remotes::install_local(pkgfile, build = FALSE)
  pkg <- strsplit(basename(pkgfile), "_", fixed = TRUE)[[1]][1]

  # Build the website
  title <- sprintf("rOpenSci: %s", pkg)
  url <- paste0(deploy_url, "/", pkg)
  dest <- paste0(doc_dir, pkg)
  tmp <- paste0(dest, "_TMP")
  template <- list(package = "rotemplate")
  unlink(tmp, recursive = TRUE)

  # Remove temp site in case of failure
  on.exit(unlink(tmp, recursive = TRUE))
  pkgdown::build_site(document = FALSE, preview = FALSE, override =
    list(destination = tmp, title = title, url = url, template = template))

  # Save some info about the repo
  head <- gert::git_log(max = 1, repo = src)
  jsonlite::write_json(list(commit = as.list(head), remote = remote, pkg = pkg),
                       file.path(tmp, 'info.json'), auto_unbox = TRUE)

  # Store the source pkg and update repo (todo: use cranlike)
  dir.create(src_dir, showWarnings = FALSE)
  unlink(sprintf("%s%s_*.tar.gz", src_dir, pkg))
  file.copy(pkgfile, src_dir)
  tools::write_PACKAGES(src_dir)

  # Move site to final location
  unlink(dest, recursive = TRUE)
  file.rename(tmp, dest)
  invisible(dest)
}

#' @export
#' @rdname build
build_all_sites <- function(dest = "."){
  registry <- "https://ropensci.github.io/roregistry/registry.json"
  packages <- jsonlite::fromJSON(registry)$packages
  success <- vector("list", 10)
  for(i in 1:nrow(packages)){
    url <- packages[i, "url"]
    success[[i]] <- tryCatch(build_site(url, dest = dest), error = function(e){
      cat(sprintf("Failure for: %s:\n", url))
      print(e)
      return(e$message)
    })
  }
  names(success) <- packages$name
  jsonlite::write_json(success, file.path(dest, 'build.json'), auto_unbox = TRUE)
}
