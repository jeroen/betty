#' CRAN tools
#'
#' Tools for getting CRAN metadata
#'
#' @export
#' @rdname cran
cran_registry <- function(){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  curl::curl_download('https://cloud.r-project.org/web/packages/packages.rds', destfile = tmp)
  packages <- tibble::as_tibble(readRDS(tmp))
  input <- paste(packages$BugReports, packages$URL)
  pattern <- 'https?://(github.com|gitlab.com|bitbucket.org)/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+'
  m <- regexpr(pattern, input, ignore.case = TRUE)
  rows <- !is.na(m) & m > -1
  urls <- regmatches(input, m)
  packages[rows,'Git'] <- sub("^http://", "https://", tolower(urls))
  return(packages)
}

#' @export
sync_cran_dev <- function(){
  packages <- cran_registry()
  packages <- packages[!is.na(packages$Git),]
  for(i in seq_len(nrow(packages))){
    pkg <- as.list(packages[i,])
    cat("Testing", pkg$Package, "from", pkg$Git, "...")
    desc_url <- paste0(pkg$Git, '/raw/master/DESCRIPTION')
    tryCatch ({
      desc <- read_description(desc_url)
      if(desc$Package != pkg$Package){
        stop(paste("Package name does not match description:", desc$Package))
      }
      message("OK!")
    }, error = function(e){
      message("Failure for ", pkg$Package, ": ", e$message)
    })
  }
}

read_description <- function(desc_url){
  con <- curl::curl(desc_url)
  on.exit(close(con))
  tibble::as_tibble(read.dcf(con))
}
