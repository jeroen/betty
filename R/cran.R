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
  statusvec <- rep(NA_integer_, nrow(packages))
  pool <- curl::new_pool()
  lapply(seq_len(nrow(packages)), function(i){
    k <- i
    pkg <- as.list(packages[k,])
    desc_url <- paste0(pkg$Git, '/raw/master/DESCRIPTION')
    curl::curl_fetch_multi(desc_url, done = function(res){
      statusvec[k] <<- res$status
      message("Done ", pkg$Package, " from ", pkg$Git,  ": ", res$status)
    }, fail = function(e){
      message("Failure for ", pkg$Package, ": ", e$message)
    }, pool = pool)
  })
  curl::multi_run(pool = pool)
  packages$status <- statusvec
  return(packages)
}

read_description <- function(desc_url){
  con <- curl::curl(desc_url)
  on.exit(close(con))
  tibble::as_tibble(read.dcf(con))
}
