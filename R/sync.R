#' Sync rOpenSci with Jenkins
#'
#' Checks the rOpenSci registery and add/delete corresponding projects on Jenkins
#' and the ropensci-docs repositories on Github.
#'
#' @export
#' @rdname sync_ropensci
#' @importFrom jenkins jenkins
#' @param update_jobs update the xml config of existing repos.
#' @param remove_jobs delete jobs that are no longer in the registry
#' @param update_views update the views (per-author package list)
sync_ropensci_jenkins <- function(update_jobs = FALSE, remove_jobs = TRUE, update_views = TRUE){
  anychange <- FALSE
  jk <- jenkins::jenkins('http://jenkins.ropensci.org')
  jobs <- jk$project_list()
  url <- "https://ropensci.github.io/roregistry/registry.json"
  packages <- jsonlite::fromJSON(url)$packages
  for(i in seq_len(nrow(packages))){
    name <- packages[i, "name"]
    git_url <- packages[i, "url"]
    xml <- config_template(git_url)
    if(name %in% jobs$name){
      job <- as.list(jobs[jobs$name == name,])
      if(isTRUE(update_jobs)){
        caterr(sprintf("Updating job config for %s...", name))
        jk$project_update(name, xml_string = xml)
        anychange <- TRUE
      } else if(job$git != git_url){
        caterr(sprintf("Updating git URL for job %s (%s -> %s)...", name, git_url, job$git))
        jk$project_update(name, xml_string = xml)
      } else {
        caterr(sprintf("Job config for %s already exists...", name))
      }
    } else {
      caterr(sprintf("Creating new job for %s...", name))
      jk$project_create(name, xml_string = xml)
      jk$project_build(name)
      anychange <- TRUE
    }
    caterr("OK!\n")
  }
  if(isTRUE(remove_jobs)){
    gone <- !(jobs$name %in% packages$name)
    lapply(jobs$name[gone], function(name){
      caterr(sprintf("Deleting job %s which is no longer in the roregistry...", name))
      jk$project_delete(name)
      caterr("OK!\n")
    })
    if(length(jobs$name[gone])){
      anychange <- TRUE
    }
  }
  if(isTRUE(update_views) && isTRUE(anychange)){
    views <- jk$view_list()
    packages$maintainer <- asciify(packages$maintainer)
    authors <- unique(packages$maintainer)
    lapply(authors, function(author){
      pkg_names = packages[packages$maintainer == author, "name"]
      if(!length(pkg_names))
        stop(sprintf("Failed to find packages for author %s", author))
      xml <- view_template(pkg_names)
      if(author %in% views$name){
        caterr(sprintf("Updating view for %s...", author))
        jk$view_update(author, xml_string = xml)
      } else {
        caterr(sprintf("Creating new view for %s...", author))
        jk$view_create(author, xml_string = xml)
      }
      caterr("OK!\n")
    })
    views_gone <- !(views$name %in% c(authors, 'all'))
    lapply(views$name[views_gone], function(author){
      caterr(sprintf("Deleting view %s which is no longer a maintainer...", author))
      jk$view_delete(author)
      caterr("OK!\n")
    })
  }
  invisible(jk$server_info())
}

#' @export
#' @rdname sync_ropensci
#' @param update_sitemap generate updated sitemap.xml and index.html files
sync_ropensci_docs <- function(update_sitemap = TRUE){
  registry <- jsonlite::fromJSON("https://ropensci.github.io/roregistry/registry.json")
  packages <- c(registry$packages$name, 'ropensci-docs.github.io')
  repos <- get_docs_repos()
  added <- packages[!(packages %in% repos)]
  message("Authenticated as", gh::gh_whoami()$name)
  if(length(added)){
    caterr("Adding new packages: ", paste(added, collapse = ', '), "\n")
    if(utils::askYesNo("are you sure you want to add these packages?")){
      lapply(added, create_new_docs_repo)
    }
  }
  deleted <- repos[!(repos %in% packages)]
  if(length(deleted)){
    caterr("Removed packages: ", paste(deleted, collapse = ', '), "\n")
    if(utils::askYesNo("are you sure you want to delete these?")){
      lapply(deleted, function(name){
        message("Deleting: ropensci-docs/", name)
        gh::gh(paste0('/repos/ropensci-docs/', name), .method = 'DELETE')
      })
    }
  }
  message("Everything in sync!")
  if(isTRUE(update_sitemap)){
    message("Updating sitemap...")
    sync_sitemap()
  }
  invisible()
}

#' @export
#' @rdname sync_ropensci
sync_ropensci_dev <- function(){
  registry <- jsonlite::fromJSON("https://ropensci.github.io/roregistry/registry.json")
  pkgs <- jsonlite::fromJSON('https://dev.ropensci.org/packages')
  deleted <- pkgs[!(pkgs %in% registry$packages$name)]
  if(length(deleted)){
    caterr("Removed packages: ", paste(deleted, collapse = ', '), "\n")
    if(utils::askYesNo("are you sure you want to delete these from the repository?")){
      lapply(deleted, function(package){
        message("Deleting: ", package)
        h <- curl::new_handle(customrequest = 'DELETE', userpwd = Sys.getenv("DEV_USERPWD"))
        url <- sprintf("https://dev.ropensci.org/packages/%s", package)
        out <- parse_res(curl::curl_fetch_memory(url, handle = h))
        stopifnot(out$Package == package)
      })
    }
  }
  sync_ropensci_universe()
}

#' @export
#' @rdname sync_ropensci
list_missing_docs <- function(){
  packages <- jsonlite::fromJSON("https://ropensci.github.io/roregistry/registry.json")$packages
  df <- jsonlite:::simplify(list_ropensci_docs_repos())
  names <- subset(df, active == FALSE)$name
  subset(packages, name %in% names, select = c("name", "url"))
}

sync_ropensci_universe <- function(){
  universe <- jsonlite::fromJSON('https://api.github.com/repos/r-universe/ropensci/contents/')
  packages <- jsonlite::fromJSON("https://ropensci.github.io/roregistry/registry.json")$packages
  packages <- packages[order(as.Date(packages$date_last_commit)),]
  missing <- packages[!(packages$name %in% universe$name),]
  for(i in seq_len(nrow(missing))){
    update_universe(missing$url[i], missing$name[i])
  }
}

#' @export
#' @rdname sync_ropensci
list_ropensci_docs_repos <- function(){
  repos <- gh::gh('/users/ropensci-docs/repos?per_page=100', .limit = 1e6)
  lapply(repos, function(x){
    x$active = abs(parse_time(x$pushed_at) - parse_time(x$created_at)) > 1
    return(x)
  })
}

#' @param active_only only list repositories which have content in them
get_docs_repos <- function(active_only = FALSE){
  out <- list_ropensci_docs_repos()
  if(isTRUE(active_only))
    out <- Filter(function(x){isTRUE(x$active)}, out)
  unlist(lapply(out, `[[`, 'name'))
}

update_sitemap <- function(path){
  sites <- get_docs_repos(active_only = TRUE)

  skiplist <- 'ropensci-docs.github.io'
  sites <- Filter(function(x){!(x %in% skiplist)}, sites)

  # Generate sitemap.xml
  body <- sprintf("  <url>\n    <loc>https://docs.ropensci.org/%s/</loc>\n  </url>", sites)
  sitemap <- paste(c('<?xml version="1.0" encoding="UTF-8"?>',
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
    body, '</urlset>'), collapse = '\n')
  writeLines(sitemap, file.path(path, 'sitemap.xml'))

  # Generate index.html
  template <- system.file('templates/index.html', package = 'betty')
  input <- rawToChar(readBin(template, raw(), file.info(template)$size))
  li <- sprintf('  <li><a href="https://docs.ropensci.org/%s/">%s</a></li>', sites, sites)
  output <- sub('INSERT_REPO_LIST', paste(li, collapse = '\n'), input)
  writeLines(output, file.path(path, 'index.html'))
}

sync_sitemap <- function(){
  tmpdir <- tempfile()
  repo <- gert::git_clone('https://github.com/ropensci-docs/ropensci-docs.github.io', tmpdir)
  update_sitemap(tmpdir)
  gert::git_add(c('index.html', 'sitemap.xml'), repo = repo)
  if(any(gert::git_status(repo = repo)$staged)){
    gert::git_commit(sprintf("Update sitemap (%s)", Sys.Date()), repo = repo)
    gert::git_push(repo = repo)
  } else {
    message("No changes in sitemap")
  }
}

parse_time <- function(str){
  strptime(str, '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC')
}

#' @export
#' @param git_url HTTPS git url of the target repository
#' @rdname sync_ropensci
config_template <- function(git_url){
  if(!grepl("^https://", git_url))
    stop("Please use https git URL")
  template <- system.file('templates/config.xml', package = 'betty')
  input <- rawToChar(readBin(template, raw(), file.info(template)$size))
  gsub("INSERT_GIT_REPO_URL", git_url, input, fixed = TRUE)
}

#' @export
#' @param view_jobs Character vector with jobs to add to this view
#' @rdname sync_ropensci
view_template <- function(view_jobs){
  template <- system.file('templates/view.xml', package = 'betty')
  input <- rawToChar(readBin(template, raw(), file.info(template)$size))
  jobstring <- paste(sprintf('    <string>%s</string>', view_jobs), collapse = "\n")
  gsub("INSERT_VIEW_JOBS", jobstring, input, fixed = TRUE)
}

create_new_docs_repo <- function(name){
  message("Creating: ropensci-docs/", name)
  description <- paste0('auto-generated pkgdown website for: ', name)
  homepage <- paste0("https://docs.ropensci.org/", name)
  gh::gh('/orgs/ropensci-docs/repos', .method = 'POST',
         name = name, description = description, homepage = homepage,
         has_issues = FALSE, has_wiki = FALSE)
}

set_repo_homepage <- function(repo, homepage){
  gh::gh(paste0('/repos/', repo), .method = 'PATCH', homepage = homepage)
}

disable_legacy_pkgdown <- function(repo){
  testurl <- sprintf('https://ropensci.github.io/%s/pkgdown.yml', basename(repo))
  req <- curl::curl_fetch_memory(testurl)
  if(req$status_code == 200){
    if(!grepl("https://ropensci.github.io", req$url)){
      message(sprintf("Pkgdown site for %s has custom domain: %s", basename(repo), dirname(req$url)))
      return()
    }
    message("Disabling legacy site for:", dirname(testurl))
    endpoint <- sprintf("/repos/%s/pages", repo)
    gh::gh(endpoint, .method = 'DELETE', .send_headers = c(Accept = 'application/vnd.github.switcheroo-preview+json'))
  } else {
    message("No legacy pkgdown for:", dirname(testurl))
  }
}

#' @export
sync_ropensci_homepages <- function(){
  packages <- jsonlite::fromJSON("https://ropensci.github.io/roregistry/registry.json")$packages
  sites <- get_docs_repos(active_only = TRUE)
  skiplist <- readLines('https://raw.githubusercontent.com/ropenscilabs/makeregistry/master/inst/automation/exclude_list.txt')
  skiplist <- c(skiplist, 'git2rdata')
  excluded <- sites %in% skiplist
  message("EXCLUDED: ", sites[excluded])
  # visdat uses custom pkgdown domain
  sites <- sites[!excluded]
  for(pkg in sites){
    url <-  subset(packages, name == pkg)$url[1]
    if(!length(url) || !grepl('^https://github.com/(ropensci|ropenscilabs)', url)){
      message("No Github URL found for: ", pkg)
      next
    }
    repo <- sub('https://github.com/', '', url)
    homepage <- paste0('https://docs.ropensci.org/', pkg)
    set_repo_homepage(repo, homepage)
    message(sprintf("Homepage for package '%s' updated to: %s", pkg, homepage))
    disable_legacy_pkgdown(repo)
  }
}

# Not sure how well jenkins deals with strange characters...
asciify <- function(x){
  gsub("[^a-zA-Z0-9' .-]", "", stringi::stri_trans_general(x, "latin-ascii"))
}

parse_res <- function(res){
  text <- rawToChar(res$content)
  if(res$status >= 400)
    stop(text)
  jsonlite::fromJSON(text)
}

caterr <- function(...){
  base::cat(..., file = stderr())
}
