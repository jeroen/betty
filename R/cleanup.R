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
  jk <- jenkins::jenkins('http://jenkins.ropensci.org')
  jobs <- jk$project_list()
  url <- "https://ropensci.github.io/roregistry/registry.json"
  packages <- jsonlite::fromJSON(url)$packages
  for(i in seq_len(nrow(packages))){
    name <- packages[i, "name"]
    xml <- config_template(packages[i, "url"])
    if(name %in% jobs$name){
      if(isTRUE(update_jobs)){
        cat(sprintf("Updating job config for %s...", name))
        jk$project_update(name = name, xml_string = xml)
      } else {
        cat(sprintf("Job config for %s already exists...", name))
      }
    } else {
      cat(sprintf("Creating new job for %s...", name))
      jk$project_create(name = name, xml_string = xml)
    }
    cat("OK!\n")
  }
  if(isTRUE(remove_jobs)){
    gone <- !(jobs$name %in% packages$name)
    lapply(jobs$name[gone], function(name){
      cat(sprintf("Deleting job %s which is no longer in the roregistry...", name))
      jk$project_delete(name)
      cat("OK!\n")
    })
  }
  if(isTRUE(update_views)){
    views <- jk$view_list()
    packages$maintainer <- asciify(packages$maintainer)
    authors <- unique(packages$maintainer)
    lapply(authors, function(author){
      pkg_names = packages[packages$maintainer == author, "name"]
      if(!length(pkg_names))
        stop(sprintf("Failed to find packages for author %s", author))
      xml <- view_template(pkg_names)
      if(author %in% views$name){
        cat(sprintf("Updating view for %s...", author))
        jk$view_update(name = author, xml_string = xml)
      } else {
        cat(sprintf("Creating new view for %s...", author))
        jk$view_create(name = author, xml_string = xml)
      }
      cat("OK!\n")
    })
    views_gone <- !(views$name %in% c(authors, 'all'))
    lapply(views$name[views_gone], function(author){
      cat(sprintf("Deleting view %s which is no longer a maintainer...", author))
      jk$view_delete(author)
      cat("OK!\n")
    })
  }
  invisible(jk$server_info())
}

#' @export
#' @rdname sync_ropensci
sync_ropensci_docs <- function(){
  registry <- jsonlite::fromJSON("https://ropensci.github.io/roregistry/registry.json")
  packages <- c(registry$packages$name, 'ropensci-docs.github.io')
  repos <- list_all_docs()
  added <- packages[!(packages %in% repos)]
  if(length(added)){
    cat("Adding new packages: ", paste(added, collapse = ', '), "\n")
    if(utils::askYesNo("are you sure you want to add these packages?")){
      lapply(added, function(name){
        message("Creating: ropensci-docs/", name)
        description <- paste0('auto-generated pkgdown website for: ', name)
        homepage <- paste0("https://docs.ropensci.org/", name)
        gh::gh('/orgs/ropensci-docs/repos', .method = 'POST',
               name = name, description = description, homepage = homepage,
               has_issues = FALSE, has_wiki = FALSE)
      })
    }
  }
  deleted <- repos[!(repos %in% packages)]
  if(length(deleted)){
    cat("Removed packages: ", paste(deleted, collapse = ', '), "\n")
    if(utils::askYesNo("are you sure you want to delete these?")){
      lapply(deleted, function(name){
        message("Deleting: ropensci-docs/", name)
        gh::gh(paste0('/repos/ropensci-docs/', name), .method = 'DELETE')
      })
    }
  }
  message("Everything in sync!")
  invisible()
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

list_all_docs <- function(){
  page <- 1
  repos <- NULL
  repeat {
    out <- gh::gh(paste0('/orgs/ropensci-docs/repos?per_page=100&page=', page))
    names <- unlist(lapply(out, `[[`, 'name'))
    repos <- c(repos, names)
    if(length(names) < 100)
      return(repos)
    page <- page + 1
  }
}

# Not sure how well jenkins deals with strange characters...
asciify <- function(x){
  gsub("[^a-zA-Z0-9' .-]", "", stringi::stri_trans_general(x, "latin-ascii"))
}
