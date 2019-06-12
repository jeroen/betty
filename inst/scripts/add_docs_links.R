# ghrecipes lives at github.com/ropenscilabs/ghrecipes
docs <- gh::gh('/users/ropensci-docs/repos?per_page=100', .limit = 1000)
repos <- ghrecipes::get_repos("ropensci")
repos2 <- ghrecipes::get_repos("ropenscilabs")
repos <- dplyr::bind_rows(repos[, "name"], repos2[, "name"])
docs <- tibble::tibble(name = purrr::map_chr(docs, "name"))
repos <- tidyr::separate(repos, name, "/", into = c("org", "name"))
docs <- dplyr::left_join(docs, repos, by = "name")
docs <- docs[!is.na(docs$org),]


get_and_update_url <- function(repo, owner){
  message(repo)
  
  url <- gh::gh("GET /repos/:owner/:repo",
                   repo = repo, owner = owner)$homepage

  if(is.null(url)){
    newurl <- paste0("https://docs.ropensci.org/", repo)
    do <- gh::gh("PATCH /repos/:owner/:repo",
           repo = repo, owner = owner, homepage = newurl)
    return(TRUE)
  }
  
  if(url == ""){
    newurl <- paste0("https://docs.ropensci.org/", repo)
    do <- gh::gh("PATCH /repos/:owner/:repo",
                 repo = repo, owner = owner, homepage = newurl)
    return(TRUE)
  }
  
  return(FALSE)
  
}

pof <- purrr::map2_lgl(docs$name, docs$org,
            get_and_update_url)

sum(pof)

