#' Modify readme
#'
#' Hack some stuff into the readme file
#'
modify_readme <- function(file, pkg, git_url = ""){
  readme <- readLines(file)
  h1_line <- find_h1_line(readme)
  banner <- if(grepl('ropenscilabs', git_url)){
    ropensci_labs_banner(pkg)
  } else {
    ropensci_main_banner(pkg)
  }
  if(is.na(h1_line)){
    readme <- c(banner, readme)
  } else {
    readme[h1_line] <- banner
  }
  writeLines(readme, file)
}

ropensci_main_banner <- function(pkg){
  sprintf('# rOpenSci: %s <img src="hexlogo.png" align="right" height="134.5" />', pkg)
}

ropensci_labs_banner <- function(pkg){
  sprintf('# rOpenSci Labs: %s <img src="labs.png" align="right" height="134.5" />', pkg)
}

find_h1_line <- function(txt){
  doc <- xml2::read_xml(commonmark::markdown_xml(txt, sourcepos = TRUE))
  doc <- xml2::xml_ns_strip(doc)
  node <- xml2::xml_find_first(doc, "//heading[@level='1'][text]")
  sourcepos <- xml2::xml_attr(node, 'sourcepos')
  as.integer(strsplit(sourcepos, ':')[[1]][1])
}
