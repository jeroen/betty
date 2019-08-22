#' Modify readme
#'
#' Hack some stuff into the readme file
#'
modify_readme <- function(file, pkg){
  readme <- readLines(file)
  h1_line <- find_h1_line(readme)
  if(is.na(h1_line)){
    readme <- c(ropensci_banner(pkg), readme)
  } else {
    readme[h1_line] <- ropensci_banner(pkg)
  }
  writeLines(readme, file)
}

ropensci_banner <- function(pkg){
  sprintf('# rOpenSci: %s <img src="hexlogo.png" align="right" height="134.5" />', pkg)
}

find_h1_line <- function(txt){
  doc <- xml2::read_xml(commonmark::markdown_xml(txt, sourcepos = TRUE))
  doc <- xml2::xml_ns_strip(doc)
  node <- xml2::xml_find_first(doc, "//heading[@level='1'][text]")
  sourcepos <- xml2::xml_attr(node, 'sourcepos')
  as.integer(strsplit(sourcepos, ':')[[1]][1])
}
