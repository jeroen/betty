# Move rOpenSci specific stuff into this file
commit_for_ropensci <- function(message, author, time = NULL){
  # Set author
  author_name <- sub('^(.*)<(.*)>$', '\\1', author)
  author_email <- sub('^(.*)<(.*)>$', '\\2', author)
  author_sig <- gert::git_signature(name = author_name, email = author_email, time = time)
  commit_sig <- gert::git_signature(name = 'rOpenSci', email = 'info@ropensci.org', time = time)

  # Add on trailer to message (only useful if commit is signed)
  # message <- paste0(message, "\n\n\non-behalf-of: @ropensci <info@ropensci.org>")
  gert::git_commit(message = message, author = author_sig, committer = commit_sig)
}

modify_ropensci_readme <- function(file, pkg, git_url = ""){
  readme <- readLines(file)
  h1 <- find_h1_line(readme)
  is_labs <- isTRUE(grepl('ropenscilabs', git_url))
  has_logo <- isTRUE(grepl("(<img|!\\[)", h1$input))
  if(is_labs || has_logo){
    cat("Replacing H1 line\n")
    banner <- if(is_labs){
      ropensci_labs_banner(pkg)
    } else {
      ropensci_main_banner(pkg)
    }
    if(is.na(h1$pos)){
      readme <- c(banner, readme)
    } else {
      readme[h1$pos] <- banner
      if(isTRUE(grepl("^[= ]+$", readme[h1$pos + 1])))
        readme[h1$pos + 1] = ""
    }
  }
  ugly_footer <- find_old_footer_banner(readme)
  readme[ugly_footer] = ""
  writeLines(readme, file)
}

ropensci_main_banner <- function(title){
  sprintf('# rOpenSci: The *%s* package <img src="hexlogo.png" align="right" height="134.5" />', title)
}

ropensci_labs_banner <- function(title){
  sprintf('# rOpenSci Labs: *%s* <small>(unofficial community project)</small> <img src="https://github.com/ropenscilabs.png" align="right" height="134.5" />', title)
}

find_h1_line <- function(txt){
  doc <- xml2::read_xml(commonmark::markdown_xml(txt, sourcepos = TRUE))
  doc <- xml2::xml_ns_strip(doc)
  node <- xml2::xml_find_first(doc, "//heading[@level='1'][text]")
  sourcepos <- xml2::xml_attr(node, 'sourcepos')
  pos <- as.integer(strsplit(sourcepos, ':')[[1]][1])
  title <- xml2::xml_text(xml2::xml_find_all(node, 'text'))
  input <- xml2::xml_text(node)
  list(pos = pos, title = title, input = input)
}

find_old_footer_banner <- function(txt){
  which(grepl('\\[.*\\]\\(.*/(ropensci|github)_footer.png\\)', txt))
}
