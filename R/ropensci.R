# Move rOpenSci specific stuff into this file
commit_for_ropensci <- function(message){
  # Set author
  author_email <- Sys.getenv("GIT_EMAIL", NA)
  author_sig <- if(is.na(author_email)){
    gert::git_signature_default()
  } else {
    author_name <- Sys.getenv('GIT_USER', 'rOpenSci user')
    gert::git_signature(name = author_name, email = author_email)
  }

  # Set the committer
  commit_sig <- gert::git_signature(name = 'rOpenSci', email = 'myrmecocystus+ropenscibot@gmail.com')

  # Add on trailer to message
  commit_msg <- paste0(message, "\n\n\non-behalf-of: @ropensci <support@ropensci.org>")
  gert::git_commit(message = commit_msg, author = author_sig, committer = commit_sig)
}

modify_readme <- function(file, pkg, git_url = ""){
  readme <- readLines(file)
  h1 <- find_h1_line(readme)
  is_labs <- isTRUE(grepl('ropenscilabs', git_url))
  if(!is_labs && isTRUE(grepl("(<img|!\\[)", h1$input))){
    cat("Found an image in H1, not replacing title line\n");
    return()
  }

  cat("Replacing H1 line\n")
  title <- pkg
  #title <- ifelse(length(h1$title) && !is.na(h1$title), h1$title, sprintf('the __%s__ package', pkg))

  banner <- if(is_labs){
    ropensci_labs_banner(title)
  } else {
    ropensci_main_banner(title)
  }
  if(is.na(h1$pos)){
    readme <- c(banner, readme)
  } else {
    readme[h1$pos] <- banner
    if(isTRUE(grepl("^[= ]+$", readme[h1$pos + 1])))
      readme[h1$pos + 1] = ""
  }
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