# Move rOpenSci specific stuff into this file
commit_for_ropensci <- function(message){
  # Set author
  author_email <- Sys.getenv("GIT_EMAIL", NA)
  author_sig <- if(is.na(author_email)){
    gert::git_signature_default()
  } else {
    author_name <- Sys.getenv('GIT_USER', 'rOpenSci user')
    gert::git_signature(name = name, email = author_email)
  }

  # Set the committer
  commit_sig <- gert::git_signature(name = 'rOpenSci', email = 'myrmecocystus+ropenscibot@gmail.com')

  # Add on trailer to message
  commit_msg <- paste0(message, "\n\n\non-behalf-of: @ropensci <support@ropensci.org>")
  gert::git_commit(message = commit_msg, author = author_sig, committer = commit_sig)
}
