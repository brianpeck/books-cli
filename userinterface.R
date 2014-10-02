user.response <- function(prompt=NULL) {
  if(!is.null(prompt)) {
    cat(prompt,"\n")
  }
  #readLines(file("stdin"),n=1)
  if(interactive()) {
    readline()
  } else {
    readLines(file("stdin"),n=1)
  }

}

#' user.menu
#' A menu that works in either mode.
#' TODO
user.menu <- function(choices,title=NULL,returnval=FALSE) {
  r <- menu(choices=choices, graphics=FALSE,title=title)
  if(returnval) {
    return(choices[r])
  }
  r
}
