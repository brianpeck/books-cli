source("uuid.R")
source("csvinterface.R")
source("pages.R")
source("plotting.R")
source("tags.R")

authors <- setup.csv("authors")
tags <- setup.csv("tags")
booksauthors <- setup.csv("booksauthors")
books <- setup.csv("books")
log <- setup.csv("log")
readings <- setup.csv("readings")

add.progress <- function() {
  book.num <- menu(strtrim(list.book.titles(),32),title="Select Book:")
  book.uuid <- list.book.uuid()[book.num]
  day.num <- menu(Sys.Date()-0:10,title="Select Date Read:") # How to display vertically!?
  day <- Sys.Date()-(day.num-1)
  cat("Enter type (percent, pages, location)")
  type <- user.response()
  cat("Enter Start Position")
  startpos <- user.response()
  cat("Enter Stop Position")
  stoppos <- user.response()
  add.log(book.uuid,day,startpos,stoppos,type)
}



add.new.book <- function() {
  cat("Enter Book Name:\n")
  book.title <- user.response()
  cat("Enter Number of Pages:\n")
  pages <- user.response()
  book.uuid <- add.book(book.title,pages)
  
  ## Add authors
  cat("Enter Authors (Enter to end)\n")
  repeat{
    cat("Enter Author Name:\n")
    author.name <- user.response()
    if(nchar(author.name)==0) {
      break
    }
    author.uuid <- insert.author(author.name)
    add.booksauthors(book.uuid,author.uuid)
  }
  
  ## Add Tags
  cat("Enter Tags (Enter to end)\n")
  repeat{
    cat("Enter Tag Name:\n")
    tag.name <- user.response()
    if(nchar(tag.name)==0) {
      break
    }
    add.booktag(book.uuid,tag.name)
  }
  
}

user.response <- function() {
  #readLines(file("stdin"),n=1)
  readline()
}

list.book.titles <- function() {
  b <- get.bookfile(books,TRUE)
  as.character(b$BookTitle)
}

list.book.uuid <- function() {
  b <- get.bookfile(books,TRUE)
  as.character(b$BookID)
}

list.books <- function() {
    b <- get.bookfile(books,TRUE)
    apply(b,1,print.book)
    return(0)
}

print.book <- function(book) {
  cat("Book Title:",book[2],"\n")
  cat("\tPages:\t",book[3],"\n")
  cat("\tAuthors:\t", get.book.authors(book[1]),"\n")
  cat("\tTags:\t", get.book.tags(book[1]),"\n")
}

get.book.tags <- function(book.uuid) {
  t <- get.bookfile(tags,TRUE)
  as.character(t[t$BookID == book.uuid,"Tag"])
}

get.book.authors <- function(book.uuid) {
  ba <- get.bookfile(booksauthors,TRUE)
  a <- get.bookfile(authors,TRUE)
  author.uuids <- as.character(ba[ba$BookID == book.uuid,"AuthorID"])
  as.character(a[a$AuthorID %in% author.uuids,"AuthorName"])
}




