source("uuid.R")
source("csvinterface.R")
source("pages.R")
source("plotting.R")
source("tags.R")
source("curreading.R")
source("userinterface.R")

authors <- setup.csv("authors")
tags <- setup.csv("tags")
booksauthors <- setup.csv("booksauthors")
books <- setup.csv("books")
log <- setup.csv("log")
curreading <- setup.csv("curreading")

add.progress <- function() {
  book.num <- user.menu(strtrim(list.curreading(),32),title="Select Book:")
  # Fill in info from currently reading file
  cr <- get.bookfile(curreading,TRUE)
  book <- cr[book.num,]
  book.uuid <- as.character(book$BookID)
  type <- as.character(book$Type)
  startpos <- as.numeric(book$Progress)
  
  day <- as.character(user.menu(Sys.Date()-0:4,title="Select Date Read:",returnval=1))
  stoppos <- as.numeric(user.response("Enter Stop Position"))
  
  pages <- get.pages.read(type,startpos,stoppos,book.uuid)
  add.log(book.uuid,day,startpos,stoppos,type,pages)
  
  # Update currently reading
  cr[book.num,"Progress"] <- stoppos
  write.bookfile("curreading",cr)
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




