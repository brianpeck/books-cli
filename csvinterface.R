book_dir <- "~/.books"

#' add.book
#' Adds book without concern for duplicates
add.book <- function(book.title,pages=0) {
  uuid <- uuid()
  append.bookfile("books",data.frame(uuid,book.title,pages))
  uuid
}

#' add.booksauthors
#' Adds booksauthors without concern for duplicates
add.booksauthors <- function(book.id,author.id) {
  append.bookfile("booksauthors",data.frame(book.id,author.id))
  uuid
}

#' add.booktag
#' Adds bookgat without concern for duplicates
add.booktag <- function(book.id,tag) {
  append.bookfile("tags",data.frame(book.id,tag))
}

add.log <- function(book.id,date,startpos,stoppos,type,pages) {
  append.bookfile("log",data.frame(date,book.id,startpos,stoppos,type,pages))
}

#' Naming things is hard.
add.reading <- function(book.id,type,progress,total) {
  append.bookfile("curreading",data.frame(book.id,type,progress,total))
}

#' add.author
#' Adds author without concern for duplicates
add.author <- function(author.name) {
  uuid <- uuid()
  append.bookfile("authors",data.frame(uuid,author.name))
  uuid
}

#' insert.author
#' Attempts to add author.  Does not add if author is already present
#' Note: This currently assumes all authors with the same name are the same person
insert.author <- function(author.name) {
  uuid <- get.author.uuid(author.name)
  if(uuid != 0) {
    return(uuid)
  }
  add.author(author.name)
  
}

#' get.author.uuid
#' Gets the uuid for an author, 0 if it doesn't exist
get.author.uuid <- function(author.name) {
  a <- get.bookfile(authors,TRUE)
  uuid <- a[a$AuthorName==author.name,"AuthorID"]
  if(length(uuid)==0) {
    return(0)
  } else {
    return(as.character(uuid))
  }
}

setup.csv <- function(name) {
  df <- NULL
  set <- function(y) {
    name <<- y
    df <<- NULL
  }
  get <- function() name
  setdf <- function(newdf) df <<- newdf
  getdf <- function() df
  list(set = set, get=get,
       setdf = setdf,
       getdf = getdf)
}

get.bookfile <- function(name,force=FALSE) {
  df <- name$getdf()
  if(!is.null(df) && !force) {
    #message("getting cached data")
    return(df)
  }
  data <- name$get()
  df <- read.bookfile(data)
  name$setdf(df)
  df
}

append.bookfile <- function(file, data) {
  write.table(
    x=data,
    file=paste0(book_dir,"/",file,".csv"),
    col.names = FALSE,
    append = TRUE,
    row.names = FALSE,
    sep = ","
  )
}

read.bookfile <- function(file) {
  read.csv(
    paste0(book_dir,"/",file,".csv"),
    fill=TRUE
  )
}

write.bookfile <- function(file,data) {
  write.table(
    x=data,
    file=paste0(book_dir,"/",file,".csv"),
    col.names = TRUE,
    append = FALSE,
    row.names = FALSE,
    sep = ","
  )
}