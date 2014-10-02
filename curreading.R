add.curreading <- function() {
  book.num <- user.menu(strtrim(list.book.titles(),32),title="Select Book:")
  book.uuid <- list.book.uuid()[book.num]
  type <- user.menu(c("percent","pages","location"),title="Enter type:",returnval=1)
  progress <- user.response("Enter Current Progress")
  total <- get.book.end(book.uuid,type)
  add.reading(book.uuid,type,progress,total)
}

remove.curreading <- function() {
  book.num <- user.menu(strtrim(list.curreading(),32),title="Select Book:")
  book.uuid <- list.book.uuid()[book.num]
  cr <- get.bookfile(curreading)
  toremove <- which(cr$BookID == book.uuid)
  cr <- cr[-toremove,]
  write.bookfile("curreading",cr)
}

list.curreading <- function() {
  b <- get.bookfile(books,TRUE)
  cr <- get.bookfile(curreading,TRUE)
  as.character(b[b$BookID %in% cr$BookID,"BookTitle"])
}




get.book.end <- function(book.uuid,type) {
  if(type=="percent") {
    return(100)
  }
  b <- get.bookfile(books)
  book <- b[b$BookID==book.uuid,]
  if(type=="pages") {
    return(book$Pages)
  } else {
    return(book$Locations)
  }
}
