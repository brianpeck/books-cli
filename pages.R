get.pages.read <- function(type,startpos,stoppos,book.uuid) {
  switch(type,
         "location" = get.pages.location(startpos,stoppos,book.uuid),
         "pages" = get.pages(startpos,stoppos),
         "percent" = get.pages.percent(startpos,stoppos,book.uuid)
  )
}

get.pages.location <- function(startpos,stoppos,book.uuid) {
  b <- get.bookfile(books)
  num.pages <-  b[b$BookID == book.uuid,"Pages"]
  num.locations <-  b[b$BookID == book.uuid,"Locations"]
  startpage <- floor((startpos/num.locations)*num.pages)
  stoppage <- floor((stoppos/num.locations)*num.pages)
  stoppage-startpage
}

get.pages <- function(startpos,stoppos) {
  stoppos - startpos
}

get.pages.percent <- function(startpos,stoppos,book.uuid) {
  b <- get.bookfile(books)
  num.pages <-  b[b$BookID == book.uuid,"Pages"]
  startpage <- floor((startpos/100)*num.pages)
  stoppage <- floor((stoppos/100)*num.pages)
  stoppage-startpage
}