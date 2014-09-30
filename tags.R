get.primary.tag <- function(book.uuid) {
  t <- get.bookfile(tags)
  t[t$BookID==book.uuid,"Tag"][1]
}