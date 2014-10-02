make.log.histogram <- function() {
  require(ggplot2)
  require(ggthemes)
  lall <- get.bookfile(log,TRUE)
  lall$Category <- sapply(lall$BookID,get.primary.tag)
  lall <- lall[with(lall,order(Date,Category)),]
  thedates <- as.Date(lall$Date)
  p <- ggplot(lall)
  p <- p+geom_histogram(aes(x=as.Date(Date),y=PagesRead,fill=Category),stat="identity",binwidth=1)
  #p <- p+scale_x_date(breaks=date_breaks("day"))
  p+theme_wsj()+scale_fill_wsj(palette="rgby")
}

make.log.timeplot <- function(book.uuid) {
  b <- get.bookfile(books,TRUE)
  l <- get.bookfile(log,TRUE)
  lthis <- l[l$BookID == bid,]
  end <- get.book.end(book.uuid,lthis[1,"Type"])
  p <- ggplot(lthis)
  p <- p+geom_point(aes(x=as.Date(Date),y=Stop))
  p+theme_bw()+ylim(0,end)
}