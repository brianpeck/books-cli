make.log.histogram <- function() {
  lall <- get.bookfile(log)
  lall$PagesRead <- apply(lall,1,get.pages.read)
  lall$Category <- sapply(lall$BookID,get.primary.tag)
  thedates <- as.Date(lall$Date)
  p <- ggplot(lall)
  p <- p+geom_histogram(aes(x=as.Date(Date),y=PagesRead,fill=Category),stat="identity",binwidth=1)
  #p <- p+scale_x_date(breaks=date_breaks("day"))
  p+theme_wsj()+scale_fill_wsj(palette="rgby")
}