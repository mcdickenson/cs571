setwd("~/Dropbox/gdelt-aggregations")
# install.packages('filehash')
# install.packages( 'tikzDevice', repos = 'http://rforge.net', type = 'source' )
library(tikzDevice)

# agg = read.csv("1979-2012-agg.csv", as.is=TRUE)
agg = read.csv("1979-2001-agg-corrected.csv", as.is=TRUE)
dim(agg)
names(agg)
head(agg)
tail(agg)

quad1 = c("event1", "event2", "event3", "event4", "event5")
quad2 = c("event6", "event7", "event8", "event9")
quad3 = c("event10", "event11", "event12", "event13", "event14")
quad4 = c("event15", "event16", "event17", "event18", "event19", "event20")
agg$quad1 = rowSums(agg[ , quad1]) # verbal cooperation
agg$quad2 = rowSums(agg[ , quad2]) # material cooperation
agg$quad3 = rowSums(agg[ , quad3]) # verbal conflict
agg$quad4 = rowSums(agg[ , quad4]) # material conflict

plot.event.counts = function(data, country1, country2, title="") {
  sub = data[which(data$country_1==country1 & data$country_2==country2), ]
 	sub$X.Date = as.Date(paste(as.character(sub$year), "-", as.character(sub$month), "-01", sep=''))

 	tssub = ts(sub)
 	tsdiff1 <- diff(tssub, differences=1)

 	title = ifelse(title=="", paste(country1, country2, sep="-"), title) 

	plot(sub$X.Date[2:nrow(sub)], tsdiff1[, 'quad1'], type='l') 	
}

plot.event.counts(agg, "USA", "ISR")
