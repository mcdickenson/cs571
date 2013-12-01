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

green = rgb(27, 158, 119, maxColorValue=255)
red = rgb(217, 95, 2, maxColorValue=255)
blue = rgb(117, 112, 179, maxColorValue=255)
purple = rgb(231, 41, 138, maxColorValue=255)

plot.event.counts = function(data, country1, country2, xl="", leg=FALSE, start=c(), stop=c()) {
  sub = data[which(data$country_1==country1 & data$country_2==country2), ]
 	sub$X.Date = as.Date(paste(as.character(sub$year), "-", as.character(sub$month), "-01", sep=''))
 	want = which(sub$X.Date > as.Date("1992-01-01") & sub$X.Date < as.Date("2002-01-01"))

 	title = paste(country1, country2, sep="-")

 	mx = max(c(sub$quad1[want], sub$quad2[want], sub$quad3[want], sub$quad4[want]))
 	tmp = ceiling(mx/200)*200
 	mx = tmp
 	plot(sub$X.Date[want], sub$quad1[want], ylim=c(0, mx),
 		type='l', col=blue, lwd=3,
 		xlab=xl, ylab="",
 		main=title, axes=FALSE)
 	axis(1, at=c(as.Date("1992-01-01"), as.Date("1995-01-01"), as.Date("1998-01-01"), as.Date("2001-01-01")),
 		labels=c("1992", "1995", "1998", "2001"))
 	axis(2, at=seq(0, mx, by=200), labels=as.character(seq(0, mx, by=200)) )
 	if(length(start)>0){
	 	for(i in 1:length(start)){
	 		x = c(start[i], start[i], stop[i], stop[i])
	 		y = c(-20, mx+100, mx+100, -20)
	 		polygon(x, y, col="gray", border=NA)
	 	}
 	}
 	lines(sub$X.Date[want], sub$quad2[want], col=green, lwd=3)
 	lines(sub$X.Date[want], sub$quad3[want], col=purple, lwd=3)
 	lines(sub$X.Date[want], sub$quad4[want], col=red, lwd=3)
 	if(leg){
 		legend("topleft", col=c(blue, green, purple, red),
 			pch=16, cex=0.5,
 			legend=c("Verbal Cooperation", "Material Cooperation",
 				"Verbal Conflict", "Material Conflict"))
 	}

}

?legend


pathGraphics = "~/github/cs571/graphics"
setwd(pathGraphics)

tikz("timelines.tex", standAlone=FALSE, width=4.5, height=5)
par(mfrow=c(2,2))
plot.event.counts(agg, "ISR", "USA", leg=TRUE)

plot.event.counts(agg, "DEU", "FRA")

strt = c(as.Date("1993-09-09"), as.Date("2001-07-01"))
stps = c(as.Date("1999-07-31"), as.Date("2001-12-31"))
plot.event.counts(agg, "IND", "PAK", start=strt, stop=stps, xl="Date")

strt = c(as.Date("1995-12-01"), as.Date("1999-07-01"), as.Date("2000-10-01"))
stps = c(as.Date("1996-07-31"), as.Date("1999-10-31"), as.Date("2001-04-30"))
plot.event.counts(agg, "GRC", "TUR", start=strt, stop=stps, xl="Date")
dev.off()