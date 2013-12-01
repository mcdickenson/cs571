#!/usr/bin/Rscript

install.packages('rworldmap')


# set up workspace
rm(list=ls())
setwd('~/github/gdelt-to-mids')
source('start.R')
library(DMwR)
library(countrycode)
library(doBy)
library(rworldmap)
data(countryExData)
library(wicews)
data(icews)
data(icewsmap)

library(tikzDevice)

# tree libraries
library(rpart)				# Popular decision tree algorithm
library(rattle)				# Fancy tree plot
library(rpart.plot)		# Enhanced tree plots
library(RColorBrewer) # Color selection for fancy tree plot
library(party) 				# Alternative decision tree algorithm
library(partykit) 		# Convert rpart object to BinaryTree

# load data
setwd(pathData)

load('output6.rda')
# load("train-data.rda")
# load("test-data.rda")
dim(train)
dim(test)

load("tree_mine.rda")
load("tree_mine_pruned.rda")



###
# make graphics 


# map countries based on proportion of 1992-2001 they spent in a MID
setwd(pathData)
getwd()
load("monthlymids.rda")
dim(monthlymids)
head(monthlymids)

countries = sort(unique(c(unique(monthlymids$CCodeA), unique(monthlymids$CCodeB))))
mths = seq(min(monthlymids$start), max(monthlymids$end), by='month')

combos = expand.grid(first=countries, second=mths)
colnames(combos) = c("ccode", "date")
combos$midmonths = 0

for(i in 1:nrow(monthlymids)){
	a = monthlymids$CCodeA[i]
	b = monthlymids$CCodeB[i]
	d = monthlymids$date[i]
	combos[which(combos$ccode==a & combos$date==d), 'midmonths'] =  1
	combos[which(combos$ccode==b & combos$date==d), 'midmonths'] =  1	
}
head(combos)
tail(combos)
summary(combos$midmonths)
combos$mid = combos$midmonths

mid = summaryBy(mid ~ ccode, data=combos, FUN=mean)
head(mid)
mid$iso3 = countrycode(mid$ccode, "cown", "iso3c")

# subset = icews[icews$year==2010 & icews$month==1, c("ccode", "year", "month")]
# subset$mid.mean = 0 
# for(i in 1:nrow(subset)){
# 	code = subset[i, "ccode"]
# 	midmean = mean.mid[which(mid$ccode==code)[1], "mid.mean"]
# 	subset$mid.mean[i] = ifelse(is.na(midmean), 0, midmean)
# }

# head(subset)
# subset$iso3 = countrycode(subset$ccode, "cown", "iso3c")

# mult = 1
# colorscheme = rgb(red=1, blue=1-(subset$mid.mean*mult), green=1-(subset$mid.mean*mult) )
# props = c(0.10*mult, 0.50*mult, 0.90*mult)
# legendcolors = rgb(red=1, blue=1-props, green=1-props )

pathGraphics = "~/github/cs571/graphics"
setwd(pathGraphics)

# tikz('midmap.tex', standAlone=TRUE, width=4.5, height=5)
# plot(icewsmap, col=colorscheme)
# legend("right", legend=c("10\\%", "50\\%", "90\\%"), cex=0.5, col=legendcolors, pch=15)
# dev.off()


data(countryExData)
sPDF = joinCountryData2Map( mid, joinCode="ISO3", nameJoinColumn="iso3")

tikz('midmap.tex', standAlone=FALSE, width=4.5, height=3)
mapDevice(device="tikz output")
# mapDevice()
mapCountryData(sPDF, nameColumnToPlot="mid.mean", mapTitle="",
	numCats=10)
dev.off()

?mapCountryData