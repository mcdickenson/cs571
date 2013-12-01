#!/usr/bin/Rscript

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

pathGraphics = "~/github/cs571/graphics"
setwd(pathGraphics)


data(countryExData)
sPDF = joinCountryData2Map( mid, joinCode="ISO3", nameJoinColumn="iso3")

tikz('midmap.tex', standAlone=FALSE, width=4.5, height=3)
mapDevice(device="tikz output")
# mapDevice()
mapCountryData(sPDF, nameColumnToPlot="mid.mean", mapTitle="",
	numCats=10)
dev.off()

