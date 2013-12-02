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
load("train-data.rda")
load("test-data.rda")
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
	catMethod='pretty')
dev.off()


####
# map accuracy in training set 
ls()
names(train)
countries = sort(unique(c(unique(train$country_1), unique(train$country_2))))
mths = seq(min(train$date), max(train$date), by='month')

combos = as.data.frame(countries)
head(combos)
colnames(combos) = c("country")

combos$tp = 0
combos$tn = 0
combos$fp = 0 
combos$fn = 0 
head(combos)

yhat = predict(tree_mine, type='class')
yobs = train$hostile4

for(i in 1:nrow(train)){
	if(i%%1000==0){print(i)}
	a = train$country_1[i]
	b = train$country_2[i]
	est = yhat[i]
	obs = yobs[i]
	var = ""
	if(obs==0 & est==0){
		var = "tn"
	} else if(obs==0 & est==1){
		var = "fp"
	} else if(obs==1 & est==0){
		var = "fn"
	} else {
		var = "tp"
	}

	combos[which(combos$country==a), var] = combos[which(combos$country==a), var] + 1
	combos[which(combos$country==b), var] = combos[which(combos$country==b), var] + 1	
}

combos$total = combos$tp + combos$tn + combos$fp + combos$fn 
combos$tp.perc = combos$tp / combos$total
combos$tn.perc = combos$tn / combos$total
combos$fp.perc = combos$fp / combos$total
combos$fn.perc = combos$fn / combos$total

head(combos)
tail(combos)

pathGraphics = "~/github/cs571/graphics"
setwd(pathGraphics)

summary(combos$fn.perc)
combos[order(combos$fn.perc),]
names(combos)
combos[which(combos$country=="SRB"), 'fn.perc'] = 0.049999

data(countryExData)
sPDF = joinCountryData2Map( combos, joinCode="ISO3", nameJoinColumn="country")

tikz('train-false-positives.tex', standAlone=FALSE, width=4.5, height=3)
mapDevice(device="tikz output")
# mapDevice()
mapCountryData(sPDF, nameColumnToPlot="fp.perc", mapTitle="", catMethod=seq(0, 0.005, length.out=10),
	addLegend=FALSE)
dev.off()

tikz('train-false-negatives.tex', standAlone=FALSE, width=4.5, height=3)
mapDevice(device="tikz output")
# mapDevice()
mapCountryData(sPDF, nameColumnToPlot="fn.perc", mapTitle="", catMethod=seq(0, 0.12, length.out=10),
	addLegend=FALSE)
dev.off()


####
# map accuracy in test set 

countries = sort(unique(c(unique(test$country_1), unique(test$country_2))))
mths = seq(min(test$date), max(test$date), by='month')

combos2 = as.data.frame(countries)
head(combos2)
colnames(combos2) = c("country")

combos2$tp = 0
combos2$tn = 0
combos2$fp = 0 
combos2$fn = 0 
head(combos2)

yhat_test = as.numeric(predict(tree_mine_pruned, type='class', newdata=test)) - 1
yobs_test = test$hostile4

for(i in 1:nrow(test)){
	if(i%%1000==0){print(i)}
	a = test$country_1[i]
	b = test$country_2[i]
	est = yhat_test[i]
	obs = yobs_test[i]
	var = ""
	if(obs==0 & est==0){
		var = "tn"
	} else if(obs==0 & est==1){
		var = "fp"
	} else if(obs==1 & est==0){
		var = "fn"
	} else {
		var = "tp"
	}

	combos2[which(combos2$country==a), var] = combos2[which(combos2$country==a), var] + 1
	combos2[which(combos2$country==b), var] = combos2[which(combos2$country==b), var] + 1	
}

combos2$total = combos2$tp + combos2$tn + combos2$fp + combos2$fn 
combos2$tp.perc = combos2$tp / combos2$total
combos2$tn.perc = combos2$tn / combos2$total
combos2$fp.perc = combos2$fp / combos2$total
combos2$fn.perc = combos2$fn / combos2$total

head(combos2)
tail(combos2)

# pathGraphics = "~/github/cs571/graphics"
# setwd(pathGraphics)

combos2[order(combos2$fp.perc),]
combos2[which(combos2$country=="PSE"), 'fp.perc'] = 0.0049999

data(countryExData)
sPDF2 = joinCountryData2Map( combos2, joinCode="ISO3", nameJoinColumn="country")

tikz('test-false-positives.tex', standAlone=FALSE, width=4.5, height=3)
mapDevice(device="tikz output")
# mapDevice()
mapCountryData(sPDF2, nameColumnToPlot="fp.perc", mapTitle="", catMethod=seq(0, 0.005, length.out=10) )
dev.off()


tikz('test-false-negatives.tex', standAlone=FALSE, width=4.5, height=3)
# mapDevice(device="tikz output")
mapDevice()
par(mfrow=c(2,1))
mapCountryData(sPDF2, nameColumnToPlot="fn.perc", mapTitle="", catMethod=seq(0, 0.12, length.out=10))
# dev.off()


tikz('false-positives.tex', standAlone=FALSE, width=4.5, height=6)
mapDevice(device="tikz output")
par(mfrow=c(2,1))
par(oma=c(-1,-1,-1,-1))
# mapDevice()
mapCountryData(sPDF, nameColumnToPlot="fp.perc", mapTitle="False Positives", catMethod=seq(0, 0.005, length.out=10),
	addLegend=FALSE)
mapCountryData(sPDF2, nameColumnToPlot="fp.perc", mapTitle="", catMethod=seq(0, 0.005, length.out=10) )
dev.off()

setwd(pathGraphics)
tikz('diagnostics.tex', standAlone=TRUE, width=4.5, height=8.5)
mapDevice(device="tikz output")
# mapDevice()
par(mfrow=c(4,1))
par(mar=c(0,0,2,2))
mapCountryData(sPDF, nameColumnToPlot="fp.perc", mapTitle="False Positives in Training Data", catMethod=seq(0, 0.005, length.out=10),
	addLegend=TRUE)
mapCountryData(sPDF2, nameColumnToPlot="fp.perc", mapTitle="False Positives in Test Data", catMethod=seq(0, 0.005, length.out=10) ,
	addLegend=FALSE)
mapCountryData(sPDF2, nameColumnToPlot="fn.perc", mapTitle="False Negatives in Training Data", catMethod=seq(0, 0.12, length.out=10),
	addLegend=TRUE)
mapCountryData(sPDF2, nameColumnToPlot="fn.perc", mapTitle="False Negatives in Test Data", catMethod=seq(0, 0.12, length.out=10),
	addLegend=FALSE)
dev.off()


