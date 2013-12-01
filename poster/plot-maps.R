#!/usr/bin/Rscript

# set up workspace
rm(list=ls())
setwd('~/github/gdelt-to-mids')
source('start.R')
library(DMwR)
library(countrycode)
library(wicews)
library(doBy)
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

pathGraphics = "~/github/cs571/graphics"
setwd(pathGraphics)

# map countries based on proportion of 1992-2001 they spent in a MID
setwd(pathData)
getwd()
load("monthlymids.rda")
dim(monthlymids)
head(monthlymids)

countries = sort(unique(c(unique(monthlymids$CCodeA), unique(monthlymids$CCodeB))))
countries

# mean.mid = matrix(NA, nrow=length(countries), ncol=2)
# mean.mid = as.data.frame(mean.mid)
# colnames(mean.mid) = c("ccode", "midmonths")
# mean.mid$ccode = countries
# mean.mid$midmonths = 0 
# head(mean.mid)
# tail(mean.mid)

head(monthlymids)

mths = seq(min(monthlymids$start), max(monthlymids$end), by='month')
mths

combos = expand.grid(first=countries, second=mths)
head(combos)
tail(combos)
colnames(combos) = c("ccode", "date")
combos$midmonths = 0
head(combos)

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
mid

# mean.mid[which(mean.mid$midmonths==max(mean.mid$midmonths)), ]
# sort(unique(mean.mid$midmonths))

# summary(newdata$hostile4)
# summary(newdata$reverse)
# summary(newdata$mid)
# mean.mid = summaryBy(mid ~ country_1, data=newdata, FUN=mean)

# head(mean.mid)
# tail(mean.mid)
# summary(mean.mid$mid.mean)
# mean.mid$ccode = countrycode(mean.mid$country_1, "iso3c", "cown")
# length(which(is.na(mean.mid$ccode)))

# mean.mid[which(is.na(mean.mid$ccode)), ]

# mean.mid[which(mean.mid$country_1=="PRK"), "ccode"] = 731
# mean.mid[which(mean.mid$country_1=="SRB"), "ccode"] = 345

# mid = mean.mid[which(!(is.na(mean.mid$ccode))), ]
# mid

# summary(mid$mid.mean)
# summary(mid$mid.mean)

# sort(mid$mid.mean)

subset = icews[icews$year==2010 & icews$month==1, c("ccode", "year", "month")]
subset$mid.mean = 0 
for(i in 1:nrow(subset)){
	code = subset[i, "ccode"]
	midmean = mean.mid[which(mid$ccode==code)[1], "mid.mean"]
	subset$mid.mean[i] = ifelse(is.na(midmean), 0, midmean)
}


mult = 1

# subset$mid.mean = ifelse(subset$mid.mean>1/mult, 1/mult, subset$mid.mean)

colorscheme = rgb(red=1, blue=1-(subset$mid.mean*mult), green=1-(subset$mid.mean*mult) )
props = c(0.10*mult, 0.50*mult, 0.90*mult)
legendcolors = rgb(red=1, blue=1-props, green=1-props )

plot(icewsmap, col=colorscheme)
legend("right", legend=c("10\%", "50\%", "90%"), col=legendcolors, pch=15)

# map countries based on proportion of 2001-2013 they spent in insurgency
# mean.ins = summaryBy(insurgency ~ country, data=ins, FUN=mean)
# mean.ins$insurgency.mean = ifelse(is.na(mean.ins$insurgency.mean), 0, mean.ins$insurgency.mean)
# mean.ins$ccode = countrycode(mean.ins$country, "country.name", "cown")

# subset = icews[icews$year==2010 & icews$month==1, c("ccode", "year", "month")]
# head(subset)
# subset$insurgency.mean = 0
# for(i in 1:nrow(subset)){
# 	code = subset[i, "ccode"]
# 	ins.mean = mean.ins[which(mean.ins$ccode==code)[1], "insurgency.mean"]
# 	subset$insurgency.mean[i] = ifelse(is.na(ins.mean), 0, ins.mean)
# }

colorscheme<-rgb(red=1, blue=1-subset$insurgency.mean, green=1-subset$insurgency.mean)
props = c(0.1, 0.5, 0.9)
legendcolors = rgb(red=1, blue=1-props, green=1-props)

png("insurgency-prop-map.png", width=1280, height=960)
plot(icewsmap, col=colorscheme)
legend("right", legend=c("10%", "50%", "90%"), col=rgb(red=1, blue=1-props, green=1-props), pch=15)
dev.off()