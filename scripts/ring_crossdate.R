#Load packages####
library(dplR)
library(ggplot2)
library(tidyverse)
library(wesanderson)
library(signal)
library(shiny)
library(treeclim)
library(ggplot2)

#Load the data####
#Rename it so I can modify it without altering the original file.
grow.rwl <- read.rwl("data/ring_data/e13c/e13c.bet.n/e13c_bet_n_01/e13c_bet_n_01_R01.rwl")        
grow.rwl <- grow.rwl[-1,] #remove the core

#Data Analysis####
##Statistics####
rwl_stats <- rwl.stats(grow.rwl) #summary and stats
print(rwl_stats)

ms <- sens2(grow.rwl) #calculates the mean sensivity
print(ms)

rwl_report <- rwl.report(grow.rwl)  #report on rwl
print(rwl_report)

##Crossdating and alignment####

#Check the alingment of the series
#graphs
seg.plot(grow.rwl) #creates a segment plot
spag.plot(grow.rwl, zfac=0.01) #creates a spaghetti plot

##Analysis####

inter <- interseries.cor(grow.rwl, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(inter)

###General correlation####

corr.rwl.seg(rwl = grow.rwl, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "E13CBetn01r01", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

###Serires vs Master####
#E13CBetn01r01r1
E13CBetn01r01r1 <- series.rwl.plot(grow.rwl, series = "E13CBetn01r01r1", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE)
  title(sub= "E13CBetn01r01r1", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r01r2
E13CBetn01r01r2 <- series.rwl.plot(grow.rwl, series = "E13CBetn01r01r2", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
  title(sub= "E13CBetn01r01r2", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r01r3
E13CBetn01r01r3 <- series.rwl.plot(grow.rwl, series = "E13CBetn01r01r3", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
  title(sub= "E13CBetn01r01r3", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r01r4
E13CBetn01r01r4 <- series.rwl.plot(grow.rwl, series = "E13CBetn01r01r4", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= "E13CBetn01r01r4", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title





seg.60 <- corr.series.seg(rwl=dat, series="E13CBetn01r03",
                          bin.floor = 0,
                          seg.length=6)


win <- 2006:2015
dat.yrs <- time(dat)
dat.win <- subset(dat,dat.yrs %in% win)
ccf.30 <- ccf.series.rwl(rwl=dat.win, series="E13CBetn01r03", 
                         seg.length=2, bin.floor=1, lag.max = 1)

xskel.ccf.plot(rwl=dat, series="E13CBetn01r03",
               win.start=2008, win.width=12)





