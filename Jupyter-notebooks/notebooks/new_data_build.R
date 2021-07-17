require('dplyr')
require('ggplot2')
require('broom')
require('tidyverse')
require('data.table')
require('car')
require('boot')
require("tidyr")
require('purrr')
require('gplots')
require('repr')

rm(list=ls())
graphics.off()

setwd("/home/primuser/Documents/VBTraining3/Jupyter-notebooks/")


df <- as_tibble(read.csv("../Jupyter-notebooks/data/mismatch_dat.csv"))


mosquito <-  df[grep("Aedes aegypti", df$interactor1), ]

bollworm <- df[grep("Helicoverpa armigera", df$interactor1), ]

spidermite <- df[grep("Tetranychus mcdanieli", df$interactor1), ]

nlls <- rbind(mosquito,bollworm,spidermite)

#write.csv(nlls, '../Jupyter-notebooks/data/nllsdataset.csv')


plot( nlls$ambienttemp, log(nlls$standardisedtraitvalue), col = nlls$standardisedtraitname)

lm1 <- as.data.frame(t(nlls))
#write.csv(lm1, '../Jupyter-notebooks/data/wranglingdataset.csv')
