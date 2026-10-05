## Cluster analyses
#
# This R script allows you to run simple cluster analyses of the form in the FAO multispecies toollbox.

# Clean up the R space
rm(list=ls())			

# Load Libraries
library(tidyverse)
library(devtools)
library(ggplot2)
library(RColourBrewer)
library(factoextra)
library (FactoMineR)
library(corrplot)
library(ape) 

# First start by loading in catch data with years as the columns and species as the rows.
# Load data from csv file
YR_as_col <- read.table("CatchData_YrCol_File.csv",sep=",",header=T,row.names=1)

# Then perform the clustering 
# Clustering using Euclidean distance measure and the Ward Hierarchical cluster approach. 
# Look up hclust to see what other clustering options are available.
dd <- dist(scale(YR_as_col), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

# Plot the dendrogram – this one flows down the page 
plot(hc, hang = -1, cex = 0.6)   # Put the labels at the same height: hang = -1

# To plot a dendrogram flowing across the page use
hcd <- as.dendrogram(hc)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19),  cex = 0.7, col = "blue")
par(cex=0.5)   # Customized plot; remove labels
plot(hcd,  xlab = "Height", nodePar = nodePar, horiz = TRUE)

# You can also plot unrooted dendrograms that flow as branches across the page
plot(as.phylo(hc), type = "unrooted", cex = 0.6, no.margin = TRUE)

# Lastly you can plot it as a circular arc or fan
par(cex=0.8)
plot(as.phylo(hc), type = "fan")
colours = c("red", "blue", "green", "black", "yellow", "purple", "grey", "brown", "magenta", "cyan", "violetred", "tomato")
clus = cutree(hc, 12) 			# Identify the clusters based on cutting the tree at a specific height
plot(as.phylo(hc), type = "fan", tip.colour = colours[clus],label.offset = 1, cex = 0.7). # Colour the names of each tip based on the clusters formed
