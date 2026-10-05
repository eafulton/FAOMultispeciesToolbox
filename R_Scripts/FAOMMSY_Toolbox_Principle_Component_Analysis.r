## Principal component analyses
#
# This R script allows you to reproduce the kind of PCA analysis discussed in the main text

# Start by preparing the R space
rm(list=ls())

## Libraries
# Data handling and table reshaping
library(tidyverse)
library(reshape2)
library(devtools)
# Plotting
library(ggplot2)
library(RColourBrewer)
library(ggbiplot)
library("plot3D")
library(plotly)
# PCA and Clustering
library(factoextra)
library (FactoMineR)
library(corrplot)
library(ape)


# This code assumes the data is formatted as a csv with a filename RawCatchData.csv
# Assumed column structure is
# Year	Gear	Area	Species	Group	Yield
#
#
# Load the data. We have chosen to use read.table so that you can specify 
# yourself whether you are using a true csv file with commas between the 
# values in a row (comma delimited) or whether you are using a tab delimited. 
# If you are sure you are using a true csv that is comma delimited then read.csv 
# or read_csv is also a good choice.

CatchData <-read.table("RawCatchData.csv", header=TRUE, sep = ",")

# Total catch per species per year (i.e. sum over areas and gears) . 
# It is possible to use the filter function to pull out specific gear or area before continuing
# For example
#
# df_tmp <- filter(CatchData, Gear == "trawl")
# CatchSp <- df_tmp %>%
# group_by(Year, Species) %>%
#  dplyr::summarise(TotalCatch = sum(Yield))
#
CatchSp <- RawCatchData %>%
  group_by(Year, Species) %>%
  dplyr::summarise(TotalCatch = sum(Yield))

# Recaste so rows are years and columns are species
df_extract <- as.data.table(CatchSp)
SP_as_col <- reshape2::dcast(df_extract, Year ~ Group_name, value.var = "TotalCatch")

# Replace NA with zeros
SP_as_colA[is.na(SP_as_colA)] <- 0

# Strip out columns of all zeros
SP_as_col <- SP_as_colA[, colSums(SP_as_colA != 0) > 0]

# Strip out Year but keep as row names
row.names(SP_as_col) <- SP_as_col$Year
SP_as_col_noYr <- subset(SP_as_col, select = -c(Year))

# Get the column headers - so can paste to the pca as factors to consider
dimC <- dim(SP_as_col_noYr)
pc.f <- formula(paste("~", paste(names(SP_as_col_noYr)[2:dimC[2]], collapse = "+")))

# PCA calculations - using spectral decomposition approach via the princomp approach
pl.pca <- princomp(pc.f, cor=TRUE, data=SP_as_col_noYr)

# PCA calculations - using spectral decomposition approach via the princomp approach
pl.pca <- princomp(pc.f, cor=TRUE, data=SP_as_col_noYr)

# Put on row named
row.names(pl.pca$scores) <- SP_as_col$Year

# Plot results - look to see number of PCA axes to retain
# OutFilename <- paste(OutDir,"/PCA_VarExplained.png",sep="") # Uncomment if want to print to file not screen
# png(OutFilename, 1200, 800) # Uncomment if want to print to file not screen
plot(pl.pca, type="lines")
# dev.off() # Uncomment if want to print to file not screen

# Plot biplot
# outBiplot <- paste(OutDir,"/PCA_Biplot_Thru_Time.png",sep="")   # Uncomment if want to print to file not screen
dfPCA <- data.frame(comp1=pl.pca$scores[,1],
                    comp2=pl.pca$scores[,2])
ggplot(data = dfPCA, aes(x=comp1, y=comp2, group=1)) +
  geom_point(size=5, aes(colour=rownames(pl.pca$scores))) +
  geom_path(size = 0.2) +
  geom_text(label=rownames(pl.pca$scores)) + 
  theme(legend.position="none")
# ggsave(file=outBiplot) # Uncomment if want to print to file not screen

# Print out PCA loadings
pl.pca$loadings

# Print PCA summary – showing variance explained
summary(pl.pca)

# If you would rather use singular value decomposition instead for the PCA then the command is
pl.pca <- prcomp(SP_as_col_noYr, scale = TRUE)

# Regardless of the method used plot the variance explained
fviz_eig(pl.pca)

## Create a plot of the PCA space – 2D
PoV <- pl.pca$sdev^2/sum(pl.pca$sdev^2) # Variance explained (so can put it on the axis labels)

# Sort labels to put on the plot
row.names(pl.pca$scores) <- SP_as_col$Year
pcx <- pl.pca$scores[,1]
pcy <- pl.pca$scores[,2]
pcz <- pl.pca$scores[,3]
pcxlab <- paste("PC1 (", round(PoV[1] * 100, 2), "%)")
pcylab <- paste("PC2 (", round(PoV[2] * 100, 2), "%)")
pczlab <- paste("PC3 (", round(PoV[3] * 100, 2), "%)")

# Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
# OutFilename <- paste(OutDir,"/PCA_Scree_plot.png",sep=""). # Uncomment if want to print to file not screen
# png(OutFilename, 1200, 800). # Uncomment if want to print to file not screen
fviz_eig(res.pca)
# dev.off() # Uncomment if want to print to file not screen

# Plot the results
df <- data.frame(comp1=pl.pca$scores[,1], comp2=pl.pca$scores[,2])
ggplot(data = df, aes(x=comp1, y=comp2, group=1)) +
  geom_point(size=5, aes(colour=rownames(pl.pca$scores))) +
  geom_path(size = 0.2) +
  geom_text(label=rownames(pl.pca$scores)) + 
  theme(legend.position="none")

# An alternative way of making a plot of the 2D space is
fviz_pca_ind(pl.pca,
             col.ind = "contrib", # Colour by congtribution
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             #label=SP_as_col$Year
) + labs(title ="PCA", x = "PC1", y = "PC2")

# To put the vectors of the variables on to the biplot
fviz_pca_var(pl.pca,
             col.var = "contrib", # Colour by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Graph of variables
PCA_color_gradient <- c("#36648B", "#FFA500", "#8B2500")
PCA_color_gradient <- brewer.pal(9, name="YlOrBr")[c(3,4,5,6,7,8,9)]

# OutFilename <- paste(OutDir,"/Biplot_attribution.png",sep="") # Uncomment if want to print to file not screen
# png(OutFilename, 1200, 800). # Uncomment if want to print to file not screen
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = PCA_color_gradient,
             repel = TRUE     # Avoid text overlapping
)
# dev.off(). # Uncomment if want to print to file not screen

# Compute hierarchical clustering on principal components
res.pca <- PCA(SP_as_col, ncp = 3, graph = FALSE)
res.hcpc <- HCPC(res.pca, graph = FALSE)

# Plot the resulting dendrogram
# OutFilename <- paste(OutDir,"/PCA_score_dendrogram.png",sep=""). # Uncomment if want to print to file not screen
# png(OutFilename, 1200, 800) # Uncomment if want to print to file not screen
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Colour palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle colour
          labels_track_height = 0.8      # Augment the room for labels
)
# dev.off() # Uncomment if want to print to file not screen

# Now replot the PCA space with those clusters marked
# OutFilename <- paste(OutDir,"/PCA_score_clusters.png",sep=""). # Uncomment if want to print to file not screen
# png(OutFilename, 1200, 800).  # Uncomment if want to print to file not screen
fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Colour palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)
# dev.off(). # Uncomment if want to print to file not screen

## Create a plot of the PCA space – 3D
# Plot simply as points in 3D space
scatter3D(pcx, pcy, pcz, bty = "g", pch = 20, cex = 2, 
          col = gg.col(100), theta = 150, phi = 0, main = "PCA Scores", xlab = pcxlab,
          ylab =pcylab, zlab = pczlab)
text3D(pcx, pcy, pcz,  labels = rownames(pl.pca$scores), add = TRUE, colkey = FALSE, cex = 0.7)

# 3D plot with connected line showing path through time
scatter3D(pcx, pcy, pcz, bty = "g", type = "b", pch = 20, cex = 2, 
          col = gg.col(100), theta = 150, phi = 0, lwd = 4, main = "PCA Scores", xlab = pcxlab,
          ylab =pcylab, zlab = pczlab)
text3D(pcx, pcy, pcz,  labels = rownames(pl.pca$scores), add = TRUE, colkey = FALSE, cex = 0.7)

# Plot3D with plotly 
# Which allows interaction with the plot, you can spin it, zoom and hover over data points and see the associated data values)
manual_palette <- c("#551A8B", "#8968CD", "#AB82FF", "#FF8C00", "#CD6600", "#8B4500")
df3D <- data.frame(comp1=pl.pca$scores[,1],
                   comp2=pl.pca$scores[,2],
                   comp3=pl.pca$scores[,3])
fig <- plot_ly(df3D, x = ~comp1, y = ~comp2, z = ~comp3, colour = ~comp3,  mode = 'lines+markers',
               # Hover text:
               text = ~rownames(pl.pca$scores))
fig <- fig %>% add_markers()
fig <- fig %>% add_text(textposition = "top right")
fig <- fig %>% layout(scene = list(xaxis = list(title = pcxlab),
                                   yaxis = list(title = pcylab),
                                   zaxis = list(title = pczlab)),
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        text = 'PC3 Score',
                        showarrow = FALSE
                      ))
fig  # Plot the final figure