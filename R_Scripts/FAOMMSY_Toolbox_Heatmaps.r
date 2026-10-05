## Clear the space
rm(list = ls()) # clear memory

# Data handling and table reshaping
library(tidyverse)
library(reshape2)
library(devtools)
library(readxl)  ## For reading Excel files
library(fs)      ## For file system operations
library(openxlsx) ## For writing out Excel files
# Plotting
library(ggplot2)
library(RColorBrewer)
library(ggbiplot)
library("plot3D")
library(plotly)
library(heatmaply)
library (data.table)
# PCA and Clustering
library(factoextra)
library (FactoMineR)
library(corrplot)
library(ape)
# Correlations
library(corrplot)
library(RColorBrewer)

# Set the working directory
setwd("/Users/ful083/Work/Lenfest_EBFM/case_studies/Thailand/Analyses_&_data_workup/working_up_data/")

###################################################################################
# Define the read_sheet function
###################################################################################

## Need to force a read in as text due to initial blanks in many columns
read_sheet<-function(sheet_name, excel_file) {
  ## Read the Excel sheet
  sheet_data <- read_excel(excel_file, sheet = sheet_name, col_types = "text")
}

###################################################################################
# Data Handling
###################################################################################

##### If loading CSV file use this load code ####

# Load data - years as rows, species as columns if already have the file formatted in the desired way
#pca.data <- read.table('updated_thai_stat_sp_as_col.csv',sep=",",header=T,row.names=1). # For use with PCAs
#hmp.data <- read.table('updated_thai_stat_sp_as_col.csv',sep=",",header=T). # For use with heatmap
#hp.data <- read.table('simplified_thai_stat_sp_as_col.csv',sep=",",header=T)

# Can also consider inverse data - species as rows and years as columns
# pca.data <- read.table('Year_as_column_data.csv',sep=",",header=T,row.names=1)

##### If loading from excel file generated from a database query use this load code ####

# Option when loading from database table
input_directory <- "/Users/ful083/Work/Lenfest_EBFM/case_studies/Thailand/Analyses_&_data_workup/data/Database query output"
file_name <- "Stats catch per group with corrections.xlsx"
datsheet_name <- "Stats_data"
excel_file <- file.path(input_directory, file_name)
## Check if the file exists
if (!file.exists(excel_file)) {
  stop(paste("File not found:", excel_file))
}
## Read in the data - need to convert columns containing numbers 
## back to numeric variable types (as had to read in all as strings)
df <- read_sheet(datsheet_name, excel_file) # This is the name of the sheet in the file
df <- type_convert(df)

# Replace any spaces in species names
df$Group_name <- gsub(" ", "_", df$Stat_name_eng)
df$Group_name <- gsub("-", "_", df$Group_name)

# Now extract data from the dataframe and reshape to format needed
# filter by area or sector if desired - currently only filtering based on sector
sector <- "COM"
df_tmp <- filter(df, grepl(sector, Fishing_Sector))
df_tmp <- df_tmp %>%
  group_by(yearAD, Group_name) %>%
  dplyr::summarise(TotalCatch = sum(EstCatch))

# Recaste so rows are years and columns are species
df_extract <- as.data.table(df_tmp)
pca.data <- dcast(df_extract, yearAD ~ Group_name, value.var = "TotalCatch")

# Replace NA with zero
pca.data[is.na(pca.data)] <- 0

# Rename an NA column
pca.data <- pca.data %>% rename(Unknown = "NA")
hmp.data <- pca.data

# Check for any columns with column sum of zero and filter them out

# print out data as a check
View(pca.data)
# prints out a copy of the data table

# To do PCA calculations, the data matrix needs to be complete (no blank cells) 
# So create variable names to follow
dimC <- dim(pca.data)
pc.f <- formula(paste("~", paste(names(pca.data)[1:dimC[2]], collapse = "+")))

###############################################################################################################
# Run PCA 
# First start with using spectral decomposition. For that to work there has to be more rows than columns. This
# is NOT the case here typically so try the other method. If you do need to switch method skip down to the
# singular value decomposition version (search for prcomp in the code)
###############################################################################################################

# DO PCA calculations
pl.pca <- princomp(pc.f, cor=TRUE, data=pca.data)

## PCA statistics (how variance is explainedin the PCA)
# Print out PCA loadings
pl.pca$loadings

# Print out eigenvalues
pl.pca$sd^2

# Print PCA summary
summary(pl.pca)

## Plot results - this is the variance is explained per axes of the PCA.
# Use this to look to see number of PCA axes to retain - it is best if it is only 2-3
plot(pl.pca, type="lines")

# Plot points - how the years map against the PCA axes (to see which years group together)
text(pl.pca$scores, labels=as.character(row.names(pca.data)), pos=1, cex=0.7)

# Biplot of PCA - to see which species group with which area of the PCA
biplot(pl.pca, cex=0.8, col=c(1,8))

# Linked biplot = to show how the PCA shifts through time
View(pl.pca$scores)
library(ggplot2)
df <- data.frame(comp1=pl.pca$scores[,1],
                 comp2=pl.pca$scores[,2])
ggplot(data = df, aes(x=comp1, y=comp2, group=1)) +
  geom_point(size=5, aes(colour=rownames(pl.pca$scores))) +
  geom_path(size = 0.2) +
  geom_text(label=rownames(pl.pca$scores)) + 
  theme(legend.position="none")

### Optional plots for exploring the results ###
# This is still using the output of the spectral decomposition from princomp function
PoV <- pl.pca$sdev^2/sum(pl.pca$sdev^2)
fviz_eig(pl.pca)

# Put on row named
#Rename Col 1
View(pca.data)

pcx <- pl.pca$scores[,1]
pcy <- pl.pca$scores[,2]
pcz <- pl.pca$scores[,3]

pcxlab <- paste("PC1 (", round(PoV[1] * 100, 2), "%)")
pcylab <- paste("PC2 (", round(PoV[2] * 100, 2), "%)")
pczlab <- paste("PC3 (", round(PoV[3] * 100, 2), "%)")

View(pl.pca$scores)

# 3D as points - to show how years group in the PCA space
scatter3D(pcx, pcy, pcz, bty = "g", pch = 20, cex = 2, 
          col = gg.col(100), theta = 150, phi = 0, main = "PCA Scores", xlab = pcxlab,
          ylab =pcylab, zlab = pczlab)
text3D(pcx, pcy, pcz,  labels = rownames(pl.pca$scores), add = TRUE, colkey = FALSE, cex = 0.7)

# 3D as connected line - to show how years group in the PCA space and how that changes through time
scatter3D(pcx, pcy, pcz, bty = "g", type = "b", pch = 20, cex = 2, 
          col = gg.col(100), theta = 150, phi = 0, lwd = 4, main = "PCA Scores", xlab = pcxlab,
          ylab =pcylab, zlab = pczlab)
text3D(pcx, pcy, pcz,  labels = rownames(pl.pca$scores), add = TRUE, colkey = FALSE, cex = 0.7)

# Plotly version so you can directly interact with the plot (you can click on data points)
df3D <- data.frame(comp1=pl.pca$scores[,1],
                   comp2=pl.pca$scores[,2],
                   comp3=pl.pca$scores[,3])
fig <- plot_ly(df3D, x = ~comp1, y = ~comp2, z = ~comp3, color = ~comp3,  mode = 'lines+markers',
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
fig

########################### Use prcomp() instead - this uses singular value decomposition  ###########################
# This should work regardless of the number of rows and columns
res.pca <- prcomp(pca.data, scale = TRUE)

# Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component. 
# The PCA is useful if most of the variance is explained in the first 2-3 dimensions
fviz_eig(res.pca)

# Graph of individuals. Individual years with a similar composition profile are grouped together.
fviz_pca_ind(res.pca,
             col.ind = "contrib", # Color by congtribution
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             #label=SP_as_col$Year
) +
  labs(title ="PCA", x = "PC1", y = "PC2")

# Graph of variables - this tries to show which species align with each area of the plot. It is quite crowded for Thailand.
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#36648B", "#FFA500", "#8B2500"),
             repel = TRUE     # Avoid text overlapping
)

# Compute hierarchical clustering on principal components (to most easily see which years group together)
res.pca4 <- PCA(pca.data, ncp = 3, graph = FALSE)
res.hcpc <- HCPC(res.pca4, graph = FALSE)

# Plots the dendrogram marking years that cluster together (the earliest year will show as 1 and years will increment from there)
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

# This is another way of plotting the clusters (again the earliest year will show as 1 and years will increment from there)
fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_bw(),
             main = "Factor map"
)


### Optional plots for exploring the results ###
# This is still using the output of the ingular value decomposition from prcomp function
PoV <- res.pca$sdev^2/sum(res.pca$sdev^2)
fviz_eig(res.pca)

pcx <- res.pca$x[,1]
pcy <- res.pca$x[,2]
pcz <- res.pca$x[,3]

pcxlab <- paste("PC1 (", round(PoV[1] * 100, 2), "%)")
pcylab <- paste("PC2 (", round(PoV[2] * 100, 2), "%)")
pczlab <- paste("PC3 (", round(PoV[3] * 100, 2), "%)")

# 3D as points - to show how years group in the PCA space
scatter3D(pcx, pcy, pcz, bty = "g", pch = 20, cex = 2, 
          col = gg.col(100), theta = 150, phi = 0, main = "PCA Scores", xlab = pcxlab,
          ylab =pcylab, zlab = pczlab)
text3D(pcx, pcy, pcz,  labels = pca.data$yearAD, add = TRUE, colkey = FALSE, cex = 0.7)

# 3D as connected line - to show how years group in the PCA space and how that changes through time
scatter3D(pcx, pcy, pcz, bty = "g", type = "b", pch = 20, cex = 2, 
          col = gg.col(100), theta = 80, phi = 0, lwd = 2, main = "PCA Scores", xlab = pcxlab,
          ylab =pcylab, zlab = pczlab)
text3D(pcx, pcy, pcz,  labels = pca.data$yearAD, add = TRUE, colkey = FALSE, cex = 0.7)

# Rainbow version- to show how years group in the PCA space and how that changes through time
df <- data.frame(comp1=pcx, comp2=pcy)
plot_indx <- "2D_PCA_ts_Thailand.png"
ggplot(data = df, aes(x=comp1, y=comp2, group=1)) +
  geom_point(size=5, aes(colour=pca.data$yearAD)) +
  geom_path(linewidth = 0.2) +
  geom_text(label=pca.data$yearAD) + 
  theme(legend.position="none")
ggsave(file=plot_indx)

ggplot(data = df, aes(x=comp1, y=comp2, group=1)) +
  geom_point(size=5, aes(colour=pca.data$yearAD)) +
  geom_text(label=pca.data$yearAD) + 
  theme(legend.position="none")

# Plotly version so you can directly interact with the plot (you can click on data points)
df3D <- data.frame(comp1=pcx, comp2=pcy, comp3=pcz)
fig <- plot_ly(df3D, x = ~comp1, y = ~comp2, z = ~comp3, color = ~comp3,  mode = 'lines+markers',
               # Hover text:
               text = ~rownames(res.pca$x))
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
fig

################################# DENDROGRAMS ###############################
# Dendrogram flowing down the page
dd <- dist(scale(pca.data), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc, hang = -1, cex = 0.8)   # Put the labels at the same height: hang = -1

# Dendrogram flowing across the page
# Define nodePar
hcd <- as.dendrogram(hc)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19),  cex = 0.7, col = "blue")
par(cex=0.5)   # Customized plot; remove labels
par(cex=1.0)   # Customized plot; remove labels
plot(hcd,  xlab = "Height", nodePar = nodePar, horiz = TRUE)

# Unrooted - dendrogram on an arc
plot(as.phylo(hc), type = "unrooted", cex = 0.6, no.margin = TRUE)

# Fan
par(cex=0.3)
par(cex=1.0)
plot(as.phylo(hc), type = "fan")
colors = c("red", "blue", "green", "black", "yellow", "purple", "grey", "brown", "magenta", "cyan", "violetred", "tomato")
clus = cutree(hc, 12)
plot(as.phylo(hc), type = "fan", tip.color = colors[clus],label.offset = 1, cex = 0.8)


########################### CORRELATION RESULTS ###########################  
# Perform correlation on indicators

M <-cor(pca.data, method="pearson")
#M <-cor(pca.data, method="spearman")
corrplot(M, type="upper", order="hclust", col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.cex=0.5)

############################# AREA plots #################################
col3Palette <- c(brewer.pal(9, "Oranges")[c(8, 7, 6, 5, 2)], 
                 brewer.pal(11, "PiYG")[c(1, 2, 3, 4, 5)],
                 brewer.pal(9, "Purples")[c(2, 4, 3, 6, 7, 8, 9)],
                 'mediumorchid4', 'mediumorchid3', 'mediumorchid2', 'mediumorchid1',
                 'orchid4', 'orchid3', 'orchid2', 'orchid1',
                 'hotpink4', 'hotpink2', 'hotpink1',
                 'violetred2', 'violetred3', 'violetred4',
                 brewer.pal(9, "Greens")[c(2, 6, 7, 8, 9)],
                 'turquoise4', 'turquoise3', 'turquoise2', 'turquoise1', 'paleturquoise1',
                 brewer.pal(9, "Blues")[c(1, 2, 3, 4, 5, 6, 7, 8, 9)],
                 'royalblue3', 'darkslateblue', 'midnightblue',
                 brewer.pal(9, "BrBG")[c(1, 2, 3, 4, 5)], 
                 'chocolate4', 'chocolate3', 'chocolate2', 'chocolate1',
                 brewer.pal(9, "BrBG")[c(3, 2, 1)], 
                 'goldenrod2', 'goldenrod1', 'lightgoldenrod1', 'khaki4',
                 'black', brewer.pal(9, "Greys")[c(5, 4, 3, 2)],
                 'aliceblue', 'darkred', 'red3', 'red1',
                 'firebrick4', 'firebrick3', 'firebrick1', 'rosybrown1')

final_catch <- reshape2::melt(pca.data, id = c("yearAD"))
colnames(final_catch)[2:3] <- c("Species", "Catch")
colourCount = length(unique(final_catch$Species))
ggplot(data = final_catch, aes(x = yearAD, y = Catch, fill = Species)) + geom_bar(colour = "black", stat="identity", size = 0.1) +
  scale_fill_manual(values = col3Palette) + theme_bw() +
  labs(x="Years", y = "Tonnes") + theme(legend.position = "none") +
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=20,face="bold"))

ggplot(data = final_catch, aes(x = yearAD, y = Catch, fill = Species)) + geom_bar(colour = "black", stat="identity", size = 0.1) +
  scale_fill_manual(name = "Species", values = col3Palette) + theme_bw() +
  labs(x="Years", y = "Tonnes") + theme(legend.text=element_text(size=10)) +
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=14,face="bold"))

ggplot(data = final_catch, aes(x = yearAD, y = Catch, fill = Species)) + geom_bar(colour = "black", position="fill", stat="identity", size = 0.1) +
  scale_fill_manual(name = "Species", values = col3Palette) + theme_bw() +
  labs(x="Years", y = "proportion of catch") + theme(legend.text=element_text(size=10)) +
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=14,face="bold"))

################################# Heatmap ####################################################
# Create row names and trim off first column
#SP_as_col <-filter(hmp.data, yearAD < 2019)
SP_as_col <- hmp.data
rownames(SP_as_col) <- SP_as_col$yearAD
SP_as_colA <- SP_as_col %>% dplyr::select(-yearAD)

# For any species starting with a zero catch replace 0 with 1 for purposes of 
zero_cols <- which(SP_as_colA[1, ] == 0)
SP_as_colA[1, zero_cols] <- 1

# Transpose SP_as_col so years are column headers
# Here the values are proportion of the catch but could be absolute catch
Yr_as_col <- t(SP_as_colA)
colnames(Yr_as_col) <- SP_as_col$yearAD

# Get the relative value vs start year
Prop_Catch <- apply(Yr_as_col, 2, function(i) i/sum(i))

nSP <- length(Yr_as_col[,1])
nYr <- length(Yr_as_col[1,])

# Create new df to populate
drRanked <- data.frame(matrix(ncol = nYr, nrow = nSP))

# Do ranking (using order() routine as want largest value to be rated 1)
for (i in 1:nSP) {
  drRanked[i,] <- frank(Yr_as_col[i,], ties.method ="dense")
}
colnames(drRanked)[1:nYr] = as.character(colnames(Yr_as_col)[1:nYr])
rownames(drRanked)[1:nSP] = as.character(rownames(Yr_as_col)[1:nSP])

# Create the heatmap - switch our row for both to see if years cluster up
HeatmapTitle <- paste ("Heatmap of contribution to catch through time, yellow represents a larger contribution",sep="") 
HeatmapFile <- "Heatmap_Thai_2018.png"

heatmaply(drRanked,
          xlab = "Year",
          ylab = "Species",
          main = HeatmapTitle,
          dendrogram = "row",
          #dendrogram = "both",
          fontsize_row = 6,
          file = HeatmapFile
)

################## Other ecological indicators #################################

# Load data for other ecological indicators
sector <- "COM"

# First the trophic level data
TL_file_name <- "Stats catch TL.xlsx"
TL_datsheet_name <- "stats_TL"
TL_excel_file <- file.path(input_directory, TL_file_name)
## Read in the data - need to convert columns containing numbers 
## back to numeric variable types (as had to read in all as strings)
df <- read_sheet(TL_datsheet_name, TL_excel_file) # This is the name of the sheet in the file
df <- type_convert(df)

# Replace any spaces in species names
df$Group_name <- gsub(" ", "_", df$Stat_name_eng)
df$Group_name <- gsub("-", "_", df$Group_name)

# Now extract data from the dataframe and reshape to format needed
# filter by area or sector if desired - currently only filtering based on sector
df_TL <- filter(df, grepl(sector, Fishing_Sector))
df_TL <- filter(df_TL, Avg_TL > 0.0)
colnames(df_TL)[which(names(df_TL) == "RV_Year")] <- "yearAD"

# Now the Linf data
Linf_file_name <- "Stats catch Linf.xlsx"
Linf_datsheet_name <- "stats_Linf"
Linf_excel_file <- file.path(input_directory, Linf_file_name)
## Read in the data - need to convert columns containing numbers 
## back to numeric variable types (as had to read in all as strings)
df <- read_sheet(Linf_datsheet_name, Linf_excel_file) # This is the name of the sheet in the file
df <- type_convert(df)

# Replace any spaces in species names
df$Group_name <- gsub(" ", "_", df$Stat_name_eng)
df$Group_name <- gsub("-", "_", df$Group_name)

# Now extract data from the dataframe and reshape to format needed
# filter by area or sector if desired - currently only filtering based on sector
df_Linf <- filter(df, grepl(sector, Fishing_Sector))
df_Linf <- filter(df_Linf, Avg_Linf > 0.0)
colnames(df_Linf)[which(names(df_Linf) == "RV_Year")] <- "yearAD"

##### Calculate the actual indicators
# Mean trophic level of the catch - this is the MTI with no cut-off
# (so all species across all trophic levels are included)
df_TL$TLcatch <- df_TL$SumOfEstCatch * df_TL$Avg_TL

df_overallTL <- df_TL %>%
  group_by(yearAD) %>%
  dplyr::summarise(avgTLcatch = sum(TLcatch)/sum(SumOfEstCatch))

# MTI - this is often used with a cut-off (so all catch below that cut off 
# trophic level value is ignored) in the calculation 
# The most common cut-off is trophic level 3.25 - this is MTI_3.25
df_tmp <- filter(df_TL, Avg_TL >= 3.25)
df_tmp <- filter(df_tmp, SumOfEstCatch > 0.0)

df_MTI_325 <- df_tmp %>%
  group_by(yearAD) %>%
  dplyr::summarise(MTI_3_25 = sum(TLcatch)/sum(SumOfEstCatch))

# In some places the cut off used is 4 - this is MTI_4. This is for places focusing on the conservation of large fish
df_tmp <- filter(df_TL, Avg_TL >= 4)

df_MTI_4 <- df_tmp %>%
  group_by(yearAD) %>%
  dplyr::summarise(MTI_4 = sum(TLcatch)/sum(SumOfEstCatch))


# Proportion of large fish (LFI)  - which is the proportion of the catch (or survey if on survey data) 
# made up of species with Linf >= 40cm
df_Linf <- filter(df_Linf, SumOfEstCatch > 0.0)
df_Linf$included <- ifelse(df_Linf$Avg_Linf < 40, 0, 1)
df_Linf$LFI_catch <- df_Linf$included * df_Linf$SumOfEstCatch

df_tmp <- filter(df_Linf, !grepl("Trash_fish", Group_name)) # Excluded as ends up wtith high values in final years due to presence of juveniles of market fish

df_LFI <- df_tmp %>%
  group_by(yearAD) %>%
  dplyr::summarise(LFI = sum(LFI_catch)/sum(SumOfEstCatch))


## Plot the results
plot_indx <- paste("ecological_indcators - mean trophic level per group.png",sep="")
ggplot(data = df_TL, aes(x = yearAD, y = Avg_TL)) +
  geom_line(color = "deepskyblue4") +
  labs(
    title = "Average trophic of groups in the catch",
    x = "Year",
    y = "Average trophic level"
  ) + 
  facet_wrap(~ Group_name, scales = 'free') +
  theme(axis.text.y=element_text(size=10),axis.text.x=element_text(size=10), axis.title=element_text(size=12,face="bold"), strip.text = element_text(face="bold", size=10))
ggsave(file=plot_indx)


plot_indx <- paste("ecological_indcators - mean trophic level of the catch.png",sep="")
ggplot(data = df_overallTL, aes(x = yearAD, y = avgTLcatch)) +
  geom_line(color = "deepskyblue4", linewidth = 2) +
  labs(
    title = "Average trophic level of the catch",
    x = "Year",
    y = "Average trophic level"
  ) + 
  theme(axis.text.y=element_text(size=10),axis.text.x=element_text(size=10), axis.title=element_text(size=12,face="bold"), strip.text = element_text(face="bold", size=10))
ggsave(file=plot_indx)

plot_indx <- paste("ecological_indcators - MTI.png",sep="")
ggplot() +
  geom_line(data = df_MTI_325, aes(x = yearAD, y = MTI_3_25), color = "chocolate", linewidth = 2) +
  geom_line(data = df_MTI_4, aes(x = yearAD, y = MTI_4), color = "deepskyblue3", linewidth = 2) +
  labs(
    title = "Ecological indicator time series - average trophic level and MTI",
    subtitle = "Blue line is MTI_4 and orange line is MTI_3.25",
    x = "Year"
  ) + 
  theme(axis.text.y=element_text(size=10),axis.text.x=element_text(size=10), axis.title=element_text(size=12,face="bold"), strip.text = element_text(face="bold", size=10))
ggsave(file=plot_indx)

plot_indx <- paste("ecological_indcators - catch per group.png",sep="")
ggplot(data = df_Linf, aes(x = yearAD, y = SumOfEstCatch)) +
  geom_line(color = "midnightblue") +
  labs(
    title = "Average trophic level of the catch",
    x = "Year",
    y = "Average trophic level"
  ) + 
  facet_wrap(~ Group_name, scales = 'free') +
  theme(axis.text.y=element_text(size=10),axis.text.x=element_text(size=10), axis.title=element_text(size=12,face="bold"), strip.text = element_text(face="bold", size=10))
ggsave(file=plot_indx)

plot_indx <- paste("ecological_indcators - Linf per group.png",sep="")
ggplot(data = df_Linf, aes(x = yearAD, y = Avg_Linf)) +
  geom_line(color = "firebrick4") +
  labs(
    title = "Average Linf per groupin the catch",
    x = "Year",
    y = "Average Linf"
  ) + 
  facet_wrap(~ Group_name, scales = 'free') +
  theme(axis.text.y=element_text(size=10),axis.text.x=element_text(size=10), axis.title=element_text(size=12,face="bold"), strip.text = element_text(face="bold", size=10))
ggsave(file=plot_indx)

plot_indx <- paste("ecological_indcators - Large Fish Index.png",sep="")
ggplot(data = df_LFI, aes(x = yearAD, y = LFI)) +
  geom_line(color = "firebrick4", linewidth = 2) +
  labs(
    title = "Large Fish Index - proportion of the catch with Linf > 40cm",
    x = "Year",
    y = "LFI"
  ) + 
  theme(axis.text.y=element_text(size=10),axis.text.x=element_text(size=10), axis.title=element_text(size=12,face="bold"), strip.text = element_text(face="bold", size=10))
ggsave(file=plot_indx)
