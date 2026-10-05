## Clear the space
rm(list = ls()) # clear memory

# Data handling and table reshaping
library(tidyverse)
library(reshape2)
library(devtools)
library(readxl)  ## For reading Excel files
library(fs)      ## For file system operations
library(openxlsx) ## For writing out Excel files
# Correlations
library(corrplot)
library(RColorBrewer)

# Perform correlation on indicators

# Filter out the year
SP_as_colnY <- subset(SP_as_col, select = -c(Year))
M <-cor(SP_as_colnY, method="spearman")
corrplot(M, type="upper", order="hclust", col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.cex=0.4)


M <-cor(pca.data, method="pearson")
corrplot(M, type="upper", order="hclust", col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.cex=0.5)

M <-cor(pca.data, method="spearman")
corrplot(M, type="upper", order="hclust", col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.cex=0.5)

## Correlation analysis
M <-cor(SP_as_col, method="pearson")   # For Pearson correlation
M <-cor(SP_as_col, method="spearman") # For Spearman correlation

# Plot resulting correlations
corrplot(M, type="upper", order="hclust", col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.cex=0.4) 
