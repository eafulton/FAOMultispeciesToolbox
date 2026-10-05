## Exploring data

# We are assuming that anyone reading this section is relatively new to R and so we 
# are providing simple scripts to show the steps needed to plot and work with data. 
# There are however very many professional packages and sophisticated data handling 
# approaches so we encourage the interested reader to look into what are the latest 
# tools available. As you become more familiar with R you will likely consider these 
# scripts simplistic, but they have the benefit of being relatively easy to follow, 
# please feel free to tailor your own scripts to whatever you feel is the best approach. 

# This script is to plot a stacked bar plot (essentially creating an area plot) 

# Clean up the R space - this is to make sure that there is no memory hold over from past analyses
rm(list=ls())			

# Load Libraries - relevant libraries needed in processing the data
library(tidyverse)
library(devtools)
library(ggplot2)
library(RColourBrewer)

# This code assumes the data is formatted as a csv with a filename RawCatchData.csv
# Assumed column structure is
# Year	Gear	Area	Species	Group	Yield
#
#
# and for effort it assumes a csv with a filename RawEffortData.csv
# And assumed column structure
# Year	Gear	Area	Effort

# Load the data. We have chosen to use read.table so that you can specify 
# yourself whether you are using a true csv file with commas between the 
# values in a row (comma delimited) or whether you are using a tab delimited. 
# If you are sure you are using a true csv that is comma delimited then read.csv 
# or read_csv is also a good choice.

CatchData <-read.table("RawCatchData.csv", header=TRUE, sep = ",")
EffortData <-read.table("RawEffortData.csv ", header=TRUE, sep = ",")

# These loaded datasets are known as dataframes

# Summarise the data. 
# To summarise the data pipe (%>%) the original data through a group_by(), 
# which lists which variables (columns) to use when grouping the data, 
# and then through summarise() which shows what actions to take. 
# summarise() has a lot of useful functions (options) to choose from such 
# as sum(), mean(), median(), min(), max(), quantile(), sd() which is the 
# standard deviation and n() which is count. The format of the summarise() call is 
# summarise( NewVariableName = function(DataColumnName))

# You can do more than one thing at a time. For example
# summarise( NewSum = sum(ColName), NewMean = mean(ColName))
# As multiple R libraries have a summarise() function included, to avoid 
# confusion some people like to make the call to the tidyverse (a useful R 
# package for handling data) version by writing dplyr::summarise. So to 
# summarise the catch and effort loaded in above we would use:
  
# Total catch per species per gear per year (i.e. sum over areas) 
SumCatchSp <- RawCatchData %>%
  group_by(Year, Gear, Species) %>%
  dplyr::summarise(TotalYieldSp = sum(Yield))

# Total catch per gear per year (i.e. sum over areas and species)
SumCatchGear <- RawCatchData %>%
  group_by(Year, Gear) %>%
  dplyr::summarise(TotalYieldGear = sum(Yield), MeanYieldGear = mean(Yield), StdDevYieldGear = sd(Yield))

# Total catch per year (i.e. sum over areas, gears and species)
SumTotalCatch <- RawCatchData %>%
  group_by(Year) %>%
  dplyr::summarise(TotalYield = sum(Yield))

# Total effort per gear per year (i.e. sum over areas)
SumEffortGear <- RawCatchData %>%
  group_by(Year, Gear) %>%
  dplyr::summarise(TotalEffortGear = sum(Effort))

# For large datasets with many species you may want to group the species based
# on the kinds of functional groups listed earlier (e.g. Demersal fish, 
# Pelagic fish, Cephalopods etc). To do this you would summarise the data 
# per Group instead of Species: 
  
SumCatchGroup <- RawCatchData %>%
  group_by(Year, Gear, Group) %>%
  dplyr::summarise(TotalYieldGroup = sum(Yield))

# Do any simple data transformations that are necessary. For example to create 
# a raw CPUE (literally catch divided by effort with no further standardisation)

SumCPUE <- merge(SumCatchGear, SumEffort, by=c("Year","Gear "))
SumCPUE$CPUE <- (SumCPUE$TotalYieldGear / SumCPUE$TotalEffortGear)

# The first line merges the two data frames together into one – joining them 
# based on Year (y) and Gear (g). The second line then calculates CPUE as 
#〖CPUE〗_(y,g) =〖Catch〗_(y,g)/〖Effort〗_(y,g) 

# Also calculate the Coefficient of Variation (CV) for catch. 
# Effort could be done in the same way
SumCatchGear$CV <- (SumCatchGear$StdDevYieldGear / SumCatchGear$MeanYieldGear) * 100.0

# Plot the results. A number of powerful plotting options exist in R. 
# For demonstration purposes we have chosen to use ggplot2 but if you are 
# more familiar with another approach please use that. In the examples below we 
# have put in bold the things that differ between the different scripts

# First create a mapping of the gear names to use in the plots
# GearLabel <- c("Name1", " Name2", " Name3", …. , " NameY")
# gear_names <- as_labeller(c(`GearCode1` = "Name1", ` GearCode2` = "Name3", …. , `GearCodeY` = "NameY"))

# Plot total catch per species per year – one stacked on top of the other – Using the BrownGreen colour palette
colourCount = length(unique(SumCatchSp$Species))			  # So that the colour key matches the number of species     
ggplot(data = SumCatchSp, aes(x = Year, y = TotalYieldSp, fill = Species)) + # This says what data set to look at and then what to use as the x and y  data
  geom_bar(colour = "black", stat="identity", size = 0.1) +		  # This indicates it will be a bar plot with black line edge
  scale_fill_manual(values = colourRampPalette(brewer.pal(12, "BrBG"))(colourCount)) +   # This says what colour palette to use
  theme_bw() +						 # This sets the background colour and axes format etc
  labs(x="Years", y = "Tonnes", fill = "Species\n") +  		 # This sets the axis and legend titles 
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=20,face="bold"), 
        legend.title=element_text(size=18,face="bold"), legend.text = element_text(size=16)) 	# Font sizes

# Plot total catch per group per year – one stacked on top of the other. If you prefer a true area plot use geom_area() instead of geom_bar()
colourCount = length(unique(SumCatchGroup$Group)) 
ggplot(data = SumCatchGroup, aes(x = Year, y = TotalYieldGroup, fill = Group)) +
  geom_bar(colour = "black", stat="identity", size = 0.1) +
  scale_fill_manual(values = colourRampPalette(brewer.pal(12, "BrBG"))(colourCount)) +
  theme_bw() +
  labs(x="Years", y = "Tonnes", fill = "Major Groups\n") + 
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=20,face="bold"), 
        legend.title=element_text(size=18,face="bold"), legend.text = element_text(size=16))  

# Plot proportion of total catch per group per year 
colourCount = length(unique(SumCatchGroup$Group)) 
ggplot(data = SumCatchGroup, aes(x = Year, y = TotalYieldGroup, fill = Group)) + 
  geom_bar(colour = "black", position="fill", stat="identity", size = 0.1) +
  scale_fill_manual(values = colourRampPalette(brewer.pal(12, "BrBG"))(colourCount)) + 
  theme_bw() +
  labs(x="Years", y = "Proportional catch composition", fill = "Major Groups\n") + 
  
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=20,face="bold"), 
        legend.title=element_text(size=18,face="bold"), legend.text = element_text(size=16))

# Plot total effort per gear per year (not a colourCount isn’t needed if the number of things being plotted (e.g. gears) is less than the default colour palette
ggplot(data = SumEffortGear, aes(x = Year, y = TotalEffortGear, fill = Gear)) + 
  geom_bar(colour = "black", stat="identity", size = 0.1) +
  scale_fill_brewer(palette="PiYG" , labels = GearLabel) +    # Using the PinkGreen colour palette and replacing codes with labels
  theme_bw() +
  labs(x="Years", y = "Effort", fill = "Major gears\n") + 
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=20,face="bold"), 
        legend.title=element_text(size=18,face="bold"), legend.text = element_text(size=16))

# Plot total effort per gear per year – with each gear plotted in it’s own small plot 
ggplot(data = SumEffortGear, aes(x = Year, y = TotalEffortGear, fill = Gear)) + 
  geom_bar(colour = "black", stat="identity", size = 0.1) +
  facet_wrap(Gear~., scales="free", ncol = 2, labeller = gear_names) +   # So each gear is in own plot panel
  scale_fill_brewer(palette="PiYG", labels = GearLabel) + 
  theme_bw() +
  labs(x="Years", y = "Effort", fill = "Major gears\n") + 
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=20,face="bold"), 
        legend.title=element_text(size=18,face="bold"), legend.text = element_text(size=16))

# Plot total CPUE per year – Using the PurpleOrange colour palette
ggplot(data = SumCPUE, aes(x = Year, y = CPUE, fill = Gear)) + 
  geom_bar(colour = "black", stat="identity", size = 0.1) +
  scale_fill_brewer(palette="PuOr", labels = GearLabel) + 
  theme_bw() +
  labs(x="Years", y = "CPUE", fill = "Major gears\n") + 
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=20,face="bold"), 
        legend.title=element_text(size=18,face="bold"), legend.text = element_text(size=16))

# Plot CV of catch per gear per year – Using a manually defined colour palette
manual_palette <- c("#551A8B", "#8968CD", "#AB82FF", "#FF8C00", "#CD6600", "#8B4500")
ggplot(data = SumCatchGear, aes(x = Year, y = CV, colour = Gear)) + geom_line() +
  facet_wrap(Gear~., scales="free", ncol = 2, labeller = gear_names) + 
  
  scale_colour_manual(values=manual_palette, name="Gear") +
  theme_bw() + labs(x="Years", y = "CV of Catch") + ylim(0, 650) + 			# ylim lets you set the limits of the y axis
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=20,face="bold"), 
        legend.title=element_text(size=18,face="bold"), legend.text = element_text(size=16))

# Note that if you want to print to a file rather than to the screen use the plotting c
# ommands above but with these extra couple of lines of script (in bold):
colourCount = length(unique(SumCatchSp$Species)).     
png('Total_Catch_Per_SpeciesGroup.png', 1200, 800)
ggplot(data = SumCatchSp, aes(x = Year, y = TotalYieldSp, fill = Species)) + 
  geom_bar(colour = "black", stat="identity", size = 0.1) +
  scale_fill_manual(values = colourRampPalette(brewer.pal(12, "BrBG"))(colourCount)) + theme_bw() +
  labs(x="Years", y = "Tonnes") +  		 
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=20,face="bold"), 
        legend.title=element_text(size=18,face="bold"), legend.text = element_text(size=16)) 

dev.off()
