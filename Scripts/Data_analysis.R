#############################
### Harry Zhang, UBC Okanagan
### 20230922
### Course Project - BIOL 520I -
### Productivity and Reproducibility
### [Data Analysis]
#############################
## This script is a component of a mock project
## for BIOL 520I 2023W1 to demonstrate research
## workflows to facilitate computational
## reproducibility using various tools.
#############################
## This script acomplishes the following:
## - Load in data from source package
## - Short exploration of selected variables from the data
## - Visually represent some interesting findings
## - Save resultant tables and figures as the process goes
#############################
## The data from package 'palmerpenguins' is
## used for this demonstration.
## Data credits:
## Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer Archipelago (Antarctica) penguin
## data. R package version 0.1.0. https://allisonhorst.github.io/palmerpenguins/. doi:
## 10.5281/zenodo.3960218.
## For more information about the dataset, see:
## https://allisonhorst.github.io/palmerpenguins/articles/intro.html


############## START OF SCRIPT ##############

###
## Load packages
library(tidyverse)
library(ggplot2)
library(palmerpenguins)


###
## Check work directory.
getwd()
## Verify subdirectories.
list.dirs(full.names = T, recursive = F)
# [1] "./.git"          "./.Rproj.user"   "./Data_1_Raw"    "./Data_2_Work"   "./Data_3_Out"
# [6] "./Figures" "./Manuscripts"   "./Scripts"

## Read in data from package
data <- penguins
## Save a copy of the untouched data as CSV
write.csv(data, paste("./Data_1_Raw", "penguins.csv", sep = '/'), row.names = F)


###
## Get to know the data. Check rows and columns, etc.
names(data)
head(data)
dim(data)

## Explore the data
# How many species are there?
data %>% count(species)

# Trim columns. Choose bill characteristics for a closer look.
data <- data %>% 
    select(species, bill_length_mm, bill_depth_mm) %>% 
    # Drop rows with missing data for simplicity
    filter(complete.cases(.))

# How different are the bill dimensions between species?
data %>% 
    group_by(species) %>% 
    # Only looking at the mean values for a first impression
    summarize(bill_l_mean=mean(bill_length_mm), bill_d_mean=mean(bill_depth_mm)) %>% 
    # Optionally, save a copy to the working data folder
    write.csv(paste("./Data_2_Work", "bill_dim_means_by_species.csv", sep = '/'), row.names=F)

# Alternatively, a more comprehensive comparison of bill length.
data %>% 
    # Check several stats
    summarize(min=min(bill_length_mm), 
              mean=mean(bill_length_mm), 
              median=median(bill_length_mm), 
              max=max(bill_length_mm),
              .by=species) %>% 
    # Optionally, save a copy to the output data folder
    write.csv(paste("./Data_3_Out", "bill_len_by_species.csv", sep = '/'), row.names=F)

# Some simple boxplots to visualize the difference.
data %>%
    # bill length by species
    boxplot(bill_length_mm ~ species, ., ylab="bill length (mm)")
data %>% 
    # bill depth by species
    boxplot(bill_depth_mm ~ species, ., ylab="bill depth (mm)")

# A histogram of bill length for species "Adelie"
(data %>% filter(species=='Adelie'))$bill_length_mm %>% 
    hist(breaks = 7, main="Bill Length (mm)", xlab="Species: Adelie")

# A scatter plot of bill length vs depth
# The result suggests positive correlations and clustering by species.
data %>% 
    ggplot(mapping=aes(x=bill_length_mm, y=bill_depth_mm)) + 
    labs(title="Penguins Bill Length - Bill Depth by Species",
         x = "Bill Length (mm)",
         y = "Bill Depth (mm)") +
    geom_point(aes(color=species), size=2)
# Save a copy to drive for future use
ggsave("scatterplot_billLenDep.jpg", path = "./Figures")


############## END OF SCRIPT ##############