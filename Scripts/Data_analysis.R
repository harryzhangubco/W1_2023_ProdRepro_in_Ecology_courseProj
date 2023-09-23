#############################
### Harry Zhang, UBC Okanagan
### 20230922
### Course Project - BIOL 520I -
### Productivity and Reproducibility
#############################
## This script is a component of a mock project
## for BIOL 520I 2023W1 to demonstrate research
## workflows to facilitate computational
## reproducibility using various tools.
#############################
## This script acomplishes the following:
## - 
## - 
## - 
## -
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

## Load packages
library(tidyverse)
library(ggplot2)
library(palmerpenguins)

## Check work directory.
getwd()
## Verify subdirectories.
list.dirs(full.names = T, recursive = F)
# [1] "./.git"          "./.Rproj.user"   "./Data_1_Raw"    "./Data_2_Work"   "./Data_3_Out"
# [6] "./FiguresTables" "./Manuscripts"   "./Scripts"


data <- penguins
names(data)
head(data)

data %>% count(species)

data <- data %>% 
    select(species, bill_length_mm, bill_depth_mm) %>% 
    filter(complete.cases(.))

data %>% 
    group_by(species) %>% summarize(bill_l_mean=mean(bill_length_mm), bill_d_mean=mean(bill_depth_mm))

data %>% 
    summarize(min=min(bill_length_mm), 
              mean=mean(bill_length_mm), 
              median=median(bill_length_mm), 
              max=max(bill_length_mm),
              .by=species)

data %>% 
    boxplot(bill_length_mm ~ species, ., ylab="bill length (mm)")
data %>% 
    boxplot(bill_depth_mm ~ species, ., ylab="bill depth (mm)")

(data %>% filter(species=='Adelie'))$bill_length_mm %>% hist()

data %>% 
    ggplot(mapping=aes(x=bill_length_mm, y=bill_depth_mm)) + 
    labs(title="Penguins Bill Length - Bill Depth by Species",
         x = "Bill Length (mm)",
         y = "Bill Depth (mm)") +
    geom_point(aes(color=species), size=2) %>% 
    save


############## END OF SCRIPT ##############