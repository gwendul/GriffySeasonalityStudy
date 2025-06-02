# Data Repository for Soil Micro and Macro Fauna at Griffy Woods IURTP
Scripts, Workflows, and Data associated with the manuscript:

_Seasonal variation of micro and macro soil invertebrate communities in a temperate forest_, in review

## Authors
Kieran C Ball

Gwendolyn Lloyd (corresponding author gwlloyd@iu.edu)

Jaeyoung Yoo

Olivia C Hurley

Leah Hagemeier

André LC Franco


## Abstract

The biotic and abiotic factors driving seasonal changes in soil fauna are not well understood. In an end moraine forest ecosystem of southern Indiana, we studied the seasonal variation of both micro and macro fauna communities, as well as how topographic variability, tree functional type, and ecological group impact the seasonality of soil fauna. Bi-weekly for a 9-month period, we collected soil and litter samples by tree species which associate with arbuscular mycorrhizae (AM) or ectomycorrhiza (ECM) on uphill and downhill positions. We found that nematode abundance was highest in early winter, while macrofauna abundance was highest in fall, which directly conflicts with traditional collection times for these taxa. Abundance and diversity of different functional groups of nematodes and macrofauna responded variably to seasonality, reflecting differences in the timing of resource availability and acquisition. While season alone accounted for most of the variation in soil fauna abundance and diversity, the seasonality of community composition was dependent on topographic position and habitat type. Bacterial-feeding nematodes were strongly associated with spring and summer, and downhill positions, while predator and plant-parasitic nematodes were both closely associated to fall and winter, and uphill positions. Macrofauna communities were highly partitioned between endogeic and epigeic ecological groups, with endogeic communities appearing far more stable over seasonal changes. Although tree functional type did not affect the seasonality of soil fauna, AM-associated trees in the uphill position had greatest nematode abundance. Therefore, topographic variability and habitat type affect the seasonality of soil fauna community composition in temperate forests.

## Research Questions

- Understand the patterns of micro- and macro- fauna abundance, diversity, and community composition between seasons by sampling across an entire year
- Understand how seasonal patterns of micro- and macro- fauna are influenced by topography, tree functional type, and habitat type.

## Build Notes
All analyses were conducted using R version 4.4.1 and RStudio "Race for Your Life" using Windows

## Required Packages  
- ggplot2  
- nlme  
- emmeans  
- glmmTMB  
- piecewiseSEM  
- car  
- vegan  
- lme4  
- lmerTest  
- MASS  
- piecewiseSEM  
- AICcmodavg  
- dplyr  
- ggrepel  
- patchwork  
- ggeffects  

## Repository Contents
_DataAnalysis_SOP.docx_ : Workflow for Model Selection

_GW_Nematode.csv_ : Datatable containing all Nematode Data

_GW_SoilMacrofauna.csv_ : Datatable containing all Macrofauna Data

_GriffyLakeAnalyses_final.Rmd_ : R Markdown file of Final Models and Figures

_ManuscriptFigures.R_ : Code for Figures

## View R Markdown File Here:

https://gwendul.github.io/GriffySeasonalityStudy/
