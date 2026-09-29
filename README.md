In this research, we examine the similarities and differences between the two types of kinship models: the matrix kinship models and the demographic microsimulation with SOCSIM
We run microsimulation and formal demographic models in stable (time-invariant) and dynamic (time-varying) settings, using age-specific fertility rates and age-specific probabilities of death from the World Population Prospects (WPP) 2024 Revision for Sweden over the period 1950–2100.
We compare the characteristics, assumptions, data requirements and outputs of the two models and time settings, focusing on the mean and distribution of the numbers and ages of different types of kin. 
We use the example of grandparents and grandchildren to illustrate differences in the averages and distributions, as well as the proportion of individuals who have no grandparents or grandchildren, or a large number of either.


---
title: "Code for reproducible results for the paper “Modelling Kinship Networks using Analytical and Microsimulation Approaches”"
output:
  github_document:
    pandoc_args: --webtex
    toc: true
    toc_depth: 1
---

```{r setup, include=FALSE}
# General set up, change if message and warnings should be included
knitr::opts_chunk$set(echo = FALSE, 
                      message = FALSE, 
                      warning = FALSE, 
                      fig.align='center')
```
# Introduction

We ran demographic microsimulation and formal demographic models using data for Sweden from the [World Population Prospects (WPP) 2024 Revision](https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=CSV%20format) (2024-2100) 

# Scripts for reproducible results

**NB**: If you want to re-run the analysis from scratch (i.e., retrieve the data from [WPP](https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=CSV%20format) run the analytical and microsimulation models and reproduce the results), you need R, some standard packages, `DemoKin` and `rsocsim`. 
Please, follow these steps:

1. Download this repository to your computer. 
2. Download and save WPP 2024 data as .csv using the function download_WPP2024(), provided in the script "Functions/Functions_Download_WPP2024.R"
3. Install `DemoKin` from GitHub. Please go to <https://github.com/IvanWilli/DemoKin> to find the instructions on how to install `DemoKin`. 
4. Install `rsocsim` from CRAN using install.packages("rsocsim") 
5. Open the provided RStudio project. 
6. Run the scripts sequentially in the order suggested by the numbers of the files (0-5). 
7. To get exactly the same simulation results, you should use the same randomly generated seed mentioned in script 0.