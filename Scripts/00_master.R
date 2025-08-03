# 00_master

# load required packages
library(lubridate)
library(tidyverse)
library(here)
library(knitr)
library(codebook)
library(dagitty)
library(ggdag)
library(binom)
library(fastDummies)
library(car)
library(pscl)
library(segmented)

# source custom functions
functions <- list.files(
  here("Scripts", "functions"),
  full.names = TRUE
)


for(f in functions) {source(f)}

# run scripts in order
source(here("Scripts", "01_read_data.R")) # source data
source(here("Scripts", "02_clean_data.R")) # clean data
source(here("Scripts", "03_transform_data.R")) # transform data

