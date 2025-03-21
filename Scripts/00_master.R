# 00_master

# load required packages
library(lubridate)
library(tidyverse)
library(here)
library(knitr)
library(GGally)

# run scripts in order
source("Scripts/01_read_data.R") # source data
source("Scripts/02_clean_data.R") # clean data

