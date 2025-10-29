library(readxl)
library(tidyverse)
library(ggplot2)
library(ggprism)

all_clin_data<- read.csv("master_table.csv")
dim(all_clin_data)

table(all_clin_data$Study.Type)
dat <- all_clin_data

dat <- all_clin_data %>% 
  filter(Study.Type == "INTERVENTIONAL")
dim(dat)

dat$CT_YEAR <- as.numeric(dat$CT_YEAR)

dat <- dat %>% 
  filter(CT_YEAR > 2004 & CT_YEAR < 2026 )
dim(dat)
#######################################