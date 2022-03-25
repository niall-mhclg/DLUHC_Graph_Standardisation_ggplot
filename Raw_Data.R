#####Introduction ####
#Created by Niall McSharry, Analysis and Data Directorate, DLUHC, 25-03-2022

#Raw data for using to create and test new graph formats

#Most data collected and presented by DLUHC will be at either a regional or local authority level
#The example dataset used will be the social housing sales and demolitions open data

#This dataset will be linked directly, but this link may break in the future, so can be found at
# https://www.gov.uk/government/statistical-data-sets/live-tables-on-social-housing-sales#social-housing-sales-and-demolitions-open-data

#####Loading the required packages ####
library(tidyverse)

##### Reading in the raw data ####
RawData <- read.csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1050038/SHSD_Open_Data_1980_2021.csv") 
  
RawData_w_Date <- RawData %>%  mutate(Date = as.Date(paste0(as.numeric(substr(Year,1,4))+1,"-03-31")))

##### Creating summary dataframes for graphs to be developed from

### Time Series with one variable
# This example is Total demolitions of social housing stock in england by year
TimeSeries_1var <- RawData %>%
  filter(Disposal.Type == "Demolition") %>%
  select(Date,Units) %>%
  group_by(Date) %>%
  summarise(Demolitions = sum(Units,na.rm=T))

### Time Series with 2 variables
TimeSeries_2var <- RawData %>%
  filter(Disposal.Type=="Sale") %>%
  group_by(Year,Provider) %>%
  summarise(Sales = sum(Units,na.rm=T))

