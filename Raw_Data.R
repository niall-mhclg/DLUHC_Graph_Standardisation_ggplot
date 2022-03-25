#####Introduction ####
#Created by Niall McSharry, Analysis and Data Directorate, DLUHC, 25-03-2022

#Raw data for using to create and test new graph formats

#Most data collected and presented by DLUHC will be at either a regional or local authority level
#The example dataset used will be the social housing sales and demolitions open data and the Affordable Housing Supply open data

#These datasets will be linked directly, but this link may break in the future, so can be found at
# https://www.gov.uk/government/statistical-data-sets/live-tables-on-social-housing-sales#social-housing-sales-and-demolitions-open-data
# 

#####Loading the required packages ####
library(tidyverse)

##### Reading in the raw data ####
Sales_Dems <- read.csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1050038/SHSD_Open_Data_1980_2021.csv") 
  
Sales_Dems_w_Date <- Sales_Dems %>%  mutate(Date = as.Date(paste0(as.numeric(substr(Year,1,4))+1,"-03-31")))

Supply <- read.csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1034092/AHS_199192_to_202021_open_data.csv")

##### Creating summary dataframes for graphs to be developed from

### Time Series with no variable
# This example is Total demolitions of social housing stock in england by year
TimeSeries_1var <- Sales_Dems_w_Date %>%
  filter(Disposal.Type == "Demolition") %>%
  group_by(Date) %>%
  summarise(Demolitions = sum(Units,na.rm=T))

### Time Series with 2 factors of one variable
TimeSeries_2x1var <- Sales_Dems_w_Date %>%
  filter(Disposal.Type=="Sale") %>%
  group_by(Date,Provider) %>%
  summarise(Sales = sum(Units,na.rm=T))

### Time Series with 3 factors of 1 variable
TimeSeries_3x1var <- Sales_Dems_w_Date %>%
  filter(Disposal.Type=="Sale") %>%
  group_by(Date,Disposal.Detail.Tier.1) %>%
  summarise(Sales = sum(Units,na.rm=T))

### Time Series with n factors of variable
#Set value for n
n = 10
#Select n different local authorities
LA_List_1 <- unique(Sales_Dems_w_Date$LA_NAME)[1:n]

TimeSeries_n_var <- Sales_Dems_w_Date %>%
  filter(Disposal.Type=="Sale"&LA_NAME %in% LA_List_1) %>%
  group_by(Date,LA_NAME) %>%
  summarise(Sales = sum(Units,na.rm=T))

  
### Discrete Data with 2 factors of 2 variables
Discrete_2x2var <- Sales_Dems_w_Date %>%
  filter(Year == "2020-21") %>%
  group_by(Provider,Disposal.Type) %>%
  summarise(Units = sum(Units,na.rm=T))

### Discrete Data with up to 4 factors of 3 variables
Discrete_4x3var <- Sales_Dems_w_Date %>%
  filter(Year == "2020-21") %>%
  group_by(Provider,LAD20TYPE,Disposal.Type) %>%
  summarise(Units = sum(Units,na.rm=T))


### Discrete Data with m factors of 2 variables
#Set value for m
m = 10

#Select m different local authorities
LA_List_2 <- unique(filter(Sales_Dems,Year =="2020-21")$LA_NAME)[1:m]

Discrete_n_var <- Sales_Dems_w_Date %>%
  filter(Year == "2020-21"&LA_NAME %in% LA_List_2) %>%
  group_by(Disposal.Detail.Tier.1,LA_NAME) %>%
  summarise(Sales = sum(Units,na.rm=T))

### Regional Time Series with no other variables
Reg_TimeSeries <- Sales_Dems_w_Date %>%
  filter(Disposal.Type=="Sale") %>%
  group_by(Date,RGN20NM) %>%
  summarise(Sales=sum(Units,na.rm=T))

### Regional Time Series with 2 factors of 1 variable
Reg_TimeSeries_2x1var <- Sales_Dems_w_Date %>%
  filter(Date > "2014-03-30"&Disposal.Type=="Demolition") %>%
  group_by(Date,RGN20NM,Provider) %>%
  summarise(Demolitions = sum(Units,na.rm=T))

### Regional Time Series with 3 factors of 1 variable
Reg_TimeSeries_3x1var <- Sales_Dems_w_Date %>%
  filter(Date > "2014-03-30"&Disposal.Type=="Sale") %>%
  group_by(Date,RGN20NM,Disposal.Detail.Tier.1) %>%
  summarise(Sales = sum(Units,na.rm=T))

### Regional Discrete data with 2 factors of 1 variable
Reg_Discrete_2x1var <- Sales_Dems_w_Date %>%
  filter(Year =="2020-21"&Disposal.Type=="Demolition") %>%
  group_by(RGN20NM,Provider) %>%
  summarise(Demolitions=sum(Units,na.rm=T))

### Regional Discrete data with 3 factors of 1 variable
Reg_Discrete_3x1var <- Sales_Dems_w_Date %>%
  filter(Year =="2020-21"&Disposal.Type=="Sale") %>%
  group_by(RGN20NM,Disposal.Detail.Tier.1) %>%
  summarise(Sales=sum(Units,na.rm=T))

### Scatter Graph
Scatter_xy <- Sales_Dems