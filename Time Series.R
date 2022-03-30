# R Code for creating a consistent DLUHC style for Line Graphs in ggplot2
#Ensure that you have run the "Raw_Data.R" script before running this script so that you have the raw data loaded

#Read in the required packages
library(tidyverse)
library(reshape2)
library(lubridate)
library(RColorBrewer)
library(janitor)
library(scales)
library(gghighlight)
library(ggrepel)

#Load the GovUK colour set
GovUK <- c("red"="#d4351c"
           ,"yellow"="#ffdd00"
           ,"green"="#00703c"
           ,"blue"="#1D70B8"
           ,"dark-blue"="#003078"
           ,"light-blue"="#5694CA"
           ,"purple"="#4c2c92"
           ,"black"="#0b0c0c"
           ,"dark-grey"="#505a5f"
           ,"mid-grey"="#b1b4b6"
           ,"light-grey"="#f3f2f1"
           ,"white"="#ffffff"
           ,"light-purple"="#6f72af"
           ,"bright-purple"="#912b88"
           ,"pink"="#d53880"
           ,"light-pink"="#f499be"
           ,"orange"="#f47738"
           ,"brown"="#b58840"
           ,"light-green"="#85994b"
           ,"turquoise"="#28a197"
           ,"DLUHC" = "#012169")


# Time Series with 1 variable
Time_Series <- function(.data,xcol,ycol,groupcol=NULL){
  ymax <- .data %>% select({{ycol}},{{xcol}}) %>%
    summarise(x = max({{ycol}})) %>%  pull()
  ymax = max(ymax)
  graph <- ggplot(data = .data,aes(x={{xcol}}, y={{ycol}})) +
    geom_line(show.legend = TRUE , size = 1.5,color = unname(GovUK[5])) +
    scale_colour_manual(values = unname(GovUK["DLUHC"]))+
    theme_classic(base_size = 18) +
    scale_y_continuous(limits = c(0,ymax*1.1),expand = c(0,0))
  graph
}

Time_Series(TimeSeries_1var,xcol = Date,ycol = Demolitions)

#Function to create nudges for labels
Label_nudges = function(.data,xcol,ycol){
  ymax <- .data %>% select({{ycol}},{{xcol}}) %>%
    summarise(x = max({{ycol}})) %>%  pull()
  ymax = max(ymax)
  numchar = nchar(ymax)
  xmax <- .data %>% select({{ycol}},{{xcol}}) %>%
    summarise(x = max({{xcol}})) %>%  pull()
  xmin <- .data %>% select({{ycol}},{{xcol}}) %>%
    summarise(x = min({{xcol}})) %>%  pull()
  xrange = as.numeric(xmax-xmin)
  .data <- .data  %>%
    arrange({{xcol}}) %>%
    mutate(changebefore = {{ycol}}-lag({{ycol}}),
           changeafter = lead({{ycol}})-{{ycol}},
           change = case_when(
             (is.na(changebefore)&changeafter>0)|(is.na(changeafter)&changebefore<0) ~ "Trough",
             (is.na(changebefore)&changeafter<0)|(is.na(changeafter)&changebefore>0) ~ "Peak",
             changebefore>0&changeafter>0&changebefore>changeafter ~ "Dec_Growth",
             changebefore>0&changeafter>0&changebefore<=changeafter ~ "Inc_Growth",
             changebefore<=0&changeafter<=0&changebefore<=changeafter ~ "Dec_Fall",
             changebefore<=0&changeafter<=0&changebefore>changeafter ~ "Inc_Fall",
             changebefore==0&changeafter==0 ~ "No_Change",
             changebefore<=0&changeafter>0 ~ "Trough",
             TRUE ~ "Peak")) %>%
    mutate(y_nudge = case_when(
      change =="Dec_Growth" ~ 0.01*ymax,
      change =="Inc_Growth" ~ -0.01*ymax,
      change =="Dec_Fall" ~ -0.01*ymax,
      change == "Inc_Fall"~ 0.01*ymax,
      change == "No_Change"~ 0.01*ymax,
      change == "Trough"~ -0.03*ymax,
      TRUE ~ 0.03*ymax)) %>%
    mutate(x_nudge = case_when(
      change =="Dec_Growth" ~ -0.007*numchar*xrange,
      change =="Inc_Growth" ~ 0.007*numchar*xrange,
      change =="Dec_Fall" ~ -0.007*numchar*xrange,
      change == "Inc_Fall"~ 0.007*numchar*xrange,
      change == "No_Change"~ 0.007*numchar*xrange,
      change == "Trough"~ -0,
      TRUE ~ 0))
  return(.data)
}

Label_nudges(TimeSeries_1var,Date,Demolitions)

#Time series with value labels for 1 variable
Time_Series_w_labels <- function(.data,xcol,ycol,FUN=comma,xnudge=1,ynudge=1,textsize=4){
  .data <- Label_nudges(.data,{{xcol}},{{ycol}})
  ymax <- .data %>% select({{ycol}},{{xcol}}) %>%
    summarise(x = max({{ycol}})) %>%  pull()
  ymax = max(ymax)
  graph <- ggplot(data = .data,aes(x={{xcol}}, y={{ycol}},label = FUN({{ycol}},accuracy=1))) +
    geom_line(show.legend = TRUE , size = 1.5,color = unname(GovUK["DLUHC"])) +
    geom_point(size = 3,colour = unname(GovUK["DLUHC"])) +
    geom_text(size=textsize,fontface ="bold", colour = unname(GovUK["DLUHC"]), key_glyph = "rect",nudge_x = xnudge*.data$x_nudge,nudge_y = ynudge*.data$y_nudge,na.rm = T)+
    scale_colour_manual(values = unname(GovUK["DLUHC"]))+
    theme_classic(base_size = 18) +
    scale_y_continuous(limits = c(0,ymax*1.1))
  graph
}

Time_Series_w_labels(TimeSeries_1var,xcol = Date,ycol = Demolitions,xnudge = 0.7,ynudge=0.7,textsize = 3)

TimeSeries_1var_perc <-mutate(TimeSeries_1var,Demolitions = Demolitions/max(Demolitions)) 
Time_Series_w_labels(TimeSeries_1var_perc,Date,Demolitions,FUN=percent,xnudge = 4,ynudge=1.5,textsize = 5)




# Time Series with 2+ variable
Time_Series_vars <- function(.data,xcol,ycol,groupcol){
  ymax <- .data %>% select({{ycol}},{{xcol}}) %>%
    summarise(x = max({{ycol}})) %>%  pull()
  ymax = max(ymax)
  group_count <- nrow(.data %>% ungroup() %>% select({{groupcol}}) %>% distinct({{groupcol}},.keep_all = FALSE))
  
  graph <- ggplot(data = .data,aes(x={{xcol}}, y={{ycol}},group={{groupcol}})) +
    geom_line(show.legend = TRUE , size = 1.5,aes(color = {{groupcol}})) +
    scale_colour_manual(values = 
                          if(group_count<4){unname(GovUK[c("blue","dark-blue","light-blue")])}
                        else if(group_count<10){unname(GovUK[c("red","green","blue","purple","mid-grey","pink","orange","brown","turquoise","yellow","dark-blue")])}
                        else{unname(GovUK)})+
    theme_classic(base_size = 18)+
    scale_y_continuous(limits = c(0,ymax*1.1),expand=c(0,0),labels=comma) +
    scale_x_date(expand=c(0,0))
  graph
}

Time_Series_vars(TimeSeries_3x1var,Date,Sales,variable)

Time_Series_vars(filter(TimeSeries_3x1var,Date>"2000-01-01"),Date,Sales,variable) +
  theme(legend.position = c(0.7,0.8))

Time_Series_vars(TimeSeries_n_var,Date,Sales,LA_NAME)


#Regional Time Series
Region_Time_Series_highlight = function(.data,xcol,ycol,RegionCol){
  .data <- .data %>% rename("RegionName"={{RegionCol}}) %>% filter(!is.na(RegionName)&RegionName!="")
  graph <- ggplot(.data, mapping=aes(x={{xcol}}, y={{ycol}})) +  
  geom_line(show.legend = FALSE, col = GovUK["DLUHC"] , size = 2) +
  geom_line(aes(x={{xcol}},y={{ycol}}, colour=RegionName), colour = GovUK["DLUHC"])+
  facet_wrap(~ RegionName, scales='free',strip.position="top") +
  gghighlight(use_direct_label = FALSE) +
  theme_classic(base_size = 18)+
  theme(strip.background = element_blank(), axis.text.x = element_text(angle=0, vjust = 0.4,size=10),axis.title = element_text(), axis.line  = element_line(), axis.ticks = element_blank())
  graph
}

Region_Time_Series_highlight(Reg_TimeSeries,Date,Sales,RGN20NM)

#Regional Time Series multi variables
Region_Time_Series_n_var = function(.data,xcol,ycol,groupcol,RegionCol,linesize=1.5){
  group_count <- nrow(.data %>% ungroup() %>% select({{groupcol}}) %>% distinct({{groupcol}},.keep_all = FALSE))
  .data %>% 
    rename("RegionName"={{RegionCol}}) %>% 
    filter(!is.na(RegionName)&RegionName!="")%>%
    ggplot(aes(x={{xcol}},y={{ycol}},group = {{groupcol}},colour={{groupcol}})) +
    scale_colour_manual(values = 
                          if(group_count<4){unname(GovUK[c("blue","dark-blue","light-blue")])}
                        else if(group_count<10){unname(GovUK[c("red","green","blue","purple","mid-grey","pink","orange","brown","turquoise","yellow","dark-blue")])}
                        else{unname(GovUK)}) +
    geom_line(size = linesize) +
    facet_wrap(~RegionName,scales="free") +    
    theme_classic(base_size = 12)+
    theme(legend.position = "top",strip.background = element_blank(), axis.text.x = element_text(angle=0, vjust = 0.4),axis.title = element_text(), axis.line  = element_line(), axis.ticks = element_blank())
    
}

Region_Time_Series_n_var(Reg_TimeSeries_2x1var,Date,Demolitions,Provider,RGN20NM)

Region_Time_Series_n_var(Reg_TimeSeries_3x1var,Date,Sales,Disposal.Detail.Tier.1,RGN20NM)
