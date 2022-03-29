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
  graph <- ggplot(data = .data,aes(x={{xcol}}, y={{ycol}})) +
    geom_line(show.legend = TRUE , size = 1.5,color = unname(GovUK[5])) +
    scale_colour_manual(values = unname(GovUK["DLUHC"]))+
    theme_classic(base_size = 18)
  graph
}

Time_Series(TimeSeries_1var,xcol = Date,ycol = Demolitions)

#Time series with values for 1 variable
Time_Series_labels <- function(.data,xcol,ycol){
  ymax <- .data %>% select({{ycol}},{{xcol}}) %>%
    summarise(x = max({{ycol}})) %>%  pull()
  ymax = max(ymax)
  .data <- .data  %>%
    arrange({{xcol}}) %>%
    mutate(changebefore = replace_na({{ycol}}-lag({{ycol}}),0),
           changeafter = replace_na(lead({{ycol}})-{{ycol}},0),
           change = case_when(
             changebefore>0&changeafter>0&changebefore>changeafter ~ "Dec_Growth",
             changebefore>0&changeafter>0&changebefore<=changeafter ~ "Inc_Growth",
             changebefore<=0&changeafter<=0&changebefore<=changeafter ~ "Dec_Fall",
             changebefore<=0&changeafter<=0&changebefore>changeafter ~ "Inc_Fall",
             changebefore==0&changeafter==0 ~ "No_Change",
             changebefore<=0&changeafter>0 ~ "Trough",
             TRUE ~ "Peak")) %>%
    mutate(y_nudge = case_when(
      change =="Dec_Growth" ~ 0.05*ymax,
      change =="Inc_Growth" ~ -0.01*ymax,
      change =="Dec_Fall" ~ -0.01*ymax,
      change == "Inc_Fall"~ 0.05*ymax,
      change == "No_Change"~ 0.05*ymax,
      change == "Trough"~ -0.05*ymax,
      TRUE ~ 0.05*ymax
    )) %>%
    mutate(x_nudge = case_when(
      change =="Dec_Growth" ~ 0.05*xrange,
      change =="Inc_Growth" ~ -0.01*x,
      change =="Dec_Fall" ~ -0.01*ymax,
      change == "Inc_Fall"~ 0.05*ymax,
      change == "No_Change"~ 0.05*ymax,
      change == "Trough"~ -0.05*ymax,
      TRUE ~ 0.05*ymax
    ))
  .data}
  graph <- ggplot(data = .data,aes(x={{xcol}}, y={{ycol}},label = {{ycol}})) +
    geom_line(show.legend = TRUE , size = 1.5,color = unname(GovUK[5])) +
    geom_point(size = 3) +
    geom_text(size=6.8,fontface ="bold", key_glyph = "rect")+
    scale_colour_manual(values = unname(GovUK["DLUHC"]))+
    theme_classic(base_size = 18) 
  graph
}

Time_Series_labels(TimeSeries_1var,xcol = Date,ycol = Demolitions)



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


