# DLUHC_Graph_Standardisation_ggplot
 
A set of functions, which may lead to the creation of a package to create standard graphs in ggplot2 for DLUHC publications 

This project was initially started by 
Niall McSharry, 
Data Analysis and Statistics Division,
Analysis and Data Directorate, 
Department for Levelling Up, Housing and Communities 
on 25 March 2022

This project builds on work done on previous statistical releases in the department, 
notably those by [Anthony Ash](https://www.gov.uk/government/statistics/rough-sleeping-snapshot-in-england-autumn-2020/rough-sleeping-snapshot-in-england-autumn-2020)
and [Niall McSharry](https://www.gov.uk/government/statistics/help-to-buy-equity-loan-scheme-data-to-30-september-2021/help-to-buy-equity-loan-scheme-data-to-30-september-2021)

The aim of the project is to create a reuseable code for creating consistently formatted graphs with the ggplot2 package within R, for use in official statistics publications


The R code within this folder has multiple different scripts

1. Raw_Data.R is a script which creates a series of raw data sets which can be used for creating graphs from based on the groupings and types of data.
The script creates a dataframes which are either Discrete, Conitnuous or time series

2. Line_Graphs.R contains functions which can be used for creating line graphs with one or more variables

3. Bar_Charts.R contains functions which create charts for discrete variables

4. Scatter_Plots.R contains functions for creating simple xy plots, or bubble charts

5. Interactive_Scatter_Plots.R contains functions for creating interactive graphs to use in HTMLs or dashboards internally


