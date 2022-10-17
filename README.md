Haitian Banking Trend and Performance
================
![GitHub top language](https://img.shields.io/github/languages/top/raulincadet/BanksHT_dashboard?style=plastic)
![GitHub repo size](https://img.shields.io/github/repo-size/raulincadet/BanksHT_dashboard?color=green)
![GitHub language count](https://img.shields.io/github/languages/count/raulincadet/BanksHT_dashboard?style=flat-square)
![Lines of code](https://img.shields.io/tokei/lines/github/raulincadet/BanksHT_dashboard?color=orange&style=plastic)
![dashboard_BKHT3](https://user-images.githubusercontent.com/11329136/191504232-5ae90511-3d48-4875-a1ba-60f1086ef1c6.png)

## About the project

This project builds a dashboard of the financial performance of the Haitian banks. Why is this project useful and challenging? Indeed, although the central bank provide data related to the financial statements of the commercial banks, they cannot be retrieve using an API. In addition, although they are provide in xlx and xlsx files on the website of the central bank, they are not structured as time series data that can be used immidiately. Since there is one excel file by quarterly, with many sheets, it is hard for most potential users of these data to gather them to verify the financial performance of each bank. This project relies on data collected and cleaned from [another project](https://github.com/raulincadet/BanksHT). Indeed, both projects are two parts of one project to gather data and build the dashboard.

The dashboard is not intended to be used as a tool for decision making, although the information extracted from raw financial data may relevent
to understand some aspects of the Haitian banking sector. Any suggestion to improve the dashboard is welcomed. Do not hesitate to send me an
[email](mailto:raulin.cadet@uniq.edu.ht). Click [here](https://cours.shinyapps.io/BanksHT_dashboard/) to navigate through the dashboard

## R packages required
The following packages are required to run the project:
* shiny
* shinydashboard
* ggplot2
* dplyr
* dygraphs
* forcats
* igraph
* googleVis
* htmltools

## Approach

The dashboard has been realized, using the packages Shiny and Shinydashboard of R programming language. The page Raw indicators of the dashboard can be used to observe changes in financial variables such as total assets, net banking income etc. This page presents also a graph of network similarity between the banks (including the banking system), for each variable selected by the user. The network is builded considering the significant Pearson correlation between each pair of banks (including the banking system). The threshold of 5% is used for the test of correlation. The user can select the banks he wants to consider. Data related to the whole banking system is considered, allowing to compare its behavior with the one of any bank.

## Features of the dashboard
The page Financial ratios presents some gauges related to some key financial ratios, by bank and by quarter. A user of the dashboard can
choose the quarter and the bank he wants to consider. In addition, this page allowd the user to choose a financial ratio and one or several
banks, to observe the trend of the selected indicator and its network of similarity.

To define the intervals of the gauges, the percentile approach is used. Since, three intervals should be defined, the following probabilities
are considered, to calculate the percentile: 0, 0.33, 0.67, 1. The outliers are removed before calculating the bounds of a gauge. To ensure
that each gauge accounts for the context of the Haitian banking sector, ratios related to agregate data of the banking system are used to define
their bounds. Ratios related to a bank are indicated in the gauges, whithout remove any outliers. Thus, the user of the dashboard can
observe when an entity outperforms or underperforms the banking industry.
