
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LA\_covid

<!-- badges: start -->

<!-- badges: end -->

An R project for scraping COVID-19 case reports from the
<a href="http://www.publichealth.lacounty.gov/media/Coronavirus/">Los
Angeles Department of Public Health</a>.

To download, scrape, and analyze the data run the “LA\_covid.R” script
in the code folder:

    source("LA_covid.R") 

## Shiny App

The data here can be displayed by a shiny R app:
<a href= https://beautiful.shinyapps.io/COVID19-in-Los-Angeles/>“<https://beautiful.shinyapps.io/COVID19-in-Los-Angeles/>”</a>

Code to run the app is available in the “app/” folder in this project.

### Built With

platform x86\_64-apple-darwin15.6.0  
arch x86\_64  
os darwin15.6.0  
system x86\_64, darwin15.6.0  
status  
major 3  
minor 6.1  
year 2019  
month 07  
day 05  
svn rev 76782  
language R  
version.string R version 3.6.1 (2019-07-05) nickname Action of the Toes

### Required R packages

1.  tidyverse\_1.3.0
2.  lubridate\_1.7.4  
3.  stringr\_1.4.0  
4.  sf\_0.8.0
5.  rvest\_0.3.5
6.  shiny\_1.4.0
7.  leaflet\_2.0.3
8.  plotly\_4.9.2
9.  viridis\_0.5.1
10. htmlwidgets\_1.5.1
11. RColorBrewer\_1.1.2

### Authors

Andrew Kleinhesselink and Jane Li.

# Detailed information about data sources:

Data on reported cases are taken from the
<a href="http://www.publichealth.lacounty.gov/media/Coronavirus/">Los
Angeles Department of Public Health</a>. Many new cases are reported
daily, absence of reported cases in an area does not mean there are no
cases in that area. Reported cases are subject to change based on
further investigations. Differences between areas in number of cases
reported may reflect differences in access to testing rather than actual
differences in coronavirus infection. “NA” indicates areas where no data
area available.

<P>

Population data and boundaries for each community are taken from the LA
Countywide Statistical Areas dataset available online:
<a href="https://egis3.lacounty.gov/dataportal/2017/11/02/board-approved-statistical-areas-communities-final-draft/">https://egis3.lacounty.gov/dataportal/2017/11/02/board-approved-statistical-areas-communities-final-draft/</a>.
Data and code to reproduce figures are available at
<a href="https://github.com/akleinhesselink/LA_covid">https://github.com/akleinhesselink/LA\_covid</a>.

<P>

Map and figures by Andy Kleinhesselink and Jane Li.

<P>

From Los Angeles Department of Public Health: "Always check with trusted
sources for the latest accurate information about novel coronavirus:

<ul>

<li>

Los Angeles County Department of Public Health
<a href="http://publichealth.lacounty.gov/media/Coronavirus/">
http://publichealth.lacounty.gov/media/Coronavirus/</a>  

<li>

California Department of Public Health
<a   href="https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/Immunization/ncov2019.aspx">https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/Immunization/ncov2019.aspx
</a>  

<li>

Centers for Disease Control and Prevention (CDC)
<a href="https://www.cdc.gov/coronavirus/2019-ncov/index.html">https://www.cdc.gov/coronavirus/2019-ncov/index.html</a>  
Spanish<a href=" https://www.cdc.gov/coronavirus/2019-ncov/index-sp.html">
https://www.cdc.gov/coronavirus/2019-ncov/index-sp.html </a>  

<li>

World Health Organization
<a href="https://www.who.int/health-topics/coronavirus">https://www.who.int/health-topics/coronavirus</a>

<li>

LA County residents can also call 2-1-1 "

</ul>
