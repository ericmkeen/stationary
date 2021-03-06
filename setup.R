# Super quick load (no install)
library(devtools) ; document() ; load_all()

# Quick install
#library(devtools) ; remove.packages(hyfer) ; document() ; install() ; library(stationary)

# Full load and check
#library(devtools) ; document() ; load_all() ; check() ; install() ; library(stationary)

# Create package environment

#library(devtools)
getwd()
#setwd('../')
getwd()
create_package('/Users/erickeen/repos/stationary')

# Import packages
if(FALSE){
  use_package('magrittr')
  use_package('plyr')
  use_package('dplyr')
  use_package('readr')
  use_package('stringr')
  use_package('lubridate')
  use_package('usethis')
  use_package('devtools')
  use_package('shiny')
  use_package('shinyjs')
  use_package('shinydashboard')
  use_package('shinythemes')
  use_package('shinyWidgets')
  use_package('rintrojs')
  use_package('DataCombine')
  use_package('DT')
  use_package('wesanderson')
  use_package('bangarang')
  use_package('suncalc')
  use_package('truncnorm')
  use_package('plotrix')
  use_package('sp')
  use_package('sf')
  use_package('beepr')
}

#use_mit_license()

#### Try it out


# Install shipstrike
library(devtools)
devtools::install_github('ericmkeen/stationary')
library(soundcheck)













