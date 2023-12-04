#-------------------------------------------------------------------------------
# MARRIAGE EXPECTATIONS PROJECT
# ME_00-1_setup and packages.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------


# PACKAGES ---------------------------------------------------------------------

## WARNING: Remove the leading # to install packages below the first time. 
## Change filepaths below once

# if (!require(remotes)) install.packages("remotes") 
# if (!require(devtools)) install.packages("devtools") 
# remotes::install_github("fsolt/icpsrdata")                      # download ICPSR data
# install.packages("pacman")                                      # Install pacman package if not installed

# Installs and loads packages automatically
library("pacman")                  # Load pacman package

# Install packages not yet installed & load them
pacman::p_load(
  here,       # relative file paths
  foreign,    # read data
  plyr,       #
  dplyr,      # variable processing
  tidyr,      # reshaping data
  forcats,    # reverse factor variables
  srvyr,      # calc % with survey weights
  scales,     # % in ggplots
  purrr,      # to use modify_at
  MESS,       # round prop & preserve sum to 100%
  data.table, #
  survey,     # create weighted dataset for tables
  gtsummary,  # pretty weighted tables
  flextable,  # displaying pretty tables
  ggplot2,    # graphing
  colorspace, # color palettes
  ggtext,     # color text labels and hyperlinks
  ggpubr,     # stitch plots together
  conflicted) # choose default packages


# https://cran.r-project.org/web/packages/icpsrdata/vignettes/icpsrdata-vignette.html
library("icpsrdata")

# Address any conflicts in the packages
conflict_scout() # identify the conflicts
conflict_prefer("here", "here")
conflict_prefer("mutate", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("count", "dplyr")

# Set-up the Directories -------------------------------------------------------

## Set the project directory to the current working directory.
projDir <- here::here()                                                # File path to this project's directory
dataDir <- "./../../../Dropbox/Data/@Monitoring the Future/icpsr_data" # File path to where data will be downloaded
outDir  <- "docs"                                                      # Name of the sub-folder where we will save results

## This will create sub-directory folders in the master project directory if doesn't exist
if (!dir.exists(here::here(outDir))){
  dir.create(outDir)
} else {
  print("Output directory already exists!")
}

# Download the data ------------------------------------------------------------
## Users should remove the # before the source() line to run the data download.
## This script only needs to be run the first time and if there is a new wave of data.

# source(ME_00-2_data download.R)

message("End of MA_00-1_setup and packages") # Marks end of R Script
