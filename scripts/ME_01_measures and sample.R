#-------------------------------------------------------------------------------
# MARRIAGE EXPECTATIONS PROJECT
# ME_01_measures and sample.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------

# Project Environment ----------------------------------------------------------
## If not starting from MA_00: setup the repository environment.
## If starting from GA_00: skip this section.

## Load the packages
library("pacman")                  # Load pacman package

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
  ggtext,     # color text labels
  ggpubr,     # stitch plots together
  conflicted) # choose default packages

## Address any conflicts in the packages
conflict_scout() # identify the conflicts
conflict_prefer("here", "here")
conflict_prefer("mutate", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("count", "dplyr")


## Specify the file paths
projDir <- here::here()                                                # File path to this project's directory
dataDir <- "./../../../Dropbox/Data/@Monitoring the Future/icpsr_data" # File path to where data will be downloaded
outDir  <- "docs"                                                      # Name of the sub-folder where we will save results


# DATA -------------------------------------------------------------------------
load(paste0(dataDir, "/mtf_form2.Rda"))

## Load the data and create a new df containing only the variables of interest.  
data <- select(mtf_V2, V5, ARCHIVE_WT, V1, V13, TABLET,     # Survey variables
               V2239,                                       # Project specific
               V2151, V2150, V2164,                         # Demographic (V2165 - momemp ! 2022)
               V2169, V2155, V2156)           

## Rename Variables
data <- dplyr::rename(data,      
                      wt7611 = V5,       wt1222 = ARCHIVE_WT,  getmar   = V2239,
                      raceeth  = V2151,  year   = V1,          region   = V13,
                      gender   = V2150,  momed  = V2164,       religion = V2169,
                      father = V2155,    mother   = V2156)

## Sample size
count(data)

## Create crosswalk of survey year and ICPSR Study ID 
studyid    <- c( 7927,  7928,  7929,  7930,
                 7900,  9013,  9045,  8387,  8388,
                 8546,  8701,  9079,  9259,  9397,
                 9745,  9871,  6133,  6367,  6517,
                 6716,  2268,  2477,  2751,  2939,
                 3184,  3425,  3753,  4019,  4264,
                 4536, 20022, 22480, 25382, 28401,
                30985, 34409, 34861, 35218, 36263,
                36408, 36798, 37182, 37416, 37841,
                38156, 38503, 38882)

surveyyear <- c(1976, 1977, 1978, 1979,
                1980, 1981, 1982, 1983, 1984,
                1985, 1986, 1987, 1988, 1989,
                1990, 1991, 1992, 1993, 1994,
                1995, 1996, 1997, 1998, 1999,
                2000, 2001, 2002, 2003, 2004,
                2005, 2006, 2007, 2008, 2009,
                2010, 2011, 2012, 2013, 2014,
                2015, 2016, 2017, 2018, 2019,
                2020, 2021, 2022)

Xwalk <- data.frame(surveyyear, studyid)

# VARIABLES --------------------------------------------------------------------

## Year
data$year <- as.character(data$year)
data$year[data$year == "76"] <- "1976"
data$year[is.na(data$year)]  <- "1978" # 34 people in 1978 have a missing year variable

data$year <- as.integer(data$year)

## Weights
colSums(!is.na(data))
table(data$year, !is.na(data$wt7611)) # 102810
table(data$year, !is.na(data$wt1222)) # 21861

data <- data %>%
  mutate(
    svyweight = case_when(
      year  <= 2011 ~ wt7611,
      year  >= 2012 ~ wt1222))

# https://stats.stackexchange.com/questions/553014/problem-with-weigts-in-survey-analysis-of-gss-cross-sectional-data
# data[ , 'svyweight_scaled'] <- data[ , svyweight] * 124671 /nrow(data) ## THIS DIDN'T CHANGE ANYTHING


## Categorical Variables

### creates a function so level order remains the same as argument order
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

data <- data %>%
  mutate(
  # Get married
    getmar = fct_case_when(
      getmar == 3 | getmar == "MARRY"    | getmar == "MARRY:(3)"    | getmar == "Getting married"     | getmar == "MARRIED:(3)"            ~ "GETTING MARRIED",
      getmar == 1 | getmar == "NT MARRY" | getmar == "NT MARRY:(1)" | getmar == "Not getting married" | getmar == "NOT MAR:(1)"            ~ "NOT GETTING MARRIED",
      getmar == 2 | getmar == "NO IDEA"  | getmar == "NO IDEA:(2)"  | getmar == "I have no idea"                                           ~ "I HAVE NO IDEA",
      getmar == 8                        | getmar == "MARRIED:(8)"                                    | getmar == "ALREADY MAR:(8)"        ~ "ALREADY MARRIED",
      getmar == 9 | getmar == "MISSING"  | getmar == "MISSING:(-9)" | getmar == "Missing"             | getmar == "R ASSIGNED ORANGE:(-8)" | 
      getmar == 0 ~ "MISSING"),
  # 3 category getmar
    mar3 = fct_case_when(
      getmar == "GETTING MARRIED" | getmar == "ALREADY MARRIED" ~ "Getting married",
      getmar == "NOT GETTING MARRIED"                           ~ "Not getting married",
      getmar == "I HAVE NO IDEA"                                ~ "I have no idea",
      TRUE                                                      ~  NA_character_ ),
  # getmar dummy
    mardum = fct_case_when(
      mar3     == "Getting married"      ~ 1,
      mar3     == "Not getting married"  |
      mar3     == "I have no idea"       ~ 0),
  # tablet -- only avail. for 2019
    tablet = fct_case_when(
      TABLET == "TABLET:(1)" & year == 2019 ~ "Tablet",
      TABLET == "PAPER:(0)"  & year == 2019 ~ "Paper",
      TRUE ~ NA_character_),
  # Gender
    sex = fct_case_when(
      gender == 1 | gender == "MALE"    | gender == "MALE:(1)"    | gender == "Male"           ~ "Men",
      gender == 2 | gender == "FEMALE"  | gender == "FEMALE:(2)"  | gender == "Female"         ~ "Women",
      TRUE ~ NA_character_),
  # Race
    race = fct_case_when(
      raceeth == 0 | raceeth == "WHITE"    | raceeth == "WHITE: (2)"    | raceeth == "White (Caucasian)"          | raceeth == "WHITE:(2)" ~ "White",
      raceeth == 1 | raceeth == "BLACK"    | raceeth == "BLACK: (1)"    | raceeth == "Black or African-American"  | raceeth == "BLACK:(1)" ~ "Black",
      TRUE ~ NA_character_),
  # Racesex
    racesex = fct_case_when(
      race == "White" & sex == "Man"   ~ "White men",
      race == "White" & sex == "Woman" ~ "White women",
      race == "Black" & sex == "Man"   ~ "Black men",
      race == "Black" & sex == "Woman" ~ "Black women",
      TRUE                                ~  NA_character_),
  # Mothers' Education
    momed = fct_case_when(
      momed == "1" | momed == "GRDE SCH" | momed == "GRDE SCH:(1)" | momed == "Completed grade school or less"     |
      momed == "2" | momed == "SOME HS"  | momed == "SOME HS:(2)"  | momed == "Some high school"                   |
      momed == "3" | momed == "HS GRAD"  | momed == "HS GRAD:(3)"  | momed == "Completed high school"              |
      momed == "4" | momed == "SOME CLG" | momed == "SOME CLG:(4)" | momed ==  "Some college"                      ~ "No college degree",
      momed == "5" | momed == "CLG GRAD" | momed == "CLG GRAD:(5)" | momed ==  "Completed college"                 |
      momed == "6" | momed == "GRAD SCH" | momed == "GRAD SCH:(6)" | momed ==  "Graduate or professional school"   ~ "Completed college",
      momed == "7" | momed == "MISSING"  | momed == "DK:(7)"       | momed ==  "Don't know, or does not apply"     | # These don't match but make missing so doesn't matter
      TRUE                                                                                                         ~  NA_character_ ),
  # Religiosity
    religion = fct_case_when(
      religion == 1 | religion == "NEVER"    | religion == "NEVER:(1)"    | religion == "Never"                     ~ "Never",
      religion == 2 | religion == "RARELY"   | religion == "RARELY:(2)"   | religion == "Rarely"                    ~ "Rarely",
      religion == 3 | religion == "1-2X/MO"  | religion == "1-2X/MO:(3)"  | religion == "Once or twice a month"     ~ "Once or twice a month",
      religion == 4 | religion == "1/WK OR+" | religion == "1/WK OR+:(4)" | religion == "About once a week or more" ~ "About once a week or more",
      TRUE        ~  NA_character_ ),
  # Family Structure
    mother = fct_case_when(
      mother == 1 | mother == "MARKED"   | mother == "MARKED:(1)"   | mother == "Yes" ~ "YES",
      mother == 0 | mother == "NT MARKD" | mother == "NT MARKD:(0)" | mother == "No"  ~ "NO",
      TRUE        ~ NA_character_),
    father = fct_case_when(
      father == 1 | father == "MARKED"   | father == "MARKED:(1)"   | father == "Yes" ~ "YES",
      father == 0 | father == "NT MARKD" | father == "NT MARKD:(0)" | father == "No"  ~ "NO",
      TRUE        ~ NA_character_),
    famstru = fct_case_when(
      mother == "YES" & father == "YES" ~ "Both Mother & Father",
      mother == "YES" & father == "NO"  ~ "Mother Only",
      mother == "NO"  & father == "YES" ~ "Father Only",
      mother == "NO"  & father == "NO"  ~ "Neither Mother/Father",
      TRUE                              ~  NA_character_),
  # Region
    region = fct_case_when(
      region == 1 | region == "NE" | region == "NE:(1)"   | region == "NORTHEAST"     | region == "NORTHEAST:(1)" ~ "Northeast",
      region == 2 | region == "NC" | region == "NC:(2)"   | region == "NORTH CENTRAL" | region == "MIDWEST:(2)"   |
      region == "NORTH CENTRL:(2)" | region == "NORTH CENTRAL:(2)"                                                ~ "Midwest",
      region == 3 | region == "S"  | region == "S:(3)"    | region == "SOUTH"         | region == "SOUTH:(3)"     ~ "South",
      region == 4 | region == "W"  | region == "W:(4)"    | region == "WEST"          | region == "WEST:(4)"      ~ "West")) %>%
  select(svyweight, year, mar3, mardum,  
         sex, race, racesex, momed, famstru, religion, region, tablet) %>%
  drop_na(mar3) # exclude cases missing on DV
  
## New Sample size
count(data)

glimpse(data)

# Sample -----------------------------------------------------------------------

## Missing data  
colSums(is.na(data))

## Create survey data 
mtf_svy <- data %>%
  drop_na(mar3) %>% # exclude cases missing on DV
  as_survey_design(id = 1,
                   weights = svyweight)

## Create table
tabA <- mtf_svy %>%
  select(c(-svyweight, -year, -mardum, -racesex)) %>%
  tbl_svysummary(
    label = list(mar3     ~ "Marriage expectations",
                 sex      ~ "Gender",
                 race     ~ "Race",
                 momed    ~ "Mothers' education",
                 famstru  ~ "Family structure when growing up",
                 religion ~ "Religiosity",
                 region   ~ "Region",
                 tablet   ~ "Survey mode (2019)"))  %>%
  modify_header(
    label = '**Variable**',
    stat_0 = '**N (unweighted) = 118966**') %>%
  modify_caption("Summary statistics of the pooled (weighted) analytic sample") %>%
  as_flex_table() 

save_as_docx(tabA, path = file.path(outDir, "tabA.docx"))
  
tabA # show table
