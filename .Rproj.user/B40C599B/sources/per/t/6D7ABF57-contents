#####################################################################################
# Set-up the environment

## Load the libraries
library("here")
library("haven")
library("tidyverse")
library("forcats")
library("psych")
library("tools")
library("survey")
library("srvyr")
library("directlabels")

## Set-up the Directories

repoDir     <- here()                                          # File path to your master project folder (Project GitRepository)
dataDir     <- "../../Data/GSS/GSS_7222"                       # File path to where the data was downloaded

srcDir      <- file.path(repoDir, "scripts")                   # File path to the R scripts
outDir      <- file.path(repoDir, "output")                    # File path to save table/processed data
figDir      <- file.path(repoDir, "figures")                   # File path to save figures


## This will create a data sub-directory folder in the master project directory if doesn't exist
if (!dir.exists(outDir)){
  dir.create(outDir)
} else {
  print("output directory already exists!")
}

## This will create a figures sub-directory folder in the master project directory if doesn't exist
if (!dir.exists(figDir)){
  dir.create(figDir)
} else {
  print("figure directory already exists!")
}


## Get data
# http://gss.norc.org/get-the-data/stata
# GSS 1972-2022 Cross-Sectional Cumulative Data (Release 1, May 2023)

# Import the downloaded data file.
data <- read_dta(file.path(dataDir,"gss7222_r1.dta"), 
                 col_select = c( 
                   # Survey variables
                        "year", "id", "wtssall", "wtssps", "ballot", "mode",
                        "vpsu", "vstrat", "oversamp", "formwt", "sampcode", "sample",
                   # Project specific
                        "fefam", "fechld", "fepresch", "fepol", "meovrwrk",
                   # Demographic
                        "age", "sex", "race")) 

#####################################################################################
## Modified code from https://kieranhealy.org/blog/archives/2019/03/22/a-quick-and-tidy-look-at-the-2018-gss/

cont_vars <- c("year", "id", "ballot", "age")

cat_vars <- c("race", "sex", "mode",  "fefam", "fechld", "fepresch", "fepol",  "meovrwrk")

wt_vars <- c("vpsu",
             "vstrat",
             "oversamp",
             "formwt",              # weight to deal with experimental randomization
             "wtssall",             # weight variable
             "wtssps",              # 2021 weight variable
             "sampcode",            # sampling error code
             "sample")              # sampling frame and method

vars <- c(cont_vars, cat_vars, wt_vars)


data <- data %>%
  modify_at(vars(), zap_missing) %>%
  modify_at(wt_vars, as.numeric) %>%
  modify_at(cat_vars, as_factor) %>%
  modify_at(cat_vars, fct_relabel, toTitleCase) 

# Define the recoding function
recode_function <- function(x) {
  # Add your recoding logic here
  # For example, recoding values greater than 5 to NA
  ifelse(x == "Strongly Agree"    | x == "Agree",    "Agree", 
         ifelse(x == "Strongly Disagree" | x == "Disagree", "Disagree", 
                NA_character_))
}

# Recode multiple variables using mutate_at()
data <- data %>%
  mutate_at(vars(fefam, fechld, fepresch, fepol, meovrwrk), recode_function)

# recode sex
data <- data %>%
  mutate(
    sex = case_when(
      sex == "Male"     ~ "Men",
      sex == "Female"   ~ "Women",
      TRUE              ~  NA_character_ 
    ))

## Now we need to take this data and use the survey variables in it, 
## so we can properly calculate population means and errors and so on. 
## We use svyr's wrappers to survey for this:
  
options(survey.lonely.psu = "adjust")
options(na.action="na.pass")

### combining survey weights...bad idea? probably...
data  <- data  %>%
  mutate(
    svyweight = case_when(
      year != 2021 & year != 2022  ~ wtssall,
      year == 2021 | year == 2022  ~ wtssps
    ))

# mothers' employment variables --------------------------------------------------

## create survey data
gss_svy <- data %>%
  filter(year > 1976) %>%
  drop_na(fefam) %>%
  drop_na(fechld) %>%
  drop_na(fepresch) %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(id = vpsu,
                   strata = stratvar,
                   weights = svyweight,
                   nest = TRUE)

gss_svy2 <- data %>%
  filter(year > 1977 & year < 2020) %>%
  drop_na(fepol) %>% # not asked after 2018
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(id = vpsu,
                   strata = stratvar,
                   weights = svyweight,
                   nest = TRUE)

## Create yearly averages
fefam_yr <- gss_svy %>%
  group_by(year, fefam) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fechld_yr <- gss_svy %>%
  group_by(year, fechld) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fepresch_yr <- gss_svy %>%
  group_by(year, fepresch) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fepol_yr <- gss_svy2 %>%
  group_by(year, fepol) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

### Combine the averages
fefam_yr$att     <- "fefam" 
fechld_yr$att    <- "fechld" 
fepresch_yr$att  <- "fepresch" 
fepol_yr$att     <- "fepol"

colnames(fefam_yr)[colnames(fefam_yr)=="fefam"]          <- "val"
colnames(fechld_yr)[colnames(fechld_yr)=="fechld"]       <- "val"
colnames(fepresch_yr)[colnames(fepresch_yr)=="fepresch"] <- "val"
colnames(fepol_yr)[colnames(fepol_yr)=="fepol"]          <- "val"


figdata <- rbind(fefam_yr, fechld_yr, fepresch_yr, fepol_yr)

figdata <- figdata %>%
  mutate(
    prog = case_when(
      val == "Disagree" & att != "fechld"    ~ "Feminist",
      val == "Agree"    & att == "fechld"    ~ "Feminist",
      TRUE                                   ~  NA_character_ 
    ))

write.csv(figdata, file.path(outDir,"gss_ga.csv")) # Save the data file

figdata$att <- factor(figdata$att, 
                        levels = c("fepol", "fechld", "fefam", "fepresch"), 
                        ordered = FALSE)


## Graph it!
fig1 <- ggplot(subset(figdata, prog == "Feminist" & att != "fepol"),
       aes(x = year, y = prop,
           ymin = prop_low, ymax = prop_upp,
           color = att)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3, aes(shape = att)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.2, 1)) +
  scale_x_continuous(breaks = c(1977, 2000, 2018, 2022), label = c("'77", "'00", "'18", "'22")) +
  scale_colour_manual(name="",
                      breaks=c("fechld", "fefam", "fepresch"),
                      labels=c("Agree a working mother can have warm \n relationship with her kids",
                               "Disagree woman takes care of home",
                               "Disagree preschooler suffers if mom works"),
                      values=c("#2EA5D7", "#F27575", "#51E0CE")) +
  scale_shape_manual(name="",
                      labels=c("Agree a working mother can have warm \n relationship with her kids",
                               "Disagree woman takes care of home",
                               "Disagree preschooler suffers if mom works"),
                      values=c(15, 19, 17, 3)) +
  theme_minimal(20) +
  theme(legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52"),
        legend.key.size    = unit(1.5, "cm"),
        strip.text.x       = element_text(face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption       = element_text(color = "grey70", face = "italic")) +
  labs( x        = "Survey Year", 
        y        = " ",
       title    = "Support for mothers' employment in 2022 \nis similar to recent pre-pandemic years",
       caption  = "General Social Surveys 1977-2022 | Joanna Pepin") 

fig1

ggsave(file.path(figDir,"fig1.png"), fig1,width=11, height=6, units="in", dpi=300, bg = "white")

# meovrwrk ---------------------------------------------------------------------

## create survey data
mwrk_svy <- data %>%
  filter(year > 1992) %>%
  drop_na(meovrwrk) %>%
  drop_na(sex)      %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(id = vpsu,
                   strata = stratvar,
                   weights = svyweight,
                   nest = TRUE)

## create the averages
meovrwrk_yr <- mwrk_svy %>%
  group_by(year, sex, meovrwrk) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))


## Graph it!
fig2 <- ggplot(subset(meovrwrk_yr, meovrwrk == "Agree"),
        aes(x = year, y = prop,
           ymin = prop_low, ymax = prop_upp,
           color = sex)) +
  geom_line(size = 1.5) +
  geom_pointrange(color = "#605A52") +
  geom_point(size = 3, shape=21, fill="white") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.2, 1)) +
  scale_x_continuous(limits = c(1993, 2023), breaks = c(1994, 2008, 2018, 2022), label = c("1994", "2008", "2018", "2022")) +
  theme_minimal(14) +
  theme(legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52"),
        legend.key.size    = unit(1.5, "cm"),
        strip.text.x       = element_text(face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption       = element_text(color = "grey70", face = "italic")) +
  labs( x        = "Survey Year", 
        y        = " ", 
        title    = "Majority U.S. adults agree that men hurt the family when they \nfocus too much on work",
        caption  = "General Social Surveys 1994-2022 | Joanna Pepin") +
  scale_color_manual(guide = 'none', values = c("#2EA5D7", "#F27575")) +
  scale_shape_discrete(guide = 'none') +
  geom_dl(aes(label = sex), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8))

fig2

ggsave(file.path(figDir,"fig2.png"), fig2,width=9, height=6, units="in", dpi=300, bg = "white")

