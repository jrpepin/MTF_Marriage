#-------------------------------------------------------------------------------
# MARRIAGE ATTITUDES PROJECT
# MA_02_analysis and figures.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------

# DESCRIPTIVE STATISTICS -------------------------------------------------------

## Create survey data 
mtf_svy <- data %>%
  drop_na(mar3) %>% # exclude cases missing on DV
  as_survey_design(id = 1,
                   weights = svyweight)

## Create table
tabA <- mtf_svy %>%
  select(c(-svyweight, -mardum, -racesex)) %>%
  tbl_svysummary(
    label = list(year     ~ "Year",
                 mar3     ~ "Marriage expectations",
                 sex      ~ "Gender",
                 race     ~ "Race",
                 momed    ~ "Mothers' education",
                 momemp   ~ "Mothers' employment when growing up",
                 famstru  ~ "Family structure when growing up",
                 religion ~ "Religiosity",
                 region   ~ "Region",
                 tablet   ~ "Survey mode (2019)"))  %>%
  modify_header(
    label = '**Variable**',
    stat_0 = '**N (unweighted) = 118966**') %>%
  as_flex_table() 

tabA # show table


# DICHOTOMOUS ------------------------------------------------------------------

## Create yearly averages
mar_yr <- mtf_svy %>%
  group_by(year, mardum) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

### PLOT
mar_yr %>%
  filter(mardum == 1) %>%
  ggplot(aes(x = year, y = prop, ymin = prop_low, ymax = prop_upp)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.5, 1)) +
  geom_pointrange(color = "#605A52") +
  theme_minimal() +
  theme(strip.text.x       = element_text(face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption       = element_text(color = "grey70", face = "italic")) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Percent of U.S. twelfth-graders who think they 'will get married'",
        caption  = "Monitoring the Future Surveys 1976-2022")
  
##  Mode sensitivity check

### Create yearly averages
mar_mode <- mtf_svy        %>%
  drop_na(tablet)          %>%
  group_by(tablet, mardum) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

### PLOT
mar_mode %>%
  filter(mardum == 1) %>%
  ggplot(aes(x = tablet, y = prop, ymin = prop_low, ymax = prop_upp)) +
  geom_pointrange(color = "#605A52") +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.5, 1)) +
  theme_minimal() +
  theme(strip.text.x       = element_text(face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption       = element_text(color = "grey70", face = "italic")) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Percent of 2019 U.S. twelfth-graders who think they 'will get married'",
        subtitle = "R received tablet or paper survey",
        caption  = "2019 Monitoring the Future Survey")


# 3 CATEGORY VARIABLE ----------------------------------------------------------

## Create yearly averages
mar_yr_3 <- mtf_svy %>%
  group_by(year, mar3) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

## Structure levels
mar_yr_3$mar3 <- factor(mar_yr_3$mar3, 
                        levels = c("GETTING MARRIED", "I HAVE NO IDEA", 
                                   "NOT GETTING MARRIED"), ordered = FALSE)

### PLOT
mar7622 <- mar_yr_3 %>%
  ggplot(aes(x = year, y = prop, color = mar3, ymin = prop_low, ymax = prop_upp)) +
  geom_pointrange(aes(shape=mar3)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    theme_minimal() +
  theme(strip.text.x       = element_text(face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption       = element_text(color = "grey70", face = "italic"),
        legend.position    = "top") +
  labs( x        = " ", 
        y        = " ", 
        title    = "U.S. twelfth-graders marriage expectations",
        caption  = "Monitoring the Future Surveys 1976-2022")

mar7622

ggsave(file.path(figDir,"mar7622.png"), mar7622, width=7, height=5, units="in", dpi=300, bg = "white")

  
# 3 CATEGORY * SEX -------------------------------------------------------------

## Create survey data
mar3yrS_svy <- data %>%
  drop_na(mar3) %>%
  drop_na(sex) %>%
  as_survey_design(weights = svyweight)


## Create yearly averages
mar3yrS <- mar3yrS_svy %>%
  group_by(year, sex, mar3) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

## Structure levels
mar3yrS$mar3 <- factor(mar3yrS$mar3, 
                        levels = c("GETTING MARRIED", "I HAVE NO IDEA", 
                                   "NOT GETTING MARRIED"), ordered = FALSE)

### PLOT
mar3yrS %>%
  ggplot(aes(x = year, y = prop, color = sex, ymin = prop_low, ymax = prop_upp)) +
  geom_pointrange(aes(shape=sex)) +
  facet_grid(~ mar3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme_minimal() +
  theme(strip.text.x       = element_text(face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption       = element_text(color = "grey70", face = "italic"),
        legend.position    = "top") +
  labs( x        = " ", 
        y        = " ", 
        title    = "U.S. twelfth-graders marriage expectations",
        caption  = "Monitoring the Future Surveys 1976-2022")


# 3 CATEGORY * RACE ------------------------------------------------------------
## Create survey data
mar3yrR_svy <- data %>%
  drop_na(mar3) %>%
  drop_na(race) %>%
  as_survey_design(weights = svyweight)


## Create yearly averages
mar3yrR <- mar3yrR_svy %>%
  group_by(year, race, mar3) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

## Structure levels
mar3yrR$mar3 <- factor(mar3yrR$mar3, 
                       levels = c("GETTING MARRIED", "I HAVE NO IDEA", 
                                  "NOT GETTING MARRIED"), ordered = FALSE)

### PLOT
mar3yrR %>%
  ggplot(aes(x = year, y = prop, color = race, ymin = prop_low, ymax = prop_upp)) +
  geom_pointrange(aes(shape=race)) +
  facet_grid(~ mar3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.1, 1)) +
  theme_minimal() +
  theme(strip.text.x       = element_text(face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption       = element_text(color = "grey70", face = "italic"),
        legend.position    = "top") +
  labs( x        = " ", 
        y        = " ", 
        title    = "U.S. twelfth-graders marriage expectations",
        caption  = "Monitoring the Future Surveys 1976-2022")

# 3 CATEGORY * RACESEX ----------------------------------------------------------
## Create survey data
mar3yrRS_svy <- data %>%
  drop_na(mar3) %>%
  drop_na(racesex) %>%
  as_survey_design(weights = svyweight)

## Create yearly averages
mar3yrRS <- mar3yrRS_svy %>%
  group_by(year, racesex, mar3) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

## Structure levels
mar3yrRS$mar3 <- factor(mar3yrRS$mar3, 
                       levels = c("GETTING MARRIED", "I HAVE NO IDEA", 
                                  "NOT GETTING MARRIED"), ordered = FALSE)

### PLOT
mar3yrRS %>%
  ggplot(aes(x = year, y = prop, color = mar3, ymin = prop_low, ymax = prop_upp)) +
  geom_pointrange(aes(shape=mar3)) +
  facet_grid(~ racesex) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.1, 1)) +
  theme_minimal() +
  theme(strip.text.x       = element_text(face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption       = element_text(color = "grey70", face = "italic"),
        legend.position    = "top") +
  labs( x        = " ", 
        y        = " ", 
        title    = "U.S. twelfth-graders marriage expectations",
        caption  = "Monitoring the Future Surveys 1976-2022")
