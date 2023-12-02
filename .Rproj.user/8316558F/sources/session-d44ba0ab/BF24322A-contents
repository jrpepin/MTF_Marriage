#------------------------------------------------------------------------------------
# FINAL SAY PROJECT
# FS_02_quant analyses.R
# Joanna R. Pepin & William J. Scarborough
#------------------------------------------------------------------------------------

# This file analyzes the decision making variables.

#####################################################################################
# Paper Tables and Figures (quant)
#####################################################################################

# Table 02. -------------------------------------------------------------------------

## Specify reference levels
quantdata$relate   <- relevel(quantdata$relate,   ref = "Never married")
quantdata$raceeth  <- relevel(quantdata$raceeth,  ref = "White")
quantdata$educ     <- relevel(quantdata$educ,     ref = "High school")
quantdata$employ   <- relevel(quantdata$employ,   ref = "Employed")
quantdata$incdum   <- relevel(quantdata$incdum,   ref = "< than $50,000")
quantdata$earner   <- relevel(quantdata$earner,   ref = "Higher earner")
quantdata$order    <- relevel(quantdata$order,    ref = "Same")
quantdata$perI     <- as.numeric(quantdata$iperson == "Michelle") # create dummy variables
quantdata$perA     <- as.numeric(quantdata$aperson == "Michelle") # create dummy variables


### export to stata for FE models (Table C)
femodels <- quantdata %>%
  select(CaseID, idum, adum, perI, perA, 
         relinc, organize, mar, child, dur, item, gender, relate, parent, 
         raceeth, educ, employ, incdum, age, activity, order, weight)

write_dta(femodels , path = file.path(outDir, "femodels.dta")) 

## Run the models

logit1 <- glm(idum ~ perI + relinc + organize + mar + child + dur + item +
                gender+relate+parent+raceeth+educ+employ+incdum+age,
              quantdata, weights = weight, family="binomial")

logit2 <- glm(adum ~ perA + relinc + organize + mar + child + dur + order + activity +
                gender+relate+parent+raceeth+educ+employ+incdum+age,
              quantdata, weights = weight, family="binomial")

logit3 <- glm(idum ~ perI * relinc + organize + mar + child + dur + item +
                gender+relate+parent+raceeth+educ+employ+incdum+age,
              quantdata, weights = weight, family="binomial")

logit4 <- glm(adum ~ perA * relinc + organize + mar + child + dur + order + activity +
                gender+relate+parent+raceeth+educ+employ+incdum+age,
              quantdata, weights = weight, family="binomial")

## Average marginal effects
### https://strengejacke.github.io/ggeffects/articles/introduction_marginal_effects.html
### https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html

## Panel A ------------------------------------------------------------------------------------
AME_log1  <- summary(margins(logit1,  variables = c("perI", "relinc")))
AME_log2  <- summary(margins(logit2,  variables = c("perA", "relinc")))

summary(margins(logit1, variables = c("gender", "relate", "parent", "raceeth", "educ", "incdum", "age")))
summary(margins(logit2, variables = c("gender", "relate", "parent", "raceeth", "educ", "incdum", "age", "order")))

# test equality of coefficients between Item & Activity
# https://stats.stackexchange.com/questions/363762/testing-the-equality-of-two-regression-coefficients-from-same-data-but-different
z_genderA <- (AME_log1[[1,2]] - AME_log2[[1,2]]) / sqrt(AME_log1[[1,3]]^2 + AME_log2[[1,3]]^2)
z_equalA  <- (AME_log1[[2,2]] - AME_log2[[2,2]]) / sqrt(AME_log1[[2,3]]^2 + AME_log2[[2,3]]^2)
z_fmoreA  <- (AME_log1[[3,2]] - AME_log2[[3,2]]) / sqrt(AME_log1[[3,3]]^2 + AME_log2[[3,3]]^2)

p_genderA <- 2*pnorm(-abs(z_genderA)) 
p_equalA  <- 2*pnorm(-abs(z_equalA)) 
p_fmoreA  <- 2*pnorm(-abs(z_fmoreA)) 

AME_log1
AME_log2

print(paste("Gender test of equality: p =", round(p_genderA, digits = 3)))
print(paste("Woman Higher-Earner test of equality: p =", round(p_fmoreA, digits = 3)))
print(paste("Equal test of equality: p =", round(p_equalA, digits = 3)))

### -- sensitivity test -- include order of decider*gender of decider
quantdata <- quantdata %>%
  mutate(orderN= case_when (order == "Same"  ~ 0,
                            order == "Mixed" ~ 1))

logit1o <- glm(idum ~ perI * orderN + relinc + organize + mar + child + dur + item + 
                 gender+relate+parent+raceeth+educ+employ+incdum+age,
               quantdata, weights = weight, family="binomial")
logit2o <- glm(adum ~ perA * orderN + relinc + organize + mar + child + dur + activity +
                 gender+relate+parent+raceeth+educ+employ+incdum+age,
               quantdata, weights = weight, family="binomial")

AME1o <- summary(margins(logit1o, 
                         variables = "perI",
                         at = list(orderN= 0:1)))
AME2o <- summary(margins(logit2o, 
                         variables = "perA",
                         at = list(orderN= 0:1)))
AME1o 
AME2o

## Panel B ------------------------------------------------------------------------------------

### Item **************************************************************************************
AME3 <- summary(margins(logit3, 
                        variables = "relinc",
                        at = list(perI = 0:1)))
AME3
## test difference between coefficients
### create Z scores
z_equalI <- (AME3[[1,3]] - AME3[[2,3]]) / sqrt(AME3[[1,4]]^2 + AME3[[2,4]]^2)
z_womanI <- (AME3[[3,3]] - AME3[[4,3]]) / sqrt(AME3[[3,4]]^2 + AME3[[4,4]]^2)
z_equalI
z_womanI
### create p values
p_equalI  <- 2*pnorm(-abs(z_equalI))
p_womanI  <- 2*pnorm(-abs(z_womanI))

### report p values
print(paste("(ITEM) test of equality-- Equal Earners * gender: p =", round(p_equalI, digits = 3)))
print(paste("(ITEM) test of equality-- Woman Higher-Earner * gender: p =", round(p_womanI, digits = 3)))


### Activity **********************************************************************************
AME4 <- summary(margins(logit4, 
                        variables = "relinc",
                        at = list(perA = 0:1)))
AME4
## test difference between coefficients
### create Z scores
z_equalA <- (AME4[[1,3]] - AME4[[2,3]]) / sqrt(AME4[[1,4]]^2 + AME4[[2,4]]^2)
z_womanA  <- (AME4[[3,3]] - AME4[[4,3]]) / sqrt(AME4[[3,4]]^2 + AME4[[4,4]]^2)
z_equalA
z_womanA

### create p values
p_equalA  <- 2*pnorm(-abs(z_equalA))
p_womanA  <- 2*pnorm(-abs(z_womanA))

### report p values
print(paste("(ACTIVITY) test of equality-- Equal Earners * gender: p =", round(p_equalA, digits = 3)))
print(paste("(ACTIVITY) test of equality-- Woman Higher-Earner * gender: p =", round(p_womanA, digits = 3)))


### test equality of coefficients between Item & Activity ************************************

## Z scores
z_equal_MAN_AB  <- (AME3[[1,3]] - AME4[[1,3]]) / sqrt(AME3[[1,4]]^2 + AME4[[1,4]]^2)
z_equal_FEM_AB  <- (AME3[[2,3]] - AME4[[2,3]]) / sqrt(AME3[[2,4]]^2 + AME4[[2,4]]^2)

z_lower_MAN_AB  <- (AME3[[3,3]] - AME4[[3,3]]) / sqrt(AME3[[3,4]]^2 + AME4[[3,4]]^2)
z_lower_FEM_AB  <- (AME3[[4,3]] - AME4[[4,3]]) / sqrt(AME3[[4,4]]^2 + AME4[[4,4]]^2)

z_equal_MAN_AB
z_equal_FEM_AB
z_lower_MAN_AB
z_lower_FEM_AB

## p values
p_lower_MAN_AB  <- 2*pnorm(-abs(z_lower_MAN_AB)) 
p_equal_MAN_AB  <- 2*pnorm(-abs(z_equal_MAN_AB)) 

p_lower_FEM_AB  <- 2*pnorm(-abs(z_lower_FEM_AB)) 
p_equal_FEM_AB  <- 2*pnorm(-abs(z_equal_FEM_AB)) 

print(paste("(ITEM VS ACT) Man * Equal Earners test of equality: p =", round(p_equal_MAN_AB, digits = 3)))
print(paste("(ITEM VS ACT) Female * Equal Earners test of equality: p =", round(p_equal_FEM_AB, digits = 3)))

print(paste("(ITEM VS ACT) Man * Lower-Earner test of equality: p =", round(p_lower_MAN_AB, digits = 3)))
print(paste("(ITEM VS ACT) Female * Lower-Earner test of equality: p =", round(p_lower_FEM_AB, digits = 3)))


# Figure 2. --------------------------------------------------------------------------
# (*Figure 1 is the coherence plot generated in FS_03_qual analyses)
### https://strengejacke.github.io/ggeffects/articles/introduction_marginal_effects.html
### https://github.com/easystats/insight/issues/451 <- delta??
### https://data.library.virginia.edu/a-beginners-guide-to-marginal-effects/

## Create predicted probabilities datesets
pp3   <- ggeffect(logit3, terms = c("perI", "relinc"))
pp4   <- ggeffect(logit4, terms = c("perA", "relinc"))

pp3$type <- "purchase"
pp4$type <- "activity"

data_fig1 = merge(pp3, pp4, all = TRUE)
head(data_fig1)

data_fig1$x <-factor(data_fig1$x)
levels(data_fig1$x)[levels(data_fig1$x)=="1"] <- "She decided"
levels(data_fig1$x)[levels(data_fig1$x)=="0"] <- "He decided"

data_fig1$x    <- factor(data_fig1$x, levels = c("She decided", "He decided"), ordered = FALSE)
data_fig1$type <- factor(data_fig1$type, levels = c("purchase", "activity"), ordered = FALSE)

qualitative_hcl(4, palette = "Dark 3") # show color hex codes

fig2 <- data_fig1 %>%
  ggplot(aes(x = x, y = predicted, fill = x)) +
  geom_col(width = 0.6, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                stat="identity", position=position_dodge(.7), color="#ADB5BD") +
  geom_text(position = position_dodge(width = .7),
            vjust = -0.5,
            aes(label=sprintf("%1.0f%%", predicted*100))) +
  facet_grid(type ~ group,
             scales="free",
             space = "free",
             switch = "y") +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme_minimal() +
  theme(legend.position     = "none",
        panel.grid.major.x  = element_blank(),
        strip.text          = element_text(face = "bold"),
        strip.text.y.left   = element_text(angle = 0),
        axis.text.y         = element_blank(),  #remove y axis labels
        axis.ticks.y        = element_blank(),  #remove y axis ticks
        plot.subtitle       = element_text(face = "italic", color = "#707070"),
        plot.caption        = element_text(face = "italic", color = "#707070"),
        plot.title          = ggtext::element_markdown(),
        plot.title.position = "plot") +
  scale_y_continuous(labels=scales::percent, limits = c(0, .8)) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Perceptions of <span style = 'color: #E16A86;'>women's</span> and <span style = 'color: #00AD9A;'>men's</span> 
        decision-making about <p>purchases and activities.",
        subtitle = "% of respondents who said the decision was somewhat or very fair",
        caption  = "Predicted percentages calculated from logistic regression models reported in Table 2, Panel B.") 

fig2

ggsave(filename = file.path(figDir, "fig2.png"), fig2, width=6, height=6, units="in", dpi=300)


#####################################################################################
# Appendix Tables and Figures (quant)
#####################################################################################

# Table A ---------------------------------------------------------------------------
## Weighted Descriptive Statistics of Respondent Characteristics

## Create weighted data 
tabAdata <- quantdata %>%
  select("weight", "gender", "relate", "parent", "raceeth", 
         "educ", "employ", "incdum", "age")

tabASvy <- svydesign(ids = ~1, weights = ~ weight, data = tabAdata)

tabA <- tabASvy %>%
  tbl_svysummary(
    label = list(gender  ~ "Women",
                 relate  ~ "Relationship Status",
                 parent  ~ "Parent",
                 raceeth ~ "Respondent race/ethnicity",
                 educ    ~ "Educational attainment",
                 employ  ~ "Employment status",
                 incdum  ~ "Household income > $50,000",
                 age     ~ "Respondent age"),
    type  = list(gender  ~ "dichotomous",
                 parent  ~ "dichotomous",
                 incdum  ~ "dichotomous"),
    value = list(gender  = "Female",
                 parent  = "Parent",
                 incdum  = "> than $50,000"))  %>%
  modify_header(
   label = '**Variable**',
   stat_0 = '**N = 3,978**') %>%
  as_flex_table() 

tabA # show table

## https://mran.microsoft.com/snapshot/2017-12-11/web/packages/officer/vignettes/word.html
read_docx() %>% 
  body_add_par("Table A. Weighted Sample Characteristics") %>% 
  body_add_flextable(value = tabA) %>% 
  print(target = file.path(outDir, "finalsay_tableA.docx"))


# Table B ---------------------------------------------------------------------------
## Weighted Bi-variate Statistics of Perceptions of Fairness in Decision Making by Type of Decision

tabBdata <- quantdata %>%
  # Create long data for item/activity vars
  pivot_longer(
    cols = c(idum, adum),
    names_to = "type",
    values_to = "fairness") %>%
  # Add a gender of decider variable
  mutate(
    person = case_when(
      (type  == "idum"   &  iperson == "Michelle") |
        (type  == "adum"   &  aperson == "Michelle") ~ "Woman",
      (type  == "idum"   &  iperson == "Anthony")  |
        (type  == "adum"   &  aperson == "Anthony")  ~ "Man")) %>%
  # Create long data for vignette manipulation
  pivot_longer(
    cols = c(relinc, person, organize, mar, child, dur),
    names_to = "variable",
    values_to = "level") %>%
  # Keep vignette variables
  select("CaseID", "weight", "type", "variable", "level", "fairness")

## Re order variable manipulations
tabBdata$variable <- factor(tabBdata$variable, 
                            levels = c("relinc", "person", "organize",
                                       "mar", "child", "dur"), ordered = FALSE)
## Set as weighted survey data
tabBSvy <- tabBdata %>% 
  srvyr::as_survey_design(id = CaseID,
                          weights = weight)
## Create summary data
tabBsum <- tabBSvy %>%
  group_by(type, variable, level) %>%
  summarize(mean = survey_mean(fairness, na.rm = TRUE),
            sd = sd(fairness, na.rm = TRUE)) %>%
  subset(select = -c(variable, mean_se)) %>%
  pivot_wider(names_from = type, values_from = c(mean, sd)) %>%
  select(level, mean_idum, sd_idum, mean_adum, sd_adum)

tabBsum <- tabBsum %>%
  mutate(
    cat = case_when(
      tabBsum$level == "Man higher-earner"     |
        tabBsum$level == "Woman higher-earner" |
        tabBsum$level == "Equal earners"       ~ "Relative Earnings",
      tabBsum$level == "Woman"                 |
        tabBsum$level == "Man"                 ~ "Gender of Decider",
      tabBsum$level == "Shared"                |
        tabBsum$level == "Separate"            |
        tabBsum$level == "Both"                ~ "Financial Allocation Strategy",
      tabBsum$level == "live together"         |
        tabBsum$level == "are married"         ~ "Marital Status",
      tabBsum$level == "no children"           |
        tabBsum$level == "one child together"  ~ "Parental Status",
      tabBsum$level == "3 years"               |
        tabBsum$level == "7 years"             ~ "Relationship Duration"))

## Create Flextable
tabBsum <- as_grouped_data(x = tabBsum, groups = c("cat"), columns = NULL) # Group by vignette condition

tabB <- tabBsum %>%
  flextable::as_flextable(hide_grouplabel = TRUE) %>%
  add_header_row(values = c("", "Purchase", "Activity"), colwidths = c(1, 2, 2)) %>%
  flextable::align(i = 1, align = "center", part = "header") %>%
  colformat_double(digits = 2) %>%
  set_header_labels(level = "Vignette Variables",
                    mean_idum = "M",
                    sd_idum   = "SD", 
                    mean_adum = "M",
                    sd_adum   = "SD" ) %>% 
  autofit() %>%
  padding(i=c(2:4,6:7,9:11,13:14,16:17,19:20), j=1, padding.left=25) %>%
  add_footer(level = "Note: Descriptive statistics include survey weights.\n
             Range is from 0 (not fair) to 1 (fair).") %>%
  merge_at(j = 1:5, part = "footer")

num <-nrow(quantdata) #number of observations

tabB # show table

read_docx() %>% 
  body_add_par(paste("Table 02. Weighted Bivariate Statistics of Perceptions of Fairness in Decision Making 
               by Type of Decision (N = ", num,")", sep="")) %>% 
  body_add_flextable(value = tabB) %>% 
  print(target = file.path(outDir, "finalsay_tableB.docx")) # save table


# Appendix Figure A. --------------------------------------------------------------------------
## Fairness Evaluation by Item/Activity Presented to Respondent

data_figA <- quantdata %>%
  select("CaseID", "item", "activity", "ifair", "afair") %>%
  # Create long data for item/activity fairness vars
  pivot_longer(
    cols = c(ifair, afair),
    names_to = "drop",
    values_to = "fairness") %>%
  # Create long data for item/activity decision vars
  pivot_longer(
    cols = c(item, activity),
    names_to = "type",
    values_to = "category") %>%
  # remove duplicates
  filter((drop == "ifair" & type == "item") |
           (drop == "afair" & type == "activity")) %>%
  select(-c("drop")) %>%
  # create percentage data
  group_by(type, category) %>%
  count(category = factor(category), fairness = factor(fairness)) %>% 
  mutate(pct = prop.table(n)) %>%
  ungroup()

data_figA$type[data_figA$type == "item"] <-"purchase"
data_figA$type <- factor(data_figA$type, levels = c("purchase", "activity"), ordered = FALSE)


figA <- data_figA %>%
  ggplot(aes(x = category, y = pct, fill = fairness)) +
  geom_col(position = "fill",
           width = 0.6) +
  facet_grid(type ~ .,
             scales="free",
             space = "free",
             switch = "y") +
  geom_text(aes(label=sprintf("%1.0f%%", pct*100)),
            position=position_stack(vjust=0.5),
            colour = "white",
            size = 3) +
  coord_flip()+
  scale_fill_manual(values = c("#18BC9C", "#3498DB", "#F39C12", "#E74C3C")) +
  theme_minimal() +
  theme(legend.position      = "top",
        legend.justification = c(1, 0),
        panel.grid.major.x   = element_blank(),
        strip.placement      = 'outside',
        strip.text.y         = element_text(face = "bold"),
        strip.text.y.left    = element_text(angle = 0),
        plot.title           = element_text(face = "bold"),
        plot.title.position  = "plot",
        plot.subtitle       = element_text(face = "italic", color = "#707070"),
        plot.caption        = element_text(face = "italic", color = "#707070")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "Fairness evaluation by purchase & activity presented to respondent",
        subtitle = "How fair do you think the decision was?")

figA   

ggsave(filename = file.path(figDir, "figA.png"), figA, width=6, height=4, units="in", dpi=300)



# # Appendix Figure B. --------------------------------------------------------------------------
# 
# ## Create predicted probabilities datesets
# pp1   <- ggeffect(logit1, terms = "perI")
# pp2   <- ggeffect(logit2, terms = "perA")
# 
# # https://github.com/easystats/insight/issues/451 <- delta??
# 
# pp1$type <- "item"
# pp2$type <- "activity"
# 
# data_figB = merge(pp1, pp2, all = TRUE)
# head(data_figB)
# 
# data_figB$x[data_figB$x==1] <- "She decided"
# data_figB$x[data_figB$x==0]  <- "He decided"
# data_figB$type <- factor(data_figB$type, levels = c("item", "activity"), ordered = FALSE)
# 
# figB <- data_figB %>%
#   ggplot(aes(x = x, y = predicted, fill = x)) +
#   geom_col(width = 0.6, position = position_dodge(0.7)) +
#   geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
#                 stat="identity", position=position_dodge(.7), color="#ADB5BD") +
#   geom_text(position = position_dodge(width = .7),
#             vjust = -0.5,
#             aes(label=sprintf("%1.0f%%", predicted*100))) +
#   facet_wrap(~ type) +
#   scale_fill_manual(values = c("#18BC9C", "#F39C12")) +
#   theme_minimal() +
#   theme(legend.position     = "top",
#         panel.grid.major.x  = element_blank(),
#         plot.title          = element_text(face = "bold"),
#         plot.title.position = "plot",
#         plot.subtitle       = element_text(face = "italic", color = "#707070"),
#         plot.caption        = element_text(face = "italic", color = "#707070")) +
#   scale_y_continuous(labels=scales::percent, limits = c(0, .8)) +
#   labs( x        = " ", 
#         y        = " ", 
#         fill     = " ",
#         title    = "Gender differences in perceptions of fairness \nin decision-making for items and activities.",
#         subtitle = "% of respondents who said the decision was somewhat or very fair...",
#         caption  = "Predicted percentages adjust for other vignette manipulations and respondent demographic characteristics.") 
# 
# figB
# 
# ggsave(filename = file.path(figDir, "figB.png"), figB, width=5, height=5, units="in", dpi=300)


