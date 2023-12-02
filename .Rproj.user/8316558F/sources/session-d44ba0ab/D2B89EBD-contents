library(survey)
library(ggeffects)
library(splines)
library(lspline)
library(gtools)

logdata <- data %>%
  mutate(
    home = case_when(
      home     == "Disagree"     ~ 1,
      home     == "Agree"        ~ 0),
    hdecide = case_when(
      hdecide  == "Disagree"     ~ 1,
      hdecide  == "Agree"        ~ 0),
    suffer = case_when(
      suffer   == "Disagree"     ~ 1,
      suffer   == "Agree"        ~ 0),
    warm = case_when(
      warm     == "Disagree"     ~ 0,
      warm     == "Agree"        ~ 1),
    jobopp = case_when(
      jobopp   == "Disagree"     ~ 0,
      jobopp   == "Agree"        ~ 1),
    lead = case_when(
      lead     == "Disagree"     ~ 0,
      lead     == "Agree"        ~ 1))

cat_vars <- c( "home", "hdecide", "suffer", "warm", "lead", "jobopp")

logdata <- logdata %>%
  modify_at(cat_vars, as.integer)

logdata_svy <- svydesign(ids     = ~1,
                         weights = ~weight,
                         data    = logdata)

# https://cran.r-project.org/web/packages/lspline/vignettes/lspline.html
lm_home    <- svyglm(home    ~ lspline(year, knots = c(1994, 2012)) + racesex + momed + momemp + religion, design= logdata_svy, family=binomial)
lm_hdecide <- svyglm(hdecide ~ lspline(year, knots = c(1994, 2012)) + racesex + momed + momemp + religion, design= logdata_svy, family=binomial)

knitr::kable(broom::tidy(lm_home))
knitr::kable(broom::tidy(lm_hdecide))
phome   <- ggeffect(lm_home, terms = c("year[1976:2018]"))
phdec   <- ggeffect(lm_hdecide, terms = c("year[1976:2018]"))

## Put it all together
phome$variable <- "home"
phdec$variable <- "hdecide"

pdata = smartbind(phome, phdec)
colnames(pdata)[colnames(pdata)=="x"] <- "year"
colnames(pdata)[colnames(pdata)=="predicted"] <- "prop"
head(pdata)

write.csv(pdata, "data/dol_model.csv")

## Visualize
ggplot(pdata, aes(x=year, y=prop, group = variable)) +
  geom_line()

## Averages
famd <- figdata_svy %>%
  filter(sphere != "Employed Mothers") %>%
  group_by(year, sphere, variable, val) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

## Spline Figure
fam <- ggplot(NULL, mapping = aes(x = year, y = prop,
                   color = variable, group = variable)) +
  geom_point(data = subset(famd, val == "Feminist" & sphere == "Family"), size = 1, alpha = 0.5) +
  geom_line(data  = pdata, size = 1.2)  +
  geom_ribbon(data  = pdata, aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.2, 1)) +
  scale_x_continuous(breaks = c(1976, 1996, 2018), label = c("'76", "'96", "'18")) +
  scale_colour_manual(name="",
                      breaks= c("hdecide", "home"),
                      labels=c("Disagree husband makes all family decisions", "Disagree woman takes care of home"),
                      values=c("#5D478B", "#D1AF00")) +
  ggtitle(label = "Young People's Attitudes About Gender in Families",
          subtitle = "Lines adjusted for demographic characteristics") +
  labs(caption = "Pepin & Cotter \nData source: Monitoring the Future Surveys") +
  theme_minimal() +
  theme(legend.position    = "top", 
        plot.title         = element_text(size=12),
        plot.subtitle      = element_text(size=10),
        legend.text        = element_text(color = "#605A52"),
        strip.text.x       = element_text(face = "bold"),
        plot.caption       = element_text(color = "#605A52", size = 8),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(color=guide_legend(ncol=1))

fam

ggsave("figures/model.png", fam, width = 5, height = 5, dpi = 300)
