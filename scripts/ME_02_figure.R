#-------------------------------------------------------------------------------
# MARRIAGE EXPECTATIONS PROJECT
# ME_02_figure.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------

# Project Environment ----------------------------------------------------------
## The ME_00-1_setup and packages.R script  & ME_01_measures and sample.R
## should should be run before this script.

# source(ME_00-1_setup and packages.R)
# source(ME_01_measures and sample.R)


# Define color palette ---------------------------------------------------------
goodsp_palette <- c("#18BC9C","#6f42c1", "#F39C12", "#3498DB", "#E74C3C")

mar3_palette = c( "#18BC9C","#F39C12", "#E74C3C")

# CREATE FIGURE DATA -----------------------------------------------------------

##GOOD SP
df1 <- mtf_svy %>%
  drop_na(sex) %>% # remove cases w/ missing sex
  group_by(year, sex, goodsp) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) # create summary proportions

## Point Change dfs
df_pc1 <- mtf_svy %>%
  drop_na(sex) %>% # remove cases w/ missing sex
  group_by(year, sex, goodsp_lbl) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  filter(year == 2013 | year == 2023) %>%
  pivot_wider(id_cols     = c(sex, goodsp_lbl), 
              names_from  = year, 
              values_from = c(vals, vals_low, vals_upp)) %>%
  mutate(pct_chg = vals_2023 - vals_2013,
         label   = scales::percent(pct_chg %>% round(2)))

## GETMAR
df2 <- mtf_svy %>%
  drop_na(sex) %>% # remove cases w/ missing sex
  group_by(year, sex, mar3_lbl) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) # create summary proportions

## Point Change dfs
df_pc2 <- df2 %>%
  filter(year == 2013 | year == 2023) %>%
  pivot_wider(id_cols     = c(sex, mar3_lbl), 
              names_from  = year, 
              values_from = c(vals, vals_low, vals_upp)) %>%
  mutate(pct_chg = vals_2023 - vals_2013,
         label   = scales::percent(pct_chg %>% round(2)))

names <- list('goodsp' = df1, 'goodsp_change' = df_pc1, 'mar3' = df2, 'mar3_change' = df_pc2)
write.xlsx(names, file = file.path(outDir, "figdata_updated.xlsx"))

# VISUALIZE ------------------------------------------------------------------
### GOOD SP

p1 <- df1 %>%
  ggplot(aes(x = year, y = vals, color = goodsp, shape = goodsp, ymin = vals_low, ymax = vals_upp)) +
  geom_hline(yintercept  = c(0, .25, .5, .75), color = "grey90") +
  geom_smooth(method     = loess, fill = "grey80", linewidth = .65) +
  geom_point(aes(alpha   = .7), show.legend = FALSE) +
  # Only plot the labels 1 time -- using 2021 for not so good/poor order
  geom_text(data         = subset(df1, sex == "Women" & year == 2021 & goodsp != "Fairly good" & goodsp != "Not so good"), 
            aes(label    = goodsp), 
            nudge_x      = 3,
            hjust        = 0,
             size        = 3)   +
  geom_text(data         = subset(df1, sex == "Women" & year == 2021 & (goodsp == "Fairly good" | goodsp == "Not so good")), 
            aes(label    = goodsp), 
            nudge_x      = 3,
            nudge_y      = .03, # adding space between labels
            hjust        = 0,
            size         = 3)   +
  facet_wrap("sex", scales = "free_x")  +
  coord_cartesian(clip = 'off') +   # Allow labels to bleed past the canvas boundaries
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, .9), 
                     breaks = c(.25, .5, .75)) +
  scale_x_continuous(limit  = c(1976, 2025),
                     breaks = c(1976, 2023)) +
  theme(strip.text.x        = element_text(face = "bold.italic", size = 10, hjust = 0),
        axis.title.y        = element_blank(),
        panel.spacing       = unit(2, "lines"),
        panel.grid.minor    = element_blank(),
        panel.grid.major    = element_blank(),
        legend.position     = "none",
        plot.title.position = "plot",
        plot.margin         = unit(c(0.25, .5, 0.00, 0.25), "inches")) +
  scale_color_manual(values = c(goodsp_palette)) +
  labs( x        = NULL, 
        y        = NULL,
        title    = NULL,
        subtitle = "Panel A.% who think they will be _______ as a spouse")
p1

### MAR3
p2 <- df2 %>%
  ggplot(aes(x = year, y = vals, color = mar3_lbl, shape = mar3_lbl, ymin = vals_low, ymax = vals_upp)) +
  geom_hline(yintercept  = c(0, .25, .5, .75), color = "grey90") +
  geom_smooth(method     = loess, fill = "grey80", linewidth = .65) +
  geom_point(aes(alpha   = .7), show.legend = FALSE) +
  geom_text(data           = subset(df2, sex == "Men" & year == 2022), # Only plot the labels 1 time
            aes(label      = mar3_lbl), 
            nudge_x        = 2,
            hjust          = 0,
            size           = 3)   +
  facet_wrap("sex", scales = "free_x")  +
  coord_cartesian(clip = 'off') +   # Allow labels to bleed past the canvas boundaries
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, .9), 
                     breaks = c()) +
  scale_x_continuous(limit  = c(1976, 2025),
                     breaks = c(1976, 2023)) +
  scale_color_manual(values = c(mar3_palette)) +
  scale_shape_manual(values = c(16, 15, 12)) +
  theme_minimal() +
  theme(strip.text.x        = element_text(face = "bold.italic", size = 10, hjust = 0),
        axis.title.y        = element_blank(),
        panel.spacing       = unit(2, "lines"),
        panel.grid.minor    = element_blank(),
        panel.grid.major    = element_blank(),
        legend.position     = "none",
        plot.title.position = "plot",
        plot.margin         = unit(c(0.25, 0.25, 0.00, 0.25), "inches")) +
  labs( x        = NULL, 
        y        = NULL,
        title    = NULL,
        subtitle = "Panel B. % who think they will _______")
p2


## Panel C. Change from 1976 to 2023
p3 <- df_pc1 %>%
  ggplot(aes(x = goodsp_lbl, y = pct_chg, fill = goodsp_lbl)) +
  geom_col(aes(alpha = .9), width = 0.4) +
  geom_hline(yintercept = 0) +
  facet_wrap("sex", ncol = 2) +
  geom_text(aes(y = pct_chg + .02 * sign(pct_chg), label = label), size= 3) +
  scale_y_continuous(limits = c(-.2, 0.15)) +
  theme_minimal() +
  theme(strip.text.x        = element_text(face = "bold.italic", size = 9, hjust = 0),
        strip.placement     = "outside",
        panel.spacing       = unit(.5, "lines"),
        axis.text.x         = element_markdown(colour = c(goodsp_palette)),
        axis.text.y         = element_blank(), 
        axis.ticks.y        = element_blank(),
        panel.grid.minor    = element_blank(),
        panel.grid.major    = element_blank(),
        plot.title.position = "plot",
        legend.position     = "none",
        plot.margin         = unit(c(0.25, 0.5, 0.00, 0.3), "inches")) +
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = c(goodsp_palette)) +
  labs( x        = NULL, 
        y        = NULL,
        title    = " ",
        subtitle = "Panel C. % point change from 2013 to 2023")
p3


## Panel D. Change from 1976 to 2023
p4 <- df_pc2 %>%
  ggplot(aes(x = mar3_lbl, y = pct_chg, fill = mar3_lbl)) +
  geom_col(aes(alpha = .9), width = 0.25) +
  geom_hline(yintercept = 0) +
  facet_wrap("sex", ncol = 2) +
  geom_text(aes(y = pct_chg + .02 * sign(pct_chg), label = label), size= 3) +
  scale_y_continuous(limits = c(-.25, 0.15)) +
  theme_minimal() +
  theme(strip.text.x        = element_text(face = "bold.italic", size = 9, hjust = 0),
        strip.placement     = "outside",
        panel.spacing       = unit(.5, "lines"),
        axis.text.x         = element_markdown(colour = c(mar3_palette)),
        axis.text.y         = element_blank(), 
        axis.ticks.y        = element_blank(),
        panel.grid.minor    = element_blank(),
        panel.grid.major    = element_blank(),
        plot.title.position = "plot",
        legend.position     = "none",
        plot.margin         = unit(c(0.25, 0.25, 0.00, 0.3), "inches")) +
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = c(mar3_palette)) +
  labs( x        = NULL, 
        y        = NULL,
        title    = " ",
        subtitle = "Panel D. % point change from 2013 to 2023")
p4

### Put it all together
p <- ggarrange(p1, p2, p3, p4,
              ncol = 2, nrow = 2, heights = c(1.1, 1), widths = c(1.1, 1))
p


ggsave("marfig_updated.png", p, width = 9, height = 6.5, dpi = 300, bg = 'white')
#ggsave("peer-review/marfig.png", p, width = 9, height = 6.5, dpi = 300, bg = 'white') # duplicate in peer review folder

