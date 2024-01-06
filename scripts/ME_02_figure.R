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
goodsp_palette <- c("#18BC9C","#3498DB", "#6f42c1","#E74C3C", "#F39C12")

mar3_palette = c( "#18BC9C","#3498DB", "#E74C3C")

# CREATE FIGURE DATA -----------------------------------------------------------

##GOOD SP
df1 <- mtf_svy %>%
  drop_na(sex) %>% # remove cases w/ missing sex
  group_by(year, sex, goodsp) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) # create summary proportions

## GETMAR
df2 <- mtf_svy %>%
  drop_na(sex) %>% # remove cases w/ missing sex
  group_by(year, sex, mar3) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) # create summary proportions

## Point Change dfs
df_pc2 <- df2 %>%
  filter(year == 2012 | year == 2022) %>%
  pivot_wider(id_cols     = c(sex, mar3), 
              names_from  = year, 
              values_from = c(vals, vals_low, vals_upp)) %>%
  mutate(pct_chg = vals_2022 - vals_2012,
         label   = scales::percent(pct_chg %>% round(2)))

names <- list('goodsp' = df1, 'mar3' = df2, 'mar3_change' = df_pc2)
write.xlsx(names, file = file.path(outDir, "figdata.xlsx"))

# VISUALIZE ------------------------------------------------------------------
### GOOD SP
p1 <- df1 %>%
  ggplot(aes(x = year, y = vals, color = goodsp, shape = goodsp, ymin = vals_low, ymax = vals_upp)) +
  geom_smooth(method = loess, fill = "grey90", linewidth = .75) +
  geom_point(aes(alpha = .9), show.legend = FALSE) +
  facet_wrap("sex")  +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 1), 
                     breaks = c(.5)) +
  scale_x_continuous(breaks = c(1976, 2022)) +
  theme(strip.text.x        = element_text(face = "bold.italic", size = 10, hjust = 0),
        axis.title.y        = element_blank(),
        panel.spacing       = unit(2, "lines"),
        panel.grid.minor    = element_blank(),
        panel.grid.major    = element_blank(),
        plot.caption        = element_text(color = "grey70", face = "italic"),
        legend.position     = "none",
        plot.title.position = "plot",
        plot.margin = unit(c(0.25, 0.5, 0.05, 0.25), "inches")) +
  geom_hline(yintercept=c(0, .5), color = "grey90") +
  scale_color_manual(values = c(goodsp_palette)) +
  labs( x        = " ", 
        y        = " ",
        title    = "Panel A.",
        subtitle = "% who think they will be _______ as a spouse")
p1

### MAR3
p2 <- df2 %>%
  mutate(mar3 = factor(mar3, levels = c("Getting married", "I have no idea", "Not getting married"))) %>%
  ggplot(aes(x = year, y = vals, color = mar3, ymin = vals_low, ymax = vals_upp)) +
  geom_smooth(method = loess, fill = "grey80", linewidth = .75) +
  geom_point(aes(alpha = .9), show.legend = FALSE) +
  facet_wrap("sex")  +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 1), 
                     breaks = c()) +
  scale_x_continuous(breaks = c(1976, 2022)) +
  scale_color_manual(values = c(mar3_palette)) +
  theme_minimal() +
  theme(strip.text.x        = element_text(face = "bold.italic", size = 10, hjust = 0),
        axis.title.y        = element_blank(),
        panel.spacing       = unit(2, "lines"),
        panel.grid.minor    = element_blank(),
        panel.grid.major    = element_blank(),
        legend.position     = "none",
        plot.title.position = "plot",
        plot.margin = unit(c(0.25, 0.2, 0.05, 0.75), "inches")) +
  geom_hline(yintercept=c(0, .5), color = "grey90") +
  labs( x        = " ", 
        y        = " ",
        title    = "Panel B.",
        subtitle = "% who think they will _______")
p2

## Panel C. Change from 1976 to 2022
p3 <- df_pc2 %>%
  mutate(mar3 = factor(mar3, levels = c("Getting married", "I have no idea", "Not getting married"))) %>%
  ggplot(aes(x = mar3, y = pct_chg, fill = mar3)) +
  geom_col(aes(alpha = .9), width = 0.4) +
  geom_hline(yintercept = 0) +
  facet_wrap("sex", ncol = 1) +
  geom_text(aes(label = label, vjust = -0.5)) +
  scale_y_continuous(limits = c(-.2, 0.15)) +
  theme_minimal() +
  theme(strip.text.x        = element_text(face = "bold.italic", size = 10, hjust = 0),
        strip.placement     = "outside",
        panel.spacing       = unit(.5, "lines"),
        axis.text.x         = element_markdown(colour = c(mar3_palette)),
        axis.text.y         = element_blank(), 
        axis.ticks.y        = element_blank(),
        panel.grid.minor    = element_blank(),
        panel.grid.major    = element_blank(),
        plot.title.position = "plot",
        legend.position     = "none",
        plot.margin = unit(c(0.25, 0.25, 0.05, 0.2), "inches")) +
  scale_x_discrete(position = "top",
                   labels   = c("Getting married"     = "Get\nmarried", 
                                  "I have no idea"      = "Has\nno idea",
                                  "Not getting married" = "Not get\nmarried")) +
  scale_fill_manual(values = c(mar3_palette)) +
  labs( title    = "Panel C.",
        subtitle = "Percentage point change\n2012 - 2022",
        x        = " ", 
        y        = " ",
        caption  = " ")
p3

### Put it all together
p <- ggarrange(p1, p2, p3,
              ncol = 3, nrow = 1, widths = c(1,1, .65))
p

ptext <- annotate_figure(p, 
                bottom = text_grob("  Figure 1. Trends in U.S. twelfth-graders marriage expectations
  Source: Data are from the Monitoring the Future Surveys (U.S.), 1976-2022.
  Note: Percentages are weighted to be nationally representative of U.S. high school seniors.
  Additional details about data access and variable construction are available at: https://github.com/jrpepin/MTF_Marriage.
  ", hjust = 0, x = 0, size = 9))

plbl <- ptext +
  annotate("text", x = .33, y = .54, label = "very good",        size = 8/.pt, fontface =2, hjust = 0, color = "#18BC9C") +
  annotate("text", x = .33, y = .43, label = "good",             size = 8/.pt, fontface =2, hjust = 0, color = "#3498DB") +
  annotate("text", x = .33, y = .34, label = "fairly good",      size = 8/.pt, fontface =2, hjust = 0, color = "#6f42c1") +
  annotate("text", x = .33, y = .29, label = "not so good",      size = 8/.pt, fontface =2, hjust = 0, color = "#E74C3C") +
  annotate("text", x = .33, y = .27, label = "poor",             size = 8/.pt, fontface =2, hjust = 0, color = "#F39C12") +
  annotate("text", x = .57, y = .64, label = "get\nmarried",     size = 8/.pt, fontface =2, hjust = 0, color = "#18BC9C") +
  annotate("text", x = .57, y = .39, label = "has\nno idea",     size = 8/.pt, fontface =2, hjust = 0, color = "#3498DB") +
  annotate("text", x = .57, y = .29, label = "not get\nmarried", size = 8/.pt, fontface =2, hjust = 0, color = "#E74C3C")

plbl

ggsave("marfig.png", plbl, width = 9, height = 6.5, dpi = 300, bg = 'white')
