#-------------------------------------------------------------------------------
# MARRIAGE ATTITUDES PROJECT
# MA_02_figure.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------

# Define color palette ---------------------------------------------------------
mar3_palette = c("#18BC9C", "#E74C3C", "#3498DB")

c_palette <- c("#6f42c1", "#F39C12")


# CREATE FIGURE DATA -----------------------------------------------------------

## TRENDS
### ALL
df0 <- mtf_svy %>%
  group_by(year, mar3) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round_percent(vals)/100, # round with preserving to 100%
         vals_low = round_percent(vals_low)/100, 
         vals_upp = round_percent(vals_upp)/100)

### GENDER
df1 <- mtf_svy %>%
  drop_na(sex) %>% # remove cases w/ missing sex
  group_by(year, sex, mar3) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  filter(mar3 == "Getting married") # only look at getmar

### RACE
df2 <- mtf_svy %>%
  drop_na(race) %>% # remove cases w/ missing race
  group_by(year, race, mar3) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  filter(mar3 == "Getting married") # avoiding negative vals_low for 2020 Black

### MOTHERS' EDUC
df3 <- mtf_svy %>%
  drop_na(momed) %>% # remove cases w/ missing mothers' educ.
  group_by(year, momed, mar3) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  filter(mar3 == "Getting married") # avoiding negative vals_low for 2020 Black

#### Combine dfs
df1$cat    <- "Gender" 
df2$cat    <- "Race" 
df3$cat    <- "Mother's education" 

colnames(df1)[colnames(df1)=="sex"]   <- "demo"
colnames(df2)[colnames(df2)=="race"]  <- "demo"
colnames(df3)[colnames(df3)=="momed"] <- "demo"

df4 <- rbind(df1, df2, df3)

df4$cat <- factor(df4$cat, levels = c("Gender", "Race", 
                                      "Mother's education"), ordered = FALSE)

## Point Change dfs

### ALL
df_pc0 <- df0 %>%
  filter(year == 1976 | year == 2022) %>%
  pivot_wider(names_from = year, values_from = c(vals, vals_low, vals_upp)) %>%
  mutate(pct_chg = vals_2022 - vals_1976,
         label   = scales::percent(pct_chg %>% round(2)))

### GENDER
df_pc1 <- df1 %>%
  filter(year == 1976 | year == 2022) %>%
  pivot_wider(id_cols     = demo, 
              names_from  = year, 
              values_from = c(vals, vals_low, vals_upp)) %>%
  mutate(pct_chg = vals_2022 - vals_1976,
         label   = scales::percent(pct_chg %>% round(2)))

### RACE
df_pc2 <- df2 %>%
  filter(year == 1976 | year == 2022) %>%
  pivot_wider(id_cols     = demo, 
              names_from  = year, 
              values_from = c(vals, vals_low, vals_upp)) %>%
  mutate(pct_chg = vals_2022 - vals_1976,
         label   = scales::percent(pct_chg %>% round(2)))

### MOTHERS' EDUC
df_pc3 <- df3 %>%
  filter(year == 1976 | year == 2022) %>%
  pivot_wider(id_cols     = demo, 
              names_from  = year, 
              values_from = c(vals, vals_low, vals_upp)) %>%
  mutate(pct_chg = vals_2022 - vals_1976,
         label   = scales::percent(pct_chg %>% round(2)))

#### Combine dfs
df_pc1$cat    <- "Gender" 
df_pc2$cat    <- "Race" 
df_pc3$cat    <- "Mother's education" 

df_pc4 <- rbind(df_pc1, df_pc2, df_pc3)

df_pc4$cat <- factor(df_pc4$cat, levels = c("Gender", "Race", 
                                            "Mother's education"), ordered = FALSE)

# VISUALIZE ------------------------------------------------------------------
## Prep the data for plotting

### ALL
p1 <- df0 %>%
  ggplot(aes(x = year, y = vals, color = mar3, ymin = vals_low, ymax = vals_upp)) +
  stat_smooth(method = "lm", size = .5, fill = "grey80") +
  geom_pointrange(aes(shape=mar3, alpha = .5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_manual(values = c(mar3_palette)) +
  theme_minimal() +
  theme(strip.text.x        = element_text(face = "bold"),
        axis.title.y        = element_blank(),
        panel.grid.minor    = element_blank(),
        panel.grid.major    = element_blank(),
        plot.caption        = element_text(color = "grey70", face = "italic"),
        legend.position     = "none",
        plot.title.position = "plot") +
  geom_hline(yintercept=.5, color = "grey90") +
  labs( x        = " ", 
        y        = " ", 
        title    = "U.S. twelfth-graders marriage expectations",
        subtitle = "Panel A. % who think they will _______")

p1


### ALL DEMO
p2 <- df4 %>%
  ggplot(aes(x = year, y = vals, color = demo, ymin = vals_low, ymax = vals_upp)) +
  stat_smooth(method = "lm", size = .5, fill = "grey80") +
  geom_pointrange(aes(alpha = .9)) +
  facet_wrap("cat", ncol = 1)  +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(.5, 1), 
                     breaks = c(.5, .75, 1)) +
  scale_color_manual(values = c(c_palette, 
                                c_palette, 
                                c_palette)) +
  theme_minimal() +
  theme(strip.text          = element_text(hjust = 0),
        strip.text.x        = element_text(face = "italic"),
        axis.title.y        = element_blank(),
        panel.grid.minor    = element_blank(),
        panel.grid.major    = element_blank(),
        plot.caption        = element_text(color = "grey70", face = "italic"),
        legend.position     = "none",
        plot.title.position = "plot",
        plot.subtitle       = element_markdown()) +
  geom_hline(yintercept=.5, color = "grey90") +
  labs( x        = " ", 
        y        = " ",
        subtitle = "Panel B. % of U.S. twelfth-graders who think they will <span style = 'color: #18BC9C;'>'get married'</span>")
p2

### Graphing % point change

p3 <- df_pc0 %>%
  ggplot(aes(x=mar3, y= pct_chg, fill = mar3)) +
  geom_col(width = 0.5) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = label, vjust = -0.5)) +
  scale_y_continuous(limits = c(-.10, .1)) +
  scale_fill_manual(values  = c(mar3_palette)) +
  theme_minimal() +
  theme(axis.text.x         = element_markdown(face = "bold", colour = c(mar3_palette)),
        axis.text.y         = element_blank(), 
        axis.ticks.y        = element_blank(),
        panel.grid.minor    = element_blank(),
        panel.grid.major    = element_blank(),
        plot.caption        = element_text(color = "grey70", face = "italic"),
        legend.position     = "none") +
  scale_x_discrete(position = "top",
                   labels=c("Getting married" = "Get\nmarried", 
                            "Not getting married" = "Not get\nmarried",
                            "I have no idea" = "I have\nno idea")) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Percentage point change",
        subtitle = "1976 - 2022")
p3

p4 <- df_pc4 %>%
  ggplot(aes(x=demo, y= pct_chg, fill = demo)) +
  geom_col(aes(alpha = .9), width = 0.3) +
  geom_hline(yintercept = 0) +
  facet_wrap("cat", ncol = 1, scales = "free_x") +
  geom_text(aes(label = label, vjust = -0.5)) +
  scale_y_continuous(limits = c(-.15, 0.05)) +
  theme_minimal() +
  scale_fill_manual(values = c(c_palette, 
                               c_palette, 
                               c_palette)) +
  theme(strip.text.x       = element_blank(),
        axis.text.x        = element_markdown(colour = c(c_palette)),
        axis.text.y        = element_blank(), 
        axis.ticks.y       = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major   = element_blank(),
        plot.caption       = element_text(color = "grey70", face = "italic"),
        legend.position    = "none") +
  scale_x_discrete(position = "top",
                   labels=c("Men" = "Men", "Women" = "Women",
                            "White" = "White", "Black" = "Black",
                            "No college degree" = "No college\ndegree",
                            "Completed college" = "College\ndegree")) +
  labs( x        = " ", 
        y        = " ",
        caption  = "Monitoring the Future Surveys 1976-2022",
        title    = " ")
p4

### Put it all together
p <- ggarrange(p1, p3, p2, p4,
               ncol = 2, nrow = 2, heights = c(1, 2), widths = c(1.75,1))
p

ggsave("marfig.png", p, width = 6.5, height = 8, dpi = 300, bg = 'white')
