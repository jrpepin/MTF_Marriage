
## % who think they will be _______ as a spouse
pp1 <- df1 %>%
  ggplot(aes(x = year, y = vals, color = goodsp, shape = goodsp, ymin = vals_low, ymax = vals_upp)) +
  geom_hline(yintercept  = c(0, .25, .5, .75), color = "grey90") +
  geom_smooth(method     = loess, fill = "grey80", linewidth = .85) +
  geom_point(aes(alpha   = .7), show.legend = FALSE) +
  # Only plot the labels 1 time -- using 2021 for not so good/poor order
  geom_text(data         = subset(df1, sex == "Women" & year == 2021 & goodsp != "Fairly good" & goodsp != "Not so good"), 
            aes(label    = goodsp), 
            nudge_x      = 3,
            hjust        = 0,
            size        = 5)   +
  geom_text(data         = subset(df1, sex == "Women" & year == 2021 & (goodsp == "Fairly good" | goodsp == "Not so good")), 
            aes(label    = goodsp), 
            nudge_x      = 3,
            nudge_y      = .03, # adding space between labels
            hjust        = 0,
            size         = 5)   +
  facet_wrap("sex", scales = "free_x")  +
  coord_cartesian(clip = 'off') +   # Allow labels to bleed past the canvas boundaries
  theme_minimal(20) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, .75), 
                     breaks = c(.25, .5, .75)) +
  scale_x_continuous(limit  = c(1976, 2024),
                     breaks = c(1976, 2022)) +
  theme(strip.text.x        = element_text(face = "bold.italic", size = 20, hjust = 0),
        axis.title.y        = element_blank(),
        panel.spacing       = unit(2, "lines"),
        panel.grid.minor    = element_blank(),
        panel.grid.major    = element_blank(),
        legend.position     = "none",
        plot.title.position = "plot",
        plot.margin         = unit(c(0.25, 1, 0.00, 0.25), "inches")) +
  scale_color_manual(values = c(goodsp_palette)) +
  labs( x        = NULL, 
        y        = NULL,
        title    = NULL)

pp1

ggsave(file.path(outDir, "pp1.png"), pp1, width = 7, height = 4, dpi = 300) 



### % who think they will _______
pp2 <- df2 %>%
  ggplot(aes(x = year, y = vals, color = mar3_lbl, shape = mar3_lbl, ymin = vals_low, ymax = vals_upp)) +
  geom_hline(yintercept  = c(0, .25, .5, .75), color = "grey90") +
  geom_smooth(method     = loess, fill = "grey80", linewidth = .85) +
  geom_point(aes(alpha   = .7), show.legend = FALSE) +
  geom_text(data           = subset(df2, sex == "Men" & year == 2022), # Only plot the labels 1 time
            aes(label      = mar3_lbl), 
            nudge_x        = 2,
            hjust          = 0,
            size           = 5)   +
  facet_wrap("sex", scales = "free_x")  +
  coord_cartesian(clip = 'off') +   # Allow labels to bleed past the canvas boundaries
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 1), 
                     breaks = c(.25, .5, .75)) +
  scale_x_continuous(limit  = c(1976, 2024),
                     breaks = c(1976, 2022)) +
  scale_color_manual(values = c(mar3_palette)) +
  scale_shape_manual(values = c(16, 15, 12)) +
  theme_minimal(20) +
  theme(strip.text.x        = element_text(face = "bold.italic", size = 20, hjust = 0),
        axis.title.y        = element_blank(),
        panel.spacing       = unit(2, "lines"),
        panel.grid.minor    = element_blank(),
        panel.grid.major    = element_blank(),
        legend.position     = "none",
        plot.title.position = "plot",
        plot.margin         = unit(c(0.25, 0.25, 0.00, 0.25), "inches")) +
  labs( x        = NULL, 
        y        = NULL,
        title    = NULL)
pp2

ggsave(file.path(outDir, "pp2.png"), pp2, width = 7, height = 4, dpi = 300) 
