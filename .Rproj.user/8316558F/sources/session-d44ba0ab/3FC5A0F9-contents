##########################################################################
### This is the old version of Figure 1.
fig1 <- ggplot(mdata,
               aes(year, predicted, color = level, label = round(predicted, 1),
                   ymin = conf.low, ymax = conf.high)) +
  facet_wrap(~dol) +
  geom_linerange(show.legend=FALSE, color = "grey") +
  geom_line(size = 1) +
  theme_minimal() +
  ggtitle("\n") +
  scale_x_continuous(name = "", breaks = c(1976, 2014), label = c("'76", "'14")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(.5), limits = c(0, 1)) +
  theme(text             = element_text(size=12),
        legend.position  = "bottom",
        legend.title     = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x  = unit(10, "lines"),
        panel.spacing.y  = unit(1, "lines")) +
  labs(x = " ", y = " ", fill = "") +
  scale_color_brewer(palette="Dark2") +
  geom_hline(yintercept = .5,   color = "grey90") +
  geom_vline(xintercept = 1995, color = "grey90")


fig1 <- ggdraw(fig1) + draw_label("Traditional",     x = 0.15, y = 0.95, fontface='bold', size = 12)
fig1 <- ggdraw(fig1) + draw_label("Matched",         x = 0.53, y = 0.95, fontface='bold', size = 12)
fig1 <- ggdraw(fig1) + draw_label("Gender Atypical", x = 0.91, y = 0.95, fontface='bold', size = 12)

fig1

ggsave("figures/dol_figure 1.png", fig1, width = 9, height = 5, dpi = 300)

##########################################################################
# Create old version of Figure 1 with Race/Gender trends
setwd(file.path(repoDir)) # Set the working-directory to the respository

## Create predicted probilities datesets

pm1   <- ggeffect(m1, terms = c("year[1976:2014]", "racesex"))
pm2   <- ggeffect(m2, terms = c("year[1976:2014]", "racesex"))
pm3   <- ggeffect(m3, terms = c("year[1976:2014]", "racesex"))
pm4   <- ggeffect(m4, terms = c("year[1976:2014]", "racesex"))
pm5   <- ggeffect(m5, terms = c("year[1976:2014]", "racesex"))
pm6   <- ggeffect(m6, terms = c("year[1976:2014]", "racesex"))

pm1$dol <- "hfw0"
pm2$dol <- "hfwh"
pm3$dol <- "hfwf"
pm4$dol <- "hhwh"
pm5$dol <- "h0wf"
pm6$dol <- "hhwf"

mdata = smartbind(pm1, pm2, pm3, pm4, pm5, pm6)
head(mdata)

mdata$dol <- as.factor(mdata$dol)
levels(mdata$dol)[levels(mdata$dol)=="hfw0"] <- "Husband full-time;\n Wife at home"
levels(mdata$dol)[levels(mdata$dol)=="hfwh"] <- "Husband full-time;\n Wife part-time"
levels(mdata$dol)[levels(mdata$dol)=="hfwf"] <- "Both work full-time"
levels(mdata$dol)[levels(mdata$dol)=="hhwh"] <- "Both work part-time"
levels(mdata$dol)[levels(mdata$dol)=="hhwf"] <- "Husband part-time;\n Wife full-time"
levels(mdata$dol)[levels(mdata$dol)=="h0wf"] <- "Husband at home;\n Wife full-time"

mdata$dol    <- ordered(mdata$dol,   levels = c("Husband full-time;\n Wife at home", "Husband full-time;\n Wife part-time", "Both work full-time", 
                                                "Both work part-time", "Husband at home;\n Wife full-time", "Husband part-time;\n Wife full-time"))

colnames(mdata)[colnames(mdata)=="response.level"] <- "level"
mdata$level <- as_factor(mdata$level, ordered = TRUE)
levels(mdata$level)[levels(mdata$level)=="NOT.AT.ALL.ACCEPTABLE"] <- "NOT AT ALL\n ACCEPTABLE"
levels(mdata$level)[levels(mdata$level)=="SOMEWHAT.ACCEPTABLE"] <- "SOMEWHAT\n ACCEPTABLE"

mdata$level <- ordered(mdata$level, levels = c("DESIRABLE", "ACCEPTABLE", "SOMEWHAT\n ACCEPTABLE",  "NOT AT ALL\n ACCEPTABLE"))

mdata$group <- ordered(mdata$group, levels = c("White men", "White women", "Black men", "Black women"))

colnames(mdata)[colnames(mdata)=="x"] <- "year"

write.csv(mdata, "figures/dol_Figure 2.csv")

figAC <- mdata %>%
  ggplot(aes(year, predicted, color = group, label = round(predicted, 1))) +
  geom_line(aes(linetype = group), size =1) +
  facet_grid(~ level ~ dol) +
  scale_linetype_manual(values=c("solid", "twodash", "longdash", "dotted"))+
  theme_minimal() +
  scale_x_continuous(name = "", breaks = c(1976, 2014), label = c("'76", "'14")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0, .25, .5, .75)) +
  theme(axis.text.x      = element_text(size = 9),
        strip.text.x     = element_text(size = 8, face = "bold"),
        strip.text.y     = element_text(size = 9, face = "bold"),
        axis.title       = element_text(size = 9), 
        axis.text        = element_text(size = 9), 
        legend.position  = 'top',
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.spacing = unit(1.25, "lines")) +
  labs(x = " ", y = "\n", fill = "") +
  scale_color_manual(values=c("#fdb863", "#e66101", "#b2abd2", "#5e3c99")) +
  geom_hline(yintercept = .5,   color = "grey90") +
  geom_vline(xintercept = 1995, color = "grey90")

figAC