#==============================================================================
# 01-fig1b-browsing-data.R
# Purpose: to replicate Figure 1b of the paper, in which we use the browsing 
#          data we collected for some of the participants assigned to treatment
#          to check whether on average they actually complied with the treatment.
# Article: "Exposure to extremely partisan news from the other political side 
#          shows scarce boomerang effects"
# Journal: Political Behavior
# Year:    2022
# Authors: Andreu Casas, Ericka Menchen-Trevino, Magdalena Wojcieszak
#==============================================================================


# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)


# DATA
#===============================================================================
# - browsing data for respondents in the treatment group. Contains information
#   about how many times they accessed the extreme sites of the opposing ideology
#   the days before, during, and after participating in the experiment. These
#   counts are based on historical browsing data they provided about 2.5 months 
#   after they participated in the experiment. We obtained this browsing data
#   from many but not all participants for 2 main reasons: 1) some decided to
#   not take part of W3 of the study in which we embedded our experiment (in W2),
#   and 2) some did take part of W3 and donated their browsing data, but not 
#   enough data going back to the time they participated in the experiment right
#   after W2 (so in other words, they provided < 75 days of past browsing data).

db <- read.csv("./data/extreme-sites-exp-browsing-data-toplot.csv")


# DATA WRANGLING
#===============================================================================
# - calculate the average number of visits to extreme sites of the opposing
#   ideology, by ideological group (liberals v. conservatives), and by day.
plotdb <- db02 %>%
  group_by(exp_group, t) %>%
  summarise(pe = mean(sum_extreme, na.rm = TRUE),
            lwr = ifelse(length(unique(na.omit(sum_extreme))) > 1,
                                 t.test(na.omit(sum_extreme))$conf.int[1],
                                 mean(sum_extreme, na.rm = TRUE)),
            upr = ifelse(length(unique(na.omit(sum_extreme))) > 1,
                                 t.test(na.omit(sum_extreme))$conf.int[2],
                                 mean(sum_extreme, na.rm = TRUE))) %>%
  gather(variable, value, -exp_group, -t)

plotdb$estimate_type <- as.character(sapply(plotdb$variable, function(x)
  strsplit(x, split = "_")[[1]][1]))

plotdb$var_type <- as.character(sapply(plotdb$variable, function(x)
  strsplit(x, split = "_")[[1]][2]))

# - human-readable labels for the ideological groups
plotdb <- plotdb %>%
  dplyr::select(-variable) %>%
  mutate(group = ifelse(exp_group == "cons_t", "Conservatives", "Liberals")) %>%
  spread(estimate_type, value)


# OUTPUT
#===============================================================================
# - un-comment to save a pdf version of the plot.
#pdf("./figures/fig1b-browsing-data.pdf", width = 10, height = 4)
ggplot(plot_db01 %>%
         filter(t < 31) %>%
         mutate(group = factor(group, levels = c("Liberals", "Conservatives"))), 
       aes(x = t, y = pe, fill = group)) +
  geom_line(aes(color = group), size = 0.3) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 12) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
  scale_x_continuous("\nNumber of days before/after respondents started the experiment",
                     breaks = seq(-5, 40, 5)) +
  scale_y_continuous("\nNumber of times respondents accessed\nextreme news domains of opposing ideology\n") +
  scale_fill_manual("", guide = FALSE, values = rev(c("firebrick3",
                                                      "dodgerblue4"))) +
  scale_color_manual("", guide = FALSE, values = rev(c("firebrick3",
                                                       "dodgerblue4"))) +
  #facet_grid(var_type ~ group, scales = "free") +
  facet_wrap(~group, nrow = 1) +
  theme(panel.background = element_blank(),
        axis.line.y = element_line(),
        strip.text = element_text(size = 14))
#dev.off()
