#==============================================================================
# 06-fig6-outlet-evaluations.R
# Purpose: to replicate Figure 6 of the paper, where we provide information about
#          how respondents in the treatment group evaluated the extremes sites
#          of the other ideology that they visited during the experiment.
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
# - dataset with self-reported and coded information for the URLs respondents 
#   visited as part of the experimental treatment.
db <- read.csv("./data/extreme-sites-coded-visited-urls.csv")

# DATA WRANGLING
#===============================================================================
# - keep only the info for the urls we coded as being about a political topic
db_pol <- db %>%
  filter(political == 1)

# - provide human-readable labels to some key variables to plot
urls_out <- db_pol %>%
  mutate(EXTREMITY = as.numeric(as.character(recode(as.character(EXTREMITY),
                                                    `Moderate` = "1",
                                                    `Somewhat extreme` = "4",
                                                    `8` = "6",
                                                    `Very Extreme` = "7"))),
         QUALITY = as.numeric(as.character(recode(as.character(QUALITY),
                                                  `Very poor quality` = "1",
                                                  `Poor quality` = "2",
                                                  `Acceptable quality` = "3",
                                                  `High quality` = "4",
                                                  `Very high quality` = "5")))) %>%
  filter(!is.na(outlet)) %>%
  # - calculate average values for key variables of interest
  group_by(outlet) %>%
  summarise(fake_pe = round(length(which(FAKE == "Yes"))/ n(), 3),
            fake_lwr = fake_pe - (1.96 * sqrt(fake_pe * (1-fake_pe)/n())),
            fake_upr = fake_pe + (1.96 * sqrt(fake_pe * (1-fake_pe)/n())),
            notneg_pe = round(
              length(which(sentiment %in% c("Neutral", "Positive"))) / n(), 4),
            notneg_lwr = notneg_pe - (1.9600 * sqrt(notneg_pe * (1-notneg_pe)/n())),
            notneg_upr = notneg_pe + (1.9600 * sqrt(notneg_pe * (1-notneg_pe)/n())),
            incivility_pe = round(sum(as.numeric(as.character(incivility))) /
                                    n(), 3),
            incivility_lwr = incivility_pe - (1.96 * sqrt(incivility_pe * (1-incivility_pe)/n())),
            incivility_upr = incivility_pe + (1.96 * sqrt(incivility_pe * (1-incivility_pe)/n())),
            extremity_pe = mean(EXTREMITY),
            extremity_lwr = t.test(EXTREMITY)$conf.int[1],
            extremity_upr = t.test(EXTREMITY)$conf.int[2],
            quality_pe = mean(QUALITY),
            quality_lwr = t.test(QUALITY)$conf.int[1],
            quality_upr = t.test(QUALITY)$conf.int[2]) 


# MAIN
#===============================================================================
# - visualize all these descriptive information
urls_toplot <- urls_out %>%
  gather(variable, value, -outlet)
urls_toplot$type <- as.character(
  sapply(as.character(urls_toplot$variable), function(x)
    strsplit(x, split = "_")[[1]][2]))
urls_toplot$variable <- as.character(
  sapply(as.character(urls_toplot$variable), function(x)
    strsplit(x, split = "_")[[1]][1]))

urls_toplot02 <- urls_toplot %>%
  spread(type, value)

# - specify some additional human-readable labels, and their order.
urls_toplot02$outlet <- factor(urls_toplot02$outlet,
                               levels = rev(c(
                                 "The American Spectator", "Breitbart", "The Blaze", 
                                 "Mother Jones", "Democracy Now", "The Nation"
                               )))

urls_toplot02$variable <- recode(urls_toplot02$variable,
                                 `extremity` = "Extremity\n{1-7} scale",
                                 `fake` = "False information\n{proportion}",
                                 `incivility` = "Incivil reactions\n{proportion}",
                                 `notneg` = "Neutral or Positive reaction\n{proportion}",
                                 `quality` = "Quality\n{1-5} scale")

urls_toplot02$variable <- factor(urls_toplot02$variable, levels = c(
  "False information\n{proportion}",
  "Incivil reactions\n{proportion}",
  "Neutral or Positive reaction\n{proportion}",
  "Extremity\n{1-7} scale",
  "Quality\n{1-5} scale"
))

# - add some empty observations in order to specify the range of each facet
add_rows <- data.frame(
  outlet = rep("Breitbart", 5),
  variable = c(
    "False information\n{proportion}",
    "Incivil reactions\n{proportion}",
    "Neutral or Positive reaction\n{proportion}",
    "Extremity\n{1-7} scale",
    "Quality\n{1-5} scale"),
  pe = c(rep(0.5, 3), 4, 3), 
  lwr = c(rep(0, 3), rep(1, 2)), 
  upr = c(rep(1, 3), 7, 5),
  type = "fake"
) %>%
  mutate(variable = factor(variable, levels = c(
    "False information\n{proportion}",
    "Incivil reactions\n{proportion}",
    "Neutral or Positive reaction\n{proportion}",
    "Extremity\n{1-7} scale",
    "Quality\n{1-5} scale"
  )))

urls_toplot03 <- rbind(urls_toplot02 %>% mutate(type = "real"), add_rows) %>%
  mutate(variable = factor(variable, levels = c(
    "False information\n{proportion}",
    "Incivil reactions\n{proportion}",
    "Neutral or Positive reaction\n{proportion}",
    "Extremity\n{1-7} scale",
    "Quality\n{1-5} scale"
  ))) %>%
  arrange(variable)


pdf("./figures/fig6-extreme-sites-outlet-evaluations.pdf", width = 11, height = 6)
ggplot(urls_toplot03,
       aes(x = outlet, y = pe, ymin = lwr, ymax = upr)) +
  geom_pointrange(aes(alpha = type)) +
  geom_hline(
    data = add_rows,
    aes(yintercept = pe)) +
  coord_flip() +
  facet_wrap(~variable, scales = "free_x", nrow = 2) +
  scale_alpha_discrete(range = c(0, 1), guide = FALSE) +
  xlab("") +
  ylab("") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 14),
        panel.spacing = unit(2, "lines"))
dev.off()
