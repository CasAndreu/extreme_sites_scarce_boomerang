#==============================================================================
# 02-fig2-ideo-scores.R
# Purpose: to replicate Figure 2 of the paper, where we show the distribution
#          of the ideology scores (from Robertson et al. 2018) of the extreme 
#          sites to which respondents in the treatment group were assigned, as
#          well as the scores for some well-known sites, for face-validity 
#          purposes.
# Article: "Exposure to extremely partisan news from the other political side 
#          shows scarce boomerang effects"
# Journal: Political Behavior
# Year:    2022
# Authors: Andreu Casas, Ericka Menchen-Trevino, Magdalena Wojcieszak
#==============================================================================


# PACKAGES
#===============================================================================
library(dplyr)
library(ggplot2)
library(ggrepel)

# DATA
#===============================================================================
# - ideology scores from Robertson et al. 2018, for the sites plotted in Fig.2
db <- read.csv("./data/robertson-fig2-sites-ideo-scores.csv")


# DATA WRANGLING
#===============================================================================
# - a variable indicating which of these news sites were part of the treatment
db <- db %>%
  mutate(treatment_site = ifelse(
    news_site %in% c("democracynow.org", "thenation.com", "motherjones.com",
                     "breitbart.com", "theblaze.com", "spectator.org"),
    1, 0
  ))

# - a variable specifying which news-site labels to show above v. below the 
#   x-axis in the figure
db <- db %>%
  mutate(above_x = ifelse(
    news_site %in% c("motherjones.com", "nytimes.com", "cnn.com", "cbs.com", 
                     "nypost.com", "foxnews.com", "breitbart.com",
                     "spectator.org"), 1, 0
  ))


# OUTPUT
#===============================================================================
# - the plot. Un-comment to generate a pdf version in the "./figures/" dir.
#   Please note that the exact position of the labels for the news sites may
#   slightly change since the ggrepel package decides location at random each
#   time
pdf("./figures/fig2-ideo-scores.pdf", width = 10, height = 4)
set.seed(42)
ggplot(db,
       aes(x = ideo_score, y = 0)) + 
  geom_point(aes(color = factor(treatment_site)), size = 2) +
  geom_text_repel(data = db %>% filter(above_x == 1),
                  aes(label = news_site, color = factor(treatment_site)),
                  direction = "y",
                  nudge_y = 0.25) +
  geom_text_repel(data = db %>% filter(above_x == 0),
                  aes(label = news_site, color = factor(treatment_site)),
                  direction = "y",
                  nudge_y = -0.25) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_y_continuous("", breaks = NULL) +
  scale_x_continuous("\n\nIdeology score of news sites",
                     limits = c(-1, 1),
                     breaks = seq(-0.75, 0.75, 0.25),
                     labels = c("Extremely\nliberal", "-0.5", "-0.25",
                                "Moderate", "0.25", "0.5",
                                "Extremely\nconservative")) +
  scale_color_manual(values = c("gray60", "black")) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.x = element_text(vjust = -0.5),
        axis.title = element_text(size = 10))
dev.off()
