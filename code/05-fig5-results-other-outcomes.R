#==============================================================================
# 05-fig5-results-other-outcomes.R
# Purpose: to replicate Figure 5 of the paper, where we show the main (and 
#          moderator) effects of the treatment on many additional outcomes. We
#          first generate Figure 5.A and then Figure 5.B. For the paper, we then
#          pasted the two subfigures together.
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

# DATA
#===============================================================================
# - dataset with data from the pre and post survey, for the many outcome and
#   moderator variables of interest.
db <- read.csv("./data/extreme-sites-experiment-maindb.csv")

# DATA WRANGLING
#===============================================================================
# OTHER OUTCOMES
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# - create indices averaging scores for items in the same battery
db <- db %>%
  mutate(
    # - /!\ REVERSE items of trust in inst battery so that higher values
    #       mean that people polarized
    TRUST_ALL = -(TRUST_1 + TRUST_2 + TRUST_3 + TRUST_4 + TRUST_5 +
                    TRUST_6) / 6,
    # - /!\ REVERSE items of freedom of speech battery so that higher values
    #       mean that people polarized
    SPEECH_ALL = -(SPEECH_1 + SPEECH_2 + SPEECH_3 + SPEECH_4) / 4,
    SMS_ALL = (SMS_1 + SMS_2 + SMS_3) / 3,
    # - /!\ REVERSE some of the items in the well being battery so higher
    #       values mean polarization (worse well being)
    WELLBEING_ALL = (WELL_FEELINGS_1 + WELL_FEELINGS_2 + 
                       (-WELL_FEELINGS_3) + (-WELL_FEELINGS_4) +
                       (-WELL_FEELINGS_5) + (-WELL_FEELINGS_6)) / 6,
    # - /!\ REVERSE exercise so higher values mean healthier habit
    WELLACT_ALL = (WELL_ACT_1 + WELL_ACT_2 + WELL_ACT_3 + WELL_ACT_4 +
                     (-WELL_ACT_5)) / 5
  )


# - calculate ITT and CACE for High Compliers
outcomes <- c("MAL", "PREPOL", "COMPROMISE", "TRUST_ALL", "SPEECH_ALL",
              "SMS_ALL", "WELLBEING_ALL", "WELLACT_ALL")

# - specify two samples: ITT and High compliers
compl_groups <- c("Intention to Treat\nN = 505", 
                  "High compliance\n(5-6 story surveys)\nN=337")

rest_out <- NULL
for (outcome in outcomes){
  # - calculate SD of pre-treatment values
  pre_sd <- sd(db[,outcome], na.rm = TRUE)
  for (cgroup in compl_groups) {
    if (grepl("High", cgroup)) {
      run_db <- db %>%
        filter(
          is.na(compliance_level) |
            compliance_level == "High Compliance \n(5-6 story surveys)\nN = 337")
    } else {
      run_db <- db
    }
    # - calculate treatment effect /!\ NOT CONTROLLING FOR PRE because we are
    #                                  already using the DELTAs for the outcomes
    #                                  for which we have PRE and POST
    model_formula <- paste0(outcome, " ~ condition")
    model_out <- broom::tidy(lm(model_formula, 
                                data = run_db)) %>%
      filter(term == "condition") %>% 
      mutate(significant = ifelse(p.value < 0.05, 1, 0)) %>%
      mutate(outcome = outcome,
             sample = cgroup,
             sd = pre_sd,
             pe = estimate / pre_sd,
             se_std = std.error / pre_sd,
             lwr_90 = pe - (se_std * 1.64),
             lwr_95 = pe - (se_std * 1.96),
             upr_90 = pe + (se_std * 1.64),
             upr_95 = pe + (se_std * 1.96))
    rest_out <- rbind(rest_out, model_out) 
  }
}

# - the plot
rest_out02 <- rest_out %>%
  # - /!\ REVERSE results for Support for Compromise (so higher means > polariz.)
  mutate(pe = ifelse(outcome == "PREPOL", -pe, pe),
         lwr_90 = ifelse(outcome == "PREPOL", -lwr_90, lwr_90),
         upr_90 = ifelse(outcome == "PREPOL", -upr_90, upr_90),
         lwr_95 = ifelse(outcome == "PREPOL", -lwr_95, lwr_95),
         upr_95 = ifelse(outcome == "PREPOL", -upr_95, upr_95)
  ) %>%
  # - give human readable labels to outcome variables
  mutate(
    outcome = factor(recode(outcome,
                            `MAL` = "Attribution of malevolence",
                            `PREPOL` = "Perceived polarization",
                            `COMPROMISE` = "Against political compromise",
                            `TRUST_ALL` = "Distrust in institutions",
                            `SPEECH_ALL` = "Against freedom of speech",
                            `SMS_ALL` = "Against freedom of press",
                            `WELLBEING_ALL` = "Lower well-being",
                            `WELLACT_ALL` = "Unhealthy habits"),
                     levels = rev(c(
                       "Attribution of malevolence",
                       "Perceived polarization",
                       "Against political compromise",
                       "Distrust in institutions",
                       "Against freedom of speech",
                       "Against freedom of press",
                       "Lower well-being",
                       "Unhealthy habits"
                     )))) %>%
  arrange(outcome) %>%
  mutate(significant = as.character(significant),
         sample = recode(sample,
                         `High compliance\n(5-6 story surveys)\nN=337` =
                           "High compliance\n(5-6 story surveys)\nN = 337"))

# - the plot
pdf("./figures/fig5a-extreme-sites-other-out-MAIN.pdf", width = 9, height = 4)
ggplot(rest_out02,
       aes(x = outcome, y = pe)) +
  # - this is so I can have a legend indicating what the 2 colors mean
  geom_point(aes(x = 9, y = 0, color = sample)) +
  scale_color_manual("", values = c("mediumorchid4",
                                    "darkolivegreen4")) +
  # - plot 90% and 95% ITT estimates
  #geom_point()
  geom_point(inherit.aes = FALSE,
             data = rest_out02 %>% filter(grepl("Intent", sample)),
             aes(x = as.numeric(outcome) + 0.15, y = pe,
                 alpha = significant), 
             size = 3, color = "darkolivegreen4") +
  geom_segment(inherit.aes = FALSE, 
               data = rest_out02 %>% filter(grepl("Intent", sample)),
               aes(x = as.numeric(outcome) + 0.15, xend = as.numeric(outcome) + 0.15,
                   y = lwr_90, yend = upr_90, alpha = significant), size = 1.5, 
               color = "darkolivegreen4") +
  geom_segment(inherit.aes = FALSE,
               data = rest_out02 %>% filter(grepl("Intent", sample)),
               aes(x = as.numeric(outcome) + 0.15, xend = as.numeric(outcome) + 0.15,
                   y = lwr_95, yend = upr_95, alpha = significant), size = 0.75,
               color = "darkolivegreen4") +
  # - plot 90% and 95% High compliers CACE estimates
  geom_point(inherit.aes = FALSE,
             data = rest_out02 %>% filter(!grepl("Intent", sample)),
             aes(x = as.numeric(outcome) - 0.15, y = pe,
                 alpha = significant), size = 3,
             color = "mediumorchid4") +
  geom_segment(inherit.aes = FALSE,
               data = rest_out02 %>% filter(!grepl("Intent", sample)),
               aes(x = as.numeric(outcome) - 0.15, xend = as.numeric(outcome) - 0.15,
                   y = lwr_90, yend = upr_90, alpha = significant), size = 1.5,
               color = "mediumorchid4") +
  geom_segment(inherit.aes = FALSE,
               data = rest_out02 %>% filter(!grepl("Intent", sample)),
               aes(x = as.numeric(outcome) - 0.15, xend = as.numeric(outcome) - 0.15,
                   y = lwr_95, yend = upr_95, alpha = significant), size = 0.75,
               color = "mediumorchid4") +
  scale_x_continuous("", breaks = seq(1, length(levels(rest_out02$outcome))),
                     labels = levels(rest_out02$outcome),
                     limits = c(0.5,8.5)) +
  scale_y_continuous("\nTreatment Effect (in standard deviation changes)",
                     breaks = seq(-1, 1, 0.05),
                     limits = c(-0.20, 0.35)) +
  geom_hline(yintercept = 0, color = "red") +
  # - the rest 
  scale_alpha_discrete(range = c(0.35, 1), guide = FALSE) +
  coord_flip() +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 10),
        legend.key.size = unit(1.2, "cm"),
        legend.text = element_text(size = 8))
dev.off()


# REST OUTCOMES: Moderators
#-------------------------------------------------------------------------------
moderators <- c("party_folded", "ideo_folded", "pid_strength")

rest_modout <- NULL
for (outcome in outcomes){
  # - calculate SD of pre-treatment values
  pre_sd <- sd(db[,outcome], na.rm = TRUE)
  # - using ALL users in the sample to calculate these moderator effects
  run_db <- db
  # - calculate moderation effect
  for (moderator in moderators) {
    model_formula <- paste0(outcome, " ~ condition * ", moderator)
    model_out <- broom::tidy(lm(model_formula, 
                                data = run_db)) %>%
      filter(grepl(":", term)) %>% 
      mutate(significant = ifelse(p.value < 0.05, 1, 0)) %>%
      mutate(outcome = outcome,
             moderator = moderator,
             sd = pre_sd,
             pe = estimate / pre_sd,
             se_std = std.error / pre_sd,
             lwr_90 = pe - (se_std * 1.64),
             lwr_95 = pe - (se_std * 1.96),
             upr_90 = pe + (se_std * 1.64),
             upr_95 = pe + (se_std * 1.96))
    rest_modout <- rbind(rest_modout, model_out) 
  }
}

# - a variable indicating type of moderator
rest_modout02 <- rest_modout %>%
  mutate(term = recode(term,
                       `condition:ideo_folded` = "Ideology strength\n{6-point scale}",
                       `condition:pid_strength` = "Party identity strength\n{7-point scale}",
                       `condition:party_folded` = "Party strength\n{4-point scale}")) %>%
  mutate(significant = as.character(significant),
         term = as.character(term),
         outcome = factor(recode(outcome,
                                 `MAL` = "Attribution of malevolence",
                                 `PREPOL` = "Perceived polarization",
                                 `COMPROMISE` = "Against political compromise",
                                 `TRUST_ALL` = "Distrust in institutions",
                                 `SPEECH_ALL` = "Against freedom of speech",
                                 `SMS_ALL` = "Against freedom of press",
                                 `WELLBEING_ALL` = "Lower well-being",
                                 `WELLACT_ALL` = "Unhealthy habits"),
                          levels = rev(c(
                            "Attribution of malevolence",
                            "Perceived polarization",
                            "Against political compromise",
                            "Distrust in institutions",
                            "Against freedom of speech",
                            "Against freedom of press",
                            "Lower well-being",
                            "Unhealthy habits"
                          ))))

pdf("./figures/fig5b-extreme-sites-other-out-MOD.pdf", width = 9, height = 4)
ggplot(rest_modout02,
       aes(x = outcome, y = pe)) +
  # - this is so I can have a legend indicating what the 3 colors mean
  geom_point(aes(x = 10, y = 0, color = term)) +
  scale_color_manual("", values = c("darkolivegreen4",
                                    "orange3",
                                    "mediumorchid4")) +
  # - plot 90% and 95% IDEO STRENGTH moderation estimates
  geom_point(inherit.aes = FALSE,
             data = rest_modout02 %>% filter(grepl("Ideo", term)),
             aes(x = as.numeric(outcome) + 0.2, y = pe,
                 alpha = significant),
             size = 3, color = "darkolivegreen4") +
  geom_segment(inherit.aes = FALSE, 
               data = rest_modout02 %>% filter(grepl("Ideo", term)),
               aes(x = as.numeric(outcome) + 0.2, xend = as.numeric(outcome) + 0.2,
                   y = lwr_90, yend = upr_90,
                   alpha = significant), size = 1.5, 
               color = "darkolivegreen4") +
  geom_segment(inherit.aes = FALSE,
               data = rest_modout02 %>% filter(grepl("Ideo", term)),
               aes(x = as.numeric(outcome) + 0.2, xend = as.numeric(outcome) + 0.2,
                   y = lwr_95, yend = upr_95,
                   alpha = significant), size = 0.75,
               color = "darkolivegreen4") +
  # - plot 90% and 95% PARTY STRENGTH moderator estimates
  geom_point(inherit.aes = FALSE,
             data = rest_modout02 %>% filter(grepl("Party stre", term)),
             aes(x = as.numeric(outcome) - 0, y = pe,
                 alpha = significant), size = 3,
             color = "mediumorchid4") +
  geom_segment(inherit.aes = FALSE,
               data = rest_modout02 %>% filter(grepl("Party stre", term)),
               aes(x = as.numeric(outcome) - 0, xend = as.numeric(outcome) - 0,
                   y = lwr_90, yend = upr_90, alpha = significant), size = 1.5,
               color = "mediumorchid4") +
  geom_segment(inherit.aes = FALSE,
               data = rest_modout02 %>% filter(grepl("Party stre", term)),
               aes(x = as.numeric(outcome) - 0, xend = as.numeric(outcome) - 0,
                   y = lwr_95, yend = upr_95, alpha = significant), size = 0.75,
               color = "mediumorchid4") +
  # - plot 90% and 95% PARTY IDENTITY STRENGTH moderator
  geom_point(inherit.aes = FALSE,
             data = rest_modout02 %>% filter(grepl("Party ident", term)),
             aes(x = as.numeric(outcome) - 0.2, y = pe,
                 alpha = significant), size = 3,
             color = "darkgoldenrod2") +
  geom_segment(inherit.aes = FALSE,
               data = rest_modout02 %>% filter(grepl("Party ident", term)),
               aes(x = as.numeric(outcome) - 0.2, xend = as.numeric(outcome) - 0.2,
                   y = lwr_90, yend = upr_90, alpha = significant), size = 1.5,
               color = "darkgoldenrod2") +
  geom_segment(inherit.aes = FALSE,
               data = rest_modout02 %>% filter(grepl("Party ident", term)),
               aes(x = as.numeric(outcome) - 0.2, xend = as.numeric(outcome) - 0.2,
                   y = lwr_95, yend = upr_95, alpha = significant), size = 0.75,
               color = "darkgoldenrod2") +
  scale_x_continuous("", breaks = seq(1, length(levels(rest_modout02$outcome))),
                     labels = levels(rest_modout02$outcome),
                     limits = c(0.5, 8.5)) +
  scale_y_continuous("\nMarginal effect of a 1 point increase in ideology, party and party identity strength\n(in standard deviation changes)",
                     breaks = seq(-1, 1, 0.05),
                     limits = c(-0.20, 0.35)) +
  geom_hline(yintercept = 0, color = "red") +
  # - the rest
  scale_alpha_discrete(range = c(0.25, 1), guide = FALSE) +
  coord_flip() +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 10),
        legend.key.size = unit(1.2, "cm"),
        legend.text = element_text(size = 8))
dev.off()
