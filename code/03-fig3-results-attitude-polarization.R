#==============================================================================
# 03-fig3-results-attitude-polarization.R
# Purpose: to replicate Figure 3 of the paper, where we show the main (and 
#          moderator) effects of the treatment on attitude polarization. We
#          first generate Figure 3.A and then Figure 3.B. For the paper, we then
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
library(ggrepel)

# DATA
#===============================================================================
# - dataset with data from the pre and post survey, for the many outcome and
#   moderator variables of interest.
db <- read.csv("./data/extreme-sites-experiment-maindb.csv")


# MAIN
#===============================================================================

#-------------------------------------------------------------------------------
# [ Figure 3.A ] MAIN (ITT and CACE) EFFECTS
#-------------------------------------------------------------------------------
# - specify outcomes of interest (issue attitudes)
outcomes <-  c(
  # - attitudes on economic issues (last one is an index combining the 3)
  "ECON_1", "ECON_2", "ECON_3", "ECON",
  # - attitudes on climate change (last one is an index combining the 3)
  "ENV_1", "ENV_2", "ENV_3", "ENV",
  # - attitudes on immigration (last one is an index combining the 3)
  "IMMIG_1", "IMMIG_2", "IMMIG_3", "IMMIG",
  # - attitudes on gun control (last one is an index combining the 3)
  "GUN_1", "GUN_2", "GUN_3", "GUN",
  # - attitudes on President Trump (last one is an index combining the 3)
  "TRUMP_1", "TRUMP_2", "TRUMP_3", "TRUMP")
  
# - calculate changes in issue attitudes between pre and post survey
for (outcome in outcomes) {
  db[, paste0(outcome, "_DELTA")] <- db[,paste0(outcome, "_POST")] -
    db[,paste0(outcome, "_PRE")]
  # - reverse for those in the LIB group, so higher values mean POLARIZATION
  db[which(grepl("lib", as.character(db$exp_group))), 
           paste0(outcome, "_DELTA")] <- -db[
             which(grepl("lib", as.character(db$exp_group))), 
             paste0(outcome, "_DELTA")]
}

# - specify the two groups/samples for which we want to calculate treatment
#   effects: all assigned to treatment (ITT) and high compliers (CACE)
compl_groups <- c("Intention to Treat\nN = 505", 
                  "High compliance\n(5-6 story surveys)\nN=337")

# - calculate treatment effects
att_out <- NULL
for (outcome in outcomes){
  # - calculate SD of pre-treatment values
  pre_sd <- sd(db[,paste0(outcome, "_PRE")], na.rm = TRUE)
  for (cgroup in compl_groups) {
    if (grepl("High", cgroup)) {
      run_db <- db %>%
        filter(
          is.na(compliance_level) |
            compliance_level == "High Compliance \n(5-6 story surveys)\nN = 337")
    } else {
      run_db <- db
    }
    # - calculate treatment effect
    model_formula <- paste0(outcome, "_DELTA ~ condition")
    model_out <- broom::tidy(lm(model_formula, 
                                data = run_db)) %>%
      filter(term == "condition") %>% 
      mutate(significant = ifelse(p.value < 0.05, 1, 0)) %>%
      mutate(outcome = outcome,
             sample = cgroup,
             sd = pre_sd,
             # - express treatment effect in SD changes
             pe = estimate / pre_sd,
             se_std = std.error / pre_sd,
             lwr_90 = pe - (se_std * 1.64),
             lwr_95 = pe - (se_std * 1.96),
             upr_90 = pe + (se_std * 1.64),
             upr_95 = pe + (se_std * 1.96))
    att_out <- rbind(att_out, model_out) 
  }
}

# - prepare data to be plotted. Give human-readable labels to variables/classes.
att_out02 <- att_out %>%
  filter(!grepl("_", outcome)) %>%
  mutate(
    outcome = recode(outcome,
                     `TRUMP` = "Presidency of Donald Trump",
                     `IMMIG` = "Immigration",
                     `GUN` = "Gun control",
                     `ENV` = "Climate change",
                     `ECON` = "Economy"
    )) %>%
  arrange(desc(pe)) %>%
  mutate(outcome = factor(outcome, levels = unique(outcome)),
         significant = as.character(significant),
         sample = recode(sample,
                         `High compliance\n(5-6 story surveys)\nN=337` =
                           "High compliance\n(5-6 story surveys)\nN = 337"))

#pdf("./figures/fig3b-extreme-sites-att-pol-MAIN.pdf", width = 9, height = 4)
ggplot(att_out02 %>%
         filter(!grepl("_", outcome)),
       aes(x = outcome, y = pe)) +
  # - this is so I can have a legend indicating what the 2 colors mean
  geom_point(aes(x = 9, y = 0, color = sample)) +
  scale_color_manual("", values = c("mediumorchid4",
                                    "darkolivegreen4")) +
  # - plot 90% and 95% ITT estimates
  #geom_point()
  geom_point(inherit.aes = FALSE,
             data = att_out02 %>% filter(grepl("Intent", sample)),
             aes(x = as.numeric(outcome) + 0.15, y = pe,
                 alpha = significant), 
             size = 3, color = "darkolivegreen4") +
  geom_segment(inherit.aes = FALSE, 
               data = att_out02 %>% filter(grepl("Intent", sample)),
               aes(x = as.numeric(outcome) + 0.15, xend = as.numeric(outcome) + 0.15,
                   y = lwr_90, yend = upr_90, alpha = significant), size = 1.5, 
               color = "darkolivegreen4") +
  geom_segment(inherit.aes = FALSE,
               data = att_out02 %>% filter(grepl("Intent", sample)),
               aes(x = as.numeric(outcome) + 0.15, xend = as.numeric(outcome) + 0.15,
                   y = lwr_95, yend = upr_95, alpha = significant), size = 0.75,
               color = "darkolivegreen4") +
  # - plot 90% and 95% High compliers CACE estimates
  geom_point(inherit.aes = FALSE,
             data = att_out02 %>% filter(!grepl("Intent", sample)),
             aes(x = as.numeric(outcome) - 0.15, y = pe,
                 alpha = significant), size = 3,
             color = "mediumorchid4") +
  geom_segment(inherit.aes = FALSE,
               data = att_out02 %>% filter(!grepl("Intent", sample)),
               aes(x = as.numeric(outcome) - 0.15, xend = as.numeric(outcome) - 0.15,
                   y = lwr_90, yend = upr_90, alpha = significant), size = 1.5,
               color = "mediumorchid4") +
  geom_segment(inherit.aes = FALSE,
               data = att_out02 %>% filter(!grepl("Intent", sample)),
               aes(x = as.numeric(outcome) - 0.15, xend = as.numeric(outcome) - 0.15,
                   y = lwr_95, yend = upr_95, alpha = significant), size = 0.75,
               color = "mediumorchid4") +
  scale_x_continuous("", breaks = seq(1, length(levels(att_out02$outcome))),
                     labels = levels(att_out02$outcome),
                     limits = c(0.5,5.5)) +
  scale_y_continuous("\nTreatment Effect (in standard deviation changes)",
                     breaks = seq(-1, 1, 0.05),
                     limits = c(-0.22, 0.12)) +
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

#-------------------------------------------------------------------------------
# [ Figure 3.B ] MODERATOR EFFECTS
#-------------------------------------------------------------------------------
# - specify list of moderators
moderators <- c("party_folded", "ideo_folded", "pid_strength")

att_modout <- NULL
for (outcome in outcomes){
  # - calculate SD of pre-treatment values
  pre_sd <- sd(db[,paste0(outcome, "_PRE")], na.rm = TRUE)
  # - using ALL users in the sample to calculate these moderator effects
  run_db <- db
  # - calculate moderation effect
  for (moderator in moderators) {
    model_formula <- paste0(outcome, "_DELTA ~ condition * ", moderator)
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
    att_modout <- rbind(att_modout, model_out) 
  }
}

# - provide human-readable names for variables/classes
att_modout02 <- att_modout %>%
  filter(!grepl("_", outcome)) %>%
  mutate(term = recode(term,
                       `condition:ideo_folded` = "Ideology strength\n{6-point scale}",
                       `condition:pid_strength` = "Party identity strength\n{7-point scale}",
                       `condition:party_folded` = "Party strength\n{4-point scale}")) %>%
  mutate(significant = as.character(significant),
         term = as.character(term),
         outcome = factor(recode(outcome,
                                 `TRUMP` = "Presidency of Donald Trump",
                                 `IMMIG` = "Immigration",
                                 `GUN` = "Gun control",
                                 `ENV` = "Climate change",
                                 `ECON` = "Economy"),
                          levels = c(
                            "Climate change",
                            "Presidency of Donald Trump",
                            "Immigration",
                            "Economy",
                            "Gun control"
                          )))

#pdf("./figures/fig3b-extreme-sites-att-pol-MOD.pdf", width = 9, height = 4)
ggplot(att_modout02,
       aes(x = outcome, y = pe)) +
  # - this is so I can have a legend indicating what the 3 colors mean
  geom_point(aes(x = 10, y = 0, color = term)) +
  scale_color_manual("", values = c("darkolivegreen4",
                                    "orange3",
                                    "mediumorchid4")) +
  # - plot 90% and 95% IDEO STRENGTH moderation estimates
  geom_point(inherit.aes = FALSE,
             data = att_modout02 %>% filter(grepl("Ideo", term)),
             aes(x = as.numeric(outcome) + 0.2, y = pe,
                 alpha = significant),
             size = 3, color = "darkolivegreen4") +
  geom_segment(inherit.aes = FALSE, 
               data = att_modout02 %>% filter(grepl("Ideo", term)),
               aes(x = as.numeric(outcome) + 0.2, xend = as.numeric(outcome) + 0.2,
                   y = lwr_90, yend = upr_90,
                   alpha = significant), size = 1.5, 
               color = "darkolivegreen4") +
  geom_segment(inherit.aes = FALSE,
               data = att_modout02 %>% filter(grepl("Ideo", term)),
               aes(x = as.numeric(outcome) + 0.2, xend = as.numeric(outcome) + 0.2,
                   y = lwr_95, yend = upr_95,
                   alpha = significant), size = 0.75,
               color = "darkolivegreen4") +
  # - plot 90% and 95% PARTY STRENGTH moderator estimates
  geom_point(inherit.aes = FALSE,
             data = att_modout02 %>% filter(grepl("Party stre", term)),
             aes(x = as.numeric(outcome) - 0, y = pe,
                 alpha = significant), size = 3,
             color = "mediumorchid4") +
  geom_segment(inherit.aes = FALSE,
               data = att_modout02 %>% filter(grepl("Party stre", term)),
               aes(x = as.numeric(outcome) - 0, xend = as.numeric(outcome) - 0,
                   y = lwr_90, yend = upr_90, alpha = significant), size = 1.5,
               color = "mediumorchid4") +
  geom_segment(inherit.aes = FALSE,
               data = att_modout02 %>% filter(grepl("Party stre", term)),
               aes(x = as.numeric(outcome) - 0, xend = as.numeric(outcome) - 0,
                   y = lwr_95, yend = upr_95, alpha = significant), size = 0.75,
               color = "mediumorchid4") +
  # - plot 90% and 95% PARTY IDENTITY STRENGTH moderator
  geom_point(inherit.aes = FALSE,
             data = att_modout02 %>% filter(grepl("Party ident", term)),
             aes(x = as.numeric(outcome) - 0.2, y = pe,
                 alpha = significant), size = 3,
             color = "darkgoldenrod2") +
  geom_segment(inherit.aes = FALSE,
               data = att_modout02 %>% filter(grepl("Party ident", term)),
               aes(x = as.numeric(outcome) - 0.2, xend = as.numeric(outcome) - 0.2,
                   y = lwr_90, yend = upr_90, alpha = significant), size = 1.5,
               color = "darkgoldenrod2") +
  geom_segment(inherit.aes = FALSE,
               data = att_modout02 %>% filter(grepl("Party ident", term)),
               aes(x = as.numeric(outcome) - 0.2, xend = as.numeric(outcome) - 0.2,
                   y = lwr_95, yend = upr_95, alpha = significant), size = 0.75,
               color = "darkgoldenrod2") +
  scale_x_continuous("", breaks = seq(1, length(levels(att_modout02$outcome))),
                     labels = levels(att_modout02$outcome),
                     limits = c(0.5, 5.5)) +
  scale_y_continuous("\nMarginal effect of a 1 point increase in ideology, party and party identity strength\n(in standard deviation changes)",
                     breaks = seq(-1, 1, 0.05),
                     limits = c(-0.22, 0.12)) +
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
#dev.off()
