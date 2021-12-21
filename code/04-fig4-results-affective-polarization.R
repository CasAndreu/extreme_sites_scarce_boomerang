#==============================================================================
# 04-fig4-results-affective-polarization.R
# Purpose: to replicate Figure 4 of the paper, where we show the main (and 
#          moderator) effects of the treatment on affective polarization. We
#          first generate Figure 4.A and then Figure 4.B. For the paper, we then
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
# - determining thermo opposing ideology and party based on treatment assignment
db <- db %>%
  mutate(thermo_opideo_PRE = ifelse(grepl("con", exp_group),
                                    thermo_lib_PRE, thermo_con_PRE),
         thermo_opparty_PRE = ifelse(grepl("con", exp_group),
                                     thermo_dem_PRE, thermo_rep_PRE),
         thermo_opideo_POST = ifelse(grepl("con", exp_group),
                                     thermo_lib_POST, thermo_con_POST),
         thermo_opparty_POST = ifelse(grepl("con", exp_group),
                                      thermo_dem_POST, thermo_rep_POST)) 

# MAIN
#===============================================================================

#-------------------------------------------------------------------------------
# [ Figure 4.A ] MAIN (ITT and CACE) EFFECTS
#-------------------------------------------------------------------------------
# - calculating affective polarization general effects
outcomes <- c("thermo_opideo", "thermo_opparty",
              "thermo_gun", "thermo_immig", "thermo_climate",
              "thermo_econ", "thermo_trumpissue",
              
              "understand_opideo", "understand_opparty",
              "understand_climate", "understand_econ", "understand_gun",
              "understand_immig", "understand_trumpissue",
              
              "negtrait_otherideo_stupid", "negtrait_otherparty_stupid", 
              "stupid_gun", "stupid_imig", "stupid_econ", "stupid_climate",
              "stupid_trumpissue"
              #"stupid_issues"
)

# - take the post pre diff for these outcomes
for (outcome in outcomes) {
  db[, paste0(outcome, "_DELTA")] <- db[,paste0(outcome, "_POST")] -
    db[,paste0(outcome, "_PRE")]
}

# - sepcify two samples: ITT and High compliers
compl_groups <- c("Intention to Treat\nN = 505", 
                  "High compliance\n(5-6 story surveys)\nN=337")

ap_out <- NULL
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
             pe = estimate / pre_sd,
             se_std = std.error / pre_sd,
             lwr_90 = pe - (se_std * 1.64),
             lwr_95 = pe - (se_std * 1.96),
             upr_90 = pe + (se_std * 1.64),
             upr_95 = pe + (se_std * 1.96))
    ap_out <- rbind(ap_out, model_out) 
  }
}

# - a variable indicating type of measure (Thermo, Understand, Stupid)
ap_out02 <- ap_out %>%
  mutate(measure = ifelse(grepl("therm", outcome),
                          "THERMOMETER (reversed)",
                          ifelse(grepl("understand", outcome),
                                 "UNDERSTAND (reversed)",
                                 "STUPID")),
         measure = factor(measure,
                          levels = c(
                            "THERMOMETER (reversed)",
                            "UNDERSTAND (reversed)",
                            "STUPID"
                          )),
         outcome = gsub("stupid_", "", gsub("understand_", "", 
                                            gsub("thermo_", "", outcome))),
         outcome = recode(
           outcome,
           `opideo` = "Opposing Ideology",
           `opparty` = "Opposing Party",
           `gun` = "Opposing views on Gun Control",
           `immig` = "Opposing views on Immigration",
           `econ` = "Opposing views on the Economy",
           `trumpissue` = "Opposing views on Trump Presidency",
           `climate` = "Opposing views on Climate Change",
           `negtrait_otherideo_stupid` = "Opposing Ideology",
           `negtrait_otherparty_stupid` = "Opposing Party",
           `imig` = "Opposing views on Immigration"),
         outcome = factor(outcome, levels = rev(c(
           "Opposing Ideology", 
           "Opposing Party", 
           "Opposing views on Climate Change",
           "Opposing views on the Economy",
           "Opposing views on Gun Control", 
           "Opposing views on Trump Presidency",
           "Opposing views on Immigration")))) %>%
  # /!\ REVERSE values for the Feeling Thermometer and Understand measures so 
  #     higher values mean higher polarization
  mutate(pe = ifelse(grepl("reversed", measure), -pe, pe),
         lwr_90 = ifelse(grepl("reversed", measure), -lwr_90, lwr_90),
         lwr_95 = ifelse(grepl("reversed", measure), -lwr_95, lwr_95),
         upr_90 = ifelse(grepl("reversed", measure), -upr_90, upr_90),
         upr_95 = ifelse(grepl("reversed", measure), -upr_95, upr_95),
         significant = as.character(significant)) 


# - give human redable labels to outcomes
#pdf("./figures/fig4a-extreme-sites-aff-pol-MAIN.pdf", width = 10, height = 5)
ggplot(ap_out02,
       aes(x = outcome, y = pe)) +
  # - this is so I can have a legend indicating what the 2 colors mean. Will
  #   return a code error that we can ignore:
  #   "Removed 42 rows containing missing values (geom_point).".
  geom_point(aes(x = 10, y = 0, color = sample)) +
  scale_color_manual("", values = c("mediumorchid4",
                                    "darkolivegreen4")) +
  # - plot 90% and 95% ITT estimates
  geom_point(inherit.aes = FALSE,
             data = ap_out02 %>% filter(grepl("Intent", sample)),
             aes(x = as.numeric(outcome) + 0.15, y = pe,
                 alpha = significant), 
             size = 3, color = "darkolivegreen4") +
  geom_segment(inherit.aes = FALSE, 
               data = ap_out02 %>% filter(grepl("Intent", sample)),
               aes(x = as.numeric(outcome) + 0.15, xend = as.numeric(outcome) + 0.15,
                   y = lwr_90, yend = upr_90, alpha = significant), size = 1.5, 
               color = "darkolivegreen4") +
  geom_segment(inherit.aes = FALSE,
               data = ap_out02 %>% filter(grepl("Intent", sample)),
               aes(x = as.numeric(outcome) + 0.15, xend = as.numeric(outcome) + 0.15,
                   y = lwr_95, yend = upr_95, alpha = significant), size = 0.75,
               color = "darkolivegreen4") +
  # - plot 90% and 95% High compliers CACE estimates
  geom_point(inherit.aes = FALSE,
             data = ap_out02 %>% filter(!grepl("Intent", sample)),
             aes(x = as.numeric(outcome) - 0.15, y = pe,
                 alpha = significant), size = 3,
             color = "mediumorchid4") +
  geom_segment(inherit.aes = FALSE,
               data = ap_out02 %>% filter(!grepl("Intent", sample)),
               aes(x = as.numeric(outcome) - 0.15, xend = as.numeric(outcome) - 0.15,
                   y = lwr_90, yend = upr_90,  alpha = significant), size = 1.5,
               color = "mediumorchid4") +
  geom_segment(inherit.aes = FALSE,
               data = ap_out02 %>% filter(!grepl("Intent", sample)),
               aes(x = as.numeric(outcome) - 0.15, xend = as.numeric(outcome) - 0.15,
                   y = lwr_95, yend = upr_95, alpha = significant), size = 0.75,
               color = "mediumorchid4") +
  scale_x_continuous("", breaks = seq(1, length(levels(ap_out02$outcome))),
                     labels = levels(ap_out02$outcome),
                     limits = c(0.5, 7.5)) +
  scale_y_continuous("\nTreatment Effect (in standard deviation changes)",
                     breaks = seq(-1, 1, 0.2),
                     limits = c(-0.6, 0.6)) +
  geom_hline(yintercept = 0, color = "red") +
  # - the rest
  facet_grid(~ measure) +
  coord_flip() +
  scale_alpha_discrete(range = c(0.3, 1), guide = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text (size = 10),
        axis.text.y = element_text(size = 11),
        strip.text = element_text(size = 11))
#dev.off()

#-------------------------------------------------------------------------------
# [ Figure 4.B ] MODERATOR EFFECTS
#-------------------------------------------------------------------------------
# - specify list of moderators
moderators <- c("party_folded", "ideo_folded", "pid_strength")

ap_modout <- NULL
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
    ap_modout <- rbind(ap_modout, model_out) 
  }
}

# - a variable indicating type of measure (Thermo, Understand, Stupid)
ap_modout02 <- ap_modout %>%
  mutate(measure = ifelse(grepl("therm", outcome),
                          "THERMOMETER (reversed)",
                          ifelse(grepl("understand", outcome),
                                 "UNDERSTAND (reversed)",
                                 "STUPID")),
         measure = factor(measure,
                          levels = c(
                            "THERMOMETER (reversed)",
                            "UNDERSTAND (reversed)",
                            "STUPID"
                          )),
         outcome = gsub("stupid_", "", gsub("understand_", "", 
                                            gsub("thermo_", "", outcome))),
         outcome = recode(
           outcome,
           `opideo` = "Opposing Ideology",
           `opparty` = "Opposing Party",
           `gun` = "Opposing views on Gun Control",
           `immig` = "Opposing views on Immigration",
           `econ` = "Opposing views on the Economy",
           `trumpissue` = "Opposing views on Trump Presidency",
           `climate` = "Opposing views on Climate Change",
           `negtrait_otherideo_stupid` = "Opposing Ideology",
           `negtrait_otherparty_stupid` = "Opposing Party",
           `imig` = "Opposing views on Immigration"),
         outcome = factor(outcome, levels = rev(c(
           "Opposing Ideology", 
           "Opposing Party", 
           "Opposing views on Climate Change",
           "Opposing views on the Economy",
           "Opposing views on Gun Control", 
           "Opposing views on Trump Presidency",
           "Opposing views on Immigration")))) %>%
  # /!\ REVERSE values for the Feeling Thermometer and Understand measures so 
  #     higher values mean higher polarization
  mutate(pe = ifelse(grepl("reversed", measure), -pe, pe),
         lwr_90 = ifelse(grepl("reversed", measure), -lwr_90, lwr_90),
         lwr_95 = ifelse(grepl("reversed", measure), -lwr_95, lwr_95),
         upr_90 = ifelse(grepl("reversed", measure), -upr_90, upr_90),
         upr_95 = ifelse(grepl("reversed", measure), -upr_95, upr_95),
         term = recode(term,
                       `condition:ideo_folded` = "Ideology strength\n{6-point scale}",
                       `condition:pid_strength` = "Party identity strength\n{7-point scale}",
                       `condition:party_folded` = "Party strength\n{4-point scale}",
                       `condition:PARTY_W2` = "Conservatism\n{7-point scale}",
                       `condition:INTERESTS_POL_W2` = "Interest in politics\n{7-point scale}",
                       `condition:follow_W0` = "Follows politics\n{7-point scale}"),
         tern = factor(term, levels = c(
           "Ideology strength\n{6-point scale}",
           "Party identity strength\n{7-point scale}",
           "Party strength\n{4-point scale}",
           "Conservatism\n{7-point scale}",
           "Interest in politics\n{7-point scale}",
           "Follows politics\n{7-point scale}"))) %>%
  mutate(significant = as.character(significant))

#pdf("./figures/fig4b-extreme-sites-aff-pol-MOD.pdf", width = 10, height = 5)
ggplot(ap_modout02,
       aes(x = outcome, y = pe)) +
  # - this is so I can have a legend indicating what the 3 colors mean. Will
  #   return a code error that we can ignore:
  #   "Removed 63 rows containing missing values (geom_point).".
  geom_point(aes(x = 10, y = 0, color = term)) +
  scale_color_manual("", values = c("darkolivegreen4",
                                    "orange3",
                                    "mediumorchid4")) +
  # - plot 90% and 95% IDEO STRENGTH moderation estimates
  geom_point(inherit.aes = FALSE,
             data = ap_modout02 %>% filter(grepl("Ideo", term)),
             aes(x = as.numeric(outcome) + 0.2, y = pe,
                 alpha = significant),
             size = 3, color = "darkolivegreen4") +
  geom_segment(inherit.aes = FALSE, 
               data = ap_modout02 %>% filter(grepl("Ideo", term)),
               aes(x = as.numeric(outcome) + 0.2, xend = as.numeric(outcome) + 0.2,
                   y = lwr_90, yend = upr_90,
                   alpha = significant), size = 1.5, 
               color = "darkolivegreen4") +
  geom_segment(inherit.aes = FALSE,
               data = ap_modout02 %>% filter(grepl("Ideo", term)),
               aes(x = as.numeric(outcome) + 0.2, xend = as.numeric(outcome) + 0.2,
                   y = lwr_95, yend = upr_95,
                   alpha = significant), size = 0.75,
               color = "darkolivegreen4") +
  # - plot 90% and 95% PARTY STRENGTH moderator estimates
  geom_point(inherit.aes = FALSE,
             data = ap_modout02 %>% filter(grepl("Party stre", term)),
             aes(x = as.numeric(outcome) - 0, y = pe,
                 alpha = significant), size = 3,
             color = "mediumorchid4") +
  geom_segment(inherit.aes = FALSE,
               data = ap_modout02 %>% filter(grepl("Party stre", term)),
               aes(x = as.numeric(outcome) - 0, xend = as.numeric(outcome) - 0,
                   y = lwr_90, yend = upr_90, alpha = significant), size = 1.5,
               color = "mediumorchid4") +
  geom_segment(inherit.aes = FALSE,
               data = ap_modout02 %>% filter(grepl("Party stre", term)),
               aes(x = as.numeric(outcome) - 0, xend = as.numeric(outcome) - 0,
                   y = lwr_95, yend = upr_95, alpha = significant), size = 0.75,
               color = "mediumorchid4") +
  # - plot 90% and 95% PARTY IDENTITY STRENGTH moderator
  geom_point(inherit.aes = FALSE,
             data = ap_modout02 %>% filter(grepl("Party ident", term)),
             aes(x = as.numeric(outcome) - 0.2, y = pe,
                 alpha = significant), size = 3,
             color = "darkgoldenrod2") +
  geom_segment(inherit.aes = FALSE,
               data = ap_modout02 %>% filter(grepl("Party ident", term)),
               aes(x = as.numeric(outcome) - 0.2, xend = as.numeric(outcome) - 0.2,
                   y = lwr_90, yend = upr_90, alpha = significant), size = 1.5,
               color = "darkgoldenrod2") +
  geom_segment(inherit.aes = FALSE,
               data = ap_modout02 %>% filter(grepl("Party ident", term)),
               aes(x = as.numeric(outcome) - 0.2, xend = as.numeric(outcome) - 0.2,
                   y = lwr_95, yend = upr_95, alpha = significant), size = 0.75,
               color = "darkgoldenrod2") +
  scale_x_continuous("", breaks = seq(1, length(levels(ap_modout02$outcome))),
                     labels = levels(ap_modout02$outcome),
                     limits = c(0.5, 7.5)) +
  scale_y_continuous("\nMarginal effect of a 1 point increase in ideology, party and party identity strength\n(in standard deviation changes)",
                     breaks = seq(-1, 1, 0.2),
                     limits = c(-0.6, 0.6)) +
  geom_hline(yintercept = 0, color = "red") +
  # - the rest
  scale_alpha_discrete(range = c(0.25, 1), guide = FALSE) +
  facet_grid(~ measure) +
  coord_flip() +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text (size = 10),
        axis.text.y = element_text(size = 11),
        strip.text = element_text(size = 11))
#dev.off()
