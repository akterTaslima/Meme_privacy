library(ggpubr)
library(multcomp)
library(car)
library(FSA) 
library(ggplot2)
library(phia)
#library(psych)
library(likert)
library(plyr)
library(tidyr)
library(tidyverse)
#library(dplyr)
library(emmeans)
library(lme4)
library(lmerTest)
library(lsr)
library(car)
library(rstatix)
library(heplots)
library(texreg)

library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(lsr)
library(car)
library(rstatix)
library(heplots)
library(effsize)
library(psych)
library(corrplot)
library(FactoMineR)
library(ez)
library(multcomp)
library(lavaan)

#convert data to wide format
convert_wide <- function(array) {
  
  df_mean= array %>%
    group_by(pid) %>%
    summarise_at(vars(sharing_likelihood), list(sharing_likelihood = mean))
  
  temp <-
    array[,] %>% 
    group_by(pid) %>% 
    filter(row_number()==1)
  
  temp = subset(temp, select = -c(sharing_likelihood) )
  data_wide <- merge(temp, df_mean, by=c("pid"))
  return(data_wide)
  
}  
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ----------------- Data filtering:: Check if same responses were selected by participants more than 90% of the time -------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------

clean_data=read.csv("sharing_V3_clean_final.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]

#clean_data=read.csv("data_same_response.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]

df=clean_data
# df = df[df$pid!="2" & df$pid!="3" & df$pid!="199" & df$pid!="318" & 
#                 df$pid!="227" & df$pid!="151" & df$pid!="152" & df$pid!="158" & df$pid!="169" & df$pid!="298",]

df_mean= df %>%
  group_by(pid) %>%
  summarise_at(vars(sharing_likelihood), list(sharing_likelihood = mean))

temp <-
  df[,] %>% 
  group_by(pid) %>% 
  filter(row_number()==1)

temp = subset(temp, select = -c(sharing_likelihood) )
clean_data_wide <- merge(temp, df_mean, by=c("pid"))

white = df[df$race=="White",]
black = df[df$race=="Black",]
asian = df[df$race=="Asian",]


ggplot(white, aes(x = sharing_likelihood)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_wrap(~ pid )

ggplot(black, aes(x = sharing_likelihood)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_wrap(~ pid )

ggplot(asian, aes(x = sharing_likelihood)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_wrap(~ pid )


# ---------------------------------------------------------------------------------------
# ----------------- Data filtering:: Check if same responses were selected by participants more than 90% of the time -------------------------------------------------------
# ---------------------------------------------------------------------------------------

clean_data=read.csv("sharing_V3_clean_final.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]

data_filter = clean_data[, c("pid","sharing_likelihood", "race")]
data_perid = table(data_filter$pid, data_filter$sharing_likelihood)
percent_data_perid = (prop.table(data_perid, margin =1)*100)
percent_tab = round(percent_data_perid,1)

data_perid_f = as.data.frame(percent_tab)
data_perid_above = data_perid_f[data_perid_f$Freq>=90.0,]
colnames(data_perid_above) = c("pid","sharing_likelihood_response", "Freq")
#res <- ddply(clean_race_rating, groupColumns, function(x) colSums(x[dataColumns]))
temp = clean_data_wide
data_same_response <- merge(temp, data_perid_above, by=c("pid"))

ggplot(data_same_response, aes(x = osn_imgshare_freq)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_wrap(~ sharing_likelihood_response )

write.csv(data_same_response, file="data_same_response.csv") # Write in a new csv file

# --------------------------------------------------------------------------
# ----------------------------  Normality test -------------------------------------
# --------------------------------------------------------------------------

clean_data_wide %>%
  group_by(race) %>%
  shapiro_test(sharing_likelihood)



# --------------------------------------------------------------------------
# ----------------------------  R1: perceivedPercentageRating ~ perceivedPercentageRace -------------------------------------
# --------------------------------------------------------------------------

#clean_data_wide = na.omit(clean_data_wide)

temp_percent = gather(clean_data_wide, key="perceivedPercentRace", value="perceivedPercentRating", 98:100)
ggline(temp_percent, x = "race", y = "perceivedPercentRating", color = "perceivedPercentRace", 
       add = c("mean_ci"), ylab="perceivedPercentRating", xlab="Participant race"
) 

temp_percent %>%
  group_by(race) %>%
  shapiro_test(perceivedPercentRating)


ggqqplot(temp_percent, "perceivedPercentRating", facet.by = "race")

## --------- Best models 
white_percent = temp_percent[temp_percent$race=="White",]
black_percent = temp_percent[temp_percent$race=="Black",]
asian_percent = temp_percent[temp_percent$race=="Asian",]


# -- White percent ---
white_percent %>% friedman_test(perceivedPercentRating ~ perceivedPercentRace |pid)
white_percent %>% friedman_effsize(perceivedPercentRating ~ perceivedPercentRace |pid)

pwc <- white_percent %>%
  wilcox_test(perceivedPercentRating ~ perceivedPercentRace, paired = TRUE, p.adjust.method = "bonferroni")
pwc


# res.aov <- anova_test(data = white_percent, dv = perceivedPercentRating, wid = pid, within = c("perceivedPercentRace"))
# get_anova_table(res.aov)
# pwc <- white_percent %>%
#   pairwise_t_test(
#     perceivedPercentRating ~ perceivedPercentRace, paired = TRUE,
#     p.adjust.method = "bonferroni"
#   )
# pwc

# -- Black percent ---
black_percent %>% friedman_test(perceivedPercentRating ~ perceivedPercentRace |pid)
black_percent %>% friedman_effsize(perceivedPercentRating ~ perceivedPercentRace |pid)

pwc <- black_percent %>%
  wilcox_test(perceivedPercentRating ~ perceivedPercentRace, paired = TRUE, p.adjust.method = "bonferroni")
pwc


# res.aov <- anova_test(data = black_percent, dv = perceivedPercentRating, wid = pid, within = c("perceivedPercentRace"))
# get_anova_table(res.aov)
# 
# pwc <- black_percent %>%
#   pairwise_t_test(
#     perceivedPercentRating ~ perceivedPercentRace, paired = TRUE,
#     p.adjust.method = "bonferroni"
#   )
# pwc


# -- Asian percent ---
asian_percent %>% friedman_test(perceivedPercentRating ~ perceivedPercentRace |pid)
asian_percent %>% friedman_effsize(perceivedPercentRating ~ perceivedPercentRace |pid)

pwc <- asian_percent %>%
  wilcox_test(perceivedPercentRating ~ perceivedPercentRace, paired = TRUE, p.adjust.method = "bonferroni")
pwc


# res.aov <- anova_test(data = asian_percent, dv = perceivedPercentRating, wid = pid, within = c("perceivedPercentRace"))
# get_anova_table(res.aov)
# 
# pwc <- asian_percent %>%
#   pairwise_t_test(
#     perceivedPercentRating ~ perceivedPercentRace, paired = TRUE,
#     p.adjust.method = "bonferroni"
#   )
# pwc



# --------------------------------------------------------------------------
# ----------------------  Feeling thermometer ::: R1: feelingThermometerRating ~ feelingThermometerRace -------------------------------------
# --------------------------------------------------------------------------


temp_ft = gather(clean_data_wide, key="feelingThermometerRace", value="feelingThermometerRating", 36:38)

temp_ft %>%
  group_by(race) %>%
  shapiro_test(feelingThermometerRating)


ggqqplot(temp_ft, "feelingThermometerRating", facet.by = "race")


ggline(temp_ft, x = "race", y = "feelingThermometerRating", color = "feelingThermometerRace", 
       add = c("mean_ci"), ylab="feelingThermometerRating", xlab="Participant race"
) 

## --------- Best models 
white_ft = temp_ft[temp_ft$race=="White",]
black_ft = temp_ft[temp_ft$race=="Black",]
asian_ft = temp_ft[temp_ft$race=="Asian",]


# -- White Feeling thermometer ---
white_ft %>% friedman_test(feelingThermometerRating ~ feelingThermometerRace |pid)
white_ft %>% friedman_effsize(feelingThermometerRating ~ feelingThermometerRace |pid)

pwc <- white_ft %>%
  wilcox_test(feelingThermometerRating ~ feelingThermometerRace, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# res.aov <- anova_test(data = white_ft, dv = feelingThermometerRating, wid = pid, within = c("feelingThermometerRace"))
# get_anova_table(res.aov)
# pwc <- white_ft %>%
#   pairwise_t_test(
#     feelingThermometerRating ~ feelingThermometerRace, paired = TRUE,
#     p.adjust.method = "bonferroni"
#   )
# pwc

# -- Black Feeling thermometer ---
black_ft %>% friedman_test(feelingThermometerRating ~ feelingThermometerRace |pid)
black_ft %>% friedman_effsize(feelingThermometerRating ~ feelingThermometerRace |pid)

pwc <- black_ft %>%
  wilcox_test(feelingThermometerRating ~ feelingThermometerRace, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# res.aov <- anova_test(data = black_ft, dv = feelingThermometerRating, wid = pid, within = c("feelingThermometerRace"))
# get_anova_table(res.aov)
# pwc <- black_ft %>%
#   pairwise_t_test(
#     feelingThermometerRating ~ feelingThermometerRace, paired = TRUE,
#     p.adjust.method = "bonferroni"
#   )
# pwc


# -- Asian Feeling thermometer ---
asian_ft %>% friedman_test(feelingThermometerRating ~ feelingThermometerRace |pid)
asian_ft %>% friedman_effsize(feelingThermometerRating ~ feelingThermometerRace |pid)

pwc <- asian_ft %>%
  wilcox_test(feelingThermometerRating ~ feelingThermometerRace, paired = TRUE, p.adjust.method = "bonferroni")
pwc


# res.aov <- anova_test(data = asian_ft, dv = feelingThermometerRating, wid = pid, within = c("feelingThermometerRace"))
# get_anova_table(res.aov)
# pwc <- asian_ft %>%
#   pairwise_t_test(
#     feelingThermometerRating ~ feelingThermometerRace, paired = TRUE,
#     p.adjust.method = "bonferroni"
#   )
# pwc



# --------------------------------------------------------------------------
# - Mixed model::: RQ2. Do participant’s racial biases influence their meme sharing of different target races? -------------------------------------
# --------------------------------------------------------------------------
# ------------- Baseline: White -----
# clean_data$race <- factor(clean_data$race,levels = c("White", "Black","Asian"))
# clean_data$target_race <- factor(clean_data$target_race,levels = c("white", "black","asian"))

# ------------- Baseline: Asian -----
clean_data$race <- factor(clean_data$race,levels = c("Asian", "Black","White"))
clean_data$target_race <- factor(clean_data$target_race,levels = c("asian", "black","white"))

# ------------- Baseline: Black -----
# clean_data$race <- factor(clean_data$race,levels = c("Black", "Asian","White"))
# clean_data$target_race <- factor(clean_data$target_race,levels = c("black", "asian","white"))


# clean_data$race <- factor(clean_data$race,levels = c("Black", "Asian","White"))
clean_data$target_race <- factor(clean_data$target_race,levels = c("black", "asian","white"))

emm_options(lmerTest.limit = 22000)
emm_options(pbkrtest.limit = 22000)
emm_options(lmer.df = "satterthwaite")

white = clean_data[clean_data$race=="White",]
black = clean_data[clean_data$race=="Black",]
asian = clean_data[clean_data$race=="Asian",]

# --------------------------------------------------------------------------
# --- Feeling thermometer  -------------------------------------
# --------------------------------------------------------------------------


ggplot(clean_data, aes(x=ft_white, y=sharing_likelihood)) + geom_point() + facet_wrap(~target_race)

baseline = lmer(sharing_likelihood ~ 1 + (1|pid) + (1|memeid_sharing), data=clean_data, REML = FALSE)

mod_ft <-update(baseline, .~. + valence_rating + ft_white * target_race + ft_black * target_race + ft_asian * target_race + race)
print(summary(mod_ft))
anova(mod_ft)

emmeans(mod_ft, pairwise ~  target_race | ft_white)
emmeans(mod_ft, pairwise ~  target_race | ft_black)
emmeans(mod_ft, pairwise ~  target_race | ft_asian)

# ------ White ----------
baseline = lmer(sharing_likelihood ~ 1 + (1|pid) + (1|memeid_sharing), data=white, REML = FALSE)

mod_ft <-update(baseline, .~. + valence_rating + ft_white * target_race + ft_black * target_race + ft_asian * target_race)
print(summary(mod_ft))
anova(mod_ft)

# ------ Black ----------
baseline = lmer(sharing_likelihood ~ 1 + (1|pid) + (1|memeid_sharing), data=black, REML = FALSE)

mod_ft <-update(baseline, .~. + valence_rating + ft_white * target_race + ft_black * target_race + ft_asian * target_race)
print(summary(mod_ft))
anova(mod_ft)

# ------ Asian ----------
baseline = lmer(sharing_likelihood ~ 1 + (1|pid) + (1|memeid_sharing), data=asian, REML = FALSE)

mod_ft <-update(baseline, .~. + valence_rating + ft_white * target_race + ft_black * target_race + ft_asian * target_race)
print(summary(mod_ft))
anova(mod_ft)


mod_ft <-update(baseline, .~. + valence_rating + ZHSQ_agg * target_race + ZHSQ_aff * target_race + ZHSQ_sd * target_race + ZHSQ_se * target_race + race)
print(summary(mod_ft))
anova(mod_ft)
# --------------------------------------------------------------------------
# --- Perceived Percentage Ratings  -------------------------------------
# --------------------------------------------------------------------------

# ------------- Baseline: White -----
# clean_data$race <- factor(clean_data$race,levels = c("White", "Black","Asian"))
# clean_data$target_race <- factor(clean_data$target_race,levels = c("white", "black","asian"))

# ------------- Baseline: Asian -----
clean_data$race <- factor(clean_data$race,levels = c("Asian", "Black","White"))
clean_data$target_race <- factor(clean_data$target_race,levels = c("asian", "black","white"))

# ------------- Baseline: Black -----
clean_data$race <- factor(clean_data$race,levels = c("Black", "Asian","White"))
clean_data$target_race <- factor(clean_data$target_race,levels = c("black", "asian","white"))


# clean_data$race <- factor(clean_data$race,levels = c("Black", "Asian","White"))
# clean_data$target_race <- factor(clean_data$target_race,levels = c("black", "asian","white"))

emm_options(lmerTest.limit = 22000)
emm_options(pbkrtest.limit = 22000)
emm_options(lmer.df = "satterthwaite")

white = clean_data[clean_data$race=="White",]
black = clean_data[clean_data$race=="Black",]
asian = clean_data[clean_data$race=="Asian",]

baseline = lmer(sharing_likelihood ~ 1 + (1|pid) + (1|memeid_sharing), data=clean_data, REML = FALSE)

mod_percent <-update(baseline, .~. + valence_rating + black_percent * target_race + white_percent * target_race + 
                       asian_percent * target_race + race)
print(summary(mod_percent))
anova(mod_percent)


# ------ White ----------
baseline = lmer(sharing_likelihood ~ 1 + (1|pid) + (1|memeid_sharing), data=white, REML = FALSE)

mod_percent <-update(baseline, .~. + valence_rating + black_percent * target_race + white_percent * target_race + 
                       asian_percent * target_race)
print(summary(mod_percent))
anova(mod_percent)

# ------ Black ----------
baseline = lmer(sharing_likelihood ~ 1 + (1|pid) + (1|memeid_sharing), data=black, REML = FALSE)

mod_percent <-update(baseline, .~. + valence_rating + black_percent * target_race + white_percent * target_race + 
                       asian_percent * target_race)
print(summary(mod_percent))
anova(mod_percent)


# ------ Asian ----------
baseline = lmer(sharing_likelihood ~ 1 + (1|pid) + (1|memeid_sharing), data=asian, REML = FALSE)

mod_percent <-update(baseline, .~. + valence_rating + black_percent * target_race + white_percent * target_race + 
                       asian_percent * target_race)
print(summary(mod_percent))
anova(mod_percent)


emmeans(mod_percent, pairwise ~  target_race | black_percent)
emmeans(mod_percent, pairwise ~  target_race | white_percent)


mod_percent <-update(baseline, .~. + valence_rating + black_percent * target_race + white_percent * target_race + 
                       asian_percent * target_race)
print(summary(mod_percent))
anova(mod_percent)


# --------------------------------------------------------------------------
# --- Target race * Participant race  -------------------------------------
# --------------------------------------------------------------------------

baseline = lmer(sharing_likelihood ~ 1 + (1|pid) + (1|memeid_sharing), data=clean_data, REML = FALSE)

mod_trace_prace <-update(baseline, .~. + valence_rating + race * target_race)
print(summary(mod_trace_prace))
anova(mod_trace_prace)

emmeans(mod_trace_prace, pairwise ~  target_race|race)


# --------------------------------------------------------------------------
# --- Target race * Humor styles  -------------------------------------
# --------------------------------------------------------------------------

baseline = lmer(sharing_likelihood ~ 1 + (1|pid) + (1|memeid_sharing), data=clean_data, REML = FALSE)

mod_trace_hsq <-update(baseline, .~. + valence_rating + ZHSQ_aff * target_race + ZHSQ_agg * target_race + 
                         ZHSQ_sd * target_race + ZHSQ_se * target_race)
print(summary(mod_trace_hsq))
anova(mod_trace_hsq)


# --------------------------------------------------------------------------
# ----------------------------  Divide participant into groups based on Feeling thermometers -------------------------------------
# --------------------------------------------------------------------------
#ft_black_med = median(clean_data$ft_black)
ft_black_med = median(clean_data_wide$ft_black)
ft_white_med = median(clean_data_wide$ft_white)
ft_asian_med = median(clean_data_wide$ft_asian)

ft_data = clean_data
ft_data <- ft_data %>% 
  mutate(ft_black_grp = if_else(ft_black >= ft_black_med, "Warm", "Cold"))
ft_data <- ft_data %>% 
  mutate(ft_white_grp = if_else(ft_white >= ft_white_med, "Warm", "Cold"))
ft_data <- ft_data %>% 
  mutate(ft_asian_grp = if_else(ft_asian >= ft_asian_med, "Warm", "Cold"))

#check = cbind(ft_data$ft_asian, ft_data$ft_asian_grp)

ft_data$ft_white_grp<-factor(as.character(ft_data$ft_white_grp))
ft_data$pid<-factor(as.character(ft_data$pid))

emm_options(lmerTest.limit = 22000)
emm_options(pbkrtest.limit = 22000)
emm_options(lmer.df = "satterthwaite")

white = ft_data[ft_data$race=="White",]
black = ft_data[ft_data$race=="Black",]
asian = ft_data[ft_data$race=="Asian",]


ggline(ft_data, x = "ft_white_grp", y = "sharing_likelihood", color = "target_race", 
       add = c("mean_ci"), ylab="Sharing likelihood", xlab="Feeling thermometer (Towards White)"
) 
ggline(ft_data, x = "ft_black_grp", y = "sharing_likelihood", color = "target_race", 
       add = c("mean_ci"), ylab="Sharing likelihood", xlab="Feeling thermometer (Towards Black)"
)
ggline(ft_data, x = "ft_asian_grp", y = "sharing_likelihood", color = "target_race", 
       add = c("mean_ci"), ylab="Sharing likelihood", xlab="Feeling thermometer (Towards Asian)"
)

ggline(white, x = "ft_white_grp", y = "sharing_likelihood", color = "target_race", 
       add = c("mean_ci"), ylab="Sharing likelihood", xlab="Feeling thermometer (Towards White)"
) 

ggline(black, x = "ft_black_grp", y = "sharing_likelihood", color = "target_race", 
       add = c("mean_ci"), ylab="Sharing likelihood", xlab="Feeling thermometer (Towards Black)"
)

ggline(asian, x = "ft_asian_grp", y = "sharing_likelihood", color = "target_race", 
       add = c("mean_ci"), ylab="Sharing likelihood", xlab="Feeling thermometer (Towards Asian)"
)

baseline = lmer(sharing_likelihood ~ 1 + (1|pid) + (1|memeid_sharing), data=ft_data, REML = FALSE)

mod_ft <-update(baseline, .~. + valence_rating + ft_white_grp * target_race + ft_black_grp * target_race + ft_asian_grp * target_race + race)
print(summary(mod_ft))
anova(mod_ft)

emmeans(mod_ft, pairwise ~  ft_white_grp | target_race, adjust = "bonferroni")
emmeans(mod_ft, pairwise ~  ft_black_grp | target_race)
emmeans(mod_ft, pairwise ~  ft_asian_grp | target_race)

#-----------------------------------------------
white_target = ft_data[ft_data$target_race=="white",]
black_target = ft_data[ft_data$target_race=="black",]
asian_target = ft_data[ft_data$target_race=="asian",]
# white_target_mean= white_target %>%
#   group_by(pid) %>%
#   summarise_at(vars(sharing_likelihood), list(sharing_likelihood = mean))
# 
# temp <-
#   white_target[,] %>% 
#   group_by(pid) %>% 
#   filter(row_number()==1)
# 
# temp = subset(temp, select = -c(sharing_likelihood) )
# white_target_wide <- merge(temp, white_target_mean, by=c("pid"))
# 
# 
library(nortest)
ad.test(white_target$sharing_likelihood)
ad.test(black_target$sharing_likelihood)
ad.test(asian_target$sharing_likelihood)

# white_target = na.omit(white_target)
# white_target
shapiro_test(white_target_wide$sharing_likelihood)

white_target_wide$ft_white_grp<-(as.character(white_target_wide$ft_white_grp))
white_target_wide$pid<-(as.character(white_target_wide$pid))
#white_target_wide = na.omit(white_target_wide)

# -- White Feeling thermometer for White targets ---
pwc <- white_target %>%
  wilcox_test(sharing_likelihood ~ ft_white_grp, p.adjust.method = "bonferroni")
pwc

# -- Black Feeling thermometer for Black targets ---
pwc <- black_target %>%
  wilcox_test(sharing_likelihood ~ ft_black_grp, p.adjust.method = "bonferroni")
pwc

# -- Asian Feeling thermometer for Asian targets ---
pwc <- asian_target %>%
  wilcox_test(sharing_likelihood ~ ft_asian_grp, p.adjust.method = "bonferroni")
pwc


# --------------------------------------------------------------------------
# ----------------------------  Divide participant into groups based on Perceived percentage -------------------------------------
# --------------------------------------------------------------------------
#ft_black_med = median(clean_data$ft_black)
black_percent_med = median(na.omit(clean_data_wide$black_percent))
white_percent_med = median(na.omit(clean_data_wide$white_percent))
asian_percent_med = median(na.omit(clean_data_wide$asian_percent))

percent_data = clean_data
percent_data = na.omit(percent_data)
percent_data <- percent_data %>% 
  mutate(black_percent_grp = if_else(black_percent >= black_percent_med, "Overestimate", "Underestimate"))

percent_data <- percent_data %>% 
  mutate(white_percent_grp = if_else(white_percent >= white_percent_med, "Overestimate", "Underestimate"))

percent_data <- percent_data %>% 
  mutate(asian_percent_grp = if_else(asian_percent >= asian_percent_med, "Overestimate", "Underestimate"))



ggline(percent_data, x = "white_percent_grp", y = "sharing_likelihood", color = "target_race", 
       add = c("mean_ci"), ylab="Sharing likelihood", xlab="Perceived percentage (White target)"
) 

ggline(percent_data, x = "black_percent_grp", y = "sharing_likelihood", color = "target_race", 
       add = c("mean_ci"), ylab="Sharing likelihood", xlab="Perceived percentage (Black target)"
)

ggline(percent_data, x = "asian_percent_grp", y = "sharing_likelihood", color = "target_race", 
       add = c("mean_ci"), ylab="Sharing likelihood", xlab="Perceived percentage (Asian target)"
)

# -- Overall participant data ---

#pwc <- percent_data %>%
wilcox_test(sharing_likelihood ~ asian_percent_grp, data = percent_data, p.adjust.method = "bonferroni")
wilcox_test(sharing_likelihood ~ white_percent_grp, data = percent_data, p.adjust.method = "bonferroni")
wilcox_test(sharing_likelihood ~ black_percent_grp, data = percent_data, p.adjust.method = "bonferroni")
#pwc

percent_data_wide = convert_wide(percent_data)
wilcox_test(sharing_likelihood ~ black_percent_grp, data = percent_data_wide, p.adjust.method = "bonferroni")
wilcox_test(sharing_likelihood ~ white_percent_grp, data = percent_data_wide, p.adjust.method = "bonferroni")
wilcox_test(sharing_likelihood ~ asian_percent_grp, data = percent_data_wide, p.adjust.method = "bonferroni")

# ----- Across target race -----
per_white_target = percent_data[percent_data$target_race=="white",]
per_black_target = percent_data[percent_data$target_race=="black",]
per_asian_target = percent_data[percent_data$target_race=="asian",]

wilcox_test(sharing_likelihood ~ white_percent_grp, data = per_white_target, p.adjust.method = "bonferroni")
wilcox_test(sharing_likelihood ~ black_percent_grp, data = per_black_target, p.adjust.method = "bonferroni")
wilcox_test(sharing_likelihood ~ asian_percent_grp, data = per_asian_target, p.adjust.method = "bonferroni")

per_white_target_wide = convert_wide(per_white_target)
per_black_target_wide = convert_wide(per_black_target)
per_asian_target_wide = convert_wide(per_asian_target)

wilcox_test(sharing_likelihood ~ white_percent_grp, data = per_white_target_wide, p.adjust.method = "bonferroni")
wilcox_test(sharing_likelihood ~ black_percent_grp, data = per_black_target_wide, p.adjust.method = "bonferroni")
wilcox_test(sharing_likelihood ~ asian_percent_grp, data = per_asian_target_wide, p.adjust.method = "bonferroni")

# --------------------------------------------------------------------------
# ----------------------------  :::: path model analysis :::: -------------------------------------
# --------------------------------------------------------------------------
clean_data_dc = cbind(clean_data, dummy.code(clean_data$race))
colnames(clean_data_dc)[139] <- "prace_Asian"
colnames(clean_data_dc)[140] <- "prace_Black"
colnames(clean_data_dc)[141] <- "prace_White"

clean_data_dc = cbind(clean_data_dc, dummy.code(clean_data$target_race))
colnames(clean_data_dc)[142] <- "trace_Asian"
colnames(clean_data_dc)[143] <- "trace_Black"
colnames(clean_data_dc)[144] <- "trace_White"

mediation.model <- "
  sharing_likelihood ~ ft_white + ft_black + ft_asian + prace_Asian + prace_White + trace_Asian + trace_White
  ft_white ~ prace_Asian + trace_Asian + trace_White
  ft_white ~ prace_White
  ft_black ~ prace_Asian + prace_White + trace_Asian + trace_White
  ft_asian ~ prace_Asian + prace_White + trace_Asian + trace_White
  
"

mediation.model <- "
  sharing_likelihood ~ ft_white + ft_black + ft_asian + prace_Asian + prace_White + trace_Asian + trace_White
  ft_white ~ prace_Asian 
  ft_white ~ prace_White
  ft_black ~ prace_Asian + prace_White 
  ft_asian ~ prace_Asian + prace_White 
  
"

mediation.model <- "
  sharing_likelihood ~ ft_white + ft_black + ft_asian + prace_Asian + prace_White + trace_Asian + trace_White
  ft_white ~ prace_Asian 
  ft_white ~ prace_White
  ft_black ~ prace_Asian + prace_White 
  ft_asian ~ prace_Asian + prace_White 
  
"

mediation.model <- "
  sharing_likelihood ~ ft_white + ft_black + ft_asian + prace_Asian + prace_White + trace_Asian + trace_White
  ft_white ~ trace_Asian 
  ft_white ~ trace_White
  ft_black ~ trace_Asian + trace_White 
  ft_asian ~ trace_Asian + trace_White 
"

mediation.fit <- sem(mediation.model, data=clean_data_dc)
summary(mediation.fit, fit.measures = TRUE)

library(semPlot)
semPlot::semPaths(mediation.fit, "par",
                  sizeMan = 15, sizeInt = 15, sizeLat = 15,
                  edge.label.cex=1.5,
                  fade=FALSE)

semPaths(mediation.fit,"std",layout = 'tree', sizeMan = 12, sizeInt = 12, sizeLat = 12,
         edge.label.cex=1.5, curvePivot = TRUE)


