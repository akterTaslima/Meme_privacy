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
library(ggcorrplot)
#Author DataFlair

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------

clean_data=read.csv("sharing_V3_clean_final.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]


# --------------------------------------------------------------------------
# ---------------------------- Generating Correlation Matrix -------------------------------------
# --------------------------------------------------------------------------
temp <-
  clean_data[,] %>% 
  group_by(pid) %>% 
  filter(row_number()==1)

df_mean= clean_data %>%
  group_by(pid) %>%
  summarise_at(vars(sharing_likelihood), list(sharing_likelihood = mean))

temp = subset(temp, select = -c(sharing_likelihood) )
clean_data_wide <- merge(temp, df_mean, by=c("pid"))

white = clean_data_wide[clean_data_wide$race=="White",]
black = clean_data_wide[clean_data_wide$race=="Black",]
asian = clean_data_wide[clean_data_wide$race=="Asian",]

### --------------- ALL participant combined ---------------------
all_data = subset(clean_data_wide, select = c(sharing_likelihood, black_percent, white_percent, asian_percent, ft_asian, ft_black, ft_white, 
                                      ZHSQ_aff, ZHSQ_se, ZHSQ_agg, ZHSQ_sd, doom_scrolling, 
                                      fomo, extraversion, agreeableness, conscientiousness, neuroticism, openness) )
colnames(all_data) = c("sharing_likelihood", "percent_black", "percent_white", "percent_asian", "ft_asian", "ft_black", "ft_white", 
                         "ZHSQ_aff", "ZHSQ_se", "ZHSQ_agg", "ZHSQ_sd", "doom_scrolling", "fomo", 
                         "big5_extraversion", "big5_agreeableness", "big5_conscientiousness", "big5_neuroticism", "big5_openness")
all_data = na.omit(all_data)

all_corr<-cor(all_data)
p.mat <- cor.mtest(all_data)

corrplot(all_corr, 
         method = "color", 
         #type = "upper", 
         title = "Correlation Matrix for ALL participants", 
         addCoef.col = "black",
         p.mat = p.mat, 
         sig.level = 0.05, insig = "blank",
         #mar = c(0,0,1,0), 
         number.cex = 0.8, 
         number.digits = 2, 
         tl.cex = 0.7)

library(tidyverse)  
library(corrr)
all_data %>% correlate() %>% 
  network_plot(min_cor = 0.1)

### --------------- White participant ---------------------
white_data = subset(white, select = c(sharing_likelihood, black_percent, white_percent, asian_percent, ft_asian, ft_black, ft_white, 
                                      ZHSQ_aff, ZHSQ_se, ZHSQ_agg, ZHSQ_sd, doom_scrolling, 
                                fomo, extraversion, agreeableness, conscientiousness, neuroticism, openness) )
colnames(white_data) = c("sharing_likelihood", "percent_black", "percent_white", "percent_asian", "ft_asian", "ft_black", "ft_white", 
                         "ZHSQ_aff", "ZHSQ_se", "ZHSQ_agg", "ZHSQ_sd", "doom_scrolling", "fomo", 
                         "big5_extraversion", "big5_agreeableness", "big5_conscientiousness", "big5_neuroticism", "big5_openness")
white_data = na.omit(white_data)

white_corr<-cor(white_data)
p.mat <- cor.mtest(white_data)

# corrplot(white_corr, type="upper", order="alphabet", #tl.srt=45,
#          p.mat = p.mat, sig.level = 0.05, insig = "blank")

#corrplot(white_corr, method="number", type="upper")
# col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# corrplot(white_corr, method="color", col=col(200),  
#          type="upper", order="hclust", 
#          addCoef.col = "black", # Add coefficient of correlation
#          tl.col="black", tl.srt=45, #Text label color and rotation
#          # Combine with significance
#          p.mat = p.mat, 
#          sig.level = 0.05, insig = "blank", 
#          # hide correlation coefficient on the principal diagonal
#          diag=FALSE 
# )
corrplot(white_corr, 
         method = "color", 
         #type = "upper", 
         title = "Correlation Matrix for White participants", 
         addCoef.col = "black",
         p.mat = p.mat, 
         sig.level = 0.05, insig = "blank",
         mar = c(0,0,1,0), 
         number.cex = 0.8, 
         number.digits = 2, 
         tl.cex = 0.7)

### --------------- Black participant ---------------------
black_data = subset(black, select = c(sharing_likelihood, black_percent, white_percent, asian_percent, ft_asian, ft_black, ft_white, ZHSQ_aff, ZHSQ_se, ZHSQ_agg, ZHSQ_sd, doom_scrolling, 
                                      fomo, extraversion, agreeableness, conscientiousness, neuroticism, openness) )

colnames(black_data) = c("sharing_likelihood", "percent_black", "percent_white", "percent_asian", "ft_asian", "ft_black", "ft_white", 
                         "ZHSQ_aff", "ZHSQ_se", "ZHSQ_agg", "ZHSQ_sd", "doom_scrolling", "fomo", 
                         "big5_extraversion", "big5_agreeableness", "big5_conscientiousness", "big5_neuroticism", "big5_openness")


black_data = na.omit(black_data)

black_corr<-cor(black_data)
p.mat <- cor.mtest(black_data)
# corrplot(black_corr, type="upper", order="alphabet", #tl.srt=45,
#          p.mat = p.mat, sig.level = 0.05, insig = "blank")
# 
corrplot(black_corr, 
         method = "color", 
         #type = "upper", 
         title = "Correlation Matrix for Black participants", 
         addCoef.col = "black",
         p.mat = p.mat, 
         sig.level = 0.05, insig = "blank",
         #mar = c(0,0,1,0), 
         number.cex = 0.8, 
         number.digits = 2,
         tl.cex = 0.7)
### --------------- Asian participant ---------------------
asian_data = subset(asian, select = c(sharing_likelihood, black_percent, white_percent, asian_percent, ft_asian, ft_black, ft_white, ZHSQ_aff, ZHSQ_se, ZHSQ_agg, ZHSQ_sd, doom_scrolling, 
                                      fomo, extraversion, agreeableness, conscientiousness, neuroticism, openness) )

colnames(asian_data) = c("sharing_likelihood", "percent_black", "percent_white", "percent_asian", "ft_asian", "ft_black", "ft_white", 
                         "ZHSQ_aff", "ZHSQ_se", "ZHSQ_agg", "ZHSQ_sd", "doom_scrolling", "fomo", 
                         "big5_extraversion", "big5_agreeableness", "big5_conscientiousness", "big5_neuroticism", "big5_openness")

asian_data = na.omit(asian_data)

asian_corr<-cor(asian_data)
p.mat <- cor.mtest(asian_data)

corrplot(asian_corr, 
         method = "color", 
         #type = "upper", 
         title = "Correlation Matrix for Asian participants", 
         addCoef.col = "black",
         p.mat = p.mat, 
         sig.level = 0.05, insig = "blank",
         #mar = c(0,0,1,0), 
         number.cex = 0.8, 
         number.digits = 2,
         tl.cex = 0.7)

# corrplot(asian_corr, type="upper", order="alphabet", #tl.srt=45,
#          p.mat = p.mat, sig.level = 0.05, insig = "blank")

# --------------------------------------------------------------------------
# ----------------------------  Correlation Analysis -------------------------------------
# --------------------------------------------------------------------------


clean_data_na = na.omit(clean_data)

clean_data_na %>% 
  #group_by(race) %>%
  #  summarize(cor=cor(black_percent, sharing_likelihood))
  do(data.frame(Cor=t(cor(.[,99:101], .[,3], method = c("spearman")))))


clean_data_na %>% 
  group_by(race) %>%
#  summarize(cor=cor(black_percent, sharing_likelihood))
  do(data.frame(Cor=t(cor(.[,99:101], .[,3], method = c("spearman")))))

clean_data_na %>% 
  group_by(target_race) %>%
  #  summarize(cor=cor(black_percent, sharing_likelihood))
  do(data.frame(Cor=t(cor(.[,99:101], .[,3], method = c("spearman")))))


clean_data_na %>% 
  do(data.frame(Cor=t(cor(.[,37:45], .[,3], method = c("spearman")))))

corr_ft_prace = clean_data_na %>% 
  group_by(race, target_race) %>%
  #  summarize(cor=cor(black_percent, sharing_likelihood))
  do(data.frame(Cor=t(cor(.[,37:45], .[,3], method = c("spearman")))))
print(corr_ft_prace)

corr_ft_trace = clean_data_na %>% 
  group_by(target_race) %>%
  #  summarize(cor=cor(black_percent, sharing_likelihood))
  do(data.frame(Cor=t(cor(.[,37:45], .[,3], method = c("spearman")))))
print(corr_ft_trace)


# corrplot(fomo_estimate, method="number")
# ggcorrplot(fomo_estimate)

corr_ds_prace = clean_data_na %>% 
  group_by(race) %>%
  do(data.frame(Cor=t(cor(.[,132], .[,3], method = c("spearman")))))
print(corr_ds_prace)

corr_ds_trace = clean_data_na %>% 
  group_by(target_race) %>%
  do(data.frame(Cor=t(cor(.[,132], .[,3], method = c("spearman")))))
print(corr_ds_trace)

corr_big5_prace = clean_data_na %>% 
  group_by(race) %>%
  do(data.frame(Cor=t(cor(.[,134:138], .[,3], method = c("spearman")))))
print(corr_big5_prace)

corr_big5_trace = clean_data_na %>% 
  group_by(target_race) %>%
  do(data.frame(Cor=t(cor(.[,134:138], .[,3], method = c("spearman")))))
print(corr_big5_trace)

corr_big5_ds = clean_data_na %>% 
  do(data.frame(Cor=t(cor(.[,134:138], .[,132], method = c("spearman")))))
print(corr_big5_ds)

corr_fomo_prace = clean_data_na %>% 
  group_by(race) %>%
  do(data.frame(Cor=t(cor(.[,133], .[,3], method = c("spearman")))))
print(corr_fomo_prace)

# corr_big5_trace %>%
#   cor_reorder() %>%
#   pull_lower_triangle() %>%
#   cor_plot(label = TRUE)

corr_fomo_trace = clean_data_na %>% 
  group_by(target_race) %>%
  do(data.frame(Cor=t(cor(.[,133], .[,3], method = c("spearman")))))
print(corr_fomo_trace)


fomo_estimate=clean_data_na %>%
  group_by(race) %>%
  summarize(cor.test(sharing_likelihood,fomo, method = c("spearman"))[["estimate"]])

fomo_pval=clean_data_na %>%
  group_by(race) %>%
  summarize(cor.test(sharing_likelihood,fomo, method = c("spearman"))[["p.value"]])

# --------------------------------------------------------------------------
# ----------------------------  Correlation with Doomscrolling -------------------------------------
# --------------------------------------------------------------------------

cor.test(clean_data$doom_scrolling, clean_data$asian, method = c("spearman"))
cor.test(clean_data$doom_scrolling, clean_data$black, method = c("spearman"))
cor.test(clean_data$doom_scrolling, clean_data$christians, method = c("spearman"))
cor.test(clean_data$doom_scrolling, clean_data$muslims, method = c("spearman"))
cor.test(clean_data$doom_scrolling, clean_data$republicans, method = c("spearman"))
cor.test(clean_data$doom_scrolling, clean_data$democrats, method = c("spearman"))
cor.test(clean_data$doom_scrolling, clean_data$hispanic, method = c("spearman"))
cor.test(clean_data$doom_scrolling, clean_data$native, method = c("spearman"))
cor.test(clean_data$doom_scrolling, clean_data$white, method = c("spearman"))

cor.test(clean_data$doom_scrolling, clean_data$ZHSQ_aff, method = c("spearman"))
cor.test(clean_data$doom_scrolling, clean_data$ZHSQ_se, method = c("spearman"))
cor.test(clean_data$doom_scrolling, clean_data$ZHSQ_sd, method = c("spearman"))
cor.test(clean_data$doom_scrolling, clean_data$ZHSQ_agg, method = c("spearman"))


# --------------------------------------------------------------------------
# ----------------------------  Percent questions -------------------------------------
# --------------------------------------------------------------------------

