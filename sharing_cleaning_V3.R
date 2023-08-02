library(ggpubr)
library(multcomp)
library(car)
library(FSA) 
library(ggplot2)
library(phia)
library(psych)
library(likert)
library(plyr)
library(tidyr)
library(tidyverse)
library(dplyr)
library(emmeans)
library(lme4)
library(lmerTest)
library(lsr)
library(car)
library(rstatix)
library(heplots)
library(devtools)
library(sjmisc)
library(lsr)
library(car)
library(rstatix)
library(heplots)
library(effsize)
library(psych)
library(corrplot)
library(FactoMineR)   #Author DataFlair

## extract all the necessary columns from the dataset

col_extract <- function(ques_sahring, cat,  humor_scale, feeling_thermometer, sns_privacy, doom_scrolling, big5, fomo, others_priv) {
  array_name_sharing = c()
  if( cat == 'sharing'){
    for (i in 1:79){
      coln = paste("X",toString(i),"_", ques_sahring, sep = "" )
      response_time_coln = paste("X",toString(i),"_Q10.2","_Page.Submit", sep = "" )
      array_name_sharing <- c (array_name_sharing, coln)
      array_name_sharing <- c (array_name_sharing, response_time_coln)
    }
    
  }
  final_array_name <- c (array_name_sharing, humor_scale, feeling_thermometer,sns_privacy, doom_scrolling, big5, fomo, others_priv, "Q6.1","Q6.2","Q7.1", "Q7.1_8_TEXT","Q7.2","Q7.3", "Q7.3_8_TEXT", "Q7.4", "Q7.5",
                         "Q11.1", "Q12.1_4", "Q13.1_4", "Q14.1_4","Q16.1", "Q17.1", "Q17.2", "Q17.3", "Q24.1", "Q24.2","Q24.3","Q24.4", "Q24.4_7_TEXT", "Q24.5", "Q24.6", "Q24.6_21_TEXT",
                         "Q24.7", "Q24.7_8_TEXT", "Q25.1", "Q25.2","sequence")
  return(final_array_name)
}



# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# Convert the data into long format and add the participant number and image number
# ---------------------------------------------------------------------------------------
data_cleaning <- function(photo_rating, sequence_sharing, humor_scale, feeling_thermometer, sns_privacy, doom_scrolling, big5, fomo, others_priv, grp, ques1, ques2) {
  nrow_matrix=nrow(photo_rating)
  ID <- seq.int(nrow(photo_rating))
  row_bind = c()
  combine = c()
  
  for (i in 1:nrow_matrix){
    pid = i
    split_photo_sequence_sharing = strsplit(sequence_sharing[i], ",")
    split_photo_sequence_sharing = unlist(split_photo_sequence_sharing)
    
    
    rating_each_person = photo_rating[i,]
    humor_scale_rating = rating_each_person[humor_scale]
    feeling_thetmometer_rating = rating_each_person[feeling_thermometer]
    sns_privacy_rating = rating_each_person[sns_privacy]
    doomscrolling_rating = rating_each_person[doom_scrolling]
    big5_rating = rating_each_person[big5]
    fomo_rating = rating_each_person[fomo]
    others_priv_rating = rating_each_person[others_priv]
    
    osn_accnt = rating_each_person["Q7.1"]
    osn_freq = rating_each_person["Q7.2"]
    osn_visit_freq = rating_each_person["Q7.3"]
    osn_share_whom = rating_each_person["Q7.4"]
    osn_imgshare_freq = rating_each_person["Q7.5"]
    imgshare_acquaintance = rating_each_person["Q17.1"]
    imgshare_fnf = rating_each_person["Q17.2"]
    imgshare_public = rating_each_person["Q17.3"]
    priv_perception = rating_each_person["Q16.1"]
    
    black_perecent = rating_each_person["Q12.1_4"]
    white_perecent = rating_each_person["Q13.1_4"]
    asian_perecent = rating_each_person["Q14.1_4"]
    
    age = rating_each_person["Q6.1"]
    lived_us = rating_each_person["Q6.2"]
    prolific_id = rating_each_person["Q24.1"]
    gender = rating_each_person["Q24.2"]
    education = rating_each_person["Q24.3"]
    race = rating_each_person["Q24.4"]
    socioeconomic_status = rating_each_person["Q24.5"]
    religion = rating_each_person["Q24.6"]
    political_orientation = rating_each_person["Q24.7"]
    feedback = rating_each_person["Q25.1"]
    payment_enough = rating_each_person["Q25.2"]
    
    if (grp =="sharing"){
    
      for (j in 1:79){
        coln = paste("X",toString(j), ques1, sep = "" )
        print(coln)
        response_time_coln = paste("X",toString(j),ques2,"_Page.Submit", sep = "" )
        
        qid_sharing = split_photo_sequence_sharing[j]
        sharing_rating= rating_each_person[coln]
        sharing_response_time= rating_each_person[response_time_coln]
        
        
        combine = cbind (pid, qid_sharing, sharing_rating, sharing_response_time, humor_scale_rating, feeling_thetmometer_rating, sns_privacy_rating, doomscrolling_rating,
                         big5_rating, fomo_rating, others_priv_rating, black_perecent, white_perecent, asian_perecent,
                         osn_accnt, osn_freq, osn_visit_freq, osn_share_whom, osn_imgshare_freq, imgshare_acquaintance, imgshare_fnf, imgshare_public, 
                         priv_perception, prolific_id, lived_us, age, gender, race, education, socioeconomic_status, religion, political_orientation, feedback, payment_enough )
        colnames(combine) <- c("pid", "memeid_sharing", "sharing_likelihood", "sharing_response_time", humor_scale, feeling_thermometer, sns_privacy, doom_scrolling,
                               big5, fomo, others_priv, "black_percent", "white_percent", "asian_percent",
                               "osn_accnt", "osn_freq", "osn_visit_freq", "osn_share_whom","osn_imgshare_freq", "imgshare_acquaintance", "imgshare_fnf", "imgshare_public", 
                               "priv_perception", "prolific_id", "lived_us","age", "gender", "race", "education", "socioeconomic_status","religion", "political_orientation","feedback", "payment_enough" )
        row_bind = rbind(row_bind, combine)
        #print(head(row_bind))
      }
    }
    
 
  }
  return(row_bind)
}

# ---------------------------------------------------------------------------------------
# Making a table with mean, variance, minimum and maximum values of the valence rating
# ---------------------------------------------------------------------------------------

data_aggreagation <- function(array, iv, col_number) {
  combine_values = c()
  if (iv == "qid_sharing"){
    mean_values = aggregate(array[, col_number], list(array$qid), mean)
    var_values = aggregate(array[, col_number], list(array$qid), var)
    min_values = aggregate(array[, col_number], list(array$qid), min)
    max_values = aggregate(array[, col_number], list(array$qid), max)
    combine_values = cbind(mean_values, var_values[2], min_values[2], max_values[2])
    colnames(combine_values) = c("qid", "mean", "variance", "min", "max")
    
  }
  if (iv == "pid"){
    mean_values = aggregate(array[, col_number], list(array$pid), mean)
    var_values = aggregate(array[, col_number], list(array$pid), var)
    min_values = aggregate(array[, col_number], list(array$pid), min)
    max_values = aggregate(array[, col_number], list(array$pid), max)
    combine_values = cbind(mean_values, var_values[2], min_values[2], max_values[2])
    colnames(combine_values) = c("pid", "mean", "variance", "min", "max")
    
  }
  
  return(combine_values)
}

# ---------------------------------------------------------------------------------------
# Find the failed attention checks
# ---------------------------------------------------------------------------------------
attention_check <- function(array, grp) {
  
  if (grp == "not6"){
    attention_chk1_fail = array[array$X11_Q8.1!="1",]
    attention_chk2_fail = array[array$X22_Q8.1!="6",]
    attention_chk3_fail = array[array$X34_Q8.1!="2",]
    attention_chk4_fail = array[array$X43_Q8.1!="7",]
    
  }
  if (grp == "is6"){
    attention_chk1_fail = array[array$X11_Q8.1!="1",]
    attention_chk2_fail = array[array$X22_Q8.1!="6",]
    attention_chk3_fail = array[array$X34_Q8.1!="2",]
    attention_chk4_fail = array[array$X43_Q8.1!="7",]
    
  }
  
  combine_values = rbind(attention_chk1_fail, attention_chk2_fail, attention_chk3_fail, attention_chk4_fail)
  return(combine_values)
}
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
### ------- Renaming ----------
y=read.csv("sharing_followup_V3_main04.csv", header = FALSE) 
#y=y[,-c(606:669)] # removing unnecessary columns
#y=y[,-c(511:931)]
x = y[1,] #extracting the column names
#y_selected=y[38:(nrow(y)), ]
y_selected = y
y_selected=y_selected[!apply(y_selected == "", 1, all),] # Removing the empty lines
colnames(y_selected) <- unlist(x[1,])   # set the column names

y_selected=y_selected[y_selected$Q25.3=="1",] # Complete responses
write.csv(y_selected, file="Sharing_followup_V3_renamed.csv") # Write in a new csv file

### ------- Attention check ----------
y_selected=read.csv("Sharing_followup_V3_renamed.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]


### ------- Sharing only rating : Data cleaning ----------

#sharing_data = y_selected
sharing_attention_chk1_fail = y_selected[y_selected$X16_Q10.1!="1",]
sharing_attention_chk2_fail = y_selected[y_selected$X33_Q10.1!="5",]
sharing_attention_chk3_fail = y_selected[y_selected$X49_Q10.1!="2",]
sharing_attention_chk4_fail = y_selected[y_selected$X63_Q10.1!="7",]


attn = y_selected[y_selected$X16_Q10.1!="1" | y_selected$X33_Q10.1!="5" 
                        | y_selected$X49_Q10.1!="2" | y_selected$X63_Q10.1!="7",]

 

attn_s = cbind(attn["X16_Q10.1"], attn["X33_Q10.1"], attn["X49_Q10.1"], attn["X63_Q10.1"])


y_selected = y_selected[y_selected$X16_Q10.1=="1" & y_selected$X33_Q10.1=="5" 
                        & y_selected$X49_Q10.1=="2" & y_selected$X63_Q10.1=="7",]

table(y_selected$Q24.4)
median(y_selected$Duration..in.seconds.) # 29.6 mins, 1778
mean(y_selected$Duration..in.seconds.) # 31.7 mins, 1902
sd(y_selected$Duration..in.seconds.) # 12.55 mins, 753
quantile(y_selected$Duration..in.seconds., c(.25, .50, .75)) # 25% - 23.6 mins, 75%- 35.8 mins

white = y_selected[y_selected$Q24.4=="6",]
black = y_selected[y_selected$Q24.4=="4",]
asian = y_selected[y_selected$Q24.4=="3",]
# 
median(white$Duration..in.seconds.) # 33.7 mins, 1891
median(black$Duration..in.seconds.) # 47.7 mins, 2812
median(asian$Duration..in.seconds.) # 33.6 mins, 1870

mean(white$Duration..in.seconds.) # 31.6 mins
mean(black$Duration..in.seconds.) # 50.7 mins
mean(asian$Duration..in.seconds.) # 45.7 mins
# 
quantile(white$Duration..in.seconds., c(.25, .50, .75))
quantile(black$Duration..in.seconds., c(.25, .50, .75))
quantile(asian$Duration..in.seconds., c(.25, .50, .75))


humor_scale =c()
for (i in 2:34){
  coln = paste("Q20.",toString(i), sep = "" )
  humor_scale = c(humor_scale, coln)
}

feeling_thermometer =c()
for (i in 2:10){
  coln = paste("Q15.",toString(i), "_1", sep = "" )
  feeling_thermometer = c(feeling_thermometer, coln)
}

sns_privacy =c()
for (i in 1:16){
  coln = paste("Q19.",toString(i), sep = "" )
  sns_privacy = c(sns_privacy, coln)
}

doom_scrolling =c()
for (i in 2:5){
  coln = paste("Q18.",toString(i), sep = "" )
  doom_scrolling = c(doom_scrolling, coln)
}

big5 =c()
for (i in 2:11){
  coln = paste("Q21.",toString(i), sep = "" )
  big5 = c(big5, coln)
}

fomo =c()
for (i in 2:11){
  coln = paste("Q22.",toString(i), sep = "" )
  fomo = c(fomo, coln)
}

others_priv =c()
for (i in 2:15){
  coln = paste("Q23.",toString(i), sep = "" )
  others_priv = c(others_priv, coln)
}

sharing_data = y_selected[col_extract("Q10.1", "sharing", humor_scale, feeling_thermometer, sns_privacy, doom_scrolling, big5, fomo, others_priv)] # extract columns

photo_sequence_sharing= sharing_data[,c("sequence")]     # extract the photo sequence
#photo_sequence_humor= sharing_data[,c("sequence_humor")]     # extract the photo sequence


clean_sharing_data = data_cleaning(sharing_data, photo_sequence_sharing, humor_scale, feeling_thermometer, sns_privacy, doom_scrolling, big5, fomo, others_priv, "sharing", "_Q10.1", "_Q10.2")  # create the clean data table
reserve_data = clean_sharing_data

#black = clean_sharing_data[clean_sharing_data$race=="4",]

# clean_blockwise_data = clean_sharing_data[clean_sharing_data$race=="6" | clean_sharing_data$race=="4" 
#                                 | clean_sharing_data$race=="3",]
# 
# clean_blockwise_data$race[clean_blockwise_data$race == "6"] <- "white"
# clean_blockwise_data$race[clean_blockwise_data$race == "4"] <- "black"
# clean_blockwise_data$race[clean_blockwise_data$race == "3"] <- "asian"
write.csv(clean_sharing_data, file="sharing_V3_clean_gather.csv") # Write in a new csv file

### --------------------------------------------------------
### --------------------------------------------------------
### ------- Sharing rating : read data and analysis ----------
### --------------------------------------------------------
### --------------------------------------------------------
clean_blockwise_data=read.csv("sharing_V3_clean_gather.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]

race_data = clean_blockwise_data[clean_blockwise_data$race=="4" | clean_blockwise_data$race=="6" | clean_blockwise_data$race=="3",]
pid_data <-
  clean_blockwise_data[,] %>% 
  group_by(pid) %>% 
  filter(row_number()==1)

black = pid_data[pid_data$race=="4",]
white = pid_data[pid_data$race=="6",]
asian = pid_data[pid_data$race=="3",]

black = clean_blockwise_data[clean_blockwise_data$race=="4",]
white = clean_blockwise_data[clean_blockwise_data$race=="6",]
asian = clean_blockwise_data[clean_blockwise_data$race=="3",]

p <- ggplot(race_data, aes(x=race, y=sharing_response_time)) + 
  geom_boxplot() 
p 

p <- ggplot(black, aes(x=race, y=sharing_response_time)) + 
  geom_boxplot() + facet_wrap(black$pid)
p 
p <- ggplot(white, aes(x=race, y=sharing_response_time)) + 
  geom_boxplot() + facet_wrap(white$pid)
p 
p <- ggplot(asian, aes(x=race, y=sharing_response_time)) + 
  geom_boxplot() + facet_wrap(asian$pid)
p 


summary(black$sharing_response_time)
sd(black$sharing_response_time)

summary(white$sharing_response_time)
sd(white$sharing_response_time)
### --------------------------------------------------------
### ------- Adding target race ----------
### --------------------------------------------------------
clean_blockwise_data=read.csv("sharing_V3_clean_gather.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]

humor_data=read.csv("humor_rating_with_target_race.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]
meme_target <-
  humor_data[,] %>% 
  group_by(qid) %>% 
  filter(row_number()==1)


meme_qid = meme_target$qid
meme_race = meme_target$target_race
clean_blockwise_data$target_race_sharing = "c"
#clean_blockwise_data$target_race_humor = "c"

for (i in 1:length(meme_qid)){
  image_id =  meme_qid[i]
  clean_blockwise_data$target_race_sharing[clean_blockwise_data$memeid_sharing==image_id] <- meme_race[i]
  #clean_blockwise_data$target_race_humor[clean_blockwise_data$qid_humor==image_id] <- meme_race[i]
}

clean_blockwise_data = clean_blockwise_data[clean_blockwise_data$target_race_sharing!="c",]

write.csv(clean_blockwise_data, file="sharing_V3_clean.csv") # Write in a new csv file

####################################################################################################
#############       ADDING HUMOR SCALES        ####################
####################################################################################################

clean_blockwise_data=read.csv("sharing_V3_clean.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]


temp = clean_blockwise_data
temp = temp[temp$Q20.12=="3",]


temp = subset(temp, select = -c(Q20.12) )
temp <- temp %>%
  rename(target_race=target_race_sharing, Q20.1 = Q20.2, Q20.2=Q20.3, Q20.3 = Q20.4, Q20.4=Q20.5, Q20.5=Q20.6, Q20.6=Q20.7,  Q20.7=Q20.8,
         Q20.8=Q20.9, Q20.9=Q20.10, Q20.10=Q20.11, Q20.11=Q20.13, Q20.12=Q20.14, Q20.13=Q20.15, Q20.14=Q20.16,
         Q20.15=Q20.17, Q20.16=Q20.18, Q20.17=Q20.19, Q20.18=Q20.20, Q20.19=Q20.21, Q20.20=Q20.22, Q20.21=Q20.23,
         Q20.22=Q20.24, Q20.23=Q20.25, Q20.24=Q20.26, Q20.25=Q20.27, Q20.26=Q20.28, Q20.27=Q20.29, Q20.28=Q20.30, 
         Q20.29=Q20.31, Q20.30=Q20.32, Q20.31=Q20.33, Q20.32=Q20.34)


Flip <- function(x) abs(8 - x)


temp <- mutate_at(temp,.vars = c("Q20.1","Q20.9","Q20.17", "Q20.25", "Q20.29", "Q20.22",
                                 "Q20.7", "Q20.15", "Q20.23", "Q20.31", "Q20.16"), Flip )



HSQ_values <-
  temp[,] %>% 
  group_by(pid) %>% 
  filter(row_number()==1)

HSQ_values <- HSQ_values %>% 
  mutate(HSQ_aff = sum(Q20.1, Q20.5, Q20.9, Q20.13, Q20.17, Q20.21, Q20.25, Q20.29),
         HSQ_se = sum(Q20.2,Q20.6,Q20.10,Q20.14,Q20.18,Q20.22, Q20.26, Q20.30), 
         HSQ_agg = sum(Q20.3, Q20.7, Q20.11, Q20.15, Q20.19, Q20.23, Q20.27, Q20.31), 
         HSQ_sd = sum(Q20.4, Q20.8, Q20.12, Q20.16, Q20.20, Q20.24, Q20.28, Q20.32))


data_pid = HSQ_values$pid
HSQ_aff = HSQ_values$HSQ_aff
HSQ_se = HSQ_values$HSQ_se
HSQ_agg = HSQ_values$HSQ_agg
HSQ_sd = HSQ_values$HSQ_sd

temp$HSQ_aff = "c"
temp$HSQ_se = "c"
temp$HSQ_agg = "c"
temp$HSQ_sd = "c"

for (i in 1:length(data_pid)){
  pid =  data_pid[i]
  temp$HSQ_aff[temp$pid==pid] <- HSQ_aff[i]
  temp$HSQ_se[temp$pid==pid] <- HSQ_se[i]
  temp$HSQ_agg[temp$pid==pid] <- HSQ_agg[i]
  temp$HSQ_sd[temp$pid==pid] <- HSQ_sd[i]
  
}

#temp = as.data.frame(temp)
#newdata = as.data.frame(temp)
temp <- temp[order(temp$pid, temp$memeid_sharing),]

temp$HSQ_aff = as.numeric(temp$HSQ_aff)
temp$HSQ_se = as.numeric(temp$HSQ_se)
temp$HSQ_agg = as.numeric(temp$HSQ_agg)
temp$HSQ_sd = as.numeric(temp$HSQ_sd)

temp$ZHSQ_aff = scale(temp$HSQ_aff)
temp$ZHSQ_se = scale(temp$HSQ_se)
temp$ZHSQ_agg = scale(temp$HSQ_agg)
temp$ZHSQ_sd = scale(temp$HSQ_sd)

write.csv(temp, file="sharing_V3_HSQ.csv") # Write in a new csv file

####################################################################################################
#############       reverse the Big5 data        ####################
####################################################################################################

data_big5 = read.csv("sharing_V3_HSQ.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]
Flip <- function(x) abs(6 - x)


data_big5 <- mutate_at(data_big5,.vars = c("Q21.2","Q21.8","Q21.4", "Q21.5", "Q21.6"), Flip )
write.csv(data_big5, file="sharing_V3_HSQ_big5.csv") # Write in a new csv file

####################################################################################################
#############       Reverse Other's privacy scale        ####################
####################################################################################################

others_priv = read.csv("sharing_V3_HSQ_big5.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]
Flip <- function(x) abs(8 - x)

others_priv = others_priv[others_priv$Q23.9=="2",]
others_priv = subset(others_priv, select = -c(Q23.9) )

others_priv <- mutate_at(others_priv,.vars = c("Q23.8","Q23.12","Q23.13"), Flip )
write.csv(others_priv, file="sharing_V3_HSQ_big5_priv.csv") # Write in a new csv file

####################################################################################################
#############       Reverse image privacy scale        ####################
####################################################################################################

img_priv = read.csv("sharing_V3_HSQ_big5_priv.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]
Flip <- function(x) abs(9 - x)


img_priv <- mutate_at(img_priv,.vars = c("osn_freq","osn_imgshare_freq","imgshare_acquaintance","imgshare_fnf","imgshare_public"), Flip )
# remove attention check of social media privacy
img_priv = img_priv[img_priv$Q19.10=="1",]

write.csv(img_priv, file="sharing_V3_HSQ_big5_img_priv.csv") # Write in a new csv file


####################################################################################################
#############       Add Valence rating        ####################
####################################################################################################

sharing_data = read.csv("sharing_V3_HSQ_big5_img_priv.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]
clean_valence_data=read.csv("sharing_valence_data_final.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]

extract_valence_rating = cbind(clean_valence_data$qid, clean_valence_data$valence_rating) 
colnames(extract_valence_rating) <- c("qid","valence_rating")
extract_valence_rating = as.data.frame(extract_valence_rating)

mean_values = aggregate(extract_valence_rating[, 2], list(extract_valence_rating$qid), mean)
colnames(mean_values) <- c("memeid_sharing","valence_rating")

clean_data_valence <- merge(sharing_data, mean_values, by=c("memeid_sharing"))
write.csv(clean_data_valence, file="sharing_V3_HSQ_big5_img_priv_valence.csv") # Write in a new csv file




##################################################
################     Doomscrolling::: Factor ANALYSIS    #####################
##################################################
# --------------------- Factor analysis -----------
clean_sharing_data=read.csv("sharing_V3_HSQ_big5_img_priv_valence.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]

each_participant_data <-
  clean_sharing_data[,] %>% 
  group_by(pid) %>% 
  filter(row_number()==1)
#each_participant_data = na.omit(each_participant_data)
#---------- Feeling thermometer ----------------
doomscrolling_data =each_participant_data[,c("Q18.2",  "Q18.3",  "Q18.4",  "Q18.5")]

#doomscrolling_data=na.omit(doomscrolling_data)
cor_mat <- cor(doomscrolling_data)   #Creating Correlation Matrix 

cortest.bartlett(doomscrolling_data)
det(cor(doomscrolling_data))
parallel <- fa.parallel(doomscrolling_data)

KMO(cor(doomscrolling_data))
mydat <- doomscrolling_data[, KMO(doomscrolling_data)$MSAi>0.50]
mydat
KMO(mydat)

ev <- eigen(cor(doomscrolling_data)) # get eigenvalues
ev$values
plot(ev$values, type ="b")

scree(doomscrolling_data, pc=FALSE)
fa.parallel(doomscrolling_data, fa="fa")


fit <- factanal(doomscrolling_data, 1, rotation="varimax", scores = c("regression"))
#fit <- factanal(doomscrolling_data, 1)
print(fit, digits=2, cutoff=0.3, sort=TRUE)
loads <- fit$loadings
fa.diagram(loads)
head(fit$scores)
dim(fit$scores)

each_participant_data$doom_scrolling = fit$scores
colnames(each_participant_data)[132] ="doom_scrolling"
colnames(each_participant_data)

doomscrolling_data_subset =  cbind(each_participant_data$pid, each_participant_data$doom_scrolling)
colnames(doomscrolling_data_subset) <- c("pid", "doom_scrolling")

clean_data_doomscrolling <- merge(clean_sharing_data, doomscrolling_data_subset, by=c("pid"))

write.csv(clean_data_doomscrolling, file="sharing_V3_HSQ_big5_img_priv_valence_ds.csv") # Write in a new csv file


####################################################################################################
#############       FoMo data        ####################
####################################################################################################
fomo_data = read.csv("sharing_V3_HSQ_big5_img_priv_valence_ds.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]

fomo_data$fomo = rowSums(fomo_data[,c(76:85)])
write.csv(fomo_data, file="sharing_V3_HSQ_big5_img_priv_valence_ds_fomo.csv") # Write in a new csv file

####################################################################################################
#############       categorizing Big5 data        ####################
####################################################################################################
big5_data=read.csv("sharing_V3_HSQ_big5_img_priv_valence_ds_fomo.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]

# a$mean <- rowMeans(a[,c('high', 'low')], na.rm=TRUE)
# 
# extraversion = rowSums(big5_data[,c('Q21.2', 'Q21.7')], na.rm=TRUE)

big5_data$extraversion = rowSums(big5_data[,c('Q21.2', 'Q21.7')])
big5_data$agreeableness = rowSums(big5_data[,c('Q21.3', 'Q21.8')])
big5_data$conscientiousness = rowSums(big5_data[,c('Q21.4', 'Q21.9')])
big5_data$neuroticism = rowSums(big5_data[,c('Q21.5', 'Q21.10')])
big5_data$openness = rowSums(big5_data[,c('Q21.6', 'Q21.11')])

write.csv(big5_data, file="sharing_V3_HSQ_big5_img_priv_valence_ds_fomo.csv") # Write in a new csv file




####################################################################################################
#############      Removing participants who selected same responses 90% time        ####################
####################################################################################################
same_responses_data=read.csv("data_same_response.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]
all_data=read.csv("sharing_V3_HSQ_big5_img_priv_valence_ds_fomo.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]

subset_data = all_data[!(all_data$pid %in% same_responses_data$pid)]
subset_data = all_data %>%
  filter(!all_data$pid %in% same_responses_data$pid)

write.csv(subset_data, file="sharing_V3_HSQ_big5_img_priv_valence_ds_fomo_same_response.csv") # Write in a new csv file

####################################################################################################
#############      ::Categorized memes based on valence::        ####################
####################################################################################################
val_data=read.csv("sharing_V3_HSQ_big5_img_priv_valence_ds_fomo_same_response.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]
cat_valence_data=read.csv("sharing_val_categorized.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]

extract_valence_rating = cbind(cat_valence_data$qid, cat_valence_data$cat_val) 
colnames(extract_valence_rating) <- c("memeid_sharing","cat_valence")
extract_valence_rating = as.data.frame(extract_valence_rating)

each_meme <-
  extract_valence_rating[,] %>% 
  group_by(memeid_sharing) %>% 
  filter(row_number()==1)


clean_data_valence <- merge(val_data, each_meme, by=c("memeid_sharing"))

write.csv(clean_data_valence, file="sharing_V3_HSQ_big5_img_priv_valence_ds_fomo_same_response_val_cat.csv") # Write in a new csv file



####################################################################################################
#############       Renaming Demographic data        ####################
####################################################################################################
demog_data=read.csv("sharing_V3_HSQ_big5_img_priv_valence_ds_fomo_same_response_val_cat.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]


demog_data$gender[demog_data$gender == "Cisgender female " | demog_data$gender == "female (she/they)" | demog_data$gender == "female" |
                    demog_data$gender == "Female " | demog_data$gender == "woman"|
                    demog_data$gender == "female " | demog_data$gender == "Cis-Female"|
                    demog_data$gender == "Woman" | demog_data$gender == "She/Her" | 
                    demog_data$gender == "Female/Woman"] <- "Female"

demog_data$gender[demog_data$gender == "heterosexual male" | demog_data$gender == "male" |
                    demog_data$gender == "MALE" | demog_data$gender == "Man" |
                    demog_data$gender == "I am a Mang." | demog_data$gender == "boy" |
                    demog_data$gender == "mALE" | demog_data$gender == "man"] <- "Male"

demog_data$gender[demog_data$gender == "Non Binary" | demog_data$gender == "nonbinary" |
                    demog_data$gender == "Nonbinary" | demog_data$gender == "agender" | demog_data$gender == "non binary" |
                    demog_data$gender == "non-binary" | demog_data$gender == "trans male" | 
                    demog_data$gender == "Trans man" | demog_data$gender == "Genderfluid" |
                    demog_data$gender == "trans male "] <- "Non-binary"
table(demog_data$gender)

demog_data = demog_data[demog_data$gender=="Male" | demog_data$gender=="Female" | demog_data$gender=="Non-binary",] 

demog_data$age[demog_data$age == "1"] <- "under_18"
demog_data$age[demog_data$age == "2"] <- "18-29"
demog_data$age[demog_data$age == "3"] <- "30-49"
demog_data$age[demog_data$age == "4"] <- "50-64"
demog_data$age[demog_data$age == "5"] <- "65_older"
table(demog_data$age)

demog_data$education[demog_data$education == "1"] <- "none"
demog_data$education[demog_data$education == "2"] <- "1st-4th_grade"
demog_data$education[demog_data$education == "3"] <- "4th-8th_grade"
demog_data$education[demog_data$education == "4"] <- "9th-12th_grade"
demog_data$education[demog_data$education == "5"] <- "High_school_graduate_or_GED"
demog_data$education[demog_data$education == "6"] <- "Some_college_no_degree"
demog_data$education[demog_data$education == "7"] <- "Associate’s_degree"
demog_data$education[demog_data$education == "8"] <- "Bachelor's"
demog_data$education[demog_data$education == "9"] <- "Master's"
demog_data$education[demog_data$education == "10"] <- "Professional_degree"
demog_data$education[demog_data$education == "11"] <- "Doctoral"
table(demog_data$education)

#demog_data = demog_data[demog_data$race=="4" | demog_data$race=="6" | demog_data$race=="3",]

demog_data$race[demog_data$race == "6"] <- "White"
demog_data$race[demog_data$race == "4"] <- "Black"
demog_data$race[demog_data$race == "3"] <- "Asian"

demog_data = demog_data[demog_data$race=="White" | demog_data$race=="Black" | demog_data$race=="Asian",] 
table(demog_data$race)

table(demog_data$political_orientation)

### ------- Rename feeling thermometer scales 
demog_data <- demog_data %>% 
  rename( "ft_asian" = "Q15.2_1",
          "ft_black" = "Q15.3_1",
         "ft_white" = "Q15.4_1", 
          "ft_christians" = "Q15.5_1",
         "ft_muslims" = "Q15.6_1",
         "ft_republicans" = "Q15.7_1",
         "ft_democrats" = "Q15.8_1",
         "ft_hispanic" = "Q15.9_1",
         "ft_native" = "Q15.10_1")

write.csv(demog_data, file="sharing_V3_clean_final.csv") # Write in a new csv file



