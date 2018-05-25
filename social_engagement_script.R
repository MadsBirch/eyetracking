# social engagement analysis and plotting script

# ----- load libraries ----
library(tidyverse)
library(caret)
library(lme4)
library(lmerTest)
library(ModelMetrics)
library(ggplot2)
library(pROC)
library(jpeg)
library(grid)
library(pastecs)
library(waic)
library(brms)
# subset data
sample = subset(samples_V2, Task == "SocialEngagement" & Blink == 0)

# downsample
#sample = sample[sample(nrow(sample), 10000), ]

# ------ modelling ----- 
# plotting pupil size to investigate distribution
# looks binomial?
ggplot(sample, aes(PupilSize))+geom_density()
ggplot(sample, aes(logPupil))+geom_density()

# scaling time and pupil size
sample$TimeScaled = scale(sample$TrialTime)
sample$PupilScaled = scale(sample$PupilSize)

# log transform pupilsize
sample$logPupil = log(sample$PupilSize+1)


# creating models

# want to specify model with family = "binomial" as PupilSize looks like it is binomial
# this model gives the following error: Error in eval(family$initialize, rho) : y values must be 0 <= y <= 1
soc_m1_test = glmer(logPupil~ Directionality + Ostension + TimeScaled + (1+ Directionality+Ostension|ParticipantID), sample, family = "binomial")
summary(soc_m1_test)


# tried recoding variables to 1 and 0, did not help
soc_sub$Directionality = as.character(soc_sub$Directionality)
soc_sub$Ostension = as.character(soc_sub$Ostension)

soc_sub$Directionality[soc_sub$Directionality == "dir"] = 1
soc_sub$Directionality[soc_sub$Directionality == "div"] = 0

soc_sub$Ostension[soc_sub$Ostension == "+o"] = 1
soc_sub$Ostension[soc_sub$Ostension == "-o"] = 0



# MODELS 
soc_null_m = lmer(logPupil ~ 1 + (1|ParticipantID), sample)
summary(soc_null_m)

soc_m1 = lmer(logPupil ~ Directionality * Ostension + TimeScaled + (1+ Directionality+Ostension|ParticipantID), REML = F, sample)
summary(soc_m1)
plot(soc_m1)

soc_m2 = lmer(logPupil ~ Directionality + Ostension + TimeScaled + (1+ Directionality+Ostension|ParticipantID), sample)
summary(soc_m2)
plot(soc_m2)

soc_m3 = lmer(logPupil ~ Directionality + TimeScaled + (1|ParticipantID), sample)
summary(soc_m3)

soc_m4 = lmer(logPupil~ Ostension + TimeScaled + (1+ Ostension|ParticipantID), sample)
summary(soc_m4)


AIC(soc_m1, soc_m2,soc_m3,soc_m4)
anova(soc_m1, soc_m2,soc_m3,soc_m4)

sjp.lmer(soc_m1)

# Create folds from ParticipantID, which needs to be transformed first. 
# Transformed from factor to character to make the variable forget the factor information
# Then transformed to factor again and then to numeric
# sub$ParticipantN=as.numeric(as.factor(as.character(sub$ParticipantID)))
sample$ParticipantID = as.character(sample$ParticipantID)
sample$ParticipantID = as.factor(sample$ParticipantID)
sample$ParticipantID = as.numeric(sample$ParticipantID)
folds=createFolds(unique(sample$ParticipantID), k = 3)

# define which model you want to run cross validation on
model = soc_m1

list = 1

results1_a = data.frame()
results_soc = data.frame()

# k = 1
for (d in list){
  for (k in folds){
    #------ Split into training and test data ------ 
    #Create training dataset, data not in fold k
    data_train=subset(sample,!(ParticipantID %in% k))
    #Create test dataset, data in fold k
    data_test=subset(sample,ParticipantID %in% k)
    
    #------ train model - apply model to data_train ------
    predict_train = predict(model, data_train, allow.new.levels =TRUE)
    rmse_train = rmse(data_train$logPupil, predict_train)
    
    #------ test the model - test model on data_test (last quarter) ------
    #Make predictions based on modeVIS
    predict_test=predict(model, data_test, allow.new.levels = TRUE)
    rmse_test = rmse(data_test$logPupil, predict_test)
    
    #------ save the performance ------  
    one_row = data.frame(rmse_train, rmse_test)
    results1_a = rbind(results1_a, one_row)
    
  }
  # adding mean and sd rmse
  results1_b = data.frame(mean_rmse = mean(results1_a$rmse_test), sd_rmse = sd(results1_a$rmse_test))
  results_soc = rbind(results1_c, results1_b)
}


# ----- visualizing -----

coef = coef(soc_m1)
plot(coef)

plot(soc_m1)

ggplot(sample, aes(x=TrialTime, y=PupilSize))+
  geom_smooth(aes(color = Ostension))+
  facet_grid(~ Directionality)


coef %>% 
  # save predicted values
  mutate(pred_dist = fitted(soc_m1)) %>% 
  # graph
  ggplot(aes(x=logPupil, y=pred_dist, group=ParticipantID, color=ParticipantID)) + theme_classic() +
  geom_line(size=1)

data %>% 
  # data
  mutate(L1_resid = residuals(soc_m1, type = "response")) %>% 
  # graph
  ggplot(aes(x=L1_resid)) + theme_classic() +
  geom_histogram(colour="black", fill="grey") 
