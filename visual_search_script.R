# visual foraging analysis and plotting script

# libraries
library(tidyverse)
library(caret)
library(lme4)
library(lmerTest)
library(ModelMetrics)
library(ggplot2)
library(pROC)
library(jpeg)
library(grid)


#-------- # visual search modelling and cross validation loop ----

# making a subset without the social engament tasks as they have NA for SearchType and R can't deal with that
sub = subset(fix_V2, Task == "VisualSearch")

# full model
m1 = glmer(Duration ~ SearchType*Trial + (1+SearchType|ParticipantID), data=sub, family = gaussian(link = log))
summary(m1)

# we remove "Searchorder" as the variable is balanced, half the participants start with star and vice versa
# "Fixation" we drop as well
# Condition and Trial, should it be interaction or not? Conceptually it makes sense, but does it make our model better?
# We log transform because duration is not normally distributed, like its specified in the model (no need to log transform duration as we did on first semeter) 
# if we want to scale to z scores you use the scale(trial) and scale(duration)

# Conceptually, we have removed some of the variables that are less important for the hypothesis
# But we want to test the remaining variables to see which variables are important statistically
# we want to test which variables make sense to have in the model by cross validating
# For that reason, we create 3 models and cross validate them

m2 = glmer(Duration ~ SearchType + Trial + (1+SearchType|ParticipantID), data=sub, family = gaussian(link = log))
m3 = glmer(Duration ~ SearchType + (1+SearchType|ParticipantID), data=sub, family = gaussian(link = log))
summary(m3)

plot(m3)
anova(m1,m2,m3)

#Create folds from ParticipantID, which needs to be transformed first. 
# Transformed from factor to character to make the variable forget the factor information
# Then transformed to factor again and then to numeric
# sub$ParticipantN=as.numeric(as.factor(as.character(sub$ParticipantID)))
sub$ParticipantID = as.character(sub$ParticipantID)
sub$ParticipantID = as.factor(sub$ParticipantID)
sub$ParticipantID = as.numeric(sub$ParticipantID)

folds=createFolds(unique(sub$ParticipantID), k = 3)

# define which model you want to run cross validation on
model = m2
list = 1

results3_a = data.frame()
results3_c = data.frame()

k = 1
for (d in list){
  for (k in folds){
    #------ Split into training and test data ------ 
    #Create training dataset, data not in fold k
    data_train=subset(sub,!(ParticipantID %in% k))
    #Create test dataset, data in fold k
    data_test=subset(sub,ParticipantID %in% k)
    
    #------ train model - apply model to data_train ------
    predict_train = predict(model, data_train, allow.new.levels =TRUE)
    rmse_train = rmse(data_train$Duration, predict_train)
    
    #------ test the model - test model on data_test (last quarter) ------
    #Make predictions based on modeVIS
    predict_test=predict(model, data_test, allow.new.levels = TRUE)
    rmse_test = rmse(data_test$Duration, predict_test)
    
    #------ save the performance ------  
    one_row = data.frame(rmse_train, rmse_test)
    results3_a = rbind(results3_a, one_row)
    
  }
  # adding mean and sd rmse
  results3_b = data.frame(mean_rmse = mean(results3_a$rmse_test), sd_rmse = sd(results3_a$rmse_test))
  results3_c = rbind(results3_c, results3_b)
}
  

  #------------# visualizing SearchType----


ggplot(subset(fix_V2, SearchType == "Count"), aes(Duration)) +geom_density()


#---------# heatmap plot from Fabio ----
# defining colors, could be anything
jet.colors = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

# load image
img <- readJPEG("eyetrackingscripts/foraging/ng090ws.jpg")
g <- rasterGrob(img, interpolate=TRUE)

# density plot
ggplot(subset(fix_V2, Task=='VisualSearch' & ParticipantID=='6_3_m2' & Trial==6), aes(x = PositionX, y = PositionY)) +
  xlim(0,1920) + # resolution of experiment screen
  ylim(0, 1080) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=0, ymax=1080) + #xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  stat_density2d(geom="raster", aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=1000) + 
  scale_alpha(range = c(0.1, 0.6)) + scale_fill_gradientn(colours = jet.colors(10), trans="sqrt")

#-----# heatmap plot for one participant for two conditions ----

# defining colors, could be anything
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

# plot for search condition
# load image
img2 <- readJPEG("eyetrackingscripts/foraging/ng064ws.jpg")
g2 <- rasterGrob(img2, interpolate=TRUE)

# density plot
ggplot(subset(fix_V2, Task=='VisualSearch' & ParticipantID=='3_1_f1' & Trial==4), aes(x = PositionX, y = PositionY)) +
  xlim(0,1920) + # resolution of experiment screen
  ylim(0, 1080) +
  annotation_custom(g2, xmin=-Inf, xmax=Inf, ymin=0, ymax=1080) + #xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  stat_density2d(geom="raster", aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=1000) + 
  scale_alpha(range = c(0.1, 0.6)) + scale_fill_gradientn(colours = jet.colors(10), trans="sqrt")

# plot for count condition
# load image
img3 <- readJPEG("eyetrackingscripts/foraging/ng090ws.jpg")
g3 <- rasterGrob(img3, interpolate=TRUE)

# density plot
ggplot(subset(fix_V2, Task=='VisualSearch' & ParticipantID=='3_1_f1' & Trial==6), aes(x = PositionX, y = PositionY)) +
  xlim(0,1920) + # resolution of experiment screen
  ylim(0, 1080) +
  annotation_custom(g3, xmin=-Inf, xmax=Inf, ymin=0, ymax=1080) + #xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  stat_density2d(geom="raster", aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=1000) + 
  scale_alpha(range = c(0.1, 0.6)) + scale_fill_gradientn(colours = jet.colors(10), trans="sqrt")

# scanpath plot for participant "1_1_f1"
# load image
img4 <- readJPEG("eyetrackingscripts/foraging/ng021ws.jpg")
g4 <- rasterGrob(img4, interpolate=TRUE)

# scanpath plot
# redoing the order, because the scanpath plot takes the order of the data frame
# this is ordered in a subset data set by Fixation, so we can see the fixation order in the plot
x = subset(fix_V2, Task == 'VisualSearch' & ParticipantID=='1_1_f1' & Trial==1)
x = x[order(x$Fixation),]

ggplot(x, aes(x=PositionX, y=1081-PositionY, label=Fixation)) +
  annotation_custom(g4, xmin=-Inf, xmax=Inf, ymin=0, ymax=1080)+
  geom_point(size = subset(fix_V2, Task == 'VisualSearch' & ParticipantID=='1_1_f1' & Trial==1)$Duration/25 , alpha = 0.5, color = "red") +
  geom_path(size = 1, alpha = 0.3, color = "red") +
  geom_text(aes(label = Fixation, size = 5))
  
  
