#calculate play-by-play spread changes
#setwd("/Users/evanthompson/SDP/spread")

#load packages
library(nflscrapR)
library(nnet)
library(dplyr)
library(caret)
library(lubridate)
library(rpart)
library(tictoc)
library(randomForest)

set.seed(70)



# # load 2018 play-by-play data
# season2009 <- season_play_by_play(Season = 2009)
# season2010 <- season_play_by_play(Season = 2010)
# season2011 <- season_play_by_play(Season = 2011)
# season2012 <- season_play_by_play(Season = 2012)
# season2013 <- season_play_by_play(Season = 2013)
# season2014 <- season_play_by_play(Season = 2014)
# season2015 <- season_play_by_play(Season = 2015)
# season2016 <- season_play_by_play(Season = 2016)
# season2017 <- season_play_by_play(Season = 2017)
# season2018 <- season_play_by_play(Season = 2018)
# 
# 
# # binding into a single dataset
# all_seasons_orig <- rbind(season2009, season2010, season2011, season2012,
#                      season2013,season2014, season2015, season2016,
#                      season2017, season2018)
# saveRDS(all_seasons_orig, file = "/Users/evanthompson/SDP/spread_data/all_seasons_orig.rds")



#start from here
#all_seasons_orig <- readRDS("~/SDP/spread_data/all_seasons_orig.rds")
all_seasons_orig <- readRDS("C:/Users/v-evtho/Documents/Personal/all_seasons_orig.rds")
all_seasons <- all_seasons_orig
historicalspreads_clean <- read.csv("C:/Users/v-evtho/Documents/Personal/historicalspreads_clean.csv")
historicalspreads_clean$Date <- as.POSIXlt(mdy(as.character(historicalspreads_clean$Date)))


#merge on historical spreads by HomeTeam, Date
all_seasons <- merge(all_seasons, historicalspreads_clean, by = c("Date", "HomeTeam"))


#cleaning
all_seasons <- all_seasons[ which(is.na(all_seasons$ExPointResult)),]
all_seasons <- all_seasons[ which(is.na(all_seasons$TwoPointConv)),]
all_seasons$DefensiveTeam <- ifelse(all_seasons$DefensiveTeam == "JAX", "JAC", all_seasons$DefensiveTeam)
all_seasons <- all_seasons[ which(!is.na(all_seasons$down)),]
all_seasons$por <- ifelse(substr(all_seasons$desc, start=5, stop=21) == "play under review", 1, 0)
all_seasons <- all_seasons[ which( all_seasons$por == 0),]
all_seasons <- all_seasons[ which(all_seasons$qtr != 5),]
all_seasons$DesiredPlayType <- ifelse(all_seasons$PlayType == "Run" | all_seasons$PlayType == "Pass" 
                                      | all_seasons$PlayType == "Field Goal" | all_seasons$PlayType == "Sack" 
                                      | all_seasons$PlayType == "Punt",1,0)
all_seasons <- all_seasons[ which(all_seasons$DesiredPlayType == 1),]
all_seasons$offense_year <- paste(all_seasons$posteam, all_seasons$Season, sep = "_", collapse = NULL)
all_seasons$defense_year <- paste(all_seasons$DefensiveTeam, all_seasons$Season, sep = "_", collapse = NULL)
all_seasons$AwayTeam <- all_seasons$AwayTeam.x
# all_seasons$end_of_half <- ifelse(all_seasons$TimeSecs <= 2040 & all_seasons$TimeSecs >= 1800, 1, 0)
# all_seasons <- all_seasons[order(all_seasons$GameID, -all_seasons$TimeSecs),] 
# all_seasons$end_of_half_remove <- 0
# for (k in 1:nrow(all_seasons) - 1){
#   all_seasons$end_of_half_remove[k] <- ifelse(all_seasons$end_of_half[k] == 1 & 
#                                                 all_seasons$Drive[k] == all_seasons$Drive[k+1], 1, 0)
# }
# all_seasons$end_of_half_remove <- ifelse(all_seasons$TimeSecs <= 1840 & all_seasons$TimeSecs >= 1800, 1, 0)
# all_seasons <- all_seasons[ which(all_seasons$end_of_half_remove == 0 | all_seasons$end_of_half_remove == "NA"), ]
# all_seasons$clock_melt_1 <- ifelse(all_seasons$qtr == 4 & all_seasons$AbsScoreDiff > 17, 1, 0)
# all_seasons$clock_melt_2 <- ifelse(all_seasons$TimeSecs < 300 & all_seasons$AbsScoreDiff > 8, 1, 0)
# all_seasons$clock_melt_3 <- ifelse(all_seasons$TimeSecs < 240, 1, 0)
# all_seasons <- all_seasons[order(all_seasons$GameID, -all_seasons$TimeSecs),] 
# all_seasons$clock_melt <- 0
# for (k in 1:nrow(all_seasons)){
#   all_seasons$clock_melt[k] <- max(all_seasons$clock_melt_1[k], all_seasons$clock_melt_2[k], all_seasons$clock_melt_3[k])
# }
# all_seasons <- all_seasons[order(all_seasons$GameID, -all_seasons$TimeSecs),] 
# all_seasons$clock_melt_remove <- 0
# for (k in 1:nrow(all_seasons) - 1){
#   all_seasons$clock_melt_remove[k] <- ifelse(all_seasons$clock_melt[k] == 1 & 
#                                                all_seasons$Drive[k] == all_seasons$Drive[k+1], 1, 0)
# }
# all_seasons <- all_seasons[ which(all_seasons$clock_melt_remove != 1), ]
#end data cleaning



#create data labels
all_seasons$home_team_score <- as.numeric(ifelse(all_seasons$posteam == all_seasons$HomeTeam, 
                                                 all_seasons$PosTeamScore, 
                                                 all_seasons$DefTeamScore)) 
all_seasons$away_team_score <- as.numeric(ifelse(all_seasons$posteam == all_seasons$AwayTeam, 
                                                 all_seasons$PosTeamScore, 
                                                 all_seasons$DefTeamScore)) 
all_seasons$home_team_margin <- all_seasons$home_team_score - all_seasons$away_team_score
final_scores <- all_seasons %>%
  group_by(GameID) %>%
  filter(row_number()==n())
final_scores <- final_scores[,c("GameID", "home_team_margin")]
names(final_scores) <- c("GameID", "home_team_margin_final")
all_seasons <- merge(all_seasons, final_scores, by = "GameID")
all_seasons$home_team_pos_ball <- ifelse(all_seasons$posteam == all_seasons$HomeTeam, 1, 0)
all_seasons$home_team_pos_ball_neg <- ifelse(all_seasons$posteam == all_seasons$HomeTeam, 1, -1)
all_seasons$home_team_margin_final <- all_seasons$home_team_margin_final_validate





# rm(season2009)
# rm(season2010)
# rm(season2011)
# rm(season2012)
# rm(season2013)
# rm(season2014)
# rm(season2015)
# rm(season2016)
# rm(season2017)
# rm(season2018)

#all_seasons <- all_seasons[c(1:2000),]


#partition
inTrain <- createDataPartition(y=all_seasons$home_team_margin_final, p=0.7, list = FALSE)
training <- all_seasons[inTrain,]
testing <- all_seasons[-inTrain,]




# models

# # # MODEL 1: create multinomial logistic regression model 1 (w/ team effect, year-specific team)
# modFit_multi1 <- multinom(home_team_margin_final ~ down + ydstogo + yrdline100 + home_team_pos_ball + home_team_margin + TimeSecs, 
#                  data=training, MaxNWts = 99999)
# print(modFit_multi1)
# training_predictions <- cbind(predict(modFit_multi1, training), training)
# training_predictions <- training_predictions[,c(1,120,2:119)]
# names(training_predictions)[1] <- "home_team_margin_final_predict"
# RMSE(training_predictions$home_team_margin_final_predict, training_predictions$home_team_margin_final)
# # saveRDS(modFit_multi1, file = "modFit_multi1.rds")


# # MODEL 2 (random forest)
# modFit_rf <- train(home_team_margin_final ~ down + ydstogo + yrdline100 + home_team_pos_ball + 
#                          home_team_margin + TimeSecs + HomeTeam + AwayTeam + home_team_spread, 
#                    method = "rf",
#                    trControl = trainControl(method = "cv", number = 3), 
#                    metric = "RMSE",
#                    data = training)
# print(modFit_rf) # RMSE (in-sample) = 2.86
# 
# 
# #merge predictions onto dataset
# training_predictions <- cbind(predict(modFit_rf, training), training)
# testing_predictions <- cbind(predict(modFit_rf, testing), testing)
# training_predictions <- training_predictions[,c(1,120,2:119)]
# testing_predictions <- testing_predictions[,c(1,120,2:119)]
# names(training_predictions)[1] <- "home_team_margin_final_predict"
# names(testing_predictions)[1] <- "home_team_margin_final_predict"
# RMSE(predict(modFit_rf, testing), testing$home_team_margin_final)
# testing_predictions <- testing_predictions[,c("home_team_score", "away_team_score", "home_team_margin_final",
#                                               "home_team_margin_final_predict", "home_team_spread")]
# #test
# pred_rf <- predict(modFit_rf, testing)
# testing_predictions <- cbind(predict(modFit_rf, testing), testing)
# testing_predictions <- testing_predictions[,c(1,120,2:119)]
# names(testing_predictions)[1] <- "home_team_margin_final_predict"





# # MODEL 3 (linear regression)
# modFit_linear1 <- lm(home_team_margin_final ~ down*home_team_pos_ball_neg + ydstogo*home_team_pos_ball_neg + 
#                            yrdline100*home_team_pos_ball_neg + home_team_margin + TimeSecs + HomeTeam + AwayTeam +
#                            home_team_spread
#                      ,
#                      data=training)
# print(modFit_linear1)
# training_predictions <- cbind(predict(modFit_linear1, training), training)
# training_predictions <- training_predictions[,c(1,120,2:119)]
# names(training_predictions)[1] <- "home_team_margin_final_predict"
# RMSE(training_predictions$home_team_margin_final_predict, training_predictions$home_team_margin_final)
# # saveRDS(modFit_multi1, file = "modFit_multi1.rds")
# pred_rf <- predict(modFit_linear1, testing)
# testing_predictions <- cbind(predict(modFit_linear1, testing), testing)
# testing_predictions <- testing_predictions[,c(1,120,2:119)]
# names(testing_predictions)[1] <- "home_team_margin_final_predict"
# RMSE(testing_predictions$home_team_margin_final_predict, testing_predictions$home_team_margin_final)







# # MODEL 4 (random forest) 
# modFit_rf4 <- train(home_team_margin_final ~ down + ydstogo + yrdline100 + home_team_pos_ball + 
#                           home_team_margin + TimeSecs + HomeTeam + AwayTeam + home_team_spread, 
#                     method = "rf",
#                     trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), 
#                     metric = "RMSE",
#                     data = training)
# print(modFit_rf4) # RMSE (in-sample) =




# # MODEL 5 (regression tree rpart)
# modFit_rpart <- rpart(home_team_margin_final ~ down + ydstogo + yrdline100 + home_team_pos_ball +
#                           home_team_margin + TimeSecs + HomeTeam + AwayTeam + home_team_spread, 
#                     method="anova",
#                     data = training)
# print(modFit_rpart) # RMSE (in-sample) 
# training_predictions <- cbind(predict(modFit_rpart, training), training)
# training_predictions <- training_predictions[,c(1,120,2:119)]
# names(training_predictions)[1] <- "home_team_margin_final_predict"
# RMSE(training_predictions$home_team_margin_final_predict, training_predictions$home_team_margin_final)


# 
# 
# # MODEL 6 (Bagged CART) TOO GOOD TO BE TRUE
# modFit_tbag <- train(home_team_margin_final ~ down + ydstogo + yrdline100 + home_team_pos_ball + 
#                            home_team_margin + TimeSecs + HomeTeam + AwayTeam + home_team_spread, 
#                      method = "brnn",
#                      trControl = trainControl(method = "cv", number = 5), 
#                      metric = "RMSE",
#                      data = training)
# print(modFit_tbag) # RMSE (in-sample) =
# training_predictions <- cbind(predict(modFit_tbag, training), training)
# training_predictions <- training_predictions[,c(1,120,2:119)]
# names(training_predictions)[1] <- "home_team_margin_final_predict"
# RMSE(training_predictions$home_team_margin_final_predict, training_predictions$home_team_margin_final)
# testing_predictions <- cbind(predict(modFit_tbag, testing), testing)
# testing_predictions <- testing_predictions[,c(1,120,2:119)]
# names(testing_predictions)[1] <- "home_team_margin_final_predict"
# RMSE(testing_predictions$home_team_margin_final_predict, testing_predictions$home_team_margin_final)





# # MODEL 7 (bayes glm)
# modFit_bayesglm <- train(home_team_margin_final ~ down + ydstogo + yrdline100 + home_team_pos_ball + 
#                           home_team_margin + TimeSecs + HomeTeam + AwayTeam + home_team_spread, 
#                     method = "bayesglm",
#                     trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), 
#                     metric = "RMSE",
#                     data = training)
# print(modFit_bayesglm) # RMSE (in-sample) =




# # MODEL 8 (gamboost)
# modFit_gamb <- train(home_team_margin_final ~ down + ydstogo + yrdline100 + home_team_pos_ball + 
#                                home_team_margin + TimeSecs + HomeTeam + AwayTeam + home_team_spread, 
#                          method = "gamboost",
#                          trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), 
#                          metric = "RMSE",
#                          data = training)
# print(modFit_gamb) # RMSE (in-sample) =




# # MODEL 9 (glmboost)
# modFit_glmb <- train(home_team_margin_final ~ down + ydstogo + yrdline100 + home_team_pos_ball + 
#                            home_team_margin + TimeSecs + HomeTeam + AwayTeam + home_team_spread, 
#                      method = "glmboost",
#                      trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), 
#                      metric = "RMSE",
#                      data = training)
# print(modFit_glmb) # RMSE (in-sample) =




# # MODEL 10 (BstLm) 
# modFit_bstlm <- train(home_team_margin_final ~ down + ydstogo + yrdline100 + home_team_pos_ball + 
#                            home_team_margin + TimeSecs + HomeTeam + AwayTeam + home_team_spread, 
#                      method = "BstLm",
#                      trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), 
#                      metric = "RMSE",
#                      data = training)
# print(modFit_bstlm) # RMSE (in-sample) =




# # MODEL 11 (blackboost)
# modFit_black <- train(home_team_margin_final ~ down + ydstogo + yrdline100 + home_team_pos_ball + 
#                             home_team_margin + TimeSecs + HomeTeam + AwayTeam + home_team_spread, 
#                       method = "blackboost",
#                       trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), 
#                       metric = "RMSE",
#                       data = training)
# print(modFit_black) # RMSE (in-sample) =






keep_vars <- c("home_team_margin_final", "down", "ydstogo", "yrdline100", "home_team_pos_ball", "home_team_pos_ball_neg",
               "home_team_margin", "TimeSecs", "HomeTeam", "AwayTeam", "home_team_spread")
training <- training[,keep_vars]
testing <- testing[,keep_vars]




#testing with randomForest package
training_small <- sample_n(training, 120000)
testing_small <- sample_n(testing, 30000)
training_small=training_small %>% mutate_if(is.character, as.factor)
testing_small=testing_small %>% mutate_if(is.character, as.factor)
modFit_test <- randomForest(home_team_margin_final ~ down + ydstogo + yrdline100 + home_team_pos_ball + 
                              home_team_margin + TimeSecs + HomeTeam + AwayTeam + home_team_spread, 
                            trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), 
                            ntree = 200,
                            data = training_small)
print(modFit_test)
postResample(predict(modFit_test, training_small), training_small$home_team_margin_final)
postResample(predict(modFit_test, testing_small), testing_small$home_team_margin_final)
testing_predict <- cbind(testing_small, predict(modFit_test, testing_small))
testing_predict <- testing_predict[,c(11,7,1,12,2:6,8:10)]
names(testing_predict)[4] <- "home_team_margin_final_predict"
testing_predict$home_team_margin_final_predict <- round(testing_predict$home_team_margin_final_predict,1)
testing_predict$e <- (testing_predict$home_team_margin_final_predict - testing_predict$home_team_margin_final)
testing_predict$ee <- testing_predict$e**2
#RMSE
mean(testing_predict$ee)**0.5
testing_predict$e_abs <- abs(testing_predict$e)
mean(testing_predict$e_abs)

save(modFit_test, file = "C:/Users/v-evtho/Documents/Personal/modFit_test.rds")















# #FINAL MODEL: Model 4 (rf)
# #train final model on master dataset, merge onto master dataset
# modFit_final <- train(home_team_margin_final ~ down + ydstogo + yrdline100 + home_team_pos_ball + 
#                         home_team_margin + TimeSecs + HomeTeam + AwayTeam + home_team_spread, 
#                       method = "rf",
#                       trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), 
#                       metric = "RMSE",
#                       data = all_seasons)
# print(modFit_final) # RMSE (in-sample) =
# 
# #merge predictions onto dataset
# all_seasons_predictions <- cbind(predict(modFit_final, all_seasons), all_seasons)
# all_seasons_predictions <- all_seasons_predictions[,c(1,120,2:119)]
# names(all_seasons_predictions)[1] <- "home_team_margin_final_predict"
# RMSE(all_seasons_predictions$home_team_margin_final_predict, all_seasons_predictions$home_team_margin_final)
# saveRDS(modFit_final, file = "modFit_final.rds")
# 
# 
# 
# 
# 
# rm(training)
# rm(testing)
# rm(inTrain)
# rm(final_scores)
# rm(all_seasons)
# rm(all_seasons_orig)
# 
# write.csv(all_seasons_predictions, "/Users/evanthompson/SDP/spread_data/all_seasons_predictions.csv")
