#calculate play-by-play spread changes

setwd("/Users/evanthompson/SDP/spread")

#load packages
library(nflscrapR)
library(nnet)
library(dplyr)
library(caret)



# # load 2018 play-by-play data
season2009 <- season_play_by_play(Season = 2009)
season2010 <- season_play_by_play(Season = 2010)
season2011 <- season_play_by_play(Season = 2011)
season2012 <- season_play_by_play(Season = 2012)
season2013 <- season_play_by_play(Season = 2013)
season2014 <- season_play_by_play(Season = 2014)
season2015 <- season_play_by_play(Season = 2015)
season2016 <- season_play_by_play(Season = 2016)
season2017 <- season_play_by_play(Season = 2017)
season2018 <- season_play_by_play(Season = 2018)



# binding into a single dataset
all_seasons_orig <- rbind(season2009, season2010, season2011, season2012,
                     season2013,season2014, season2015, season2016,
                     season2017, season2018)
saveRDS(all_seasons, file = "/Users/evanthompson/SDP/spread_data/all_seasons_orig.rds")


#can start from here
#all_seasons_orig <- readRDS("~/SDP/spread_data/all_seasons.rds")
all_seasons <- all_seasons_orig


#cleaning
all_seasons <- all_seasons[ which(all_seasons$ExPointResult == "NA"),]
all_seasons <- all_seasons[ which(all_seasons$TwoPointConv == "NA"),]
all_seasons$DefensiveTeam <- ifelse(all_seasons$DefensiveTeam == "JAX", "JAC", all_seasons$DefensiveTeam)
all_seasons <- all_seasons[ which(all_seasons$down != "NA"),]
all_seasons$por <- ifelse(substr(all_seasons$desc, start=5, stop=21) == "play under review", 1, 0)
all_seasons <- all_seasons[ which( all_seasons$por == 0),]
all_seasons <- all_seasons[ which(all_seasons$qtr != 5),]
all_seasons$DesiredPlayType <- ifelse(all_seasons$PlayType == "Run" | all_seasons$PlayType == "Pass" 
                                      | all_seasons$PlayType == "Field Goal" | all_seasons$PlayType == "Sack" 
                                      | all_seasons$PlayType == "Punt",1,0)
all_seasons <- all_seasons[ which(all_seasons$DesiredPlayType == 1),]
all_seasons$offense_year <- paste(all_seasons$posteam, all_seasons$Season, sep = "_", collapse = NULL)
all_seasons$defense_year <- paste(all_seasons$DefensiveTeam, all_seasons$Season, sep = "_", collapse = NULL)
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

# # MODEL 1: create multinomial logistic regression model 1 (w/ team effect, year-specific team)
# modFit_multi1 <- multinom(home_team_margin_final ~  factor(down) + ydstogo + yrdline100 + factor(offense_year) + 
#                    factor(defense_year), 
#                  data=all_seasons, MaxNWts = 99999)
# print(modFit_multi1)
# saveRDS(modFit_multi1, file = "modFit_multi1.rds")


# MODEL 2 (random forest)
modFit_rf <- train(home_team_margin_final ~ down + ydstogo + yrdline100 + offense_year
                   + defense_year + TimeSecs + home_team_score + away_team_score, 
                   method = "rf",
                   trControl = trainControl(method = "cv", number = 5), 
                   metric = "RMSE",
                   data = training)
print(modFit_rf) # RMSE (in-sample) = 

#test
pred_rf <- predict(modFit_rf, testing)
testing_predictions <- cbind(predict(modFit_rf, testing), testing)
testing_predictions <- testing_predictions[,c(1,112,2:111)]
names(testing_predictions)[1] <- "home_team_margin_final_predict"


#merge predictions onto dataset
training_predictions <- cbind(predict(modFit_rf, training), training)
training_predictions <- training_predictions[,c(1,112,2:111)]
names(training_predictions)[1] <- "home_team_margin_final_predict"
RMSE(predict(modFit_rf, testing), testing$home_team_margin_final)
testing_predictions <- testing_predictions[,c("home_team_score", "away_team_score", "home_team_margin_final",
                                              "home_team_margin_final_predict")]





















#train final model on master dataset, merge onto master dataset
modFit_rf_final <- train(home_team_margin_final ~ down + ydstogo + yrdline100 + offense_year
                   + defense_year + TimeSecs + home_team_score + away_team_score, 
                   method = "rf",
                   trControl = trainControl(method = "cv", number = 5), 
                   metric = "RMSE",
                   data = all_seasons)
print(modFit_rf_final) # RMSE (in-sample) = 

#merge predictions onto dataset
all_seasons_predictions <- cbind(predict(modFit_rf_final, all_seasons), all_seasons)
#all_seasons_predictions <- tall_seasons_predictions[,c(1,112,2:111)]
#names(training_predictions)[1] <- "home_team_margin_final_predict"
