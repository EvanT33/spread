#predict play-by-play spread changes

#How to use: predict(modFit_final, sample_input) outputs spread prediction,
#where sample_input is a n x 9 dataframe consisting of the 9 input vars.

#What this script does:
# 0. (Optionally creates historical pbp dataset using nflscrapR)
# 1. Read in historical pbp data
# 2. Read in historical spread data
# 3. Merge datasets, create all_seasons (master dataset)
# 4. Clean data. Remove unwanted observations
# 5. Create dependent variable (what we want to predict), home_team_margin_final
# 6. (Optionally creates train/test split, for model tweaking)



#change working directory to wherever 2 input files are located. This script
#will create all_seasons.rds & modFit_final.rds, which are too large for GitHub. User
#should maintain a separate, local directory to copy those files into before pushing to GH

#change this
setwd("/Users/evanthompson/SDP/spread_data")
#setwd("C:/Users/v-evtho/Documents/Personal")


wd <- getwd()


#load packages
#library(nflscrapR)
#library(nnet)
library(dplyr)
library(caret)
library(lubridate)
#library(rpart)
#library(tictoc)
library(randomForest)

#set.seed(42069)



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
# #season2019 <- season_play_by_play(Season = 2019) # IGNORE 2019 UNTIL WE GET 2019 SPREADS
# 
# 
# # binding into a single dataset
# all_seasons_orig <- rbind(season2009, season2010, season2011, season2012,
#                      season2013,season2014, season2015, season2016,
#                      season2017, season2018 #, season2019
# )
# saveRDS(all_seasons_orig, file = paste(wd,"all_seasons_orig.rds",sep="/"))



#If you have all_seasons_orig.rds and historicalspreads_clean.csv, start from here
all_seasons_orig <- readRDS(paste(wd,"all_seasons_orig.rds",sep="/"))
all_seasons <- all_seasons_orig
all_seasons$HomeTeam <- ifelse(all_seasons$HomeTeam == "JAC", "JAX", all_seasons$HomeTeam)
all_seasons$HomeTeam <- ifelse(all_seasons$HomeTeam == "LA", "LAR", all_seasons$HomeTeam)
all_seasons$HomeTeam <- ifelse(all_seasons$HomeTeam == "AZ", "ARI", all_seasons$HomeTeam)
all_seasons$HomeTeam <- ifelse(all_seasons$HomeTeam == "SD", "LAC", all_seasons$HomeTeam)
all_seasons$HomeTeam <- ifelse(all_seasons$HomeTeam == "STL", "LAR", all_seasons$HomeTeam)
all_seasons$DefensiveTeam <- ifelse(all_seasons$DefensiveTeam == "JAC", "JAX", all_seasons$DefensiveTeam)
all_seasons$DefensiveTeam <- ifelse(all_seasons$DefensiveTeam == "LA", "LAR", all_seasons$DefensiveTeam)
all_seasons$DefensiveTeam <- ifelse(all_seasons$DefensiveTeam == "AZ", "ARI", all_seasons$DefensiveTeam)
all_seasons$DefensiveTeam <- ifelse(all_seasons$DefensiveTeam == "SD", "LAC", all_seasons$DefensiveTeam)
all_seasons$DefensiveTeam <- ifelse(all_seasons$DefensiveTeam == "STL", "LAR", all_seasons$DefensiveTeam)
all_seasons$posteam <- ifelse(all_seasons$posteam == "JAC", "JAX", all_seasons$posteam)
all_seasons$posteam <- ifelse(all_seasons$posteam == "LA", "LAR", all_seasons$posteam)
all_seasons$posteam <- ifelse(all_seasons$posteam == "AZ", "ARI", all_seasons$posteam)
all_seasons$posteam <- ifelse(all_seasons$posteam == "SD", "LAC", all_seasons$posteam)
all_seasons$posteam <- ifelse(all_seasons$posteam == "STL", "LAR", all_seasons$posteam)
all_seasons$DefensiveTeam <- ifelse(all_seasons$DefensiveTeam == "JAC", "JAX", all_seasons$DefensiveTeam)
all_seasons$DefensiveTeam <- ifelse(all_seasons$DefensiveTeam == "LA", "LAR", all_seasons$DefensiveTeam)
all_seasons$DefensiveTeam <- ifelse(all_seasons$DefensiveTeam == "AZ", "ARI", all_seasons$DefensiveTeam)
all_seasons$DefensiveTeam <- ifelse(all_seasons$DefensiveTeam == "SD", "LAC", all_seasons$DefensiveTeam)
all_seasons$DefensiveTeam <- ifelse(all_seasons$DefensiveTeam == "STL", "LAR", all_seasons$DefensiveTeam)
historicalspreads_clean <- read.csv(paste(wd,"historicalspreads_clean.csv",sep="/"))
historicalspreads_clean$Date <- as.POSIXlt(mdy(as.character(historicalspreads_clean$Date)))


#merge on historical spreads by HomeTeam, Date
all_seasons <- merge(all_seasons, historicalspreads_clean, by = c("Date", "HomeTeam"))




#cleaning
all_seasons <- all_seasons[ which(is.na(all_seasons$ExPointResult)),]
all_seasons <- all_seasons[ which(is.na(all_seasons$TwoPointConv)),]
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



#create data labels (home_team_margin_final)
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
all_seasons=all_seasons %>% mutate_if(is.character, as.factor)




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
# rm(season2019)


#partition
# inTrain <- createDataPartition(y=all_seasons$home_team_margin_final, p=0.7, list = FALSE)
# training <- all_seasons[inTrain,]
# testing <- all_seasons[-inTrain,]



# keep_vars <- c("home_team_margin_final", "down", "ydstogo", "yrdline100", "home_team_pos_ball", "home_team_pos_ball_neg",
#                "home_team_margin", "TimeSecs", "HomeTeam", "DefensiveTeam", "home_team_spread")
# training <- training[,keep_vars]
# testing <- testing[,keep_vars]



#further train/test subsetting
# training_small <- sample_n(training, 100000)
# testing_small <- sample_n(testing, 30000)



#subset. comment out when model finalized
all_seasons  <- sample_n(all_seasons, 20000)



#FINAL MODEL
modFit_final <- randomForest(home_team_margin_final ~ (down + ydstogo + yrdline100)*home_team_pos_ball_neg + 
                              home_team_margin + TimeSecs + HomeTeam + DefensiveTeam + home_team_spread, 
                            trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), 
                            ntree = 256,
                            data = all_seasons)
print(modFit_final)

postResample(predict(modFit_final, all_seasons), all_seasons$home_team_margin_final)

save(modFit_final, file = paste(wd,"modFit_final.rds",sep="/"))









# rm(training)
# rm(testing)
# rm(inTrain)
# rm(final_scores)
# rm(all_seasons)
# rm(all_seasons_orig)










#Exploratory plots, etc. Consider binning?
# g1 <- ggplot(subset(all_seasons_test, offense_year %in% c("PIT_2011")),
#                      aes(yrdline100, prediction ) ) +
#      geom_smooth(method = "lm") +
#      geom_point() +
#      facet_grid(home_team_pos_ball_neg ~ .)
# g1 
# 
# 
# 
# 
# g2 <- ggplot(subset(all_seasons_test, offense_year %in% c("PIT_2011")),
#   aes(x = ydstogo, y = prediction )) +
#   geom_smooth(method = "lm") +
#   geom_point() +
#   facet_grid(home_team_pos_ball_neg ~ .) +
#   geom_jitter(colour=alpha("black",0.15) )
# g2
# 
# 
# 
# g3 <- ggplot(subset(all_seasons_test, offense_year %in% c("PIT_2011")),
#              aes(x = down, y = prediction )) +
#   geom_smooth(method = "lm") +
#   geom_point() +
#   facet_grid(home_team_pos_ball_neg ~ .) +
#   geom_jitter(colour=alpha("black",0.15) )
# g3
# 
# 
# 
# 
# 
# g4 <- ggplot(all_seasons_test,
#              aes(x = home_team_spread, y = prediction )) +
#   geom_smooth(method = "lm") +
#   geom_point() +
#   #facet_grid(home_team_pos_ball_neg ~ .) +
#   geom_jitter(colour=alpha("black",0.15) )
# g4










# #for demo
# all_seasons_2 <- all_seasons
# keep_names <- c("down", "ydstogo", "yrdline100", "home_team_pos_ball_neg", "home_team_margin", 
#                               "TimeSecs", "HomeTeam", "DefensiveTeam", "home_team_spread")
# sample_input1 <- all_seasons_2[1,keep_names]
# sample_input2 <- all_seasons_2[1,keep_names]
# sample_input3 <- all_seasons_2[1,keep_names]
# sample_input4 <- all_seasons_2[1,keep_names]
# sample_input5 <- all_seasons_2[1,keep_names]
# 
# 
# #pregame spread prediction:
# sample_input1[1,] <- c(1, 10, 75, -1, 0, 3600, "GB", "SEA", 4) # prior to kickoff. Hawks lose toss, will receive
# predict(modFit_final, sample_input1)
# 
# 
# 
# #scenario 1. Huge play by Green Bay
# sample_input2[1,] <- c(1, 10, 90, 1, -7, 45, "GB", "SEA", 4) # coming out of halftime, GB down 7 with the ball at their own 10
# predict(modFit_final, sample_input2)
# 
# sample_input3[1,] <- c(1, 10, 21, 1, -7, 30, "GB", "SEA", 4) # Rodgers to Davante Adams for 69 yards!
# predict(modFit_final, sample_input3)
# 
# 
# 
# #scenario 2. Huge sack
# sample_input4[1,] <- c(3, 7, 25, 1, -7, 25, "GB", "SEA", 4) # GB ball down 7. 3rd and 7 at the SEA 12 yard line. 0:25 on the clock.
# predict(modFit_final, sample_input4)
# 
# sample_input5[1,] <- c(4, 17, 35, 1, -7, 15, "GB", "SEA", 4) # Aaron sacked for a loss of 10
# predict(modFit_final, sample_input5)

