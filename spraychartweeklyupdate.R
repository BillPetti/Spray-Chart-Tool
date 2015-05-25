##code for querying the baseballheatmaps pitch by pitch database for weekly updates to my sprach chart tool
##Bill Petti 4-25-2015

#load RMySQL and dplyr packagea for fetching and manipulating the data

require(methods)
require(RMySQL)
require(dplyr)

con<-dbConnect(RMySQL::MySQL(), dbname="x", username = "xx", password = "xxx", host = "xxxx", port = 3306)

res<-dbGetQuery(con, "SELECT a.game_id, g.home AS `Home`,g.away AS `Away`,g.wind_dir AS `Wind Direction`,g.wind AS `Wind MPH`,a.des AS `Description`,a.event AS `Outcome`,CONCAT(p.first,' ',p.last) AS `Name`,a.batter AS `MLBAM_ID_B`,stand,SUBSTR(DATE,1,4) AS yearid,a.hit_x,a.hit_y,ROUND((SQRT((((hit_x-125.42) * (hit_x-125.42)) + ((198.27-hit_y) * (198.27-hit_y))))*2.2),1) AS `distance`,ROUND((TAN((hit_x-125.42)/(198.27-hit_y))*180/PI()*.75),1) AS `angle`,((IF(EVENT='Single',1,0)))*.5 + ((IF(EVENT='Double',1,0)))*.79 + ((IF(EVENT='Triple',1,0 )))*1.07 + ((IF(EVENT='Home Run',1,0)))*1.41 + ((IF((EVENT ='Bunt Ground Out' OR EVENT ='Bunt Groundout' OR EVENT ='Bunt Lineout' OR EVENT ='Bunt Pop Out' OR EVENT ='Double Play' OR EVENT ='Field Error' OR EVENT ='Fielders Choice' OR EVENT ='Fielders Choice Out' OR EVENT ='Fly Out' OR EVENT ='Flyout' OR EVENT ='Force Out' OR EVENT ='Forceout' OR EVENT ='Ground Out' OR EVENT ='Grounded Into DP' OR EVENT ='Groundout' OR EVENT ='Line Out' OR EVENT ='Lineout' OR EVENT ='Pop Out' OR EVENT ='Sac Bunt' OR EVENT ='Sac Fly' OR EVENT ='Sac Fly DP' OR EVENT ='Sacrifice Bunt DP' OR EVENT ='Triple Play') AND hit_type IS NOT NULL,1,0)))*-.28 AS RunValue, a.hit_x AS `hit_x_1`, ROUND((a.hit_y*-1),2) AS `hit_y_rotated`, CONCAT(l.first,' ',l.last) AS `Pitcher Name`, a.pitcher AS `MLBAM_ID_P`, g.date AS `Date`, b.start_speed AS `Velocity`, b.px AS `Horizontal Location`, b.pz AS `Vertical Location`, b.ball AS `Balls`, b.strike AS `Strikes`, b.pitch_type AS `Pitch Type`, l.throws AS `Throws`, b.sv_id AS pitchID FROM atbats a JOIN players p ON a.batter = p.eliasid JOIN players l ON a.pitcher = l.eliasid JOIN games g ON a.game_id = g.game_id  JOIN pitches b ON a.ab_id = b.ab_id WHERE (hit_x IS NOT NULL OR hit_y IS NOT NULL)  AND (SQRT((((hit_x-125.42) * (hit_x-125.42)) + ((198.27-hit_y) * (198.27-hit_y))))*2.2) <1500  AND hit_y > -198.27 AND g.date BETWEEN (CURRENT_DATE() - INTERVAL 7 DAY) AND (CURRENT_DATE() - INTERVAL 1 DAY) AND b.type = 'X' ORDER BY hit_x ASC, distance DESC")

#eliminate batted balls with bad plotting coordinates

res<-filter(res, distance<=513)

#reformat dates, balls, and strikes

res$Date<-strftime(res$Date,"%m/%d/%Y")
res$yearid<-as.numeric(res$yearid)
res$Balls<-as.numeric(res$Balls)
res$Strikes<-as.numeric(res$Strikes)

#recode batted ball types based on play description

res$Batted_Ball_Type<-ifelse(grepl("line", res$Description), "LD", ifelse(grepl("fly", res$Description), "FB", ifelse(grepl("flies", res$Description), "FB", ifelse(grepl("ground", res$Description), "GB", "PU"))))

#recode the zone the ball was hit based on angle

res$Zone<-ifelse(res$angle<=-55,1,ifelse(res$angle<=-45,2,ifelse(res$angle<=-35,3,ifelse(res$angle<=-25,4,ifelse(res$angle<=-15,5,ifelse(res$angle<=-5,6,ifelse(res$angle<=5,7,ifelse(res$angle<=15,8,ifelse(res$angle<=25,9,ifelse(res$angle<=35,10,ifelse(res$angle<=45,11,ifelse(res$angle<=55,12,13))))))))))))

#load team_parks table

teams_parks_table <- read.csv("/teams_parks_table.csv")

#join team parks table and res to merge in League and Division information

res<-left_join(res, teams_parks_table, by = "Home")

#recode the batted ball outcome based on run values

res$Outcome_Type<-ifelse(res$RunValue == 1.41, "HR", ifelse(res$RunValue == 1.07, "3B", ifelse(res$RunValue == 0.79, "2B", ifelse(res$RunValue == 0.50, "1B", "Out"))))

#move pitchID column to end of res dataframe so it matches the existing data file in Tableau

#function for moving pitchID to the last column

movetolast <- function(data, move) {
	data[c(setdiff(names(data), move), move)]
	}
	
res<-movetolast(res, c("pitchID"))

#disconnect from BBHM database and connect to FanGraphs

dbDisconnect(con)

con<-dbConnect(RMySQL::MySQL(), dbname="x", username = "xx", password = "xxx", host = "xxxx", port = 3306)

#query home and away teams

teams<-dbGetQuery(con, "SELECT sv_id as pitchID, b.ShortName as 'Batting Team', p.ShortName as 'Pitching Team' FROM gd_pitch g JOIN season_team p on p.TeamID = (case when InningHalf = 1 then `AwayTeamId` else `HomeTeamId` end) JOIN season_team b on b.TeamID = (case when InningHalf = 0 then `AwayTeamId` else `HomeTeamId` end) WHERE type='X' AND g.gamedate BETWEEN (CURRENT_DATE() - INTERVAL 7 DAY) AND (CURRENT_DATE() - INTERVAL 1 DAY)")


#join the home and away teams to the larger batted ball data set

res<-left_join(res, teams, by = "pitchID")

colnames(res)<-c("game_id", "Home", "Away", "Wind Direction", "Wind MPH", "Description", "Outcome", "Name", "MLBAM_ID_B", "stand", "yearid", "hit_x", "hit_y", "distance", "angle", "RunValue", "hit_x_1", "hit_y_rotated", "Pitcher Name", "MLBAM_ID_P", "Date", "Velocity", "Horizontal Location", "Vertical Location", "Balls", "Strikes", "Pitch Type", "Throws", "Batted_Ball_Type", "Zone", "League", "Division", "Outcome_Type", "PitchID", "Batting Team", "Pitching Team")

#load the existing data set

test <- read.csv("master.spray.data.csv")

write.csv(test, file="master.spray.data.prev.csv", row.names=FALSE, col.names=TRUE, na="")

#reformat date column
#test$Date<-strftime(test$Date,"%m/%d/%Y")

#rename columns to match query res data

colnames(test)<-c("game_id", "Home", "Away", "Wind Direction", "Wind MPH", "Description", "Outcome", "Name", "MLBAM_ID_B", "stand", "yearid", "hit_x", "hit_y", "distance", "angle", "RunValue", "hit_x_1", "hit_y_rotated", "Pitcher Name", "MLBAM_ID_P", "Date", "Velocity", "Horizontal Location", "Vertical Location", "Balls", "Strikes", "Pitch Type", "Throws", "Batted_Ball_Type", "Zone", "League", "Division", "Outcome_Type", "PitchID", "Batting Team", "Pitching Team")

#bind the new rows to the existing data set

res2<-rbind(test, res)

#rename columns to match existing Tableau data

colnames(res2)<-c("game_id", "Home", "Away", "Wind Direction", "Wind MPH", "Description", "Outcome", "Name", "MLBAM_ID_B", "stand", "yearid", "hit_x", "hit_y", "Distance (ft.)", "Angle", "RunValue", "hit_x_1", "hit_y_rotated", "Pitcher Name", "MLBAM_ID_P", "Date", "Velocity", "Horizontal Location (ft.)", "Vertical Location (ft.)", "Balls", "Strikes", "Pitch Type", "Throws", "Batted_Ball_Type", "Zone", "League", "Division", "Outcome Type", "PitchID", "Batting Team", "Pitching Team")

#export the updated data set to folder for uploading into Tableau - export as csv

write.csv(res2, file="master.spray.data.csv", row.names=FALSE, col.names=TRUE, na="")