#install necessary packages

#below is code to install the ncaahoopR package, if needed
# devtools::install_github("lbenz730/ncaahoopR")

library(ncaahoopR)
library(data.table)
library(purrr)
library(tictoc)
library(iterators)
library(parallelly)
library(doParallel)
library(tidyr)
library(kableExtra)
library(scales)
library(fst)
library(ltm)
`%!in%` <- Negate(`%in%`)

memory.limit(20000)

#first we will load in all play by play data from the last 5 seasons
#the ncaahoopR package has a function called get_pbp() which loads in play by play data,
#but it does not always work for every team

#instead, I downloaded the individual game csv files directly from the ncaahoopR data repository
#and will load in the data for each game
path <- "C:/Users/ljdub/Downloads/ncaahoopR_data-master/ncaahoopR_data-master"
seasons <- list.files(path = path, 
                      pattern = "")

#the following code loads in each team's schedule for each season from 2017-18 to 2021-22
#it creates one large data.table of every game played in college basketball during that time
df <- data.table()

seasons <- seasons[16:20] #only get the last five seasons

for (season in seasons) {
  
  setwd(paste(path, season, 'schedules', sep = '/'))
  testData <- list.files(path = paste(path, season, 'schedules', sep = '/'), 
                         pattern = ".csv") %>% map_df(~fread(.))
  
  testData$season <- as.character(season)
  
  #create a column in the data.table that contains the file path for the game
  #we will use this column to load in the play by play data for each game one by one
  testData <- testData %>% mutate(pbpFile = paste(paste(path, season, 'pbp_logs', date, game_id, sep = '/'), '.csv', sep = ''))
  
  df <- rbind(df, testData, fill = TRUE)
  
}

#now, for each entry in the pbpFile column, we will load in the corresponding play by play data for the game
no_cores <- availableCores() - 1
registerDoParallel(no_cores) #set up parallel process to make this run much faster

out <- foreach(i = df$pbpFile, 
               .packages='data.table',
               .errorhandling = 'remove',
               .inorder = FALSE) %dopar% {
          
          fread(i)
          
        }
  
out <- rbindlist(out, fill = TRUE) #combine all the games into one data.table
out <- unique(out) #filter out duplicates so we don't count some games twice

#code to write this data.table to an fst file to save me time later

# setwd("C:/Users/ljdub/OneDrive/Documents/Sports Research/March Madness")
# write.fst(out, "All_PbP_Stats.fst", compress = 100)

#since we're only looking at analyzing free throws, we can filter the large play by play data.table
#to only the plays that involve free throws
freethrows <- out %>% filter(free_throw == TRUE,
                             !is.na(shooter), 
                             !is.na(shot_team))

#we will now create a column in the freethrows data.table for the season in which the free throw occurred
#this will help later when we try to separate free throw statistics into individual player-seasons
freethrows$date <- as.POSIXct( freethrows$date, format="%Y-%m-%d" )

freethrows <- freethrows %>% mutate(season = case_when(
  freethrows$date > "2017-11-01" & freethrows$date < "2018-06-01" ~ "2017-18",
  freethrows$date > "2018-11-01" & freethrows$date < "2019-06-01" ~ "2018-19",
  freethrows$date > "2019-11-01" & freethrows$date < "2020-06-01" ~ "2019-20",
  freethrows$date > "2020-11-01" & freethrows$date < "2021-06-01" ~ "2020-21", 
  freethrows$date > "2021-11-01" & freethrows$date < "2022-06-01" ~ "2021-22"
))

#do some cleaning on player names so we can join with additonal data.tables later
freethrows$shooter <- tolower(freethrows$shooter)
freethrows$shooter <- gsub('(sr|jr|,|\\.|")', "", freethrows$shooter, ignore.case = TRUE)
freethrows$shooter <- gsub(" i*$v*", "", freethrows$shooter, ignore.case = TRUE)
freethrows$shooter <- gsub("^ ", "", freethrows$shooter, ignore.case = TRUE)
freethrows$shooter <- gsub("'", "", freethrows$shooter, ignore.case = TRUE)

#unfortunately there are several team names that are not consistent between the different datasets
#we need to join
#we will change the names of these teams so joining with other datasets is possible
freethrows$shot_team[freethrows$shot_team == 'Arkansas State'] <- 'Arkansas St'
freethrows$shot_team[freethrows$shot_team == 'CSU Fullerton'] <- 'CS Fullerton'
freethrows$shot_team[freethrows$shot_team == 'California Baptist'] <- 'Cal Baptist'
freethrows$shot_team[freethrows$shot_team == 'Georgia State'] <- 'Georgia St'
freethrows$shot_team[freethrows$shot_team == 'Little Rock'] <- 'Arkansas-Little Rock'
freethrows$shot_team[freethrows$shot_team == 'UMass'] <- 'Massachusetts'
freethrows$shot_team[freethrows$shot_team == 'McNeese'] <- 'Mcneese St'
freethrows$shot_team[grep(" St. Mary's", freethrows$shot_team)] <- "Mt. St. Mary'S"
freethrows$shot_team[freethrows$shot_team == 'Presbyterian'] <- 'Presbyterian College'
freethrows$shot_team[grep('San Jos', freethrows$shot_team)] <- 'San José State'
freethrows$shot_team[freethrows$shot_team == 'UConn'] <- 'Connecticut'
freethrows$shot_team[freethrows$shot_team == 'UL Monroe'] <- 'Ul Monroe'
freethrows$shot_team[freethrows$shot_team == 'UTSA'] <- 'UT San Antonio'
freethrows$shot_team[freethrows$shot_team == 'Youngstown St'] <- 'Youngstown State'
freethrows$shot_team[freethrows$shot_team == 'LIU Brooklyn'] <- 'Long Island University'
freethrows$shot_team[freethrows$shot_team == 'SE Missouri St'] <- 'Southeast Missouri State'
freethrows$shot_team[freethrows$shot_team == 'Seattle'] <- 'Seattle U'
freethrows$shot_team[freethrows$shot_team == 'SIU-Edwardsville'] <- 'SIU Edwardsville'
freethrows$shot_team[freethrows$shot_team == 'UMKC' | freethrows$shot_team == 'Kansas City'] <- 'UM Kansas City'
freethrows$shot_team[freethrows$shot_team == 'Sam Houston'] <- 'Sam Houston State'
freethrows$shot_team[freethrows$shot_team == "Norfolk St"] <- "Norfolk State"
freethrows$shot_team[freethrows$shot_team == 'American University'] <- 'American'
freethrows$shot_team[freethrows$shot_team == 'Morgan St'] <- 'Morgan State'
freethrows$shot_team[freethrows$shot_team == 'Boston Univ.'] <- 'Boston University'

#we will start making a data.table with every player season in our sample
#the first step is filtering the freethrows data.table by player, season, and team
#team is included in case there are multiple players with the same name in the same season
#on different teams
players <- freethrows %>% group_by(shooter, season, shot_team) %>%
  summarise(FTs = n(),
            FTM = sum(shot_outcome == 'made')) %>%
  arrange(desc(FTs))

#we will now load in all teams' rosters for the last 5 years
#roster data contains information on the player's position and class
rosters <- data.table()

data(ids)#this dataset contains every team name included in the ncaahoopR package

#for each team and season, use the get_roster function (from ncaahoopR) to load in roster data
for (team in ids$team){

  teamRoster <- foreach(i = seasons,
                 .packages=c('ncaahoopR', 'dplyr'),
                 .errorhandling = 'remove',
                 .inorder = FALSE) %dopar% {

                   get_roster(team, i) %>% mutate(season = i)

                 }

  teamRoster <- rbindlist(teamRoster, fill = TRUE)

  #sometimes no roster is available
  #so we only add the data when a roster is found
  if (nrow(teamRoster) > 0){

    teamRoster <- teamRoster %>% mutate(team = team) %>%
      dplyr::select(name, position, class, season, team)
    rosters <- rbind(rosters, teamRoster, fill = TRUE)

  }

}

#remove any player seasons that are missing data
players <- na.omit(players)

#remove any players on a roster that don't have a valid class or position
rosters <- rosters %>% filter(class %in% c('FR', 'SO', 'JR', 'SR'),
                  !is.na(position)) %>%
  mutate(name = tolower(name))

#do the same cleaning up of player names so we can join the roster data with the player data
rosters$name <- gsub('(sr|jr|,|\\.|")', "", rosters$name, ignore.case = TRUE)
rosters$name <- gsub(" i*$v*", "", rosters$name, ignore.case = TRUE)
rosters$name <- gsub("^ ", "", rosters$name, ignore.case = TRUE)
rosters$name <- gsub("'", "", rosters$name, ignore.case = TRUE)

#the dict dataset (ncaahoopR) contains team names for various sources (ESPN, NCAA, etc.)
#however, we need to adjust some of the names that are not compatible with the dataset we currently have
data(dict)
dict$ESPN[dict$ESPN == 'Dixie State'] <- 'Utah Tech'
dict$ESPN_PBP[dict$ESPN == 'Savannah St'] <- 'Savannah St'
dict$Trank[dict$ESPN == "Houston Baptist"] <- "Houston Christian"
dict$ESPN_PBP[dict$ESPN == 'Louisiana'] <- 'Louisiana'

#now join player data with dict (will make it possible to join roster data)
players <- players %>% left_join(dict, by = c('shot_team' = 'ESPN_PBP'))
#remove any players that don't have a valid team name
#this removes any players for non-D1 schools
players <- players %>% filter(!is.na(ESPN))

#join roster data and only select columns we need for analysis
playersCombo <- left_join(players, rosters, by = c('shooter' = 'name',
                                                   'season' = 'season',
                                                   'ESPN' = 'team')) %>%
  dplyr::select(shooter, season, FTs, FTM, position, class, Trank, ESPN, shot_team) %>%
  mutate(FTpct = FTM/FTs) #calculates FT% from the FT attempts and FTs made data

#we need to get data on each team's conference
#this data comes from barttorvik.com, which has team stat leaderboards for each season, 
#which includes conference information

#I downloaded csv files directly from barttorvik.com so I can load the data into R
#information for how to download these files can be found here: http://adamcwisports.blogspot.com/p/data.html
trankFiles <- sprintf('%s_team_results', 2018:2022)

teamStats <- foreach(i = trankFiles,
                      .packages=c('data.table'),
                      .errorhandling = 'remove',
                      .combine = 'rbind',
                      .inorder = FALSE) %dopar% {

                        #we will add a variable for the season in case a team switched conferences at some point in our sample
                        fread(paste("C:/Users/ljdub/Downloads/TRank Data/", i, ".csv", sep = ""))[,2:3] %>%
                          mutate(season = case_when(
                            substr(i, 1,4) == '2018' ~ '2017-18',
                            substr(i, 1,4) == '2019' ~ '2018-19',
                            substr(i, 1,4) == '2020' ~ '2019-20',
                            substr(i, 1,4) == '2021' ~ '2020-21',
                            substr(i, 1,4) == '2022' ~ '2021-22'))

                      }

#join the team data with our player-seasons data
playersCombo <- playersCombo %>% left_join(teamStats, by=c('season' = 'season',
                                                           'Trank' = 'team'))


#there are several players that do not have data on position and class
#they were players on teams whose rosters were not available in the get_roster function
#barttorvik.com has information for some of these players, which we will load in now

#again, I downloaded the csv files directly from barttorvik.com so I could load them in
trankPlayerFiles <- sprintf('%s_player_stats', 2018:2022)

playerStats <- foreach(i = trankPlayerFiles,
                     .packages=c('data.table'),
                     .errorhandling = 'remove',
                     .combine = 'rbind',
                     .inorder = FALSE) %dopar% {

                       #again, add a column for season
                       fread(paste("C:/Users/ljdub/Downloads/TRank Data/", i, ".csv", sep = ""),
                             drop = c(4:25,27:64,66), #we don't need most of the columns, which include player stats for the given year
                             header = FALSE) %>%
                         mutate(season = case_when(
                           substr(i, 1,4) == '2018' ~ '2017-18',
                           substr(i, 1,4) == '2019' ~ '2018-19',
                           substr(i, 1,4) == '2020' ~ '2019-20',
                           substr(i, 1,4) == '2021' ~ '2020-21',
                           substr(i, 1,4) == '2022' ~ '2021-22'))

                     }

#change column names to make joining possible
colnames(playerStats) <- c('name', 'team', 'conf', 'class', 'position', 'season')

#clean up player names as with previous datasets so we can join player stats data
playerStats$name <- tolower(playerStats$name)
playerStats$name <- gsub("(jr|,|\\.)", "", playerStats$name, ignore.case = TRUE)
playerStats$name <- gsub(" i*v*$", "", playerStats$name, ignore.case = TRUE)
playerStats$name <- gsub("^ ", "", playerStats$name, ignore.case = TRUE)
playerStats$name <- gsub("'", "", playerStats$name, ignore.case = TRUE)

#many players have different spellings of their names
#for example, one player might be named Cameron in our player-seasons data and Cam in our player stats data
#to fix this issue (so we can properly join the data together) we will create a distinct player code
#which is made up of the first two letters of the first name and the last name
playerStats <- playerStats %>%
  separate(name, c('Fname', 'Lname'), remove = FALSE) %>%
  mutate(playerCode = paste(substr(Fname, 1, 2), Lname, sep = ""))

#there are some players that have duplicate player codes
#this occurs most often when brothers are on a team together and have very similar first names
#ex. Jaron and Jared Cumberland

#we will add a "1" or a "2" at the end of their player codes to make them distinct
duplicates <- playerStats %>% 
  group_by(playerCode, season, team) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) %>%
  left_join(playerStats)

#remove duplicates from the player stats data.table (these will be added back once we have fixed the issue)
playerStats <- playerStats[-c(which(duplicated(playerStats[,c(4,8,9)])), which(duplicated(playerStats[,c(4,8,9)], fromLast = TRUE))),]

#get the player codes that need changing
changes <- duplicates$playerCode

#add a 1 or 2 at the end of the player code
duplicates$playerCode[seq(1, nrow(duplicates),2)] <- paste(duplicates$playerCode[seq(1, nrow(duplicates),2)], "1", sep = "")
duplicates$playerCode[seq(2, nrow(duplicates),2)] <- paste(duplicates$playerCode[seq(2, nrow(duplicates),2)], "2", sep = "")

#get the new player codes
changed <- duplicates$playerCode

#we need to do the same thing for the playersCombo dataset, which is our player-season data
#that way we can join playerStats with playersCombo by playerCode, which will ensure
#proper matching of player stats and player-seasons
playersCombo <- playersCombo %>% filter(!is.na(conf)) %>% 
  separate(shooter, c('Fname', 'Lname'), remove = FALSE) %>%
  mutate(playerCode = paste(substr(Fname, 1, 2), Lname, sep = ""))

#there are more duplicate playerCodes in playersCombo
#not all of these duplicates are the same as the ones from before, but some are
duplicates2 <- playersCombo %>% 
  group_by(playerCode, season, Trank, shot_team) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) %>%
  left_join(playersCombo)

#first step will be to fix the duplicated player codes we already saw in playerStats
#we will remove the player-season we are about to fix
playersCombo <- playersCombo[-c(which(duplicated(playersCombo[,c(4,9,14)])), which(duplicated(playersCombo[,c(4,9,14)], fromLast = TRUE))),]

#here, we replace the duplciated player codes with the ones that had a 1 or 2 added to the end
duplicates2$playerCode[which(duplicates2$playerCode %in% changes)] <- changed[-c(11:14, 17:18, 23:24, 29:30, 39:40)]

#to add the fixed player codes back to playersCombo, we need to make sure the columns are the same
#otherwise rbind won't work
duplicates2 <- duplicates2 %>% dplyr::select(colnames(playersCombo))

#this code joins the our player codes with the freethrows dataset
#it will make sure we add the player code column to the freethrows data.table
freethrows <- freethrows %>% left_join(rbind(playersCombo, duplicates2)[,c(1, 4, 11, 14)], by = c('shooter', 'season', 'shot_team'))

#at this point we have fixed some of the duplicated player codes in our player-seasons dataset
#there are still several problems
#for some select cases, players went by two different names in the freethrows dataset
#and so their stats for a season were separated
#but they have the same playercode, so we can fix their name and combine their season stats
#ex. someone named "Charles" and "Chuck"
duplicates2 <- duplicates2 %>% mutate(FnameUpdated = substr(Fname, 1, 3))
duplicates2$FnameUpdated <- ifelse(duplicates2$FnameUpdated == 'mik' & duplicates2$Lname != 'thomas', 'mic', duplicates2$FnameUpdated)
duplicates2$FnameUpdated <- ifelse(duplicates2$FnameUpdated == 'mic' & duplicates2$Lname == 'thomas', 'mik', duplicates2$FnameUpdated)
duplicates2$FnameUpdated <- ifelse(duplicates2$FnameUpdated == 'cha' & duplicates2$Lname == 'obannon', 'chu', duplicates2$FnameUpdated)

#other players have two separate records because they are listed as having two different classes
#ex. Barnie Andre listed as a FR and a JR
#we need to eliminate one of these records to correct the issue 
#(I looked up the correct class for these players on their team websites)
duplicates2 <- duplicates2 %>% filter(!(shooter == 'bernie andre' & class == 'FR'),
                               !(shooter == 'michael johnson' & class == 'SR')) %>%
  group_by(playerCode, season, Trank, shot_team, FnameUpdated, Lname, ESPN, conf) %>%
  summarise(fname = Fname[!is.na(position)],
            shootr = shooter[!is.na(position)],
            pos = position[!is.na(position)],
            cla = class[!is.na(class)],
            Fts = sum(FTs),
            Ftm = sum(FTM),
            Ftpct = Ftm/Fts) #finally, we combine all the players whose stats were separated and shouldn't have been 

#eliminate duplicated rows
duplicates2 <- unique(duplicates2)

#finally, add the fixed duplicates back into the playerStats data.table
duplicates <- duplicates %>% dplyr::select(colnames(playerStats))
playerStats <- rbind(playerStats, duplicates)

#and add the fixed duplicates back into the playersCombo data.table
duplicates2 <- duplicates2 %>% ungroup() %>% dplyr::select(shootr, fname, Lname, season, Fts, Ftm, pos, cla, Trank, ESPN, shot_team, Ftpct, conf, playerCode)
colnames(duplicates2) <- colnames(playersCombo)
playersCombo <- rbind(playersCombo, duplicates2)

#with the player codes all fixed, we will join the player stats data with the player-seasons data
#so that we can get accurate position and class information for some of the players who were missing these things
playersCombo <- playersCombo %>% left_join(playerStats, by = c('playerCode' = 'playerCode',
                                               'season' = 'season',
                                               'Trank' = 'team'))

#this code makes sure there is one column in the dataset with a player's class
#and one column with a player's position
playersCombo$position.x[is.na(playersCombo$position.x)] <- playersCombo$position.y[is.na(playersCombo$position.x)]
playersCombo$class.x[is.na(playersCombo$class.x)] <- playersCombo$class.y[is.na(playersCombo$class.x)]

#if we still can't find the player's position and class, we will remove them from the sample
playersCombo <- playersCombo %>% filter(!is.na(position.x), !is.na(class.x))

#some of the barttorvik positions were different than the positions in the ncaahoopR data
#we will adjust the position names to be consistent
#I decided which barttorvik names corresponded to which ncaahoopR names (one could disagree with my decisions)
playersCombo$position.x[playersCombo$position.y == 'Wing F'| 
                          playersCombo$position.y == 'Stretch 4' | 
                          playersCombo$position.y == 'PF'] <- 'F'
playersCombo$position.x[grepl('G', playersCombo$position.y)] <- 'G'
playersCombo$position.x[playersCombo$position.y == 'PF/C'] <- 'F/C'
playersCombo$position.x[playersCombo$position.x == 'G/F'] <- 'G' #there were so few G/Fs that I combined them with guards to make analysis meaningful
                                                                  #again, one could disagree with this decision

playersCombo$class.x <- tolower(playersCombo$class.x)

#remove any players that don't have a position, and clean up the data.table to only include columns we need
playersCombo <- playersCombo %>% filter(position.x != '') %>%
  dplyr::select(shooter, season, ESPN, conf.x, class.x, position.x, FTs, FTM, FTpct, Trank, shot_team, playerCode) %>%
  rename(team = ESPN,
         position = position.x,
         class = class.x,
         conf = conf.x)

#join the player-seasons data with the freethrows data.table
#the reason for this is some of the names of players in the freethrows data.table were inaccurate
#ex. Brandon Kuminga and Brandonn Kuminga both existed, but are the same player
#when we corrected the duplicate player codes we fixed this issue
#we will now add the corrected player names to the free throws data
freethrows <- freethrows %>% left_join(playersCombo[,c(1,2,11,12)], by = c('season', 'shot_team', 'playerCode')) %>%
  rename(shooter = shooter.x,
         adjShooter = shooter.y)

# write.fst(freethrows, "All_FTs.fst", compress = 100)

#the remaining code prints out tables based on the player-season data
#these tables give some interesting info about the average FT% based on position, class, etc.
playersCombo$class <- factor(playersCombo$class, levels = c('fr', 'so', 'jr', 'sr'))

#here's a glimpse at the entire playersCombo data.table
head(playersCombo)

#how does FT% differ by season?
playersCombo %>% group_by(season) %>%
  summarise(`Free Throws Made` = format(sum(FTM), big.mark = ',', scientific = FALSE),
            `Free Throws Attempted` = format(sum(FTs), big.mark = ',', scientific = FALSE),
            `Free Throw Percentage` = scales::percent(sum(FTM)/sum(FTs), accuracy = 0.1)) %>%
  rename(Season = season) %>%
  kable(booktabs = T, align = 'c') %>%
  kable_styling(font_size = 14)

#by team?
head(playersCombo %>% group_by(team) %>%
  summarise(`Free Throws Made` = format(sum(FTM), big.mark = ',', scientific = FALSE),
            `Free Throws Attempted` = format(sum(FTs), big.mark = ',', scientific = FALSE),
            `Free Throw Percentage` = scales::percent(sum(FTM)/sum(FTs), accuracy = 0.1)) %>%
  arrange(desc(`Free Throw Percentage`)), 10) %>%
  rename(Team = team) %>%
  kable(booktabs = T, align = 'c') %>%
  kable_styling(font_size = 14)

#who are the worst teams at FT% in the sample?
tail(playersCombo %>% group_by(team) %>%
       summarise(`Free Throws Made` = format(sum(FTM), big.mark = ',', scientific = FALSE),
                 `Free Throws Attempted` = format(sum(FTs), big.mark = ',', scientific = FALSE),
                 `Free Throw Percentage` = scales::percent(sum(FTM)/sum(FTs), accuracy = 0.1)) %>%
       arrange(desc(`Free Throw Percentage`)), 10) %>%
  rename(Team = team) %>%
  kable(booktabs = T, align = 'c') %>%
  kable_styling(font_size = 14)

#by conference?
playersCombo %>% group_by(conf) %>%
  summarise(`Free Throws Made` = format(sum(FTM), big.mark = ',', scientific = FALSE),
            `Free Throws Attempted` = format(sum(FTs), big.mark = ',', scientific = FALSE),
            `Free Throw Percentage` = scales::percent(sum(FTM)/sum(FTs), accuracy = 0.1)) %>%
  arrange(desc(`Free Throw Percentage`)) %>%
  rename(Conference = conf) %>%
  kable(booktabs = T, align = 'c') %>%
  kable_styling(font_size = 14)

#add a column for whether the conference is a "power conference" or not
playersCombo$power <- ifelse(playersCombo$conf %in% c('ACC', 'P12', 'SEC', 'B10', 'B12', 'BE'), 'Power', 'Non-Power')

# write.fst(playersCombo, "PlayerSeasons.fst", compress = 100)

#Are power conferences better or worse at FT%?
playersCombo %>% group_by(power) %>%
  summarise(`Free Throws Made` = format(sum(FTM), big.mark = ',', scientific = FALSE),
            `Free Throws Attempted` = format(sum(FTs), big.mark = ',', scientific = FALSE),
            `Free Throw Percentage` = scales::percent(sum(FTM)/sum(FTs), accuracy = 0.1)) %>%
  arrange(desc(`Free Throw Percentage`)) %>%
  rename(Conference = power) %>%
  kable(booktabs = T, align = 'c') %>%
  kable_styling(font_size = 14)

#by class?
playersCombo %>% group_by(class) %>%
  summarise(`Free Throws Made` = format(sum(FTM), big.mark = ',', scientific = FALSE),
            `Free Throws Attempted` = format(sum(FTs), big.mark = ',', scientific = FALSE),
            `Free Throw Percentage` = scales::percent(sum(FTM)/sum(FTs), accuracy = 0.1)) %>%
  rename(Class = class) %>%
  arrange(Class) %>%
  kable(booktabs = T, align = 'c') %>%
  kable_styling(font_size = 14)

#by position?
playersCombo %>% group_by(position) %>%
  summarise(`Free Throws Made` = format(sum(FTM), big.mark = ',', scientific = FALSE),
            `Free Throws Attempted` = format(sum(FTs), big.mark = ',', scientific = FALSE),
            `Free Throw Percentage` = scales::percent(sum(FTM)/sum(FTs), accuracy = 0.1)) %>%
  rename(Position = position) %>%
  arrange(desc(`Free Throw Percentage`)) %>%
  kable(booktabs = T, align = 'c') %>%
  kable_styling(font_size = 14)

#how about by power, class, and position?
playersCombo %>% group_by(power, class, position) %>%
  summarise(`Free Throws Made` = format(sum(FTM), big.mark = ',', scientific = FALSE),
            `Free Throws Attempted` = format(sum(FTs), big.mark = ',', scientific = FALSE),
            `Free Throw Percentage` = scales::percent(sum(FTM)/sum(FTs), accuracy = 0.1)) %>%
  rename(Position = position,
         Class = class,
         Conference = power) %>%
  arrange(desc(`Free Throw Percentage`)) %>%
  kable(booktabs = T, align = 'c') %>%
  kable_styling(font_size = 14)
