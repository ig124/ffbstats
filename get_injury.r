# This script reads injury data from the PRF website and creates a data frame with this data for all players. It cross-references
# this dataset with the GSIS IDs in the NFLFastR tool.
# A limitation of this data set is that players who appear on multiple teams during the season are listed multiple times in the final data set. You have
# to manually search for and delete/merge these extra occurrences to match their actual season. There are also some small differences in player name spelling
# between PFR and NFLFastR.
# Another big limitation is that PFR designates players who did not play with hash marks. This script doesn't read hash marks. So, if a player is "Q" but they
# do not play, then this script still says they were active.

library(tidyverse)
library(readxl)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(openxlsx)
library(rvest)
library(nflfastR)
library('xml2')
options(scipen = 9999)

sched_loc <- "..<file location on computer>/sched_2020.rds" # https://github.com/guga31bb/nflfastR-data/tree/master/schedules
sched_2020 <- readRDS(sched_loc)
roster_2020 <- fast_scraper_roster(2020, pp=FALSE) %>%  #Roster data, this is a data.frame
  filter(!is.na(gsis_id)) # There are a few players without a gsis_id
roster_2020_reduced <- select(roster_2020, gsis_id, team, full_name) #Reduced roster data, only include ID, team, and player name in this matrix

teams_pfr <- c("buf", "nyg", "chi", "cle", "rai", "det", "htx", "kan", "min", "nor", "nyj", "oti", "crd", "gnb", "tam", "den", "rav", "car", "cin", "clt", "jax", "mia", "nwe", "phi", "sea", "ram", "sdg", "pit", "sfo", "atl", "dal", "was")
teams_nfl <- c("BUF", "NYG", "CHI", "CLE", "LV", "DET", "HOU", "KC", "MIN", "NO", "NYJ", "TEN", "ARI", "GB", "TB", "DEN", "BAL", "CAR", "CIN", "IND", "JAX","MIA", "NE", "PHI", "SEA", "LA", "LAC", "PIT", "SF", "ATL", "DAL", "WAS")

for (i in 1:length(teams_nfl)[1])
{
  inj_url <- paste0("https://www.pro-football-reference.com/teams/",teams_pfr[i],"/2020_injuries.htm")
  content <- read_html(inj_url)
  tables <- content %>% html_table(fill = TRUE)
  #assign(paste0("inj_data_",teams_nfl[i]),tables[[1]]) # Creates variable for inj_data for each team
  temp_inj <- tables[[1]]
  team_schedule <- filter(sched_2020,home_team==teams_nfl[i] | away_team==teams_nfl[i])
  #colnames(inj_data)[2:dim(inj_data)] <- team_schedule$week #Converts dates to weeks in column names of injury data
  colnames(temp_inj)[2:dim(temp_inj)[2]] <- team_schedule$week #Converts dates to weeks in column names of injury data
  # Reformat temp_inj
  temp_inj2 <- data.frame(matrix(, nrow = 100, ncol = 5, byrow=TRUE)) # re-initialize temp matrix
  colnames(temp_inj2) <- c("inj_des", "out", "week", "team", "player")
  k <- 1
  for (m in 1:dim(temp_inj)[1])
  {
    for (n in 1:(dim(temp_inj)[2]-1))
    {
      temp_inj2[k,5] <- temp_inj[m,1] # player name
      temp_inj2[k,3] <- colnames(temp_inj)[n+1] # week
      temp_inj2[k,1] <- temp_inj[m,(n+1)] # inj_desig
      temp_inj2[k,4] <- teams_nfl[i] # team
      if ((temp_inj2[k,1] == "IR") | (temp_inj2[k,1] == "O") | (temp_inj2[k,1] == "PUP") | (temp_inj2[k,1] == "/"))
      {
        temp_inj2[k,2] <- 1
      } else {
        temp_inj2[k,2] <- 0
      }
      k <- k + 1 # counter for overall row number in new matrix
    }
  }
  if (i == 1)
  {
    inj_data <- temp_inj2
  } else {
    #inj_data <- full_join(temp_inj2, inj_data, by = c("week")) 
    inj_data <- rbind(inj_data, temp_inj2)
  }
}

## Assign gsis_id to each player
inj_data_final <- left_join(inj_data, roster_2020_reduced, by = c('team', 'player' = 'full_name')) # this assumes that all fantasy-relevant players are included in the PFR system

### Export to Excel
OUT <- createWorkbook()
file="injuries_2020.xlsx"
addWorksheet(OUT, "injury_data")
writeData(OUT, sheet = "injury_data", x = inj_data_final)
saveWorkbook(OUT, file)

