library(rvest)
library(dplyr)
library(data.table)
library(vtable)

#init data frame that will contains observations
overall <- data.frame()

#generate list of seasons since expansions to 12, minus lockout year
years <- seq(1968, 2021, by=1)
years <- years[which(years!=2005)]

#iterate over all seasons and appends players who receive Hart votes from each to 'overall'
for(y in years){

  #get list of skaters who played at least one game in year y
  skaters <- read_html(paste0("https://www.hockey-reference.com/leagues/NHL_", y, "_skaters.html"))%>%
    html_table()

  #get unique ID for each player based on their hockey-reference URL
  skaters_links <- read_html(paste0("https://www.hockey-reference.com/leagues/NHL_", y, "_skaters.html"))%>%
    html_nodes(xpath="//td/a") %>%
    html_attr("href")
  df <- data.frame(skaters_links) #convert to data frame
  df <- data.frame(df[grepl("players",df$skaters_links) ,]) #keep links only associated with players, not their teams
  df <- data.frame(gsub(".*\\/([^.]*).*", "\\1", df[,1])) #keep only the ID part of links
  names(df) <- "ID" #rename column appropriately

  #keep only columns of interest
  skaters <- data.frame(skaters[[1]]) #convert to data frame
  skaters <- skaters[-1,c(2,4,5, 6,12)] #remove unnecessary columns
  skaters <- skaters[-which(skaters$Var.2=="Player"),] #remove header columns
  names(skaters)<- c("Player", "Team", "Pos", "GP", "PS") #rename columns appropriately
  skaters$ID <- df #add ID columns to list of players

  #similar process begins for goalies

  #get list of all goalies who played at least one game in season y
  goalies <- read_html(paste0("https://www.hockey-reference.com/leagues/NHL_", y, "_goalies.html"))%>%
    html_table()

  #get unique ID for each goalie based on their hockey-reference URL
  goalies_links <- read_html(paste0("https://www.hockey-reference.com/leagues/NHL_", y, "_goalies.html"))%>%
    html_nodes(xpath="//td/a") %>%
    html_attr("href")
  df <- data.frame(goalies_links) #convert to data frame
  df <- data.frame(df[grepl("players",df$goalies_links) ,])#keep links only associated with players, not their teams
  df <- data.frame(gsub(".*\\/([^.]*).*", "\\1", df[,1])) #keep only the ID part of links
  names(df) <- "ID" #rename column appropriately

  #keep only columns of interest
  goalies <- data.frame(goalies[[1]]) #convert to data frame
  goalies <- goalies[-1,c(2,4,5,16)] #remove unnecessary columns
  goalies <- goalies[-which(goalies$Var.2=="Player"),] #remove header columns
  names(goalies)<- c("Player", "Team", "GP", "PS") #rename columns appropriately
  goalies$Pos <- "G" #add position attribute to facilitate comparison with skaters
  goalies <- goalies[,c(1,2,5,3,4)] #reorder columns to match the skaters' df
  goalies$ID <- df #add ID columns to list of players

  #combine the data frames of skaters and goalies into one, players
  players <- rbind(skaters, goalies)

  #ensure that point shares and games played are treated as numbers
  players$PS <- as.numeric(players$PS)
  players$GP <- as.numeric(players$GP)

  #remove asterisk associated with hall of famers
  players$Player <- gsub("[*]", "", players$Player)

  #generate list of teams in season y
  teams <- unique(players$Team)
  teams <- teams[-which(teams=="TOT")]

  #start lookup table to hold team's total point shares
  lookup <- data.frame(teams, as.numeric(1), as.numeric(1))
  names(lookup)[[2]] <- "team_PS" #rename col appropriately
  names(lookup)[[3]] <- "team_pts_per" #rename col to team pts %

  #for each team in list, find their point shares and point percentages
  for(t in 1:length(teams)) {
    #find team t's statistics
    tags <- read_html(paste0("https://www.hockey-reference.com/teams/", teams[[t]], "/", y, ".html"))%>%
      html_table()

    #find and store their total point shares
    tags4 <- data.frame(tags[[4]])
    lookup[t,2] <- as.numeric(tags4$Point.Shares.2[[nrow(tags4)]])

    #find and store their points percentages
    tags1 <- data.frame(tags[[1]])
    lookup[t,3] <- as.numeric(tags1$PTS.[[1]])
  }

  #match player to their team's total point shares
  players$team_PS <- with(lookup, team_PS[match(players$Team, teams)])
  lookup2 <- players[players$Team=="TOT",] #identify which players were traded midseason
  players$G_adj <- with(lookup2, GP[match(players$Player, Player)]) #collect number of games played
  players$team_pts_per <- with(lookup, team_pts_per[match(players$Team, teams)]) #match team PTS% to players
  players$team_PS[is.na(players$team_PS)] <- 0 #players who were traded are labelled with 0 and dealt with below

  #directly calculate PS% for players who only played for one team, and calculate intermediate values for players
  #who were traded, to be summed over later
  players$PS_init <- ifelse(players$team_PS==0, 0,
                            ifelse(!is.na(players$G_adj),
                                   (players$GP*players$PS)/(players$G_adj*players$team_PS),
                                   players$PS/players$team_PS))

  #similarly, if player played for multiple teams, label PTS% with 0 now and then adjust
  players$team_pts_per[is.na(players$team_pts_per)] <- 0

  #if player played for one team, directly calculate PTS%, otherwise calculate intermediate values
  #to be summed over later
  players$PTSP_init <- ifelse(players$team_pts_per==0, 0,
                            ifelse(!is.na(players$G_adj),
                                   (players$GP*players$team_pts_per)/(players$G_adj),
                                   players$team_pts_per))

  #names(players)[[6]] <- "ID"

  #collapse each player's data into one observation
  res <- data.table(players) #get overall list of players
  res <- res[,list(Player=Player, Position = Pos, Team=Team, Team_PTSP = sum(PTSP_init),
                   PS_P=sum(PS_init)), by='ID'] #collapse PTSP and PS_P data for players who were traded mid-season
  res <- res[match(unique(res$ID), res$ID),]
  res$Year <- y #attach year to res for classification purposes later

  #generate list of players who received Hart votes in season y
  award <- read_html(paste0("https://www.hockey-reference.com/awards/voting-", y, ".html"))%>%
    html_table()
  award <- award[[1]] #obtain df
  award <- award[-1,c(2,7)] #eliminate unnecessary columns

  #generate IDs of players who received Hart votes in year y
  award_links <- read_html(paste0("https://www.hockey-reference.com/awards/voting-", y, ".html"))%>%
    html_nodes(xpath="//td/a") %>%
    html_attr("href")
  df2 <- data.frame(award_links) #obtain df
  df2<- data.frame(df2[grepl("players",df2$award_links) ,]) #only keep links connected to players
  df2 <- data.frame(gsub(".*\\/([^.]*).*", "\\1", df2[,1])) #extract ID
  names(df2) <- "ID" #rename column appropriately

  award$ID <- df2 #attach player ID to df
  names(award) <- c("Player", "Vote_Pct", "ID") #correct column names
  award <- award[,c(2,3)] #only keep ID and Vote_Pct

  res <- merge(res, award, by="ID") #keep only players who received at one least Hart vote during season y
  res$Vote_Pct <- as.numeric(res$Vote_Pct) #ensure Vote_Pct is treated numerically

  #append season y's observations to overall df
  overall <- rbind(overall, res)

  print(y) #helper to see which years have already been scraped
}

#adjust Aho's position and remove Hull from 1995 ballot
overall$Position[overall$Position=="F"] <- "C" #S. Aho adjustment
overall <- overall[overall$Vote_Pct!=0,] #Br. Hull adjustment

#convert Vote_Pct to a decimal for consistency
overall$Vote_Pct <- overall$Vote_Pct/100

#convert year to numeric, then generate new column that indicates which voting system applies to the player
overall$Year <- as.numeric(overall$Year)
overall$system <- ifelse(overall$Year<1982,0,
                                  ifelse(overall$Year>1981&overall$Year<1996, 1, 2))

#eliminate unnecessary columns
overall$ID <- overall$Year <- overall$Player <- overall$Team <- NULL

#generate model and print summary
mod1 <-  lm(log(asin(sqrt(Vote_Pct)))~factor(system)+factor(Position)+Team_PTSP+PS_P, data=overall)
summary(mod1)

################### various charts and tables used in the report

#table of coefficients, SE, t-values from model
coef <- data.frame(summary(mod1)$coefficients)
coef$Significance <- ifelse(coef$`Pr...t..`<.001, "***", ifelse(coef$`Pr...t..`<.01, "**",ifelse(coef$`Pr...t..`<.05, "*","n.s.")))
names(coef) <- c("Estimate", "SE", "t-value", "Pr(>|t|)", "Significance")

#confidence intervals for coefficients
ints <- confint(mod1)

#histograms of Vote_Pct, both overall and within each system
hist(overall$Vote_Pct, xlab="Vote %", main="Hart Vote %, 1968-2021")
hist(overall$Vote_Pct[overall$system==0], xlab="Vote %", main="Hart Vote %, 1968-1981")
hist(overall$Vote_Pct[overall$system==1], xlab="Vote %", main="Hart Vote %, 1982-1995")
hist(overall$Vote_Pct[overall$system==2], xlab="Vote %", main="Hart Vote %, 1996-2021")

#descriptive statistics for the continuous predictors in overall data set and Vote_Pct
#in the three subsets
sumtable(overall[,c(2:4)], add.median=T)
sumtable(overall[overall$system==0], add.median=T)
sumtable(overall[overall$system==1], add.median=T)
sumtable(overall[overall$system==2], add.median=T)

#histograms of Points % and Point Share %
hist(overall$Team_PTSP, xlab="TP%", main="Team Points %")
hist(overall$PS_P, xlab="PS%", main="Point Share %")

#bar graph of the counts at each position
barplot(table(overall$Position), xlab="Position", main="Hart Nominations by Position")