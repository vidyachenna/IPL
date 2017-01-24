library(ggplot2) #Adding the packages required.
library(plyr)
library(ggrepel)
library(reshape2)
library(graphics)

mat_data <- read.csv("matches.csv") #reading the .csv files into datasets  
del_data <- read.csv("deliveries.csv")

win_data <- count(mat_data, c("winner", "season")) #taking total count of winners corresponding to season
win_data <- subset(win_data, winner != "") #eliminating NA values

#Plotting season Vs Frequency of winning of teams
#------------------------------------------------

ggplot(data = win_data, aes(x = "",y = freq, fill = winner)) +
  geom_bar(width=1, stat = "identity") + 
  facet_grid(facets = .~season) +
  xlab("Season") +
  ylab("No of matches won") +
  guides(fill = guide_legend(title = "Team"))
  

champs = c() #creating dataset champs to store Champions data so far

for(i in 2:nrow(mat_data))  
  if(mat_data$season[i - 1] < mat_data$season[i])
    champs = rbind(champs, subset(mat_data, mat_data$id == mat_data$id[i] - 1))

#Plotting Pie plot for Winning Frequency for each Champion team
#--------------------------------------------------------------

win_count <- count(champs, "winner")
pie(win_count$freq, labels = win_count$winner, main = "Winning Rate of Champions")

#Holding the data of 2016 ChampionShip match(i.e, the last one) 

cham_mat <- subset(del_data, del_data$match_id == champs$id[nrow(champs)])
cham_data <- data.frame(cham_mat$over, cham_mat$total_runs, cham_mat$inning)

#Calculatinfg the runrate for each over in two innings

rr_data <- aggregate(cham_data$cham_mat.total_runs, 
                by = list(Over = cham_data$cham_mat.over, 
                          inning = cham_data$cham_mat.inning), FUN = sum)
names(rr_data)[3] <- paste("RunRate")
rr_data$inning[rr_data$inning == 1] <- "1"
rr_data$inning[rr_data$inning == 2] <- "2"

#Plotting the runrate for two innings in 2016 ChampionShip match
#---------------------------------------------------------------

qplot(Over, RunRate,data = rr_data, geom = c("point","line"), colour =inning) + 
  geom_point() + 
  geom_text_repel(aes(label = inning), size = 3) +
  ggtitle("Plot of Runrate per Over in each Innings") +
  xlab("Overs in each Innings") + ylab("Run Rate")

#Holding the the Top ten Batsman according to their MOM(Man of the Match) Awards

player_mat <- count(mat_data, "player_of_match")
player_order <- player_mat[order(player_mat$freq,player_mat$player_of_match,decreasing = TRUE),]
bst_btsman <- head(player_order, n = 10)

#Holding the data of batsman with fours and sixes as total runs

four_tot <- subset(del_data, del_data$total_runs == 4)
six_tot <- subset(del_data, del_data$total_runs == 6)

four_count <- count(four_tot, "batsman")
six_count <- count(six_tot, "batsman")

names(four_count)[2] <- paste("No.of Fours")
names(six_count)[2] <- paste("No.of Sixes")
names(bst_btsman)[1] <- paste("batsman")
names(bst_btsman)[2] <- paste("Freq")

#Merging the datasets so that we are having the Frequency Count of MOM Awards, Sixes, Fours.

tot_perf <- merge(six_count, four_count)
tot_perf <- merge(tot_perf, bst_btsman)

#Holding each column in merged data set as a variable

new.man =as.vector(tot_perf$batsman)
player_of_match = new.man

new.freq = as.vector(tot_perf$Freq)
MOMawards = new.freq

new.fours = as.vector(tot_perf$`No.of Fours`)
Fours = new.fours

new.six = as.vector(tot_perf$`No.of Sixes`)
Sixes = new.six

#Framing a data frame by above variables and Melting it to make tit compatible for plotting.

df = data.frame(player_of_match,MOMawards, Fours, Sixes)
df <- melt(df, id.vars='player_of_match')

#Plotting the above data frame in a grouped barplot
#--------------------------------------------------

ggplot(df, aes(x=player_of_match, y=value, fill=variable)) + 
  ggtitle("Performance of Top Ten Players") +
  geom_bar(stat='identity', position='dodge')+
  xlab("Top Ten Players") +
  ylab("Range") +
  guides(fill = guide_legend(title = "Parameters"))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#Holding count of each team as team1 & team2 in each city

t1_count<- count(mat_data, c("city", "team1"))
t2_count <- count(mat_data, c("city", "team2"))

#combining above two datasets to get total frequency count of team played in each city

names(t1_count)[2] <- paste("team")
names(t2_count)[2] <- paste("team")
team_tot <- rbind(t1_count, t2_count)
team_count <- count(team_tot, c("city", "team"))

#Holding frequency count of team won in a each city 

winner_count <- count(mat_data, c("city", "winner"))

#Now I am taking "MUMBAI INDIANS" as my team to perform regression between.....
# matches played in each city and matches won in that city.

mi_played <- subset(team_count, team == "Mumbai Indians")
mi_won <- subset(winner_count, winner == "Mumbai Indians")

names(mi_won)[3] <- paste("matches_won")
names(mi_played)[3] <- paste("matches_played")

mi_res <- merge(mi_played, mi_won)
mi_res$team <- NULL
mi_res$winner<- NULL

#plotting my regression showing the sucess rate of "MUMBAI INDIANS" in each city
#-------------------------------------------------------------------------------

attach(mi_res)
plot(matches_played, matches_won,
     main = "Success rate of MUMBAI INDIANS in each city", col = "blue",
     abline(lm(matches_won ~ matches_played)),
     pch = 19,
     xlab = "Matches Played",
     ylab = "Matches Won",
     xlim = c(0, 12), ylim = c(0, 10)) 
text(matches_played, matches_won, labels = city, cex = .7, pos = 4)
detach(mi_res)

#########################################################################################
