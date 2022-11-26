


setwd("C:/Users/Gal/Documents/FRI/UI/Seminarska/")
md <- read.table(file="nbadata.txt", sep=",", header=TRUE)


SEASONS = unique(md$gmSeason);
TEAMS = unique(c(md$awayAbbr, md$homeAbbr))

# povprecno stevilo metov trojk obeh ekip skupaj na sezono
averageThrees <- vector()
for(season in SEASONS) {
    seasonGames <- md$gmSeason == season;
    averageThrees <- c(averageThrees, mean(md$home3PA[seasonGames] + md$away3PA[seasonGames]));
}
barplot(averageThrees, names=SEASONS, main="Povprecno število vseh metov za tri pike v tekmi na sezono");

# stevilo zmag home ekip vs stevilo zmag away ekip
homeWins = table(md$homePTS > md$awayPTS);
pie(homeWins, labels=c("Away", "Home"), main="Zmage doma VS zmage v gostovanju");


# najboljsa ekipa po razmerju zmag
winRatioByTeam = list();
for (team in TEAMS) {
    homeGames <- md$homeAbbr == team;
    awayGames <- md$awayAbbr == team;
    homeWins <- md$homePTS[homeGames] > md$awayPTS[homeGames];
    awayWins <- md$homePTS[awayGames] < md$awayPTS[awayGames];
    totalWins  <- sum(homeWins) + sum(awayWins);
    totalGames <- sum(homeGames) + sum(awayGames);
    ratio <- totalWins / totalGames;
    winRatioByTeam[[team]] <- ratio;
}
winRatioByTeam <- sort(unlist(winRatioByTeam), decreasing=TRUE);
barplot(winRatioByTeam[1:5], names=names(winRatioByTeam)[1:5], main="Top 5 najboljših ekip po razmerju zmag");

homeFreeShotSuccessRatio = md$homeFTM / md$homeFTA;
awayFreeShotSuccessRatio = md$awayFTM / md$awayFTA;
totalFreeShotSuccessRatio = append(homeFreeShotSuccessRatio, awayFreeShotSuccessRatio);
daysOff = append(md$homeDayOff, md$awayDayOff);
plot(totalFreeShotSuccessRatio,daysOff, main="Razmerje uspešnih prostih metov odvisno od števila prostih dni", xlab="Število prostih dni", ylab="Razmerje uspešnih metov za tri pike");



md <- md[order(md$gmDate),]

train <- md[1:round(0.7*nrow(md)),]
test <- md[-(1:round(0.7*nrow(md))),]

# Damo namesto absolutno stevilo metov v procente uspesnih?
# convert successful points to percentages
train$homePTS <- train$homePTS / train$homeFGA;
train$homeFGA <- NULL;
train$awayPTS <- train$awayPTS / train$awayFGA;
train$awayFGA <- NULL;
# convert successful free throws to percentages
train$homeFTM <- train$homeFTM / train$homeFTA;
train$homeFTA <- NULL;
train$awayFTM <- train$awayFTM / train$awayFTA;
train$awayFTA <- NULL;
# convert successful three pointers to percentages
train$home3PM <- train$home3PM / train$home3PA;
train$home3PA <- NULL;
train$away3PM <- train$away3PM / train$away3PA;
train$away3PA <- NULL;
# convert successful two pointers to percentages
train$home2PM <- train$home2PM / train$home2PA;
train$home2PA <- NULL;
train$away2PM <- train$away2PM / train$away2PA;
train$away2PA <- NULL;
summary(train)



