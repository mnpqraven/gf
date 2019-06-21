# IF YOU CHANGED THE FILENAME, YOU NEED TO ALSO CHANGE THE FILENAME HERE
pathwd<-sub("/dataframes.R","",system("find . -type f -name dataframes.R",intern=T)[1])
setwd(pathwd)
library(condformat)
library(htmlTable)
library(dplyr)
library(knitr)
library(plyr)
library(kableExtra)
library(data.table)
## TODO: Integrate with rmd
## https://stackoverflow.com/questions/10966109/how-to-source-r-markdown-file-like-sourcemyfile-r

## DOLL TABLE
# read the doll list table

## DATA MANIPULATION
# core table
core.link <- c(1,2,3,4,5)
core.link.2star <- c(0,1,1,2,3)
core.link.3star <- c(0,3,3,6,9)
core.link.4star <- c(0,9,9,18,27)
core.link.5star <- c(0,15,15,30,45)
df.core.doll <- as.data.frame(cbind(core.link, core.link.2star, core.link.3star, core.link.4star, core.link.5star))

# datasim dataframes
datasim.slv <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)

datasim.doll.tier1 <- c(100, 200, 300, 0, 0, 0, 0, 0, 0)
datasim.doll.tier2 <- c(0, 0, 0, 120, 200, 300, 400, 0, 0)
datasim.doll.tier3 <- c(0, 0, 0, 0, 0, 0, 0, 200, 300)
df.datasim.doll <- as.data.frame(cbind(datasim.slv, datasim.doll.tier1, datasim.doll.tier2, datasim.doll.tier3))

datasim.fairyStrat.tier1 <- c(200, 400, 600, 0, 0, 0, 0, 0, 0)
datasim.fairyStrat.tier2 <- c(0, 0, 0, 240, 400, 600, 800, 0, 0)
datasim.fairyStrat.tier3 <- c(0, 0, 0, 0, 0, 0, 0, 400, 600)
df.datasim.fairyStrat <- as.data.frame(cbind(datasim.slv, datasim.fairyStrat.tier1, datasim.fairyStrat.tier2, datasim.fairyStrat.tier3))

datasim.fairyBattl.tier1 <- c(200, 400, 600, 0, 0, 0, 0, 0, 0)
datasim.fairyBattl.tier2 <- c(0, 0, 0, 200, 400, 600, 0, 0, 0)
datasim.fairyBattl.tier3 <- c(0, 0, 0, 0, 0, 0, 200, 400, 600)
df.datasim.fairyBattl <- as.data.frame(cbind(datasim.slv, datasim.fairyBattl.tier1, datasim.fairyBattl.tier2, datasim.fairyBattl.tier3))

datasim.HOC.skill1.tier1 <- c(200, 400, 600, 0, 0, 0, 0, 0, 0)
datasim.HOC.skill1.tier2 <- c(0, 0, 0, 200, 400, 600, 0, 0, 0)
datasim.HOC.skill1.tier3 <- c(0, 0, 0, 0, 0, 0, 200, 400, 600)
datasim.HOC.skill2.tier1 <- datasim.HOC.skill1.tier1/2
datasim.HOC.skill2.tier2 <- datasim.HOC.skill1.tier2/2
datasim.HOC.skill2.tier3 <- datasim.HOC.skill1.tier3/2
datasim.HOC.batts <- c(50, 50, 50, 80, 80, 80, 100, 100, 100)
df.datasim.HOC <- as.data.frame(cbind(datasim.slv, datasim.HOC.skill1.tier1, datasim.HOC.skill1.tier2, datasim.HOC.skill1.tier3, datasim.HOC.skill2.tier1, datasim.HOC.skill2.tier2, datasim.HOC.skill2.tier3, datasim.HOC.batts))

digimind.mod <- c(1,2,3)
digimind.2star.frag <- c(200, 600, 1200)
digimind.2star.core <- c(10, 20, 30)
digimind.3star.frag <- c(200, 800, 1600)
digimind.3star.core <- c(15, 30, 45)
digimind.4star.frag <- c(200, 1000, 2000)
digimind.4star.core <- c(20, 40, 60)
df.digimind <- as.data.frame(cbind(digimind.mod, digimind.2star.frag, digimind.2star.frag, digimind.3star.frag, digimind.3star.core, digimind.4star.frag, digimind.4star.core))
# datasim calculation
# TODO HOC



## FILTERING
# use square brackets [] adding a , means no row filtering
# FACTORING, can be loosely interpretiert as categorizing
# type <- as.factor(df.dolllist$type)


## DISPLAYING
#testfilter <- df.dolllist[ar,c("type","name","lv","slv")]

# formatted doll df
df.dolllist.pretty <- df.dolllist[,c("type", "name", "link", "lv", "slv", "mod")]
df.dolllist.pretty$Ringed <- ifelse(df.dolllist$ring == 1, "Yes", "No")
df.dolllist.pretty <- df.dolllist.pretty[,c("type", "Ringed" ,"name", "lv","mod", "link", "slv")]

# init pretty var for todo dataframe
todo.pretty <- todo

#format .pretty
#todo.pretty[(nrow(todo.pretty) + 1), -(1:9)] <- colSums(todo.pretty[, -(1:9)], na.rm = T)
#levels(todo.pretty$type)[nrow(todo.pretty) + 1] <- "Total"
#todo.pretty$type[nrow(todo.pretty)] <- "Total"

#clean up column names
#colnames(todo.pretty) <- c("Type",
#                           "Rarity",
#                           "Dupe ID",
#                           "Name",
#                           "Goal SLv",
#                           "To MOD",
#                           "To Link")
#todo.pretty[todo.pretty == 0] <- NA

a <- c(1:nrow(df.dolllist.pretty[df.dolllist.pretty$type == "AR",]),
       1:nrow(df.dolllist.pretty[df.dolllist.pretty$type == "SMG",]),
       1:nrow(df.dolllist.pretty[df.dolllist.pretty$type == "RF",]),
       1:nrow(df.dolllist.pretty[df.dolllist.pretty$type == "HG",]),
       1:nrow(df.dolllist.pretty[df.dolllist.pretty$type == "MG",]),
       1:nrow(df.dolllist.pretty[df.dolllist.pretty$type == "SG",]))
df.dolllist.pretty$count <- a
df.dolllist.pretty <- df.dolllist.pretty[c("count", "type", "Ringed", "name", "lv", "mod", "link", "slv")]
colnames(df.dolllist.pretty) <- c("No.",
                                  "Type",
                                  "Ringed",
                                  "Name",
                                  "Lv",
                                  "MOD",
                                  "Link",
                                  "SLv")

df.fairylist.pretty <- df.fairylist[c("type", "rarity", "name","lv", "slv")]
colnames(df.fairylist.pretty) <- c("Type", "Rarity", "Name", "Lv", "SLv")
## EXPORTING
# export to csv
# write.table(df.dolllist, file = "dbs/dolllist_output.csv", sep = ",", quote = F, row.names=F)
# write.table(todo, file = "dbs/todolist_output.csv", sep = ",", quote = F, row.names=F)
# write.table(df.fairylist, file = "dbs/df.fairylist_output.csv", sep = ",", quote = F, row.names=F)


#TODO:
# CSV export to human readable + human input (<space> separator, no double quote)
# rework todo (df)

df.joined.fairy.doll <- full_join(df.dolllist, df.fairylist, copy = TRUE)
fulltodo <- merge(todo, df.joined.fairy.doll, by.x = c("name", "dID"), by.y = c("name", "dID"))
fulltodo <- fulltodo[c("type.y", "rarity.y", "dID", "name", "lv", "slv", "slv2", "slvto", "slv2to", "link", "linkto", "mod", "modto")]

fulltodo$datasim.total.tier1 <- sapply(seq_len(nrow(fulltodo)), function(i) with(fulltodo,
sum(df.datasim.fairyStrat$datasim.fairyStrat.tier1[fulltodo$type.y[i] == "fairyStrat" & df.datasim.fairyStrat$datasim.slv >= fulltodo$slv[i] & df.datasim.fairyStrat$datasim.slv < fulltodo$slvto[i]]) +
sum(df.datasim.fairyBattl$datasim.fairyBattl.tier1[fulltodo$type.y[i] == "fairyBattl" & df.datasim.fairyBattl$datasim.slv >= fulltodo$slv[i] & df.datasim.fairyBattl$datasim.slv < fulltodo$slvto[i]]) +
sum(df.datasim.doll$datasim.doll.tier1[fulltodo$type[i] != "fairyStrat" & fulltodo$type[i] != "fairyBattl" & df.datasim.doll$datasim.slv >= fulltodo$slv[i] & df.datasim.doll$datasim.slv < fulltodo$slvto[i]]) +
sum(df.datasim.doll$datasim.doll.tier1[!is.na(fulltodo$slv2[i]) & df.datasim.doll$datasim.slv >= fulltodo$slv2[i] & df.datasim.doll$datasim.slv < fulltodo$slv2to[i]])
))
fulltodo$datasim.total.tier2 <- sapply(seq_len(nrow(fulltodo)), function(i) with(fulltodo,
sum(df.datasim.fairyStrat$datasim.fairyStrat.tier2[fulltodo$type.y[i] == "fairyStrat" & df.datasim.fairyStrat$datasim.slv >= fulltodo$slv[i] & df.datasim.fairyStrat$datasim.slv < fulltodo$slvto[i]]) +
sum(df.datasim.fairyBattl$datasim.fairyBattl.tier2[fulltodo$type.y[i] == "fairyBattl" & df.datasim.fairyBattl$datasim.slv >= fulltodo$slv[i] & df.datasim.fairyBattl$datasim.slv < fulltodo$slvto[i]]) +
sum(df.datasim.doll$datasim.doll.tier2[fulltodo$type[i] != "fairyStrat" & fulltodo$type[i] != "fairyBattl" & df.datasim.doll$datasim.slv >= fulltodo$slv[i] & df.datasim.doll$datasim.slv < fulltodo$slvto[i]]) +
sum(df.datasim.doll$datasim.doll.tier2[!is.na(fulltodo$slv2[i]) & df.datasim.doll$datasim.slv >= fulltodo$slv2[i] & df.datasim.doll$datasim.slv < fulltodo$slv2to[i]])
))
fulltodo$datasim.total.tier3 <- sapply(seq_len(nrow(fulltodo)), function(i) with(fulltodo,
sum(df.datasim.fairyStrat$datasim.fairyStrat.tier3[fulltodo$type.y[i] == "fairyStrat" & df.datasim.fairyStrat$datasim.slv >= fulltodo$slv[i] & df.datasim.fairyStrat$datasim.slv < fulltodo$slvto[i]]) +
sum(df.datasim.fairyBattl$datasim.fairyBattl.tier3[fulltodo$type.y[i] == "fairyBattl" & df.datasim.fairyBattl$datasim.slv >= fulltodo$slv[i] & df.datasim.fairyBattl$datasim.slv < fulltodo$slvto[i]]) +
sum(df.datasim.doll$datasim.doll.tier3[fulltodo$type[i] != "fairyStrat" & fulltodo$type[i] != "fairyBattl" & df.datasim.doll$datasim.slv >= fulltodo$slv[i] & df.datasim.doll$datasim.slv < fulltodo$slvto[i]]) +
sum(df.datasim.doll$datasim.doll.tier3[!is.na(fulltodo$slv2[i]) & df.datasim.doll$datasim.slv >= fulltodo$slv2[i] & df.datasim.doll$datasim.slv < fulltodo$slv2to[i]])
))
fulltodo$digimind.total.frag <- sapply(seq_len(nrow(fulltodo)), function(i) with(fulltodo,
sum(df.digimind$digimind.2star.frag[fulltodo$rarity[i] == 2 & df.digimind$digimind.mod > fulltodo$mod[i] & df.digimind$digimind.mod <= fulltodo$modto[i]]) +
sum(df.digimind$digimind.3star.frag[fulltodo$rarity[i] == 3 & df.digimind$digimind.mod > fulltodo$mod[i] & df.digimind$digimind.mod <= fulltodo$modto[i]]) +
sum(df.digimind$digimind.4star.frag[fulltodo$rarity[i] == 4 & df.digimind$digimind.mod > fulltodo$mod[i] & df.digimind$digimind.mod <= fulltodo$modto[i]])
))
fulltodo$digimind.total.core <- sapply(seq_len(nrow(fulltodo)), function(i) with(fulltodo,
sum(df.digimind$digimind.2star.core[fulltodo$rarity[i] == 2 & df.digimind$digimind.mod > fulltodo$mod[i] & df.digimind$digimind.mod <= fulltodo$modto[i]]) +
sum(df.digimind$digimind.3star.core[fulltodo$rarity[i] == 3 & df.digimind$digimind.mod > fulltodo$mod[i] & df.digimind$digimind.mod <= fulltodo$modto[i]]) +
sum(df.digimind$digimind.4star.core[fulltodo$rarity[i] == 4 & df.digimind$digimind.mod > fulltodo$mod[i] & df.digimind$digimind.mod <= fulltodo$modto[i]])
))

fulltodo$link.total.core <- sapply(seq_len(nrow(fulltodo)), function(i) with(fulltodo,
sum(df.core.doll$core.link.2star[fulltodo$rarity[i] == 2 & df.core.doll$core.link > fulltodo$link[i] & df.core.doll$core.link <= fulltodo$linkto[i]]) +
sum(df.core.doll$core.link.3star[fulltodo$rarity[i] == 3 & df.core.doll$core.link > fulltodo$link[i] & df.core.doll$core.link <= fulltodo$linkto[i]]) +
sum(df.core.doll$core.link.4star[fulltodo$rarity[i] == 4 & df.core.doll$core.link > fulltodo$link[i] & df.core.doll$core.link <= fulltodo$linkto[i]]) +
sum(df.core.doll$core.link.5star[fulltodo$rarity[i] == 5 & df.core.doll$core.link > fulltodo$link[i] & df.core.doll$core.link <= fulltodo$linkto[i]])
))

fulltodo <- fulltodo[!(is.na(fulltodo$slvto) & is.na(fulltodo$slv2to) & is.na(fulltodo$modto) & is.na(fulltodo$linkto)),]
#format total for merge
fulltodo.total <- fulltodo
fulltodo.total[(nrow(fulltodo) + 1), -(1:9)] <- colSums(fulltodo[, -(1:9)], na.rm = T)
levels(fulltodo.total$name)[nrow(fulltodo) + 1] <- "Total"
fulltodo.total$name[nrow(fulltodo)] <- "Total"

#filtered for better categorizing
filter.todo.core <- fulltodo[c("type.y", "name", "link", "linkto", "link.total.core", "dID", "rarity.y")]
#catch only non-NAs
filter.todo.core <- filter.todo.core[!is.na(filter.todo.core$link.total.core) & filter.todo.core$link.total.core != 0,]
rownames(filter.todo.core) <- NULL
filter.todo.core[(nrow(filter.todo.core) + 1), 5] <- sum(filter.todo.core[, 5])
filter.todo.core$type.y[nrow(filter.todo.core)] <- "Total"
colnames(filter.todo.core) <- c("Type", "Name", "Current Link", "Link Goal", "Cores needed", "dID", "rarity.y")

filter.todo.MOD  <- fulltodo[c("type.y", "name",  "modto", "digimind.total.frag", "digimind.total.core", "dID", "rarity.y")]
filter.todo.MOD  <- filter.todo.MOD[complete.cases(filter.todo.MOD[,3]),]
rownames(filter.todo.MOD) <- NULL
filter.todo.MOD[(nrow(filter.todo.MOD) + 1), -(1:3)] <- colSums(filter.todo.MOD[, -(1:3)])
filter.todo.MOD$type.y[nrow(filter.todo.MOD)] <- "Total"
colnames(filter.todo.MOD) <- c("Type", "Name", "Mod Goal", "Fragments Needed", "Cores Needed", "dID", "rarity.y")

filter.todo.slv  <- fulltodo[c("type.y", "name", "slv", "slvto", "slv2to", "datasim.total.tier1", "datasim.total.tier2", "datasim.total.tier3")]
filter.todo.slv  <- filter.todo.slv[!is.na(filter.todo.slv$slvto) | !is.na(filter.todo.slv$slv2to),]
rownames(filter.todo.slv) <- NULL
filter.todo.slv[(nrow(filter.todo.slv) + 1), -(1:4)] <- colSums(filter.todo.slv[, -(1:4)])
filter.todo.slv$type.y[nrow(filter.todo.slv)] <- "Total"
colnames(filter.todo.slv) <- c("Type", "Name", "SLv", "SLv Goal","SLv2 Goal", "Basic", "Intermediate", "Advanced")
