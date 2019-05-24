# IF YOU CHANGED THE FILENAME, YOU NEED TO ALSO CHANGE THE FILENAME HERE
pathwd<-sub("/dataframes.R","",system("find . -type f -name dataframes.R",intern=T)[1])
setwd(pathwd)
library(dplyr)
library(knitr)
## TODO: Integrate with rmd
## https://stackoverflow.com/questions/10966109/how-to-source-r-markdown-file-like-sourcemyfile-r

## DOLL TABLE
# read the doll list table
df.dolllist = read.table(file="dbs/list.csv", header = T, sep = " ")
todo = read.table(file="dbs/todo.csv", header = T, sep = " ")
todo.datasim <- todo

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
todo.datasim$datasim.total.tier1 <- sapply(seq_len(nrow(todo)), function(i) with(todo,
                                                sum(df.datasim.fairyStrat$datasim.fairyStrat.tier1[todo$type[i] == "fairyStrat" & df.datasim.fairyStrat$datasim.slv >= todo$slvfrom[i] & df.datasim.fairyStrat$datasim.slv < todo$slvto[i]]) +
                                                sum(df.datasim.fairyBattl$datasim.fairyBattl.tier1[todo$type[i] == "fairyBattl" & df.datasim.fairyBattl$datasim.slv >= todo$slvfrom[i] & df.datasim.fairyBattl$datasim.slv < todo$slvto[i]]) +
                                                sum(df.datasim.doll$datasim.doll.tier1[todo$type[i] == "doll" & df.datasim.doll$datasim.slv >= todo$slvfrom[i] & df.datasim.doll$datasim.slv < todo$slvto[i]])
                                                )
                     )
todo.datasim$datasim.total.tier2 <- sapply(seq_len(nrow(todo)), function(i) with(todo,
                                                sum(df.datasim.fairyStrat$datasim.fairyStrat.tier2[todo$type[i] == "fairyStrat" & df.datasim.fairyStrat$datasim.slv >= todo$slvfrom[i] & df.datasim.fairyStrat$datasim.slv < todo$slvto[i]]) +
                                                sum(df.datasim.fairyBattl$datasim.fairyBattl.tier2[todo$type[i] == "fairyBattl" & df.datasim.fairyBattl$datasim.slv >= todo$slvfrom[i] & df.datasim.fairyBattl$datasim.slv < todo$slvto[i]]) +
                                                sum(df.datasim.doll$datasim.doll.tier2[todo$type[i] == "doll" & df.datasim.doll$datasim.slv >= todo$slvfrom[i] & df.datasim.doll$datasim.slv < todo$slvto[i]])
                                                )
                     )
todo.datasim$datasim.total.tier3 <- sapply(seq_len(nrow(todo)), function(i) with(todo,
                                                sum(df.datasim.fairyStrat$datasim.fairyStrat.tier3[todo$type[i] == "fairyStrat" & df.datasim.fairyStrat$datasim.slv >= todo$slvfrom[i] & df.datasim.fairyStrat$datasim.slv < todo$slvto[i]]) +
                                                sum(df.datasim.fairyBattl$datasim.fairyBattl.tier3[todo$type[i] == "fairyBattl" & df.datasim.fairyBattl$datasim.slv >= todo$slvfrom[i] & df.datasim.fairyBattl$datasim.slv < todo$slvto[i]]) +
                                                sum(df.datasim.doll$datasim.doll.tier3[todo$type[i] == "doll" & df.datasim.doll$datasim.slv >= todo$slvfrom[i] & df.datasim.doll$datasim.slv < todo$slvto[i]])
                                                )
                     )

todo.datasim$digimind.total.frag <- sapply(seq_len(nrow(todo)), function(i) with(todo,
                                                sum(df.digimind$digimind.2star.frag[todo$rarity[i] == 2 & df.digimind$digimind.mod > todo$modfrom[i] & df.digimind$digimind.mod <= todo$modto[i]]) +
                                                sum(df.digimind$digimind.3star.frag[todo$rarity[i] == 3 & df.digimind$digimind.mod > todo$modfrom[i] & df.digimind$digimind.mod <= todo$modto[i]]) +
                                                sum(df.digimind$digimind.4star.frag[todo$rarity[i] == 4 & df.digimind$digimind.mod > todo$modfrom[i] & df.digimind$digimind.mod <= todo$modto[i]])
                                                )
                     )
todo.datasim$digimind.total.core <- sapply(seq_len(nrow(todo)), function(i) with(todo,
                                                sum(df.digimind$digimind.2star.core[todo$rarity[i] == 2 & df.digimind$digimind.mod > todo$modfrom[i] & df.digimind$digimind.mod <= todo$modto[i]]) +
                                                sum(df.digimind$digimind.3star.core[todo$rarity[i] == 3 & df.digimind$digimind.mod > todo$modfrom[i] & df.digimind$digimind.mod <= todo$modto[i]]) +
                                                sum(df.digimind$digimind.4star.core[todo$rarity[i] == 4 & df.digimind$digimind.mod > todo$modfrom[i] & df.digimind$digimind.mod <= todo$modto[i]])
                                                )
                     )

todo.datasim$link.total.core <- sapply(seq_len(nrow(todo)), function(i) with(todo,
                                                                             sum(df.core.doll$core.link.2star[todo$rarity[i] == 2 & df.core.doll$core.link >= todo$linkfrom[i] & df.core.doll$core.link <= todo$linkto[i]]) +
                                                                             sum(df.core.doll$core.link.3star[todo$rarity[i] == 3 & df.core.doll$core.link >= todo$linkfrom[i] & df.core.doll$core.link <= todo$linkto[i]]) +
                                                                             sum(df.core.doll$core.link.4star[todo$rarity[i] == 4 & df.core.doll$core.link >= todo$linkfrom[i] & df.core.doll$core.link <= todo$linkto[i]]) +
                                                                             sum(df.core.doll$core.link.5star[todo$rarity[i] == 5 & df.core.doll$core.link >= todo$linkfrom[i] & df.core.doll$core.link <= todo$linkto[i]])
)
)
#todo.datasim <- add_row(todo.datasim,
#                        name = "Total", datasim.total.tier1 = sum(todo.datasim$datasim.total.tier1),
#                        datasim.total.tier2 = sum(todo.datasim$datasim.total.tier2, na.rm = T),
#                        datasim.total.tier3 = sum(todo.datasim$datasim.total.tier3, na.rm = T),
#                        digimind.total.frag = sum(todo.datasim$digimind.total.frag, na.rm = T),
#                        digimind.total.core = sum(todo.datasim$digimind.total.core, na.rm = T))



## FILTERING
# use square brackets [] adding a , means no row filtering
# FACTORING, can be loosely interpretiert as categorizing
# type <- as.factor(df.dolllist$type)

ar  <- df.dolllist$type == "AR"
# smg <- df.dolllist[df.dolllist$type == "SMG",]
# rf  <- df.dolllist[df.dolllist$type == "RF",]
# hg  <- df.dolllist[df.dolllist$type == "HG",]
# mg  <- df.dolllist[df.dolllist$type == "MG",]
# sg  <- df.dolllist[df.dolllist$type == "SG",]

## DISPLAYING
testfilter <- df.dolllist[ar,c("type","name","lv","slv")]
total.core <- todo.datasim$link.total.core[nrow(todo.datasim)] + todo.datasim$digimind.total.core[nrow(todo.datasim)]

# formatted doll df
df.dolllist.pretty <- df.dolllist[,c("type", "name", "link", "lv", "slv")]
df.dolllist.pretty$Ringed <- ifelse(is.na(df.dolllist$ring), "No", "Yes")
df.dolllist.pretty <- df.dolllist.pretty[,c("type", "Ringed" ,"name", "lv", "link", "slv")]

# formatted todo df
todo.pretty <- todo.datasim
todo.pretty[(nrow(todo.datasim) + 1), -(1:9)] <- colSums(todo.pretty[, -(1:9)], na.rm = T)
levels(todo.pretty$type)[nrow(todo.pretty) + 1] <- "Total"
todo.pretty$type[nrow(todo.pretty)] <- "Total"

colnames(todo.pretty) <- c("Type",
                           "Rarity",
                           "Name",
                           "SLv",
                           "Goal SLv",
                           "MOD",
                           "To MOD",
                           "Link",
                           "To Link",
                           "Basic",
                           "Intermediate",
                           "Advanced",
                           "Memory Frags",
                           "MOD Cores",
                           "Total Link")
todo.pretty[todo.pretty == 0] <- NA

## EXPORTING
# export to csv
write.table(df.dolllist, file = "dbs/dolllist_output.csv", sep = " ", quote = F)
write.table(todo.datasim, file = "dbs/todolist_output.csv", sep = " ", quote = F)

#useless
fn <- function(x) {
  x + 1 # A comment, kept as part of the source
}
getSrcDirectory(fn)
