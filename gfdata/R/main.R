library(dplyr)

## TODO: Integrate with rmd
## https://stackoverflow.com/questions/10966109/how-to-source-r-markdown-file-like-sourcemyfile-r

## DOLL TABLE
# read the doll list table
dolldf = read.table(file="list.txt", header = T, sep = " ")
todo = read.table(file="todo.txt", header = T, sep = " ")
todo.datasim <- todo

## DATA MANIPULATION
# ring
dolldf$Ringed <- ifelse(is.na(dolldf$ring), "No", "Yes")
# core table
core.link <- c(1,2,3,4,5)
core.link.2star <- c(0,1,1,2,3)
core.link.3star <- c(0,3,3,6,9)
core.link.4star <- c(0,9,9,18,27)
core.link.5star <- c(0,15,15,30,45)
df.core.doll <- as.data.frame(cbind(core.link, core.link.2star, core.link.3star, core.link.4star, core.link.5star))
# datasim
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
df.digimind.2star <- as.data.frame(cbind(digimind.mod, digimind.2star.frag, digimind.2star.core))
df.digimind.3star <- as.data.frame(cbind(digimind.mod, digimind.3star.frag, digimind.3star.core))
df.digimind.4star <- as.data.frame(cbind(digimind.mod, digimind.4star.frag, digimind.4star.core))
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
                                                sum(df.digimind.2star$digimind.2star.frag[todo$rarity[i] == 2 & df.digimind.2star$digimind.mod > todo$modfrom[i] & df.digimind.2star$digimind.mod <= todo$modto[i]]) +
                                                sum(df.digimind.3star$digimind.3star.frag[todo$rarity[i] == 3 & df.digimind.3star$digimind.mod > todo$modfrom[i] & df.digimind.3star$digimind.mod <= todo$modto[i]]) +
                                                sum(df.digimind.4star$digimind.4star.frag[todo$rarity[i] == 4 & df.digimind.4star$digimind.mod > todo$modfrom[i] & df.digimind.4star$digimind.mod <= todo$modto[i]])
                                                )
                     )
todo.datasim$digimind.total.core <- sapply(seq_len(nrow(todo)), function(i) with(todo,
                                                sum(df.digimind.2star$digimind.2star.core[todo$rarity[i] == 2 & df.digimind.2star$digimind.mod > todo$modfrom[i] & df.digimind.2star$digimind.mod <= todo$modto[i]]) +
                                                sum(df.digimind.3star$digimind.3star.core[todo$rarity[i] == 3 & df.digimind.3star$digimind.mod > todo$modfrom[i] & df.digimind.3star$digimind.mod <= todo$modto[i]]) +
                                                sum(df.digimind.4star$digimind.4star.core[todo$rarity[i] == 4 & df.digimind.4star$digimind.mod > todo$modfrom[i] & df.digimind.4star$digimind.mod <= todo$modto[i]])
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

#datasimTotal <- todo.datasim[(nrow(todo.datasim) + 1), -(1:7)]
#datasimTotal <- colSums(todo.datasim[, -(1:7)], na.rm = T)
todo.datasim[(nrow(todo.datasim) + 1), -(1:9)] <- colSums(todo.datasim[, -(1:9)], na.rm = T)
levels(todo.datasim$type)[nrow(todo.datasim) + 1] <- "Total"
todo.datasim$type[nrow(todo.datasim)] <- "Total"

## FILTERING
# use square brackets [] adding a , means no row filtering
# FACTORING, can be loosely interpretiert as categorizing
# type <- as.factor(dolldf$type)

ar  <- dolldf$type == "AR"
# smg <- dolldf[dolldf$type == "SMG",]
# rf  <- dolldf[dolldf$type == "RF",]
# hg  <- dolldf[dolldf$type == "HG",]
# mg  <- dolldf[dolldf$type == "MG",]
# sg  <- dolldf[dolldf$type == "SG",]

## DISPLAYING
testfilter <- dolldf[ar,c("type","name","lv","slv")]
total.core <- todo.datasim$link.total.core[nrow(todo.datasim)] + todo.datasim$digimind.total.core[nrow(todo.datasim)]
#str(dolldf)
# formatted doll df
dolldf.pretty <- dolldf[,c("type","Ringed", "name", "lv", "slv")]
todo.pretty <- todo.datasim
colnames(todo.pretty) <- c("Type",
                           "Rarity",
                           "Name",
                           "SLv",
                           "Goal SLv",
                           "Link",
                           "Goal Link",
                           "MOD",
                           "Goal MOD",
                           "Basic",
                           "Intermediate",
                           "Advanced",
                           "Memory Frags",
                           "MOD Cores",
                           "Total Link")
## EXPORTING
# export to csv
# write.csv(dolldf,file='dolltable.csv', row.names = F)

