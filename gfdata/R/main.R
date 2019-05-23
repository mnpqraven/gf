library(dplyr)

## DOLL TABLE
# read the doll list table
dolldf = read.table(file="list.txt", header = T, sep = " ")
todo = read.table(file="todo.txt", header = T, sep = " ")
todo.datasim <- todo

## DATA MANIPULATION
# ring
dolldf$Ringed <- ifelse(is.na(dolldf$ring), "No", "Yes")
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
                                                sum(mat.digimind$digimind.frag[todo$rarity[i] == 2 & mat.digimind$digimind.mod > todo$modfrom[i] & mat.digimind$digimind.mod <= todo$modto[i]]) +
                                                sum(mat.digimind$digimind.frag[todo$rarity[i] == 3 & mat.digimind$digimind.mod > todo$modfrom[i] & mat.digimind$digimind.mod <= todo$modto[i]]) +
                                                sum(mat.digimind$digimind.frag[todo$rarity[i] == 4 & mat.digimind$digimind.mod > todo$modfrom[i] & mat.digimind$digimind.mod <= todo$modto[i]])
                                                )
                     )
todo.datasim$digimind.total.core <- sapply(seq_len(nrow(todo)), function(i) with(todo,
                                                sum(mat.digimind$digimind.core[todo$rarity[i] == 2 & mat.digimind$digimind.mod > todo$modfrom[i] & mat.digimind$digimind.mod <= todo$modto[i]]) +
                                                sum(mat.digimind$digimind.core[todo$rarity[i] == 3 & mat.digimind$digimind.mod > todo$modfrom[i] & mat.digimind$digimind.mod <= todo$modto[i]]) +
                                                sum(mat.digimind$digimind.core[todo$rarity[i] == 4 & mat.digimind$digimind.mod > todo$modfrom[i] & mat.digimind$digimind.mod <= todo$modto[i]])
                                                )
                     )
todo.datasim <- add_row(todo.datasim, name = "Total", datasim.total.tier1 = sum(todo.datasim$datasim.total.tier1), datasim.total.tier2 = sum(todo.datasim$datasim.total.tier2), datasim.total.tier3 = sum(todo.datasim$datasim.total.tier3))

## FILTERING
# use square brackets [] adding a , means no row filtering
# FACTORING, can be loosely interpretiert as categorizing
# type <- as.factor(dolldf$type)
# level display
levels(dolldf$type)

ar  <- dolldf$type == "AR"
# smg <- dolldf[dolldf$type == "SMG",]
# rf  <- dolldf[dolldf$type == "RF",]
# hg  <- dolldf[dolldf$type == "HG",]
# mg  <- dolldf[dolldf$type == "MG",]
# sg  <- dolldf[dolldf$type == "SG",]

## DISPLAYING
testfilter <- dolldf[ar,c("type","name","lv","slv")]
#str(dolldf)
# formatted doll df
dolldf.pretty <- dolldf[,c("type","Ringed", "name", "lv", "slv")]

## EXPORTING
# export to csv
# write.csv(dolldf,file='dolltable.csv', row.names = F)

