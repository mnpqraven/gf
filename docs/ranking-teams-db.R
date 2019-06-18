# IF YOU CHANGED THE FILENAME, YOU NEED TO ALSO CHANGE THE FILENAME HERE
#pathwd<-sub("/ranking-teams-db.R","",system("find . -type f -name ranking-teams-db.R",intern=T)[1])
#setwd(pathwd)
library(dplyr)
library(knitr)
library(kableExtra)
require(knitr)
require(kableExtra)

# TEAM SECTION EXAMPLE
example <- array(dim=c(3, 3, 1))
example[,,1] <- rbind(c(7,8,9),
                      c(4,5,6),
                      c(1,2,3))
colnames(example) <- c("R1","R2","R3")
################

# functions
print.team <- function(table, i) {
    if (i > 0 && i <= 10) {
        kable(table[,,i]) %>%
            kable_styling(c("bordered"), full_width=F) %>%
            column_spec(1, width = "15em") %>%
            column_spec(2, width = "15em") %>%
            column_spec(3, width = "15em")
    } else print("1 to 10 please")
}


# assign dbs
source('rank-dd.R')
