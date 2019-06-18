library(dplyr,knitr)
example <- array(dim=c(3, 3, 1))
example[,,1] <- rbind(c(7,8,9),
                      c(4,5,6),
                      c(1,2,3))
a <- array(dim=c(3, 3, 10))
a[,,1] <- rbind(c("G11","Vector",NA),
                c("M4A1","UMP45",NA),
                c("G36",NA,NA))
colnames(a) <- c("R1","R2","R3")

print.team <- function(i) {
    if (i > 0 && i <= 10) {
        kable(a[,,i])
    } else print("1 to 10 please")
}
