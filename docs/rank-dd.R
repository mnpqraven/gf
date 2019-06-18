rank.dd <- array(dim=c(3, 3, 10))

# team section
rank.dd[,,1] <- rbind(
  c("G36","Skorpion",NA),
  c("M4A1","RO635",NA),
  c("G11",NA,NA)
)
rank.dd[,,2] <- rbind(
  c("G11","G36c",NA),
  c("K5","UMP45",NA),
  c("RFB",NA,NA)
)
rank.dd[,,3] <- rbind(
  c("G11","K5",NA),
  c("ST AR-15","Type 79",NA),
  c("K2",NA,NA)
)
rank.dd[,,4] <- rbind(
  c("Howa Type 64","Vector",NA),
  c("Grizzly","UMP9",NA),
  c("G11",NA,NA)
)
rank.dd[,,5] <- rbind(
  c("WA2000","PPK",NA),
  c("Five-Seven","Welrod",NA),
  c("SVD",NA,NA)
)
rank.dd[,,6] <- rbind(
  c("WA2000","Mk23",NA),
  c("Five-Seven","Welrod",NA),
  c("SVD",NA,NA)
)
rank.dd[,,7] <- rbind(
  c("M2HB",NA,NA),
  c("Colt SAA","Grizzly","Elphelt"),
  c("PK",NA,NA)
)
rank.dd[,,8] <- rbind(
  c("MG5",NA,NA),
  c("LWMMG","MP-446","Type 79-S"),
  c("PK",NA,NA)
)
rank.dd[,,9] <- rbind(
  c("PKP",NA,NA),
  c("M1919","Mk23","M37"),
  c("MG5",NA,NA)
)
rank.dd[,,10] <- rbind(
  c("Negev",NA,NA),
  c("Nagant","Contender","KSG"),
  c("MG5",NA,NA)
)
# colnames
colnames(rank.dd) <- c("R1","R2","R3")
