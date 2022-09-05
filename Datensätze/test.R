?read.csv
help("read.csv")


testdaten <- read.table("test.csv", 
                        header = TRUE,
                        sep = ", " )
testdaten

install.packages("psych")
library(psych)
?psych


setwd("ordnerpfad")

d <- read.table("erstis.dat")

