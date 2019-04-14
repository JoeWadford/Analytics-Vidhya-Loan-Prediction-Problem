setwd("C:/Users/joewa/OneDrive/Documents/MBA/MBAD 6211/5- Hackathon")
teams_sc <- fread("train_u6lujuX_CVtuZ9i.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
test_sc <- fread("test_Y3wMUE5_7gLdaTN.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
