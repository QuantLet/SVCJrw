# List of libraries to be used
lib <- list("truncnorm", "MASS", "MCMCpack", "ggplot2", "readr", "Rlab",
            "dplyr", "tidyverse")

# Installing or calling the libraries
invisible(lapply(lib, function(x){
  result <- library(x, logical.return=T, character.only =T)
  if(result == F) install.packages(x)
  library(x, character.only =T)
  print(paste0(x, " loaded"))
}))



#load SVCJ code from Perez (2018)
source("svcj_model.R")

#load CRIX data
#tmp <- read.csv("http://data.thecrix.de/data/new_crix.csv")
tmp <- read.csv("crix_price.csv")
tmp$date <- as.Date(tmp$date)
prices = list() #format it to a list to be in line with Perez'code
prices[["crix"]] <- as.data.frame(tmp)

#SVCJ-fit to CRIX for the whole period of analysis
crix_svcj <- svcj_model(prices$crix$price, N = 5000, n = 1000)
crix_svcj$parameters
save(crix_svcj,file="crix_svcj.Rda")

#here starts the rolling window approach: the estimates are fitted to every second data point
# in order to reduce the computation time

#600
seq.vec <- as.vector(seq(from=1, to=nrow(tmp)-600, by=2)) 
param <- data.frame(matrix(nrow = 10, ncol = ceiling((nrow(tmp)-600)/2)), 
                    row.names =crix_svcj$parameters$parameter)
colnames(param) <- seq.vec

for (i in seq.vec) {
  count.tmp <- i+600
  crix_svcj.tmp <- svcj_model(prices$crix$price[i:count.tmp], N = 5000, n = 1000)
  param[,as.character(i)] <- crix_svcj.tmp$parameters$mean
  # colnames(param)[i] <- i
}
param600 <- param
save(param600,file="param600.Rda")
write.csv(param600, file="param600csv.csv")

#300
seq.vec <- as.vector(seq(from=1, to=nrow(tmp)-300, by=2)) 
param <- data.frame(matrix(nrow = 10, ncol = ceiling((nrow(tmp)-300)/2)), 
                    row.names =crix_svcj$crix$parameters$parameter)
colnames(param) <- seq.vec

for (i in seq.vec) {
  count.tmp <- i+300
  crix_svcj.tmp <- svcj_model(prices$crix$price[i:count.tmp], N = 5000, n = 1000)
  param[,as.character(i)] <- crix_svcj.tmp$parameters$mean
  # colnames(param)[i] <- i
}
param300 <- param 
write.csv(param300, file = "param300csv.csv")
save(param300, file = "param300.rda")

#150
seq.vec <- as.vector(seq(from=1, to=nrow(tmp)-150, by=2)) 
param <- data.frame(matrix(nrow = 10, ncol = ceiling(nrow(tmp)-150)/2), 
                    row.names =crix_svcj$parameters$parameter)
colnames(param) <- seq.vec

for (i in seq.vec) {
  count.tmp <- i+150
  crix_svcj.tmp <- svcj_model(prices$crix$price[i:count.tmp], N = 5000, n = 1000)
  param[,as.character(i)] <- crix_svcj.tmp$parameters$mean
  # colnames(param)[i] <- i
}
param150 <- param
save(param150,file="param150.Rda")
write.csv(param150, file="param150csv.csv")

#####################################################
### merge data sets param150, param300, param600#####
#####################################################
load("param150.Rda")
load("param300.Rda")
load("param600.Rda")


date <- prices$crix$date
ind <- as.vector(seq(from=1, to=length(date), by=2)) #2325
date2 <- date[ind]

param150.t <- as.data.frame(t(param150))
rownames(param150.t) <- date2[1:nrow(param150.t)]
colnames(param150.t) <- paste(colnames(param150.t), "150", sep = ".")

param300.t <- as.data.frame(t(param300))
rownames(param300.t) <- date2[1:nrow(param300.t)]
colnames(param300.t) <- paste(colnames(param300.t), "300", sep = ".")

param600.t <- as.data.frame(t(param600))
rownames(param600.t) <- date2[1:nrow(param600.t)]
colnames(param600.t) <- paste(colnames(param600.t), "600", sep = ".")

#merge 150/300/600 
param.t.2 <- merge(param150.t, param300.t, by="row.names", all.x = T) 
rownames(param.t.2) <- param.t.2$Row.names
param.t.all <- merge(param.t.2, param600.t, by="row.names", all.x = T)
param.t.all <- param.t.all[, !duplicated(colnames(param.t.all))]
rownames(param.t.all) <- param.t.all$Row.names
param.t.all$Row.names <- as.Date(as.character(param.t.all$Row.names, "%Y-%m-%d"))
colnames(param.t.all)[1] <- "date"
save(param.t.all, file = "param_t_all.rda")


