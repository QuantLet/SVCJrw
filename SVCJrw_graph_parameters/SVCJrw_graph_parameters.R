##################################
### Empirical Results ############
##################################


#load SVCJ estimates. note: estimates at time t were obtained from interval [t, t+n]. Therefore, 
#in the next step, they are shifted towards the end of the interval. 
load("param_t_all.rda")


#shift estimates (from the left to the right end of the estimation interval). Estimates are obtained for 
# every second day, that's why they are shifted by n/2.
var.list <- c("mu", "mu_y", "sigma_y", "lambda", "alpha", "beta", "rho","sigma_v", "rho_j", "mu_v")
param.t.all[paste0(var.list, ".150")] <- lapply(param.t.all[paste0(var.list, ".150")], function(x)lag(x,75))
param.t.all[paste0(var.list, ".300")] <- lapply(param.t.all[paste0(var.list, ".300")], function(x)lag(x,150))
param.t.all[paste0(var.list, ".600")] <- lapply(param.t.all[paste0(var.list, ".600")], function(x)lag(x,300))

#weights for moving average
f20 <- rep(1/20, 20) #MA of 20 days

#plot results
par(mfrow = c(2,2), mar = c(2,4,2,1), bg="transparent")

#elegant way (but wihtout greek-latex-style y-labels)
for (i in var.list) {
  #calculate moving averages (ma) for each window size
  ma.tmp1 <-  stats::filter(param.t.all[,paste(i, ".150", sep = "")],filter = f20, method = "convolution", sides = 2)
  ma.tmp2 <-  stats::filter(param.t.all[,paste(i, ".300", sep = "")],filter = f20, method = "convolution", sides = 2)
  ma.tmp3 <-  stats::filter(param.t.all[,paste(i, ".600", sep = "")],filter = f20, method = "convolution", sides = 2)
  #plot ma's
  plot(ma.tmp1 ~param.t.all$date,  xaxt='n',
       type="l", ylab=i, xlab="", col= "#FD8D3C",
       ylim=c(min(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T), max(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T)))
  axis.Date(1, param.t.all$date)
  lines(ma.tmp2 ~param.t.all$date,  type="l", col= "#9E9AC8")
  lines(ma.tmp3 ~param.t.all$date,  type="l", col= "#1B9E77", pch=18)
}

##########################################################################################################
#non-elegant way (but with greek-latex-style y-labels):
#"mu" 
ma.tmp1 <-  stats::filter(param.t.all[,paste("mu", ".150", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp2 <-  stats::filter(param.t.all[,paste("mu" , ".300", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp3 <-  stats::filter(param.t.all[,paste("mu" , ".600", sep = "")],filter = f20, method = "convolution", sides = 2)
#plot ma's
plot(ma.tmp1 ~param.t.all$date,  xaxt='n',
     type="l", 
     ylab=expression(mu), 
     xlab="", col= "#FD8D3C",
     ylim=c(min(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T), max(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T)))
axis.Date(1, param.t.all$date)
lines(ma.tmp2 ~param.t.all$date,  type="l", col= "#9E9AC8")
lines(ma.tmp3 ~param.t.all$date,  type="l", col= "#1B9E77", pch=18)
#"mu_y"  
ma.tmp1 <-  stats::filter(param.t.all[,paste("mu_y", ".150", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp2 <-  stats::filter(param.t.all[,paste("mu_y" , ".300", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp3 <-  stats::filter(param.t.all[,paste("mu_y" , ".600", sep = "")],filter = f20, method = "convolution", sides = 2)
#plot ma's
plot(ma.tmp1 ~param.t.all$date,  xaxt='n',
     type="l", 
     ylab=expression(mu[y]), 
     xlab="", col= "#FD8D3C",
     ylim=c(min(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T), max(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T)))
axis.Date(1, param.t.all$date)
lines(ma.tmp2 ~param.t.all$date,  type="l", col= "#9E9AC8")
lines(ma.tmp3 ~param.t.all$date,  type="l", col= "#1B9E77", pch=18)

# "sigma_y"
ma.tmp1 <-  stats::filter(param.t.all[,paste("sigma_y", ".150", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp2 <-  stats::filter(param.t.all[,paste("sigma_y" , ".300", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp3 <-  stats::filter(param.t.all[,paste("sigma_y" , ".600", sep = "")],filter = f20, method = "convolution", sides = 2)
#plot ma's
plot(ma.tmp1 ~param.t.all$date,  xaxt='n',
     type="l", 
     ylab=expression(sigma[y]), 
     xlab="", col= "#FD8D3C",
     ylim=c(min(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T), max(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T)))
axis.Date(1, param.t.all$date)
lines(ma.tmp2 ~param.t.all$date,  type="l", col= "#9E9AC8")
lines(ma.tmp3 ~param.t.all$date,  type="l", col= "#1B9E77", pch=18)

#"lambda"  
ma.tmp1 <-  stats::filter(param.t.all[,paste("lambda", ".150", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp2 <-  stats::filter(param.t.all[,paste("lambda", ".300", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp3 <-  stats::filter(param.t.all[,paste("lambda", ".600", sep = "")],filter = f20, method = "convolution", sides = 2)
#plot ma's
plot(ma.tmp1 ~param.t.all$date,  xaxt='n',
     type="l", 
     ylab=expression(lambda), 
     xlab="", col= "#FD8D3C",
     ylim=c(min(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T), max(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T)))
axis.Date(1, param.t.all$date)
lines(ma.tmp2 ~param.t.all$date,  type="l", col= "#9E9AC8")
lines(ma.tmp3 ~param.t.all$date,  type="l", col= "#1B9E77", pch=18)

#"alpha" 
ma.tmp1 <-  stats::filter(param.t.all[,paste("alpha", ".150", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp2 <-  stats::filter(param.t.all[,paste("alpha", ".300", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp3 <-  stats::filter(param.t.all[,paste("alpha", ".600", sep = "")],filter = f20, method = "convolution", sides = 2)
#plot ma's
plot(ma.tmp1 ~param.t.all$date,  xaxt='n',
     type="l", 
     ylab=expression(alpha), 
     xlab="", col= "#FD8D3C",
     ylim=c(min(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T), max(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T)))
axis.Date(1, param.t.all$date)
lines(ma.tmp2 ~param.t.all$date,  type="l", col= "#9E9AC8")
lines(ma.tmp3 ~param.t.all$date,  type="l", col= "#1B9E77", pch=18)

#"beta"    
ma.tmp1 <-  stats::filter(param.t.all[,paste("beta" , ".150", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp2 <-  stats::filter(param.t.all[,paste("beta", ".300", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp3 <-  stats::filter(param.t.all[,paste("beta", ".600", sep = "")],filter = f20, method = "convolution", sides = 2)
#plot ma's
plot(ma.tmp1 ~param.t.all$date,  xaxt='n',
     type="l", 
     ylab=expression(beta), 
     xlab="", col= "#FD8D3C",
     ylim=c(min(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T), max(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T)))
axis.Date(1, param.t.all$date)
lines(ma.tmp2 ~param.t.all$date,  type="l", col= "#9E9AC8")
lines(ma.tmp3 ~param.t.all$date,  type="l", col= "#1B9E77", pch=18)

#"rho"     
ma.tmp1 <-  stats::filter(param.t.all[,paste("rho", ".150", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp2 <-  stats::filter(param.t.all[,paste("rho", ".300", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp3 <-  stats::filter(param.t.all[,paste("rho", ".600", sep = "")],filter = f20, method = "convolution", sides = 2)
#plot ma's
plot(ma.tmp1 ~param.t.all$date,  xaxt='n',
     type="l", 
     ylab=expression(rho), 
     xlab="", col= "#FD8D3C",
     ylim=c(min(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T), max(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T)))
axis.Date(1, param.t.all$date)
lines(ma.tmp2 ~param.t.all$date,  type="l", col= "#9E9AC8")
lines(ma.tmp3 ~param.t.all$date,  type="l", col= "#1B9E77", pch=18)

#"sigma_v" 
ma.tmp1 <-  stats::filter(param.t.all[,paste("sigma_v", ".150", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp2 <-  stats::filter(param.t.all[,paste("sigma_v", ".300", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp3 <-  stats::filter(param.t.all[,paste("sigma_v", ".600", sep = "")],filter = f20, method = "convolution", sides = 2)
#plot ma's
plot(ma.tmp1 ~param.t.all$date,  xaxt='n',
     type="l", 
     ylab=expression(sigma[v]), 
     xlab="", col= "#FD8D3C",
     ylim=c(min(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T), max(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T)))
axis.Date(1, param.t.all$date)
lines(ma.tmp2 ~param.t.all$date,  type="l", col= "#9E9AC8")
lines(ma.tmp3 ~param.t.all$date,  type="l", col= "#1B9E77", pch=18)

#"rho_j"   
ma.tmp1 <-  stats::filter(param.t.all[,paste("rho_j", ".150", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp2 <-  stats::filter(param.t.all[,paste("rho_j", ".300", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp3 <-  stats::filter(param.t.all[,paste("rho_j", ".600", sep = "")],filter = f20, method = "convolution", sides = 2)
#plot ma's
plot(ma.tmp1 ~param.t.all$date,  xaxt='n',
     type="l", 
     ylab=expression(rho[j]), 
     xlab="", col= "#FD8D3C",
     ylim=c(min(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T), max(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T)))
axis.Date(1, param.t.all$date)
lines(ma.tmp2 ~param.t.all$date,  type="l", col= "#9E9AC8")
lines(ma.tmp3 ~param.t.all$date,  type="l", col= "#1B9E77", pch=18)

#"mu_v" 
ma.tmp1 <-  stats::filter(param.t.all[,paste("mu_v", ".150", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp2 <-  stats::filter(param.t.all[,paste("mu_v", ".300", sep = "")],filter = f20, method = "convolution", sides = 2)
ma.tmp3 <-  stats::filter(param.t.all[,paste("mu_v", ".600", sep = "")],filter = f20, method = "convolution", sides = 2)
#plot ma's
plot(ma.tmp1 ~param.t.all$date,  xaxt='n',
     type="l", 
     ylab=expression(mu[v]), 
     xlab="", col= "#FD8D3C",
     ylim=c(min(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T), max(c(ma.tmp1, ma.tmp2, ma.tmp3), na.rm = T)))
axis.Date(1, param.t.all$date)
lines(ma.tmp2 ~param.t.all$date,  type="l", col= "#9E9AC8")
lines(ma.tmp3 ~param.t.all$date,  type="l", col= "#1B9E77", pch=18)
