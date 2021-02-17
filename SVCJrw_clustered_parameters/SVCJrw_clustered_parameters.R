library(RColorBrewer)
library(ggplot2)

#read data
cluster_data <- read.csv("/Users/konstantin/Desktop/dateien/IRTG 1792/econCRIX/svcj/svcj/clustering_data.csv")
cluster_scaled <- scale(cluster_data[ , c(2:5)])

#define colors
display.brewer.all()
colors <- brewer.pal(3, "Paired")
names(colors) <- levels(cluster_scaled$cluster_mu_alpha)
custom_colors <- scale_colour_manual(name = "Cluster #", values = colors)  

#elbow method to decide on optimal number of clusters

# Function to compute total within-cluster sum of square
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

# plotting values for each cluster starting from 1 to 9
par(mfrow = c(1,1))
wssplot(cluster_scaled[,c("mu", "beta")], nc = 9)

#compute clusters
mu_beta <- kmeans(cluster_data[,c("mu", "beta")], centers = 4,
                  iter.max = 10,
                  nstart = 25)

##mu beta
ggplot(data=cluster_scaled, aes(x=beta, y=mu, color=cluster_mu_beta))+
  geom_point(size=2.2)+ #shape=cluster_scaled$cluster_mu_alpha, stroke=2
  custom_colors +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot(data=cluster_scaled, aes(x=date, y=crix, color=cluster_mu_beta))+
  geom_point()+
  custom_colors +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#--------------------------------------------------------------------------------------------------
# movie of parameter interactions
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(gifski)
library(magick)
library(viridis)

#load data
load(file = "gif_data.Rda")
s <- na.omit(s)

#plot with "transition reveal"
gif1 <-  s %>% 
  ggplot(aes(x=s.alpha.150, y=s.sigma_v.150))  +
  geom_line(colour = 'grey') +
  geom_point() +
  theme_ipsum() +
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  transition_reveal(as.Date(date)) +
  ggtitle("Date: {frame_along}")

#save .gif
animate(gif1, renderer = gifski_renderer("alpha_sigma_v.gif"), bg = "transparent",
        nframes = 100)
#save all frames
animate(gif1, nframes = 100, device = "png",
        renderer = file_renderer("/Users/konstantin/Desktop/dateien/IRTG 1792/econCRIX/talk/gganim", 
                                 prefix = "gganim_mu_v_sigma_v", overwrite = TRUE))

