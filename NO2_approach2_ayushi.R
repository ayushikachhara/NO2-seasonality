##### another approach for no2 seasonality corrections###
### seasonal factor effects vary across sites. so the seasonal adjustment can be calculated for each site individually.

### library #####
library(data.table)
library(forecast)
library(stlplus)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(RColorBrewer)

##### import files: #####
path <- "S:/kachharaa/NZTA NO2 seasonality/"
setwd(path)

no2.file <- read.csv("nationalNO2_2007to2016.csv")
mfE_file <- read.csv("MfE_classification_v3.csv")

unique(mfE_file$Site.Classification)

no2.long <- reshape(no2.file, direction="long", varying=list(names(no2.file)[3:15]), v.names="no2", 
                    idvar=c("Site.ID","Year"), timevar= "Month", times = names(no2.file)[3:15])


no2.long <- as.data.table(no2.long)


## add site classification:
no2.long <- merge(no2.long, mfE_file, by = "Site.ID", all = T)

## separating out annual means from the main dataset####
no2.averages <- no2.long[which(no2.long$Month == "Average"),]

no2.sites <- unique(no2.long$Site.ID)
no2.long <- no2.long[which(no2.long$Month != "Average"),]

no2.long <- no2.long[order(no2.long$Year),]


#### checking for trends in seasonality. 
boxplot(no2.long$no2~no2.long$Year, main = "NO2 from 2007 to 2016", col = "indianred")

### since the range of no2 values across site vary across the same range from year to year, 
###additive model is used for further analysis

months <- c("Jan", "Feb", "Mar", "Apr" , "May", "Jun", "Jul", 
            "Aug", "Sep", "Oct", "Nov", "Dec")

## factor conversion for calendar order ####
no2.long$Month_f<-  factor(no2.long$Month, 
                             levels=months)
#### split file sitewise:
no2.list <- split(no2.long, no2.long$Site.ID)

site.count <- sapply(no2.list, NROW)

# PDFfile <- paste0("S:/kachharaa/NZTA NO2 seasonality/ouputs/NO2 seasonality paper_outputs/SeasonalityPerSite.pdf")
# pdf(file=PDFfile, paper = "USr", width = 22)
#### seasnoal decomposition loop#####
for(i in 1:length(no2.list)) {
  no2.site <- no2.list[[i]]
  non.na <- length(no2.site$no2[which(!is.na(no2.site$no2))])
  if(nrow(no2.site) < 40) {
    
    print(paste(no2.site$Site.ID[1], "dataframe too small"))
    no2.site$seasonal <- NA
    no2.site$trend <- NA
    no2.site$remainder <- NA
    no2.site$no2.adj <- NA
    
  } else {
    
    ### creating a time series object: 
    no2.ts <- ts(no2.site$no2, start=c(min(no2.site$Year), 1), 
                 end=c(max(no2.site$Year), 12), frequency=12) 
    #plot(no2.ts)
   
    ### clean for outliers and interpolate for missing values:
    no2.ts <- tsclean(no2.ts)
    
    #plot(no2.ts)
    ## robust = T meaning the seasonality calculations are "robust" to outliers - median is used, not mean. 
    fit <- stl(no2.ts, s.window = "period", t.window = 12, robust = T)
    
    plot(fit, main = paste("Site.ID =",no2.site$Site.ID[1],"|| SiteName = ",no2.site$SiteName[1],
                           "|| Monitoring Zone = ", no2.site$MonitoringZone[1],
                           "|| Site Classification = ", no2.site$Site.Classification[1]))
    
  
    #### add it to the main dataframe
    no2.site$seasonal <- melt(fit$time.series[,1])
    no2.site$trend <- melt(fit$time.series[,2])
    no2.site$remainder <- melt(fit$time.series[,3])
    no2.site$no2.adj <- melt(seasadj(fit))

    #### seasonally adjusted = trend + remainder
    #### noise = remainder
    ##### trend = seasonal - noise
  }
  
  no2.list[[i]] <- no2.site
  #print(i)
}

# dev.off()

plot(no2.ts)

# PDFfile <- paste0("S:/kachharaa/NZTA NO2 seasonality/ouputs/NO2 seasonality paper_outputs/Trends_persite_firstyear_removed.pdf")
# pdf(file=PDFfile, paper = "USr", width = 20)

trendparam <- data.frame(Site.ID = NA, 
                         Site.Classification = NA,
                         Slope = NA,
                         Rsq = NA)

for( i in 1:length(no2.list)) {
  plot.site <- no2.list[[i]]
  plot.site $date <- paste0("01-",plot.site $Month,"-",plot.site $Year)
  plot.site $date <- as.Date(plot.site $date, format = "%d-%b-%Y")
  plot.site$weights <- ifelse(plot.site$Year == min(plot.site$Year, na.rm = T), 0,1)

  if(is.na(all(plot.site$trend))) {
    print("All NAs")
  } else {

    # p1 <-  ggplot(plot.site, aes(x = date, y = trend)) +
    #   geom_point(size = 1) +
    #   scale_y_continuous(limit = c(0,60), breaks = seq(0,55,5), name ="NO2 - Trends")+
    #   scale_x_date(date_breaks = "1 year",
    #                limits = as.Date(c('2006-01-01','2017-01-01')),
    #                date_labels = "%Y")+
    #   ggtitle(paste("NO2 Trends for Site ID: ", plot.site$Site.ID,
    #                 "Site Classification:", plot.site$Site.Classification)) +
    #   geom_smooth(method = "lm",formula = y~x, se = F, aes(weight = weights)) +
    #   theme_bw()
    # 
    # print(p1)
    
    fit <- lm(trend~date,data = plot.site[which(plot.site$weights == 1),])
    
    param.t <- data.frame(Site.ID = plot.site$Site.ID[1],
                          Site.Classification = plot.site$Site.Classification[1],
                          Slope = fit$coefficients[[2]],
                          Rsq = summary(fit)$adj.r.squared)
    trendparam <- rbind(trendparam,param.t)
    
    }
}


boxplot(trendparam$Slope~trendparam$Site.Classification)
# dev.off()

# write.csv(trendparam, "S:/kachharaa/NZTA NO2 seasonality/ouputs/NO2 seasonality paper_outputs/trend_slopes.csv")



#### combine all sites again:
no2.long <- rbind_list(no2.list)
## removing NAs for ease of calculation - only sites where seasonal decomposition didn't happen.
no2.long <- no2.long[complete.cases(no2.long$seasonal),]

no2.long$date <- paste0("01-",no2.long$Month,"-",no2.long$Year)
no2.long$date <- as.Date(no2.long$date, format = "%d-%b-%Y")

trendparam <- trendparam[c("Site.ID", "Slope", "Rsq")]

no2.long <- merge(no2.long, trendparam, by = "Site.ID")


## Range of middle 50% of the data: 
IQR.aggregates <- group_by(no2.long,Site.ID) %>%
  dplyr::summarise(SiteName = unique(SiteName),
                   Site.Classification = unique(Site.Classification),
                   Region = unique(Region),
                   Start.Year = min(Year, na.rm = T),
                   End.Year = max(Year, na.rm = T),
                   mean.no2 = mean(no2,na.rm = T),
                   mean.seas = mean(seasonal,na.rm = T),
                   mean.remainder = mean(remainder,na.rm = T),
                   trendslope = mean(Slope, na.rm = T),
                   IQR.no2 = IQR(no2, na.rm = T),
                   IQR.seasonal= IQR(seasonal, na.rm = T),
                   IQR.trend= IQR(trend, na.rm = T),
                   IQR.residual= IQR(remainder, na.rm = T),
                   IQR.no2.adj = IQR(no2.adj, na.rm = T),
                   R.no2 = max(no2, na.rm = T) - min(no2, na.rm = T),
                   R.seasonal= max(seasonal, na.rm = T)-min(seasonal, na.rm = T),
                   R.trend= max(trend, na.rm = T) - min(trend, na.rm = T),
                   R.residual= max(remainder, na.rm = T) - min(remainder, na.rm = T),
                   R.no2.adj = max(no2.adj, na.rm = T) - min(no2.adj, na.rm = T),
                   amp.seasonal = max(seasonal, na.rm = T) - mean(seasonal, na.rm = T),
                   amp.trend = max(trend, na.rm = T) - mean(trend, na.rm = T),
                   amp.residual = max(remainder, na.rm = T) - mean(remainder, na.rm = T),
                   count = n())

# write.csv(IQR.aggregates,"S:/kachharaa/NZTA NO2 seasonality/ouputs/NO2 seasonality paper_outputs/Variability in NO2 results.csv")


### seasonal variability versus long term mean NO2 ###
ggplot(IQR.aggregates[which(!is.na(IQR.aggregates$Site.Classification) & 
                              is.finite(IQR.aggregates$IQR.seasonal)),],
       aes(mean.no2, R.seasonal)) + 
  geom_point(aes(mean.no2, R.seasonal, 
                 shape = Site.Classification, color = Site.Classification), size = 2)+ 
  geom_smooth(aes(mean.no2, R.seasonal, 
                  color = Site.Classification), method = "lm", se = F)+ 
  ggtitle("Seasonality Range versus the long-term mean NO2 at each site")+
  scale_y_continuous(name = "Range of variability in seasonality (ug/m3)", breaks = seq(0,30,2), limits = c(0,30))+
  scale_x_continuous(name = "Long term mean NO2 per site (ug/m3)", breaks = seq(0,50,5), limits = c(0,50))+
  theme_bw() +
  theme(axis.text = element_text(size = 13), plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm"),
        axis.title = element_text(size = 13))

### seasonal amplitude versus long term mean NO2 ###
ggplot(IQR.aggregates[which(!is.na(IQR.aggregates$Site.Classification) & 
                              is.finite(IQR.aggregates$IQR.seasonal)),],
       aes(mean.no2, amp.seasonal)) + 
  geom_point(aes(mean.no2, amp.seasonal, 
                 shape = Site.Classification, color = Site.Classification), size = 3)+ 
  geom_smooth(aes(mean.no2, amp.seasonal, 
                  color = Site.Classification), method = "lm", se = F)+ 
  ggtitle("Seasonal Amplitude versus the long-term mean NO2 at each site")+
  scale_y_continuous(name = "Amplitude of seasonality (ug/m3)", breaks = seq(0,20,2), limits = c(0,20))+
  scale_x_continuous(name = "Long term mean NO2 per site (ug/m3)",breaks = seq(0,50,5), limits = c(0,50))+
  theme_bw() +
  theme(axis.text = element_text(size = 13), plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm"),
        axis.title = element_text(size = 13))

### Trend variability versus long term mean NO2 ###
ggplot(IQR.aggregates[which(!is.na(IQR.aggregates$Site.Classification)),],
       aes(mean.no2, trendslope)) + 
  geom_point(aes(mean.no2, trendslope, 
                 shape = Site.Classification, color = Site.Classification), size = 3)+ 
  geom_smooth(aes(mean.no2, trendslope, 
                  color = Site.Classification), method = "lm", se = F)+ 
  ggtitle("Trend Range versus the long-term mean NO2 at each site")+
  scale_y_continuous(name = "Trends of NO2 across sites")+
  scale_x_continuous(name = "Long term mean NO2 per site (ug/m3)", breaks = seq(0,50,5), limits = c(0,50))+
  theme_bw() +
  theme(axis.text = element_text(size = 13), plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm"),
        axis.title = element_text(size = 13))


## histogram of trend slopes

ggplot(IQR.aggregates) +
  geom_histogram(aes(trendslope), bins = 30, color = "black", fill = "white" ) + theme_bw() +
  annotate("text",
           x = 0, y = 20,
           label = paste("Mean = ", round(mean(IQR.aggregates$trendslope, na.rm = T),3),
                         "\n Standard Deviation =", round(sd(IQR.aggregates$trendslope, na.rm = T),3)))+
  ggtitle("Slope of Trend in NO2 across all sites")+
  theme(plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm"))

# no2.long <- merge(no2.long, IQR.aggregates, by = c("Site.ID", "SiteName",
#                                                    "Site.Classification","Region"), all = T)



##normalised trend range

#no2.2010 <- no2.long[which(no2.long$Year == 2011),]
myPalette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))

no2.long$Site.Classification <-  factor(no2.long$Site.Classification, c("Urban Background", "Roadside", "Peak"))
boxplot(no2.long$seasonal~no2.long$Site.Classification, col = c("steelblue2", "steelblue3","steelblue"),
        main = "Range of variability in seasonal term")



########## what is the variation in seasonality every year?
myPalette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))

par(cex.lab=1.5, cex.axis = 1, mar = c(8,8,4,2))
boxplot(no2.long$no2~no2.long$Month_f, outline =F, col = "steelblue", ylab = expression('Observed NO'[2]*'(ug/m'^3*')'))
boxplot(no2.long$no2.adj~no2.long$Month_f, outline =F, main = "NO2 seasonally adjusted using additive model", col = "indianred")
boxplot(no2.long$remainder~no2.long$Month_f, outline =F)
hist(no2.long$remainder,40)
sd(no2.long$remainder, na.rm = T)
mean(no2.long$remainder, na.rm = T)
boxplot(no2.long$amp.residual~no2.long$Month_f, outline = F)
boxplot(no2.long$no2.adj~no2.long$Year, outline =F)
boxplot(no2.long$remainder~no2.long$Year, outline =F)
#write.csv(monthly.seasonal, "Seasonality_approach2.csv")

p1 <- ggplot(no2.long) +
  geom_histogram(aes(remainder), bins = 30, color = "black", fill = "white" ) + theme_bw() +
  annotate("text",
           x = 10, y = 4000,
           label = paste("Mean = ", round(mean(no2.long$remainder, na.rm = T),3),
                         "\n Standard Deviation =", round(sd(no2.long$remainder, na.rm = T),3)))+
  ggtitle("Remainder across all sites")+
  theme(plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm"))

p1

monthly.data <- split(no2.long, no2.long$Month_f)
p <- list()

for(i in 1:length(monthly.data)) {
  cur.month <- monthly.data[[i]]
  p[[i]] <- ggplot(cur.month) +
    geom_histogram(aes(remainder), bins = 30, color = "black", fill = "white" ) + theme_bw() +
    scale_x_continuous(limits = c(-10,10))+
    scale_y_continuous(limits = c(0,300))+
    annotate("text",
             x = 7, y = 200,
             label = paste("Mean = ", round(mean(cur.month$remainder, na.rm = T),3),
                           "\n SD =", round(sd(cur.month$remainder, na.rm = T),3)))+
    ggtitle(paste0(unique(cur.month$Month_f),""))+
    theme(axis.title = element_text(size = 14), 
          axis.text = element_text(size = 14),
          title = element_text(size = 14))

}

gridExtra::grid.arrange(grobs = p, width = c(2,2,2))


###########

class.data <- split(no2.long, no2.long$Site.Classification)
clist <- list()

for(i in 1:length(class.data)) {
  cur.class <- class.data[[i]]
  clist[[i]] <- ggplot(cur.month) +
    geom_histogram(aes(remainder), bins = 30, color = "black", fill = "white" ) + theme_bw() +
    scale_x_continuous(limits = c(-10,10))+
    scale_y_continuous(limits = c(0,300))+
    annotate("text",
             x = 7, y = 200,
             label = paste("Mean = ", round(mean(cur.class$remainder, na.rm = T),3),
                           "\n SD =", round(sd(cur.class$remainder, na.rm = T),3)))+
    ggtitle(paste0(unique(cur.class$Site.Classification),""))+
    theme(axis.title = element_text(size = 14), 
          axis.text = element_text(size = 14),
          title = element_text(size = 20))
  
}

gridExtra::grid.arrange(grobs = clist, height = c(0.5,0,1))




###########################################################################################
###########################################################################################
##### Cluster Analysis: #####

library("cluster")
library("factoextra")
library("magrittr")





### seasonal table for every month every site.
seasonal <- no2.long %>% group_by(Site.ID, Month_f) %>%
  summarise(seasonal = mean(no2.adj,na.rm = T)) %>% drop_na()

seasonal3 <- spread(seasonal, key = Month_f, value = seasonal)
#seasonal3$mean.no2 <- ifelse(seasonal3$Site.ID %in% IQR.aggregates$Site.ID, IQR.aggregates$mean.no2, NA)
seasonal3 <- seasonal3 %>% drop_na()
seasonal3 <- seasonal3[-1]
seasonal3$seas.site <- rowMeans(seasonal3)
rownames(seasonal3) <- unique(seasonal$Site.ID)

seasonal3 <- scale(seasonal3)



# Compute hierarchical clustering
res.hc <- seasonal3 %>%    
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering
# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


gradient.color <- list(low = "steelblue",  high = "white")
seasonal3 %>%     # Scale variables
  get_clust_tendency(n = 50, gradient = gradient.color)



res.dist <- get_dist(seasonal3)
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), 
          lab_size = 6)

seasonal4 <- pam(seasonal3,5)
fviz_cluster(seasonal4) 

km.res <- kmeans(seasonal3, 2, 
                 iter.max = 50000,
                 nstart = 10)

### cluster stats: #####
print(km.res)

str(km.res)
fviz_cluster(km.res, data = seasonal3,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())



### are the clusters statistically significant? Using paired t-test to evaluate the p-value. 

seasonal3 <- seasonal3 %>%
  as_tibble() %>%
  mutate(cluster =as.factor(km.res$cluster),
         Site.ID = row.names(seasonal3))

### paired t-test. 
## Gives the mean and standard error per month. 
## Checking if the mean of one cluster is different from the other cluster for each month
library(statsr)
inference(y =seas.site, x = cluster, data = seasonal3, statistic = "mean", 
          type = "ht", null = 0, conf_level = 0.95, 
          alternative = "twosided", method = "theoretical")

#### what is the ranking of each cluster in each month? 
#### Which cluster has the highest mean in a particular month?
cluster1<-colMeans(seasonal3[which(seasonal3$cluster == "1"),c(1:13)])
cluster2<-colMeans(seasonal3[which(seasonal3$cluster == "2"),c(1:13)])
#cluster3<-colMeans(seasonal3[which(seasonal3$cluster == "3"),c(1:13)])
rank.cluster<-cbind(cluster1,cluster2)

rank.cluster2 <- apply(rank.cluster,1,function(x) rank(-x))


######## cluster table characteristics. #####
cluster.centers <- as.data.frame(km.res$centers, row.names = F)
cluster.centers$Cluster.ID <- c(1:2)
cluster.table <- gather(cluster.centers,Month_f,cluster.center, -Cluster.ID)
cluster.class <- cbind.data.frame(Site.ID = unique(seasonal$Site.ID), 
                                  Cluster.ID = km.res$cluster, row.names = NULL)

cluster.char <- merge(cluster.table,cluster.class, by = "Cluster.ID", all = T)


#cluster1$Cluster.ID <- ifelse(cluster1$Cluster.ID ==1, 1, 2)
cluster.no2 <- merge(no2.long, cluster.char, 
                     by = c("Site.ID", "Month_f"), all = T)

# cluster1 <- cluster1[which(cluster1$Cluster.ID == 1),]
#
# ggplot(cluster1, aes(mean.no2,IQR.seasonal)) +
#   geom_text(aes(label = Region, color = factor(Site.Classification))) + theme_bw()

count.cluster <- cluster.no2 %>% 
  group_by(Cluster.ID, Region, Site.Classification) %>%
  summarise(count.cl = length(unique(Site.ID)),
            Cluster.Sites = list(as.character(unique(Site.ID))))

count.all <- cluster.no2 %>% 
  group_by(Region, Site.Classification) %>%
  summarise(count.all = length(unique(Site.ID)))


count.cluster1 <- merge(count.cluster, count.all, by = c("Region","Site.Classification"), all = T)
count.cluster1$proportion <- round(count.cluster1$count.cl/count.cluster1$count.all,3)

myPalette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))
p1 <- ggplot(count.cluster1) +
  geom_tile(aes(Site.Classification,Region, fill = proportion), color = "black") + theme_bw()

p1 + facet_grid(.~Cluster.ID) + 
  scale_fill_gradientn(colours = rev(myPalette(20)), limits=c(0,1))+
  ggtitle("Proportion of total sites in a region with a given site classification in each cluster")+
  theme(axis.text.x = element_text(angle = 30))


##### take a random sample from no2.long ans see if it assigns the cluster appropriately.
set.seed(1)
no2.train <- cbind.data.frame(SiteName = no2.long$SiteName,
                              Site.Classification = no2.long$Site.Classification, 
                              Region = no2.long$Region, 
                              Month_f = no2.long$Month, 
                              no2 = no2.long$no2,
                              no2.adj = no2.long$no2.adj)


sample1 <- sample_n(no2.train, 200)

# sample1$Cluster.ID <- ifelse((sample1$Site.Classification %in% count.cluster1$Site.Classification) &
#                             (sample1$Region %in% count.cluster1$Region), 
#                           count.cluster1$Cluster.ID, NA)
sample1 <- merge(sample1, count.cluster1, by = c("Region", "Site.Classification"))
sample1 <- merge(sample1, cluster.table, by = c("Month_f", "Cluster.ID"))

###reorder
sample1 <- sample1[c(5,4,3,1,6,7,2,8,10,11,12)]
sample1$no2.model.adj <- sample1$no2 + sample1$cluster.center
sample1$err <- sample1$no2.adj + sample1$no2.model.adj
plot(sample1$no2.adj,sample1$err, ylim = c(-7,7))
abline(0,0)
with(sample1, text(as.numeric(no2.adj - no2.model.adj)~no2.adj, 
                   labels = paste(Cluster.ID), pos = 2))


###function for rolling calculations:
############cumulative per site ##############
# 
# results.z <- data.frame(Site.ID = NA,
#                         Site.Classification = NA,
#                         Month = NA,
#                         Year = NA,
#                         cummean.len = NA,
#                         cummean.ID = NA,
#                         modno2 = NA,
#                         cummean.modno2 = NA)
# 
# cluster.list <- split(cluster.no2, cluster.no2$Site.ID)
# for(i in 1: length(cluster.list)) {
#   f1 <-cluster.list[[i]]
#   max.length <- nrow(f1)
#   start.point <- 1
#   #ann.site <- no2.annual[which(no2.annual$Site.ID %in% f1$Site.ID),]
#   if(max.length>0) {
#     for(start.point in 1:(max.length-1)) {
#       cumulative <- cummean(f1$no2.adj[start.point:max.length])
#       #plot(cumulative, type ="p")
#       rep.time <- length(cumulative)
#       results.table <- data.frame(Site.ID = rep(f1$Site.ID[1],rep.time),
#                                   Site.Classification = rep(f1$Site.Classification[1],rep.time),
#                                   Month = f1$Month[start.point:max.length],
#                                   Year = f1$Year[start.point:max.length],
#                                   cummean.len = c(1:rep.time),
#                                   cummean.ID = rep(start.point,rep.time),
#                                   modno2 = f1$no2.adj[start.point:max.length],
#                                   cummean.modno2 = cumulative)
#       results.z <- rbind.data.frame(results.z, results.table)
#       #print(paste("Start Point:",start.point))
#     }
#     
#   } else {
#     paste("No Data Recorded")
#   }
#   
#   # results.z$date <- paste0("01-",results.z$Month,"-",results.z$Year)
#   # results.z$date <- as.Date(results.z$date, format = "%d-%b-%Y")
#   # 
#   print(paste("Site.no:",i, "done"))  
#   
# }
# ## remove the NA row inserted before the loop.
# results.z <-  results.z[-1,]

load("CumulativeNO2.RData")

ggplot(results.z) + 
  geom_point(aes(cummean.len, cummean.modno2, color = Site.Classification), size = 0.5) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0,120,2))



#### variance in the cummean per site for all lengths. ####
res.vec <- results.z %>% group_by(Site.ID,cummean.len) %>%
  summarise(max.no2 = max(cummean.modno2, na.rm = T),
            Site.Classification = unique(Site.Classification),
            mean.no2 = mean(cummean.modno2, na.rm = T),
            min.no2 = min(cummean.modno2, na.rm = T),
            range.no2 = max(cummean.modno2, na.rm = T) - min(cummean.modno2, na.rm = T),
            IQR.no2 = IQR(cummean.modno2, na.rm = T),
            var.no2 = var(cummean.modno2, na.rm = T),
            sd.no2 = sd(cummean.modno2,na.rm = T))

res.subset <- res.vec[which(res.vec$cummean.len<=48),]


ggplot(res.subset[which(!is.na(res.subset$var.no2)),]) +
  geom_raster(aes(cummean.len,Site.ID, fill = sd.no2)) +theme_bw()+
  scale_fill_gradient(low = "whitesmoke",high = "indianred", breaks = seq(0,6,1)) +
  scale_x_continuous(breaks = seq(0,60,1), name = "Number of months")+
  theme(axis.text.y = element_text(angle = 0,size = 6), 
        legend.title = element_blank())

res.vec12 <- res.vec[which(res.vec$cummean.len == 12),]
hist(res.vec12$sd.no2,50, main = "Averaged over 12 months of data", xlab = "Standard Error", col = "indianred")
mean(res.vec12$sd.no2, na.rm = T)
IQR(res.vec12$IQR.no2, na.rm = T)


#res.vec12$IQR.no2 <- round(res.vec12$IQR.no2,2)
res.vec12$range.classification <- ifelse(res.vec12$range.no2>=5, ">5","[2,5]")
res.vec12$range.classification <- ifelse(res.vec12$range.no2<=2, "var <2",res.vec12$range.classification)



frequency.table <- table(res.vec12$Site.Classification, res.vec12$range.classification)
frequency.table <- addmargins(frequency.table)
frequency.table
frequency.table <- as.data.frame.matrix(frequency.table) 
frequency.table <- frequency.table[c(3,1,2,4)]
frequency.table

#res.vec$var.classification <- ifelse(res.vec$Site.ID %in% res.vec12$Site.ID, res.vec12$var.classification,"")

ggplot(res.vec[1:1200,]) + theme_bw() +
  geom_line(aes(cummean.len, max.no2, color = Site.ID)) +
  geom_line(aes(cummean.len, min.no2, color = Site.ID)) + 
  scale_x_continuous(breaks = seq(0,120,6), name = "number of months")+
  scale_y_continuous(breaks = seq(0,60,3),name = "cumulative no2.adj (maximum and minimum)")




p1 <- ggplot(res.vec[which(!is.na(res.vec$Site.ID)),],aes(cummean.len, var.no2)) + theme_bw() +
  geom_line(aes(cummean.len, sd.no2, group = Site.ID), color = "darkgreen") +
  scale_x_continuous(breaks = seq(0,120,6), name = "number of months")+
  scale_y_continuous(breaks = seq(0,6,1),name = "Standard Deviation")


p1 
### Site.ID versus no. of months matrix ####
res.vec2 <- res.vec[c(1,2,5)]
res.vec3 <- spread(res.vec2, key = cummean.len, value = var.no2)


####how many months we need before annual mean converges? 
## step 1: calculate annual average for each site based on this clustering method: ####

### seasonal value per month per cluster ####
seasonal.averages <- aggregate(cluster.no2$seasonal,
                               by = list(cluster.no2$Month_f,cluster.no2$Cluster.ID),
                               FUN = function(x) mean(x, na.rm = T))

colnames(seasonal.averages) <- c("Month", "Cluster.ID", "cl.seasonal")
seasonal.averages <- seasonal.averages[which(seasonal.averages$Month != "seas.site"),]
cluster.no2$cl.seasonal <- ifelse((cluster.no2$Month %in% seasonal.averages$Month)&
                                    (cluster.no2$Cluster.ID %in% seasonal.averages$Cluster.ID),
                                  seasonal.averages$cl.seasonal, NA)
cluster.no2 <- cluster.no2[which(cluster.no2$Month != "seas.site"),]

### no2 adjusted based on the calculated seasonal value ####
cluster.no2$modno2 <- cluster.no2$no2 + cluster.no2$cl.seasonal

### annual mean based on modelled no2 (seasonally adjusted) ####
no2.annual <- aggregate(cluster.no2$modno2,
                        by = list(cluster.no2$Site.ID, cluster.no2$Year),
                        FUN = function(x) mean(x, na.rm = T))
colnames(no2.annual) <- c("Site.ID", "Year", "annual.calc")


no2.annual <- merge(no2.annual, no2.averages, by = c("Site.ID","Year"), all = T)
no2.annual <- no2.annual[which(!is.na(no2.annual$annual.calc)),]

fit <- lm(annual.calc~ 0 + no2, data = no2.annual)

no2.annual$res.seas <- scale(fit$residuals)

ggplot(no2.annual, aes(no2,annual.calc)) +
  geom_point()+ geom_smooth(method = "lm", formula = y~0+x) +theme_bw()+
  scale_y_continuous(breaks = seq(0,60,4), name = "Calculated NO2")+
  scale_x_continuous(breaks = seq(0,60,4), name = "Observed NO2")+
  annotate("text",
           x = 40, y = 10,
           label = (paste0("Adj R2 = ",signif( summary(fit)$r.sq, 3),
                           "\n y = ",signif(fit$coef[[1]], 3),"x")))

ggplot(no2.annual) +
  geom_point(aes(Year,annual.calc, color =Site.ID))+
  geom_line(aes(Year,annual.calc, color =Site.ID)) + theme_bw()+
  theme(legend.position = "none")

### percentage change per region: ####

change.reg <- no2.annual %>% group_by(Site.ID, Year) %>%
  summarise(mean.no2 = mean(no2, na.rm = T))


change.reg2 <- aggregate(change.reg$mean.no2, 
                         by = list(change.reg$Site.ID), 
                         FUN = function(x) (mean(diff(x, na.rm = T))))

ggplot(change.reg) +
  geom_tile(aes(Site.ID, Year, fill = mean.no2), color = "black") + 
  scale_fill_gradient(low = "darkblue",high = "indianred", breaks = seq(10,40,5)) + 
  scale_y_reverse(breaks = seq(2006,2016,1))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))
