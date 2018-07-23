library(data.table)
library(zoo)
library(xts)
library(spatstat)
library(leaflet)
library(rgdal)
library(sp)
library(mapview)
library(reshape)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(plyr)
library(plotly)
library(tidyr)
library(mgcv)


######## ggplot function #######
# ggplotRegression <- function(fit, current.month) {
#   ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
#     geom_point() +
#     stat_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), n = summary(fit)$n, 
#                 col = "red", fullrange = F) +  theme_bw()+
#     scale_x_continuous(limits = c(0,60), 
#                        breaks = c(0,10,20,30,40,50,60)) +
#     scale_y_continuous(limits = c(0,60), 
#                        breaks = c(0,10,20,30,40,50,60)) +
#     ggtitle(paste(current.month, " -- NO2 seasonality:na-ignored")) + 
#     annotate("text",
#              x = 40, y = 10,
#              label = (paste0("Adj R2 = ",signif( summary(fit)$r.sq, 3),
#                              "\n y ~ s(x, bs = cs)",
#                              "\n Parametric Coeff estimate =", 
#                              signif(fit$coef[[1]], 3)))) +
#   xlab(paste(current.month,"NO2 / mg m-3")) + 
#     ylab("12 month rolling mean NO2 / mg m-3")
# }
#   
ggplotRegression2 <- function(fit, current.month) {
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", formula = y~0 + x, col = "red", fullrange = F) + theme_bw()+
    scale_x_continuous(limits = c(0,60), 
                       breaks = c(0,10,20,30,40,50,60)) +
    scale_y_continuous(limits = c(0,60), 
                       breaks = c(0,10,20,30,40,50,60)) +
    ggtitle(paste(current.month, " -- NO2 seasonality")) + 
    annotate("text",
             x = 40, y = 10,
             label = (paste0("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
                            "\ny = ",signif(fit$coef[[1]], 3), "x + 0"))) +
    xlab(paste(current.month,"NO2 / mg m-3")) + 
    ylab("12 month rolling mean NO2 / mg m-3") +
    theme(axis.title = element_text(size = 22), 
          axis.text = element_text(size = 22),
          title = element_text(size = 22))
}

#########constants, paths and imports #######

path <- "S:/kachharaa/NZTA NO2 seasonality/"
setwd(path)

no2.file <- read.csv("nationalNO2_2007to2016.csv")
# seasonal.adjustment <- read.csv("seasonal_adjustment2.csv")
mfE_file <- read.csv("MfE_classification_v3.csv")
#roadside_distances <- read.csv("NZTA roadside sites distances_corrected.csv")

## reshaping the data from wide to long.
no2.long <- reshape(no2.file, direction="long", varying=list(names(no2.file)[3:15]), v.names="no2", 
        idvar=c("Site.ID","Year"), timevar= "Month", times = names(no2.file)[3:15])


no2.long <- as.data.table(no2.long)

## separating out annual means from the main dataset####
no2.averages <- no2.long[which(no2.long$Month == "Average"),]

no2.sites <- unique(no2.long$Site.ID)
no2.long <- no2.long[which(no2.long$Month != "Average"),]

no2.long <- no2.long[order(no2.long$Year),]
  
#### split sitewise for rollmeans calculation #######  
no2.list <- split(no2.long, no2.long$Site.ID)
na.countpersite <- data.frame(site.ID = NA, count.na = NA)

for(i in 1:length(no2.list)) {
  no2.df <- no2.list[[i]]
  
  if(nrow(no2.df) <=12) {
    
    print(paste(no2.df$Site.ID[1], "dataframe too small"))
    
    no2.df <- no2.df[order(no2.df$Year),]
    no2.df$count.na <- NA
    no2.df$rollmean.no2 <- NA
    no2.df$rollmean.no2.corr <- NA
    
  } else {
    
    no2.df <- no2.df[order(no2.df$Year),]
    
    no2.df$count.na <- rollapply(no2.df$no2, width=12, 
                               FUN=function(x) length(x[is.na(x)]), align='right', fill = NA)
  
    no2.df$rollmean.no2 <- rollapply(no2.df$no2, width=12, 
                                     FUN=function(x) mean(x, na.rm=TRUE), fill=NA, align="right")
    
    no2.df$rollmean.no2.corr <- ifelse(no2.df$count.na>=2, NA, no2.df$rollmean.no2)
    
    # no2.df$cumsum.no2 <- cumsum(no2.df$no2)
    
    count.vector <- cbind.data.frame(site.ID = no2.df$Site.ID[1],
                                     count.na = sum(is.na(no2.df$no2)))
    na.countpersite <- rbind.data.frame(na.countpersite, count.vector)
  }
  no2.list[[i]] <- no2.df
  
  if(i == 1) {
    master.no2 <- no2.df
  } else {
     master.no2 <- rbind(master.no2, no2.df)
  }

  
}

months <- c("Jan", "Feb", "Mar", "Apr" , "May", "Jun", "Jul", 
            "Aug", "Sep", "Oct", "Nov", "Dec")

## factor conversion for calendar order ####
master.no2$Month_f<-  factor(master.no2$Month, 
                                   levels=months)

# seasonal.adjustment$Month <- as.character(seasonal.adjustment$Month)
# 
# ## adding the seasonal adjustments to the master file:
# master.no2<- merge(master.no2, seasonal.adjustment, by = "Month", all = TRUE) 
# ## calculating the fit
# master.no2$no2.adj.old <- master.no2$SlopeFactor_old*master.no2$no2
# 
# master.no2$no2.adj.new <- master.no2$SlopeFactor_new*master.no2$no2

# 
# master.no2$datetime <- paste0("01-",master.no2$Month, "-", master.no2$Year)
# master.no2$datetime <- as.POSIXct(as.character(master.no2$datetime),
#                                   format = "%d-%b-%Y")
# master.no2$datetime <- as.Date(master.no2$datetime)
# 
# 
# ggplot(master.no2[(which(master.no2$Site.ID == no2.sites[20])),])+
#   geom_line(aes(datetime,no2)) +geom_point(aes(datetime,no2)) + 
#   geom_smooth(aes(datetime,no2))

######################################




###### monthwise plots for regression ######
monthwise.list <- split(master.no2, master.no2$Month_f)
#monthwise.list <- split(master.roadside, master.roadside$Month_f)


# PDFfile <- paste0(path,"monthwise_CubicSpline_regressions2007to2016_intercept.pdf")
# pdf(file=PDFfile, paper = "USr", width = 20)
p <- list()
for(i in 1:length(monthwise.list)){
  temp.month <- monthwise.list[[i]]
  # temp.month <- temp.month[which(temp.month$rollmean.no2<32),]
  current.month <- temp.month$Month[i]
 
  # p2 <- ggplotRegression2(lm(rollmean.no2.corr ~ (0+no2), data = temp.month),
  #                        temp.month$Month[1])
  
  p1 <- ggplot(temp.month) + theme_bw() +
    geom_point(aes(no2,rollmean.no2.corr), size = 1) +
    stat_smooth(aes(no2,rollmean.no2.corr), method = lm, formula = y~0 + x, fullrange = F) +
    scale_x_continuous(limits = c(0,50), 
                       breaks = c(0,10,20,30,40,50)) +
    scale_y_continuous(limits = c(0,50), 
                       breaks = c(0,10,20,30,40,50)) +
    xlab(paste(current.month,"NO2 / mg m-3")) + 
    ylab("Rolling average") +
    theme(axis.title = element_text(size = 14), 
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
  #grid.arrange(p1, p2, ncol = 2)
  
  p[[i]] <- p1
  
}
#dev.off()

grid.arrange(p[[1]], p[[2]],p[[3]],
             p[[4]], p[[5]],p[[6]],
             p[[7]], p[[8]],p[[9]],
             p[[10]], p[[11]],p[[12]],ncol = 3)


######plotting the annual averages against monthly values for all sites. So each point is a site. ####
#plots <- list()

# for (i in 3:14) {
#   p1 <- eval(substitute(
#     ggplot(data=data.frame(no2.file),aes(x=no2.file[ ,i], y = Average))+
#       geom_point()+geom_smooth(method = 'lm')+theme_bw()+
#       xlab(colnames(no2.file)[ i]),list(i = i)))
#   print(i)
#   print(p1)
#   plots[[i]] <- p1  # add each plot into plot list
# }
# 
# plots[[4]]
# 
# multiplot(plots, cols =3)
# no2.may <- master.no2[which(master.no2$Month == "May"),]
# 
# ggplot(no2.may[which(no2.may$SiteType != "SH"),]) +
#   geom_point(aes(rollmean.no2,no2, color = Region)) + 
#   geom_smooth(aes(rollmean.no2,no2, color = Year), method = "lm", se = F)


#########OUTLIER IDENTIFICATION######


for( i in 1:12) {
  no2.df <- master.no2[which(master.no2$Month == months[i]),]
  
  ## dropping NA values to avoid different length residuals.
  no2.df <- no2.df %>% drop_na()
  reg1 <- lm(rollmean.no2 ~ 0 + no2, data = no2.df)

  no2.df$residuals <-reg1$residuals
  no2.df$std.residuals <- as.numeric(rstudent(reg1))
  no2.df$fittedvalues <- reg1$fitted.values

  
  # reg2 <-  gam(rollmean.no2 ~ no2+ s(no2, sp = 2), data = no2.df)
  # no2.df$residuals <-reg2$residuals
  # no2.df$std.residuals <-  residuals.gam(reg2, type = "scaled.pearson")
  # no2.df$fittedvalues <- reg2$fitted.values

  ### removing outliers at this stage rather than later. 
  no2.df.out.rm <- no2.df[which(abs(no2.df$std.residuals)<2),]
  outliers <- no2.df[which(abs(no2.df$std.residuals)>=2),]
  
  if(i == 1) {
    master.no2.new <-no2.df.out.rm 
    master.outliers <- outliers
  } else {
    master.no2.new <- rbind.data.frame(master.no2.new,no2.df.out.rm)
    master.outliers <- rbind.data.frame(outliers, master.outliers)

  }
  print (paste("Month", i, "done."))
}
#### adding NIWA's site classification 
master.no2.df <- merge(master.no2.new, mfE_file, by = "Site.ID", all = T)


count(master.no2.df, "Month_f")


res.averages <-aggregate(no2.final$std.residuals, 
                         list(no2.final$Site.ID, no2.final$Month_f),
                          FUN = function(x) mean(x, na.rm = T))
colnames(res.averages) <- c("Site.ID","Month_f","res.std")
site.score <- merge(no2.final, res.averages, by = c("Site.ID","Month_f"), all = T)


##########only to order the plot ###
#site.score$Site.ID <- factor(site.score$Site.ID, levels=(site.score$Site.ID)[order(abs(site.score$std.residuals))])
##### outliers in each month###
p1 <- ggplot(site.score)+
  geom_tile(aes(y = Site.ID, x = Year, fill = res.std)) +
  theme_bw()+
  scale_fill_distiller(palette = "RdGy")+
  theme(axis.text.y = element_text(angle = 0, size = 4))

p1



#### outliers scatter####
ggplot(master.no2.df, aes(x = fittedvalues, y = std.residuals)) +
  geom_point(color = "indianred", size = 0.5) + 
  geom_smooth(method = "lm", color = "black") + theme_bw()+
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70)) +
  geom_text(aes(label=ifelse(abs(std.residuals)>2,
                             as.character(master.no2.new$Site.ID),'')),
            hjust=0.5,vjust=0,size = 2, angle = 30)


############# outlier identification ###########

ggplot(master.no2.df, aes(x = rollmean.no2, y = std.residuals)) +
  geom_point(color = "indianred", size = 0.5) + 
  geom_smooth(method = "lm", color = "black") + theme_bw()+
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70))+
  geom_text(aes(label=ifelse(abs(std.residuals)>2,
                             as.character(master.no2.new$Site.ID),'')),
            hjust=0.5,vjust=0,size = 2, angle = 30)

# ggplot(master.no2.df[which(master.no2$Month == "May"),], 
#        aes(y = rollmean.no2, x = no2)) +
#   geom_point(aes(color = factor(Site.Classification))) + 
#   geom_smooth(method = "lm", color = "black") + theme_bw()
# # 
# total.outlier.sites <- count(master.no2.df[which(abs(master.no2.df$std.residuals)>2),], "Site.ID")
# total.outlier.sites <- total.outlier.sites[which(total.outlier.sites$freq>1),]
# master.no2.df$res.class <- ifelse((master.no2.df$Site.ID %in% total.outlier.sites$Site.ID) &
#                                      abs(master.no2.df$std.residuals)>2, "outlier","high confidence")
# # 
# master.highconf <- master.no2.df[which(master.no2.df$res.class == "high confidence"),]
# length(unique(master.highconf$Site.ID))
# 
# ggplot(master.no2.df, aes(x = rollmean.no2, y = fittedvalues)) +
#   geom_point(aes(color = factor(res.class)))+
#   geom_smooth(size = 1, formula = y~I(0+x), methid = "lm") + theme_bw()

# 
####### dataframe of high confidence sites: ######
master.highconf <- master.no2.df


# creating an outliers table and calculations#####

# outlier.sites <- master.no2.df[which(master.no2.df$res.class == "outlier"),]
# length(unique(outlier.sites$Site.ID))
# length(unique(master.no2.df$Site.ID))

outlier.sites <- merge(master.outliers, mfE_file, by = "Site.ID", all = T)

outlier.sites <- outlier.sites[which(!is.na(outlier.sites$Month_f)),]
## number of repetitive outliers####
ggplot(outlier.sites, aes(Site.ID)) +
  geom_histogram(stat = "count", fill = "darkgreen") + theme_bw()+
  theme(axis.text.x = element_text(angle = 90, size = 8))

## number of repetitive outliers per Monitoring Zone####
ggplot(outlier.sites, aes(MonitoringZone)) +
  geom_histogram(stat = "count", fill = "darkgreen") + theme_bw()+
  theme(axis.text.x = element_text(angle = 90, size = 8))

## number of outliers per site type: #####
ggplot(outlier.sites, aes(Site.Classification)) +
  geom_histogram(stat = "count", fill = "darkgreen") + theme_bw()

ggplot(outlier.sites, aes(Month_f)) +
  geom_histogram(stat = "count", fill = "darkgreen") + theme_bw()

##### outliers in each month###
ggplot(site.score)+
  geom_tile(aes(y = Site.ID, x = Month_f, fill = res.std)) +
  theme_bw()+ scale_x_discrete(labels = c("J","F","M","A","M","J",
                                          "J","A","S","O","N","D"),
                               name = "Month")+
  scale_fill_gradient(low = "green", high = "red",
                      na.value = "grey50", guide = "colourbar")+
  theme(axis.text.y = element_text(angle = 30, size = 6))


#### counting per site classification type #####
count(master.highconf, "Site.Classification")

## number of outliers per month
count(outlier.sites, "Month_f")

repetitive.sites <- count(outlier.sites,vars =  "Site.ID")

res.averages$outlierfreq <- ifelse(res.averages$Group.1 %in% repetitive.sites$Site.ID, repetitive.sites$freq, NA)

#write.csv(res.averages, "Site_Score_GAMbased.csv")
#### Outliers removal and performing the regression analysis again:
#master.no2.new <- master.no2[-x,]

###### monthwise plots for regression - outliers removed ######
monthwise.list2 <- split(master.highconf, master.highconf$Month_f)

fit.table <- cbind.data.frame(Month = NA,Slope = NA, Intercept = NA, Rsq = NA)

PDFfile <- paste0(path,"monthwise_linear_less32_bothaxes_forcedintercept.pdf")
pdf(file=PDFfile, paper = "USr", width = 20)

for(i in 1:length(monthwise.list2)){
  temp.month <- monthwise.list2[[i]]
  
  temp.month <- temp.month[which(temp.month$rollmean.no2<32 &
                                   temp.month$no2<32),]
  
  fit <- lm(rollmean.no2.corr ~ no2, data = temp.month)

  fit.coeffs <- cbind.data.frame(Month =unique(temp.month$Month_f)[1],
                                 Slope = fit$coefficients[[2]], 
                                 Intercept = fit$coefficients[[1]], 
                                 Rsq = summary(fit)$adj.r.sq)
  fit.table <- rbind(fit.table,fit.coeffs)
  
  # p2 <- ggplotRegression2(lm(rollmean.no2.corr ~ 0 + no2, data = temp.month),
  #                         temp.month$Month[1])
  # 
  # print(p2)
  
}
dev.off()


fit.table <- fit.table[-1,]

#write.csv(fit.table, "seasonaladjustment3.csv")
seasonal.adjust <- read.csv("seasonaladjustment3.csv")


####combine the results and calculate no2.adjusted for seasonally. ######
master.highconf$Identifier <- rep("HighConf", nrow(master.highconf))
outlier.sites$Identifier <- rep("Outlier", nrow(outlier.sites))
no2.final <- rbind.data.frame(master.highconf,outlier.sites)


no2.final <- merge(no2.final, seasonal.adjust, by = "Month_f", all = T)
no2.final$no2.forced <- no2.final$no2*(no2.final$SlopeForced) + no2.final$InterceptForced
no2.final$no2.free <-  no2.final$no2*(no2.final$SlopeFree) + no2.final$InterceptFree


######calculating annual mean based on seasonal adjustment ########

### free intercept ####
annual.means.free <- aggregate(no2.final$no2.free,by = list(no2.final$Site.ID,no2.final$Year),
                          FUN = function(x) mean(x, na.rm = T))

colnames(annual.means.free) <- c("Site.ID","Year","annual.mean.free")

####forced intercept ####
annual.means.forced <- aggregate(no2.final$no2.forced,by = list(no2.final$Site.ID,no2.final$Year),
                               FUN = function(x) mean(x, na.rm = T))

colnames(annual.means.forced) <- c("Site.ID","Year","annual.mean.forced")


##merge all of it ####
annual.means <- merge(annual.means.forced,annual.means.free, by = c("Site.ID","Year"), all = TRUE)

annual.means <- merge(no2.averages,annual.means, by = c("Site.ID","Year"), all = TRUE)

annual.means$diff.free <- annual.means$no2 - annual.means$annual.mean.free
annual.means$diff.forced <- annual.means$no2 - annual.means$annual.mean.forced

annual.means$norm.free <- annual.means$diff.free/max(annual.means$diff.free, na.rm = T)

annual.means$norm.forced <- annual.means$diff.forced/max(annual.means$diff.forced, na.rm = T)

Site.avgs.free <- aggregate(annual.means$diff.free, by = list(annual.means$Site.ID),
                       FUN = function(x) mean(x, na.rm = T))
Site.avgs.forced <- aggregate(annual.means$diff.forced, by = list(annual.means$Site.ID),
                            FUN = function(x) mean(x, na.rm = T))
colnames(Site.avgs.free) <- c("Site.ID","Avg.Error.Free")
colnames(Site.avgs.forced) <- c("Site.ID","Avg.Error.Forced")
plot(Site.avgs.forced$Avg.Error.Forced)


no2.final$diff.free <- no2.final$no2 - no2.final$no2.free
no2.final$diff.forced<- no2.final$no2 - no2.final$no2.forced

#no2.final <- no2.final[which(!is.na(no2.final$Month_f)),]

ggplot(no2.final) +
  geom_tile(aes(y = Site.ID, x = Month_f, fill = diff.forced)) +
  theme_bw()+ scale_x_discrete(labels = c("J","F","M","A","M","J",
                                          "J","A","S","O","N","D"),
                               name = "Month")+
  scale_fill_gradient(low = "green", high = "red",
                      na.value = "grey50", guide = "colourbar")+
  theme(axis.text.y = element_text(angle = 30, size = 6))


ggplot(no2.final) +
  geom_point(aes(x = no2, y = ))


