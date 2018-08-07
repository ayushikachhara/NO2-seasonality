## how many months and which months are required for apt estimation of annual mean?

library(data.table)
library(dplyr)
library(zoo)
library(ggplot2)
library(tidyr)
library(lubridate)

path <- "/Users/ayushikachhara/Desktop/NIWA/NO2_seasonality/NZTA NO2 seasonality/NO2-seasonality/"
setwd(path)
no2.file <- read.csv("nationalNO2_2007to2016.csv")
# seasonal.factor <- read.csv("seasonaladjustment3.csv")
# seasonal.factor <- seasonal.factor[,c(1:3)]
## remove all rows with NAs in it. We can do this since every row represents one year. 
## If a rolling mean is needed in the future, this will have to be replaced by a more sophisticated approach.

nonna.no2 <- na.omit(no2.file)
nonna.no2$Site.Classification <- ifelse(nonna.no2$SiteType == "Background", "Urban Background","Traffic")
## Step 2: breakdown this dataset by site:

listofsites <- split(nonna.no2, as.character(nonna.no2$Site.ID))

rm.vec <- c(1:11)
months <- c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
            "Aug", "Sep", "Oct", "Nov", "Dec","Jan" ,"Feb" ,
            "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
            "Oct", "Nov", "Dec")


load("cosineapproach.RData")
load("holepatchprocessed_1.RData")


#### cosine model #####
all.table2 <- all.table2[-1,]
all.table <- all.table[-1,]

##subset of the whole dataset ####
f1 <- cbind.data.frame(Site.ID = all.table2$Site.ID,
                       Year = all.table2$Year,
                       Month = all.table2$Month_f,
                       seasonal.factor = all.table2$seasonal.factor,
                       obs.NO2 = all.table2$NO2,
                       obs.ann = all.table2$obs.ann,
                       start.month = all.table2$start.month,
                       available.months = all.table2$available.months,
                       calc.ann = all.table2$calc.ann)



no2.long <- reshape(nonna.no2, direction="long", varying=list(names(no2.file)[3:15]), v.names="no2", 
                    idvar=c("Site.ID","Year"), timevar= "Month", times = names(no2.file)[3:15])

f2 <- no2.long[,c(1,2,11,12,13)]
rownames(f2) <- c()
f2 <- f2[which(f2$Month != "Average"),]

f2$Month <- factor(f2$Month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                        "Aug", "Sep", "Oct", "Nov", "Dec"))

f3 <- merge(f1,f2, by = c("Site.ID","Year","Month"), all = T)


#### calculating the estimated monthly data with modelled annual mean ####
f3$mod.NO2 <- ifelse(!is.na(f3$obs.NO2), f3$no2, f3$calc.ann/f3$seasonal.factor)
# ggplot(f3, aes(no2,mod.NO2,color = available.months)) +geom_point()

f4 <- f3 %>% group_by(Site.ID, start.month, available.months) %>%
  summarise(Site.Classification = unique(Site.Classification),
            mod.NO2 = mean(mod.NO2, na.rm = T),
            obs.NO2 = mean(no2, na.rm = T),
            obs.ann = mean(obs.ann,na.rm = T),
            sd = mean((mod.NO2 - obs.NO2), na.rm = T),
            norm.sd = mean((mod.NO2 - obs.NO2), na.rm = T)/mean(obs.ann,na.rm = T))

# ggplot(f4, aes(obs.NO2,mod.NO2,color = factor(available.months))) +geom_point() +theme_bw() +
#   scale_x_continuous(limits = c(0,50), breaks = seq(0,50,5)) + 
#   scale_y_continuous(limits = c(0,50), breaks = seq(0,50,5))

# ggplot(f4, aes(available.months,norm.sd, color = Site.Classification)) +
#   geom_smooth() +geom_point() + theme_bw()
# 
# 
# ggplot(f4) +geom_point(aes(available.months,norm.sd, color = Site.Classification)) + theme_bw()
# 
# ggplot(f3) +geom_point(aes(obs.ann,calc.ann)) +geom_smooth(aes(obs.ann,calc.ann))


f5 <- aggregate(list(f3$mod.NO2, f3$no2), 
                by = list(f3$Site.ID, f3$Year, f3$available.months, f3$start.month),
                FUN = function(x) mean(x, na.rm = T))
colnames(f5) <- c("Site.ID","Year","available.months","start.month","mod.ann","obs.ann")
# ggplot(f5) +geom_point(aes(obs.ann,mod.ann, color = available.months), size = 2)+
#   scale_color_continuous(low = "skyblue", high = "deepskyblue4", values = seq(1,11,1)) + theme_bw()+
#   scale_x_continuous(breaks = seq(0,50,5), limit = c(0,50), 
#                      name = "Annual mean estimation based on observed NO2 (neglecting holes)")+
#   scale_y_continuous(breaks = seq(0,50,5), limit = c(0,50), 
#                      name = "Annual mean estimation based on observed & modelled NO2 (patching holes)")+
#   theme(legend.text = element_text(size = 16),
#         legend.title = element_blank(),
#         axis.text = element_text(size = 16),
#         axis.title = element_text(size = 16))
# 

f5$available.monthsfac <- factor(round(f5$available.months/2)*2)
ggplot(f5) +geom_point(aes(obs.ann,mod.ann, color = available.monthsfac), size = 2) +
  scale_colour_brewer(palette = "Blues") +
  theme_dark()+
  scale_x_continuous(breaks = seq(0,50,5), limit = c(0,50), 
                     name = "Annual mean estimation based on observed NO2")+
  scale_y_continuous(breaks = seq(0,50,5), limit = c(0,50), 
                     name = "Annual mean estimation based on observed  & cosine model")+
  theme(legend.text = element_text(size = 16),
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))


ggplot(f5) +geom_point(aes(obs.ann,((mod.ann-obs.ann)/obs.ann), 
                           color = available.monthsfac), size = 2) +
  scale_colour_brewer(palette = "Blues") +
  theme_dark()+
  scale_x_continuous(breaks = seq(0,50,5), limit = c(0,50), 
                     name = "Annual mean estimation based on observed NO2")+
  scale_y_continuous(breaks = seq(0,2,0.5), limit = c(0,2), 
                     name = "Error in estimating annual mean based on cosine model")+
  theme(legend.text = element_text(size = 16),
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))




#### linear adjustment ####

g1 <- cbind.data.frame(Site.ID = all.table$Site.ID,
                       Year = all.table$Year,
                       Month = all.table$Month_f,
                       seasonal.factor = all.table$SlopeForced,
                       obs.NO2 = all.table$NO2,
                       obs.ann = all.table$obs.ann,
                       start.month = all.table$start.hole,
                       available.months = 12 - all.table$no.holes,
                       calc.ann = all.table$calc.ann)



no2.long <- reshape(nonna.no2, direction="long", varying=list(names(no2.file)[3:15]), v.names="no2", 
                    idvar=c("Site.ID","Year"), timevar= "Month", times = names(no2.file)[3:15])

g2 <- no2.long[,c(1,2,11,12,13)]
rownames(g2) <- c()
g2 <- g2[which(g2$Month != "Average"),]

g2$Month <- factor(g2$Month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                        "Aug", "Sep", "Oct", "Nov", "Dec"))

g3 <- merge(g1,g2, by = c("Site.ID","Year","Month"), all = T)



g3$mod.NO2 <- ifelse(!is.na(g3$obs.NO2), g3$no2, g3$calc.ann/g3$seasonal.factor)
# ggplot(g3, aes(no2,mod.NO2,color = available.months)) +geom_point()

g4 <- g3 %>% group_by(Site.ID, start.month, available.months) %>%
  summarise(Site.Classification = unique(Site.Classification),
            mod.NO2 = mean(mod.NO2, na.rm = T),
            obs.NO2 = mean(no2, na.rm = T),
            obs.ann = mean(obs.ann,na.rm = T),
            sd = mean((mod.NO2 - obs.NO2), na.rm = T),
            norm.sd = mean((mod.NO2 - obs.NO2), na.rm = T)/mean(obs.ann,na.rm = T))

# ggplot(g4, aes(obs.NO2,mod.NO2,color = factor(available.months))) +geom_point() +theme_bw() +
#   scale_x_continuous(limits = c(0,50), breaks = seq(0,50,5)) + 
#   scale_y_continuous(limits = c(0,50), breaks = seq(0,50,5))
# 
# ggplot(g4, aes(available.months,norm.sd, color = Site.Classification)) +
#   geom_smooth() +geom_point() + theme_bw()


# ggplot(g4) +geom_point(aes(available.months,norm.sd, color = Site.Classification)) + theme_bw()
# 
# ggplot(g3) +geom_point(aes(obs.ann,calc.ann)) +geom_smooth(aes(obs.ann,calc.ann))


g5 <- aggregate(list(g3$mod.NO2, g3$no2), 
                by = list(g3$Site.ID, g3$Year, g3$available.months, g3$start.month),
                FUN = function(x) mean(x, na.rm = T))
colnames(g5) <- c("Site.ID","Year","available.months","start.month","mod.ann","obs.ann")

g5$available.monthsfac <- factor(round(g5$available.months/2)*2)
ggplot(g5) +geom_point(aes(obs.ann,mod.ann, color = available.monthsfac), size = 2) +
  scale_colour_brewer(palette = "Blues") +
  theme_dark()+
  scale_x_continuous(breaks = seq(0,50,5), limit = c(0,50), 
                     name = "Annual mean estimation based on observed NO2")+
  scale_y_continuous(breaks = seq(0,50,5), limit = c(0,50), 
                     name = "Annual mean estimation based on observed & patched data")+
  theme(legend.text = element_text(size = 16),
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))


ggplot(g5) +geom_point(aes(obs.ann,((mod.ann-obs.ann)/obs.ann), 
                           color = available.monthsfac), size = 2) +
  scale_colour_brewer(palette = "Blues") +
  theme_dark()+
  scale_x_continuous(breaks = seq(0,50,5), limit = c(0,50), 
                     name = "Annual mean estimation based on observed NO2")+
  scale_y_continuous(breaks = seq(0,2,0.5), limit = c(0,2), 
                     name = "Error in estimating annual mean")+
  theme(legend.text = element_text(size = 16),
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

g5$start.month <- factor(g5$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                    "Aug", "Sep", "Oct", "Nov", "Dec"))

f5$start.month <- factor(f5$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                    "Aug", "Sep", "Oct", "Nov", "Dec"))



h1 <- merge(f5,g5, by = c("Site.ID","Year","available.months",
                          "available.monthsfac","start.month", "obs.ann"),
            all = T, suffixes = c(".cosine", ".linear"))
h1$err <- abs(h1$mod.ann.cosine - h1$mod.ann.linear)
h1$rel.err <- h1$mod.ann.cosine/h1$mod.ann.linear



h2 <- aggregate(list(h1$mod.ann.cosine, h1$mod.ann.linear, h1$err, h1$rel.err), 
                by = list(h1$available.months, h1$start.month),
                FUN = function(x) mean(x, na.rm = T))

colnames(h2) <- c("available.months","start.month","mod.ann.cosine",
                  "mod.ann.linear","abs.err","rel.err")

ggplot(h2) +
  geom_tile(aes(start.month, available.months, fill = rel.err), color = "white") +
  scale_y_continuous(breaks = seq(1,11,1))+
  scale_color_brewer()+
  theme_bw()  +
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

ggplot(h2) +
  geom_tile(aes(start.month, available.months, fill = abs.err), color = "white") +
  scale_y_continuous(breaks = seq(1,11,1)) +
  scale_color_brewer(palette = "Greens", "Absolute Error") +
  theme_bw()  +
  theme(legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))






