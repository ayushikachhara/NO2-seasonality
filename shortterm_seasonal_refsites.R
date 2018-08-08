### ref work - finer scale seasonal adjustments ####

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(plotly)
library(gridExtra)
library(zoo)

path <- "/Users/ayushikachhara/Desktop/NIWA/NO2_seasonality/NZTA NO2 seasonality/NO2-seasonality/"
setwd(path)


refraw <- read.csv("ref_site_raw.csv", stringsAsFactors = F)
refraw$Time <- ifelse(refraw$Time == "24:00:00", "00:00", refraw$Time)
refraw$DateTime <-dmy_hm(paste(refraw$Date, refraw$Time))
refraw$Date <- dmy(as.character(refraw$Date))

refraw$Year <- year(refraw$Date)
refraw$Month <- month(refraw$Date)

refmonthly <- aggregate(refraw[,c(3:14)], by = list(refraw$Month,refraw$Year),
                        FUN = function(x) mean(x, na.rm = T))
colnames(refmonthly)[ 1:2]<- c("Month","Year")


refmonthly <- refmonthly[order(refmonthly$Year, refmonthly$Month),]
refmonthly <- refmonthly[-1,]

refmonthly1 <- gather(refmonthly,Site.ID, no2,AC_Glen:AC_Whang, factor_key=TRUE)
refmonthly1 <- refmonthly1[which(refmonthly1$Year >=2007 &
                                   refmonthly1$Year <= 2016),]

ann.average <- aggregate(refmonthly1$no2,
                         by = list(refmonthly1$Site.ID, refmonthly1$Year),
                         FUN = function(x) mean(x, na.rm = T))

colnames(ann.average) <- c("Site.ID","Year","ann.no2")

refmonthly1 <- merge(refmonthly1, ann.average, by = c("Site.ID","Year"), all = T)
refmonthly1$seasonal.factor <- refmonthly1$ann.no2/refmonthly1$no2
refmonthly1 <- refmonthly1[which(refmonthly1$Site.ID %in% c("AC_Penro",
                                                            "AC_Taka",
                                                            "AC_Queen",
                                                            "AC_Hende",
                                                            "AC_Khybe")),]
refmonthly1 <- refmonthly1[complete.cases(refmonthly1$no2),]
ref.sites <- split(refmonthly1, as.character(refmonthly1$Site.ID))

#### plotting seasonal values
for(i in 1:5) {
  cur.site <- ref.sites[[i]]
  
  p1 <- ggplot(cur.site) +
    geom_line(aes(Month, seasonal.factor, 
                  color = factor(Year), group = factor(Year)), size = 1.5) +
    geom_point(aes(Month, seasonal.factor)) +
    ggtitle(paste(cur.site$Site.ID[1])) + theme_bw() +
    scale_x_continuous(breaks = seq(1,12,1)) +
    scale_y_continuous(breaks = seq(0,3,0.3))
  
  print(p1)
  
}
months <- cbind.data.frame(Month_f = c(1:12),
                           Month = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                       "Aug", "Sep", "Oct", "Nov", "Dec"))
refpen <- ref.sites[[3]]
seasonalrefs <- cbind.data.frame(Year = refpen$Year,
                                 Month_f = refpen$Month,
                                 seasonal.factor = refpen$seasonal.factor)
seasonalrefs <- merge(seasonalrefs, months, by = "Month_f", all = T)


#### seasonal.adjustment hole-patch algorithm: ####

### function definition: ####
vecrm <- function(rm, month) {
  f5 <- cbind.data.frame(Month_f = f2$Month_f, 
                         Year = f2$Year, 
                         Slope_refsite = f2$Slope_refsite)
  f6 <- f2[which(f2$Month_f %in% months[month:(month+rm)]),]
  f6 <- f6[,-5]
  f7 <- merge(f5,f6, by = c("Month_f","Year","Slope_refsite"), all = T)
  f7$seas.NO2 <- f7$NO2*f7$Slope_refsite
  f7$start.month <- months[month]
  f7$available.months <- 12 - (11- rm)
  f7$calc.ann <- mean(f7$seas.NO2, na.rm = T)
  f7$obs.ann <- f1$Average
  f7$Site.ID <- unique(f1$Site.ID)
  return(f7)
}

no2.file <- read.csv("nationalNO2_2007to2016.csv")
# seasonal.factor <- read.csv("seasonaladjustment_penrose.csv")
seasonal.factor <- seasonalrefs[,c(4,2,3)]
seasonal.factor$Month <- factor(seasonal.factor$Month,
                                levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                           "Aug", "Sep", "Oct", "Nov", "Dec"))
colnames(seasonal.factor) <- c("Month_f","Year","Slope_refsite")
## remove all rows with NAs in it. We can do this since every row represents one year. 
## If a rolling mean is needed in the future, this will have to be replaced by a more sophisticated approach.

nonna.no2 <- na.omit(no2.file)

## Step 2: breakdown this dataset by site:

listofsites <- split(nonna.no2, as.character(nonna.no2$Site.ID))

#### trial part of the code: #####

## write a function on filename, with starting month as i and number of holes as n

rm.vec <- c(1:11)
months <- c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
            "Aug", "Sep", "Oct", "Nov", "Dec","Jan" ,"Feb" ,
            "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
            "Oct", "Nov", "Dec")


all.tables.list <- list()
k = 1

for(i in 1:length(listofsites)) {
  fileName <- listofsites[[i]]
  yearwise <- split(fileName, fileName$Year)
  
  for(year in 1: length(yearwise)){
    f1 <- yearwise[[year]]
    f2 <- melt(f1[,c(3:14)], id.vars = NULL)
    f2$Year <- rep(f1$Year, 12)
    colnames(f2) <- c("Month_f","NO2", "Year")
    f2 <- merge(f2,seasonal.factor, by = c("Month_f", "Year"), all.x = T)
    f2$Month_f <- factor(f2$Month_f,
                         levels =c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                   "Aug", "Sep", "Oct", "Nov", "Dec"))
    f2 <- f2[order(f2$Month_f),]
    
    for(i in 1:12) {
      for(j in 0:10){
        f3 <- vecrm(j,i)
        # all.table <- rbind(all.table, f3)
        all.tables.list[[k]] <- f3
        k <- k + 1
      }
    }
  }
  print(as.character(unique(fileName$Site.ID)))
}

all.table <- rbindlist(all.tables.list)


proc <- all.table %>% distinct(Site.ID,Year,no.holes, start.hole, calc.ann, .keep_all = TRUE)

seasonal.factor$Month_f <- as.character(seasonal.factor$Month_f)
proc <- proc[,c(10,2,6,7,8,9)]
# proc <- merge(proc,seasonal.factor, by.x = "start.month", by.y = "Month_f", all = T)

proc$start.month <- factor(proc$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                        "Aug", "Sep", "Oct", "Nov", "Dec"))

########### model building begins here #####
proc2 <- cbind.data.frame(Site.ID = proc$Site.ID, Year = proc$Year,
                          start.month = proc$start.month, 
                          available.months = proc$available.months,
                          calc.ann = proc$calc.ann, obs.ann = proc$obs.ann)

proc2$sd <- proc2$calc.ann - proc2$obs.ann

proc3 <- dcast(proc2, formula= Site.ID + Year + start.month + obs.ann ~ available.months,
               value.var = 'calc.ann',
               fun.aggregate = function(x) mean(x, na.rm = T))


proc4 <- proc2 %>% group_by(Site.ID,  available.months) %>%
  dplyr::summarise(obs.ann = mean(obs.ann, na.rm = T),
                   calc.ann = mean(calc.ann, na.rm = T),
                   est.sd = sd(sd,na.rm = T))

ggplot(proc4[which(proc4$ available.months == 1),]) +
  geom_point(aes(x =obs.ann, y = est.sd, 
                 color = as.factor( available.months)), size = 3) +theme_bw()


proc5 <- proc2 %>% group_by( available.months, start.month) %>%
  dplyr::summarise(obs.ann = mean(obs.ann, na.rm = T),
                   calc.ann = mean(calc.ann, na.rm = T),
                   est.sd = sd(sd,na.rm = T))

proc5$start.month <- factor(proc5$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                          "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(proc5) +
  geom_point(aes(x = available.months, y = est.sd, color = as.factor(start.month)), size = 2) +
  geom_smooth(aes(x = available.months, y = est.sd), fullrange = T) +
  theme_bw() +
  ggtitle("Stadard Deviation associated with estimating the annual mean \nbased on number of holes in the data")


p1 <- proc2[which(proc2$available.months == 9),]
p2 <- p1[which(p1$start.month != "Jan"),]
p2 <- p2[which(p2$start.month != "Feb"),]
p2 <- p2[which(p2$start.month != "Mar"),]

x <- lm(calc.ann~obs.ann, data =p2)

summary(x)

ggplot(p2) +
  geom_point(aes(y = calc.ann, x = obs.ann), size = 1) +
  geom_smooth(aes(y = calc.ann, x = obs.ann), color = "red", method = "lm") +
  theme_bw()

#### working with proc2 ####
proc2$start.date <- paste0(proc2$Year,"-",proc2$start.month,"-01")
proc2$start.date <- as.Date(proc2$start.date, format = "%Y-%m-%d")
proc2$end.date <- floor_date(proc2$start.date + months(proc2$ available.months), "month") -1
proc2$totaldays <- proc2$end.date - proc2$start.date
proc2$mid.date <- proc2$start.date + (proc2$totaldays/2)
proc2$dayofyear <- yday(proc2$start.date)
proc2$norm.sd <- proc2$sd/proc2$obs.ann
proc2$rnd.obsmean <- round(proc2$obs.ann/2,0)*2
# proc2$start.month <- as.numeric(format(proc2$start.date, format = "%m"))


### standard deviation as a function of observed annual mean (bands of 2 ug/m3) ####

proc7 <- dcast(proc2, formula= rnd.obsmean ~ available.months,
               value.var = 'norm.sd',
               fun.aggregate =  function(x) sd(x, na.rm = T))

colnames(proc7) <- c("rnd.obsmean", "avbl1", "avbl2","avbl3","avbl4",
                     "avbl5","avbl6","avbl7","avbl8","avbl9","avbl10","avbl11")


fit.values <- cbind.data.frame(available.months = NA,
                               mean = NA,
                               slope = NA,
                               intercept = NA,
                               rsq = NA)
for(i in 2:12){
  mean <- mean(proc7[,i], na.rm = T)
  fit <- lm(proc7[,i]~proc7[,1], data = proc7)
  intercept <- fit$coefficients[[1]]
  slope <- fit$coefficients[[2]]
  rsq <- summary(fit)$adj.r.squared
  
  fit.table <- cbind.data.frame(available.months = (i-1),
                                mean = mean,
                                slope = slope,
                                intercept = intercept,
                                rsq = rsq)
  
  fit.values <- rbind(fit.values,fit.table)
  
  
}
fit.values <- fit.values[-1,]

plot(fit.values$available.months, fit.values$rsq, type = "l")


ggplot(data = proc7) +
  geom_line(aes(rnd.obsmean, avbl1, colour = "1"))+
  geom_line(aes(rnd.obsmean, avbl2, colour = "2"))+
  geom_line(aes(rnd.obsmean, avbl3, colour = "3"))+
  geom_line(aes(rnd.obsmean, avbl4, colour = "4"))+
  geom_line(aes(rnd.obsmean, avbl5, colour = "5"))+
  geom_line(aes(rnd.obsmean, avbl6, colour = "6"))+
  geom_line(aes(rnd.obsmean, avbl7, colour = "7"))+
  geom_line(aes(rnd.obsmean, avbl8, colour = "8"))+
  geom_line(aes(rnd.obsmean, avbl9, colour = "9"))+
  geom_line(aes(rnd.obsmean, avbl10, colour = "10"))+
  geom_line(aes(rnd.obsmean, avbl11, colour = "11"))


#### normalised standard deviation as a function of start month: ####

proc8 <- dcast(proc2, formula= start.month ~ available.months,
               value.var = 'norm.sd',
               fun.aggregate =  function(x) sd(x, na.rm = T))

colnames(proc8) <- c("start.month", "avbl1", "avbl2","avbl3","avbl4",
                     "avbl5","avbl6","avbl7","avbl8","avbl9","avbl10","avbl11")


fit.values2 <- cbind.data.frame(available.months = NA,
                                mean = NA,
                                slope = NA,
                                intercept = NA,
                                rsq = NA)
for(i in 2:12){
  mean <- mean(proc8[,i], na.rm = T)
  fit <- lm(proc8[,i]~proc8[,1], data = proc8)
  intercept <- fit$coefficients[[1]]
  slope <- fit$coefficients[[2]]
  rsq <- summary(fit)$adj.r.squared
  
  fit.table <- cbind.data.frame(available.months = (i-1),
                                mean = mean,
                                slope = slope,
                                intercept = intercept,
                                rsq = rsq)
  
  fit.values2 <- rbind(fit.values2,fit.table)
  
  
}
fit.values2 <- fit.values2[-1,]

# plot(fit.values2$available.months, fit.values2$rsq, type = "l")


#### minimizing the error
vectora <- c(1:12)
fn <- function(v) {
  x = v[1]
  y = v[2]
  z = v[3]
  vectorc <- x*sin((vectora+y)*2*pi/12)+z
  return <- sum(abs(vectorb - vectorc)^2)
}

modpram <- cbind.data.frame(available.months = c(1:11), amp = NA,
                            x_offset = NA, y_offset = NA)

vectorb <- proc8$avbl1
modpram[1,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- proc8$avbl2
modpram[2,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- proc8$avbl3
modpram[3,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- proc8$avbl4
modpram[4,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- proc8$avbl5
modpram[5,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- proc8$avbl6
modpram[6,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- proc8$avbl7
modpram[7,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- proc8$avbl8
modpram[8,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- proc8$avbl9
modpram[9,2:4] <-  optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- proc8$avbl10
modpram[10,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- proc8$avbl11
modpram[11,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

modpram$troughstart <- ifelse(modpram$x_offset<9, 9-modpram$x_offset,21 - modpram$x_offset)
modpram$troughend <- modpram$available.months + modpram$troughstart
modpram$peak <- 15 - modpram$x_offset

modpram$start.date <- as.numeric(as.Date("2018-01-01", format = "%Y-%m-%d"))
modpram$start.date <- modpram$start.date + (modpram$troughstart*(365/12))
modpram$start.date <- as.Date(modpram$start.date, 
                              origin = "1970-01-01", 
                              format = "%Y-%m-%d")

modpram$end.date <- as.numeric(as.Date("2018-01-01", format = "%Y-%m-%d"))
modpram$end.date <- modpram$end.date + (modpram$troughend*(365/12))
modpram$end.date <- as.Date(modpram$end.date, origin = "1970-01-01", format = "%Y-%m-%d")


modpram$peak.date <- as.numeric(as.Date("2018-01-01", format = "%Y-%m-%d"))
modpram$peak.date <- modpram$peak.date + (modpram$peak*(365/12))
modpram$peak.date <- as.Date(modpram$peak.date, origin = "1970-01-01", format = "%Y-%m-%d")


##plotting: ###

ggplot(modpram) +
  geom_crossbar(aes(available.months,y = end.date,ymin = start.date, ymax = end.date, color = amp), 
                size = 4, color = "steelblue") +theme_bw()


ggplot(modpram) +
  geom_linerange(aes(available.months,ymin = start.date, ymax = end.date), 
                 size = 4, color = "steelblue") +theme_bw()


proc9 <- gather(proc8, start.month, norm.sd, avbl1:avbl11, factor_key=TRUE)
colnames(proc9) <- c("start.month","available.months","norm.sd")
proc9$available.months <- rep(1:11, each = 12)

# proc9$norm.sd <- ifelse(proc9$norm.sd>0.2, 0.2, proc9$norm.sd)

proc9$norm.sdfac <- factor(round((proc9$norm.sd/2),2)*2)
N <- nlevels(proc9$norm.sdfac)
colors <- colorRampPalette(c("yellow", "darkgreen"))(11)

ggplot(proc9, aes(start.month,available.months,fill = norm.sdfac, z = norm.sdfac)) +
  geom_tile() +
  scale_fill_manual(values=colors, breaks=levels(proc9$norm.sdfac)[seq(1, N, by=1)]) +
  scale_y_continuous(breaks = seq(0,11,1), name = "Number of available monthly records") +
  scale_x_discrete(name = "Starting month of monitoring") +
  ggtitle("Based on reference site: Queen Street (month/year)") +
  theme_void() +
  theme(legend.text = element_text(size = 16),
        title = element_text(size = 16),
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.margin = margin(2, 2, 2, 2, "cm"))

library(plotly)
proc10 <- spread(proc9[,1:3],key = start.month, value = norm.sd)
proc10 <- proc10[,-1]
proc10 <- as.matrix(proc10)

# plot_ly(z = ~proc10) %>% add_contour(colors = colors)
# plot_ly(z = ~proc10) %>% add_surface(colors = colors)
# 


# write.csv(proc9, "Takapuna_refsite_model.csv")
# write.csv(proc4, "Takapuna_refsite_model_persite.csv")
# write.csv(seasonal.factor,"seasonaladjustment_Takapuna.csv")

# write.csv(proc, "holepatch_annual_Queen.csv")
# write.csv(proc9, "holepatch_ann_mosaic_Queen.csv")

