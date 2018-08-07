## how many months and which months are required for apt estimation of annual mean?

library(data.table)
library(dplyr)
library(zoo)
library(ggplot2)
library(tidyr)
library(lubridate)

path <- "/Users/ayushikachhara/Desktop/NIWA/NO2_seasonality/NZTA NO2 seasonality/NO2-seasonality/"
setwd(path)


### function definition: ####

vecrm <- function(rm, month) {
  f5 <- cbind.data.frame(Month_f = f2$Month_f, 
                         Year = f2$Year, 
                         SlopeForced = f2$SlopeForced)
  f6 <- f2[which(f2$Month_f %in% months[month:(month+rm)]),]
  f7 <- merge(f5,f6, by = c("Month_f","Year","SlopeForced"), all = T)
  f7$seas.NO2 <- f7$NO2*f7$SlopeForced
  f7$start.hole <- months[month]
  f7$no.holes <- 11- rm
  f7$calc.ann <- mean(f7$seas.NO2, na.rm = T)
  f7$obs.ann <- f1$Average
  f7$Site.ID <- unique(f1$Site.ID)
  return(f7)
}

# month = 1
# rm = 2
vecrm2 <- function(rm, month) {
  f5 <- cbind.data.frame(Month_f = f2$Month_f, 
                         Year = f2$Year, 
                         seasonal.factor = f2$seasonal.factor)
  f6 <- f2[which(f2$Month_f %in% months[month:(month+rm)]),]
  f7 <- merge(f5,f6, by = c("Month_f","Year","seasonal.factor"), all = T)
  f7$seas.NO2 <- f7$NO2*f7$seasonal.factor
  f7$start.month <- months[month]
  f7$available.months <-  rm +1
  f7$calc.ann <- mean(f7$seas.NO2, na.rm = T)
  f7$obs.ann <- f1$Average
  f7$Site.ID <- unique(f1$Site.ID)
  return(f7)
}

## Step 1: find sites with full year records and also the year.

no2.file <- read.csv("nationalNO2_2007to2016.csv")
seasonal.factor <- read.csv("seasonaladjustment3.csv")
seasonal.factor <- seasonal.factor[,c(1:3)]
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

# all.table <- cbind.data.frame("Month_f" = NA,
#                               "Year" = NA,
#                               "SlopeForced" = NA,
#                               "NO2" = NA,
#                               "Month_no" =NA,
#                               "seas.NO2" = NA,
#                               "start.hole" = NA,
#                               "no.holes" = NA,
#                               "calc.ann" = NA,
#                               "obs.ann" = NA,
#                               "Site.ID" = NA)
# 
# for(i in 1:length(listofsites)) {
#   fileName <- listofsites[[i]]
#   yearwise <- split(fileName, fileName$Year)
# 
#   for(year in 1: length(yearwise)){
#     f1 <- yearwise[[year]]
#     f2 <- melt(f1[,c(3:14)])
#     f2$Year <- rep(f1$Year, 12)
#     colnames(f2) <- c("Month_f","NO2", "Year")
#     f2 <- merge(f2,seasonal.factor, by = "Month_f")
#     f2$Month_f <- factor(f2$Month_f,
#                          levels =c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
#                                    "Aug", "Sep", "Oct", "Nov", "Dec"))
#     f2 <- f2[order(f2$Month_f),]
# 
#     for(i in 1:12) {
#       for(j in 0:10){
#         f3 <- vecrm(j,i)
#         all.table <- rbind(all.table, f3)
#       }
#     }
#   }
#   print(as.character(unique(fileName$Site.ID)))
# }


# all.table1 <- all.table[complete.cases(all.table),]
# save(all.table,file = "holepatchprocessed_1.RData")

load("holepatchprocessed.RData")

## keep only the distinct rows
proc <- all.table1 %>% distinct(Site.ID,Year,no.holes, start.hole, calc.ann, .keep_all = TRUE)

colnames(proc)[7] <- "available.months" 
colnames(proc)[6] <- "start.month" 

proc$available.months <- 12 - proc$available.months
seasonal.factor$Month_f <- as.character(seasonal.factor$Month_f)
proc <- proc[,c(10,2,6,7,8,9)]
proc <- merge(proc,seasonal.factor, by.x = "start.month", by.y = "Month_f", all = T)

proc$start.month <- factor(proc$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                        "Aug", "Sep", "Oct", "Nov", "Dec"))


# proc$Janest <- proc$calc.ann/proc$SlopeForced
# proc$Febest <- proc$calc.ann/proc$SlopeForced
# proc$Marest <- proc$calc.ann/proc$SlopeForced
# proc$Aprest <- proc$calc.ann/proc$SlopeForced
# proc$Mayest <- proc$calc.ann/proc$SlopeForced
# proc$Junest <- proc$calc.ann/proc$SlopeForced
# proc$Julest <- proc$calc.ann/proc$SlopeForced
# proc$Augest <- proc$calc.ann/proc$SlopeForced
# proc$Sepest <- proc$calc.ann/proc$SlopeForced
# proc$Octest <- proc$calc.ann/proc$SlopeForced
# proc$Novest <- proc$calc.ann/proc$SlopeForced
# proc$Decest <- proc$calc.ann/proc$SlopeForced
# 
# 
# 
# longproc <- merge(proc,nonna.no2[,c(1:14)], by = c("Site.ID", "Year"), all = T)


# write.csv(longproc, "inverse_holepatch.csv")



########### model building begins here #####
proc2 <- cbind.data.frame(Site.ID = proc$Site.ID, Year = proc$Year,
                          start.month = proc$start.month, 
                          available.months = proc$available.months,
                          calc.ann = proc$calc.ann, obs.ann = proc$obs.ann)

proc2$sd <- proc2$calc.ann - proc2$obs.ann

proc3 <- dcast(proc2, formula= Site.ID + Year + start.month + obs.ann ~ available.months,
               value.var = 'calc.ann',
               fun.aggregate = mean)


proc4 <- proc2 %>% group_by(Site.ID,  available.months) %>%
  dplyr::summarise(obs.ann = mean(obs.ann, na.rm = T),
                   calc.ann = mean(calc.ann, na.rm = T),
                   est.sd = sd(sd,na.rm = T))

ggplot(proc4[which(proc4$ available.months == 1),]) +
  geom_point(aes(x =obs.ann, y = est.sd, color = as.factor( available.months)), size = 3) +theme_bw()
  

proc5 <- proc2 %>% group_by( available.months, start.month) %>%
  dplyr::summarise(obs.ann = mean(obs.ann, na.rm = T),
                   calc.ann = mean(calc.ann, na.rm = T),
                   est.sd = sd(sd,na.rm = T))

proc5$start.month <- factor(proc5$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                          "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(proc5) +
  geom_point(aes(x = available.months, y = est.sd, color = as.factor(start.month)), size = 2) +
  theme_bw() +
  ggtitle("Stadard Deviation associated with estimating the annual mean \nbased on number of holes in the data")





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
               fun.aggregate = sd)

colnames(proc7) <- c("rnd.obsmean", "avbl1", "avbl2","avbl3","avbl4",
                     "avbl5","avbl6","avbl7","avbl8","avbl9","avbl10","avbl11")


fit.values <- cbind.data.frame(available.months = NA,
                               mean = NA,
                               slope = NA,
                               intercept = NA,
                               rsq = NA)
for(i in 2:12){
  mean <- mean(proc7[,i])
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
               fun.aggregate = sd)

colnames(proc8) <- c("start.month", "avbl1", "avbl2","avbl3","avbl4",
                     "avbl5","avbl6","avbl7","avbl8","avbl9","avbl10","avbl11")


fit.values2 <- cbind.data.frame(available.months = NA,
                               mean = NA,
                               slope = NA,
                               intercept = NA,
                               rsq = NA)
for(i in 2:12){
  mean <- mean(proc8[,i])
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

plot(fit.values2$available.months, fit.values2$rsq, type = "l")


# ggplot(data = proc8) +
#   geom_line(aes(start.month, avbl1, colour = "1"))+
#   geom_line(aes(start.month, avbl2, colour = "2"))+
#   geom_line(aes(start.month, avbl3, colour = "3"))+
#   geom_line(aes(start.month, avbl4, colour = "4"))+
#   geom_line(aes(start.month, avbl5, colour = "5"))+
#   geom_line(aes(start.month, avbl6, colour = "6"))+
#   geom_line(aes(start.month, avbl7, colour = "7"))+
#   geom_line(aes(start.month, avbl8, colour = "8"))+
#   geom_line(aes(start.month, avbl9, colour = "9"))+
#   geom_line(aes(start.month, avbl10, colour = "10"))+
#   geom_line(aes(start.month, avbl11, colour = "11"))

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

proc9$norm.sd <- ifelse(proc9$norm.sd>0.2, 0.2, proc9$norm.sd)

proc9$norm.sdfac <- factor(round((proc9$norm.sd/2),2)*2)
N <- nlevels(proc9$norm.sdfac)
colors <- colorRampPalette(c("lightblue", "darkred"))(10)

ggplot(proc9, aes(start.month,available.months,fill = norm.sdfac, z = norm.sdfac)) +
  geom_tile() +
  scale_fill_manual(values=colors, breaks=levels(proc9$norm.sdfac)[seq(1, N, by=1)]) +
  scale_y_continuous(breaks = seq(0,11,1), name = "Number of available monthly records") +
  scale_x_discrete(name = "Starting month of monitoring") +
  theme_void() +
  theme(legend.text = element_text(size = 16),
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.margin = margin(2, 2, 2, 2, "cm"))

library(plotly)
proc10 <- spread(proc9[,1:3],key = start.month, value = norm.sd)
proc10 <- proc10[,-1]
proc10 <- as.matrix(proc10)

plot_ly(z = ~proc10) %>% add_contour(colors = colors)
plot_ly(z = ~proc10) %>% add_surface(colors = colors)

################cosine approach ######

all.table2 <- cbind.data.frame("Month_f" = NA,
                               "Year" = NA,
                               "seasonal.factor" = NA,
                               "NO2" = NA,
                              "start.date" = NA,
                              "dayofyear" = NA,
                              "seas.NO2" = NA,
                              "start.month" = NA,
                              "available.months" = NA,
                              "calc.ann" = NA,
                              "obs.ann" = NA,
                              "Site.ID" = NA)

for(i in 1:length(listofsites)) {
  fileName <- listofsites[[i]]
  yearwise <- split(fileName, fileName$Year)

  for(year in 1: length(yearwise)){
    f1 <- yearwise[[year]]
    f2 <- melt(f1[,c(3:14)])
    f2$Year <- rep(f1$Year, 12)
    colnames(f2) <- c("Month_f","NO2", "Year")
    f2$start.date <- paste0(f2$Year,"-",as.character(f2$Month_f),"-01")
    f2$start.date <- as.Date(f2$start.date, format = "%Y-%b-%d")
    f2$dayofyear <- yday(f2$start.date)
    f2$seasonal.factor <- 0.23*cos(2*pi*((f2$dayofyear+12)/365)) + 1
    f2$Month_f <- factor(f2$Month_f,
                         levels =c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                   "Aug", "Sep", "Oct", "Nov", "Dec"))
    f2 <- f2[order(f2$Month_f),]

    for(i in 1:12) {
      for(j in 0:10){
        f3 <- vecrm2(j,i)
        all.table2 <- rbind(all.table2, f3)
      }
    }
  }
  print(as.character(unique(fileName$Site.ID)))
}


# save(all.table2, file = "cosineapproach.RData")


load("cosineapproach.RData")
## keep only the distinct rows
proc <- all.table2 %>% distinct(Site.ID,Year,available.months, 
                                start.month, .keep_all = TRUE)
 
# colnames(proc)[7] <- "available.months" 
# colnames(proc)[6] <- "start.month" 

# proc$available.months <- 12 - proc$available.months
# seasonal.factor$Month_f <- as.character(seasonal.factor$Month_f)
proc <- proc[-1,c(12,2,8,3,4,7,9,10,11)]
# proc <- merge(proc,seasonal.factor, by.x = "start.month", by.y = "Month_f", all = T)

proc$start.month <- factor(proc$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                        "Aug", "Sep", "Oct", "Nov", "Dec"))


# proc$Janest <- proc$calc.ann/proc$SlopeForced
# proc$Febest <- proc$calc.ann/proc$SlopeForced
# proc$Marest <- proc$calc.ann/proc$SlopeForced
# proc$Aprest <- proc$calc.ann/proc$SlopeForced
# proc$Mayest <- proc$calc.ann/proc$SlopeForced
# proc$Junest <- proc$calc.ann/proc$SlopeForced
# proc$Julest <- proc$calc.ann/proc$SlopeForced
# proc$Augest <- proc$calc.ann/proc$SlopeForced
# proc$Sepest <- proc$calc.ann/proc$SlopeForced
# proc$Octest <- proc$calc.ann/proc$SlopeForced
# proc$Novest <- proc$calc.ann/proc$SlopeForced
# proc$Decest <- proc$calc.ann/proc$SlopeForced
# 
# 
# 
# longproc <- merge(proc,nonna.no2[,c(1:14)], by = c("Site.ID", "Year"), all = T)


# write.csv(longproc, "inverse_holepatch.csv")



########### model building begins here #####
proc2 <- cbind.data.frame(Site.ID = proc$Site.ID, Year = proc$Year,
                          start.month = proc$start.month, 
                          available.months = proc$available.months,
                          calc.ann = proc$calc.ann, obs.ann = proc$obs.ann)

proc2$sd <- proc2$calc.ann - proc2$obs.ann

proc3 <- dcast(proc2, formula= Site.ID + Year + start.month + obs.ann ~ available.months,
               value.var = 'calc.ann',
               fun.aggregate = function(x) mean(x, na.rm =T))


proc4 <- proc2 %>% group_by(Site.ID,  available.months) %>%
  dplyr::summarise(obs.ann = mean(obs.ann, na.rm = T),
                   calc.ann = mean(calc.ann, na.rm = T),
                   est.sd = sd(sd,na.rm = T))

ggplot(proc4[which(proc4$ available.months == 1),]) +
  geom_point(aes(x =obs.ann, y = est.sd, color = as.factor( available.months)), size = 3) +theme_bw()


proc5 <- proc2 %>% group_by( available.months, start.month) %>%
  dplyr::summarise(obs.ann = mean(obs.ann, na.rm = T),
                   calc.ann = mean(calc.ann, na.rm = T),
                   est.sd = sd(sd,na.rm = T))

proc5$start.month <- factor(proc5$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                          "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(proc5) +
  geom_point(aes(x = available.months, y = est.sd, color = as.factor(start.month)), size = 2) +
  theme_bw() +
  ggtitle("Stadard Deviation associated with estimating the annual mean \nbased on number of holes in the data")





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
               fun.aggregate = sd)

colnames(proc7) <- c("rnd.obsmean", "avbl1", "avbl2","avbl3","avbl4",
                     "avbl5","avbl6","avbl7","avbl8","avbl9","avbl10","avbl11")


fit.values <- cbind.data.frame(available.months = NA,
                               mean = NA,
                               slope = NA,
                               intercept = NA,
                               rsq = NA)
for(i in 2:12){
  mean <- mean(proc7[,i])
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


# ggplot(data = proc7) +
#   geom_line(aes(rnd.obsmean, avbl1, colour = "1"))+
#   geom_line(aes(rnd.obsmean, avbl2, colour = "2"))+
#   geom_line(aes(rnd.obsmean, avbl3, colour = "3"))+
#   geom_line(aes(rnd.obsmean, avbl4, colour = "4"))+
#   geom_line(aes(rnd.obsmean, avbl5, colour = "5"))+
#   geom_line(aes(rnd.obsmean, avbl6, colour = "6"))+
#   geom_line(aes(rnd.obsmean, avbl7, colour = "7"))+
#   geom_line(aes(rnd.obsmean, avbl8, colour = "8"))+
#   geom_line(aes(rnd.obsmean, avbl9, colour = "9"))+
#   geom_line(aes(rnd.obsmean, avbl10, colour = "10"))+
#   geom_line(aes(rnd.obsmean, avbl11, colour = "11"))


#### normalised standard deviation as a function of start month: ####

proc8 <- dcast(proc2, formula= start.month ~ available.months,
               value.var = 'norm.sd',
               fun.aggregate = sd)

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

plot(fit.values2$available.months, fit.values2$rsq, type = "l")


ggplot(data = proc8) +
  geom_line(aes(start.month, avbl1, colour = "1"))+
  geom_line(aes(start.month, avbl2, colour = "2"))+
  geom_line(aes(start.month, avbl3, colour = "3"))+
  geom_line(aes(start.month, avbl4, colour = "4"))+
  geom_line(aes(start.month, avbl5, colour = "5"))+
  geom_line(aes(start.month, avbl6, colour = "6"))+
  geom_line(aes(start.month, avbl7, colour = "7"))+
  geom_line(aes(start.month, avbl8, colour = "8"))+
  geom_line(aes(start.month, avbl9, colour = "9"))+
  geom_line(aes(start.month, avbl10, colour = "10"))+
  geom_line(aes(start.month, avbl11, colour = "11"))

#### minimizing the error
vectora <- c(1:12)
fn <- function(v) {
  x = v[1]
  y = v[2]
  z = v[3]
  vectorc <- x*sin((vectora+y)*2*pi/12)+z
  return <- sum(abs(vectorb - vectorc)^2)
}


# proc8 <- proc8[complete.cases(proc8),]
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

# colors <- colorRampPalette(c("skyblue","darkgreen","yellow", "red"))(10)
# proc9$norm.sd <- ifelse(proc9$norm.sd>0.2, 0.2, proc9$norm.sd)
proc9$norm.sdfac <- factor(round((proc9$norm.sd/2),2)*2)
proc9$norm.sd.avbl <- proc9$norm.sd/proc9$available.months
N <- nlevels(proc9$norm.sdfac)



ggplot(proc9, aes(start.month,available.months,fill = norm.sdfac, z = norm.sdfac)) +
  geom_tile() +
  scale_fill_manual(values=colors, breaks=levels(proc9$norm.sdfac)[seq(1, N, by=1)]) +
  scale_y_continuous(breaks = seq(0,11,1), name = "Number of available monthly records") +
  scale_x_discrete(name = "Starting month of monitoring") +   
  theme_void() +
  theme(legend.text = element_text(size = 16),
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.margin = margin(2, 2, 2, 2, "cm"))


library(plotly)
proc10 <- spread(proc9[,1:3],key = start.month, value = norm.sd)
proc10 <- proc10[,-1]
proc10 <- as.matrix(proc10)

plot_ly(z = ~proc10) %>% add_contour(colors = colors)
plot_ly(z = ~proc10) %>% add_surface(colors = colors)
