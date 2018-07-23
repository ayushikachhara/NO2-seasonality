## how many months and which months are required for apt estimation of annual mean?

library(data.table)
library(dplyr)
library(zoo)
library(ggplot2)
library(tidyr)
library(lubridate)

path <- "S:/kachharaa/NZTA NO2 seasonality/"
setwd(path)


### function definition: ####

vecrm <- function(rm, month) {
  f5 <- cbind.data.frame(Month_f = f2$Month_f, Year = f2$Year, SlopeForced = f2$SlopeForced)
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


## Step 1: find sites with full year records and also the year.

no2.file <- read.csv("nationalNO2_2007to2016.csv")
seasonal.factor <- read.csv("ouputs/seasonaladjustment3.csv")
seasonal.factor <- seasonal.factor[,c(2:3)]
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

all.table <- cbind.data.frame("Month_f" = NA,
                              "Year" = NA,
                              "SlopeForced" = NA, 
                              "NO2" = NA,         
                              "seas.NO2" = NA,    
                              "start.hole" = NA,
                              "no.holes" = NA,    
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
    f2 <- merge(f2,seasonal.factor, by = "Month_f")
    f2$Month_f <- factor(f2$Month_f,
                         levels =c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul", 
                                   "Aug", "Sep", "Oct", "Nov", "Dec"))
    f2 <- f2[order(f2$Month_f),]
    
    for(i in 1:12) {
      for(j in 0:10){
        f3 <- vecrm(j,i)
        all.table <- rbind(all.table, f3)
      }
    }
  }
  print(as.character(unique(fileName$Site.ID)))
}

  



# all.table1 <- all.table[complete.cases(all.table),]
# save(all.table1,file = "holepatchprocessed.RData")

load("holepatchprocessed.RData")

proc <- all.table1 %>% distinct(Site.ID,Year,no.holes, start.hole, calc.ann, .keep_all = TRUE)

# write.csv(proc, "sequentialholepatchmodel.csv")
proc2 <- cbind.data.frame(Site.ID = proc$Site.ID, Year = proc$Year, start.month = proc$start.hole, 
                          no.holes = proc$no.holes,calc.ann = proc$calc.ann, obs.ann = proc$obs.ann)

proc2$sd <- proc2$calc.ann - proc2$obs.ann

proc3 <- dcast(proc2, formula= Site.ID + Year + start.month + obs.ann ~ no.holes,
               value.var = 'calc.ann',
               fun.aggregate = mean)

# write.csv(proc3, "sequentialholepatchmodel.csv")

# proc3$sd.1 <- proc3$`1` - proc3$obs.ann 
# proc3$sd.2 <- proc3$`2` - proc3$obs.ann 
# proc3$sd.3 <- proc3$`3` - proc3$obs.ann 
# proc3$sd.4 <- proc3$`4` - proc3$obs.ann 
# proc3$sd.5 <- proc3$`5` - proc3$obs.ann 
# proc3$sd.6 <- proc3$`6` - proc3$obs.ann 
# proc3$sd.7 <- proc3$`7` - proc3$obs.ann 
# proc3$sd.8 <- proc3$`8` - proc3$obs.ann 
# proc3$sd.9 <- proc3$`9` - proc3$obs.ann 
# proc3$sd.10 <- proc3$`10` - proc3$obs.ann 
# proc3$sd.11 <- proc3$`11` - proc3$obs.ann 


proc4 <- proc2 %>% group_by(Site.ID, no.holes) %>%
  dplyr::summarise(obs.ann = mean(obs.ann, na.rm = T),
                   calc.ann = mean(calc.ann, na.rm = T),
                   est.sd = sd(sd,na.rm = T))

ggplot(proc4[which(proc4$no.holes == 1),]) +
  geom_point(aes(x =obs.ann, y = est.sd, color = as.factor(no.holes)), size = 3) +theme_bw()
  

proc5 <- proc2 %>% group_by(no.holes, start.month) %>%
  dplyr::summarise(obs.ann = mean(obs.ann, na.rm = T),
                   calc.ann = mean(calc.ann, na.rm = T),
                   est.sd = sd(sd,na.rm = T))

proc5$start.month <- factor(proc5$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                          "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(proc5) +
  geom_point(aes(x =no.holes, y = est.sd, color = as.factor(start.month)), size = 2) +theme_bw() +
  ggtitle("Stadard Deviation associated with estimating the annual mean \nbased on number of holes in the data")





#### working with proc2 ####
proc2$start.date <- paste0(proc2$Year,"-",proc2$start.month,"-01")
proc2$start.date <- as.Date(proc2$start.date, format = "%Y-%b-%d")
proc2$end.date <- floor_date(proc2$start.date + months(12-proc2$no.holes), "month") -1
proc2$totaldays <- proc2$end.date - proc2$start.date
proc2$mid.date <- proc2$start.date + (proc2$totaldays/2)
proc2$dayofyear <- yday(proc2$mid.date)

proc6 <- proc2 %>% group_by(totaldays, dayofyear) %>%
  dplyr::summarise(start.month = unique(start.month),
                   obs.ann = mean(obs.ann, na.rm = T),
                   calc.ann = mean(calc.ann, na.rm = T),
                   est.sd = sd(sd,na.rm = T))

proc6$start.month <- factor(proc6$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                          "Aug", "Sep", "Oct", "Nov", "Dec"))


ggplot(proc6) +
  geom_point(aes(x =as.numeric(totaldays), y = est.sd, color = factor(start.month)), size = 1) +theme_bw() +
  ggtitle("")+
  scale_x_continuous(breaks = seq(0,365,50), name = "Total number of days used") +
  scale_y_continuous(breaks = seq(0,6,0.5), name = "Estimated standard deviation") +
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 18),
        title = element_text(size = 18),
        legend.title=element_blank(),
        legend.text = element_text(size = 16))


