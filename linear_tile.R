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
# all.table2 <- all.table2[-1,]
all.table <- all.table[-1,]

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

g3$available.monthsfac <- factor(round(g3$available.months/2)*2)
ggplot(g3[which(!is.na(f3$no2)),]) +
  geom_point(aes(no2,mod.NO2, color = available.monthsfac), size = 1) +
  scale_colour_brewer(palette = "Blues") +
  theme_dark()+
  scale_x_continuous(breaks = seq(0,70,5), limit = c(0,70),
                     name = "Observed NO2")+
  scale_y_continuous(breaks = seq(0,70,5), limit = c(0,70),
                     name = "Modelled_NO2 (linear)")+
  theme(legend.text = element_text(size = 16),
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.margin = margin(2, 2, 2, 2, "cm"))

## error in estimating monthly NO2 from modelled annual NO2 ####

e1 <- cbind.data.frame(Site.ID = g3$Site.ID, Year = g3$Year,
                       start.month = g3$start.month, 
                       available.months = g3$available.months,
                       calc.no2 = g3$mod.NO2, obs.no2 = g3$no2)

e1$sd <- e1$calc.no2 - e1$obs.no2

e2 <- dcast(e1, formula= Site.ID + Year + start.month + obs.no2 ~ available.months,
            value.var = 'calc.no2',
            fun.aggregate = function(x) mean(x, na.rm =T))


e3 <- e1 %>% group_by(Site.ID,  available.months) %>%
  dplyr::summarise(obs.no2 = mean(obs.no2, na.rm = T),
                   calc.no2 = mean(calc.no2, na.rm = T),
                   est.sd = sd(sd,na.rm = T))

ggplot(e3) +
  geom_point(aes(x =obs.no2, y = est.sd, 
                 color = as.factor( available.months)), size = 2) +theme_bw()

### error in the whole dataset as a function of available months and start month
e4 <- e1 %>% group_by(available.months, start.month) %>%
  dplyr::summarise(obs.no2 = mean(obs.no2, na.rm = T),
                   calc.no2 = mean(calc.no2, na.rm = T),
                   est.sd = sd(sd,na.rm = T))

e4$start.month <- factor(e4$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                    "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(e4) +
  geom_point(aes(x = available.months, y = est.sd, color = as.factor(start.month)), size = 2) +
  theme_bw() +
  ggtitle("Std Deviation in estimating monthly NO2 based on modelled annual means")


#### working with d1 ####
e1$start.date <- paste0(e1$Year,"-",e1$start.month,"-01")
e1$start.date <- as.Date(d1$start.date, format = "%Y-%b-%d")
e1$end.date <- floor_date(e1$start.date + months(e1$ available.months), "month") -1
e1$totaldays <- e1$end.date - e1$start.date
e1$mid.date <- e1$start.date + (e1$totaldays/2)
e1$dayofyear <- yday(e1$start.date)
e1$norm.sd <- e1$sd/e1$obs.no2
e1$rnd.obsmean <- round(e1$obs.no2/2,0)*2
# d1$start.month <- as.numeric(format(d1$start.date, format = "%m"))


### standard deviation as a function of observed annual mean (bands of 2 ug/m3) ####

e7 <- dcast(e1, formula= rnd.obsmean ~ available.months,
            value.var = 'norm.sd',
            fun.aggregate = sd)

colnames(e7) <- c("rnd.obsmean", "avbl1", "avbl2","avbl3","avbl4",
                  "avbl5","avbl6","avbl7","avbl8","avbl9","avbl10","avbl11")


fit.values <- cbind.data.frame(available.months = NA,
                               mean = NA,
                               slope = NA,
                               intercept = NA,
                               rsq = NA)
for(i in 2:12){
  mean <- mean(e7[,i])
  fit <- lm(e7[,i]~e7[,1], data = e7)
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


ggplot(data = e7) +
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

e8 <- dcast(e1, formula= start.month ~ available.months,
            value.var = 'norm.sd',
            fun.aggregate = sd)

colnames(e8) <- c("start.month", "avbl1", "avbl2","avbl3","avbl4",
                  "avbl5","avbl6","avbl7","avbl8","avbl9","avbl10","avbl11")


fit.values2 <- cbind.data.frame(available.months = NA,
                                mean = NA,
                                slope = NA,
                                intercept = NA,
                                rsq = NA)
for(i in 2:12){
  mean <- mean(e8[,i], na.rm = T)
  fit <- lm(e8[,i]~e8[,1], data = e8)
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




#### minimizing the error
vectora <- c(1:12)
fn <- function(v) {
  x = v[1]
  y = v[2]
  z = v[3]
  vectorc <- x*sin((vectora+y)*2*pi/12)+z
  return <- sum(abs(vectorb - vectorc)^2)
}


# d8 <- d8[complete.cases(d8),]
modpram <- cbind.data.frame(available.months = c(1:11), amp = NA,
                            x_offset = NA, y_offset = NA)

vectorb <- e8$avbl1
modpram[1,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- e8$avbl2
modpram[2,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- e8$avbl3
modpram[3,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- e8$avbl4
modpram[4,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- e8$avbl5
modpram[5,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- e8$avbl6
modpram[6,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- e8$avbl7
modpram[7,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- e8$avbl8
modpram[8,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- e8$avbl9
modpram[9,2:4] <-  optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- e8$avbl10
modpram[10,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- e8$avbl11
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


e9 <- gather(e8, start.month, norm.sd, avbl1:avbl11, factor_key=TRUE)
colnames(e9) <- c("start.month","available.months","norm.sd")
e9$available.months <- rep(1:11, each = 12)

# colors <- colorRampPalette(c("skyblue","darkgreen","yellow", "red"))(10)
# d9$norm.sd <- ifelse(d9$norm.sd>0.2, 0.2, d9$norm.sd)
e9$norm.sdfac <- factor(round((e9$norm.sd/4),2)*4)
e9$norm.sd.avbl <- e9$norm.sd/e9$available.months
N <- nlevels(d9$norm.sdfac)
# colors <- colorRampPalette(c("lightblue", "darkred"))(10)


ggplot(e9, aes(start.month,available.months,fill = norm.sdfac, z = norm.sdfac)) +
  geom_tile() +
  scale_fill_manual(values=colors, breaks=levels(e9$norm.sdfac)[seq(1, N, by=1)]) +
  scale_y_continuous(breaks = seq(0,11,1), name = "Number of available monthly records") +
  scale_x_discrete(name = "Starting month of monitoring") +   
  ggtitle("Error assiciated with estimating monthly NO2 \nbased on modelled annual mean (linear model)") +
  theme_void() +
  theme(legend.text = element_text(size = 16),
        title = element_text(size = 16),
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.margin = margin(2, 2, 2, 2, "cm"))


library(plotly)
e10 <- spread(e9[,1:3],key = start.month, value = norm.sd)
e10 <- d10[,-1]
e10 <- as.matrix(d10)

plot_ly(z = ~e10) %>% add_contour(colors = colors)
plot_ly(z = ~e10) %>% add_surface(colors = colors)
