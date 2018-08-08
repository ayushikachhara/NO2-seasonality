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
# all.table <- all.table[-1,]

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

f1$Month <- factor(f1$Month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                        "Aug", "Sep", "Oct", "Nov", "Dec"))
f1$start.month <- factor(f1$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                        "Aug", "Sep", "Oct", "Nov", "Dec"))

##### reshaping the main dataset to get no2 observed: ####
no2.long <- reshape(nonna.no2, direction="long", varying=list(names(no2.file)[3:15]), v.names="no2", 
                    idvar=c("Site.ID","Year"), timevar= "Month", times = names(no2.file)[3:15])

f2 <- no2.long[,c(1,2,11,12,13)]
rownames(f2) <- c()
f2 <- f2[which(f2$Month != "Average"),]

f2$Month <- factor(f2$Month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                        "Aug", "Sep", "Oct", "Nov", "Dec"))

f3 <- merge(f1,f2, by = c("Site.ID","Year","Month"), all = T)


f3$start.month <- factor(f3$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                    "Aug", "Sep", "Oct", "Nov", "Dec"))

f3 <- f3[order(f3$start.month),]

#### calculating the estimated monthly data with modelled annual mean ####
f3$mod.NO2 <- ifelse(!is.na(f3$obs.NO2), f3$no2, f3$calc.ann/f3$seasonal.factor)
f3 <- f3[,-5]

# f4 <- f3[which(f3$available.months == 1),]

# f3 <- f4
# ggplot(f3[which(!is.na(f3$no2)),]) +
#   geom_point(aes(no2,mod.NO2, color = available.monthsfac), size = 1) +
#   scale_colour_brewer(palette = "Blues") +
#   theme_dark()+
#   scale_x_continuous(breaks = seq(0,70,5), limit = c(0,70),
#                      name = "Observed NO2")+
#   scale_y_continuous(breaks = seq(0,70,5), limit = c(0,70),
#                      name = "Modelled_NO2 (cosine)")+
#   theme(legend.text = element_text(size = 16),
#         legend.title = element_blank(),
#         axis.text = element_text(size = 16),
#         axis.title = element_text(size = 16))

## error in estimating monthly NO2 from modelled annual NO2 ####

d1 <- cbind.data.frame(Site.ID = f3$Site.ID, Year = f3$Year,
                       Month = f3$Month,
                       start.month = f3$start.month, 
                       available.months = f3$available.months,
                       calc.no2 = f3$mod.NO2, 
                       obs.no2 = f3$no2)

d1$sd <- d1$calc.no2 - d1$obs.no2
d1$key <- ifelse(d1$sd == 0, "Available", "Missing")

d2 <- dcast(d1, formula= Site.ID + Year + Month  + obs.no2 ~ available.months + start.month,
               value.var = 'calc.no2',
               fun.aggregate = function(x) mean(x, na.rm =T))


d3 <- d1 %>% group_by(Site.ID,  available.months, Month, start.month) %>%
  dplyr::summarise(obs.no2 = mean(obs.no2, na.rm = T),
                   calc.no2 = mean(calc.no2, na.rm = T),
                   est.sd = sd(sd,na.rm = T))

ggplot(d3) +
  geom_point(aes(x =obs.no2, y = est.sd, 
                 color = Month), size = 2) +theme_bw()

### error in the whole dataset as a function of available months and start month
d4 <- d1 %>% group_by(available.months, start.month, Month) %>%
  dplyr::summarise(obs.no2 = mean(obs.no2, na.rm = T),
                   calc.no2 = mean(calc.no2, na.rm = T),
                   est.sd = sd(sd,na.rm = T))

d4$start.month <- factor(d4$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                          "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(d4) +
  geom_point(aes(x = available.months, y = est.sd, size = as.factor(start.month))) +
  theme_bw() +
  ggtitle("Std Deviation in estimating monthly NO2 based on modelled annual means")


#### working with d1 ####
d1$start.date <- paste0(d1$Year,"-",d1$start.month,"-01")
d1$start.date <- as.Date(d1$start.date, format = "%Y-%b-%d")
d1$end.date <- floor_date(d1$start.date + months(d1$ available.months), "month") -1
d1$totaldays <- d1$end.date - d1$start.date
d1$mid.date <- d1$start.date + (d1$totaldays/2)
d1$dayofyear <- yday(d1$start.date)
d1$norm.sd <- d1$sd/d1$obs.no2
d1$rnd.obsmean <- round(d1$obs.no2/2,0)*2
# d1$start.month <- as.numeric(format(d1$start.date, format = "%m"))


### standard deviation as a function of observed annual mean (bands of 2 ug/m3) ####

d7 <- dcast(d1, formula= rnd.obsmean ~ available.months+start.month,
               value.var = 'norm.sd',
               fun.aggregate = sd)

# colnames(d7) <- c("rnd.obsmean", "avbl1", "avbl2","avbl3","avbl4",
#                      "avbl5","avbl6","avbl7","avbl8","avbl9","avbl10","avbl11")


fit.values <- cbind.data.frame(available.months = NA,
                               start.month = NA,
                               mean = NA,
                               slope = NA,
                               intercept = NA,
                               rsq = NA)
# i <- 2  
for(i in 2:133){
  mean <- mean(d7[,i])
  fit <- lm(d7[,i]~d7[,1], data = d7)
  intercept <- fit$coefficients[[1]]
  slope <- fit$coefficients[[2]]
  rsq <- summary(fit)$adj.r.squared
  
  fit.table <- cbind.data.frame(available.months = strsplit(colnames(d7)[i], "_")[[1]][1],
                                start.month = strsplit(colnames(d7)[i], "_")[[1]][2],
                                mean = mean,
                                slope = slope,
                                intercept = intercept,
                                rsq = rsq)
  
  fit.values <- rbind(fit.values,fit.table)
  
  
}
fit.values <- fit.values[-1,]



plot(fit.values$available.months, fit.values$rsq, type = "l")


ggplot(data = d7) +
  geom_line(aes(rnd.obsmean, `1_Jan`)) +
  geom_line(aes(rnd.obsmean, `2_Jan`)) +
  geom_line(aes(rnd.obsmean, `3_Jan`)) +
  geom_line(aes(rnd.obsmean, `4_Jan`)) +
  geom_line(aes(rnd.obsmean, `5_Jan`)) +
  geom_line(aes(rnd.obsmean, `6_Jan`)) +
  geom_line(aes(rnd.obsmean, `7_Jan`)) +
  geom_line(aes(rnd.obsmean, `8_Jan`)) +
  geom_line(aes(rnd.obsmean, `9_Jan`))
  


#### normalised standard deviation as a function of start month: ####

d8 <- dcast(d1, formula= start.month ~ available.months+Month,
               value.var = 'norm.sd',
               fun.aggregate = sd)

# colnames(d8)[3:13] <- c("avbl1", "avbl2","avbl3","avbl4",
#                      "avbl5","avbl6","avbl7","avbl8","avbl9","avbl10","avbl11")

d9 <- gather(d8, seq,sd, `1_Jan`:`11_Dec`, factor_key = T)

d9$seq <- as.character(d9$seq)
d9$available.months <- strsplit(d9$seq, "_")
d9$Month <- unlist(strsplit(d9$seq, "_"))[2]
d9 <- d9[which(d9$sd != 0),]

ggplot(d9) +
  geom_tile(aes(start.month,Month, fill = sd)) +
  theme(axis.text = element_text(angle = 0, size = 6))

fit.values2 <- cbind.data.frame(available.months = NA,
                                Month = NA,
                                mean = NA,
                                slope = NA,
                                intercept = NA,
                                rsq = NA)
for(i in 2:133){
  mean <- mean(d8[,i], na.rm = T)
  fit <- lm(d8[,i]~d8[,1], data = d8)
  intercept <- fit$coefficients[[1]]
  slope <- fit$coefficients[[2]]
  rsq <- summary(fit)$adj.r.squared
  
  fit.table <- cbind.data.frame(available.months = strsplit(colnames(d7)[i], "_")[[1]][1],
                                Month = strsplit(colnames(d7)[i], "_")[[1]][2],
                                mean = mean,
                                slope = slope,
                                intercept = intercept,
                                rsq = rsq)
  
  fit.values2 <- rbind(fit.values2,fit.table)
  
  
}
fit.values2 <- fit.values2[-1,]

plot(fit.values2$available.months, fit.values2$rsq)




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

vectorb <- d8$avbl1
modpram[1,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- d8$avbl2
modpram[2,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- d8$avbl3
modpram[3,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- d8$avbl4
modpram[4,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- d8$avbl5
modpram[5,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- d8$avbl6
modpram[6,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- d8$avbl7
modpram[7,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- d8$avbl8
modpram[8,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- d8$avbl9
modpram[9,2:4] <-  optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- d8$avbl10
modpram[10,2:4] <- optim(c(0.01,6.5, 0.02), fn)$par

vectorb <- d8$avbl11
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


d9 <- gather(d8, start.month, norm.sd, avbl1:avbl11, factor_key=TRUE)
colnames(d9) <- c("start.month","available.months","norm.sd")
d9$available.months <- rep(1:11, each = 12)

# colors <- colorRampPalette(c("skyblue","darkgreen","yellow", "red"))(10)
# d9$norm.sd <- ifelse(d9$norm.sd>0.2, 0.2, d9$norm.sd)
d9$norm.sdfac <- factor(round((d9$norm.sd/4),2)*4)
d9$norm.sd.avbl <- d9$norm.sd/d9$available.months
N <- nlevels(d9$norm.sdfac)
colors <- colorRampPalette(c("yellow", "darkgreen"))(10)


ggplot(d9, aes(start.month,available.months,fill = norm.sdfac, z = norm.sdfac)) +
  geom_tile() +
  scale_fill_manual(values=colors, breaks=levels(d9$norm.sdfac)[seq(1, N, by=1)]) +
  scale_y_continuous(breaks = seq(0,11,1), name = "Number of available monthly records") +
  scale_x_discrete(name = "Starting month of monitoring") +   
  ggtitle("Error assiciated with estimating monthly NO2 \nbased on modelled annual mean (cosine model)") +
  theme_void() +
  theme(legend.text = element_text(size = 16),
        title = element_text(size = 16),
        legend.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.margin = margin(2, 2, 2, 2, "cm"))


library(plotly)
d10 <- spread(d9[,1:3],key = start.month, value = norm.sd)
d10 <- d10[,-1]
d10 <- as.matrix(d10)

plot_ly(z = ~d10) %>% add_contour(colors = colors)
plot_ly(z = ~d10) %>% add_surface(colors = colors)

d9$start.month <- factor(d9$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                    "Aug", "Sep", "Oct", "Nov", "Dec"))

e9$start.month <- factor(e9$start.month, levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                                    "Aug", "Sep", "Oct", "Nov", "Dec"))
                                                    



write.csv(e9, "linear_model.csv")
write.csv(d9, "cosine_model.csv")
