queen <- read.csv("holepatch_ann_mosaic_Queen.csv")
penrose <- read.csv("holepatch_ann_mosaic_Penrose.csv")

comparisons <- merge(queen,penrose, 
                     by = c("X","start.month","available.months"),
                     suffixes = c(".q",".p"))

comparisons$abs.diff <- round(comparisons$norm.sdfac.q - comparisons$norm.sdfac.p,2)
comparisons$rel.diff <- round(comparisons$norm.sdfac.q / comparisons$norm.sdfac.p,1)

comparisons$start.month <- factor(comparisons$start.month,
                                  levels = c("Jan" ,"Feb" ,"Mar", "Apr", "May", "Jun", "Jul",
                                             "Aug", "Sep", "Oct", "Nov", "Dec"))

########
ggplot(comparisons) +
  geom_tile(aes(start.month,available.months, fill = rel.diff))
 
ggplot(comparisons) +
  geom_tile(aes(start.month,available.months, fill = abs.diff))
