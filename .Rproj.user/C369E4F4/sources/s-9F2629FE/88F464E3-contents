# Generate colour palette for EFTs 

library(tidyverse)


efts <- data.frame(
  id = paste0(seq(1,64,1),")"),
  productivity = c(rep("A", 16), rep("B", 16), rep("C", 16), rep("D", 16)), 
  seasonality = rep(c(rep("a",4), rep("b", 4), rep("c", 4), rep("d", 4)), 4), 
  phenology = rep(seq(1, 4, 1), 16), 
  r = c(96,141,164,58,65,57,46,85,78,85,81,75,0,56,69,68,rep(0,20),4,25,46,67,89,110,131,153,169,191,212,233,rep(255, 14),212,191),
  g = c(rep(0,16), 93, 114,135,157,178,199,220,242,rep(255, 25),233,212,191,170,148,127,106,85,63,42,21,rep(0,4)), 
  b = c(232,255,211,230,175,185,195,220,104,124,142,166,0,50,82,92,rep(255,8), 246, 225,203,187,165,144,123,105,78,59,28,16,rep(0,28))) 

efts <- cbind(efts, expand.grid(row = seq(1,16,1),column = seq(1,4,1)))

eftLeg <- efts %>% 
  unite("eft", c("productivity", "seasonality", "phenology"), sep="", remove = FALSE) %>% 
  unite("eftName", c("id", "eft"), sep = " ") %>% 
  mutate(eftColor = rgb(r = r,g = g,b = b, maxColorValue = 255))


ggplot(eftLeg, aes(y=-row, x=column, label=eftName, colour=factor(eftColor))) + 
  geom_text(fontface = "bold", size=4) + 
  scale_colour_manual(values = as.character(eftLeg$eftColor)) + 
  xlim(0.5,4.5) + 
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_rect(fill = "#969696")
        ) 



  


  




# Plot 

colCount <- 4 # number per row
rowCount <- 16
par(bg=EFTBackground, mfrow=c(1,1), xpd=NA, xaxs="r", pin=c(3,5), adj=0,cex=1)
plot( c(1,colCount), c(0,rowCount), type="n", ylab="", xlab="",
      axes=FALSE, ylim=c(rowCount,0),
      main=rbind("","","A-D: Productivity (increasing)","a-d: Seasonality (decreasing)","1-4: Phenology (Sp-Sm-Au-Wi)"))
par(cex=1)

j <-1
for (col in 1:colCount){
  for (row in 1:rowCount){
    text(col,row, paste(EFTNames[j]), cex=1.1, col=EFTColourPalette[j])
    j<-j+1
  }
}










EFTNames <- cbind("1) Aa1",   "2) Aa2",   "3) Aa3", 	"4) Aa4", 	"5) Ab1", 	"6) Ab2", 	"7) Ab3", 	"8) Ab4", 	"9) Ac1", 	"10) Ac2", 	"11) Ac3", 	"12) Ac4", 	"13) Ad1", 	"14) Ad2", 	"15) Ad3", 	"16) Ad4", 	"17) Ba1", 	"18) Ba2", 	"19) Ba3", 	"20) Ba4", 	"21) Bb1", 	"22) Bb2", 	"23) Bb3", 	"24) Bb4", 	"25) Bc1", 	"26) Bc2", 	"27) Bc3", 	"28) Bc4", 	"29) Bd1", 	"30) Bd2", 	"31) Bd3", 	"32) Bd4", 	"33) Ca1", 	"34) Ca2", 	"35) Ca3", 	"36) Ca4", 	"37) Cb1", 	"38) Cb2", 	"39) Cb3", 	"40) Cb4", 	"41) Cc1", 	"42) Cc2", 	"43) Cc3", 	"44) Cc4", 	"45) Cd1", 	"46) Cd2", 	"47) Cd3", 	"48) Cd4", 	"49) Da1", 	"50) Da2", 	"51) Da3", 	"52) Da4", 	"53) Db1", 	"54) Db2", 	"55) Db3", 	"56) Db4", 	"57) Dc1", 	"58) Dc2", 	"59) Dc3", 	"60) Dc4", 	"61) Dd1", 	"62) Dd2", 	"63) Dd3", 	"64) Dd4")

EFTBackground <- rgb(150,150,150, maxColorValue=255)   #   EFT   0) 000
EFTColourPalette <- cbind(rgb(96,0,232, maxColorValue=255),   #   EFT   1) Aa1
                          rgb(141,0,255, maxColorValue=255),   #   EFT   2) Aa2
                          rgb(164,0,211, maxColorValue=255),   #   EFT   3) Aa3
                          rgb(58,0,230, maxColorValue=255),   #   EFT   4) Aa4
                          rgb(65,0,175, maxColorValue=255),   #   EFT   5) Ab1
                          rgb(57,0,185, maxColorValue=255),   #   EFT   6) Ab2
                          rgb(46,0,195, maxColorValue=255),   #   EFT   7) Ab3
                          rgb(85,0,220, maxColorValue=255),   #   EFT   8) Ab4
                          rgb(78,0,104, maxColorValue=255),   #   EFT   9) Ac1
                          rgb(85,0,124, maxColorValue=255),   #   EFT   10) Ac2
                          rgb(81,0,142, maxColorValue=255),   #   EFT   11) Ac3
                          rgb(75,0,166, maxColorValue=255),   #   EFT   12) Ac4
                          rgb(0,0,0, maxColorValue=255),   #   EFT   13) Ad1
                          rgb(56,0,50, maxColorValue=255),   #   EFT   14) Ad2
                          rgb(69,0,82, maxColorValue=255),   #   EFT   15) Ad3
                          rgb(68,0,92, maxColorValue=255),   #   EFT   16) Ad4
                          rgb(0,93,255, maxColorValue=255),   #   EFT   17) Ba1 
                          rgb(0,114,255, maxColorValue=255),   #   EFT   18) Ba2
                          rgb(0,135,255, maxColorValue=255),   #   EFT   19) Ba3
                          rgb(0,157,255, maxColorValue=255),   #   EFT   20) Ba4
                          rgb(0,178,255, maxColorValue=255),   #   EFT   21) Bb1
                          rgb(0,199,255, maxColorValue=255),   #   EFT   22) Bb2
                          rgb(0,220,255, maxColorValue=255),   #   EFT   23) Bb3
                          rgb(0,242,255, maxColorValue=255),   #   EFT   24) Bb4
                          rgb(0,255,246, maxColorValue=255),   #   EFT   25) Bc1
                          rgb(0,255,225, maxColorValue=255),   #   EFT   26) Bc2 
                          rgb(0,255,203, maxColorValue=255),   #   EFT   27) Bc3
                          rgb(0,255,187, maxColorValue=255),   #   EFT   28) Bc4
                          rgb(0,255,165, maxColorValue=255),   #   EFT   29) Bd1
                          rgb(0,255,144, maxColorValue=255),   #   EFT   30) Bd2
                          rgb(0,255,123, maxColorValue=255),   #   EFT   31) Bd3
                          rgb(0,255,105, maxColorValue=255),   #   EFT   32) Bd4
                          rgb(0,255,78, maxColorValue=255),   #   EFT   33) Ca1
                          rgb(0,255,59, maxColorValue=255),   #   EFT   34) Ca2
                          rgb(0,255,38, maxColorValue=255),   #   EFT   35) Ca3
                          rgb(0,255,16, maxColorValue=255),   #   EFT   36) Ca4
                          rgb(4,255,0, maxColorValue=255),   #   EFT   37) Cb1
                          rgb(25,255,0, maxColorValue=255),   #   EFT   38) Cb2
                          rgb(46,255,0, maxColorValue=255),   #   EFT   39) Cb3
                          rgb(67,255,0, maxColorValue=255),   #   EFT   40) Cb4
                          rgb(89,255,0, maxColorValue=255),   #   EFT   41) Cc1
                          rgb(110,255,0, maxColorValue=255),   #   EFT   42) Cc2
                          rgb(131,255,0, maxColorValue=255),   #   EFT   43) Cc3
                          rgb(153,255,0, maxColorValue=255),   #   EFT   44) Cc4
                          rgb(169,255,0, maxColorValue=255),   #   EFT   45) Cd1
                          rgb(191,255,0, maxColorValue=255),   #   EFT   46) Cd2
                          rgb(212,255,0, maxColorValue=255),   #   EFT   47) Cd3
                          rgb(233,255,0, maxColorValue=255),   #   EFT   48) Cd4
                          rgb(255,255,0, maxColorValue=255),   #   EFT   49) Da1
                          rgb(255,233,0, maxColorValue=255),   #   EFT   50) Da2 
                          rgb(255,212,0, maxColorValue=255),   #   EFT   51) Da3
                          rgb(255,191,0, maxColorValue=255),   #   EFT   52) Da4
                          rgb(255,170,0, maxColorValue=255),   #   EFT   53) Db1
                          rgb(255,148,0, maxColorValue=255),   #   EFT   54) Db2
                          rgb(255,127,0, maxColorValue=255),   #   EFT   55) Db3
                          rgb(255,106,0, maxColorValue=255),   #   EFT   56) Db4
                          rgb(255,85,0, maxColorValue=255),   #   EFT   57) Dc1
                          rgb(255,63,0, maxColorValue=255),   #   EFT   58) Dc2
                          rgb(255,42,0, maxColorValue=255),   #   EFT   59) Dc3
                          rgb(255,21,0, maxColorValue=255),   #   EFT   60) Dc4
                          rgb(255,0,0, maxColorValue=255),   #   EFT   61) Dd1
                          rgb(233,0,0, maxColorValue=255),   #   EFT   62) Dd2
                          rgb(212,0,0, maxColorValue=255),   #   EFT   63) Dd3
                          rgb(191,0,0, maxColorValue=255))   #   EFT   64) Dd4




plot



colCount <- 4 # number per row
rowCount <- 16
par(bg=EFTBackground, mfrow=c(1,1), xpd=NA, xaxs="r", pin=c(3,5), adj=0,cex=1)
plot( c(1,colCount), c(0,rowCount), type="n", ylab="", xlab="",
      axes=FALSE, ylim=c(rowCount,0),
      main=rbind("","","A-D: Productivity (increasing)","a-d: Seasonality (decreasing)","1-4: Phenology (Sp-Sm-Au-Wi)"))
par(cex=1)

j <-1
for (col in 1:colCount)
{
  for (row in 1:rowCount)
  {
    text(col,row, paste(EFTNames[j]), cex=1.1, col=EFTColourPalette[j])
    j<-j+1
  }
}






