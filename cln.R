#library
Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "Portuguese")

library(latticeExtra)
library(lubridate)
library(ggplot2)

setwd("C:/Users/aajr/Desktop/dirty/graphs/")

x <- read.csv('beira.csv',sep = ',', dec = '.')
z <- x[1,]
p <- z[, c(9:12, 2:8)]
#colr <- rainbow(n = 12)
#colr <- c("RoyalBlue","chartreuse1","deeppink","gold4","darkslateblue")

colr <- c("red","lightBlue","lightBlue","lightBlue",
          "red","lightBlue","lightBlue","lightBlue",
          "lightBlue","lightBlue","lightBlue","lightBlue")
bp <- 
  barplot(height = as.matrix(p[,-1]), 
          font.axis = 1, col.axis = "blue",
          width = 1,beside = T,horiz = F,
          cex.names = .99,cex.axis = 0.9,
          main = "Média Climatológica para Precipitação",
          col=colr,ylim = c(0,300), las = 2,border="white",
          cex.lab = 1.1, ylab = "Precipitação (mm)",
          xlab = "1971-2000", font.axis = 1, col.lab = "black")

axis(1,at=c(0,95),pos=0)

y<-as.matrix(p[,-1])
text(bp,y+2,labels=as.character(y), font = 2)
box()
#legend("top", c("Dia 12", "Dia 13","Dia 14", "Dia 15","Dia 16"), bty = "n",
#      col=colr, lwd=05)

#################################################################


dat <- read.csv("temperature.csv",sep = ',', dec = '.')
dat[,2:3] <- round(dat[,c(2,3)],1)
names(dat) <- c("meses","Tmaxima", "Tminima") 
dat <- dat[,-1]

Some_date <- c("01/01/1979","03/02/1979","01/03/1979",
               "01/04/1979","05/05/1979","14/06/1979",
               "01/07/1979","23/08/1979","01/09/1979",
               "11/10/1979","08/11/1979","01/12/1979") 

Some_date <- as.Date(Some_date, format = "%d/%m/%Y", "GMT")
dat <- cbind(Some_date, dat)
dat$Some_date <- as.Date(dat$Some_date, format = "%d/%m/%Y", "GMT")

g <- ggplot(data=dat)+
  geom_line(mapping=aes(y=dat$Tmaxima,x= dat$Some_date,color="Máxima"),size=1.5 ) +
  geom_line(mapping=aes(y=dat$Tminima,x= dat$Some_date,color="Mínima"),size=1.5) +
  geom_point(data = dat, 
             mapping = aes(y=dat$Tmaxima,x= dat$Some_date),size=2)+
  geom_point(data = dat,
             mapping = aes(y=dat$Tminima,x= dat$Some_date),size=2)+
  scale_color_manual(values = c('Máxima' = 'red',
                                'Mínima' = 'darkblue')) + labs(color = 'Temperatura')+
  ggtitle("Variação intra-anual da temperatura") + 
  xlab("1971 - 2000") + ylab("Temperatura (\u00B0C)")

g + 
  scale_x_date(date_breaks = "months",date_labels = toupper("%B"))+
  theme(axis.text=element_text(size=14),
        axis.text.x = element_text(angle=90, color = "black"),
        plot.title = 
          element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14))
