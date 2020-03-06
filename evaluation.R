
#library
Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "Portuguese")

library(latticeExtra)
library(lubridate)
library(ggplot2)

setwd("C:/Users/aajr/Documents/p")

Mes <- c('Janeiro','Fevereiro','Março','Abril','Maio','Junho',
         'Julho','Agosto','Setembro','Outubro','Novembro','Dezembro')
Temperatura_Maxima <- c(31.5,31.1,30.7,29.4,27.9,26.2,25.4,26.3,27.8,28.9,30.3,31.0)
Temperatura_Minima <- c(23.9,23.9,23.4,21.5,18.8,16.3,15.7,16.6,18.7,20.4,22.1,23.2)
Precipitacao <- c(253.2,263.6,236.8,118.5,65.1,35.2,33.7,33.2,24.5,47.6,114.5,209.0)
datbeira <- data.frame(Mes,Temperatura_Maxima,Temperatura_Minima,Precipitacao)
dat = setNames(data.frame(t(datbeira[,-1])), datbeira[,1])
pluv <- dat[3,c(9:12, 2:8)]

colr <- c("red","lightBlue","lightBlue","lightBlue",
          "red","lightBlue","lightBlue","lightBlue",
          "lightBlue","lightBlue","lightBlue","lightBlue")
bp <- 
  barplot(height = as.matrix(pluv), 
          font.axis = 1, col.axis = "blue",
          width = 1,beside = T,horiz = F,
          cex.names = .99,cex.axis = 0.9,
          main = "Média Climatológica para Precipitação",
          col=colr,ylim = c(0,300), las = 2,border="white",
          cex.lab = 1.1, ylab = "Precipitação (mm)",
          xlab = "1971-2000", font.axis = 1, col.lab = "black")

axis(1,at=c(0,95),pos=0)

y<-as.matrix(pluv)
text(bp,y+2,labels=as.character(y), font = 1.8)
box()

# legend("top", c("Dia 12", "Dia 13","Dia 14", "Dia 15","Dia 16"), bty = "n",
#      col=colr, lwd=05)

##################################################################

dat <- datbeira[,c(2,3)]
Some_date <- c("01/01/1979","03/02/1979","01/03/1979",
               "01/04/1979","05/05/1979","14/06/1979",
               "01/07/1979","23/08/1979","01/09/1979",
               "11/10/1979","08/11/1979","01/12/1979") 

Some_date <- as.Date(Some_date, format = "%d/%m/%Y", "GMT")
dat <- cbind(Some_date, dat)
dat$Some_date <- as.Date(dat$Some_date, format = "%d/%m/%Y", "GMT")

g <- ggplot(data=dat)+
  geom_line(mapping=aes(y=dat$Temperatura_Maxima,x= dat$Some_date,color="Máxima"),size=1.5 ) +
  geom_line(mapping=aes(y=dat$Temperatura_Minima,x= dat$Some_date,color="Mínima"),size=1.5) +
  geom_point(data = dat, 
             mapping = aes(y=dat$Temperatura_Maxima,x= dat$Some_date),size=2)+
  geom_point(data = dat,
             mapping = aes(y=dat$Temperatura_Minima,x= dat$Some_date),size=2)+
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
###################################################################


seasonalbeira <- c(19.4,156.1,379.4,49.5,395.3,305.9)
Mes <- c('Outubro','Novembro','Dezembro','Janeiro','Fevereiro','Março')
dat <- data.frame(Mes,seasonalbeira)
pluv = setNames(data.frame(t(dat[,-1])), dat[,1])

colr <- c("RoyalBlue","salmon","green","brown4","grey","red")
bp <- barplot(height = as.matrix(pluv),
        width = 1,beside = T,horiz = F,cex.names = 0.8,
        cex.axis = 0.8,main = "Epoca Chuvosa 2017/2018",
        col=colr,ylim = c(0,450), las = 1, font.axis = 2)

axis(1,at=c(0,95),pos=0)

y<-as.matrix(pluv)
text(bp,y+2,labels=as.character(y), font = 2)
box()


# text(x = bp,y = brain[,-1],labels = round(brain[,-1],0),pos = 4,xpd=1)

# axis(4, at = c(45.2,110.8,235.9,260.9,278.1,280.0),
#     col.ticks = "red",labels = c("O","N","D","J","F","M"))
# abline(h = 0)
# text(1,3, "45.2", col = 4, adj = c(-.4, -1.3))
# text(1,3, "110.8", col = 4, adj = c(-2.5, -2))
# text(1,3, "235.9", col = 4, adj = c(-5, -7.7))
# text(1,3, "260.9", col = 4, adj = c(-7.5, -8.5))
# text(1,3, "278.1", col = 4, adj = c(-10, -9))
# text(1,3, "280", col = 4, adj = c(-19, -9))

# abline(h=c(45.2,110.8,235.9,260.9,278.1,280.0),v=0,lty=1)
# legend(x = ,legend = c("O","N","D","J","F","M"), col = colr)


Sofala <- c(150.5,55.0,53.5,51.0,50.2,30.2,14.0,9.8,4.0,0.0,0.0,0.0,0.0)
Distritos <- c("Gorongosa","Machanga","Beira","Dondo","Maring","Chibaba",
               "Muanza","Buzi","Inhamin","Marrom","Chemba","Caia","Nhamata")
dat <- data.frame(Distritos,Sofala)
pluv = setNames(data.frame(t(dat[,-1])), dat[,1])

colr <- c("lightblue")

bp <- 
  barplot(height = as.matrix(pluv), 
          font.axis = 1, col.axis = "blue",
          width = 1,beside = T,horiz = F,
          cex.names = .99,cex.axis = 0.9,
          main = "Precipitação registada de 23/24 de Fevereiro de 2020",
          col=colr,ylim = c(0,160), las = 2,border="white",
          cex.lab = 1.1, ylab = "Precipitação (mm)",
          xlab = "Sofala", font.axis = 1, col.lab = "black")

axis(1,at=c(0,95),pos=-5)

y<-as.matrix(pluv)
text(bp,y+2,labels=as.character(y), font = 2)
box()

# legend("top", c("Dia 12", "Dia 13","Dia 14", "Dia 15","Dia 16"), bty = "n",
#       col=colr, lwd=05)
##################################################################

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

