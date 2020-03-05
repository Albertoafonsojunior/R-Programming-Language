setwd("C:/Users/aajr/Desktop/dirty/bioclimatology")

brain <- read.csv('rain.csv',sep = ',', dec = '.')
colnames(brain) <- c("Mounth","OUTUBRO","NOVEMBRO","DEZEMBRO","JANEIRO","FEVEREIRO","MARCO")
colr <- c("RoyalBlue","salmon","green","brown4","grey","red")
bp <- barplot(height = as.matrix(brain[,-1]),
        width = 1,beside = T,horiz = F,cex.names = 0.8,
        cex.axis = 0.8,main = "Epoca Chuvosa 2017/2018",
        col=colr,ylim = c(0,450))
text(x = bp,y = brain[,-1],labels = round(brain[,-1],0),pos = 4,xpd=1)

axis(4, at = c(45.2,110.8,235.9,260.9,278.1,280.0),
     col.ticks = "red",labels = c("O","N","D","J","F","M"))
abline(h = 0)
text(1,3, "45.2", col = 4, adj = c(-.4, -1.3))
text(1,3, "110.8", col = 4, adj = c(-2.5, -2))
text(1,3, "235.9", col = 4, adj = c(-5, -7.7))
text(1,3, "260.9", col = 4, adj = c(-7.5, -8.5))
text(1,3, "278.1", col = 4, adj = c(-10, -9))
text(1,3, "280", col = 4, adj = c(-19, -9))

#abline(h=c(45.2,110.8,235.9,260.9,278.1,280.0),v=0,lty=1)
#legend(x = ,legend = c("O","N","D","J","F","M"), col = colr)


x <- read.csv('23.csv',sep = ',', dec = '.')
# colr <- c("RoyalBlue","chartreuse1","deeppink","gold4","darkslateblue")
colr <- c("lightblue")

bp <- 
  barplot(height = as.matrix(x[,-1]), 
          font.axis = 1, col.axis = "blue",
          width = 1,beside = T,horiz = F,
          cex.names = .99,cex.axis = 0.9,
          main = "Precipitação registada de 23/24 de Fevereiro de 2020",
          col=colr,ylim = c(0,160), las = 2,border="white",
          cex.lab = 1.1, ylab = "Precipitação (mm)",
          xlab = "Sofala", font.axis = 1, col.lab = "red")

axis(1,at=c(0,95),pos=-5)

y<-as.matrix(x[,-1])
text(bp,y+2,labels=as.character(y), font = 2)

# legend("top", c("Dia 12", "Dia 13","Dia 14", "Dia 15","Dia 16"), bty = "n",
#       col=colr, lwd=05)

box()

##################################################################
employee <- c('John Doe','Peter Gynn','Jolie Hope')
salary <- c(21000, 23400, 26800)
startdate <- as.Date(c('2010-11-1','2008-3-25','2007-3-14'))
employ.data <- data.frame(employee, salary, startdate)
str(employ.data)
employ.data