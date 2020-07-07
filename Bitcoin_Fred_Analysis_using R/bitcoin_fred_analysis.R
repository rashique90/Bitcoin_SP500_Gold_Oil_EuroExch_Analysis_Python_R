library(data.table)
library(dplyr)
library(DBI)
library(tseries)
library(TSA)
library(vars)
library(ggplot2)
library(gtable)
library(grid)
library(forecast)

#use your own file to read the data
#bitcoin <- fread('bitcoin.csv')
#fred <- fread('fred.csv')
#bitcoin <- merge(bitcoin,fred,all=FALSE)
#summary(bitcoin)
bitcoin <- fread('all_combined.csv')

head(bitcoin)

bitcoin$date <- as.Date(bitcoin$date)

p1 <- ggplot(bitcoin,aes(x=date,y=bitcoin)) + geom_line() 
p2 <- ggplot(bitcoin,aes(x=date,y=sp500)) + geom_line()
p3 <- ggplot(bitcoin,aes(x=date,y=gold)) + geom_line()
p4 <- ggplot(bitcoin,aes(x=date,y=oil)) + geom_line()
p5 <- ggplot(bitcoin,aes(x=date,y=euro)) + geom_line()
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)
g5 <- ggplotGrob(p5)
g <- rbind(g1, g2, g3, g4, g5, size = "first")
g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths, g4$widths, g5$widths)
grid.newpage()
grid.draw(g)

summary(lm(bitcoin~sp500+gold+oil+euro,data=bitcoin))

rep.kpss <- function(series,alpha=0.05,dmax=5){
  diff <- 0
  for(i in 1:dmax){
    suppressWarnings(pval <- kpss.test(series,null="Level")$p.value)
    if(pval>=alpha){
      return(c(diff,0,pval))
    }
    suppressWarnings(pval <- kpss.test(series,null="Trend")$p.value)
    if(pval>=alpha){
      return(c(diff,1,pval))
    }
    diff <- diff + 1
    series <- diff(series)
  }
  return(NULL)
}

rep.kpss(bitcoin$bitcoin)
rep.kpss(bitcoin$sp500)
rep.kpss(bitcoin$gold)
rep.kpss(bitcoin$oil)
rep.kpss(bitcoin$euro)

n <- nrow(bitcoin)
summary(lm(diff(bitcoin)~diff(sp500)+diff(gold)+diff(oil)+diff(euro)+as.numeric(date)[2:n],data=bitcoin))

model0 <- lm(diff(bitcoin)~diff(sp500)+diff(gold)+diff(oil)+diff(euro)+as.numeric(date)[2:n],data=bitcoin)
coeftest(model0,vcov=NeweyWest(model0,lag=10))

bitcoin <- bitcoin[date>=as.Date('2017-01-01')]

p1 <- ggplot(bitcoin,aes(x=date,y=bitcoin)) + geom_line() 
p2 <- ggplot(bitcoin,aes(x=date,y=sp500)) + geom_line()
p3 <- ggplot(bitcoin,aes(x=date,y=gold)) + geom_line()
p4 <- ggplot(bitcoin,aes(x=date,y=oil)) + geom_line()
p5 <- ggplot(bitcoin,aes(x=date,y=euro)) + geom_line()
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)
g5 <- ggplotGrob(p5)
g <- rbind(g1, g2, g3, g4, g5, size = "first")
g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths, g4$widths, g5$widths)
grid.newpage()
grid.draw(g)


acf(diff(bitcoin$bitcoin))
acf(diff(bitcoin$sp500))
acf(diff(bitcoin$gold))
acf(diff(bitcoin$oil))
acf(diff(bitcoin$euro))

pacf(diff(bitcoin$bitcoin))
pacf(diff(bitcoin$sp500))
pacf(diff(bitcoin$gold))
pacf(diff(bitcoin$oil))
pacf(diff(bitcoin$euro))

auto.arima(bitcoin$bitcoin,d=1,max.p=10,max.q=10)

?forecast

model1 <- stats::arima(bitcoin$bitcoin, c(0,1,0))
forec1 <- forecast(model1,h=30)
plot(forec1)

bitcoin$weekday <- as.factor(weekdays(bitcoin$date))

n <- nrow(bitcoin)
summary(lm(diff(bitcoin)~weekday[2:n],data=bitcoin))
bitcoin$res[2:n] <- residuals(lm(diff(bitcoin)~weekday[2:n],data=bitcoin))


periodogram(bitcoin$res[2:n])


diff_jp <- function(x){
  n <- nrow(x)
  return(x[2:n,]-x[1:n-1,])
}
x <- bitcoin %>% dplyr::select(bitcoin,sp500,gold,oil,euro) %>% diff_jp
VAR(x,p=1,type="both") %>% AIC
VAR(x,p=2,type="both") %>% AIC
VAR(x,p=3,type="both") %>% AIC
VAR(x,p=4,type="both") %>% AIC
VAR(x,p=5,type="both") %>% AIC


model2 <- VAR(x,p=2,type="both")
library(broom)
summary(model2)


n <- nrow(bitcoin)
forec2 <- predict(model2,n.ahead=30)$fcst$bitcoin
forec2 <- forec2[,1]
forec2 <- bitcoin$bitcoin[n] + cumsum(forec2)
cbind(forec1$mean,forec2)


bitcoin$bitcoin[n]+cumsum(predict(model2,n.ahead=30)$fcst$bitcoin[,1])
