library(tidyverse)
library(rvest)
library(dbplyr)
library(dtplyr)

## problem(A)
html <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
table1 = html%>%html_table()
data = data.frame(table1[1])



## problem(B)
html01=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/healthcare/pharmaceuticals-drugs/sun-pharma-inds/company-info")
j=html01%>%html_table()
j1=data.frame(j[1])  
j1<-j1[-c(1,2,3,4,5),]
j1<-j1[,-c(12,13,14)]
j2=data.frame(j[3])
j2<-j2[,-c(12,13)]
j1 <- j1 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
j2 <- j2 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
j=rbind(j1,j2)
j<-j[-c(9),]
row.names(j)=c(1:14)


html02=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/transportation-logistics/port/adani-ports-special/company-info")
l=html02%>%html_table()
l1=data.frame(l[1])  
l1<-l1[-c(1,2,3,4,5),]
l1<-l1[,-c(12,13,14)]
l2=data.frame(l[3])
l2<-l2[,-c(12,13)]
l1 <- l1 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
l2 <- l2 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
l=rbind(l1,l2)
l<-l[-c(9),]
row.names(l)=c(1:14)


html03=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/automobiles/automobile-two-three-wheelers/eicher-motors/company-info")
k=html02%>%html_table()
k1=data.frame(k[1])  
k1<-k1[-c(1,2,3,4,5),]
k1<-k1[,-c(12,13,14)]
k2=data.frame(k[3])
k2<-k2[,-c(12,13)]
k1 <- k1 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
k2 <- k2 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
k=rbind(k1,k2)
k<-k[-c(9),]
row.names(k)=c(1:14)


html04=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/bfsi/finance-nbfc/bajaj-finance/company-info")
i=html02%>%html_table()
i1=data.frame(i[1])  
i1<-i1[-c(1,2,3,4,5),]
i1<-i1[,-c(12,13,14)]
i2=data.frame(i[3])
i2<-i2[,-c(12,13)]
i1 <- i1 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
i2 <- i2 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
i=rbind(i1,i2)
i<-i[-c(9),]
row.names(i)=c(1:14)


html05=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/healthcare/pharmaceuticals-drugs/divis-lab/company-info")
g=html02%>%html_table()
g1=data.frame(g[1])  
g1<-g1[-c(1,2,3,4,5),]
g1<-g1[,-c(12,13,14)]
g2=data.frame(g[3])
g2<-g2[,-c(12,13)]
g1 <- g1 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
g2 <- g2 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
g=rbind(g1,g2)
g<-g[-c(9),]
row.names(g)=c(1:14)



##problem(C)_part(1)
tennis<-function(p){
  a=rbinom(n=5,size=1,prob=p)
  no_of_match=0
  wins=0
  loses=0
  for(i in a){
    if(wins>=3) {return(no_of_match)}
    if(loses>=3) {return(no_of_match)}
    if(i==1){
      wins=wins+1
    }
    else{
      loses=loses+1
    }
    no_of_match=no_of_match+1
    
  }
  return(no_of_match)
  
}


##problem(C)_part(2)
games = numeric(length=1000)
for(i in 1:1000){
  games[i] <- tennis(0.7)
}
answer <-mean(games)



##problem(D)
MontyHall = function(){
  selected=sample(x=c(1,2,3),size = 1)
  if(selected==1) {
    return(1)
  }
  if(selected==2) {
    return(1)
  }
  else {
    return(0)
  }
  if(selected==3){
    return(1)
  }
  else {
    return(0)
  }
}

result = numeric(length=1000)
for(i in 1:1000) 
{
  result[i] = MontyHall()
}
answ = sum(result)/1000




##problem(E)
html3 = read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
a=html3%>%html_table()
Rank=html3%>%html_elements(".countdown-index")%>%html_text()
name_of_movie=html3%>%html_elements(".article_movie_title a")%>%html_text()
tscore=html3%>%html_elements(".tMeterScore")%>%html_text()
year=html3%>%html_elements(".start-year")%>%html_text()

data3=data.frame(Ranking=rank, Movie=name_of_movie,Tomato_Score=tscore,Year=year)