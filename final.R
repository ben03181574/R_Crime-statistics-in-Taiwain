#------------------------------------------------------------#
install.packages("readxl")
install.packages("stringr")
install.packages("purrr")
install.packages("dplyr")
install.packages("reshape2")
install.packages("ggplot")
library(stringr)
library(readxl)
library(purrr)
library(dplyr)
library(reshape2)
library(ggplot2)
#----------------------前置作業
theFiles <- dir("data/",pattern="\\.xlsx")
numhappenweeklist107 <- vector("list")
numhappenweeklist108 <- vector("list")
numhappenweeklist109 <- vector("list")
numhappenweeklist107spring <- vector("list")
numhappenweeklist107summer <- vector("list")
numhappenweeklist107fall <- vector("list")
numhappenweeklist107winter <- vector("list")
numhappenweeklist108spring <- vector("list")
numhappenweeklist108summer <- vector("list")
numhappenweeklist108fall <- vector("list")
numhappenweeklist108winter <- vector("list")
numhappenweeklist109spring <- vector("list")
numhappenweeklist109summer <- vector("list")
numhappenweeklist109fall <- vector("list")
numhappenweeklist109winter <- vector("list")
spring <- c("2月","3月","4月")
summer <- c("5月","6月","7月")
fall <- c("8月","9月","10")
winter <- c("11","12","1月")
thecasename <- c("強盜","搶奪","強制性交","汽車竊盜","住宅竊盜","毒品","機車竊盜")
#----------------------函數
builddf <- function(x)
{
  data.frame(x)
}
builddf2 <- function(x)
{
  data.frame(case=thecasename,numhappen=x)
}
#----------------------讀取檔案
for(a in theFiles)
{
  temp <-read_excel(file.path("data",a))
  temp2<-str_replace_all(string=a,pattern="[a-zA-Z.]",replacement="")
  temp2<-paste("c",temp2,sep="")
  assign(x=temp2,value=temp)
#----------------------取得資料發生數一列
  temp3 <- temp[3,2:14]
  temp3 <-as.numeric(temp3)
#----------------------將不同年份、季節之發生數資料分別儲存在不同list
  temp4<-str_sub(string=temp2,start = 2,end = 4 )
  temp5<-str_sub(string=temp2,start = 6,end = 7 )
  if(temp4=="107")
  {
    numhappenweeklist107[[temp2]] <-temp3
    if(temp5 %in% spring)
    {
      numhappenweeklist107spring[[temp2]] <- temp3
    }else if(temp5 %in% summer)
    {
      numhappenweeklist107summer[[temp2]] <- temp3
    }
    else if(temp5 %in% fall)
    {
      numhappenweeklist107fall[[temp2]] <- temp3
    }
    else if(temp5 %in% winter)
    {
      numhappenweeklist107winter[[temp2]] <- temp3
    }
  }else if(temp4=="108")
  {
    numhappenweeklist108[[temp2]] <-temp3
    if(temp5 %in% spring)
    {
      numhappenweeklist108spring[[temp2]] <- temp3
    }else if(temp5 %in% summer)
    {
      numhappenweeklist108summer[[temp2]] <- temp3
    }
    else if(temp5 %in% fall)
    {
      numhappenweeklist108fall[[temp2]] <- temp3
    }
    else if(temp5 %in% winter)
    {
      numhappenweeklist108winter[[temp2]] <- temp3
    }
  }else if(temp4=="109")
  {
    numhappenweeklist109[[temp2]] <-temp3
    if(temp5 %in% spring)
    {
      numhappenweeklist109spring[[temp2]] <- temp3
    }else if(temp5 %in% summer)
    {
      numhappenweeklist109summer[[temp2]] <- temp3
    }
    else if(temp5 %in% fall)
    {
      numhappenweeklist109fall[[temp2]] <- temp3
    }
    else if(temp5 %in% winter)
    {
      numhappenweeklist109winter[[temp2]] <- temp3
    }
  }
}
#-----------------------計算各年度總和(利用map函數)
numhappenyear107<-numhappenweeklist107%>%map(function(x) sum(x,na.rm=TRUE))%>%map_df(builddf)%>%map(sum)
numhappenyear108<-numhappenweeklist108%>%map(function(x) sum(x,na.rm=TRUE))%>%map_df(builddf)%>%map(sum)
numhappenyear109<-numhappenweeklist109%>%map(function(x) sum(x,na.rm=TRUE))%>%map_df(builddf)%>%map(sum)
#-----------------------計算各年度季節總和(利用dplyr套件)
numhappenseason107spring <- sum(bind_cols(numhappenweeklist107spring),na.rm=TRUE)
numhappenseason107summer <- sum(bind_cols(numhappenweeklist107summer),na.rm=TRUE)
numhappenseason107fall <- sum(bind_cols(numhappenweeklist107fall),na.rm=TRUE)
numhappenseason107winter <- sum(bind_cols(numhappenweeklist107winter),na.rm=TRUE)

numhappenseason108spring <- sum(bind_cols(numhappenweeklist108spring),na.rm=TRUE)
numhappenseason108summer <- sum(bind_cols(numhappenweeklist108summer),na.rm=TRUE)
numhappenseason108fall <- sum(bind_cols(numhappenweeklist108fall),na.rm=TRUE)
numhappenseason108winter <- sum(bind_cols(numhappenweeklist108winter),na.rm=TRUE)

numhappenseason109spring <- sum(bind_cols(numhappenweeklist109spring),na.rm=TRUE)
numhappenseason109summer <- sum(bind_cols(numhappenweeklist109summer),na.rm=TRUE)
numhappenseason109fall <- sum(bind_cols(numhappenweeklist109fall),na.rm=TRUE)
numhappenseason109winter <- sum(bind_cols(numhappenweeklist109winter),na.rm=TRUE)

numhappenseasondf <- data.frame(numhappenseason107spring,numhappenseason107summer,
                                numhappenseason107fall,numhappenseason107winter,
                                numhappenseason108spring,numhappenseason108summer,
                                numhappenseason108fall,numhappenseason108winter,
                                numhappenseason109spring,numhappenseason109summer,
                                numhappenseason109fall,numhappenseason109winter)
numhappenseasondf <- melt(numhappenseasondf,variable.name = "season",value.name = "numhappen")
numhappenseasondf$season <- c("1071spring","1072summmer","1073fall","1074winter",
                              "1081spring","1082summmer","1083fall","1084winter",
                              "1091spring","1092summmer","1093fall","1094winter")
#-----------------------計算各年度各犯罪總和(利用dplyr套件)
numhappenweekdf107 <- bind_cols(numhappenweeklist107)
numhappenweekdf108 <- bind_cols(numhappenweeklist108)
numhappenweekdf109 <- bind_cols(numhappenweeklist109)
temp6 <-c("numhappenweekdf107","numhappenweekdf108","numhappenweekdf109")
num2 <-6

for (a in temp6) 
{
  templist <- vector("list")
  num <- 1
  num2 <- num2+1
  a <- eval(parse(text = a))
  for(b in 1:13)
  {
    if(num>7){
      break
    }else if(is.na(sum(a[b,]))){
      next
    }else{
      templist[thecasename[num]] <-sum(a[b,])
      num <- num+1
    }
  }
  tempname <- paste("numcase10",num2,sep="")
  templist <- templist %>% map_df(builddf)
  templist <- templist %>% map_df(builddf2)
  assign(x=tempname,value = templist)
}

#-----------------------將各犯罪各年發生數依照類型分類
names <- c("107","108","109")
test2 <- data.frame(names)
num3 <- c("1","2","3","4","5","6","7")

for(a in num3)
{
  test <- list(numcase107[a,],numcase108[a,],numcase109[a,])
  test <- test %>% map_df(builddf)
  temp7 <- paste("thecase",a,sep="")
  assign(x=temp7,value=cbind(test2,test))
}

allcase <- rbind(thecase1,thecase2,thecase3,thecase4,thecase5,thecase7)
#---------繪圖
ggplot(data=numhappenseasondf,aes(x=season,y=numhappen,group=1))+
  geom_line()+
  geom_text(aes(label=numhappen),size=6)
#---------繪圖
ggplot(data=numcase107,aes(x=case,y=numhappen)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=numhappen),size=8)+
  labs(title="107各項案件發生數")
ggplot(data=numcase108,aes(x=case,y=numhappen)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=numhappen),size=8)+
  labs(title="108各項案件發生數")
ggplot(data=numcase109,aes(x=case,y=numhappen)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=numhappen),size=8)+
  labs(title="109各項案件發生數")
#---------繪圖
ggplot(data=allcase,aes(x=names,y=numhappen,group=1))+
  geom_line(aes(color=case),size=2)+
  geom_text(aes(label=numhappen),size=6)+

  facet_wrap(~case)

ggplot(data=thecase6,aes(x=names,y=numhappen,group=1))+
  geom_line(aes(color=case),size=2)+
  geom_text(aes(label=numhappen),size=6)+
  facet_wrap(~case)
  






