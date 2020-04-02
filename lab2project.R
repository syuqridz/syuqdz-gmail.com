library(readxlsb)
library(readxl)
library(ggthemes)
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(xts)
library(tibbletime)
library(scales)
library(gridExtra)
library(interval)
library(plotly)

#1. Compute the maximum and minimum power measured with respective device_id. 

#2. Identify the date and time range during which this measurement was taken

#3. Extract the number of measurements taken for individual device_id

#4. Extract and plot the power measured from individual device_id only within the 
#first week (Mon, Tue, Wed, Thur, Fri, Sat, Sun) of measurements

#5. Find the maximum power measured from all devices ONLY during the duration of 8am-10pm daily, excluding weekends (Saturday & Sunday)


nrow(data)
ncol(data)


#Pdata = read_xlsb(path = https://utpmy-my.sharepoint.com/:x:/g/personal/zahid_syuqri_23466_utp_edu_my/EQ8Bxr_QMihNpwIl5MOge4oBD57SrnAooSe0jMuxVvqoRg?e=KGY0W4("extdata", "TestBook.xlsb", package = "readxlsb"), range = "PORTFOLIO", debug = TRUE)


#read_excel("C:\Users\syuqd\OneDrive\Documents\activepowertotal.csv")
data<-read.csv("dataproject.csv",FALSE,",")
#DT <- data.table(data)

#setname
setnames(data, old = 'V1', new = 'ID')
setnames(data, old = 'V2', new = 'datetime')
setnames(data, old = 'V3', new = 'power')

#arrange
data = data %>% 
  arrange(ID)

data %>%
  group_by(ID)%>%
  summarise(id_count=n())
#number of measurement taken individual




data$datetime <- dmy_hm(data$datetime)    
#change date format

data_mentest <-filter(data,datetime>=ymd_hm("2019-08-05 8:00")& datetime<= ymd_hm("2019-08-23 22:00"))

data_mentest <-filter(data_mentest,!datetime >= ymd_hm("2019-08-6 7:00") | !datetime <= ymd_hm("2019-08-6 8:00"))
data_mentest <-filter(data_mentest,!datetime >= ymd_hm("2019-08-7 7:00") | !datetime <= ymd_hm("2019-08-7 8:00"))
data_mentest <-filter(data_mentest,!datetime >= ymd_hm("2019-08-8 7:00") | !datetime <= ymd_hm("2019-08-8 8:00"))
data_mentest <-filter(data_mentest,!datetime >= ymd_hm("2019-08-9 7:00") | !datetime <= ymd_hm("2019-08-9 8:00"))
data_mentest <-filter(data_mentest,!datetime >= ymd_hm("2019-08-12 7:00") | !datetime <= ymd_hm("2019-08-12 8:00"))
data_mentest <-filter(data_mentest,!datetime >= ymd_hm("2019-08-13 7:00") | !datetime <= ymd_hm("2019-08-13 8:00"))
data_mentest <-filter(data_mentest,!datetime >= ymd_hm("2019-08-14 7:00") | !datetime <= ymd_hm("2019-08-14 8:00"))
data_mentest <-filter(data_mentest,!datetime >= ymd_hm("2019-08-15 7:00") | !datetime <= ymd_hm("2019-08-15 8:00"))
data_mentest <-filter(data_mentest,!datetime >= ymd_hm("2019-08-16 7:00") | !datetime <= ymd_hm("2019-08-16 8:00"))
data_mentest <-filter(data_mentest,!datetime >= ymd_hm("2019-08-19 7:00") | !datetime <= ymd_hm("2019-08-19 8:00"))
data_mentest <-filter(data_mentest,!datetime >= ymd_hm("2019-08-20 7:00") | !datetime <= ymd_hm("2019-08-20 8:00"))
data_mentest <-filter(data_mentest,!datetime >= ymd_hm("2019-08-21 7:00") | !datetime <= ymd_hm("2019-08-21 8:00"))
data_mentest <-filter(data_mentest,!datetime >= ymd_hm("2019-08-22 7:00") | !datetime <= ymd_hm("2019-08-22 8:00"))
data_mentest <-filter(data_mentest,!datetime >= ymd_hm("2019-08-23 7:00") | !datetime <= ymd_hm("2019-08-23 8:00"))
#remove data from 7am to 8 am every day except weekend,

data_men <-filter(data_mentest,!datetime >= ymd_hm("2019-08-10 7:00") | !datetime <= ymd_hm("2019-08-11 22:00"))
#remove weekend "2019-08-10" "2019-08-11"

data_men <-filter(data_men,!datetime >= ymd_hm("2019-08-17 7:00") | !datetime <= ymd_hm("2019-08-18 22:00")) 
#remove weekend "2019-08-17" "2019-08-18"

datameasurement=data_men %>%
  group_by(ID)%>%
  summarise(ttlpower=sum(power))
#calculate total power measured from all device

data_max1<-filter(data_men,ID==1)
data_max=data_max %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max2<-filter(data_men,ID==2)
data_max2=data_max2 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max3<-filter(data_men,ID==3)
data_max3=data_max3 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max4<-filter(data_men,ID==4)
data_max4=data_max4 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max5<-filter(data_men,ID==5)
data_max5=data_max5 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max6<-filter(data_men,ID==6)
data_max6=data_max6 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max7<-filter(data_men,ID==7)
data_max7=data_max7 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max8<-filter(data_men,ID==8)
data_max8=data_max8 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max9<-filter(data_men,ID==9)
data_max9=data_max9 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max10<-filter(data_men,ID==10)
data_max10=data_max10 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max11<-filter(data_men,ID==11)
data_max11=data_max11 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max12<-filter(data_men,ID==12)
data_max12=data_max12 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max13<-filter(data_men,ID==13)
data_max13=data_max13 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max14<-filter(data_men,ID==14)
data_max14=data_max14 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max15<-filter(data_men,ID==15)
data_max15=data_max15 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max16<-filter(data_men,ID==16)
data_max16=data_max16 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max17<-filter(data_men,ID==17)
data_max17=data_max17 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max18<-filter(data_men,ID==18)
data_max18=data_max18 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max19<-filter(data_men,ID==19)
data_max19=data_max19 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max20<-filter(data_men,ID==20)
data_max20=data_max20 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max21<-filter(data_men,ID==21)
data_max21=data_max21 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max22<-filter(data_men,ID==22)
data_max22=data_max22 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max23<-filter(data_men,ID==23)
data_max23=data_max23 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max24<-filter(data_men,ID==24)
data_max24=data_max24 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max25<-filter(data_men,ID==25)
data_max25=data_max25 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max26<-filter(data_men,ID==26)
data_max26=data_max26 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max27<-filter(data_men,ID==27)
data_max27=data_max27 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_max28<-filter(data_men,ID==28)
data_max28=data_max28 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power))
# max power measure from all device duration of 8am-10 pm exclude weekend


#5. Find the maximum power measured from all devices ONLY during the duration of 8am-10pm daily, excluding weekends (Saturday & Sunday)
#10,11,17,18  ,"2019-08-11","2019-08-17","2019-08-18"


  

################ FAILED @not using @ try and error

#datadumb=data
#ata_id_test=data_id_1 %>% 
# select(ID,datetime,power)%>%
# select(datetime="2019-08-05"~"2019-08-11")
#ate_1a <- ymd_hm("2019-08-05 07:00")
#ate_1b <- ymd_hm("2019-08-11 07:00")
#ata_id_1$interval<-interval(date_1a, date_1b)
## filter_time('2019-08-05 ' ~ '2019-08-11 ')

#data_id_1_power=data_id_1%>%
#summarize(ID,powertotal=sum(power))
#data_day1<-filter(data_id_1,power)
#qplot(x=datetime, y=power,
#     data=data_id_1interval, na.rm=TRUE,
#     main="TIME VS POWER"
#     xlab="Datetime", ylab="power")
#p1<- ggplot(data_id_1interval,aes(x=datetime,y=power,color)) + geom_point(shape=1,size=1)
# p1
#l2<-p1 + geom_smooth(aes(group=1),method = 'lm',formula = y~log(x),se=FALSE,color='red')
#lot(pl2)
#just.dayofweek<-wday(data$datetime,label=TRUE)

#qplot(x=datetime, y=power,
#     data=data, na.rm=TRUE,
#    main="test",
#   xlab="Datetime", ylab="power")



#data_maxmin <- data %>% 
#group_by(ID) %>% 
# summarise_each(funs( min(.,na.rm=TRUE),max(.,na.rm=TRUE)),matches("power"))

#groupby
#data5=data %>%
#group_by(datetime)%>%
#summarize(date=sum(power[datetime >= "5-8-2019 7:00" & datetime <= "5-8-2019 9:00"]))

#get max@min of power desending



#ata_id_1$startTime <- as.Date("2019-08-05")
#ata_id_1$endTime <- as.Date("2019-08-11")
#ata_id_1$start.end <- c(data_id_1$startTime,data_id_1$endTime)
#tart.end

#ata_id_1interval<-filter(data_id_1,datatime="2019-08-05")


#ata_id_1interval<-filter(data_id_1,datetime %in% ymd_hm("2019-08-05 7:00"|"2019-08-11 7:00"))

#data_ta<-tbl_df(data)

#groupby //power usage each id over 18days
data2=data %>%
  group_by(ID)%>%
  summarise(totalpower=sum(power))


#groupby //powerusage each minute
data3=data %>%
  group_by(datetime)%>%
  summarize(date=sum(power))

#data_id_men$datetime <- ymd(data_id_men$datetime) 
#data_id_men <-filter(data_id_1men,!datetime >= ymd_hm("2019-08-10 7:00") && datetime < ymd_hm("2019-08-11 22:00"))


#################################################################

data_id_1<-filter(data,ID==1)
data_id_1=data_id_1 %>% 
  select(ID,datetime,power)%>%
  arrange(desc(power)) #TO GET HIGHEST POWER USAGE AND MINIMUN ACCORDING TO SORTHHIGHEST TO LOWEST

data_id_1 <- data_id_1%>% mutate(powertotal=sum(power))
just.dayofweek1<-wday(data_id_1$datetime,label=TRUE)

data_id_1interval<-filter(data_id_1,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p1<- plot_ly(x =data_id_1interval$datetime,Y=data_id_1interval$power , type="scatter", mode="markers", fill = "tonexty")
p1

#plot the power measured from individual device_id only within the 
#first week (Mon, Tue, Wed, Thur, Fri, Sat, Sun) of measurements


###########################################################################################
data_id_2<-filter(data,ID==2)

data_id_2=data_id_2 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_2 <- data_id_2%>% mutate(powertotal=sum(power))
just.dayofweek2<-wday(data_id_2$datetime,label=TRUE)

data_id_2interval<-filter(data_id_2,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p2<- plot_ly(x =data_id_2interval$datetime,Y=data_id_2interval$power , type="scatter", mode="markers", fill = "tonexty")
p2


###########################################################################################
data_id_3<-filter(data,ID==3)

data_id_3=data_id_3 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_3 <- data_id_3%>% mutate(powertotal=sum(power))
just.dayofweek3<-wday(data_id_3$datetime,label=TRUE)

data_id_3interval<-filter(data_id_3,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p3<- plot_ly(x =data_id_3interval$datetime,Y=data_id_3interval$power , type="scatter", mode="markers", fill = "tonexty")
p3

###########################################################################################

data_id_4<-filter(data,ID==4)

data_id_4=data_id_4 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_4 <- data_id_4%>% mutate(powertotal=sum(power))
just.dayofweek4<-wday(data_id_4$datetime,label=TRUE)

data_id_4inter<-filter(data_id_4,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p4<- plot_ly(x =data_id_4inter$datetime,Y=data_id_4inter$power , type="scatter", mode="markers", fill = "tonexty")
p4

###########################################################################################
data_id_5<-filter(data,ID==5)


data_id_5=data_id_5 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_5 <- data_id_5%>% mutate(powertotal=sum(power))
just.dayofweek5<-wday(data_id_5$datetime,label=TRUE)


data_id_5inter<-filter(data_id_5,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p5<- plot_ly(x =data_id_5inter$datetime,Y=data_id_5inter$power , type="scatter", mode="markers", fill = "tonexty")
p5

###########################################################################################
data_id_6<-filter(data,ID==6)

data_id_6=data_id_6 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_6 <- data_id_6%>% mutate(powertotal=sum(power))
just.dayofweek6<-wday(data_id_6$datetime,label=TRUE)

data_id_6inter<-filter(data_id_6,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p6<- plot_ly(x =data_id_6inter$datetime,Y=data_id_6inter$power , type="scatter", mode="markers", fill = "tonexty")
p6

###########################################################################################
data_id_7<-filter(data,ID==7)

data_id_7=data_id_7 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_7 <- data_id_7%>% mutate(powertotal=sum(power))
just.dayofweek7<-wday(data_id_7$datetime,label=TRUE)

data_id_7inter<-filter(data_id_7,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p7<- plot_ly(x =data_id_7inter$datetime,Y=data_id_7inter$power , type="scatter", mode="markers", fill = "tonexty")
p7

###########################################################################################
data_id_8<-filter(data,ID==8)

data_id_8=data_id_8 %>%
  select(datetime,power)%>%
  arrange(desc(power))
data_id_8 <- data_id_8%>% mutate(powertotal=sum(power))

just.dayofweek8<-wday(data_id_8$datetime,label=TRUE)

data_id_8inter<-filter(data_id_8,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p8<- plot_ly(x =data_id_8inter$datetime,Y=data_id_8inter$power , type="scatter", mode="markers", fill = "tonexty")
p8

###########################################################################################
data_id_9<-filter(data,ID==9)

data_id_9=data_id_9 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_9 <- data_id_9%>% mutate(powertotal=sum(power))
just.dayofweek9<-wday(data_id_9$datetime,label=TRUE)

data_id_9inter<-filter(data_id_9,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p9<- plot_ly(x =data_id_9inter$datetime,Y=data_id_9inter$power , type ="scatter", mode="markers", fill = "tonexty")
p9
###########################################################################################

data_id_10<-filter(data,ID==10)

data_id_10=data_id_10 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_10 <- data_id_10%>% mutate(powertotal=sum(power))
just.dayofweek10<-wday(data_id_10$datetime,label=TRUE)

data_id_10inter<-filter(data_id_10,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p10<- plot_ly(x =data_id_10inter$datetime,Y=data_id_10inter$power , type ="scatter", mode="markers", fill = "tonexty")
p10

###########################################################################################
data_id_11<-filter(data,ID==11)

data_id_11=data_id_11 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_11 <- data_id_11%>% mutate(powertotal=sum(power))
just.dayofweek11<-wday(data_id_11$datetime,label=TRUE)

data_id_11inter<-filter(data_id_11,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p11<- plot_ly(x =data_id_11inter$datetime,Y=data_id_11inter$power , type ="scatter", mode="markers", fill = "tonexty")
p11

###########################################################################################
data_id_12<-filter(data,ID==12)

data_id_12=data_id_12 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_12 <- data_id_12%>% mutate(powertotal=sum(power))
just.dayofweek12<-wday(data_id_12$datetime,label=TRUE)

data_id_12inter<-filter(data_id_12,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p12<- plot_ly(x =data_id_12inter$datetime,Y=data_id_12inter$power , type ="scatter", mode="markers", fill = "tonexty")
p12
###########################################################################################

data_id_13<-filter(data,ID==13)

data_id_13=data_id_13 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_13 <- data_id_13%>% mutate(powertotal=sum(power))
just.dayofweek13<-wday(data_id_13$datetime,label=TRUE)

data_id_13inter<-filter(data_id_13,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p13<- plot_ly(x =data_id_13inter$datetime,Y=data_id_10inter$power , type ="scatter", mode="markers", fill = "tonexty")
p13

###########################################################################################
data_id_14<-filter(data,ID==14)

data_id_14=data_id_14 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_14 <- data_id_14%>% mutate(powertotal=sum(power))
just.dayofweek14<-wday(data_id_14$datetime,label=TRUE)

data_id_14inter<-filter(data_id_14,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p14<- plot_ly(x =data_id_14inter$datetime,Y=data_id_14inter$power , type ="scatter", mode="markers", fill = "tonexty")
p14

###########################################################################################
data_id_15<-filter(data,ID==15)

data_id_15=data_id_15 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_15 <- data_id_15%>% mutate(powertotal=sum(power))
just.dayofweek15<-wday(data_id_15$datetime,label=TRUE)

data_id_15inter<-filter(data_id_15,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p15<- plot_ly(x =data_id_15inter$datetime,Y=data_id_15inter$power , type ="scatter", mode="markers", fill = "tonexty")
p15
###########################################################################################

data_id_16<-filter(data,ID==16)

data_id_16=data_id_16 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_16 <- data_id_16%>% mutate(powertotal=sum(power))
just.dayofweek16<-wday(data_id_16$datetime,label=TRUE)

data_id_16inter<-filter(data_id_16,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p16<- plot_ly(x =data_id_16inter$datetime,Y=data_id_16inter$power , type ="scatter", mode="markers", fill = "tonexty")
p16
###########################################################################################

data_id_17<-filter(data,ID==17)

data_id_17=data_id_17 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_17 <- data_id_17%>% mutate(powertotal=sum(power))
just.dayofweek17<-wday(data_id_17$datetime,label=TRUE)

data_id_17inter<-filter(data_id_17,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p17<- plot_ly(x =data_id_17inter$datetime,Y=data_id_17inter$power , type ="scatter", mode="markers", fill = "tonexty")
p17
###########################################################################################

data_id_18<-filter(data,ID==18)

data_id_18=data_id_18 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_18 <- data_id_18%>% mutate(powertotal=sum(power))
just.dayofweek18<-wday(data_id_18$datetime,label=TRUE)

data_id_18inter<-filter(data_id_18,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p18<- plot_ly(x =data_id_18inter$datetime,Y=data_id_18inter$power , type ="scatter", mode="markers", fill = "tonexty")
p18

###########################################################################################
data_id_19<-filter(data,ID==19)

data_id_19=data_id_19 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_19 <- data_id_19%>% mutate(powertotal=sum(power))
just.dayofweek19<-wday(data_id_19$datetime,label=TRUE)

data_id_19inter<-filter(data_id_19,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p19<- plot_ly(x =data_id_19inter$datetime,Y=data_id_19inter$power , type ="scatter", mode="markers", fill = "tonexty")
p19

###########################################################################################
data_id_20<-filter(data,ID==20)

data_id_20=data_id_20 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_20 <- data_id_20%>% mutate(powertotal=sum(power))
just.dayofweek20<-wday(data_id_20$datetime,label=TRUE)

data_id_20inter<-filter(data_id_20,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p20<- plot_ly(x =data_id_20inter$datetime,Y=data_id_20inter$power , type ="scatter", mode="markers", fill = "tonexty")
p20
###########################################################################################

data_id_21<-filter(data,ID==21)

data_id_21=data_id_21 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_21 <- data_id_21%>% mutate(powertotal=sum(power))
just.dayofweek21<-wday(data_id_21$datetime,label=TRUE)

data_id_21inter<-filter(data_id_21,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p21<- plot_ly(x =data_id_21inter$datetime,Y=data_id_21inter$power , type ="scatter", mode="markers", fill = "tonexty")
p21

###########################################################################################
data_id_22<-filter(data,ID==22)

data_id_22=data_id_22 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_22 <- data_id_22%>% mutate(powertotal=sum(power))
just.dayofweek22<-wday(data_id_22$datetime,label=TRUE)

data_id_22inter<-filter(data_id_22,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p22<- plot_ly(x =data_id_22inter$datetime,Y=data_id_22inter$power , type ="scatter", mode="markers", fill = "tonexty")
p22
###########################################################################################

data_id_23<-filter(data,ID==23)

data_id_23=data_id_23 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_23 <- data_id_23%>% mutate(powertotal=sum(power))
just.dayofweek23<-wday(data_id_23$datetime,label=TRUE)

data_id_23inter<-filter(data_id_23,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p23<- plot_ly(x =data_id_23inter$datetime,Y=data_id_23inter$power , type ="scatter", mode="markers", fill = "tonexty")
p23
###########################################################################################

data_id_24<-filter(data,ID==24)

data_id_24=data_id_24 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_24 <- data_id_24%>% mutate(powertotal=sum(power))
just.dayofweek24<-wday(data_id_24$datetime,label=TRUE)

data_id_24inter<-filter(data_id_24,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p24<- plot_ly(x =data_id_24inter$datetime,Y=data_id_24inter$power , type ="scatter", mode="markers", fill = "tonexty")
p24
###########################################################################################

data_id_25<-filter(data,ID==25)

data_id_25=data_id_25 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_25 <- data_id_25%>% mutate(powertotal=sum(power))
just.dayofweek25<-wday(data_id_25$datetime,label=TRUE)

data_id_25inter<-filter(data_id_25,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p25<- plot_ly(x =data_id_25inter$datetime,Y=data_id_25inter$power , type ="scatter", mode="markers", fill = "tonexty")
p25
###########################################################################################

data_id_26<-filter(data,ID==26)

data_id_26=data_id_26 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_26 <- data_id_26%>% mutate(powertotal=sum(power))
just.dayofweek26<-wday(data_id_26$datetime,label=TRUE)

data_id_26inter<-filter(data_id_26,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p26<- plot_ly(x =data_id_26inter$datetime,Y=data_id_26inter$power , type ="scatter", mode="markers", fill = "tonexty")
p26
###########################################################################################

data_id_27<-filter(data,ID==27)

data_id_27=data_id_27 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_27 <- data_id_27%>% mutate(powertotal=sum(power))
just.dayofweek27<-wday(data_id_27$datetime,label=TRUE)

data_id_27inter<-filter(data_id_27,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p27<- plot_ly(x =data_id_27inter$datetime,Y=data_id_27inter$power , type ="scatter", mode="markers", fill = "tonexty")
p27

###########################################################################################
data_id_28<-filter(data,ID==28)

data_id_28=data_id_28 %>%
  select(ID,datetime,power)%>%
  arrange(desc(power))
data_id_28 <- data_id_28%>% mutate(powertotal=sum(power))
just.dayofweek28<-wday(data_id_28$datetime,label=TRUE)

data_id_28inter<-filter(data_id_28,datetime>=ymd_hm("2019-08-05 7:00")& datetime<= ymd_hm("2019-08-11 18:59"))
p28<- plot_ly(x =data_id_28inter$datetime,Y=data_id_28inter$power , type ="scatter", mode="markers", fill = "tonexty")
p28

###########################################################################################
