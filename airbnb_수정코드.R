#미국 에어비앤비 가격 데이터 시각화
#1. 데이터 전처리
#####graphics EDA###
#Explore
#1.airbnb_us visualization
airbnb_us_new<-read.csv("airbnb_us_new.csv")
airbnb_us_new<-airbnb_us_new[,-c(1,7,21)] #host_verification/amentities->count(개수)로 변환

library(tidyverse)
library(ggplot2)
library (scales)
library(RColorBrewer)
library(ggforce)
library(gridExtra)

summary(airbnb_us_new)

########y변수#######
####전체Y####   
ggplot(data=airbnb_us_new,mapping=aes(x = Y))+
  theme_bw()+ 
  geom_freqpoly(col="skyblue")

#200부근의 가격이 가장 많아

####state별로 그려보기####
ggplot(data=airbnb_us_new,mapping=aes(x = Y,y=..density..,colour=state))+
  theme_bw()+
  geom_freqpoly()
#알아보기 힘들어.. 거의 비슷한 분포

####state별로 상자그림으로 알아보기####
ggplot(data=airbnb_us_new)+
  geom_boxplot(mapping=aes(x = reorder(state,Y,FUN=median),y=Y,alpha=0.5,fill=state),show.legend ="F")+
  theme_bw()+
  xlab("state")+
  ylab("price")+
  coord_flip()
#hawaii>boston>sanfran>>>chicago

###state별로 나누기###
ggplot(data=airbnb_us_new)+
  geom_histogram(mapping=aes(x=Y))+
  facet_wrap(~state)
##state별 count##
ggplot(data=airbnb_us_new)+
  geom_bar(mapping=aes(x=state),fill=c("#666666","#666666","#666666",2,"orange","#666666","#666666","#666666","#666666"))+
  theme_bw()
##la랑 ny이 가장 많아

###map###
library(ggmap)
library(maps)
library(dplyr)
newyork<-airbnb_us_new %>%
  filter(state=="NY")
la<-airbnb_us_new %>%
  filter(state=="LA")
lo2<-c(mean(la$longitude),mean(la$latitude))
sanfran<-airbnb_us_new %>%
  filter(state=="Sanfran")
lo3<-c(mean(sanfran$longitude),mean(sanfran$latitude))
boston<-airbnb_us_new %>%
  filter(state=="Boston")
lo4<-c(mean(boston$longitude),mean(boston$latitude))
sandiego<-airbnb_us_new %>%
  filter(state=="sandiego")
lo5<-c(mean(sandiego$longitude),mean(sandiego$latitude))
washington<-airbnb_us_new %>%
  filter(state=="Washington")
lo6<-c(mean(washington$longitude),mean(washington$latitude))
chicago<-airbnb_us_new %>%
  filter(state=="Chicago")
lo7<-c(mean(chicago$longitude),mean(chicago$latitude))
hawaii<-airbnb_us_new %>%
  filter(state=="hawaii")
lo8<-c(mean(hawaii$longitude),mean(hawaii$latitude))
seattle<-airbnb_us_new %>%
  filter(state=="seattle")
lo9<-c(mean(seattle$longitude),mean(seattle$latitude))

map1 <- get_map(location = c(-74,40.7), zoom =11  , source = "google", maptype ="roadmap" )
m1<-ggmap(map1) + geom_point(data=newyork,mapping=aes(x=longitude,y=latitude,alpha=0.01,color=Y,size=Y))+scale_color_gradient( low = '#99FF33', high = "#006633")+
  scale_size_area()
map2 <- get_map(location = lo2, zoom =11  , source = "google", maptype ="roadmap" )
m2<-ggmap(map2) + geom_point(data=la,mapping=aes(x=longitude,y=latitude,alpha=0.1,color=Y,size=Y))+scale_color_gradient( low = '#99FF33', high = "#006633")+
  scale_size_area()
map3 <- get_map(location = lo3, zoom =13  , source = "google", maptype ="roadmap" )
m3<-ggmap(map3) + geom_point(data=sanfran,mapping=aes(x=longitude,y=latitude,alpha=0.1,color=Y,size=Y))+scale_color_gradient( low = '#99FF33', high = "#006633")+
  scale_size_area()
map4 <- get_map(location = lo4, zoom =12  , source = "google", maptype ="roadmap" )
m4<-ggmap(map4) + geom_point(data=boston,mapping=aes(x=longitude,y=latitude,alpha=0.1,color=Y,size=Y))+scale_color_gradient( low = '#99FF33', high = "#006633")+
  scale_size_area()
map5 <- get_map(location = lo5, zoom =11  , source = "google", maptype ="roadmap" )
m5<-ggmap(map5) + geom_point(data=sandiego,mapping=aes(x=longitude,y=latitude,alpha=0.1,color=Y,size=Y))+scale_color_gradient( low = '#99FF33', high = "#006633")+
  scale_size_area()
map6 <- get_map(location = lo6, zoom =13  , source = "google", maptype ="roadmap" )
m6<-ggmap(map6) + geom_point(data=washington,mapping=aes(x=longitude,y=latitude,alpha=0.1,color=Y,size=Y))+scale_color_gradient( low = '#99FF33', high = "#006633")+
  scale_size_area()
map7 <- get_map(location = lo7, zoom =12  , source = "google", maptype ="roadmap" )
m7<-ggmap(map7) + geom_point(data=chicago,mapping=aes(x=longitude,y=latitude,alpha=0.1,color=Y,size=Y))+scale_color_gradient( low = '#99FF33', high = "#006633")+
  scale_size_area()
map8 <- get_map(location = lo8, zoom =9  , source = "google", maptype ="roadmap" )
m8<-ggmap(map8) + geom_point(data=hawaii,mapping=aes(x=longitude,y=latitude,alpha=0.1,color=Y,size=Y))+scale_color_gradient( low = '#99FF33', high = "#006633")+
  scale_size_area()
map9 <- get_map(location = lo9, zoom =12  , source = "google", maptype ="roadmap" )
m9<-ggmap(map9) + geom_point(data=seattle,mapping=aes(x=longitude,y=latitude,alpha=0.1,color=Y,size=Y))+scale_color_gradient( low = '#99FF33', high = "#006633")+
  scale_size_area()
m1
m2
m3
m4
m5
m6
m7
m8
m9

##########host##########
##super_host##
ggplot(data=airbnb_us_new)+
  geom_bar(mapping=aes(x=host_is_superhost))
#superhost가 더 적음
ggplot(data=airbnb_us_new)+
  geom_boxplot(mapping=aes(x=host_is_superhost,y=Y))
#superhost 여부는 가격에 큰 영향을 끼치지 않음
#host_response_rate
ggplot(data=airbnb_us_new)+
  geom_boxplot(mapping=aes(x=host_is_superhost,y=host_response_rate))
##superhost의 응답률은 100에 수렴한다.

#host_identity_verified
ggplot(data=airbnb_us_new)+
  geom_bar(mapping=aes(x=host_identity_verified))

ggplot(data=airbnb_us_new)+
  geom_bar(mapping=aes(x=host_is_superhost,fill=host_identity_verified))

ggplot(data=airbnb_us_new)+
  geom_bar(mapping=aes(x=host_identity_verified,fill=host_is_superhost))
#superhost가 아닌사람들 중에 신분보증이 안된사람이 더 많음

ggplot(data=airbnb_us_new)+
  geom_boxplot(mapping=aes(x=host_is_superhost,y=review_scores_rating))
#superhost의 review rating이 더 좋음

##host_response_time##
h_s<-ggplot(data=airbnb_us_new)+
  geom_bar(mapping=aes(x=host_response_time,alpha=0.9,fill=host_response_time))+
  theme_bw()
h_s+scale_fill_brewer(palette=4)
#within an hour이 가장 많다.

##비율로표현##
ggplot(data=airbnb_us_new)+
  geom_bar(mapping = aes(x=host_response_time,y=..prop..,group=1))+
  facet_wrap(~host_is_superhost)
#superhost의 경우가, within an hour 비율이 더 높다.

##막대그래프의 높이 계산
s_h<-airbnb_us_new %>%
  filter(host_is_superhost=="TRUE") %>%
  count(host_response_time)

h<-airbnb_us_new %>%
  filter(host_is_superhost=="FALSE") %>%
  count(host_response_time)

library(tidyverse)
library(ggplot2)
library (scales)
library(RColorBrewer)
library(ggforce)
library(gridExtra)

##host reponse time 원도표로##
ggplot()+theme_no_axes()+coord_fixed()+
  geom_arc_bar(aes(x0=0,y0=0,r0=0,r=2,amount=n,fill=host_response_time),data=s_h,stat='pie')+
  scale_fill_brewer(palette="YlOrRd")+
  ggtitle("super host")+
  theme(plot.title=element_text(size=30, vjust=2))

ggplot()+theme_no_axes()+coord_fixed()+
  geom_arc_bar(aes(x0=0,y0=0,r0=0,r=2,amount=n,fill=host_response_time),data=h,stat='pie')+
  scale_fill_brewer(palette="YlOrRd")+
  ggtitle("not super host")+
  theme(plot.title=element_text(face="bold", size=30, vjust=2))

##host_verification_count##
ggplot(data=airbnb_us_new)+
  geom_bar(mapping=aes(x=host_verification_count))
airbnb_us_new %>%
  count(host_verification_count)

#5일때가 가장 많다.

##property_type vs price##
ggplot(data=airbnb_us_new)+
  geom_boxplot(mapping=aes(x=reorder(property_type,Y,FUN=median),y=Y))+
  coord_flip()
#timeshare, resort, chalet순으로 비쌈

ggplot(data=airbnb_us_new) +
  geom_bar(mapping=aes(x=(property_type),fill=property_type),show.legend = "F")+
  theme_bw()+
  coord_flip()
#aprartment가 가장 많음

airbnb_us_new%>%
  count(property_type)
#apartment, house, condominium,townhouse 순으로 존재

new_prop<-airbnb_us_new %>%
  filter(property_type%in% c("Apartment","House","Condominium","Townhouse"))

ggplot(data=new_prop)+
  geom_bar(mapping=aes(x=property_type,fill=property_type))+
  theme_bw()

ggplot(data=new_prop)+
  geom_boxplot(mapping=aes(x=reorder(property_type,Y,FUN=median),y=Y,fill=property_type))+
  xlab("property_type")+
  theme_bw()  
##condominium이 가장 비싸다

###room_type##
ggplot(data=airbnb_us_new)+
  geom_bar(mapping=aes(x=room_type,fill=room_type,alpha=0.5))+
  theme_bw()
ggplot(data=airbnb_us_new)+
  geom_boxplot(mapping=aes(x=room_type,y=Y,fill=room_type,color=room_type,alpha=0.5))+theme_bw()
##entire home/apt가 가장 많음

##bed type##
ggplot(data=airbnb_us_new)+
  geom_bar(mapping=aes(x=bed_type))

ggplot(data=airbnb_us_new)+
  geom_boxplot(mapping=aes(x=bed_type,y=Y))
#REALBED가 가장 가격이 비씨다.

##accommodates##
ggplot(data=airbnb_us_new)+
  geom_bar(mapping=aes(x=accommodates))
airbnb_us_new %>%
  count(accommodates)
#2명이 가장 많음

#bedrooms##
ggplot(data=airbnb_us_new)+
  geom_bar(mapping=aes(x=bedrooms))

##beds##
ggplot(data=airbnb_us_new)+
  geom_bar(mapping=aes(x=beds))
#2개가 가장 많음 

ggplot(data=ames)+
  geom_point(mapping=aes(x=LotFrontage,y=SalePrice))

##minimum_nights##
ggplot(data=airbnb_us_new)+
  geom_point(mapping=aes(x=minimum_nights,y=Y))+
  facet_wrap(~state)
#최소숙박일수가 3000임에도 가격이 저렴한 이상점 존재
airbnb_us_new %>%
  filter(minimum_nights>2000)
#LA에 존재. 왜일까?

##maximum_nights##
ggplot(data=airbnb_us_new)+
  geom_point(mapping=aes(x=maximum_nights,y=Y))
airbnb_us_new %>%
  filter(maximum_nights>2.0e+09)
#최대 가능 숙박일수 이므로 가격과 관계가 없다고 판다.

##number_of_reviews##
ggplot(data=airbnb_us_new)+
  geom_histogram(mapping=aes(x=number_of_reviews))+
  facet_wrap(~state)

##review_scores_rating##
ggplot(data=airbnb_us_new)+
  geom_histogram(mapping=aes(x=review_scores_rating))

###두개다 큰 영향xx