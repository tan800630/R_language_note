library(pipeR)
library(data.table)
library(lubridate)
library(dplyr)

current_wd=getwd()
dir="C:/Users/tan/Documents/data/KoWJ"
setwd(dir)


postDT2 <- lapply(list.files(".", full.names = TRUE),function(fn){
  load(fn)
  data.table(post_ID=post$post$id,post_date = parse_date_time(
	substring(post$post$created_time,1, 19), "ymd HMS"),post$likes)
}) %>>% rbindlist

#整理完的按讚者資料
load("C:/Users/tan/Documents/data/liker.RData")
#原本的粉絲專頁資料，包含每則文章的like數量
load("C:/Users/tan/Documents/data/FBpage.RData")
rm(page2,page3,page4,page5,page6,page7)


##  0->單次按讚  1->持續時間低於三個月 2->其他  3->平均每個月至少按一次讚&持續時間高於三個月
plot_data=plot_data %>% mutate(label=as.factor(2+(count==1)*-1+
				(duration<=90)*-2+(duration/count<=30)*1))

#做不出來-fac_month變數
merge_dat=merge(postDT2 %>% select(from_id,post_date,post_ID),
	plot_data %>% select(from_id,label)) %>%
	mutate(fac_month=as.factor(substr(post_date,1,7)))

#半成品merge_dat
load("C:/Users/tan/Documents/data/merge_dat.RData")

#計算每一則post的各種人數
post_dat=merge_dat %>% dcast(post_date+post_ID+label~1,length,value.var="label") %>%
setnames(".", "count")

#以時間區段先切開---後續作圖負擔較低
merge_dat1=merge_dat %>% filter(post_date<as.Date("2015-01-01"))
merge_dat2=merge_dat %>% filter(post_date>as.Date("2015-01-01"))
merge_dat3=merge_dat %>% filter(post_date>as.Date("2016-01-01"))


#每一個label的比例  (差異無明顯)
merge_dat1 %>% dcast(label~1,function(x) length(x)/dim(merge_dat1)[1])
merge_dat2 %>% dcast(label~1,function(x) length(x)/dim(merge_dat2)[1])
merge_dat3 %>% dcast(label~1,function(x) length(x)/dim(merge_dat3)[1])


require(ggplot2)

#每一則文章的ggplot    #範圍需要縮限，從頭到尾的時間軸過長
#用POSIXct無法在x軸label  需要轉成Date  ->時間太細

#grid.arrange(plot1, plot2, nrow=1, ncol=2)

#精細
ggplot(merge_dat　%>% filter(post_date>as.Date("2016-01-01")&
	post_date<as.Date("2016-03-31")),aes(post_date))+geom_bar()


#粗糙--同一天Po文會重疊
ggplot(merge_dat,aes(as.Date(post_date)))+geom_bar()

#跟上圖一模一樣
ggplot(post_dat,aes(as.Date(post_date),y=count))+geom_col(aes(fill=label))



###數值分析##

new_dat=post_dat %>% merge(.,
post_dat %>% group_by(post_ID) %>% summarise("like"=sum(count)) %>%
	mutate("med_cut"=as.numeric(like>quantile(like,0.75))),by="post_ID")

date=new_dat %>% filter(med_cut==1) %>% select(post_date,post_ID) %>%
arrange(post_date) %>% .$post_date

#最長時間間隔----75百分位數
#90天
max(difftime(date[-1],date[-length(date)],units="days"))



#########################################################
#每個月份的ggplot
ggplot(merge_dat,aes(fac_month))+
geom_bar(aes(fill=label))

#計算每個族群的比例
propor_dat=merge_dat %>%
dcast(fac_month+label~1,length,value.var="fac_month") %>%
setnames("1", "count") %>% group_by(fac_month) %>%
mutate(proportion=count/sum(count))

ggplot(propor_dat,aes(fac_month))+geom_bar(aes(fill=label,weight=proportion))