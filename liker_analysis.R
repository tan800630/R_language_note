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

#��z�������g�̸��
load("C:/Users/tan/Documents/data/liker.RData")
#�쥻�������M����ơA�]�t�C�h�峹��like�ƶq
load("C:/Users/tan/Documents/data/FBpage.RData")
rm(page2,page3,page4,page5,page6,page7)


##  0->�榸���g  1->����ɶ��C��T�Ӥ� 2->��L  3->�����C�Ӥ�ܤ֫��@���g&����ɶ�����T�Ӥ�
plot_data=plot_data %>% mutate(label=as.factor(2+(count==1)*-1+
				(duration<=90)*-2+(duration/count<=30)*1))

#�����X��-fac_month�ܼ�
merge_dat=merge(postDT2 %>% select(from_id,post_date,post_ID),
	plot_data %>% select(from_id,label)) %>%
	mutate(fac_month=as.factor(substr(post_date,1,7)))

#�b���~merge_dat
load("C:/Users/tan/Documents/data/merge_dat.RData")

#�p��C�@�hpost���U�ؤH��
post_dat=merge_dat %>% dcast(post_date+post_ID+label~1,length,value.var="label") %>%
setnames(".", "count")

#�H�ɶ��Ϭq�����}---����@�ϭt����C
merge_dat1=merge_dat %>% filter(post_date<as.Date("2015-01-01"))
merge_dat2=merge_dat %>% filter(post_date>as.Date("2015-01-01"))
merge_dat3=merge_dat %>% filter(post_date>as.Date("2016-01-01"))


#�C�@��label�����  (�t���L����)
merge_dat1 %>% dcast(label~1,function(x) length(x)/dim(merge_dat1)[1])
merge_dat2 %>% dcast(label~1,function(x) length(x)/dim(merge_dat2)[1])
merge_dat3 %>% dcast(label~1,function(x) length(x)/dim(merge_dat3)[1])


require(ggplot2)

#�C�@�h�峹��ggplot    #�d��ݭn�Y���A�q�Y������ɶ��b�L��
#��POSIXct�L�k�bx�blabel  �ݭn�নDate  ->�ɶ��Ӳ�

#grid.arrange(plot1, plot2, nrow=1, ncol=2)

#���
ggplot(merge_dat�@%>% filter(post_date>as.Date("2016-01-01")&
	post_date<as.Date("2016-03-31")),aes(post_date))+geom_bar()


#���W--�P�@��Po��|���|
ggplot(merge_dat,aes(as.Date(post_date)))+geom_bar()

#��W�Ϥ@�Ҥ@��
ggplot(post_dat,aes(as.Date(post_date),y=count))+geom_col(aes(fill=label))



###�ƭȤ��R##

new_dat=post_dat %>% merge(.,
post_dat %>% group_by(post_ID) %>% summarise("like"=sum(count)) %>%
	mutate("med_cut"=as.numeric(like>quantile(like,0.75))),by="post_ID")

date=new_dat %>% filter(med_cut==1) %>% select(post_date,post_ID) %>%
arrange(post_date) %>% .$post_date

#�̪��ɶ����j----75�ʤ����
#90��
max(difftime(date[-1],date[-length(date)],units="days"))



#########################################################
#�C�Ӥ����ggplot
ggplot(merge_dat,aes(fac_month))+
geom_bar(aes(fill=label))

#�p��C�ӱڸs�����
propor_dat=merge_dat %>%
dcast(fac_month+label~1,length,value.var="fac_month") %>%
setnames("1", "count") %>% group_by(fac_month) %>%
mutate(proportion=count/sum(count))

ggplot(propor_dat,aes(fac_month))+geom_bar(aes(fill=label,weight=proportion))