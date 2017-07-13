require(dplyr)
require(ggplot2)
require(scales)
require(Rfacebook)

#�ϥ�token���H�n�O�o��@�U"token=fb.oauth"�o�q
#token="#your facebook API token#"
#�ݭn�ۤv���ƪ��H�i�Hgoogle  �ϥ�R���RFacebook   �����о�

start_date <- "2013/01/01"
end_date <- "2017/05/31"
page.id <- "DoctorKoWJ"

page <- getPage(page.id,token=fb.oauth,n=3000,since=start_date,until=end_date)

#�ݤ@�U���
str(page)

#�N��Ʀs�X
write.csv(page,file="page_KoWJ.csv")

#rm(list=ls())


####��Ƶ�ı��#####
#Ū���ɮ�
dat=read.csv("page_KoWJ.csv")


##��ƫe�B�z##

#��S���d��Po�媺�R��----�ݹL��l��ơA�Ҭ��W�ǷӤ�
dat=dat[-which(is.na(dat$message)),]

#�ܧ��ܶ�����
dat$message=as.character(dat$message)
dat$type=as.character(dat$type)

#��note������po��R��-�u���@��
dat=dat[-which(dat$type=="note"),]

##��ƪ�##

#table-�峹����/�������g����/�ƶq
dat %>% group_by(type) %>% summarise(�����g��=mean(likes_count),
�����d����=mean(comments_count),��������=mean(shares_count),
���e����=mean(nchar(message)),�ƶq=length(likes_count))


##��Ƨ@��##

#�[�ݦ������M�����峹�����ƶq
barplot(table(dat$type),main="�_����y�ѯ����M���峹����",xlab="�峹����",ylab="����")

##ggplot-�峹����-�g��-�ɶ��Ͷ�
ggplot(dat,aes(x=as.Date(created_time),y=log(likes_count)))+
geom_point(aes(color=type,shape=type))+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-01-19"))),colour="red",linetype="dashed")+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-11-29"))),colour="blue",linetype="dashed")+
annotate("text",x=as.Date("2014-01-19"),y=4,label="�ť��ѿ�",colour="red")+
annotate("text",x=as.Date("2014-11-29"),y=4,label="���參��",colour="blue")+
labs(title="�峹����-�g��-�ɶ��Ͷ�",x="�ɶ�",y="�g��_LOG")+
facet_grid(type~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
scale_x_date(labels = date_format("%Y-%m-%d"))
