
require("Rfacebook")

#這邊是用我的fb.oauth直接授權的，用token的人要記得改一下"token=fb.oauth"這段
#token="#your facebook API token#"

start_date <- "2016/01/01"
end_date <- "2017/04/14"

page.id <- "DoctorKoWJ"
page <- getPage(page.id,token=fb.oauth,n=1000,since=start_date,until=end_date)
str(page)


## convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}

############################
#看每一篇文章的留言持續程度
time_last <-function(post_id){
	post=getPost(post_id,token=fb.oauth,likes=F,n=10000)
	s_time=format.facebook.date(post$post$created_time)
	time_box=format.facebook.date(post$comment$created_time)
	time_box=difftime(time_box,s_time,units="mins")
	a=as.numeric(time_box)
	data.frame("id"=post_id,"time"=s_time,
		"m_post"=mean(a),"s_post"=sd(a),
		"md_post"=median(a))
}


#存檔
time_data=data.frame()
for(i in 1:291){time_data=rbind(time_data,time_last(page$id[i]))
}
time_data=data.frame(time_data,page[,9:11])
save(time_data,file="C:/Users/tan/Documents/data/time_data0422.RData")

load("C:/Users/tan/Documents/data/time_data0422.RData")


##data manipulation

#有一則po文沒有comments
#後續可以稍微看一下	id="136845026417486_820610521374263"
time_data=time_data[-which(is.na(time_data$s_post)),]

#histogram
par(mfrow=c(3,3))
for(i in 3:8){hist(time_data[,i],main=colnames(time_data)[i])}

#改取log
par(mfrow=c(3,3))
for(i in 3:8){hist(log(time_data[,i]),
	main=paste0("log_",colnames(time_data)[i]))}

#取完log後的boxplot
boxplot(log(time_data[,-c(1,2)]))

#x-y plot
pairs(time_data[,-c(1,2)])

pairs(log(time_data[,5:8]))


#新的資料用log去分析
t_dat=time_data
t_dat[,3:8]=log(t_dat[,3:8])

##analysis

#po文時間與留言持續時間的關係
plot(as.Date(t_dat$time),t_dat$md_post)




##################
##針對按讚跟留言者做分析
####
com=data.frame()
lik=data.frame()

for(i in 1:dim(page)[1]){
	post=getPost(page$id[i],token,likes=F,n=10000)
	com=rbind(com,post$comments)    #要再往下挖-> getCommentReplies
	#lik=rbind(lik,post$likes)
}

#count_lik=as.data.frame(table(lik$from_id))
count_com=as.data.frame(table(com$from_id))

#排序讚數
count_lik=count_lik[order(count_lik$Freq,decreasing=T),]
#讚是誰
#lik[which(lik$from_id=="380194235462302")[1],1:2]

#留言最多
count_com=count_com[order(count_com$Freq,decreasing=T),]



#看第一名留言者的留言日期
tim=com[com$from_id==count_com$Var1[1],]$created_time
tim=format.facebook.date(tim)
hist(tim,breaks=100,freq=T)

