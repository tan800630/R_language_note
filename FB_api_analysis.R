require(dplyr)


load("C:/Users/tan/Documents/data/FBpage.RData")

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
		"md_post"=median(a),"ninty_post"=if(dim(post$comments)[1]>9)
		{sort(a)[floor(length(a)*0.9)]}else{0})
}


#存檔
time_data=data.frame()
for(i in 1:769){time_data=rbind(time_data,time_last(page3$id[i]))
}
time_data=data.frame(time_data,page3[,9:11])
#save(time_data_KoWJ,time_data_pxmart,page,page2,
#	file="C:/Users/tan/Documents/data/FBdata.RData")

load("C:/Users/tan/Documents/data/FBdata.RData")


##data manipulation

#增加年月的欄位
time_data$YM=as.factor(format(time_data$time,"%Y-%m"))


#刪除沒有留言的資料
#KoWJ後續可以稍微看一下	id="136845026417486_820610521374263"
time_data=time_data[-which(is.na(time_data$s_post)),]

#histogram
par(mfrow=c(3,3))
for(i in 3:9){hist(time_data[,i],main=colnames(time_data)[i])}

#改取log
par(mfrow=c(3,3))
for(i in 3:9){hist(log(time_data[,i]),
	main=paste0("log_",colnames(time_data)[i]))}

#取完log後的boxplot
boxplot(log(time_data[,3:9]))

#x-y plot
pairs(time_data[,5:9])

pairs(log(time_data[time_data$shares_count!=0,5:9]))

cor(log(time_data[time_data$shares_count!=0,5:9]))

#PO文月份分布
plot(table(time_data$YM))



#新的資料用log去分析
#取log要注意原本是0的數值會變成負無限大
t_dat=time_data[time_data$ninty_post!=0,]
t_dat[,3:8]=log(t_dat[,3:8])

##analysis

#po文時間與留言持續時間的關係
plot_data=as.data.frame(t_dat %>% group_by(YM) %>%
	summarise(md=mean(md_post),ninty=mean(ninty_post),likes=mean(likes_count),
	comments=mean(comments_count),shares=mean(shares_count),
	num=length(shares_count)))

#整體而言，平均每月按讚數量下降幅度明顯-Ko
par(mfrow=c(2,3))
plot(plot_data[,c(1,2)],type="n",main="留言持續時間-50%")
plot(plot_data[,c(1,3)],type="n",main="留言持續時間-90%")
plot(plot_data[,c(1,4)],type="n",ylim=c(0,90000),main="likes")
plot(plot_data[,c(1,5)],type="n",ylim=c(0,2000),main="留言數量")
plot(plot_data[,c(1,6)],type="n",ylim=c(0,1400),main="分享數量")
plot(plot_data[,c(1,7)],type="n",ylim=c(0,35),main="文章數量")


#需要更多月份才能看出好趨勢
cor(plot_data[,-1])
pairs(plot_data[,-1])

#留言持續最久的文章前五名
id=time_data[order(time_data$ninty_post,decreasing=T)[1:5],]$id
page[page$id%in%id,]



### format(time,"%Y-%m")
### unclass(as.POSIXlt(time))

