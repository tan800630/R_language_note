
require("Rfacebook")
token="EAACEdEose0cBAC82PQ3vWsQ3ZAPxrprWBu2ZBlkQb6trZAZAyMLtrLljZAr3DyxMJ4tTBp69p82LjoACYILWkHM60vBZBPb1ctKyP9HQ1RpJrUHEvxzayGYPe25wdaETrOcg0iOmb9IIPzGMQMUs1gtJf3JCGMrceogbp8UaHMWa49KoKjriDiEZAqg2rTaT4YZD"

me <- getUsers("me",token)
me$name


page.id <- "DoctorKoWJ"
page <- getPage(page.id, token, n = 300)
str(page)


com=data.frame()
lik=data.frame()

for(i in 1:dim(page)[1]){
	post=getPost(page$id[i],token,likes=TRUE)
	com=rbind(com,post$comments)    #要再往下挖-> getCommentReplies
	lik=rbind(lik,post$likes)
}

count_lik=as.data.frame(table(lik$from_id))
count_com=as.data.frame(table(com$from_id))

#排序讚數
count_lik=count_lik[order(count_lik$Freq,decreasing=T),]

#讚是誰
#lik[which(lik$from_id=="380194235462302")[1],1:2]

#留言最多
count_com=count_com[order(count_com$Freq,decreasing=T),]


## convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}

#看第一名留言者的留言日期
tim=com[com$from_id==count_com$Var1[1],]$created_time
tim=format.facebook.date(tim)
hist(tim,breaks=100,freq=T)