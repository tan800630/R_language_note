#參考資料 http://leoluyi.logdown.com/posts/406397-crawler-mops-2

library(magrittr)
library(httr)
library(rvest)
library(XML)  # readHTMLTable
library(dplyr) # data manipulation & pipe line
library(stringr)
library(data.table)

res_doc <- GET("https://ent12.jctv.ntut.edu.tw/ugs3result/college.html") %>% 
  content(type="text", encoding = "UTF-8") %>% 
  read_html(encoding = "UTF-8")


#編號跟學校名稱列表
school_type <- data.frame(
	"num"=res_doc %>% html_node(xpath="//select") %>%
	html_children %>% html_attr("value"),
	"name"=sapply(res_doc %>% html_node(xpath="//select") %>%
	html_children %>% html_text(),function(x) gsub("[\r\n\t]","",x))) %>%
	.[-1,] %>% filter(!is.na(num))


school_data=lapply(school_type$num,function(x){
	form=list(	doit="view", code=school_type$num[x])

	res <- POST(
  	"https://ent12.jctv.ntut.edu.tw/ugs3result/college.html",
  	body = form,
  	encode = "form"
	)
	res_text <- content(res, "text", encoding = "UTF-8") %>%`Encoding<-`("UTF-8")

	#data.frame
	dt <- res_text %>%
  	read_html(encoding = "UTF-8")%>%
  	html_nodes(xpath = "//table") %>%
  	as.character %>%
  	XML::readHTMLTable(encoding = "UTF-8")%>%.[[1]]
	
	if(!is.null(dt)){
	colnames(dt)=c("校系科組學程代碼","系科組學程名稱","甄選群(類)別",
	"考生姓名(准考證號)","錄取身分")
	dt$"考生姓名(准考證號)" = sapply(dt$"考生姓名(准考證號)",
	function(x) gsub("[\r\n\t]","",x))};dt
}) %>% rbindlist()


