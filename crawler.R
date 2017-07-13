#�ѦҸ�� http://leoluyi.logdown.com/posts/406397-crawler-mops-2

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


#�s����ǮզW�٦C��
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
	colnames(dt)=c("�ըt��վǵ{�N�X","�t��վǵ{�W��","�¿�s(��)�O",
	"�ҥͩm�W(����Ҹ�)","��������")
	dt$"�ҥͩm�W(����Ҹ�)" = sapply(dt$"�ҥͩm�W(����Ҹ�)",
	function(x) gsub("[\r\n\t]","",x))};dt
}) %>% rbindlist()

