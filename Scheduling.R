library(GA)
library(ggplot2)


#######
##	製造資料(隨便設定的參數)
##	問題：我現在有2台機器可以生產零件(每一款零件在哪一台都做得出來)，1台機器可以組裝產品，
##	    但機器一次只能作一件事情(不能同時生產A零件和B零件)，隨便給我現在要做的產品列表，
##	    請幫我排出最好的製造順序。 btw 組裝產品需要等所有零件到齊才能開始
#######

##機台數量(生產零件/組裝)
num_m=c(2,1)

##每一個產品(product)需要的零件列表
o_data=data.frame("Product"=c(rep(1,4),rep(2,2),rep(3,3),rep(4,3),rep(5,4)),
	"Component"=c(1,2,2,4,3,4,1,4,4,1,3,3,2,3,4,4))

##這次要做的所有產品(順序無意義)
a_data_set=data.frame("id"=c(1,2,3,4,5),"Product"=c(1,2,3,4,1))

##製造零件需要的時間(STime為製造不同種零件時的準備時間 ex.從A零件換到C零件)
production_time=data.frame("Component"=c(1,2,3,4),"Time"=c(21,50,42,31),
	"STime"=c(0,0,0,0))

##組裝產品需要的時間
assemble_time=data.frame("Product"=c(1,2,3,4),"Time"=c(50,30,42,30),"STime"=c(0,0,0,0))


#列出這次要做的產品以及他底下的零件
data_set=c()
for(i in 1:dim(a_data_set)[1]){
	data_set=rbind(data_set,
		data.frame("ID"=a_data_set$id[i],o_data[o_data$Product==a_data_set$Product[i],]))
}



#######
##	開始寫基因演算法的評估函數(evaluate function)
##	理論上希望找到一個組合讓這個評估函數達到極值(ex.最短整體完工時間)
#######

eval_Func <-function(chromosome){
	#chromosome會是某一組數值(在這邊我想要的是一堆整數去指定說哪一個工作要先做哪一個後做)
	#前n個數字代表製造零件的順序,後面的數字代表組裝的順序
	x=c(1:dim(data_set)[1]);x=x[order(chromosome[1:length(x)])]
	y=c(1:dim(a_data_set)[1]);
	y=y[order(chromosome[dim(data_set)[1]+1:length(y)])]
	

	#接下來就開始去計算整體需要花費的時間
	#規則:依照製造順序開始把工作塞給"可以最快開始"的機器，若兩台機器現在都閒置就隨機分派
	#組裝的機器則是會等待零件都準備好時開始動工

	pm_at=c(rep(0,num_m[1]))
	am_at=c(rep(0,num_m[2]))
	
	product_machine_work=matrix(NA,dim(data_set)[1],num_m[1])
	assemble_machine_work=matrix(NA,dim(a_data_set)[1],num_m[2])

	#依照染色體順序排序
	p_data=data.frame(data_set[x,],"start"=NA,"end"=NA,"machine"=NA)
	a_data=data.frame(a_data_set[y,],"start"=NA,"end"=NA,"machine"=NA)

	###生產###
	k=1

	distribute <- function(data_ind){
		ind=which(pm_at %in% min(pm_at))
		if(length(ind)!=1){ind=sample(ind,1)}
		stime=pm_at[ind]
		p_data$machine[k] <<-ind
		
		#紀錄這次做的component類別
		product_machine_work[k,ind]<<-data_ind[3]

		#紀錄開始時間，如果前一個Component跟上一個不同的話，需要Setup time
		if(mean(tail(na.omit(product_machine_work[,ind]),2))!=data_ind[3]){
			p_data$start[k]<<-stime+production_time$STime[production_time$Component==data_ind[3]]
		}else {
			p_data$start[k]<<-stime}
		
		#更新此機台下次可以開始的時間
		pm_at[ind]<<-p_data$start[k]+production_time$Time[production_time$Component==data_ind[3]]
		
		#紀錄完成時間
		p_data$end[k]<<-pm_at[ind]

		k <<-k+1}

	#開始逐步排生產零件機台的排程
	apply(as.matrix(p_data),1,distribute)

	####裝配###
	k=1
	
	assemble=function(a_data_ind){
		
		#注意，這邊用的是col_index，變更資料格式會有差
		c_time=p_data[max(which(p_data[1]==a_data_ind[1])),5]
		ind=which(am_at %in% min(am_at))
		if(length(ind)!=1|c_time>min(am_at)){ind=sample(ind,1)}
		if(c_time>min(am_at)){
			stime=c_time
		} else { stime=am_at[ind]}
		a_data$machine[k] <<-ind
		
		#紀錄這次做類別
		assemble_machine_work[k,ind] <<- a_data_ind[2]

		#紀錄開始時間，如果前一個產品類別跟上一個不同的話，需要Setup time
		if(mean(tail(na.omit(assemble_machine_work[,ind]),2))!=a_data_ind[2]){
			a_data$start[k]<<-stime+assemble_time$STime[assemble_time$Product==a_data_ind[2]]
		}else {
			a_data$start[k]<<-stime}
		
		#更新此機台下次可以開始的時間
		am_at[ind]<<-a_data$start[k]+assemble_time$Time[assemble_time$Product==a_data_ind[2]]
		
		#紀錄完成時間
		a_data$end[k]<<-am_at[ind]

	k<<-k+1}
	
	#開始逐步排組裝機台的排程
	apply(as.matrix(a_data),1,assemble)

	#計算最終完成時間(越少越好)
	total_time=max(a_data$end)
	#res=list(p_data,a_data,product_machine_work,assemble_machine_work)
	return(-total_time)
	#return(res)
}

x=sample(c(1:dim(data_set)[1]),dim(data_set)[1])
y=sample(c(1:dim(a_data_set)[1]),dim(a_data_set)[1])

#######
##	執行基因演算法(丟入你要的評估函數，以及指定一些參數)
#######

#設定每一個世代會隨機產生100組不同的染色體，其中最好的幾個染色體會留下來延續到下一個世代
#設定跑50個世代
ga.result=ga(type="permutation",fitness=eval_Func,min=1,max=21,popSize=100,maxiter=50)

#solutions會列出最好的染色體組合(我這個問題頗簡單所以可能有超過一種以上的最佳解)
summary(ga.result)

