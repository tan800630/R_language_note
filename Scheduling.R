library(GA)
library(ggplot2)


#######
##	�s�y���(�H�K�]�w���Ѽ�)
##	���D�G�ڲ{�b��2�x�����i�H�Ͳ��s��(�C�@�ڹs��b���@�x�����o�X��)�A1�x�����i�H�ո˲��~�A
##	    �������@���u��@�@��Ʊ�(����P�ɥͲ�A�s��MB�s��)�A�H�K���ڲ{�b�n�������~�C���A
##	    �����ڱƥX�̦n���s�y���ǡC btw �ո˲��~�ݭn���Ҧ��s�����~��}�l
#######

##���x�ƶq(�Ͳ��s��/�ո�)
num_m=c(2,1)

##�C�@�Ӳ��~(product)�ݭn���s��C��
o_data=data.frame("Product"=c(rep(1,4),rep(2,2),rep(3,3),rep(4,3),rep(5,4)),
	"Component"=c(1,2,2,4,3,4,1,4,4,1,3,3,2,3,4,4))

##�o���n�����Ҧ����~(���ǵL�N�q)
a_data_set=data.frame("id"=c(1,2,3,4,5),"Product"=c(1,2,3,4,1))

##�s�y�s��ݭn���ɶ�(STime���s�y���P�عs��ɪ��ǳƮɶ� ex.�qA�s�󴫨�C�s��)
production_time=data.frame("Component"=c(1,2,3,4),"Time"=c(21,50,42,31),
	"STime"=c(0,0,0,0))

##�ո˲��~�ݭn���ɶ�
assemble_time=data.frame("Product"=c(1,2,3,4),"Time"=c(50,30,42,30),"STime"=c(0,0,0,0))


#�C�X�o���n�������~�H�ΥL���U���s��
data_set=c()
for(i in 1:dim(a_data_set)[1]){
	data_set=rbind(data_set,
		data.frame("ID"=a_data_set$id[i],o_data[o_data$Product==a_data_set$Product[i],]))
}



#######
##	�}�l�g��]�t��k���������(evaluate function)
##	�z�פW�Ʊ���@�ӲզX���o�ӵ�����ƹF�췥��(ex.�̵u���駹�u�ɶ�)
#######

eval_Func <-function(chromosome){
	#chromosome�|�O�Y�@�ռƭ�(�b�o��ڷQ�n���O�@���ƥh���w�����@�Ӥu�@�n�������@�ӫᰵ)
	#�en�ӼƦr�N���s�y�s�󪺶���,�᭱���Ʀr�N���ո˪�����
	x=c(1:dim(data_set)[1]);x=x[order(chromosome[1:length(x)])]
	y=c(1:dim(a_data_set)[1]);
	y=y[order(chromosome[dim(data_set)[1]+1:length(y)])]
	

	#���U�ӴN�}�l�h�p�����ݭn��O���ɶ�
	#�W�h:�̷ӻs�y���Ƕ}�l��u�@�뵹"�i�H�ֶ̧}�l"�������A�Y��x�����{�b�����m�N�H������
	#�ո˪������h�O�|���ݹs�󳣷ǳƦn�ɶ}�l�ʤu

	pm_at=c(rep(0,num_m[1]))
	am_at=c(rep(0,num_m[2]))
	
	product_machine_work=matrix(NA,dim(data_set)[1],num_m[1])
	assemble_machine_work=matrix(NA,dim(a_data_set)[1],num_m[2])

	#�̷ӬV���鶶�ǱƧ�
	p_data=data.frame(data_set[x,],"start"=NA,"end"=NA,"machine"=NA)
	a_data=data.frame(a_data_set[y,],"start"=NA,"end"=NA,"machine"=NA)

	###�Ͳ�###
	k=1

	distribute <- function(data_ind){
		ind=which(pm_at %in% min(pm_at))
		if(length(ind)!=1){ind=sample(ind,1)}
		stime=pm_at[ind]
		p_data$machine[k] <<-ind
		
		#�����o������component���O
		product_machine_work[k,ind]<<-data_ind[3]

		#�����}�l�ɶ��A�p�G�e�@��Component��W�@�Ӥ��P���ܡA�ݭnSetup time
		if(mean(tail(na.omit(product_machine_work[,ind]),2))!=data_ind[3]){
			p_data$start[k]<<-stime+production_time$STime[production_time$Component==data_ind[3]]
		}else {
			p_data$start[k]<<-stime}
		
		#��s�����x�U���i�H�}�l���ɶ�
		pm_at[ind]<<-p_data$start[k]+production_time$Time[production_time$Component==data_ind[3]]
		
		#���������ɶ�
		p_data$end[k]<<-pm_at[ind]

		k <<-k+1}

	#�}�l�v�B�ƥͲ��s����x���Ƶ{
	apply(as.matrix(p_data),1,distribute)

	####�˰t###
	k=1
	
	assemble=function(a_data_ind){
		
		#�`�N�A�o��Ϊ��Ocol_index�A�ܧ��Ʈ榡�|���t
		c_time=p_data[max(which(p_data[1]==a_data_ind[1])),5]
		ind=which(am_at %in% min(am_at))
		if(length(ind)!=1|c_time>min(am_at)){ind=sample(ind,1)}
		if(c_time>min(am_at)){
			stime=c_time
		} else { stime=am_at[ind]}
		a_data$machine[k] <<-ind
		
		#�����o�������O
		assemble_machine_work[k,ind] <<- a_data_ind[2]

		#�����}�l�ɶ��A�p�G�e�@�Ӳ��~���O��W�@�Ӥ��P���ܡA�ݭnSetup time
		if(mean(tail(na.omit(assemble_machine_work[,ind]),2))!=a_data_ind[2]){
			a_data$start[k]<<-stime+assemble_time$STime[assemble_time$Product==a_data_ind[2]]
		}else {
			a_data$start[k]<<-stime}
		
		#��s�����x�U���i�H�}�l���ɶ�
		am_at[ind]<<-a_data$start[k]+assemble_time$Time[assemble_time$Product==a_data_ind[2]]
		
		#���������ɶ�
		a_data$end[k]<<-am_at[ind]

	k<<-k+1}
	
	#�}�l�v�B�Ʋո˾��x���Ƶ{
	apply(as.matrix(a_data),1,assemble)

	#�p��̲ק����ɶ�(�V�ֶV�n)
	total_time=max(a_data$end)
	#res=list(p_data,a_data,product_machine_work,assemble_machine_work)
	return(-total_time)
	#return(res)
}

x=sample(c(1:dim(data_set)[1]),dim(data_set)[1])
y=sample(c(1:dim(a_data_set)[1]),dim(a_data_set)[1])

#######
##	�����]�t��k(��J�A�n��������ơA�H�Ϋ��w�@�ǰѼ�)
#######

#�]�w�C�@�ӥ@�N�|�H������100�դ��P���V����A�䤤�̦n���X�ӬV����|�d�U�ө����U�@�ӥ@�N
#�]�w�]50�ӥ@�N
ga.result=ga(type="permutation",fitness=eval_Func,min=1,max=21,popSize=100,maxiter=50)

#solutions�|�C�X�̦n���V����զX(�ڳo�Ӱ��D��²��ҥH�i�঳�W�L�@�إH�W���̨θ�)
summary(ga.result)
