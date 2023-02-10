#i2=7;optN=18#24
method.list = c("glmnet", "svmLinear", "rf", "xgbTree", "lda2","nnet","glmboost","hdda")     
library(caret)   
library(glmnet)   
library(pROC)
library(Matrix)   
library(qqman)   
library(MLmetrics)   
library(ggplot2) # Data visualization   
library(data.table)   
library(caret)   
library(LiblineaR)   
library(xgboost)   
library(lightgbm) 
library(SHAPforxgboost)  
library(MASS)   
options(scipen=999)   
library(data.table)   
if(!require("kernlab"))install.packages("kernlab",repos = "http://cran.us.r-project.org")   
library(pls)   
library(randomForest)   
A=read.csv("TMJOAI_Long_040422_Norm.csv",check.names = FALSE)   
y=A[,1]   
X=A[,-1]   
Nfold=10   
N=10   
fea=matrix(NA,dim(X)[2],Nfold*N);rownames(fea)=colnames(X)   
seed0=2022   
set.seed(seed0)   
foldsCVT <- createFolds(factor(y)[-(1:40)], k=Nfold, list=TRUE, returnTrain=FALSE)   
train.control <- trainControl(method = "cv", number = 10, # k-folds CV with k=10   
                              classProbs = TRUE,   
                              savePredictions = TRUE,   
                              summaryFunction = multiClassSummary)# save predictions for ROC   
predYT=matrix(NA,length(y),1)   
select=rep(NA,10)   
predYT_valid=matrix(NA,length(y),10) 
#file0=Sys.glob('out_valid/*')
#file0=union(union(Sys.glob('out_valid/*_lda2*'),Sys.glob('out_valid/*_glmboost*')),Sys.glob('out_valid/*_glmboost*'))
file0=union(union(Sys.glob('out_valid/*_lda2*'),Sys.glob('out_valid/*_glmboost*')),Sys.glob('out_valid/*_hdda*'))
#file0=union(union(union(Sys.glob('out_valid/*_lda2*'),Sys.glob('out_valid/*_glmboost*')),Sys.glob('out_valid/*_hdda*')),Sys.glob('out_valid/*_nnet*'))
L00=length(file0);file0=unlist(lapply(strsplit(file0,'/'),'[',2))
pred00=array(NA,c(length(y),10,L00))
pred01=matrix(NA,length(y),L00)
Shap=matrix(0,length(y),L00);colnames(Shap)= gsub('.txt','',file0)
for(ii in 1:L00){pred00[,,ii]=as.matrix(read.table(paste0('out_valid/',file0[ii])));pred01[,ii]=read.table(paste0('out/',file0[ii]))[[1]]}
for(ii in 1:Nfold)   
{   
	print(Nfold-ii)   
	indtempT=foldsCVT[[ii]]+40   
	y0=y[-indtempT]   
	X0=data.frame(pred00[-indtempT,ii,])
	X1=data.frame(pred01[indtempT,])
	colnames(X0)=colnames(X1)=gsub('.txt','',file0)
	p=dim(X0)[2]   
	training.set=as.data.frame(cbind(factor(paste0("X",y0)),X0));colnames(training.set)[1]="Y"   
	test.set=as.data.frame(X1)
	#model0 = train(Y~.,data = training.set[-c(1:40),],method = method.list[2],trControl = train.control,verbosity=0)
	model0 = train(Y~.,data = training.set[-c(1:40),],method = method.list[2],trControl = train.control,verbosity=0)#AUC
	rfImp <- varImp(model0, scale = T);ind00=sort(-as.matrix(round(rfImp[[1]][,1],3)),ind=T)$ix;
	tempp=t(t(as.matrix(round(rfImp[[1]],3))[ind00,]));rownames(tempp)=gsub("\\\\","",gsub("`","",rownames(tempp)))
	tempp=t(t(tempp[unique(rownames(tempp)),]))
	score=tempp[,1];names(score)=gsub("\\\\","",gsub("`","",names(score))) 
	#optN=which(cumsum(score)/sum(score)>0.5)[1]
	if((i2<=5)&(i2!=1)){  
		model0 = train(Y~.,data = training.set[-c(1:40),c("Y",names(score)[1:optN])],method = method.list[i2],trControl = train.control,verbosity=0)}
	if(i2==6){model0 = train(Y~.,data = training.set[-c(1:40),c("Y",names(score)[1:optN])],method = method.list[i2],center = TRUE)}   
	if(i2>6){model0 = train(Y~.,data = training.set[-c(1:40),c("Y",names(score)[1:optN])],method = method.list[i2],trControl = train.control)}   
	if(i2!=1){predYT_valid[-c(1:40,indtempT),ii] <- round(as.numeric(predict(model0,training.set[-c(1:40),c("Y",names(score)[1:optN])],type="prob")[,2]),4)
	predYT[indtempT]= round(as.numeric(predict(model0,test.set,type="prob")[,2]),4)
	for(jjj in 1:optN){ 
		datatemp=test.set;datatemp[,names(score)[jjj]]=0
		temp000=as.numeric(predict(model0, datatemp,type="prob")[,2])
		temp000=pmin(pmax(temp000,0.001),0.999)
		Shap[indtempT,names(score)[jjj]]=log(temp000/(1-temp000))-log(predYT[indtempT]/(1-predYT[indtempT]))}
	}   
	if(i2==1){
		cv0=cv.glmnet(as.matrix(training.set[-c(1:40),-1]),training.set[-c(1:40),"Y"],family="binomial",alpha=1)   
		lamb0=cv0$lambda.min   
		mod0 <- glmnet(as.matrix(training.set[-c(1:40),-1]),training.set[-c(1:40),"Y"],family="binomial",alpha=1,lambda=lamb0)   
		temp111=exp(as.numeric(predict(mod0,as.matrix(training.set[-c(1:40),-1]))[,1]))   
		predYT_valid[-c(1:40,indtempT),ii]=apply(training.set[-c(1:40),-1],1,median)#round(temp111/(1+temp111),4)  
		temp111=exp(as.numeric(predict(mod0,as.matrix(test.set))[,1]))   
		predYT[indtempT]=apply(test.set,1,median)#round(temp111/(1+temp111),4) 
	}
}
summ0<-function(pred,Y){
	pred=as.numeric(pred);Y=as.numeric(Y)
	if(max(pred)==2)pred=pred-1
	if(max(Y)==2)Y=Y-1
	acc=sum((pred>0.5)==Y)/length(Y) 
	prec1=sum((pred>0.5)&(Y==1))/(sum(pred>0.5)+.00001) 
	prec0=sum((pred<=0.5)&(Y==0))/(sum(pred<=0.5)+.00001) 
	recall1=sum((pred>0.5)&(Y==1))/(sum(Y==1)+.00001) 
	recall0=sum((pred<0.5)&(Y==0))/(sum(Y==0)+.00001) 
	auc0=pROC::roc(Y,pred,smooth=F) 
	f1score=(1/(1/prec1+1/recall1)+1/(1/prec0+1/recall0))
	acc_sd=sqrt(acc*(1-acc)/(length(Y)));prec1_sd=sqrt(prec1*(1-prec1)/(sum(pred>0.5)+.00001))
	prec0_sd=sqrt(prec0*(1-prec0)/(sum(pred<=0.5)+.00001))
	recall1_sd=sqrt(recall1*(1-recall1)/(sum(Y==1)+.00001))
	recall0_sd=sqrt(recall0*(1-recall0)/(sum(Y==0)+.00001));
	temp1=as.numeric(ci.auc(Y,pred,method='delong'))
	auc_sd=diff(temp1)[1]/1.96
	prec10=rnorm(1000,prec1,prec1_sd)
	prec00=rnorm(1000,prec0,prec0_sd)
	recall10=rnorm(1000,recall1,recall1_sd)
	recall00=rnorm(1000,recall0,recall0_sd)
	f1score00=(1/(1/prec10+1/recall10)+1/(1/prec00+1/recall00))
	statt0=c(acc,acc_sd,prec1,prec1_sd,prec0,prec0_sd,recall1,recall1_sd,recall0,recall0_sd,f1score,sd(f1score00),as.numeric(auc0$auc),auc_sd) 
	names(statt0)=c('Accuracy','Accuracy_SD','Precision_case','Precision_case_SD','Precision_control','Precision_control_SD','Recall_case','Recall_case_SD',
	'Recall_control','Recall_control_SD','F1score','F1score_SD','AUC','AUC_SD')
	return(statt0)
}
summ1<-function(pred,Y){indtemp=which(!is.na(pred));return(summ0(pred[indtemp],Y[indtemp]))}
round(summ0(predYT[-c(1:40)],y[-c(1:40)]),4)
round(apply(apply(predYT_valid,2,summ1,Y=y),1,mean),4)
write.table(predYT,file=paste0("final/comb.txt"),quote=F,col.names=F,row.names=F) 
write.table(Shap,file=paste0("final/comb_Shap.txt"),quote=F,row.names=F)  
#write.table(predYT_valid,file=paste0("final_valid/",method.list[i1],"_",method.list[i2],".txt"),quote=F,col.names=F,row.names=F)   

Glmboost combine glmboost + lda2 + hdda optN=18
                 Accuracy    Precision_case Precision_control       Recall_case 
        0.8235294         0.9999991         0.7391301         0.6470584 
   Recall_control           F1score               AUC 
        0.9999994         0.8178566         0.7231834 
validation set 	
          Accuracy    Precision_case Precision_control       Recall_case 
        0.8694288         0.9529479         0.8152706         0.7774995 
   Recall_control           F1score               AUC 
        0.9612494         0.8676725         0.9488819

Glmboost combine glmboost + lda2 + hdda + glmnet optN=24
         Accuracy    Precision_case Precision_control       Recall_case 
        0.7647059         0.9090901         0.6956519         0.5882349 
   Recall_control           F1score               AUC 
        0.9411759         0.7571424         0.7128028 

         Accuracy    Precision_case Precision_control       Recall_case 
        0.8727621         0.9612813         0.8164402         0.7774995 
   Recall_control           F1score               AUC 
        0.9679160         0.8709328         0.9493003


Shap=Shap[-c(1:40),];X0=pred01[-c(1:40),]
colnames(X0)=colnames(Shap)=gsub('lda2_','AUC_',colnames(Shap))
Shap[is.na(Shap)]=0;Shap=as.data.frame(Shap);
aa=shap.plot.summary.wrap2(round(Shap,4),X = X0,dilute =1)
pdf(paste0('final/Shap_combmethod.pdf'))
print(aa)
dev.off()
