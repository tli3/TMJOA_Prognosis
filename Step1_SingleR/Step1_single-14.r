iii=1 
iii=14 
method.list = c("glmnet", "svmLinear", "rf", "xgbTree", "lda2","nnet","glmboost","hdda")    
vecT=expand.grid(c(1,3,4,5,6,7),1:8)    
i1=vecT[iii,1]    
i2=vecT[iii,2]    
library(caret)    
library(glmnet)    
library(Matrix)    
library(qqman)    
library(MLmetrics)    
library(ggplot2) # Data visualization    
library(data.table)    
library(caret)    
library(LiblineaR)    
library(xgboost)    
library(lightgbm)    
library(MASS)    
options(scipen=999)    
library(data.table)    
if(!require("kernlab"))install.packages("kernlab",repos = "http://cran.us.r-project.org")    
library(pls)    
library(randomForest)    
A=read.csv("../TMJOAI_Long_040422_Norm.csv",check.names = FALSE)    
y=A[,1]    
X=A[,-1] 
Nfold=10    
N=10    
seed0=2022    
set.seed(seed0)    
foldsCVT <- createFolds(factor(y)[-(1:40)], k=Nfold, list=TRUE, returnTrain=FALSE)    
train.control <- trainControl(method = "cv", number = 10, # k-folds CV with k=10    
                              classProbs = TRUE,    
                              savePredictions = TRUE,    
                              summaryFunction = multiClassSummary)# save predictions for ROC    
predYT=matrix(NA,length(y),1)  
Shap=matrix(0,length(y),dim(X)[2]);colnames(Shap)= colnames(X)   
select=rep(NA,10)    
predYT_valid=matrix(NA,length(y),10)    
for(ii in 1:Nfold)    
{    
	print(Nfold-ii)    
	indtempT=foldsCVT[[ii]]+40    
	y0=y[-indtempT]    
	X0=X[-indtempT,]    
	X1=X[indtempT,]    
	p=dim(X0)[2]    
	training.set=as.data.frame(cbind(factor(paste0("X",y0)),X0));colnames(training.set)[1]="Y"    
	test.set=as.data.frame(X1)    
	#W0=table(training.set$Y)/sum(table(training.set$Y));w0=training.set$Y;w00=rep(NA,length(w0));w00[w0==names(W0)[1]]=W0[2];w00[w0==names(W0)[2]]=W0[1];    
	if(i1==1){  
		fea=matrix(NA,dim(X)[2],Nfold*N);rownames(fea)=colnames(X)   	 
		kk=1    
		predY=matrix(NA,length(y0),10)    
		for(seed1 in 2020:(2020+N-1))    
		{    
			if(seed1%%50==0)print(c((2020+N-1)-seed1,Nfold-ii))    
			set.seed(seed1)    
			foldsCV <- createFolds(factor(y0)[-(1:40)], k=Nfold, list=TRUE, returnTrain=FALSE)    
			for(i in 1:Nfold)    
			{    
				indtemp=foldsCV[[i]]+40    
				Y1=y0[-indtemp]    
				X.fea0=X0[-indtemp,]    
				p=dim(X.fea0)[2]    
				cv0=cv.glmnet(as.matrix(X.fea0),factor(Y1),family="binomial",alpha=1)    
				lamb0=cv0$lambda.min    
				mod0=glmnet(as.matrix(X.fea0),factor(Y1),family="binomial",alpha=1,lambda=lamb0)    
				fea[,kk]=mod0[[2]][,1]    
				kk=kk+1    
				predY[indtemp,seed1-2019]=predict(mod0,as.matrix(X[indtemp,]))    
			}    
		}    
		score=apply(fea!=0,1,mean)    
		score=sort(score,decreasing=T)    
	}    
	if((i1<=5)&(i1!=1)){    
	model0 = train(Y~.,data = training.set,method = method.list[i1],trControl = train.control,verbosity=0)}    
	if(i1==6){    
	model0 = train(Y~.,data = training.set,method = method.list[i1],center = TRUE)}    
	if(i1>6){    
	model0 = train(Y~.,data = training.set,method = method.list[i1],trControl = train.control)}    
	if(i1!=1){    
		rfImp <- varImp(model0, scale = T);ind00=sort(-as.matrix(round(rfImp[[1]][,1],3)),ind=T)$ix;   
		tempp=t(t(as.matrix(round(rfImp[[1]],3))[ind00,]));rownames(tempp)=gsub("\\","",gsub("`","",rownames(tempp)))  
		tempp=t(t(tempp[unique(rownames(tempp)),])) 
		write.table(tempp,file=paste0("imp/",method.list[i1],"_",ii,".txt"),quote=F,col.names=F)    
		score=tempp[,1];names(score)=gsub("\\","",gsub("`","",names(score)))  
	}  
	if(i1==1){    
	write.table(t(t(score)),file=paste0("imp/",method.list[i1],"_",ii,".txt"),quote=F,col.names=F)    
	}    
	selectT=c(5,10,15,20)    
	selectL=length(selectT)    
	AUCT=rep(NA,length(selectT))    
	for(jj in 1:length(selectT)){    
	if(i2<=5){model0 = train(Y~.,data = training.set[,c("Y",names(score)[1:selectT[jj]])],method = method.list[i2],trControl = train.control,verbosity=0)}    
	if(i2==6){model0 = train(Y~.,data = training.set[,c("Y",names(score)[1:selectT[jj]])],method = method.list[i2],center = TRUE)}    
	if(i2>6){model0 = train(Y~.,data = training.set[,c("Y",names(score)[1:selectT[jj]])],method = method.list[i2],trControl = train.control)}    
	AUCT[jj]=max(model0[[4]]$AUC)}    
	select[ii]=selectT[which.max(AUCT)]    
	for(jj in setdiff(1:10,ii)){    
		indvalidT=foldsCVT[[jj]]+40    
		y0_valid=y[-c(indtempT,indvalidT)]    
		X0_valid=X[-c(indtempT,indvalidT),]    
		X1_valid=X[indvalidT,]    
		training.set1=as.data.frame(cbind(factor(paste0("X",y0_valid)),X0_valid));colnames(training.set1)[1]="Y"    
		valid.set=as.data.frame(X1_valid)    
		if((i2<=5)&(i2!=1)){    
		model0 = train(Y~.,data = training.set1[,c("Y",names(score)[1:select[ii]])],method = method.list[i2],trControl = train.control,verbosity=0)}    
		if(i2==6){model0 = train(Y~.,data = training.set1[,c("Y",names(score)[1:select[ii]])],method = method.list[i2],center = TRUE)}    
		if(i2>6){model0 = train(Y~.,data = training.set1[,c("Y",names(score)[1:select[ii]])],method = method.list[i2],trControl = train.control)}    
		if(i2!=1){predYT_valid[indvalidT,ii] <- round(as.numeric(predict(model0, valid.set,type="prob")[,2]),4)}    
		if(i2==1){    
			cv0=cv.glmnet(as.matrix(training.set1[,c(names(score)[1:select[ii]])]),training.set1[,"Y"],family="binomial",alpha=1)    
			lamb0=cv0$lambda.min    
			mod0 <- glmnet(as.matrix(training.set1[,c(names(score)[1:select[ii]])]),training.set1[,"Y"],family="binomial",alpha=1,lambda=lamb0)    
			temp111=exp(as.numeric(predict(mod0,as.matrix(valid.set[,c(names(score)[1:select[ii]])]))[,1]))    
			predYT_valid[indvalidT,ii]=round(temp111/(1+temp111),4)    
		}    
	}    
	if(i2==1){    
		cv0=cv.glmnet(as.matrix(training.set[,c(names(score)[1:select[ii]])]),training.set[,"Y"],family="binomial",alpha=1)    
		lamb0=cv0$lambda.min    
		mod0 <- glmnet(as.matrix(training.set[,c(names(score)[1:select[ii]])]),training.set[,"Y"],family="binomial",alpha=1,lambda=lamb0)    
		temp111=exp(as.numeric(predict(mod0,as.matrix(test.set[,c(names(score)[1:select[ii]])]))[,1]))    
		predYT[indtempT]=round(temp111/(1+temp111),4) 
		for(jjj in 1:select[ii]){ 
		datatemp=test.set;datatemp[,names(score)[jjj]]=0 
		temp000=as.numeric(predict(mod0,as.matrix(datatemp[,c(names(score)[1:select[ii]])]))[,1]) 
		Shap[indtempT,names(score)[jjj]]=temp000-log(predYT[indtempT]/(1-predYT[indtempT]))} 
	}    
	if((i2<=5)&(i2!=1)){    
	model0 = train(Y~.,data = training.set[,c("Y",names(score)[1:select[ii]])],method = method.list[i2],trControl = train.control,verbosity=0)}    
	if(i2==6){model0 = train(Y~.,data = training.set[,c("Y",names(score)[1:select[ii]])],method = method.list[i2],center = TRUE)}    
	if(i2>6){model0 = train(Y~.,data = training.set[,c("Y",names(score)[1:select[ii]])],method = method.list[i2],trControl = train.control)}    
    if(i2!=1){predYT[indtempT] <- round(as.numeric(predict(model0, test.set,type="prob")[,2]),4) 
		predYT[indtempT]=pmin(pmax(predYT[indtempT],0.001),0.999) 
		for(jjj in 1:select[ii]){  
		datatemp=test.set;datatemp[,names(score)[jjj]]=0 
		temp000=as.numeric(predict(model0, datatemp,type="prob")[,2]) 
		temp000=pmin(pmax(temp000,0.001),0.999) 
		Shap[indtempT,names(score)[jjj]]=log(temp000/(1-temp000))-log(predYT[indtempT]/(1-predYT[indtempT]))} 
	} 
}    
write.table(predYT,file=paste0("out/",method.list[i1],"_",method.list[i2],".txt"),quote=F,col.names=F,row.names=F)    
write.table(predYT_valid,file=paste0("out_valid/",method.list[i1],"_",method.list[i2],".txt"),quote=F,col.names=F,row.names=F)    
write.table(select,file=paste0("select/",method.list[i1],"_",method.list[i2],".txt"),quote=F,col.names=F,row.names=F)    
write.table(Shap,file=paste0("Shap/",method.list[i1],"_",method.list[i2],".txt"),quote=F,row.names=F) 
