cd /overflow/tengfei/user/tengfei/projects/TMJ_longitudinal/20211201/prediction/NestCV;module load r/4.1.0;R
library(pROC)
library(dplyr)
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
A=read.csv('TMJOAI_Long_040422_Norm.csv',check.names = FALSE)
y=A[,1]
sub0=dir('out/')
STATT=matrix(NA,length(sub0),14);rownames(STATT)=gsub('.txt','',sub0);colnames(STATT)=c('Accuracy','Accuracy_SD','Precision_case','Precision_case_SD','Precision_control','Precision_control_SD','Recall_case','Recall_case_SD',
	'Recall_control','Recall_control_SD','F1score','F1score_SD','AUC','AUC_SD')
for(i in 1:length(sub0))
{
temp=read.table(paste0('out/',sub0[i]))[[1]]
STATT[i,]=round(summ0(temp[-c(1:40)],y[-c(1:40)]),5)
}
rownames(STATT)=gsub('lda2_','AUC_',rownames(STATT))
write.csv(STATT,'Performance_test_set_48methods.csv',quote=F)
max(STATT[,'F1score'])
temp=gsub('svmLinear','SVM',gsub('glmboost','glmboo',gsub('naive_bayes','NaiveBayes',rownames(STATT))))
SEL0=unlist(lapply(strsplit(temp,'_'),'[',1))
ML0=unlist(lapply(strsplit(temp,'_'),'[',2))
F1=STATT[,'F1score']
pdf('fig/F1score_test_set_by_selection_method.pdf')
par(mfrow=c(2,1))
boxplot(F1~SEL0)
stripchart(F1~SEL0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)    
boxplot(F1~ML0)
stripchart(F1~ML0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 
dev.off()
pdf('fig/AUC_test_set_by_selection_method.pdf')
AUC=STATT[,'AUC']   
par(mfrow=c(2,1))
boxplot(AUC~SEL0)
stripchart(AUC~SEL0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)    
boxplot(AUC~ML0)
stripchart(AUC~ML0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 
dev.off()
pdf('fig/ACC_test_set_by_selection_method.pdf')	   
ACC=STATT[,'Accuracy']   
par(mfrow=c(2,1))
boxplot(ACC~SEL0)
stripchart(ACC~SEL0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)    
boxplot(ACC~ML0)
stripchart(ACC~ML0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 
dev.off()
############################################################
cd /overflow/tengfei/user/tengfei/projects/TMJ_longitudinal/20211201/prediction/NestCV;module load r/4.1.0;R
library(pROC)
library(dplyr)
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
A=read.csv('TMJOAI_Long_040422_Norm.csv',check.names = FALSE)
y=A[,1]
sub0=dir('out_valid/')
STATT=matrix(NA,length(sub0),14);rownames(STATT)=gsub('.txt','',sub0);colnames(STATT)=c('Accuracy','Accuracy_SD','Precision_case','Precision_case_SD','Precision_control','Precision_control_SD','Recall_case','Recall_case_SD',
	'Recall_control','Recall_control_SD','F1score','F1score_SD','AUC','AUC_SD')
for(i in 1:length(sub0))
{
temp=read.table(paste0('out_valid/',sub0[i]))
STATT[i,]=apply(apply(temp,2,summ1,Y=y),1,mean)
}
rownames(STATT)=gsub('lda2_','AUC_',rownames(STATT))
write.csv(STATT,'Performance_valid_set_48methods.csv',quote=F)
max(STATT[,'F1score'])
temp=gsub('svmLinear','SVM',gsub('glmboost','glmboo',gsub('naive_bayes','NaiveBayes',rownames(STATT))))
SEL0=unlist(lapply(strsplit(temp,'_'),'[',1))
ML0=unlist(lapply(strsplit(temp,'_'),'[',2))
F1=STATT[,'F1score']
pdf('fig/F1score_valid_set_by_selection_method.pdf')
par(mfrow=c(2,1))
boxplot(F1~SEL0)
stripchart(F1~SEL0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)    
boxplot(F1~ML0)
stripchart(F1~ML0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 
dev.off()
pdf('fig/AUC_valid_set_by_selection_method.pdf')
AUC=STATT[,'AUC']   
par(mfrow=c(2,1))
boxplot(AUC~SEL0)
stripchart(AUC~SEL0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)    
boxplot(AUC~ML0)
stripchart(AUC~ML0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 
dev.off()
pdf('fig/ACC_valid_set_by_selection_method.pdf')
ACC=STATT[,'Accuracy']   
par(mfrow=c(2,1))
boxplot(ACC~SEL0)
stripchart(ACC~SEL0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)    
boxplot(ACC~ML0)
stripchart(ACC~ML0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 
dev.off()
############################################################
#SHP
cd /overflow/tengfei/user/tengfei/projects/TMJ_longitudinal/20211201/prediction/NestCV;module load r/4.1.0;R
library(SHAPforxgboost)
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
	statt0=c(acc,prec1,prec0,recall1,recall0,f1score,as.numeric(auc0$auc)) 
	names(statt0)=c('Accuracy','Precision_case','Precision_control','Recall_case','Recall_control','F1score','AUC')
	return(statt0)
}
A=read.csv('TMJOAI_Long_040422_Norm.csv',check.names = FALSE)
y=A[,1]
X=A[,-1]   
sub0=dir('out/')
STATT=matrix(NA,length(sub0),7);rownames(STATT)=gsub('.txt','',sub0);colnames(STATT)=c('Accuracy','Precision_case','Precision_control','Recall_case','Recall_control','F1score','AUC')
for(i in 1:length(sub0))
{
temp=read.table(paste0('out/',sub0[i]))[[1]]
STATT[i,]=summ0(temp[-c(1:40)],y[-c(1:40)])
}
max(STATT[,'F1score'])
#hdda_lda2
library(SHAPforxgboost)
pat='xgbTree_glmboost'
ind00=which((STATT[,'F1score']>0.65)&((STATT[,'AUC']>0.65)))
patT=names(ind00)[1:3]
aa=NULL
for(i in 1:length(patT)){
print(i)
Shap=read.table(paste0('Shap/',patT[i],'.txt'),head=T)[-c(1:40),]
X0=X[-c(1:40),];colnames(X0)=colnames(Shap)
shap0<- shap.prep(shap_contrib = round(Shap,4), X_train = X0)
aa[[i]]=shap.plot.summary.wrap2(round(Shap,4),X = X0,dilute =1, top_n = 15)
}
pdf('Shap.pdf')
for(i in 1:3)plot(aa[[i]])
dev.off()


#ind00=which((STATT[,'F1score']>0.1)&((STATT[,'AUC']>0.1)))[-3] #8 methods
#patT=c('nnet_lda2','rf_hdda','nnet_glmboost','nnet_hdda','glmnet_lda2','glmboost_lda2','lda2_hdda','xgbTree_lda2','glmnet_hdda')#names(ind00)
patT=c('nnet_lda2','rf_hdda','nnet_glmboost','nnet_hdda','glmnet_lda2','glmboost_lda2')#names(ind00)
X0=NULL;Shap=NULL
for(i in 1:length(patT)){
print(i)
Shap=rbind(Shap,read.table(paste0('Shap/',patT[i],'.txt'),head=T)[-c(1:40),])
X0=rbind(X0,X[-c(1:40),]);
}
colnames(Shap)=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',colnames(Shap))))))
colnames(X0)=colnames(Shap)
Shap[is.na(Shap)]=0;
aa=shap.plot.summary.wrap2(round(Shap,4),X = X0,dilute =1, top_n = 40)
pdf(paste0('fig/Shap_comb_feaT.pdf'))
print(aa)
dev.off()
ab=aggregate(aa[[1]]$mean_value,by=list(aa[[1]]$variable),FUN=mean)
indd=which(cumsum(ab[,2])/sum(ab[,2])>0.9)[1]
var0=ab[1:indd,1]

glmboost_glmboost   glmnet_glmboost     nnet_glmboost         nnet_lda2 
                1                 9                25                28 
          rf_lda2  xgbTree_glmboost      xgbTree_hdda 
               36                41                43 

 [1] clinic.Headache      C.highGreyLevelRun   clinic.LowerBackPain
 [4] Sal.oPG              C.shortRunHighGrey   Af.BSBV             
 [7] clinic.RestlessSleep JS.3D_Sc_Sf          Sal.vEGF            
[10] C.BVTV               Sal.mMP7             Ser.bDNF            
[13] Sal.angiogenin       clinic.MouthOpen     Af.greyLevelNonunif 
[16] Ser.eNA78            Af.longRunHighGrey   Af.correlation      
[19] Af.BVTV              Af.shortRunEmph      demo.Age 

library(ggplot2)
X0=X[-c(1:40),];colnames(X0)=gsub('\\+','\\.',colnames(X0))
colnames(X0)=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',colnames(X0))))))
feature30=scale(X0[,as.character(var0)])[T]
class0=rep(y[-c(1:40)],dim(X0[,as.character(var0)])[2])
class1=class0
name0=t(matrix(rep(colnames(X0[,as.character(var0)]),dim(X0[,as.character(var0)])[1]),dim(X0[,as.character(var0)])[2]))[T]
SCORE2=ab[1:indd,2];names(SCORE2)=ab[1:indd,1]
data00=cbind(name0,class1,feature30,as.matrix(SCORE2)[T])
colnames(data00)=c('Feature','Group','Score','Score0')
data00=data.frame(data00)
data00$Score=as.numeric(as.character(data00$Score))
data00$Score0=as.numeric(as.character(data00$Score0))
data00$Feature=as.factor(data00$Feature)
data00$Group=as.factor(data00$Group)
data00[,1]=factor(data00[,1])
temp0=-sort(-unique(SCORE2))[10]
data00=data00[data00$Score0>temp0,]
grDevices::cairo_pdf(paste0('fig/','/boxplot_rf_event.pdf'))
ggplot(aes(y = Score, x = reorder(Feature,-Score0,mean), fill = Group), data = data00) +
geom_boxplot()+ggtitle(paste0("Boxplots of top features for getting better (0) vs worse (1) "))+
theme(axis.text.x = element_text(face="bold",angle = 90, hjust = 1),
plot.title = element_text(lineheight=.8, face="bold",hjust=0.5))+xlab('')+ylab('Normalized Measures')
dev.off()





###################################
#AUCplot
###################################
library(ggplot2)
sub0=Sys.glob('out/*.txt')
name0=gsub('out/','',gsub('.txt','',sub0))
PredTest=matrix(NA,length(y),length(sub0));
colnames(PredTest)=name0
for(i in 1:length(sub0))
{
A=read.table(sub0[i])[[1]]
PredTest[,i]=A
}
PredTest=cbind(PredTest,y)
colnames(PredTest)[length(sub0)+1]='Y'
PredTest=as.data.frame(PredTest)
PredY=predict(mod0,PredTest)
PredY=exp(PredY)
PredY=PredY/(1+PredY)
STATT1=summ0(PredY,PredTest$Y)
PredTest=cbind(PredTest[,-dim(PredTest)[2]],PredY,PredTest[,dim(PredTest)[2]])
colnames(PredTest)[dim(PredTest)[2]]='Y'
res=apply(as.matrix(PredTest)[-c(1:40),],2,summ0,y[-c(1:40)])
PredTest1=PredTest[,setdiff(names(sort(-res['F1score',])[1:4]),'Y')]
PredTest1=cbind(PredTest1,read.table('final/comb.txt')[[1]])
colnames(PredTest1)[4]='Combine'
colnames(PredTest1)=gsub('naive_bayes_','auc_',gsub('hdda_','auc_',gsub('svmLinear_','auc_',colnames(PredTest1))))
#PredTest1=cbind(PredTest1,X[,var0[1:7]])
PredTest1=PredTest1[-c(1:40),]
PredTest1=as.data.frame(PredTest1)
PredTest1$Y=y[-c(1:40)]


grDevices::cairo_pdf(paste0('fig/','rocT_test.pdf'))
par(mar=c(10.1, 4.1, 10.1, 9.1), xpd=TRUE)
type0=c(rep(1,7),rep(3,6))
smooth0=F
wid0=2.1
col0=c('black','gray47','blue','darkorange','red','darkgreen','purple','mediumvioletred',
'darkred','darkgoldenrod4','slateblue4','green','gold')
pROC::plot.roc(as.numeric(PredTest1$Y)~PredTest1[,1],smooth=smooth0,lty=type0[1],lwd=wid0,col=col0[1],cex.lab=1.3,cex.axis=1.3)
for(i in 1:3)
{
pROC::plot.roc(as.numeric(PredTest1$Y)~PredTest1[,i+1],smooth=smooth0,lty=type0[i+3],
lwd=wid0,col=col0[i+1],cex.lab=1.3,cex.axis=1.3,add=T)
}
legend('bottomright', legend=colnames(PredTest1)[-dim(PredTest1)[2]],
       col=col0[1:4], lty=type0[c(1,4:6)], cex=0.75,lwd=rep(2,4),
       text.font=1,box.lwd=0,box.col='white',bg='lightgray')
dev.off()
#####
Imp=Sys.glob('imp/*.txt')
out='fig/'
for(ii in 1:length(Imp)){
	grDevices::cairo_pdf(paste0(out,gsub('.txt','.pdf',Imp[ii]))) 
	score2_1=read.table(Imp[ii]);temp=score2_1[,1];score2_1=score2_1[,-1];names(score2_1)=temp
	data00=cbind(names(score2_1),score2_1);colnames(data00)=c('TopFeatures','Select_importance')
	data00=as.data.frame(data00);rownames(data00)=NULL;data00[,2]=as.numeric(as.character(data00[,2]))
	indd=which(cumsum(score2_1)/sum(score2_1)>0.99)[1];data00=data00[1:indd,]
	print(eval(substitute(ggplot(data00, aes(x=reorder(var1,var2,mean), y=var2)) + geom_bar(stat="identity", 
			width=0.5,color="tan4",fill="darkslateblue",alpha=0.2, 
			size=0.5,)+coord_flip()+ ggtitle(paste0("Feature Importance:"))+
			stat_summary(fun.y=mean, geom="point", shape=20, size=1, color="red", fill="red") +
			theme(axis.text.x = element_text(face="bold",angle = 90, hjust = 1)),
			list(var1=as.name(colnames(data00)[1]),var2=as.name(colnames(data00)[2])))))
	dev.off() 
}

#####
#meth=c('glmboost','glmnet','xgbTree','nnet','')
meth=c('glmboost','glmnet','rf','nnet')
out='fig/'
for(ii in 4:4){
	grDevices::cairo_pdf(paste0(out,gsub('.txt','.pdf',meth[ii]))) 
	Imp=Sys.glob(paste0('imp/',meth[ii],'*.txt'))
	score2_1=read.table(Imp[jj]);tempT=score2_1[,1];score2_1=score2_1[,-1];names(score2_1)=tempT
	for(jj in 2:length(Imp)){
	score2_1temp=read.table(Imp[jj]);temp=score2_1temp[,1];score2_1temp=score2_1temp[,-1];names(score2_1temp)=temp
	score2_1=cbind(score2_1,score2_1temp[tempT])
	}
	score2_1=sort(apply(score2_1,1,mean),decreasing=T)
	data00=cbind(names(score2_1),score2_1);colnames(data00)=c('TopFeatures','Select_importance')
	data00=as.data.frame(data00);rownames(data00)=NULL;data00[,2]=as.numeric(as.character(data00[,2]))
	indd=which(cumsum(score2_1)/sum(score2_1)>0.95)[1];data00=data00[1:indd,]
	print(eval(substitute(ggplot(data00, aes(x=reorder(var1,var2,mean), y=var2)) + geom_bar(stat="identity", 
			width=0.5,color="tan4",fill="darkslateblue",alpha=0.2, 
			size=0.5,)+coord_flip()+ ggtitle(paste0("Feature Importance:"))+
			stat_summary(fun.y=mean, geom="point", shape=20, size=1, color="red", fill="red") +
			theme(axis.text.x = element_text(face="bold",angle = 90, hjust = 1)),
			list(var1=as.name(colnames(data00)[1]),var2=as.name(colnames(data00)[2])))))
	dev.off() 
}


###################################
#AUCplot
###################################
library(ggplot2)
#sub0=Sys.glob('out/*.txt')
sub0=c('out/nnet_lda2.txt','out/rf_hdda.txt','out/nnet_glmboost.txt','out/nnet_hdda.txt','out/glmnet_lda2.txt','out/glmboost_lda2.txt')
name0=gsub('out/','',gsub('.txt','',sub0))
PredTest=matrix(NA,length(y),length(sub0));
colnames(PredTest)=name0
for(i in 1:length(sub0))
{
A=read.table(sub0[i])[[1]]
PredTest[,i]=A
}
PredTest=cbind(PredTest,y)
colnames(PredTest)[length(sub0)+1]='Y'
PredTest=as.data.frame(PredTest)
PredTest1=cbind(PredTest,read.table('final/comb.txt')[[1]])
colnames(PredTest1)[dim(PredTest1)[2]]='Combine'
colnames(PredTest1)=gsub('naive_bayes_','auc_',gsub('hdda_','auc_',gsub('svmLinear_','auc_',colnames(PredTest1))))
#PredTest1=cbind(PredTest1,X[,var0[1:7]])
PredTest1=PredTest1[-c(1:40),]
PredTest1=as.data.frame(PredTest1)
PredTest1$Y=y[-c(1:40)]
PredTest1=PredTest1[,c(1:6,8,7)]


grDevices::cairo_pdf(paste0('fig/','rocT_test.pdf'))
par(mar=c(10.1, 4.1, 10.1, 9.1), xpd=TRUE)
type0=c(rep(1,7),rep(3,6))
smooth0=F
wid0=2.1
col0=c('black','gray47','blue','darkorange','red','darkgreen','purple','mediumvioletred',
'darkred','darkgoldenrod4','slateblue4','green','gold')
pROC::plot.roc(as.numeric(PredTest1$Y)~PredTest1[,1],smooth=smooth0,lty=type0[1],lwd=wid0,col=col0[1],cex.lab=1.3,cex.axis=1.3)
for(i in c(1:6))
{
pROC::plot.roc(as.numeric(PredTest1$Y)~PredTest1[,i+1],smooth=smooth0,lty=type0[i+3],
lwd=wid0,col=col0[i+1],cex.lab=1.3,cex.axis=1.3,add=T)
}
legend('bottomright', legend=colnames(PredTest1)[-dim(PredTest1)[2]],
       col=col0[1:7], lty=type0[c(1,4:9)], cex=0.75,lwd=rep(2,7),
       text.font=1,box.lwd=0,box.col='white',bg='lightgray')
dev.off()
#####