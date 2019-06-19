LungCap<-read.csv("C:/Users/AKHIL/Desktop/New folder/LungCapData.csv")


LungCap<-mutate(LungCap,Smoke1=ifelse(Smoke=="no",0,1))
LungCap<-mutate(LungCap,Gender1=ifelse(Gender=="male",1,0))
LungCap<-mutate(LungCap,Caesarean1=ifelse(Caesarean=="no",0,1))
LungCap

LCN<-LungCap[,c(1,2,3,7,8,9)]
summary(LCN)


LCN$Smoke1<-factor(LCN$Smoke1)           ##converting into factor
LCN$Gender1<-factor(LCN$Gender1)
LCN$Caesarean1<-factor(LCN$Caesarean1)
summary(LCN)



LCN_Sam<-sample(2,nrow(LCN),replace=TRUE,prob=c(.9,.1))
LCN_Train<-LCN[LCN_Sam==1,]
LCN_Test<-LCN[LCN_Sam==2,]
LCN_Train;LCN_Test


model_lm<-lm(LungCap~.,data=LCN_Train)
pred_lm<-predict(model_lm,LCN_Test)
pred_actual<-data.frame(pred_lm,LCN_Test$LungCap)
pred_actual
summary(model_lm)


model_lm<-lm(LungCap~Height+Gender1+Age,data=LCN_Train)
pred_lm<-predict(model_lm,LCN_Test)
pred_actual<-data.frame(pred_lm,LCN_Test$LungCap)
pred_actual
summary(model_lm)


pred_actual<-data.frame(pred_lm,LCN_Test$LungCap,sum_error)
pred_actual<-mutate(pred_actual,error_1=pred_lm-LCN_Test$LungCap,er1r_sq=error_1^2)
View(pred_actual)
sum_error<-sum(pred_actual$error_sq)
View(sum_error)
