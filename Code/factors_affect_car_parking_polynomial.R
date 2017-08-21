#Creche Parking data for working day as testing(10%) and training(90%)

training=read.csv("D://Data Analysis//Practicum//DCU car parking data//Final_Data//creche_Training.csv")
test=read.csv("D://Data Analysis//Practicum//DCU car parking data//Final_Data//creche_Test.csv")

#To check distribution of training data
plot(training$Occupancy~training$Hour)

#devlop parking model to predict parking tren on Working days for Creche/Library/Invent
parking_working=lm(Occupancy~poly(Hour,10,raw=TRUE),data=training)

#Check Summary
summary(parking_working)

#Test and Predict model 
pred=predict(parking_working,test)
pred

#Check test data distribution

plot(test$Occupancy~test$Hour,ylab="Parking Space % Free",xlab = "Time")



#Combine prediction and testing data Together
creche=cbind(test,pred)
creche
write.csv(d,"D://Data Analysis//Practicum//Result data//test.csv")

help("write.csv")
#Plot test data and Pediction together for Creche


plot(creche$Hour,creche$Occupancy,lwd=2,
     xlim=c(0,23),ylim=c(0,100),col="black",type='p',
     xlab="Time",ylab="Parking Space(%) Available",
     main="Prediction of Creche Car Parking on Working Day")
lines(creche$pred,col="red",type="l",lwd=2)


legend("topleft",legend=c("Actual","Predicted"),
       lty=1,lwd=2,pch=21,col=c("black","red"),
       ncol=2,bty="n",cex=0.8,
       text.col=c("black","red"),
       inset=0.01,text.font = 2)

library(forecast)
accuracy(pred,test$Occupancy)

#Checking residual and accuracy of model tested using creche data a
#It gives 86.04% accuracy

residual=data.frame(test$Hour,test$Occupancy,pred,(test$Occupancy-pred),(test$Occupancy-pred)^2)
print(residual)
SE = sqrt(sum(residual$X.test.Occupancy...pred..2)/48)
print(SE)

accuracy=100-(SE)
accuracy

#Lets test model for other parking type for Example Library on Working Day

test_library=read.csv("D://Data Analysis//Practicum//DCU car parking data//Final_Data//Prediction Files//Library_testing.csv")



#Test and Predict model 
pred_library=predict(parking_working,test_library)
pred_library

#Check test data distribution

plot(test_library$Occupancy~test_library$Hour,ylab="Parking Space % Free",xlab = "Time")



#Combine prediction and testing data Together
lib=cbind(test_library,pred_library)
lib

#Plot test data and Pediction together for Invent



plot(lib$Hour,lib$Occupancy,lwd=2,
     xlim=c(0,23),ylim=c(0,100),col="black",type='p',
     xlab="Time",ylab="Parking Space(%) Available",
     main="Prediction of Library Car Parking on Working Day")
lines(lib$pred_library,col="red",type="l",lwd=2)


legend("topleft",legend=c("Actual","Predicted"),
       lty=1,lwd=2,pch=21,col=c("black","red"),
       ncol=2,bty="n",cex=0.8,
       text.col=c("black","red"),
       inset=0.01,text.font = 2)
accuracy(pred_library,test_library$Occupancy)

#Checking residual and accuracy of model tested using library data a
#It gives 74.94% accuracy

residual_library=data.frame(test_library$Hour,test_library$Occupancy,pred_library,(test_library$Occupancy-pred_library),(test_library$Occupancy-pred_library)^2)
print(residual_library)
SE_lib = sqrt(sum(residual_library$X.test_library.Occupancy...pred_library..2/1367))
print(SE_lib)

accuracy_lib=100-(SE_lib)
accuracy_lib



#Lets test model for other parking type for Example Invent on Working Day

test_invent=read.csv("D://Data Analysis//Practicum//DCU car parking data//Final_Data//Prediction Files//Invent_testing.csv")



#Test and Predict model 
pred_invent=predict(parking_working,test_invent)
pred_invent

#Check test data distribution

plot(test_invent$Occupancy~test_invent$Hour,ylab="Parking Space % Free",xlab = "Time")



#Combine prediction and testing data Together
invent=cbind(test_invent,pred_invent)
invent

#Plot test data and Pediction together for Invent


plot(invent$Hour,invent$Occupancy,lwd=2,
     xlim=c(0,23),ylim=c(0,100),col="black",type='p',
     xlab="Time",ylab="Parking Space(%) Available",
     main="Prediction of Invent Car Parking on Working Day")
lines(invent$pred_invent,col="red",type="l",lwd=2)

legend("topleft",legend=c("Actual","Predicted"),
       lty=1,lwd=2,pch=21,col=c("black","red"),
       ncol=2,bty="n",cex=0.8,
       text.col=c("black","red"),
       inset=0.01,text.font = 2)

accuracy(pred_invent,test_invent$Occupancy)

#Checking residual and accuracy of model tested using invent data 
#It gives 84.22% accuracy

residual_invent=data.frame(test_invent$Hour,test_invent$Occupancy,pred_invent,(test_invent$Occupancy-pred_invent),(test_invent$Occupancy-pred_invent)^2)
print(residual_invent)
SE_invent = sqrt(sum(residual_invent$X.test_invent.Occupancy...pred_invent..2/1367))
print(SE_invent)

accuracy_invent=100-(SE_invent)
accuracy_invent

