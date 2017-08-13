## Project - 2 ##
## DVD SALES ##

#Data Importing and Summary
dvd=read.csv("Sales_dataset.csv")
View(dvd)
str(dvd)
summary(dvd)

#Correlation of predictor variable with response variables
cor(dvd$sales,dvd$advertise)
cor(dvd$sales,dvd$plays)
cor(dvd$sales,dvd$attractiveness)

#Model Preparation To Check Realtions
dvdmod1=lm(sales~.,dvd)
summary(dvdmod1)
dvd.fitted1=data.frame(dvd,"fitted"=fitted(dvdmod1),"residual"=resid(dvdmod1))
View(dvd.fitted1)

#Converting categorical variable to Factor
table(dvd$attractiveness)
dvd$attractiveness=as.factor(dvd$attractiveness)
summary(dvd)

#Model after Feauture Engineering
dvdmod2=lm(sales~.,dvd)
summary(dvdmod2)
plot(dvdmod2)

dvd.fitted2=data.frame(dvd,"fitted"=fitted(dvdmod2),"residual"=resid(dvdmod2))
View(dvd.fitted2)

#DataSet split in training an dtest data 
set.seed(123)
split=sample(nrow(dvd),nrow(dvd)*0.7)
dvdtrain=dvd[split,]
dvdtest=dvd[-split,]

#Model preparartion from train data
model=lm(sales~.,dvdtrain)
summary(model)
plot(model)

#Sales prediction for test data
sales.pred=predict(model,dvdtest)
new.dvdtest=dvdtest[,c(1,3,4,2)]
dvd.pred=data.frame(new.dvdtest,sales.pred,"diff"=new.dvdtest$sales-sales.pred)
View(dvd.pred)

#Final Predicted sales for test data
final=dvd.pred[,c(-4,-6)]
View(final)
