#Q1

#1
calories=read.csv("C:/Users/Shinkar/Desktop/vss 2020/Excelr/Assignments/Simple Linear Regression/calories_consumed.csv")
calories
attach(calories)
names(calories)<- c("weight","cal")
calories
attach(calories)
summary(calories)
#Weight: Min=62, Max=1100  Cal: Min=1400, Max= 3900
plot(weight,cal) # data has strong positive relationship
cor(weight,cal)  # weight and cal has stong correlation #0.946991
reg<-lm(weight~cal,data=calories)
summary(reg)   #R^2=0.8968, B0= -625.75236, B1= 0.42016
reg$residuals  
# reg$coefficients 
reg$fitted.values
mean(reg$residuals)
ggplot(data=calories,aes(x=cal,y=weight))+geom_point(color='blue')+geom_line(color='red',data=calories,aes(x=cal,y=reg$fitted.values))

#2. SQRT Transformation
attach(calories)
plot(weight,cal) 
plot(sqrt(cal),weight) #strong positive relationship
cor(weight,sqrt(cal)) #0.9255962 has strong correlation but decreased than before.
reg_sqrt_cal<-lm(weight~sqrt(cal))
summary(reg_sqrt_cal) #R^2 0.8567 is decreased

#3. log transformation
plot(log(cal),weight) #strong positive relationship 
cor(weight,log(cal))  #0.8987253, decreased
reg_log_cal<-lm(weight~log(cal))
summary(reg_log_cal) #R^2= 0.8077 decreased

#4. SQRT transformation of Y variable
plot(cal,sqrt(weight)) #strong positive relationship
cor(cal,sqrt(weight)) #0.9559736 has increased
reg_sqrt_wt<-lm(sqrt(weight)~cal)
summary(reg_sqrt_wt) #R^2=0.9139 has increased
reg_sqrt_wt$fitted.values
pred_sqrt_y<- (reg_sqrt_wt$fitted.values)^2 
pred_sqrt_y
err_pred_sqrt_y<-weight-pred_sqrt_y
err_pred_sqrt_y
mean(reg_sqrt_wt$residuals)

#5. Polynomial Transformation log y
attach(calories)
calories["cal^2"]<-cal*cal
calories
cor(log(weight),cal^2) #0.9710636 has increased
reg_poly<-lm(log(weight)~cal+cal^2,data=calories)
summary(reg_poly) # R^2=0.8968 has decreased
pred_poly_y<-exp(reg_poly$fitted.values)
pred_poly_y
err_pred_poly_y<- weight-pred_poly_y
err_pred_poly_y

#6. Polynomial Transformation sqrt y
#attach(calories)
calories["cal^2"]<-cal*cal
calories
cor(sqrt(weight),cal^2) #0.9627241
reg_poly_sqrt_y<-lm(sqrt(weight)~cal+cal^2,data=calories)
summary(reg_poly_sqrt_y) #R^2= 0.9139
pred_poly_y_sqrt<-(reg_poly$fitted.values)*(reg_poly$fitted.values)
pred_poly_y_sqrt
err_pred_poly_y_sqrt<- weight-pred_poly_y_sqrt
err_pred_poly_y_sqrt


  

#RMSE:
#1.
sqrt(mean(reg$residuals^2)) #103.3025
#2.
sqrt(mean(reg_sqrt_cal$residuals^2)) # 121.7122
#3.
sqrt(mean(reg_log_cal$residuals^2)) #141.0054
#4.
sqrt(mean(err_pred_sqrt_y^2)) # 73.74643
#5.
sqrt(mean(err_pred_poly_y^2)) #118.0452
#6.
sqrt(mean(err_pred_poly_y_sqrt^2)) # 452.2096



Model=c("-","sqrt_x","log_x","sqrt_y","poly_log_y","poly_sqrt_y")
r=c(0.946991,0.9255962,0.8987253,0.9559736,0.9710636,0.9627241)
R_2=c(0.8968,0.8567,0.8077,0.9139,0.8968,0.9139)
RMSE=c(103.3025,121.7122,141.0054,73.74643,118.0452,452.2096)
df=data.frame(Model,r,R_2,RMSE)
df
#so we prefer model 4 i.e. sqrt(weight) as it has max R^2 and minimum RMSE.


#Q2

#1.
time=read.csv("C:/Users/Shinkar/Desktop/vss 2020/Excelr/Assignments/Simple Linear Regression/delivery_time.csv")
time
attach(time)
names(time)=c("DT","ST")
time
attach(time)
plot(ST,DT)
cor(ST,DT) #0.8259973
reg<-lm(DT~ST,data = time)
summary(reg) #R^2= 0.6823 try to increase R^2
reg$residuals
reg$fitted.values

#2. Sqrt Transformation
#attach(time)
plot(sqrt(ST),DT)
cor(sqrt(ST),DT) # 0.83415
reg_sqrt_ST<-lm(DT~sqrt(ST),data = time)
summary(reg_sqrt_ST) #R^2 = 0.6958
reg_sqrt_ST$fitted.values

#3. log Transformation
attach(time)
plot(log(ST),DT)
cor(log(ST),DT) # 0.8339325
reg_log_ST<-lm(DT~log(ST),data=time)
summary(reg_log_ST) #R^2= 0.6954
reg_log_ST$fitted.values

#4. Sqrt DT Transformation
plot(ST,sqrt(DT))
cor(ST,sqrt(DT))  # 0.8390768
reg_sqrt_DT<-lm(sqrt(DT)~ST,data = time)
summary(reg_sqrt_DT) #R^2= 0.704
pred_sqrt_DT<-(reg_sqrt_DT$fitted.values)^2
pred_sqrt_DT
err_sqrt_DT<-DT-pred_sqrt_DT
err_sqrt_DT

#5. log DT Transformation
plot(ST,log(DT))
cor(ST,log(DT)) #0.8431773
reg_log_DT<-lm(log(DT)~ST,data = time)
summary(reg_log_DT)  #R^2= 0.7109
pred_log_DT<-exp(reg_log_DT$fitted.values)
pred_log_DT
err_log_DT<-DT-pred_log_DT
err_log_DT

#6. sqrt ST, DT Transformation
#attach(time)
plot(sqrt(ST),sqrt(DT))
cor(sqrt(ST),sqrt(DT))  # 0.8539328
reg_sqrt_ST_DT<-lm(sqrt(DT)~sqrt(ST),data = time)
summary(reg_sqrt_ST_DT) #R^2= 0.7292
pred_sqrt_ST_DT<-(reg_sqrt_ST_DT$fitted.values)^2
pred_sqrt_ST_DT
err_sqrt_ST_DT<-DT-pred_sqrt_ST_DT
err_sqrt_ST_DT

#7. sqrt ST, log DT Transformation
plot(sqrt(ST),log(DT))
cor(sqrt(ST),log(DT)) #0.8647743
reg_sqrt_ST_log_DT<-lm(log(DT)~sqrt(ST))
summary(reg_sqrt_ST_log_DT) #R^2= 0.7478 , Min= -0.26624 , Max= 0.39487
pred_sqrt_ST_log_DT<-exp(reg_sqrt_ST_log_DT$fitted.values)
pred_sqrt_ST_log_DT
err_sqrt_ST_log_DT<-DT-pred_sqrt_ST_log_DT
err_sqrt_ST_log_DT

#RMSE:
# 1.
sqrt(mean(reg$residuals^2))
# 2.
sqrt(mean(reg_sqrt_ST$residuals^2))
# 3.
sqrt(mean(reg_log_ST$residuals^2))
# 4.
sqrt(mean(err_sqrt_DT^2))
# 5.
sqrt(mean(err_log_DT^2))
# 6.
sqrt(mean(err_sqrt_ST_DT^2))
# 7.
sqrt(mean(err_sqrt_ST_log_DT^2))

Model=c("-","sqrt_ST","log_ST","sqrt_DT","log_DT","sqrt_ST_DT","sqrt_ST_log_DT")
r=c(0.8259973, 0.83415,0.8339325,0.8390768,0.8431773, 0.8539328,0.8647743)
R_2=c(0.6823,0.6958,0.6954,0.704,0.7109,0.7292,0.7478)
RMSE=c(2.79165,2.731543,2.733171,2.849487,2.94025,2.762294,2.821041)
df2=data.frame(Model,r,R_2,RMSE)
df2
# we will use 6th model i.e. sqrt(ST) and sqrt(DT) transformation.



#Q3

#1.
emp= read.csv("C:/Users/Shinkar/Desktop/vss 2020/Excelr/Assignments/Simple Linear Regression/emp_data.csv")
emp
names(emp)<-c("SH","CR")
emp
attach(emp)
plot(SH,CR) #strong negative relation
cor(SH,CR) #-0.9117216
reg<-lm(CR~SH)
summary(reg)  #R^2= 0.8312
reg$residuals
reg$fitted.values

#2. SQRT transformation
plot(sqrt(SH),CR) #strong negative relation
cor(sqrt(SH),CR) # -0.9165311
reg_sqrt_SH<-lm(CR~sqrt(SH))
summary(reg_sqrt_SH)   #R^2= 0.84

#3. log transformation
plot(log(SH),CR)
cor(log(SH),CR)   #-0.9212077
reg_log_SH<-lm(CR~log(SH))
summary(reg_log_SH) # R^2 =  0.8486

# SH^2 transformation
plot(SH*SH,CR)
cor(SH*SH,CR) # -0.9017223
reg_SH_sq<-lm(CR~(SH*SH))
summary(reg_SH_sq)  # R^2= 0.8312

#RMSE
# 1.
sqrt(mean(reg$residuals^2)) # 3.997528
# 2.
sqrt(mean(reg_sqrt_SH$residuals^2)) #3.891995
# 3.
sqrt(mean(reg_log_SH$residuals^2))  #3.786004 
# 4.
sqrt(mean(reg_SH_sq$residuals^2))   #3.997528

Model=c("-","sqrt_SH","log_SH","SH^2")
r=c(-0.9117216,-0.9165311,-0.9212077,-0.9017223)
R_2=c(0.8312,0.84,0.8486,0.8312)
RMSE=c( 3.997528,3.891995,3.786004,3.997528)
df3=data.frame(Model,r,R_2,RMSE)
df3
# We will use 3rd model i.e. log(SH) as r and R^2 is high and RMSE is low than other models.


#Q4

# 1.
Salary_data=read.csv("C:/Users/Shinkar/Desktop/vss 2020/Excelr/Assignments/Simple Linear Regression/Salary_Data.csv")
Salary_data
names(Salary_data)=c("Years","Sal")
Salary_data
attach(Salary_data)
plot(Years,Sal) #strong positive relation
cor(Years,Sal) # 0.9782416
reg_S<-lm(Sal~Years)
summary(reg_S)  # R^2= 0.957
reg_S$residuals
reg_S$fitted.values

# 2. Sqrt Transformation
plot(sqrt(Years),Sal) #strong positive relation
cor(Sal,sqrt(Years))  # 0.9648839
reg_sqrt_Yrs<-lm(Sal~sqrt(Years))
summary(reg_sqrt_Yrs) #R^2= 0.931

# 3. log transformation
plot(log(Years),Sal) #strong positive relation
cor(Sal,log(Years)) #0.9240611

# 4. square Transformation 
plot(Years^2,Sal)
cor(Sal,Years^2)    # 0.9567235
reg_sq_yrs<-lm(Sal~(Years^2))
summary(reg_sq_yrs)  #R^2= 0.957

# RMSE
# 1.
sqrt(mean(reg_S$residuals^2)) # 5592.044
# 2.
sqrt(mean(reg_sqrt_Yrs$residuals^2)) # 7080.096
# 4.
sqrt(mean(reg_sq_yrs$residuals^2))   # 5592.044

Model=c('without transformation','sqrt','square')
r=c( 0.9782416,0.9648839,0.9567235)
R_2=c(0.957, 0.931,0.957)
RMSE=c(5592.044,7080.096,5592.044)
df4=data.frame(Model,r,R_2,RMSE)
df4
#We will use Original model without any transformation.