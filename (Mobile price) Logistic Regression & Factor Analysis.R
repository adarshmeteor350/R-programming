library(corrplot)
library(DescTools)
mobile_price = read.csv(file.choose(), header = TRUE)
dim(mobile_price)
str(mobile_price)

colSums(is.na(mobile_price))
new_data = na.omit(mobile_price)
c = cor(mobile_price)
corrplot(c,method="circle")

library(psych)
# Kmo value is high inf=dicates the factor anlysis is good
KMO(mobile_price)

cortest.bartlett(mobile_price)
v=eigen(c)
plot(v$values,type="b")

#model
f=factanal(mobile_price,5,rotation = "varimax")
f

#Diagramatic representation of factors
loads<-f$loadings
fa.diagram(loads)

#Factor score
out<-factanal(x=new_data,factors = 5,scores = "regression")
scores = out$scores
dim(scores)
data_frame = cbind(mobile_price,scores)
dim(data_frame)
View(data_frame)
boxplot(data_frame)

#Logistic Regression for the Model
model=glm(data_frame$price_range~data_frame$Factor1+data_frame$Factor2+
            data_frame$Factor3+data_frame$Factor4+data_frame$Factor5,
          family = "gaussian", data = data_frame)
summary(model)

Accuracydp=predict(model,type="response")
p = ifelse(Accuracydp > 2,2,1)
m = table(data_frame$price_range,p)
Accuracy=sum(diag(m)/sum(m))
print(Accuracy)
