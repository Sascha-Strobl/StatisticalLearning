#Problem 1 Taylor Series Expansion (Approximate Euler's number)
fix(Taylor_expansion);
function(n,x)
{a=1;for (i in 1:n){a=a+x^i/factorial(i)};a};
Taylor_expansion(100,1)
exp(1)
fix(Taylor_seq)
function (n) 
{c=rep(0,n); for(i in 1:n){c[i]=Taylor_expansion(i,1)};
c
}
Taylor_seq(100)
plot(1:100,Taylor_seq(100))

#Problem 2 (2 Dice have to equal 6)
n=1000;
sum=0;
for (i in 1:n){
  x1=sample(1:6,1,replace=T);
  x2=sample(1:6,1,replace=T);
  x=x1+x2;
  if (x==6){a=1} else{a=0}; sum=sum+a;
  result[i]=sum/i;}
result;
plot(1:1000, result)

#Problem 3 (Median, mean and is the variance of the mean or meadian lower)
#a)
n=5
rnorm(n)
#b)
fix(sequence.c);
sequence.c(1000);
plot(1:1000,sequence.c(1000));
#c)
n=1000
b=0
for (i in 1:n){
  a=median(rnorm(100));
  b=b+a};
mean1=b/n;
mean1
#d)
n=1000
for (i in 1:n){
  a[i]=median(rnorm(100));
  v=var(a);};
v

n=1000
for (i in 1:n){
  a[i]=mean(rnorm(100));
  v2=var(a)};
v2 # Variance of the mean is lower than the variance of the median

#Problem 4 Linear Regression and prediction
cashiers=c(3, 4, 5, 6, 8, 10, 12);
wtime=c(16, 12, 9.6, 7.9, 6, 4.7, 4);
lm.fit=lm(wtime~cashiers);
summary(lm.fit);
confint(lm.fit);
newdata = data.frame(cashiers=15)
predict(lm.fit, newdata, se.fit = TRUE)
predict(lm.fit, newdata, interval = "prediction")

# Problem 5a) Predict new data with QDA and KNN
# Discriminant analysis
library(MASS);
Gender=c(0,1, 1, 1, 0, 0); # 0 = male; 1 = female
lweight=c(5.00, 4.70, 4.40, 5.12, 4.30, 5.44);
qda.fit=qda(Gender~lweight);
newdata = data.frame(lweight=4.9)
qda.pred=predict(qda.fit, newdata);
qda.pred;

#Problem 5b)
# K-nearest neighbors
library(class);
train.X=lweight;
test.X=c(4.9);
train.Direction=Gender;
set.seed(1);
knn.pred=knn(train.X,test.X,train.Direction,k=4);
knn.pred

