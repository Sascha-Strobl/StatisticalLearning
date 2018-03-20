setwd("~/Documents/CGUClasses/Statistical Learning/Lab2");
data = read.table("odor.txt", header = T);
odor_data<-data.matrix(data);

# Problem 1:
#a)
# Generate a function to return LOOCV prediction MSE, with inputs X (matrix) and Y (column vector).
# Create function name. Open an editing board to generete my function.
fix(CV);
# Edit in the attached window.
function (X,Y) 
{n = length(Y); u=numeric(length(Y));S=X%*%solve(t(X)%*%X)%*%t(X);for(i in 1:n){u[i]=S[i,i]};mean(((Y-S%*%Y)/(1-u))^2)
}

#b)
# Calculate the LOOCV prediction MSE for the 2 models. Prepare inputs.
model1=lm(Odor~poly(Temp,2)+poly(Height,2)+poly(Ratio,2),data=data);
summary(model1)
model2=lm(Odor~Height+poly(Ratio,2)+I(Temp^2),data=data);
summary(model2)

X=cbind(rep(1,15), odor_data[,2:4], odor_data[,2]^2,odor_data[,3]^2,odor_data[,4]^2)
Y=data$Odor
CV(X,Y)
Z=cbind(rep(1,15),odor_data[,3:4],odor_data[,2]^2,odor_data[,3]^2)
CV(Z,Y)
# The LOOCV prediction MSE for Model 1:
CV(X_model_1,Y)
# result: [1] 747.2333

# The LOOCV prediction MSE for Model 2:
CV(X_model_2,Y)
# result: [1] 666.8952


# Conlusion: The LOOCV prediction MSE is in favor of Model 2.

# Problem 2:
library(MASS)
#a) 
# Use help to check all data variables in "Boston".
help(Boston)
#tax: full-value property-tax rate per \$10,000.
#b)
# Calculate the median of "tax".
median(Boston$tax)
#c)
# Use bootstrap sampling method to estimate the standard deviation of the estimate of Boston tax median.
# Generate a function, in terms of bootstrap sample size B.
set.seed(1)
fix(Se);
Se(Boston$tax,1000)
#d)
upperbound=median(Boston$tax)+Se(Boston$tax,1000)
lowerbound=median(Boston$tax)-Se(Boston$tax,1000)
upperbound
lowerbound


# Problem 3:
# Apply Poisson regression.
crab = read.table("crab.txt", header = F);
colnames(crab)=c("Obs","C","S","W","Wt","Sa");
Expectation=glm(Sa~W+Wt, data=crab, family=poisson(link=log));
summary(Expectation);
# Result:
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept) -1.29168    0.89929  -1.436  0.15091   
#Crab$W       0.04590    0.04677   0.981  0.32640   
#Crab$Wt      0.44744    0.15864   2.820  0.00479

# Conclusion: from the p-values, only Crab$Wt significantly explains the value of the response Crab$Sa.

# Problem 4:
# Produce Bass curve as well as the estmates of all parameters, 
# using nonlinear least squares. Attention: here M,P,Q are 
# initial inputs of m,p,q. P=0.5, Q=0.65 are initial values 
# (the initial values should be well-chosen. we have made many tries to reduce the p-value).

library(minpack.lm)
ts = read.table("ToyotaSales.txt", header = T);
Camry=ts$CamrySales
Cruiser=ts$FJCruiserSales;
Cruiser=Cruiser[-c(1,2,3,4)];
Cruiser<-as.numeric(paste(Cruiser))
#a)
T02=1:15;
Tdelt =(1:100) / 10;
Bass1.nls= nlsLM(Camry ~ M *(((P+Q)^2/P)*exp(-(P+Q)*T02))/(1+(Q/P)*exp(-(P+Q)*T02))^2,start = list(M=sum(Camry)*1000,P=0.5, Q=0.65));
summary(Bass1.nls);
Bcoef=coef(Bass1.nls);
Cusales=cumsum(Camry);
m=Bcoef[1];
p=Bcoef[2];
q=Bcoef[3];
ngete = exp(-(p+q) * Tdelt);
Bpdf=m * ( (p+q)^2 / p ) * ngete / (1 + (q/p) * ngete)^2;
plot(Tdelt, Bpdf, xlab = "Year from 2002",ylab = "Sales per
     year", type='l');
points(T02, Camry);
Bcdf= m * (1 - ngete)/(1 + (q/p)*ngete);
plot(Tdelt, Bcdf, xlab = "Year from 2002",ylab = "Cumulative
     sales", type='l');
points(T02, Cusales);
#b)
T=1:11;
set.seed(1)
Bass2.nls= nls(Cruiser ~ M *(((P+Q)^2/P)*exp(-(P+Q)*T))/(1+(Q/P)*exp(-(P+Q)*T))^2,start = list(M=sum(Cruiser),P=0.5, Q=0.65));
summary(Bass2.nls);
Bcoef=coef(Bass2.nls);
Cusales=cumsum(Cruiser);
Tdelt =(1:100) / 10;
m=Bcoef[1];
p=Bcoef[2];
q=Bcoef[3];
ngete = exp(-(p+q) * Tdelt);
Bpdf=m * ( (p+q)^2 / p ) * ngete / (1 + (q/p) * ngete)^2;
plot(Tdelt, Bpdf, xlab = "Year from 2006",ylab = "Sales per
     year", type='l');
points(T, Cruiser);
Bcdf= m * (1 - ngete)/(1 + (q/p)*ngete);
plot(Tdelt, Bcdf, xlab = "Year from 2006",ylab = "Cumulative
     sales", type='l');
points(T, Cusales);
