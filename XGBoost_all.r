#Xgboost Subteam
#This a R code

#Cleandata
#Drop some variables
name = names(D)
D=D[,-c(5,7,10,11,12,15,18,19,20,21)]

#covert to numeric data
D$ind_empleado=as.numeric(D$ind_empleado)
#> table(D$ind_empleado,exclude = NULL)
#2        3        4        5        6     <NA> 
#  2492     3566     2523 13610977       17        0 

D$pais_residencia=as.numeric(D$pais_residencia)
#From 2 to 119, No NA

D$age=as.numeric(as.character(D$age))
#No NA
#quantile(D$age,0.99)
#99% 
#88
#Delete age>88
D=D[D$age<=88,]

#table(D$ind_nuevo)
#0        1 
#12808368   811207

D$antiguedad=as.numeric(as.character(D$antiguedad))
#median(D$antiguedad) is 50
#Change antiguated = -999999 to 50
D$antiguedad[D$antiguedad==-999999]=50

D$tiprel_1mes = as.numeric(D$tiprel_1mes)
#            A       I       N       P       R 
#  121913 6133834 7225855       4    4637     866 
#convert to 1-6

D$indresi=as.numeric(D$indresi)
#N        S    
# 65555 13421554
#N is 2,S is 3


#D$conyuemp, most of people is NA, remove this variable
D = D[,-10]

D$canal_entrada=as.numeric(D$canal_entrada)

#D$ind_actividad_cliente
#0       1    
#7298639 6188470

#Fix NA of renta by mean
D$renta[is.na(D$renta)]=mean(D$renta,na.rm = TRUE)

D$segmento = as.numeric(D$segmento)
#Null          01 - TOP  02 - PARTICULARES 03 - UNIVERSITARIO
#159192             553912            7838426            4935579

D$ind_nomina_ult1[is.na(D$ind_nomina_ult1)]=0
D$ind_nom_pens_ult1[is.na(D$ind_nom_pens_ult1)]=0

save(D,file='clean_data_pre_train.rda')
write.csv(D, file = "clean_data_pre_train.csv", row.names = FALSE)

#Creat the training data
D = D[,c(-1,-2)]
save(D,file='clean_data_train.rda')
write.csv(D, file = "clean_data_train.csv", row.names = FALSE)

library(dplyr)
D = D %>% 
  arrange(ncodpers,fecha_dato)

D$ind_nomina_ult1[is.na(D$ind_nomina_ult1)]=0
D$ind_nom_pens_ult1[is.na(D$ind_nom_pens_ult1)]=0

product = D[,14:37]
product_new1 = product[2:nrow(product),1:12]-product[1:(nrow(product)-1),1:12]
product_new2 = product[2:nrow(product),13:24]-product[1:(nrow(product)-1),13:24]
product_new = cbind(product_new1,product_new2)
rm(product_new1,product_new2,product)
#product_new = rbind(product[1,],product_new)
product_new = rbind(product_new,rep(0,24))
#fix na of the last 2 and 3 product
#product_new$ind_nom_pens_ult1[is.na(product_new$ind_nom_pens_ult1)]=0
#product_new$ind_nomina_ult1[is.na(product_new$ind_nomina_ult1)]=0

#Covert <0 to 0
for (i in 1:24){
  product_new[,i][product_new[,i]<0]=0
}

#Substitude the last row of each one with 0 vector
index=duplicated(D$ncodpers)
index_last = index[-1]
index_last = c(index_last,FALSE)
rm(D,index)
#product_new[!index_last,]=matrix(0, 941043, 24)
#product_new1 = product_new[1:6000000,]
#product_new2 = product_new[6000001:nrow(product_new),]
#index_last1 = index_last[1:6000000]
#product_new1[!index_last1,]=matrix(0,387803,24)
#table(index_last[1:600000])
#product_new[!index,1:12]=first_month[,1:12]
#product_new[!index,13:24]=first_month[,13:24]

#Convert to 1 vector
#response1 = max.col(product_new[1:5000000,],ties.method="random")
#response = max.col(product_new[5000000,],ties.method="random")

#Select the rows have change
sum1 = rowSums(product_new[1:4000000,],na.rm = TRUE)
sum2 = rowSums(product_new[4000001:8000000,],na.rm = TRUE)
sum3 = rowSums(product_new[8000001:12000000,],na.rm = TRUE)
sum4 = rowSums(product_new[12000001:nrow(product_new),],na.rm = TRUE)
sum = c(sum1,sum2,sum3,sum4)
rm(sum1,sum2,sum3,sum4)
index_1 = sum>0
response_1 = max.col(product_new[index_1,],ties.method="random")
response = rep(0,times=nrow(product_new))
response[index_1]=response_1

#Substitude the last month with 0
response[!index_last]=0

save(response,file = 'response.rda')
write.csv(response,file = 'response.csv',row.names = FALSE)

save(product_new,file = 'product_new.rda')
write.csv(product_new,file = 'product_new.csv',row.names = FALSE)

#Simplify the training data set
setwd('E:/Study/ECS171/Project')
load('clean_data_train.rda')
load('response.rda')

#For pais_residencia, if Spain 1, not Spain 0
train$pais_residencia=train$pais_residencia==38

#For age, cut into 5 group
#quantile(train$age,c(0.2,0.4,0.6,0.8))
#20% 40% 60% 80% 
# 23  31  43  53 
train$age[train$age<=23]=0
train$age[train$age>23 & train$age<=31]=1
train$age[train$age>31 & train$age<=43]=2
train$age[train$age>43 & train$age<=53]=3
train$age[train$age>53]=4

#For renta(customer seniority)
#quantile(train$renta,c(0.2,0.4,0.6,0.8))
#20% 40% 60% 80% 
#18  38  86 152
train$antiguedad[train$antiguedad<=18]=0
train$antiguedad[train$antiguedad>18 & train$antiguedad<=38]=1
train$antiguedad[train$antiguedad>38 & train$antiguedad<=86]=2
train$antiguedad[train$antiguedad>86 & train$antiguedad<=152]=3
train$antiguedad[train$antiguedad>152]=4

#For indresi, drop, same as the "pais_residencia" info now
train = train[,-7]

#For canal_entrada, delete temporally, too complex and may useless
train = train[,-7]

#For tiprel_1mes, drop, because it is silimar to ind_actividad_cliente
train = train[,-6]

#For renta
#quantile(train$renta,c(0.2,0.4,0.6,0.8))
#20%       40%       60%       80% 
#68766.63 102064.47 133876.54 155275.86
train$renta[train$renta<=68766]=0
train$renta[train$renta>68766 & train$renta<=102064]=1
train$renta[train$renta>102064 & train$renta<=133876]=2
train$renta[train$renta>133876 & train$renta<=155275]=3
train$renta[train$renta>155275]=4

#Combine response and train
train_res = cbind(train,response)
save(train_res,file='simple_train_response.rda')

#Make things more simple,only retain the response not 0 rows
train_res_no0 = train_res[train_res$response>0,]
train_res_no0$pais_residencia = as.numeric(train_res_no0$pais_residencia)
save(train_res_no0,file='simple_train_response_no0.rda')
write.csv(train_res_no0,file='simple_train_response_no0.csv',row.names = FALSE)

#Split the original training set into 2 parts: training set(70%) and testing data(30%)
n = nrow(train_res_no0)
n1 = round(n*0.7)

index = sample(1:n,n,replace = FALSE)
train_res_no0 = train_res_no0[index,]
train_point7 = train_res_no0[1:n1,]
test_point3 = train_res_no0[(n1+1):n,]

save(train_point7,file='train_point7.rda')
save(test_point3,file='test_point3.rda')

##run XGBoost algorithm

install.packages("drat", repos = "https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("xgboost", repos="http://dmlc.ml/drat/", type ="source" )
require(xgboost)

set.seed(1)
#data(agaricus.train, package = "xgboost")
#ata(agaricus.test, package = "xgboost")
load('simple_train_response_no0.rda')
#train = agaricus.train
#test = agaricus.test

param = list('objective' = 'multi:softmax',
             'eval_metric' = 'mlogloss',
             'eta' = 0.5, 'max.depth' = 6,
             'num_class'=24)
#bst.cv =xgb.cv(params = param, data = as.matrix(train$data), label = train$label,
#               nfold=10,nrounds = 80)
plot(log(bst.cv$test.logloss.mean),type = 'l')

bst = xgboost(params = param, data = as.matrix(train_point7[,1:32]), 
              label = train_point7[,dim(train_point7)[2]]-1, 
              nrounds = 50)
preds = predict(bst,as.matrix(train_point7[,1:32]))
table((preds+1)==train_point7$response)
head(preds)

names = dimnames(train_res_no0[, 1:8])[[2]]
trees = xgb.model.dt.tree(names, model = bst)
importance_matrix = xgb.importance(names, model = bst)
library(Ckmeans.1d.dp)
xgb.plot.importance(importance_matrix)
library(DiagrammeR)
xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 4)

#Accuracy
param1 = list('objective' = 'multi:softmax',
              'eval_metric' = 'mlogloss',
              'eta' = 0.5, 'max.depth' = 10,
              'num_class'=24)
bst1 = xgboost(params = param, data = as.matrix(train_res_no0[,1:8]), 
               label = train_res_no0[,dim(train_res_no0)[2]]-1, 
               nrounds = 20, nthread = 2)
preds1 = predict(bst1,as.matrix(train_res_no0[,1:8]))
table((preds1+1)==train_res_no0$response)
