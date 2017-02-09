library(readr)
train=read_csv("C:/Users/aran/Documents/ecs171/train_ver2.csv/train_ver2.csv")
library(dplyr)
use_var=select(train,-pais_residencia,-fecha_alta,-indext,-conyuemp,-canal_entrada,-tipodom,-nomprov,-ult_fec_cli_1t)


#remove NA except renta
temp=select(use_var,-renta)
com_temp=temp[complete.cases(temp),]

#fix income
no_income=use_var[is.na(use_var$renta),1:24]
sort_no=arrange(no_income,ncodpers,fecha_dato)
no_in_index=(1:nrow(sort_no))[!duplicated(sort_no$renta)]
new_sort_no=(1:nrow(sort_no))[!duplicated(sort_no$ncodpers)]



#sort by region
sort_region=arrange(com_temp,cod_prov,ncodpers)
income_group=split(sort_region$renta,sort_region$cod_prov)
income=sapply(income_group,function(x){mean(x,na.rm=TRUE)})
new_region=(1:nrow(sort_region))[!duplicated(sort_region$cod_prov)]
regions=unique(sort_region$cod_prov)

#fix
fix_income=numeric(0)
for (i in regions){
  part_renta=sort_region$renta[!is.na(sort_region$cod_prov)&sort_region$cod_prov==i]
  fix_income=c(fix_income,ifelse(is.na(part_renta),income[i],part_renta))
}

#transform 

#ind_empleado
sort_region$ind_empA=ifelse( sort_region$ind_empleado=="A",1,0)
sort_region$ind_empB=ifelse( sort_region$ind_empleado=="B",1,0)
sort_region$ind_empF=ifelse( sort_region$ind_empleado=="F",1,0)
sort_region$ind_empN=ifelse( sort_region$ind_empleado=="N",1,0)

#sexo
sort_region$sexo=ifelse(sort_region$sexo=="V",1,0)

#ind_nuevo
table(sort_region$ind_nuevo)

#indrel
table(sort_region$indrel)
sort_region$indrel=ifelse(sort_region$indrel==1,1,0)

#indrel_1mes
table(sort_region$indrel_1mes)
sort_region$indrel_1mes1=ifelse(sort_region$indrel_1mes==1|sort_region$indrel_1mes==1.0,1,0)
table(sort_region$indrel_1mes1)
sort_region$indrel_1mes2=ifelse(sort_region$indrel_1mes==2|sort_region$indrel_1mes==2.0,1,0)
table(sort_region$indrel_1mes2)
sort_region$indrel_1mes3=ifelse(sort_region$indrel_1mes==3|sort_region$indrel_1mes==3.0,1,0)
table(sort_region$indrel_1mes3)
sort_region$indrel_1mes4=ifelse(sort_region$indrel_1mes==4|sort_region$indrel_1mes==4.0,1,0)
table(sort_region$indrel_1mes4)

#tiprel_1mes
sort_region$tiprel_1mesA=ifelse(sort_region$tiprel_1mes=="A",1,0)
sort_region$tiprel_1mesP=ifelse(sort_region$tiprel_1mes=="P",1,0)
sort_region$tiprel_1mesI=ifelse(sort_region$tiprel_1mes=="I",1,0)

#indresi
sort_region$indresi=ifelse(sort_region$indresi=="S",1,0)

#indfall
table(sort_region$indfall)
sort_region$indfall=ifelse(sort_region$indfall=="S",1,0)

#ind_actividad_cliente
table(sort_region$ind_actividad_cliente)

#segmento
table(sort_region$segmento)
sort_region$segmento3=ifelse(sort_region$segmento=="03 - UNIVERSITARIO",1,0)
table(sort_region$segmento3)

sort_region$segmento2=ifelse(sort_region$segmento=="02 - PARTICULARES",1,0)
table(sort_region$segmento2)

#independent
train_ind=select(sort_region,-segmento,-tiprel_1mes,-ind_empleado,-sexo,-indrel_1mes,-cod_prov,-ult_fec_cli_1t)
colnames(train_ind)
sort_cus=arrange(train_ind,ncodpers,fecha_dato)

#depedent
temp_train=select(sort_cus,ends_with("ult1"))
head(temp_train)
train_dep=temp_train[2:nrow(temp_train),]-temp_train[1:(nrow(temp_train)-1),]
final_dep=ifelse(train_dep==1,1,0)
head(final_dep)


#new cunsumer index
new_index=(1:nrow(sort_cus))[!duplicated(sort_cus$ncodpers)]

#rm last obs for each consumer
rm_index=new_index-1
rm_index=rm_index[-1]

#final indepedent and dependent
fn_inp=sort_cus[-rm_index,]
fn_inp=fn_inp[-nrow(fn_inp),]
fn_dep=final_dep[-rm_index,]

#svm
train_true=temp_train[sort_cus$fecha_dato=="2016-05-28",]
fn_ind_train=select(fn_inp,-fecha_dato,-ncodpers)


#scale
fn_ind_train$antiguedad=as.numeric(fn_ind_train$antiguedad)
fn_ind_train$age=scale(fn_ind_train$age)
fn_ind_train$antiguedad=scale(fn_ind_train$antiguedad)

library(e1071)
pre_list=c()
for (i in 1:5){
  for(j in 1:5){
    for(k in 1:24){
      y_data=fn_dep[,k]
    model = svm(y = factor(y_data), x = s_train_data, kernel = "radial", gamma =0.001, cost = 0.001, na.action = na.omit)
    pred=predict(model, s_train_data)
    print(mean(pred == y_data))
    pred_list = c(pred_list,pred)
  }
  }
}

#format test
test_orig=read_csv("C:/Users/aran/Documents/ecs171/test_ver2.csv/test_ver2.csv")

#test_jun=read("test_ver2.csv")
#test_jun=read.csv("test_ver2.csv")
dim(may_data)
dim(test_jun)
may_ori=train[train$fecha_dato=="2016-05-28",]
rm(train)
dim(may_ori)


not_in_test=setdiff(may_ori$ncodpers,test_orig$ncodpers)

#renta
income_group=split(test_orig$renta,test_orig$cod_prov)
test_reg_sort=arrange(test_orig,cod_prov)
income_test=sapply(income_group,function(x){mean(as.numeric(x),na.rm=TRUE)})
income_test_fix=rep(income_test,sapply(income_group,length))
test_reg_sort$renta[is.na(test_reg_sort$renta)]=income_test_fix[is.na(test_reg_sort$renta)]#fix by province
test_reg_sort$renta[is.na(test_reg_sort$renta)]=mean(test_reg_sort$renta,na.rm = TRUE)
use_var_test=select(test_reg_sort,-pais_residencia,-fecha_alta,-indext,-conyuemp,-canal_entrada,-tipodom,-nomprov,-ult_fec_cli_1t,-cod_prov)
sapply(use_var_test, function(x){sum(is.na(x))})

#transform 

#ind_empleado
use_var_test$ind_empA=ifelse( use_var_test$ind_empleado=="A",1,0)
use_var_test$ind_empB=ifelse( use_var_test$ind_empleado=="B",1,0)
use_var_test$ind_empF=ifelse( use_var_test$ind_empleado=="F",1,0)
use_var_test$ind_empN=ifelse( use_var_test$ind_empleado=="N",1,0)
#indrel
table(use_var_test$indrel)
use_var_test$indrel=ifelse(use_var_test$indrel==1,1,0)

#indrel_1mes
table(use_var_test$indrel_1mes)
use_var_test$indrel_1mes1=ifelse(use_var_test$indrel_1mes==1|use_var_test$indrel_1mes==1.0,1,0)
table(use_var_test$indrel_1mes1)
use_var_test$indrel_1mes2=ifelse(use_var_test$indrel_1mes==2|use_var_test$indrel_1mes==2.0,1,0)
table(use_var_test$indrel_1mes2)
use_var_test$indrel_1mes3=ifelse(use_var_test$indrel_1mes==3|use_var_test$indrel_1mes==3.0,1,0)
table(use_var_test$indrel_1mes3)
use_var_test$indrel_1mes4=ifelse(use_var_test$indrel_1mes==4|use_var_test$indrel_1mes==4.0,1,0)
table(use_var_test$indrel_1mes4)

#tiprel_1mes
use_var_test$tiprel_1mesA=ifelse(use_var_test$tiprel_1mes=="A",1,0)
use_var_test$tiprel_1mesP=ifelse(use_var_test$tiprel_1mes=="P",1,0)
use_var_test$tiprel_1mesI=ifelse(use_var_test$tiprel_1mes=="I",1,0)

#indresi
use_var_test$indresi=ifelse(use_var_test$indresi=="S",1,0)

#indfall
table(use_var_test$indfall)
use_var_test$indfall=ifelse(use_var_test$indfall=="S",1,0)

#ind_actividad_cliente
table(use_var_test$ind_actividad_cliente)

#segmento
table(use_var_test$segmento)
use_var_test$segmento3=ifelse(use_var_test$segmento=="03 - UNIVERSITARIO",1,0)
table(use_var_test$segmento3)

use_var_test$segmento2=ifelse(use_var_test$segmento=="02 - PARTICULARES",1,0)
table(use_var_test$segmento2)

#independent
test_ind=select(use_var_test,-segmento,-tiprel_1mes,-ind_empleado,-sexo,-indrel_1mes,-fecha_dato)
colnames(test_ind)
test_sort_cus=arrange(test_ind,ncodpers)
sort_may=arrange(may_ori,ncodpers)

#buying history

temp_his=select(sort_may,ends_with("ult1"))
temp_his=temp_his[sort_may$ncodpers%in%test_ind$ncodpers,]
test_sort_cus=cbind(test_sort_cus,temp_his)
colnames(test_sort_cus)
test_sort_cus=test_sort_cus[,-1]
test_sort_cus=test_sort_cus[order(colnames(test_sort_cus))]
test_sort_cus=select(test_sort_cus,-tiprel_1mesA,-tiprel_1mesI,-tiprel_1mesP)
dim(test_sort_cus)

#scale
test_sort_cus$antiguedad=as.numeric(test_sort_cus$antiguedad)
test_sort_cus$age=scale(test_sort_cus$age)
test_sort_cus$antiguedad=scale(test_sort_cus$antiguedad)
test_sort_cus$renta=scale(test_sort_cus$renta)

#fit
results=apply(fn_dep,2,function(x) svm_pred(x,0.01,0.1))
results=apply(fn_dep, 2,function(x)svm_pred(x,0.01,0.001))

#########################################################################################
##### ROC & PRC
##########################################################################################
load("results/171_temp_model.RData")
set.seed(27); idx_3 = sample(1e6, 2e5)
var_3 = c("ind_cco_fin_ult1", "ind_fond_fin_ult1", "ind_deme_fin_ult1")
xx_3 = sam_ind[idx_3[1:1e5], ]
yy_3 = sam_dep[idx_3[1:1e5], var_3]

# library(LiblineaR)
# ptm = proc.time()
# fit_svm_1_lib = LiblineaR(data = xx_3, target = factor(yy_3[[1]]), type = 1)
# proc.time() - ptm

# fit_svm_1 = svm(x = xx_3, y = as.factor(yy_3[[1]]), kernel = "radial", scale = FALSE)
# fit_svm_2 = svm(x = xx_3, y = as.factor(yy_3[[2]]), kernel = "radial", scale = FALSE)
# fit_svm_3 = svm(x = xx_3, y = as.factor(yy_3[[3]]), kernel = "radial", scale = FALSE)

ptm = proc.time()
fit_svm_1 = svm(x = xx_3, y = as.factor(yy_3[[1]]), kernel = "linear", scale = FALSE)
proc.time() - ptm
fit_svm_2 = svm(x = xx_3, y = as.factor(yy_3[[2]]), kernel = "linear", scale = FALSE)
fit_svm_3 = svm(x = xx_3, y = as.factor(yy_3[[3]]), kernel = "linear", scale = FALSE)

### ROC & PRC

library(ROCR)
rocplot = function(pred, truth, ...) {
  predob = prediction(pred, truth, c(0, 1))
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...) 
}

prplot = function(pred, truth, ...) {
  predob = prediction(pred, truth, c(0, 1))
  perf = performance(predob, "prec", "rec")
  plot(perf,...) 
}

# score1 = attributes(predict(fit_svm_1, xx_3, decision.values = TRUE))$decision.values
# score2 = attributes(predict(fit_svm_2, xx_3, decision.values = TRUE))$decision.values
# score3 = attributes(predict(fit_svm_3, xx_3, decision.values = TRUE))$decision.values

set.seed(28); idx_3_tst = idx_3[-(1:1e5)]
xx_3_tst = sam_ind[idx_3_tst, ]
yy_3_tst = sam_dep[idx_3_tst, var_3]

pred1_tst = predict(fit_svm_1, xx_3_tst)
score1_tst = attributes(predict(fit_svm_1, xx_3_tst, decision.values = TRUE))$decision.values
pred2_tst = predict(fit_svm_2, xx_3_tst)
score2_tst = attributes(predict(fit_svm_2, xx_3_tst, decision.values = TRUE))$decision.values
pred3_tst = predict(fit_svm_3, xx_3_tst)
score3_tst = attributes(predict(fit_svm_3, xx_3_tst, decision.values = TRUE))$decision.values

pr_tbl = function(pred, label) {
  pred = factor(pred, levels = rev(levels(pred)))
  label = factor(label, levels = rev(levels(label)))
  tbl = table(predicted = pred, truth = label)
  accuracy = mean(pred == label)
  precision = tbl[1, 1]/sum(tbl[1, ])
  recall = tbl[1, 1]/sum(tbl[, 1])
  out = list(tbl, accuracy, precision, recall)
  names(out) = c("confusion_matrix", "accuracy", "precision", "recall")
  return(out)
  
}

out1 = pr_tbl(pred1_tst, as.factor(yy_3_tst[[1]]))
out2 = pr_tbl(pred2_tst, as.factor(yy_3_tst[[2]]))
out3 = pr_tbl(pred3_tst, as.factor(yy_3_tst[[3]]))


# rocplot(score1, as.factor(yy_3[[1]]), main = "ROC of ind_cco_fin_ult1 for training data", 
#         col = 2)
# rocplot(score2, as.factor(yy_3[[2]]), main = "ROC of ind_fond_fin_ult1 for training data", 
#         col = 3)
# rocplot(score3, as.factor(yy_3[[3]]), main = "ROC of ind_deme_fin_ult1 for training data", 
#         col = 4)

png("./results/rocprc2.png", width = 900, height = 900)
par(mfrow = c(3, 3))
barplot(table(yy_3_tst[[1]])/1e5, main = "ind_cco_fin_ult1 distribution")
barplot(table(yy_3_tst[[2]])/1e5, main = "ind_fond_fin_ult1 distribution")
barplot(table(yy_3_tst[[3]])/1e5, main = "ind_deme_fin_ult1 distribution")

rocplot(score1_tst, as.factor(yy_3_tst[[1]]), 
        main = "ROC of ind_cco_fin_ult1 for testing data", col = 2)
rocplot(score2_tst, as.factor(yy_3_tst[[2]]), 
        main = "ROC of ind_fond_fin_ult1 for testing data", col = 3)
rocplot(score3_tst, as.factor(yy_3_tst[[3]]), 
        main = "ROC of ind_deme_fin_ult1 for testing data", col = 4)

prplot(score1_tst, as.factor(yy_3_tst[[1]]), 
       main = "PRC of ind_cco_fin_ult1 for testing data", col = 2)
prplot(score2_tst, as.factor(yy_3_tst[[2]]), 
       main = "PRC of ind_fond_fin_ult1 for testing data", col = 3)
prplot(score3_tst, as.factor(yy_3_tst[[3]]), 
       main = "PRC of ind_deme_fin_ult1 for testing data", col = 4)

dev.off()


wts = 100 / table(yy_3[[3]]) 
ptm = proc.time()
fit_svm_1_wt = svm(x = xx_3, y = as.factor(yy_3[[1]]), kernel = "linear", scale = FALSE,
                   class.weights = wts)
proc.time() - ptm

pred1_tst_wt = predict(fit_svm_1_wt, xx_3_tst)
score1_tst_wt = attributes(predict(fit_svm_1_wt, xx_3_tst, decision.values = TRUE))$decision.values

out1_wt = pr_tbl(pred1_tst_wt, as.factor(yy_3_tst[[1]])) 

png("./results/rocprc3.png", width = 800, height = 800)
par(mfrow = c(1, 1))
rocplot(score1_tst, as.factor(yy_3_tst[[1]]), 
        main = "ROC comparison for adding weights and no weights", col = 2)
rocplot(score1_tst_wt, as.factor(yy_3_tst[[1]]), add = TRUE, col = 3)
legend("topleft", lty = rep(1, 2), col = c(2, 3), 
       legend = c("without weights", "with weigths"))
dev.off()