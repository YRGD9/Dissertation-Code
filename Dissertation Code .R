
## YRGD9 Dissertation: Impact of Western Mediator on the Effectiveness of Peace Agreements 

## --------- PRELEMINARIES ---------##

#Libraries
library(readxl)
library(stargazer)
library(pROC)
library(ResourceSelection)
library(car)

# Data:
peace <- read_excel("PAX - data - edited.xlsx")
View(peace)

# Main Variables

dep_var_ended <- peace$ended
indep_vars_tp <- peace[,c("tp", "local_tp", "west_tp", "int_tp")]

# Summary of Peace Agreements Variables: 
table(peace$tp)
# 0 = 399 
# 1 = 543 
table(peace$local_tp)
# 0 = 465 
# 1 = 477 
table(peace$west_tp)
# 0 = 828 
# 1 = 144
table(peace$int_tp)
# 0 = 746 
# 1 = 196
table(peace$ended)
# 0 = 715
# 1 = 227


## ---------------------- MODEL 1: BASIC MODEL ---------------------- ##

#Variables:
dep_var_ended <- peace$ended
indep_vars_tp <- peace[,c("local_tp", "west_tp", "int_tp")]

#Model:
model_basic <- glm(dep_var_ended ~ local_tp + west_tp + int_tp, data = peace, family = binomial)
summary(model_basic)

stargazer(model_basic, type = "text")

# local_tp:     0.190      (0.161)          
# west_tp:     -0.555**    (0.264)          
# int_tp:       0.396**    (0.193)          
# Constant:     1.273***   (0.114)
# Observations                942            
# Log Likelihood            -515.309            
# Akaike Inf. Crit.         1,038.617  


## MODEL FIT AND ROBUST CHECK:

# 1. Goodness of fit tests (Pearson Chi Squared Test)

observed <- table(peace$ended)
predicted <- table(predict(model_basic, type = "response") > 0.5)
observed_predicted <- cbind(observed, predicted)

# Perform the chi-squared test
chisq.test(observed_predicted)    # p-value < 2.2e-16 
# difference between the observed and predicted values is statistically significant and the model is a good fit for the data


# 1.2 Goodness of fit tests (Hosmer and Lemeshow Test)

predicted_probs <- predict(model_basic, type = "response")
observed_outcomes <- peace$ended
hoslem.test(observed_outcomes, predicted_probs) # p-value = 0.994 -- Good fit as over 0.05 signficant rate


# 2. Residual Analysis 

model_basic <- glm(ended ~ local_tp + west_tp + int_tp, data = peace, family = binomial)
roc_basic <- roc(peace$ended, predict(model_basic, type = "response"))
plot(roc_basic, main = "ROC Curve - Basic Model", print.auc = TRUE, legacy.axes = TRUE)     

# 0.559 -->  model has only slightly better than chance level predictive ability

#3. Odds Ratio Test: 

coef_1 <- summary(model_basic)$coef[,1]
se_1 <- summary(model_basic)$coef[,2]

OR_1 <- exp(coef_1)
CI_1 <- exp(cbind(coef_1 - 1.96*se_1, coef_1 + 1.96*se_1))

cbind(OR_1, CI_1)

# west_tp:  0.5741242 0.3420922 0.9635373
#The 95% CI excludes 1, indicating that the effect of west_tp is statistically significant.

#4. Variation Inflation Factor

vif(model_basic)
# local_tp: (1.099203)
# west_tp: (1.067610)
# int_tp: (1.142473)

#5. Linear Model: 
dep_var_ended_lm <- peace$ended
indep_vars_tp_lm <- peace[,c("local_tp", "west_tp", "int_tp")]

model_basic_lm <- glm(dep_var_ended_lm ~ local_tp + west_tp + int_tp, data = peace, family = binomial)
summary(model_basic_lm)



## ---------------------- MODEL 2: TIME FIXED EFFECT ---------------------- ##

#Variables:
peace_year <- factor(peace$Dat_year)
View(peace_year)

#Model: 
model_year <- glm(dep_var_ended ~ local_tp + west_tp + int_tp + peace_year, data = peace, family = binomial)
summary(model_year)

stargazer(model_year, type = "text")

# local_tp                     0.268       (0.205)          
# west_tp                     -1.361***    (0.313)          
# int_tp                       0.326       (0.238) 
# Constant                     1.721***    (0.651) 
# Observations                  942            
# Log Likelihood             -367.344          
# Akaike Inf. Crit.           806.688  


## MODEL FIT AND ROBUST CHECK:

# 1. Goodness of fit tests (Pearson Chi Squared Test)

observed_year <- table(peace$ended)
predicted_year <- table(predict(model_year, type = "response") > 0.5)
observed_predicted_year <- cbind(observed_year, predicted_year)

# Perform the chi-squared test
chisq.test(observed_predicted_year)    # p-value = 0.000428 --> less than 0.05
# difference between the observed and predicted values is statistically significant and the model is a good fit for the data


# 1.2 Goodness of fit tests (Hosmer and Lemeshow Test)

predicted_probs_year <- predict(model_year, type = "response")
observed_outcomes_year <- peace$ended
hoslem.test(observed_outcomes_year, predicted_probs_year) # p-value = 0.4792 -- Good fit as over 0.05 signficant rate


# 2. Residual Analysis 

roc_basic_year <- roc(peace$ended, predict(model_year, type = "response"))
plot(roc_basic, main = "ROC Curve - Model 2", print.auc = TRUE, legacy.axes = TRUE)     

# 0.559 -->  model has only slightly better than chance level predictive ability

#3. Odds Ratio Test: 

coef_1_year <- summary(model_year)$coef[,1]
se_1_year <- summary(model_year)$coef[,2]

OR_1_year <- exp(coef_1_year)
CI_1_year <- exp(cbind(coef_1_year - 1.96*se_1_year, coef_1_year + 1.96*se_1_year))

cbind(OR_1_year, CI_1_year)

# west_tp:  95% confidence interval: 0.47329495
#The 95% CI excludes 1, indicating that the effect of west_tp is statistically significant.

#4. Variation Inflation Factor

vif(model_year)
# local_tp: (1.229253)
# west_tp: (1.197643)
# int_tp: (1.207090)

#5. Linear Model: 
peace_year_lm <- factor(peace$Dat_year)

model_year_lm <- lm(dep_var_ended ~ local_tp + west_tp + int_tp + peace_year, data = peace, family = binomial)
summary(model_year_lm)

stargazer(model_year_lm, type = "text")


 
## ---------------------- MODEL 3: TYPES ---------------------- ##.   

#Variables:
dep_var_ended_types <- peace$ended
indep_vars_tp_types <- peace[,c("local_tp", "west_tp", "int_tp", "agtp_tertra", "Stage_comp", "Stage_ren", "agtp_inter")] 

#Model:
model_types <- glm(dep_var_ended_types ~ local_tp + west_tp + int_tp + agtp_inter + Stage_comp + Stage_ren, data = peace, family = binomial)
summary(model_types)

stargazer(model_types, type = "text")

# local_tp:     0.257     (0.164)          
# west_tp:     -0.617**   (0.268)          
# int_tp:       0.366*    (0.195)   
# Constant:    -1.374***  (0.121)
# Observations              942            
# Log Likelihood           -509.241                       
# Akaike Inf. Crit.       1,032.481  


## MODEL FIT AND ROBUST CHECK:

# 1. Goodness of fit tests (Pearson Chi Squared Test)

observed_types <- table(peace$ended)
predicted_types <- table(predict(model_types, type = "response") > 0.5)
observed_predicted_types <- cbind(observed_types, predicted_types)

# Perform the chi-squared test
chisq.test(observed_predicted_types)    # p-value < 2.2e-16 --> p-value is less than 0.05
# difference between the observed and predicted values is statistically significant and the model is a good fit for the data


# 1.2 Goodness of fit tests (Hosmer and Lemeshow Test)

predicted_probs_types <- predict(model_types, type = "response")
observed_outcomes_types <- peace$ended
hoslem.test(observed_outcomes_types, predicted_probs_types) # p-value = 0.7979 -- Good fit as over 0.05 signficant rate


# 2. Residual Analysis 

roc_basic_types <- roc(peace$ended, predict(model_types, type = "response"))
plot(roc_basic_types, main = "ROC Curve - Model 3", print.auc = TRUE, legacy.axes = TRUE)     

# 0.594 -->  model does not have a better than chance level predictive ability

#3. Odds Ratio Test: 

coef_1_types <- summary(model_types)$coef[,1]
se_1_types <- summary(model_types)$coef[,2]

OR_1_types <- exp(coef_1_types)
CI_1_types <- exp(cbind(coef_1_types - 1.96*se_1_types, coef_1_types + 1.96*se_1_types))

cbind(OR_1_types, CI_1_types)

# west_tp:  0.5392942 0.3187055 0.9125612
# 95% CI excludes 1, indicating that the effect of west_tp is statistically significant.

#4. Variation Inflation Factor

vif(model_types)
# local_tp: (1.121687)
# west_tp: (1.079700)
# int_tp: (1.161678)

#5. Linear Model: 
dep_var_ended_types_lm <- peace$ended
indep_vars_tp_types_lm <- peace[,c("local_tp", "west_tp", "int_tp", "Contp_terr", "Status_mult", "Status_uni", "agtp_tertra", "Stage_pre", "Stage_comp", "Status_unclear", "Status_sub", "Stage_ren", "agtp_inter")] 

model_types_lm <- lm(dep_var_ended_types_lm ~ local_tp + west_tp + int_tp + Contp_terr + Status_mult + Status_uni + agtp_inter + Stage_comp + Stage_ren, data = peace, family = binomial)
summary(model_types_lm)

stargazer(model_types_lm, type = "text")


## ---------------------- MODEL 4: REFERANCES ---------------------- ## 

#Variables:
dep_var_ended_ref <- peace$ended
indep_vars_tp_ref <- peace[,c("local_tp", "west_tp", "int_tp", "GDis", "GMig", "GRa", "GRe", "GOth", "GSoc", "GeMe", "GeFa", "ConRen", "Cons", "PolPar", "Civso", "Tral", "Pubad", "Polps", "Terps", "Eps", "Mps", "HrGen", "EqGen", "Prot", "HrFra", "HrCp", "HrSec", "HrNi", "HrIi", "HrDet", "JusCr", "JusEm", "JusJu", "JusPri", "JusTra", "IntFu", "Ban", "LaEn", "SsrGua", "SsrPol", "SsrArm", "SsrDdr", "SsrInt", "SsrPsf", "SsrFf", "Cor", "SsrCrOcr", "SsrDrugs", "Terr", "TjGen", "TjAm", "TjCou", "TjPrire", "TjVic", "TjMis", "TjRep", "TjNR")]

#Model: 
model_ref <- glm(dep_var_ended_ref ~ local_tp + west_tp + int_tp + LaEn:IntFu:Ban + GDis + GMig + GRa + GRe + GOth + GSoc + GeMe + GeFa + ConRen:Cons:PolPar:Civso:Tral:Pubad:Polps:Terps:Eps:Mps + HrGen:EqGen:Prot:HrFra:HrCp:HrSec:HrNi:HrIi:HrDet + JusCr:JusEm:JusJu:JusPri:JusTra + SsrGua:SsrPol:SsrArm:SsrDdr:SsrInt:SsrPsf:SsrFf:Cor:SsrCrOcr:SsrDrugs + Terr:TjGen:TjAm:TjCou:TjPrire:TjVic:TjMis:TjRep:TjNR, data = peace, family = binomial)
summary(model_ref) 

stargazer(model_ref, type = "text")

# local_tp                     0.178       (0.163)          
# west_tp                     -0.504*      (0.274)          
# int_tp                       0.388**     (0.196) 
# Constant                    -1.274***    (0.124) 
# Observations                  942            
# Log Likelihood               -506.466         
# Akaike Inf. Crit.           1,048.932    


## MODEL FIT AND ROBUST CHECK:

# 1. Goodness of fit tests (Pearson Chi Squared Test)

observed_ref <- table(peace$ended)
predicted_ref <- table(predict(model_ref, type = "response") > 0.5)
observed_predicted_ref <- cbind(observed_ref, predicted_ref)

# Perform the chi-squared test
chisq.test(observed_predicted_ref)    # p-value = 2.2e-16 --> less than 0.05
# difference between the observed and predicted values is statistically significant and the model is a good fit for the data


# 1.2 Goodness of fit tests (Hosmer and Lemeshow Test)

predicted_probs_ref <- predict(model_ref, type = "response")
observed_outcomes_ref <- peace$ended
hoslem.test(observed_outcomes_ref, predicted_probs_ref) # p-value = 0.8777 -- Good fit as over 0.05 significant rate


# 2. Residual Analysis 

roc_basic_ref <- roc(peace$ended, predict(model_ref, type = "response"))
plot(roc_basic_ref, main = "ROC Curve - Model 4", print.auc = TRUE, legacy.axes = TRUE)     

# 0.590 -->  model has only slightly better than chance level predictive ability

#3. Odds Ratio Test:  !!!

coef_1_ref <- summary(model_ref)$coef[,1]
se_1_ref <- summary(model_ref)$coef[,2]

OR_1_ref <- exp(coef_1_ref)
CI_1_ref <- exp(cbind(coef_1_ref - 1.96*se_1_ref, coef_1_ref + 1.96*se_1_ref))

cbind(OR_1_ref, CI_1_ref)

# west_tp:  95% confidence interval:  0.6074988 3.578544e-01 1.031299e+00
#The 95% CI excludes 1, indicating that the effect of west_tp is statistically significant.

#4. Variation Inflation Factor

vif(model_ref)
# local_tp: (1.113772)
# west_tp: (1.075333)
# int_tp: (1.144612)

#5. Linear Model
dep_var_ended_ref_lm <- peace$ended
indep_vars_tp_ref_lm <- peace[,c("local_tp", "west_tp", "int_tp", "GDis", "GMig", "GRa", "GRe", "GOth", "GSoc", "GeMe", "GeFa", "ConRen", "Cons", "PolPar", "Civso", "Tral", "Pubad", "Polps", "Terps", "Eps", "Mps", "HrGen", "EqGen", "Prot", "HrFra", "HrCp", "HrSec", "HrNi", "HrIi", "HrDet", "JusCr", "JusEm", "JusJu", "JusPri", "JusTra", "IntFu", "Ban", "LaEn", "SsrGua", "SsrPol", "SsrArm", "SsrDdr", "SsrInt", "SsrPsf", "SsrFf", "Cor", "SsrCrOcr", "SsrDrugs", "Terr", "TjGen", "TjAm", "TjCou", "TjPrire", "TjVic", "TjMis", "TjRep", "TjNR")]

model_ref_lm <- lm(dep_var_ended_ref_lm ~ local_tp + west_tp + int_tp + LaEn:IntFu:Ban + GDis:GMig:GRa:GRe:GOth:GSoc:GeMe:GeFa + ConRen:Cons:PolPar:Civso:Tral:Pubad:Polps:Terps:Eps:Mps + HrGen:EqGen:Prot:HrFra:HrCp:HrSec:HrNi:HrIi:HrDet + JusCr:JusEm:JusJu:JusPri:JusTra + SsrGua:SsrPol:SsrArm:SsrDdr:SsrInt:SsrPsf:SsrFf:Cor:SsrCrOcr:SsrDrugs + Terr:TjGen:TjAm:TjCou:TjPrire:TjVic:TjMis:TjRep:TjNR, data = peace, family = binomial)
summary(model_ref_lm) 

stargazer(model_ref_lm, type = "text")





## ---------------------- MODEL 5: Country Controls ---------------------- ##

#Variables:
dep_var_ended_cc <- peace$ended
indep_vars_tp_cc <- peace[,c("local_tp", "west_tp", "int_tp", "GDP", "GFS")]

#Model: 
model_cc <- glm(dep_var_ended_cc ~ local_tp + west_tp + int_tp + GDP + GFS, data = peace, family = binomial)
summary(model_cc)

stargazer(model_cc, type = "text")

# local_tp                     0.199       (0.195)          
# west_tp                     -1.761***    (0.333)          
# int_tp                       0.517**     (0.228) 
# Constant                    -2.793***    (0.190) 
# Observations                  942            
# Log Likelihood               -379.143           
# Akaike Inf. Crit.             770.286  


## MODEL FIT AND ROBUST CHECK:

# 1. Goodness of fit tests (Pearson Chi Squared Test)

observed_cc <- table(peace$ended)
predicted_cc <- table(predict(model_cc, type = "response") > 0.5)
observed_predicted_cc <- cbind(observed_cc, predicted_cc)

# Perform the chi-squared test
chisq.test(observed_predicted_cc)    # p-value = 0.0001022 --> less than 0.05
# difference between the observed and predicted values is statistically significant and the model is a good fit for the data


# 1.2 Goodness of fit tests (Hosmer and Lemeshow Test)

predicted_probs_cc <- predict(model_cc, type = "response")
observed_outcomes_cc <- peace$ended
hoslem.test(observed_outcomes_cc, predicted_probs_cc) # p-value = 0.5692 -- Fit as above 0.05 significant rate


# 2. Residual Analysis 

roc_basic_cc <- roc(peace$ended, predict(model_cc, type = "response"))
plot(roc_basic_cc, main = "ROC Curve - Model 5", print.auc = TRUE, legacy.axes = TRUE)     

# 0.832 -->  model has only slightly better than chance level predictive ability

#3. Odds Ratio Test: 

coef_1_cc <- summary(model_cc)$coef[,1]
se_1_cc <- summary(model_cc)$coef[,2]

OR_1_cc <- exp(coef_1_cc)
CI_1_cc <- exp(cbind(coef_1_cc - 1.96*se_1_cc, coef_1_cc + 1.96*se_1_cc))

cbind(OR_1_cc, CI_1_cc)

# west_tp:  95% confidence interval:   0.17188075 0.08946367 0.33022334
#The 95% CI excludes 1, indicating that the effect of west_tp is statistically significant.

#4. Variation Inflation Factor

vif(model_cc)
# local_tp: (1.105317)
# west_tp:  (1.281711)
# int_tp:   (1.111325)

#5. Linear Model:
dep_var_ended_cc_lm <- peace$ended
indep_vars_tp_cc_lm <- peace[,c("local_tp", "west_tp", "int_tp", "GDP", "GFS")]

model_cc_lm <- lm(dep_var_ended_cc_lm ~ local_tp + west_tp + int_tp + GDP + GFS, data = peace, family = binomial)
summary(model_cc_lm)

stargazer(model_cc_lm, type = "text")






## ---------------------- MODEL 6:ALL ---------------------- ##

#Variables:
peace_year <- factor(peace$Dat_year)
dep_var_ended_all <- peace$ended
indep_vars_tp_all <- peace[,c("local_tp", "west_tp", "int_tp", "GDP", "GFS", "LaEn", "IntFu", "Ban", "GDis", "GMig", "GRa", "GRe", "GOth", "GSoc", "GeMe", "GeFa", "ConRen", "Cons", "PolPar", "Civso", "Tral", "Pubad", "Polps", "Terps", "Eps", "Mps", "HrGen", "EqGen", "Prot", "HrFra", "HrCp", "HrSec", "HrNi", "HrIi", "HrDet", "JusCr", "JusEm", "JusJu", "JusPri", "JusTra", "SsrGua", "SsrPol", "SsrArm", "SsrDdr", "SsrInt", "SsrPsf", "SsrFf", "Cor", "SsrCrOcr", "SsrDrugs", "Terr", "TjGen", "TjAm", "TjCou", "TjPrire", "TjVic", "TjMis", "TjRep", "TjNR", "agtp_inter", "Stage_comp", "Stage_ren")]

#Model: 
model_all <- glm(dep_var_ended_all ~ local_tp + west_tp + int_tp + GDP + GFS + LaEn:IntFu:Ban + GDis + GMig + GRa + GRe + GOth + GSoc + GeMe + GeFa + ConRen:Cons:PolPar:Civso:Tral:Pubad:Polps:Terps:Eps:Mps + HrGen:EqGen:Prot:HrFra:HrCp:HrSec:HrNi:HrIi:HrDet + JusCr:JusEm:JusJu:JusPri:JusTra + SsrGua:SsrPol:SsrArm:SsrDdr:SsrInt:SsrPsf:SsrFf:Cor:SsrCrOcr:SsrDrugs + Terr:TjGen:TjAm:TjCou:TjPrire:TjVic:TjMis:TjRep:TjNR + agtp_inter + Stage_comp + Stage_ren, data = peace, family = binomial)
summary(model_all)

stargazer(model_all, type = "text")

# local_tp                     0.325        (0.208)          
# west_tp                     -1.918***     (0.355)          
# int_tp                       0.432*       (0.238) 
# Constant                    -3.162***     (0.225)  
# Observations                  942            
# Log Likelihood                -357.507           
# Akaike Inf. Crit.             761.014   


## MODEL FIT AND ROBUST CHECK:

# 1. Goodness of fit tests (Pearson Chi Squared Test)

observed_all <- table(peace$ended)
predicted_all <- table(predict(model_all, type = "response") > 0.5)
observed_predicted_all <- cbind(observed_all, predicted_all)

# Perform the chi-squared test
chisq.test(observed_predicted_all)    # p-value = 0.002828 --> less than 0.05
# difference between the observed and predicted values is statistically significant and the model is a good fit for the data


# 1.2 Goodness of fit tests (Hosmer and Lemeshow Test) 

predicted_probs_all <- predict(model_all, type = "response")
observed_outcomes_all <- peace$ended
hoslem.test(observed_outcomes_all, predicted_probs_all) # p-value = 0.5762 --  Fit as under 0.05 significant rate


# 2. Residual Analysis

roc_basic_all <- roc(peace$ended, predict(model_all, type = "response"))
plot(roc_basic_all, main = "ROC Curve - Model 6", print.auc = TRUE, legacy.axes = TRUE)     

# 0.869 -->  model has only slightly better than chance level predictive ability

#3. Odds Ratio Test: 

coef_1_all <- summary(model_all)$coef[,1]
se_1_all <- summary(model_all)$coef[,2]

OR_1_all <- exp(coef_1_all)
CI_1_all <- exp(cbind(coef_1_all - 1.96*se_1_all, coef_1_all + 1.96*se_1_all))

cbind(OR_1_all, CI_1_all)

# west_tp:  95% confidence interval:   0.14696643 0.0733134135 2.946137e-01
#The 95% CI excludes 1, indicating that the effect of west_tp is statistically significant.

#4. Variation Inflation Factor

vif(model_all)
# local_tp: (1.181581)
# west_tp:  (1.331889)
# int_tp:   (1.128448)

#5: Linear Model

model_all_lm <- lm(dep_var_ended_all_lm ~ local_tp + west_tp + int_tp + Dat_year + GDP + GFS + Contp_terr + Status_mult + Status_uni + agtp_inter + Stage_comp + Stage_ren + LaEn:IntFu:Ban + GDis:GMig:GRa:GRe:GOth:GSoc:GeMe:GeFa + ConRen:Cons:PolPar:Civso:Tral:Pubad:Polps:Terps:Eps:Mps + HrGen:EqGen:Prot:HrFra:HrCp:HrSec:HrNi:HrIi:HrDet + JusCr:JusEm:JusJu:JusPri:JusTra + SsrGua:SsrPol:SsrArm:SsrDdr:SsrInt:SsrPsf:SsrFf:Cor:SsrCrOcr:SsrDrugs + Terr:TjGen:TjAm:TjCou:TjPrire:TjVic:TjMis:TjRep:TjNR, data = peace)
summary(model_all_lm)
stargazer(model_all_lm, type = "text")







## -------------------- Descriptive Statistics: SENSITIVITY ANALYSIS ---------------------------------------------##


#Year Summary: 
summary(peace$Dat_year)
#Calculating type of third party mediator per year: 
table(peace$Dat_year[peace$west_tp == 1])
table(peace$Dat_year[peace$local_tp == 1])
table(peace$Dat_year[peace$int_tp == 1])
table(peace$Dat_year[peace$tp == 1])

# Country Summary (which country is most and least involved in agreements)
table(peace$Con)

# Specifying type of mediator involved in one countries agreements (Sudan who has most data): 
table(peace$Dat_year[peace$Con == "South Sudan"])
table(peace$Dat_year[peace$Con == "South Sudan" | peace$Con == "South Sudan/Sudan" | peace$Con == "Angola/Burundi/Central African Republic/Democratic Republic of Congo/Republic of Congo/Rwanda/South Africa/South Sudan/Tanzania/Uganda/Zambia/(African Great Lakes)" | peace$Con == "South Sudan/Sudan/(Southern Kordofan - Blue Nile - Abyei)" | peace$Con == "South Sudan/Sudan/Darfur" | peace$Con == "South Sudan/Sudan/Southern Kordofan - Blue Nile - Abyei"])
table(peace$Dat_year[peace$Con %in% c("South Sudan", "South Sudan/Sudan", "Angola/Burundi/Central African Republic/Democratic Republic of Congo/Republic of Congo/Rwanda/South Africa/South Sudan/Tanzania/Uganda/Zambia/(African Great Lakes)", "South Sudan/Sudan/(Southern Kordofan - Blue Nile - Abyei)", "South Sudan/Sudan/Darfur", "South Sudan/Sudan/Southern Kordofan - Blue Nile - Abyei") & peace$local_tp == 1])
table(peace$Dat_year[peace$Con %in% c("South Sudan", "South Sudan/Sudan", "Angola/Burundi/Central African Republic/Democratic Republic of Congo/Republic of Congo/Rwanda/South Africa/South Sudan/Tanzania/Uganda/Zambia/(African Great Lakes)", "South Sudan/Sudan/(Southern Kordofan - Blue Nile - Abyei)", "South Sudan/Sudan/Darfur", "South Sudan/Sudan/Southern Kordofan - Blue Nile - Abyei") & peace$west_tp == 1])
table(peace$Dat_year[peace$Con %in% c("South Sudan", "South Sudan/Sudan", "Angola/Burundi/Central African Republic/Democratic Republic of Congo/Republic of Congo/Rwanda/South Africa/South Sudan/Tanzania/Uganda/Zambia/(African Great Lakes)", "South Sudan/Sudan/(Southern Kordofan - Blue Nile - Abyei)", "South Sudan/Sudan/Darfur", "South Sudan/Sudan/Southern Kordofan - Blue Nile - Abyei") & peace$tp == 1])
table(peace$Dat_year[peace$Con %in% c("South Sudan", "South Sudan/Sudan", "Angola/Burundi/Central African Republic/Democratic Republic of Congo/Republic of Congo/Rwanda/South Africa/South Sudan/Tanzania/Uganda/Zambia/(African Great Lakes)", "South Sudan/Sudan/(Southern Kordofan - Blue Nile - Abyei)", "South Sudan/Sudan/Darfur", "South Sudan/Sudan/Southern Kordofan - Blue Nile - Abyei") & peace$int_tp == 1])
table(peace$Dat_year[peace$Con %in% c("South Sudan", "South Sudan/Sudan", "Angola/Burundi/Central African Republic/Democratic Republic of Congo/Republic of Congo/Rwanda/South Africa/South Sudan/Tanzania/Uganda/Zambia/(African Great Lakes)", "South Sudan/Sudan/(Southern Kordofan - Blue Nile - Abyei)", "South Sudan/Sudan/Darfur", "South Sudan/Sudan/Southern Kordofan - Blue Nile - Abyei") & peace$tp == 1])

# Peace Ended Descriptive Stats: 
table(peace$ended)
table(peace$ended[peace$west_tp == 1])
table(peace$ended[peace$local_tp == 1])
table(peace$ended[peace$int_tp == 1])
table(peace$ended[peace$tp == 1])

# Of Peace Agreements Ended, Type of TP involved:
table(peace$Con[peace$ended ==1])
table(interaction(peace$west_tp[peace$ended == 1], peace$Con[peace$ended == 1]))
table(interaction(peace$local_tp[peace$ended == 1], peace$Con[peace$ended == 1]))
table(interaction(peace$tp[peace$ended == 1], peace$Con[peace$ended == 1]))

# Specifying type of mediator involved in one countries agreements (Liberia who has most data of conflict ended): 
table(peace$Dat_year[peace$Con == "Liberia"])
table(peace$Dat_year[peace$Con == "Liberia" & peace$local_tp == 1])
table(peace$Dat_year[peace$Con == "Liberia" & peace$west_tp == 1])
table(peace$Dat_year[peace$Con == "Liberia" & peace$tp == 1])
table(peace$Dat_year[peace$Con == "Liberia" & peace$int_tp == 1])

# Burundi
table(peace$Dat_year[peace$Con == "Burundi"])
table(peace$Dat_year[peace$Con == "Burundi" & peace$local_tp == 1])
table(peace$Dat_year[peace$Con == "Burundi" & peace$west_tp == 1])
table(peace$Dat_year[peace$Con == "Burundi" & peace$tp == 1])
table(peace$Dat_year[peace$Con == "Burundi" & peace$int_tp == 1])

# Cote d'Ivoire
table(peace$Dat_year[peace$Con == "Cote d'Ivoire"])
table(peace$Dat_year[peace$Con == "Cote d'Ivoire" & peace$local_tp == 1])
table(peace$Dat_year[peace$Con == "Cote d'Ivoire" & peace$west_tp == 1])        # --> 0
table(peace$Dat_year[peace$Con == "Cote d'Ivoire" & peace$tp == 1])       
table(peace$Dat_year[peace$Con == "Cote d'Ivoire" & peace$int_tp == 1])


# Kenya
table(peace$Dat_year[peace$Con == "Kenya"])
table(peace$Dat_year[peace$Con == "Kenya" & peace$local_tp == 1])
table(peace$Dat_year[peace$Con == "Kenya" & peace$west_tp == 1])    #--> 0 
table(peace$Dat_year[peace$Con == "Kenya" & peace$tp == 1])
table(peace$Dat_year[peace$Con == "Kenya" & peace$int_tp == 1])     #--> 0

# Angola
table(peace$Dat_year[peace$Con == "Angola"])
table(peace$Dat_year[peace$Con == "Angola" & peace$local_tp == 1])
table(peace$Dat_year[peace$Con == "Angola" & peace$west_tp == 1])     
table(peace$Dat_year[peace$Con == "Angola" & peace$tp == 1])
table(peace$Dat_year[peace$Con == "Angola" & peace$int_tp == 1]) 

# Chad
table(peace$Dat_year[peace$Con == "Chad"])
table(peace$Dat_year[peace$Con == "Chad" & peace$local_tp == 1])
table(peace$Dat_year[peace$Con == "Chad" & peace$west_tp == 1])      
table(peace$Dat_year[peace$Con == "Chad" & peace$tp == 1])
table(peace$Dat_year[peace$Con == "Chad" & peace$int_tp == 1])         #--> 0         

# Gabon [One agreement and all involved] 
table(peace$Dat_year[peace$Con == "Gabon"])
table(peace$Dat_year[peace$Con == "Gabon" & peace$local_tp == 1])
table(peace$Dat_year[peace$Con == "Gabon" & peace$west_tp == 1])      
table(peace$Dat_year[peace$Con == "Gabon" & peace$tp == 1])
table(peace$Dat_year[peace$Con == "Gabon" & peace$int_tp == 1]) 

# Lesotho 
table(peace$Dat_year[peace$Con == "Lesotho"])
table(peace$Dat_year[peace$Con == "Lesotho" & peace$local_tp == 1])
table(peace$Dat_year[peace$Con == "Lesotho" & peace$west_tp == 1])      
table(peace$Dat_year[peace$Con == "Lesotho" & peace$tp == 1])
table(peace$Dat_year[peace$Con == "Lesotho" & peace$int_tp == 1]) 

# Madagascar 
table(peace$Dat_year[peace$Con == "Madagascar"])
table(peace$Dat_year[peace$Con == "Madagascar" & peace$local_tp == 1])
table(peace$Dat_year[peace$Con == "Madagascar" & peace$west_tp == 1])     #--> 0 
table(peace$Dat_year[peace$Con == "Madagascar" & peace$tp == 1])
table(peace$Dat_year[peace$Con == "Madagascar" & peace$int_tp == 1])  

# Republic of Congo 
table(peace$Dat_year[peace$Con == "Republic of Congo"])
table(peace$Dat_year[peace$Con == "Republic of Congo" & peace$local_tp == 1])
table(peace$Dat_year[peace$Con == "Republic of Congo" & peace$west_tp == 1])     #--> 0 
table(peace$Dat_year[peace$Con == "Republic of Congo" & peace$tp == 1])
table(peace$Dat_year[peace$Con == "Republic of Congo" & peace$int_tp == 1])      #--> 0  

# Zimbabwe 
table(peace$Dat_year[peace$Con == "Zimbabwe"])
table(peace$Dat_year[peace$Con == "Zimbabwe" & peace$local_tp == 1])
table(peace$Dat_year[peace$Con == "Zimbabwe" & peace$west_tp == 1])     #--> 0 
table(peace$Dat_year[peace$Con == "Zimbabwe" & peace$tp == 1])
table(peace$Dat_year[peace$Con == "Zimbabwe" & peace$int_tp == 1])      #--> 0 





