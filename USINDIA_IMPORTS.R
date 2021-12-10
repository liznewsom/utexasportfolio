#reg1 <- lm(USIC_Trade_Volatility_US_India$M ~ USIC_Trade_Volatility_US_India$Y_US )
#print()
#reg1 <- lm(USIC_Trade_Volatility_US_India$M ~ USIC_Trade_Volatility_US_India$Y_US, data = USIC_Trade_Volatility_US_India)
#print(reg1)
#library(readxl)
#USINDIA <- readexcel("USIC_Trade_Volatility_US_India.xls")
#regImpInd <- lm(USINDIA$M ~ USINDIA$Y_US)
#summary(regImpInd)

library(tidyverse)
library(magrittr)
library(psych)
library(foreign)
library(haven)
library(stats)
library(skimr)
library(rddtools)
library(stargazer)
library(MatchIt)
library(PowerUpR)
library(pwr)
library(dplyr)
library(ggplot2)
library(compute.es)
library(lm.beta)
library(broom)
library(AER)
library(systemfit)
library(episensr)
library(dampack)
library(qwraps2)
library(table1)
USINDIA <- USIC_Trade_Volatility_US_India
US_IND_imports <- USINDIA$M
US_income <- USINDIA$Y_US
Ind_REX <- USINDIA$REX
Ind_VOL <- USINDIA$VOL
Year <- as.factor(USINDIA$t)
Comm <- as.factor(USINDIA$Comm_Codes)
logM <- USINDIA$Ln_M
logUSIncome <- USINDIA$Ln_Y_US
logIndREX <- USINDIA$Ln_REX
logIndVOL <- USINDIA$Ln_VOL

#assigning for exports
logExports_IND <- USINDIA$Ln_X
logIncome_US_IND <- USINDIA$Ln_Y_US

#practice regressions
reg_ind_imp <- lm(US_IND_imports ~ US_income , data = USINDIA)
summary(reg_ind_imp)
reg_ind_imp_3var_1cont <- lm(US_IND_imports ~ US_income + Ind_REX + Ind_VOL + Year, data = USINDIA)
summary(reg_ind_imp_3var_1cont)

reg_ind_imp_3var_2cont <- lm(US_IND_imports ~ US_income + Ind_REX + Ind_VOL + Year + Comm, data = USINDIA)
summary(reg_ind_imp_3var_2cont)

#code for imports
LOGreg_ind_imp_3var_2cont <- lm(logM ~ logUSIncome + logIndREX + logIndVOL + Year + Comm, data = USINDIA)
summary(LOGreg_ind_imp_3var_2cont)

#code for exports

LOGreg_ind_exp_3var_2cont <- lm(logExports_IND ~ logIncome_US_IND + logIndREX + logIndVOL + Year + Comm, data = USINDIA)
summary(LOGreg_ind_exp_3var_2cont)
