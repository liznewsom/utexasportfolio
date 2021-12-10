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


#USCAN <- USIC_Trade_Volatility_US_CAN
USCAN <- Nov29_USIC_Trade_Volatility_3_
US_CAN_imports <- USCAN$M_CAN
US_CAN_income <- USCAN$Y_US
CAN_REX <- USCAN$REX_CAN
CAN_VOL <- USCAN$VOL_CAN
Year_CAN <- as.factor(USCAN$t_CAN)
Comm_CAN <- as.factor(USCAN$Comm_Code_CAN)
logM_CAN <- USCAN$Ln_M_CAN
logUSIncome_CAN <- USCAN$Ln_Y_US_CAN
logREX_CAN <- USCAN$Ln_REX_CAN
logVOL_CAN <- USCAN$Ln_VOL_CAN

#Code for US Canada Imports regression
LOGreg_can_imp_3var_2cont <- lm(logM_CAN ~ logUSIncome_CAN + logREX_CAN + logVOL_CAN + Year_CAN + Comm_CAN, data = USCAN)
summary(LOGreg_can_imp_3var_2cont)

#Code for US Canada Exports regression

logX_CAN <- USCAN$Ln_X_CAN
logIncome_CAN <- USCAN$Ln_Y_CAN


LOGreg_can_exp_3var_2cont <- lm(logX_CAN ~ logIncome_CAN + logREX_CAN + logVOL_CAN + Year_CAN + Comm_CAN, data = USCAN)
summary(LOGreg_can_exp_3var_2cont)

