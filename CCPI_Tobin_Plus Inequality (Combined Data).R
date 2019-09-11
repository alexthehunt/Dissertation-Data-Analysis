install.packages('openxlsx')
install.packages('tidyverse')
install.packages('clusterSim')

library('tidyverse')
library('openxlsx')
library('clusterSim')
library('ggplot2')

ClimateRank <- read.xlsx('CCPI_Tobin_Plus Inequality (Combined Data).xlsx') #Creates full dataframe for testing

View(ClimateRank) #view full dataframe

ClimateScore <- ClimateRank$Climate.Score
PolicyScore <- ClimateRank$Policy.Score
LeftWing <- ClimateRank$`Left-Wing.Government.(leftgov)`
EU_Member <- ClimateRank$`EU.Membership.(eumember)`
Pol_Con <- ClimateRank$`Political.Constraints.(polcon)`
GDP <- ClimateRank$`High.GDP.per.Capita.(highgdp)`
ClimateAmb <- ClimateRank$`Ambitious.Climate.Policy.(ambclimpol)`
Inequality <- ClimateRank$OECD.Inequality

CS_MLR <- lm(ClimateScore ~ Inequality + LeftWing + EU_Mem + Pol_Con + GDP, data = ClimateRank) #Multiple linear regression (MLR) of all variables against IPPC's Climate Score

summary(CS_MLR) #Inequality is the second most signficant variable (>.1) after EU Membership (>.05)


AMB_MLR <- lm(ClimateAmb ~ Inequality + LeftWing + EU_Mem + Pol_Con + GDP, data = ClimateRank) #Multiple linear regression (MLR) of all variables against Paul Tobin's Ambitious Climate Policy ranking

summary(AMB_MLR) #Inequality rank's as third most significant factor against Ambitious Climate Poliy in MLR


ClimateScore_lm <- lm(ClimateScore ~ Inequality) #OLS examining inequality's influence on Climate Score

summary(ClimateScore_lm) #Inequality has high signficance against Climate Score! >.01


EU_Score <- lm(ClimateScore ~ EU_Member, data = ClimateRank) #OLS examining EU Membership influence on Climate Score

summary(EU_Score) #EU Membership has a VERY high significance to Climate Score (also very significant for Ambition!)!


EU_Inequality <- lm(EU_Member ~ Inequality, data = ClimateRank) #OLS examing inequality's influence on EU Membership
summary(EU_Inequality) #Inequality's significance is >.05!


ClimateAmb_lm <- lm(ClimateAmb ~ Inequality, data = ClimateRank) #OLS examining inequality's influence on Ambitious Climate policy

summary(ClimateAmb_lm) #Inequality's significance is below >.1


ClimatePolicy_lm <- lm(PolicyScore ~ Inequality, data = ClimateRank) #OLS examining inequality's influence on IPPC's Policy Score variable

summary(ClimatePolicy_lm) #Inequality has a significance >.05

