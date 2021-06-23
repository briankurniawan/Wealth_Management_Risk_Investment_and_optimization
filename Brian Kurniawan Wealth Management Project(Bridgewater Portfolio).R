#Bridgewater Portfolio Analysis - Top 10
#######
## Created by Brian Kurniawan
#######
## Hult Business School
## Wealth and Investment Management Risk in R
#######

bridgewater_tickers <- c("SPY","VWO","WMT","PG","BABA",'KO','JNJ',"GLD","PEP",'IEMG')


SPY_returns <- monthlyReturn(getSymbols("SPY",auto.assign = F))
VWO_returns <- monthlyReturn(getSymbols("VWO",auto.assign = F))
WMT_returns <- monthlyReturn(getSymbols("WMT",auto.assign = F))
PG_returns <- monthlyReturn(getSymbols("PG",auto.assign = F))
BABA_returns <- monthlyReturn(getSymbols("BABA",auto.assign = F))
KO_returns <- monthlyReturn(getSymbols("KO",auto.assign = F))
JNJ_returns <- monthlyReturn(getSymbols("JNJ",auto.assign = F))
GLD_returns <- monthlyReturn(getSymbols("GLD",auto.assign = F))
PEP_returns <- monthlyReturn(getSymbols("PEP",auto.assign = F))
IEMG_returns <- monthlyReturn(getSymbols("IEMG",auto.assign = F))



bridgewater_monthly_returns <- as.data.frame(merge.xts(SPY_returns,VWO_returns,WMT_returns,PG_returns,BABA_returns,KO_returns,JNJ_returns,GLD_returns,PEP_returns,IEMG_returns))



names(bridgewater_monthly_returns)<- c("SPY","VWO","WMT","PG","BABA",'KO','JNJ',"GLD","PEP",'IEMG')

#Assets invested in each
SPY_value <- 1240
VWO_value <- 577.22
WMT_value <- 487.76
PG_value <- 435.90
BABA_value <- 318.04
KO_value <- 298.7
JNJ_value <- 281.02
GLD_value <- 277.11
PEP_value <- 252.98
IEMG_value <- 242.53

bridgewater_combined_value <- as.data.frame(rbind(SPY_value,VWO_value,WMT_value,PG_value,BABA_value,
                                    KO_value,JNJ_value,GLD_value,PEP_value,IEMG_value))

bridgewater_combined_value_adj <- as.data.frame(rbind(SPY_value,VWO_value,WMT_value,PG_value,
                                                  KO_value,JNJ_value,GLD_value,PEP_value,IEMG_value))

sum_bridgewater_value <- sum(bridgewater_combined_value)
sum_bridgewater_value_adj <- sum(bridgewater_combined_value_adj)

bridgewater_combined_value[,2] <- c()
for(i in 1:nrow(bridgewater_combined_value)){
  bridgewater_combined_value[i,2] = bridgewater_combined_value[i,1] / sum_bridgewater_value
}

bridgewater_combined_value_adj[,2] <- c()
for(i in 1:nrow(bridgewater_combined_value_adj)){
  bridgewater_combined_value_adj[i,2] = bridgewater_combined_value_adj[i,1] / sum_bridgewater_value_adj
}
  
bridgewater_combined_value_adj
  
bridgewater_portfolio_return <- as.data.frame(bridgewater_monthly_returns) %>% 
                                  mutate(portfolio = bridgewater_combined_value[1,2]*SPY +
                                                     bridgewater_combined_value[2,2]*VWO +
                                                     bridgewater_combined_value[3,2]*WMT+
                                                     bridgewater_combined_value[4,2]*PG+
                                                     bridgewater_combined_value[5,2]*BABA+
                                                     bridgewater_combined_value[6,2]*KO+
                                                     bridgewater_combined_value[7,2]*JNJ+
                                                     bridgewater_combined_value[8,2]*GLD+
                                                     bridgewater_combined_value[9,2]*PEP+
                                                     bridgewater_combined_value[10,2]*IEMG)

bridgewater_portfolio_return_adj <- as.data.frame(bridgewater_monthly_returns) %>% 
  mutate(portfolio_adj = bridgewater_combined_value[1,2]*SPY +
           bridgewater_combined_value_adj[2,2]*VWO +
           bridgewater_combined_value_adj[3,2]*WMT+
           bridgewater_combined_value_adj[4,2]*PG+
           bridgewater_combined_value_adj[5,2]*KO+
           bridgewater_combined_value_adj[6,2]*JNJ+
           bridgewater_combined_value_adj[7,2]*GLD+
           bridgewater_combined_value_adj[8,2]*PEP+
           bridgewater_combined_value_adj[9,2]*IEMG)

#View(bridgewater_portfolio_return_adj)

#Adding a benchmark - Vanguard Russel 1000 (VONE)
benchmark_returns_bridge <- as.data.frame(monthlyReturn(getSymbols("SPY",auto.assign=F)))
names(benchmark_returns_bridge) <- c("benchmark_monthly")
joined_bridge_monthly_returns <- cbind(bridgewater_portfolio_return,benchmark_returns_bridge)
joined_bridge_monthly_returns_adj <- cbind(bridgewater_portfolio_return_adj,benchmark_returns_bridge)

#calculating Sigma for our securities
#"SPY","VWO","WMT","PG","BABA",'KO','JNJ',"GLD","PEP",'IEMG'

names(joined_bridge_monthly_returns)<- c("SPY","VWO","WMT","PG","BABA",'KO','JNJ',"GLD","PEP",'IEMG',"Portfolio","Benchmark")

time_index <- nrow(joined_bridge_monthly_returns) #(number of months in data) 

SPY_sigma <- sd(joined_bridge_monthly_returns$SPY[(time_index-(59)):time_index])*sqrt(12) 
VWO_sigma <- sd(joined_bridge_monthly_returns$VWO[(time_index-(59)):time_index])*sqrt(12)
WMT_sigma <- sd(joined_bridge_monthly_returns$WMT[(time_index-(59)):time_index])*sqrt(12) 
PG_sigma <- sd(joined_bridge_monthly_returns$PG[(time_index-(59)):time_index])*sqrt(12) 
BABA_sigma <- sd(joined_bridge_monthly_returns$BABA[(time_index-(59)):time_index])*sqrt(12)
KO_sigma <- sd(joined_bridge_monthly_returns$KO[(time_index-(59)):time_index])*sqrt(12) 
JNJ_sigma <- sd(joined_bridge_monthly_returns$JNJ[(time_index-(59)):time_index])*sqrt(12) 
GLD_sigma <- sd(joined_bridge_monthly_returns$GLD[(time_index-(59)):time_index])*sqrt(12) 
PEP_sigma <- sd(joined_bridge_monthly_returns$PEP[(time_index-(59)):time_index])*sqrt(12) 
IEMG_sigma <- sd(joined_bridge_monthly_returns$IEMG[(time_index-(59)):time_index])*sqrt(12)
portfolio_sigma <- sd(joined_bridge_monthly_returns$Portfolio[(time_index-(59)):time_index])*sqrt(12)
portfolio_adj_sigma <- sd(joined_bridge_monthly_returns_adj$portfolio_adj[(time_index-(59)):time_index])*sqrt(12)

#View(joined_bridge_monthly_returns_adj)

summary_table_sigma <- as.data.frame(cbind(VWO_sigma,WMT_sigma,PG_sigma,BABA_sigma,KO_sigma,JNJ_sigma,GLD_sigma,
                  PEP_sigma,IEMG_sigma,portfolio_sigma,portfolio_adj_sigma))

summary_table_main <- NULL

names(summary_table_sigma)<- c("VWO","WMT","PG","BABA",'KO','JNJ',"GLD","PEP",'IEMG',"Portfolio","Adjusted Portfolio")
row.names(summary_table_sigma) <- c("sigma")
#View(summary_table_sigma)


#calculating betas
last_5_years <- joined_bridge_monthly_returns[(time_index-59):time_index,]
last_5_years_adj <- joined_bridge_monthly_returns_adj[(time_index-59):time_index,]

VWO_reg <- lm(VWO ~ Benchmark,data=last_5_years)
WMT_reg <- lm(WMT ~ Benchmark,data=last_5_years)
PG_reg <- lm(PG ~ Benchmark,data=last_5_years)
BABA_reg <- lm(BABA ~ Benchmark,data=last_5_years)
KO_reg <- lm(KO ~ Benchmark,data=last_5_years)
JNJ_reg <- lm(JNJ ~ Benchmark,data=last_5_years)
GLD_reg <- lm(GLD ~ Benchmark,data=last_5_years)
PEP_reg <- lm(PEP ~ Benchmark,data=last_5_years)
PG_reg <- lm(PG ~ Benchmark,data=last_5_years)
IEMG_reg <- lm(IEMG ~ Benchmark,data=last_5_years)
portfolio_reg <- lm(Portfolio ~ Benchmark,data=last_5_years)
portfolio_adj_reg <- lm(portfolio_adj ~ benchmark_monthly,data=last_5_years_adj)

#View(last_5_years_adj)


# VWO_reg <- lm(Benchmark ~ VWO,data=last_5_years)
# WMT_reg <- lm(Benchmark ~ WMT,data=last_5_years)
# PG_reg <- lm(Benchmark ~ PG,data=last_5_years)
# BABA_reg <- lm(Benchmark ~ BABA,data=last_5_years)
# KO_reg <- lm(Benchmark ~ KO,data=last_5_years)
# JNJ_reg <- lm(Benchmark ~ JNJ,data=last_5_years)
# GLD_reg <- lm(Benchmark ~ GLD,data=last_5_years)
# PEP_reg <- lm(Benchmark ~ PEP,data=last_5_years)
# PG_reg <- lm(Benchmark ~ PG,data=last_5_years)
# IEMG_reg <- lm(Benchmark ~ IEMG,data=last_5_years)
# portfolio_reg <- lm(Benchmark ~ Portfolio,data=last_5_years)

VWO_coef <- coef(VWO_reg)
WMT_coef <- coef(WMT_reg)
PG_coef <- coef(PG_reg)
BABA_coef <- coef(BABA_reg)
KO_coef <- coef(KO_reg)
JNJ_coef <- coef(JNJ_reg)
GLD_coef <- coef(GLD_reg)
PEP_coef <- coef(PEP_reg)
PG_coef <- coef(PG_reg)
IEMG_coef <- coef(IEMG_reg)
portfolio_coef <- coef(portfolio_reg)
portfolio_adj_coef <- coef(portfolio_adj_reg)



summary_table_coef <- as.data.frame(cbind(VWO_coef,WMT_coef,PG_coef,BABA_coef,KO_coef,JNJ_coef,GLD_coef,
                                     PEP_coef,IEMG_coef,portfolio_coef,portfolio_adj_coef))

names(summary_table_coef)<- c("VWO","WMT","PG","BABA",'KO','JNJ',"GLD","PEP",'IEMG',"Portfolio","Adjusted Portfolio")
row.names(summary_table_coef) <- c("Intercept","coef")
#View(summary_table_coef)


summary_table_main <- rbind(summary_table_sigma,summary_table_coef[2,])
#View(summary_table_main)


VWO_te <- sd(joined_bridge_monthly_returns$VWO[(time_index-(59)):time_index]-
                 joined_bridge_monthly_returns$Benchmark[(time_index-(59)):time_index])*sqrt(12)
WMT_te <- sd(joined_bridge_monthly_returns$WMT[(time_index-(59)):time_index]-
               joined_bridge_monthly_returns$Benchmark[(time_index-(59)):time_index])*sqrt(12)
PG_te <- sd(joined_bridge_monthly_returns$PG[(time_index-(59)):time_index]-
              joined_bridge_monthly_returns$Benchmark[(time_index-(59)):time_index])*sqrt(12)
BABA_te <- sd(joined_bridge_monthly_returns$BABA[(time_index-(59)):time_index]-
                joined_bridge_monthly_returns$Benchmark[(time_index-(59)):time_index])*sqrt(12)
KO_te <- sd(joined_bridge_monthly_returns$KO[(time_index-(59)):time_index]-
              joined_bridge_monthly_returns$Benchmark[(time_index-(59)):time_index])*sqrt(12)
JNJ_te <- sd(joined_bridge_monthly_returns$JNJ[(time_index-(59)):time_index]-
               joined_bridge_monthly_returns$Benchmark[(time_index-(59)):time_index])*sqrt(12)
GLD_te <- sd(joined_bridge_monthly_returns$GLD[(time_index-(59)):time_index]-
               joined_bridge_monthly_returns$Benchmark[(time_index-(59)):time_index])*sqrt(12)
PEP_te <- sd(joined_bridge_monthly_returns$PEP[(time_index-(59)):time_index]-
               joined_bridge_monthly_returns$Benchmark[(time_index-(59)):time_index])*sqrt(12)
IEMG_te <- sd(joined_bridge_monthly_returns$IEMG[(time_index-(59)):time_index]-
                joined_bridge_monthly_returns$Benchmark[(time_index-(59)):time_index])*sqrt(12)
portfolio_te <- sd(joined_bridge_monthly_returns$Portfolio[(time_index-(59)):time_index]-
                     joined_bridge_monthly_returns$Benchmark[(time_index-(59)):time_index])*sqrt(12)
portfolio_adj_te <- sd(joined_bridge_monthly_returns_adj$portfolio_adj[(time_index-(59)):time_index]-
                     joined_bridge_monthly_returns_adj$benchmark_monthly[(time_index-(59)):time_index])*sqrt(12)


summary_table_te <- as.data.frame(cbind(VWO_te,WMT_te,PG_te,BABA_te,KO_te,JNJ_te,GLD_te,
                                          PEP_te,IEMG_te,portfolio_te,portfolio_adj_te))

names(summary_table_te)<- c("VWO","WMT","PG","BABA",'KO','JNJ',"GLD","PEP",'IEMG',"Portfolio","Adjusted Portfolio")
row.names(summary_table_te) <- c("TE")

#View(summary_table_te)

              
summary_table_main <- rbind(summary_table_main,summary_table_te)
#View(summary_table_main)


## Sharpe Ratio
riskfree <- 0.001

WFC_sharpe <- (mean(joined_monthly_returns$monthly.returns[(time_index-11):time_index])-riskfree)/
  WFC_sigma


VWO_sharpe <- (mean(joined_bridge_monthly_returns$VWO[(time_index-(59)):time_index])-riskfree)/VWO_sigma
WMT_sharpe <- (mean(joined_bridge_monthly_returns$WMT[(time_index-(59)):time_index])-riskfree)/WMT_sigma
PG_sharpe <- (mean(joined_bridge_monthly_returns$PG[(time_index-(59)):time_index])-riskfree)/PG_sigma 
BABA_sharpe <- (mean(joined_bridge_monthly_returns$BABA[(time_index-(59)):time_index])-riskfree)/BABA_sigma
KO_sharpe <- (mean(joined_bridge_monthly_returns$KO[(time_index-(59)):time_index])-riskfree)/KO_sigma 
JNJ_sharpe <- (mean(joined_bridge_monthly_returns$JNJ[(time_index-(59)):time_index])-riskfree)/JNJ_sigma
GLD_sharpe <- (mean(joined_bridge_monthly_returns$GLD[(time_index-(59)):time_index])-riskfree)/GLD_sigma
PEP_sharpe <- (mean(joined_bridge_monthly_returns$PEP[(time_index-(59)):time_index])-riskfree)/PEP_sigma 
IEMG_sharpe <- (mean(joined_bridge_monthly_returns$IEMG[(time_index-(59)):time_index])-riskfree)/IEMG_sigma
portfolio_sharpe <- (mean(joined_bridge_monthly_returns$Portfolio[(time_index-(59)):time_index])-riskfree)/portfolio_sigma
portfolio_adj_sharpe <- (mean(joined_bridge_monthly_returns_adj$portfolio_adj[(time_index-(59)):time_index])-riskfree)/portfolio_adj_sigma


summary_table_sharpe <- as.data.frame(cbind(VWO_sharpe,WMT_sharpe,PG_sharpe,BABA_sharpe,
                                            KO_sharpe,JNJ_sharpe,GLD_sharpe,PEP_sharpe,
                                            IEMG_sharpe,portfolio_sharpe,portfolio_adj_sharpe))


names(summary_table_sharpe)<- c("VWO","WMT","PG","BABA",'KO','JNJ',"GLD","PEP",'IEMG',"Portfolio","Adjusted Portfolio")
row.names(summary_table_sharpe) <- c("Sharpe")

#View(summary_table_sharpe)

summary_table_main <- rbind(summary_table_main,summary_table_sharpe)
#View(summary_table_main)


#Treynor Ratio



VWO_tr <- (mean(joined_bridge_monthly_returns$VWO[(time_index-(59)):time_index])-riskfree)/VWO_reg$coefficients[2]
WMT_tr <- (mean(joined_bridge_monthly_returns$WMT[(time_index-(59)):time_index])-riskfree)/WMT_reg$coefficients[2]
PG_tr <- (mean(joined_bridge_monthly_returns$PG[(time_index-(59)):time_index])-riskfree)/PG_reg$coefficients[2]
BABA_tr <- (mean(joined_bridge_monthly_returns$BABA[(time_index-(59)):time_index])-riskfree)/BABA_reg$coefficients[2]
KO_tr <- (mean(joined_bridge_monthly_returns$KO[(time_index-(59)):time_index])-riskfree)/KO_reg$coefficients[2]
JNJ_tr <- (mean(joined_bridge_monthly_returns$JNJ[(time_index-(59)):time_index])-riskfree)/JNJ_reg$coefficients[2]
GLD_tr <- (mean(joined_bridge_monthly_returns$GLD[(time_index-(59)):time_index])-riskfree)/GLD_reg$coefficients[2]
PEP_tr <- (mean(joined_bridge_monthly_returns$PEP[(time_index-(59)):time_index])-riskfree)/PEP_reg$coefficients[2] 
IEMG_tr <- (mean(joined_bridge_monthly_returns$IEMG[(time_index-(59)):time_index])-riskfree)/IEMG_reg$coefficients[2]
portfolio_tr <- (mean(joined_bridge_monthly_returns$Portfolio[(time_index-(59)):time_index])-riskfree)/portfolio_reg$coefficients[2]
portfolio_adj_tr <- (mean(joined_bridge_monthly_returns_adj$portfolio_adj[(time_index-(59)):time_index])-riskfree)/portfolio_adj_reg$coefficients[2]

summary_table_tr <- as.data.frame(cbind(VWO_tr,WMT_tr,PG_tr,BABA_tr,
                                            KO_tr,JNJ_tr,GLD_tr,PEP_tr,
                                            IEMG_tr,portfolio_tr,portfolio_adj_tr))


names(summary_table_tr)<- c("VWO","WMT","PG","BABA",'KO','JNJ',"GLD","PEP",'IEMG',"Portfolio","Adjusted Portfolio")
row.names(summary_table_tr) <- c("Treynor")

#View(summary_table_tr)

summary_table_main <- rbind(summary_table_main,summary_table_tr)

#View(summary_table_main)

#print(VWO_reg)

SPY_annual_return <- annualReturn(getSymbols("SPY", auto.assign=F))
VWO_annual_return <- annualReturn(getSymbols("VWO", auto.assign=F))
WMT_annual_return <- annualReturn(getSymbols("WMT", auto.assign=F))
PG_annual_return <- annualReturn(getSymbols("PG", auto.assign=F))
BABA_annual_return <-annualReturn(getSymbols("BABA", auto.assign=F))
KO_annual_return <- annualReturn(getSymbols("KO", auto.assign=F))
JNJ_annual_return <-annualReturn(getSymbols("JNJ", auto.assign=F))
GLD_annual_return <- annualReturn(getSymbols("GLD", auto.assign=F))
PEP_annual_return <- annualReturn(getSymbols("PEP", auto.assign=F))
IEMG_annual_return <- annualReturn(getSymbols("IEMG", auto.assign=F))
portfolio_annual_return <- annualReturn(getSymbols("", auto.assign=F))



portfolio_5yr_return <- mean(mean(SPY_annual_return[10:14,])*bridgewater_combined_value[1,2]+
                              mean(VWO_annual_return[10:14,])*bridgewater_combined_value[2,2]+
                              mean(WMT_annual_return[10:14,])*bridgewater_combined_value[3,2]+
                              mean(PG_annual_return[10:14,])*bridgewater_combined_value[4,2]+
                              mean(BABA_annual_return[3:7,])*bridgewater_combined_value[5,2]+
                              mean(KO_annual_return[10:14,])*bridgewater_combined_value[6,2]+
                              mean(JNJ_annual_return[10:14,])*bridgewater_combined_value[7,2]+
                              mean(GLD_annual_return[10:14,])*bridgewater_combined_value[8,2]+
                              mean(PEP_annual_return[10:14,])*bridgewater_combined_value[9,2]+
                              mean(IEMG_annual_return[5:9,])*bridgewater_combined_value[10,2])

portfolio_adj_5yr_return <- mean(mean(SPY_annual_return[10:14,])*bridgewater_combined_value_adj[1,2]+
                               mean(VWO_annual_return[10:14,])*bridgewater_combined_value_adj[2,2]+
                               mean(WMT_annual_return[10:14,])*bridgewater_combined_value_adj[3,2]+
                               mean(PG_annual_return[10:14,])*bridgewater_combined_value_adj[4,2]+
                               mean(KO_annual_return[10:14,])*bridgewater_combined_value_adj[5,2]+
                               mean(JNJ_annual_return[10:14,])*bridgewater_combined_value_adj[6,2]+
                               mean(GLD_annual_return[10:14,])*bridgewater_combined_value_adj[7,2]+
                               mean(PEP_annual_return[10:14,])*bridgewater_combined_value_adj[8,2]+
                               mean(IEMG_annual_return[5:9,])*bridgewater_combined_value_adj[9,2])

portfolio_annualized_return <- (SPY_annual_return[10:14,]*bridgewater_combined_value[1,2]+
                               VWO_annual_return[10:14,]*bridgewater_combined_value[2,2]+
                               WMT_annual_return[10:14,]*bridgewater_combined_value[3,2]+
                               PG_annual_return[10:14,]*bridgewater_combined_value[4,2]+
                               BABA_annual_return[3:7,]*bridgewater_combined_value[5,2]+
                               KO_annual_return[10:14,]*bridgewater_combined_value[6,2]+
                               JNJ_annual_return[10:14,]*bridgewater_combined_value[7,2]+
                               GLD_annual_return[10:14,]*bridgewater_combined_value[8,2]+
                               PEP_annual_return[10:14,]*bridgewater_combined_value[9,2]+
                               IEMG_annual_return[5:9,]*bridgewater_combined_value[10,2])

portfolio_adj_annualized_return <- (SPY_annual_return[10:14,]*bridgewater_combined_value_adj[1,2]+
                                  VWO_annual_return[10:14,]*bridgewater_combined_value_adj[2,2]+
                                  WMT_annual_return[10:14,]*bridgewater_combined_value_adj[3,2]+
                                  PG_annual_return[10:14,]*bridgewater_combined_value_adj[4,2]+
                                  KO_annual_return[10:14,]*bridgewater_combined_value_adj[5,2]+
                                  JNJ_annual_return[10:14,]*bridgewater_combined_value_adj[6,2]+
                                  GLD_annual_return[10:14,]*bridgewater_combined_value_adj[7,2]+
                                  PEP_annual_return[10:14,]*bridgewater_combined_value_adj[8,2]+
                                  IEMG_annual_return[5:9,]*bridgewater_combined_value_adj[9,2])

#View(portfolio_adj_annualized_return)

portfolio_annualized_return$V2 <- c("2016-12-30","2017-12-29","2018-12-31","2019-12-31","2020-12-31")


 
ggplot(portfolio_annualized_return, aes(x=V2, y=yearly.returns))+geom_line()

barplot(portfolio_annualized_return, main="Portfolio Annualized Returns", 
        xlab="Time")

summary_table_annual_return <- as.data.frame(cbind(mean(VWO_annual_return[10:14,]),mean(WMT_annual_return[10:14,]),
                                                   mean(PG_annual_return[10:14,]),mean(BABA_annual_return[3:7,]),
                                                   mean(KO_annual_return[10:14,]),mean(JNJ_annual_return[10:14,]),
                                                   mean(GLD_annual_return[10:14,]),mean(PEP_annual_return[10:14,]),
                                                   mean(IEMG_annual_return[5:9,]),portfolio_5yr_return,portfolio_adj_5yr_return))


names(summary_table_annual_return)<- c("VWO","WMT","PG","BABA",'KO','JNJ',"GLD","PEP",'IEMG',"Portfolio","Adjusted Portfolio")
row.names(summary_table_annual_return) <- c("5 year annual return")

summary_table_main <- rbind(summary_table_main,summary_table_annual_return)
summary_table_main <- round(summary_table_main,4)
View(summary_table_main)




