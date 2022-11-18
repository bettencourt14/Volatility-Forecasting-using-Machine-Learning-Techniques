
source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/BVSP Preliminary Analysis.R")

#BVSP Monte Carlo Simulation 

nsim=nrow(BVSP2)+1
BVSP1_daily_return<- BVSP1_return_annualized/100/252
BVSP1_daily_volatility<-BVSP1_volatility_annualized/100/sqrt(252)
BVSP1_T<- nrow(BVSP2)+1
BVSP1_T
BVSP1_S0=as.numeric(BVSP1[nrow(BVSP1),2])
BVSP1_S0

mc_function=function(nsim,N,daily_mean,daily_sd,S0)
{
  Z=rnorm(nsim,0,1)
  WT=sqrt(N)*Z
  ST=S0*exp((daily_mean-0.5*daily_sd^2)*N+daily_sd*WT)
  
  
  output_list=list(ST=ST)
  output_list
}

set.seed(994)

for (i in 1:1000)
{
  a <- mc_function(nsim =nsim,N=BVSP1_T,daily_mean=BVSP1_daily_return,
                   daily_sd = BVSP1_daily_volatility, S0 = BVSP1_S0)
  a <- as.data.frame(a)
  if(i==1)
  {
    BVSP1_MC_results <- a 
  }
  else {
    BVSP1_MC_results <- cbind(BVSP1_MC_results,a)
  }
  
}


BVSP1_MC_results[1,]<- BVSP1_S0
BVSP1_MC_results

for (i in 1:ncol(BVSP1_MC_results)){
  colnames(BVSP1_MC_results)[i] <- paste0('Sim ',i)
}

for(i in 1:ncol(BVSP1_MC_results))
{
  b <- diff(log(BVSP1_MC_results[,i]))
  b <- as.data.frame(b)
  if(i==1)
  {
    BVSP1_MC_returns <- b
  }
  else {
    BVSP1_MC_returns <- cbind(BVSP1_MC_returns,b)
  }
  
}

for (i in 1:ncol(BVSP1_MC_returns)){
  colnames(BVSP1_MC_returns)[i] <- paste0('Sim ',i)
}


print(BVSP1_MC_returns)
BVSP1_MC_returns<- na.omit(BVSP1_MC_returns)

BVSP2_actual_prices<-c(BVSP1_S0,BVSP2$Close)
BVSP2_actual_prices
BVSP2_actual_returns<-diff(log(BVSP2_actual_prices))
BVSP2_actual_returns<- as.data.frame(BVSP2_actual_returns)
dim(BVSP2_actual_returns)

### Error for each day of the model by assuming daily price as correct one

BVSP2_daily_error_aux<- BVSP1_MC_returns
for (i in 1:ncol(BVSP2_daily_error_aux)) {
  BVSP2_daily_error_aux[,i]<-BVSP2_actual_returns
}
BVSP2_daily_error_aux<-as.data.frame(BVSP2_daily_error_aux)

for (i in 1:ncol(BVSP2_daily_error_aux)) {
  d<- abs(BVSP1_MC_returns[,i]-BVSP2_daily_error_aux[,i])
  d<- as.data.frame(d)
  
  if (i==1) {
    BVSP_error<-d
    
  }
  else{
    BVSP_error<-cbind(BVSP_error,d)
  }
}

BVSP_error    #use this for Error measurmments

for (i in 1:ncol(BVSP_error)){
  colnames(BVSP_error)[i] <- paste0('Sim ',i)
}

BVSP_error<-as.data.frame(BVSP_error)
BVSP_error_means<-rowMeans(BVSP_error)
BVSP2_actual_returns
BVSP2_print<-cbind(BVSP2_actual_returns,BVSP_error_means)
BVSP2_print
hist(BVSP2_actual_returns$BVSP2_actual_returns,col='light grey',
     breaks=seq(from=-0.2, to=0.2,by=0.025),
     xlab= "Daily Returns",main="Actual Returns of IBOVESPA for 21-02-2022 until 04-03-2022",
     las=1,ylim=c(0,8))
plot(BVSP2_actual_returns$BVSP2_actual_returns~BVSP2$Date,type='l',col='darkblue',
     ylim=c(-0.15,0.15), 
     xlab="Simulated Days",ylab="Return",lwd=1, main='IBOVESPA
     Actual vs. Monte Carlo Forecast')
lines(BVSP_error_means~BVSP2$Date,type='l',col='darkorange')
legend("bottomright",
       legend = c('Actual Returns','Forecasted Returns'),
       col = c('darkblue','darkorange'),
       pch = c(9,9))

##  Absolute difference between the BVSP2_Actual_Returns and BVSP_error_menans
for (i in 1:nrow(as.data.frame(BVSP_error_means))) {

  e<- abs(BVSP2_print[i,1]-BVSP2_print[i,2])
  e<-as.data.frame(e)
  
  if (i==1) {
  BVSP_abs_error<-e  
  }
  
  else{
    BVSP_abs_error<- rbind(BVSP_abs_error,e)
  }
}
e
BVSP_abs_error

plot(BVSP_abs_error$e~BVSP2$Date,xlab='Days',ylab='Difference to actual', main='IBOVESPA Monte Carlo Innefiency',
     type='l',lwd=2)

## Absolute squared error

for (i in 1:nrow(as.data.frame(BVSP_error_means))) {
  
  g<- abs(BVSP2_print[i,1]-BVSP2_print[i,2])**2
  g<-as.data.frame(g)
  
  if (i==1) {
  BVSP_mean_error<-g  
  }
  else{
  BVSP_mean_error<-rbind(BVSP_mean_error,g)
  }
}
g
BVSP_mean_error
# Mean Errors
BVSP_MC_absoulte_error<- sum(BVSP_abs_error$e)/nrow(BVSP_abs_error)
BVSP_MC_squared_mean_error<- sum(BVSP_mean_error$g)/nrow(BVSP_mean_error)
BVSP_MC_root_mean_error<-sqrt(BVSP_MC_squared_mean_error)


