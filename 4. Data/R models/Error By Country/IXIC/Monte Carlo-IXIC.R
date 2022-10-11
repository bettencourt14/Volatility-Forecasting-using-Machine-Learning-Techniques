
source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/IXIC Preliminary Analysis.R")

#IXIC Monte Carlo Simulation 

nsim=nrow(IXIC2)+1
IXIC1_daily_return<- IXIC1_return_annualized/100/252
IXIC1_daily_volatility<-IXIC1_volatility_annualized/100/sqrt(252)
IXIC1_T<- nrow(IXIC2)+1
IXIC1_T
IXIC1_S0=as.numeric(IXIC1[nrow(IXIC1),2])
IXIC1_S0

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
  a <- mc_function(nsim =nsim,N=IXIC1_T,daily_mean=IXIC1_daily_return,
                   daily_sd = IXIC1_daily_volatility, S0 = IXIC1_S0)
  a <- as.data.frame(a)
  if(i==1)
  {
    IXIC1_MC_results <- a 
  }
  else {
    IXIC1_MC_results <- cbind(IXIC1_MC_results,a)
  }
  
}


IXIC1_MC_results[1,]<- IXIC1_S0
IXIC1_MC_results

for (i in 1:ncol(IXIC1_MC_results)){
  colnames(IXIC1_MC_results)[i] <- paste0('Sim ',i)
}

for(i in 1:ncol(IXIC1_MC_results))
{
  b <- diff(log(IXIC1_MC_results[,i]))
  b <- as.data.frame(b)
  if(i==1)
  {
    IXIC1_MC_returns <- b
  }
  else {
    IXIC1_MC_returns <- cbind(IXIC1_MC_returns,b)
  }
  
}

for (i in 1:ncol(IXIC1_MC_returns)){
  colnames(IXIC1_MC_returns)[i] <- paste0('Sim ',i)
}


print(IXIC1_MC_returns)
IXIC1_MC_returns<- na.omit(IXIC1_MC_returns)

IXIC2_actual_prices<-c(IXIC1_S0,IXIC2$Close)
IXIC2_actual_prices
IXIC2_actual_returns<-diff(log(IXIC2_actual_prices))
IXIC2_actual_returns<- as.data.frame(IXIC2_actual_returns)
dim(IXIC2_actual_returns)

### Error for each day of the model by assuming daily price as correct one

IXIC2_daily_error_aux<- IXIC1_MC_returns
for (i in 1:ncol(IXIC2_daily_error_aux)) {
  IXIC2_daily_error_aux[,i]<-IXIC2_actual_returns
}
IXIC2_daily_error_aux<-as.data.frame(IXIC2_daily_error_aux)

for (i in 1:ncol(IXIC2_daily_error_aux)) {
  d<- abs(IXIC1_MC_returns[,i]-IXIC2_daily_error_aux[,i])
  d<- as.data.frame(d)
  
  if (i==1) {
    IXIC_error<-d
    
  }
  else{
    IXIC_error<-cbind(IXIC_error,d)
  }
}

IXIC_error    #use this for Error measurmments

for (i in 1:ncol(IXIC_error)){
  colnames(IXIC_error)[i] <- paste0('Sim ',i)
}

IXIC_error<-as.data.frame(IXIC_error)
IXIC_error_means<-rowMeans(IXIC_error)
IXIC2_actual_returns
IXIC2_print<-cbind(IXIC2_actual_returns,IXIC_error_means)
IXIC2_print
hist(IXIC2_actual_returns$IXIC2_actual_returns,col='light grey',
     breaks=seq(from=-0.2, to=0.2,by=0.025),
     xlab= "Daily Returns",main="Actual Returns of NASDAQ for 21-02-2022 until 04-03-2022",
     las=1,ylim=c(0,8))
plot(IXIC2_actual_returns$IXIC2_actual_returns~IXIC2$Date,type='l',col='darkblue',
     ylim=c(-0.15,0.15), 
     xlab="Simulated Days",ylab="Return",lwd=1, main='NASDAQ
     Actual vs. Monte Carlo Forecast')
lines(IXIC_error_means~IXIC2$Date,type='l',col='darkorange')
legend("bottomright",
       legend = c('Actual Returns','Forecasted Returns'),
       col = c('darkblue','darkorange'),
       pch = c(9,9))

##  Absolute difference between the IXIC2_Actual_Returns and IXIC_error_menans
for (i in 1:nrow(as.data.frame(IXIC_error_means))) {

  e<- abs(IXIC2_print[i,1]-IXIC2_print[i,2])
  e<-as.data.frame(e)
  
  if (i==1) {
  IXIC_abs_error<-e  
  }
  
  else{
    IXIC_abs_error<- rbind(IXIC_abs_error,e)
  }
}
e
IXIC_abs_error

plot(IXIC_abs_error$e~IXIC2$Date,xlab='Days',ylab='Difference to actual', main='NASDAQ Monte Carlo Innefiency',
     type='l',lwd=2)

## Absolute squared error

for (i in 1:nrow(as.data.frame(IXIC_error_means))) {
  
  g<- abs(IXIC2_print[i,1]-IXIC2_print[i,2])**2
  g<-as.data.frame(g)
  
  if (i==1) {
  IXIC_mean_error<-g  
  }
  else{
  IXIC_mean_error<-rbind(IXIC_mean_error,g)
  }
}
g
IXIC_mean_error
# Mean Errors
IXIC_MC_absoulte_error<- sum(IXIC_abs_error$e)/nrow(IXIC_abs_error)
IXIC_MC_squared_mean_error<- sum(IXIC_mean_error$g)/nrow(IXIC_mean_error)
IXIC_MC_root_mean_error<-sqrt(IXIC_MC_squared_mean_error)


