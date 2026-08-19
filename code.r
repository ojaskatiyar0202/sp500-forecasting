install.packages("languageserver")
install.packages(c("tidyverse","quantmod"))
install.packages("reshape2")
library(eodhdR2)
library(quantmod)
library(readr)
library(dplyr)

stock.names<-c( 'AAPL', 'TSLA', 'V', 'MSFT', 'JNJ', 'AMZN', 'JPM', 'AVGO', 'MA', 'NVDA',"SP500")

g<-c("^GSPC", 'AAPL', 'TSLA', 'V', 'MSFT', 'JNJ', 'AMZN', 'JPM', 'AVGO', 'MA', 'NVDA')
getSymbols(g,from="2020-11-25",to="2025-11-30")
log_prices<-merge(log(AAPL$AAPL.Adjusted), log(TSLA$TSLA.Adjusted), 
                  log(V$V.Adjusted), log(MSFT$MSFT.Adjusted),log(JNJ$JNJ.Adjusted), log(AMZN$AMZN.Adjusted),log(JPM$JPM.Adjusted),log(AVGO$AVGO.Adjusted),log(MA$MA.Adjusted),log(NVDA$NVDA.Adjusted), log(GSPC$GSPC.Adjusted))

ts.plot(log_prices, col=1:11,main="S&P 500")

legend("bottomright",
       legend = stock.names,
       col = 1:11, text.font=4, cex=.7, lty=1,)



#x<-diff(AAPL$AAPL.Adjusted)
#x=x[2:nrow(x),]
#View(x)
#View(returns)
#n = length(x) 
#sigma2 = x^2
#product = 1

#for (t in 2:n){
#  sigma2[t] = (1-lambda)*x[t-1]^2 + lambda*sigma2[t-1]
#  product = product * 1/(2*\pi*sigma2[t])^.5*\exp(-x[t]^2/(2*sigma2[t]))
#}
#View(AAPL)



stock.names.2<-c( "SP500", 'AAPL', 'TSLA', 'V', 'MSFT', 'JNJ', 'AMZN', 'JPM', 'AVGO', 'MA', 'NVDA')
r<-merge(diff(GSPC$GSPC.Adjusted),diff(AAPL$AAPL.Adjusted), diff(TSLA$TSLA.Adjusted), diff(V$V.Adjusted), diff(MSFT$MSFT.Adjusted),diff(JNJ$JNJ.Adjusted), diff(AMZN$AMZN.Adjusted),diff(JPM$JPM.Adjusted),diff(AVGO$AVGO.Adjusted),diff(MA$MA.Adjusted),diff(NVDA$NVDA.Adjusted))

r=r[-1]
colnames(r)<-stock.names.2
dim(r)

r.mtx = data.matrix(as.data.frame(r))
r.mtx <- t(r.mtx) #this transpose is important as our function pred.r.prepare will not work 
                  #if the data's dimension is incorrect we require stocks as rows 
r.mtx

pred.r.prepare <- function(max.lag = 5, split = c(50, 25), mask = rep(1, 10)) {
  
  # this function prepares the data for the prediction exercise and splits them into a train, validation and test sets
  # max.lag - the maximum lag to include in the prediction
  # split - how much of the data (in percentage terms) to include in the training and validation sets, respectively
  # mask - which other indices to include (1 for yes, 0 for no)
  r.mtx <- as.matrix(r.mtx)
  
  d <- dim(r.mtx)
  
  start.index <- max(3, max.lag + 1)
  
  y <- matrix(0, d[2] - start.index + 1, 1)
  
  x <- matrix(0, d[2] - start.index + 1, d[1] - 1 + max.lag)
  
  y[,1] <- r.mtx[1,start.index:d[2]]
  
  for (i in 1:max.lag) {
    
    x[,i] <- r.mtx[1,(start.index-i):(d[2]-i)]
    
  }
  
  shift.indices <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)  ## As the time-zones are the same the shift indices are all 0 so we can look data at time t
  
  for (i in 2:(d[1])) {
    x[,i+max.lag-1] <- r.mtx[i,(start.index-1-shift.indices[i-1]):(d[2]-1-shift.indices[i-1])]
    
  }
  
  
  end.training <- round(split[1] / 100 * d[2])
  
  end.validation <- round(sum(split[1:2]) / 100 * d[2])
  
  x <- x[,as.logical(c(rep(1, max.lag), mask))]
  
  y.train <- as.matrix(y[1:end.training], end.training, 1)
  x.train <- x[1:end.training,]
  
  y.valid <- as.matrix(y[(end.training+1):(end.validation)], end.validation-end.training, 1)
  x.valid <- x[(end.training+1):(end.validation),]
  
  y.test <- as.matrix(y[(end.validation+1):(d[2] - start.index + 1)], d[2]-start.index-end.validation+1, 1)
  x.test <- x[(end.validation+1):(d[2] - start.index + 1),]
  
  list(x=x, y=y, x.train=x.train, y.train=y.train, x.valid=x.valid, y.valid=y.valid, x.test=x.test, y.test=y.test)
  
}
#data<-pred.r.prepare()
w<-pred.r.prepare(max.lag=1)
w$x
View(r$`^GSPC`)
n = length(r$AAPL) 

## x is the return data, sigma2 is sigma-squared ##
View(r$AAPL)
x=r$AAPL
length(x)
r$AAPL[1]
r$AAPL[2]




log.lik.fcn <- function(lambda,x) {
  x<-as.numeric(x)
  n = length(x); sigma2 = x^2; loglik = 0
  for (t in 2:n){
    
    sigma2[t] = (1-lambda)*x[t-1]^2 + lambda*sigma2[t-1]
    loglik = loglik + (log(1/(2*pi*sigma2[t])^.5)+(-x[t]^2/(2*sigma2[t])))
    
  }
  return(-loglik)
}




r_df<-as.data.frame(r)
colnames(r_df)

g<-pred.r.prepare(max.lag=1)
g$x.train
colnames(r)
f<-c("AAPL")
print(stock.names)
mle_SP500<-optim( par=.93, fn = log.lik.fcn, x = g$x.train[,1], method = "L-BFGS-B", lower = 0.0001,upper = 0.9999)
mle_AAPL<-optim( par=.93, fn = log.lik.fcn, x = r_df$AAPL, method = "L-BFGS-B", lower = 0.0001,upper = 0.9999)
mle_TSLA<-optim( par=.93, fn = log.lik.fcn, x = r_df$TSLA, method = "L-BFGS-B", lower = 0.0001,upper = 0.9999)
mle_V<-optim( par=.93, fn = log.lik.fcn, x = r_df$V, method = "L-BFGS-B", lower = 0.0001,upper = 0.9999)
mle_MSFT<-optim( par=.93, fn = log.lik.fcn, x = r_df$MSFT, method = "L-BFGS-B", lower = 0.0001,upper = 0.9999)
mle_JNJ<-optim( par=.93, fn = log.lik.fcn, x = r_df$JNJ, method = "L-BFGS-B", lower = 0.0001,upper = 0.9999)
mle_AMZN<-optim( par=.93, fn = log.lik.fcn, x = r_df$AMZN, method = "L-BFGS-B", lower = 0.0001,upper = 0.9999)
mle_JPM<-optim( par=.93, fn = log.lik.fcn, x = r_df$JPM, method = "L-BFGS-B", lower = 0.0001,upper = 0.9999)
mle_AVGO<-optim( par=.93, fn = log.lik.fcn, x = r_df$AVGO, method = "L-BFGS-B", lower = 0.0001,upper = 0.9999)
mle_MA<-optim( par=.93, fn = log.lik.fcn, x = r_df$MA, method = "L-BFGS-B", lower = 0.0001,upper = 0.9999)
mle_NVDA<-optim( par=.93, fn = log.lik.fcn, x = r_df$NVDA, method = "L-BFGS-B", lower = 0.0001,upper = 0.9999)

dim(g$x.train)
g$x.train

mle.train <- function(x){
  mles <- numeric(ncol(x$x.train))
  
    for (i in 1:ncol(x$x.train)){
    v<-optim( par=.93, fn = log.lik.fcn, x = x$x.train[,i], method = "L-BFGS-B", lower = 0.0001,upper = 0.9999)
    mles[i]<-v$par

    }
  
  return(mles)
}

#the function below provides us with both daily volatilities and ex so is exponenitially smoothened
vol.exp.sm <- function(x, lambda) {
  
  # Exponential smoothing of x^2 with parameter lambda
  
  sigma2 <- x^2
  n <- length(x)
  
  for (i in 2:n)
    sigma2[i] <- sigma2[i-1] * lambda + x[i-1]^2 * (1-lambda)
  
  sigma <- sqrt(sigma2)
  
  resid <- x/sigma
  resid[is.na(resid)] <- 0
  sq.resid <- resid^2
  
  list(sigma2=sigma2, sigma=sigma, resid = resid, sq.resid = sq.resid)
  
}
x<-pred.r.prepare(1)
mle.train(x)
mles[1]
y<-vol.exp.sm(x=x$x.train[,1],lambda = mles[1])
y$resid

data$x.train[,1]

std_sd_SP500<- vol.exp.sm(x=data$x.train,lambda = mle_SP500$par)
std_sd_AAPL<- vol.exp.sm(x=r$AAPL,lambda = mle_AAPL$par)
std_sd_TSLA<- vol.exp.sm(x=r$TSLA,lambda = mle_TSLA$par)
std_sd_V<- vol.exp.sm(x=r$V,lambda = mle_V$par)
std_sd_MSFT<- vol.exp.sm(x=r$MSFT,lambda = mle_MSFT$par)
std_sd_JNJ<- vol.exp.sm(x=r$JNJ,lambda = mle_JNJ$par)
std_sd_AMZN<- vol.exp.sm(x=r$AMZN,lambda = mle_AMZN$par)
std_sd_JPM<- vol.exp.sm(x=r$JPM,lambda = mle_JPM$par)
std_sd_AVGO<- vol.exp.sm(x=r$AVGO,lambda = mle_AVGO$par)
std_sd_MA<- vol.exp.sm(x=r$MA,lambda = mle_MA$par)
std_sd_NVDA<- vol.exp.sm(x=r$NVDA,lambda = mle_NVDA$par)

std_sd_SP500
indices<-1:10
x<-pred.r.prepare()
x$y.train
s<-optim( par=.93, fn = log.lik.fcn, x = x$y.train[,1], method = "L-BFGS-B", lower = 0.0001,upper = 0.9999)
mle_y<-s$par

mles<-mle.train(x)
mles[2]
n
norm.data <- function(x){
  #x is the data returned by pred.r.prepare()
  
  
  mles<-mle.train(x)
  
  x.train.dev <- x$x.train
  y.train.dev <- x$y.train
  
  x.valid.dev <- x$x.valid
  y.valid.dev <- x$y.valid
  
  x.test.dev <- x$x.test
  y.test.dev <- x$y.test
  
  for (i in 1:ncol(x$x.train)) {
    v1 <- vol.exp.sm(x = x.train.dev[,i ], lambda = mles[i])
    x.train.dev[, i] <- v1$resid
    v2<- vol.exp.sm(x = x.valid.dev[,i ], lambda = mles[i])
    x.valid.dev[, i] <- v2$resid
    v3 <- vol.exp.sm(x = x.test.dev[,i], lambda = mles[i])
    x.test.dev[, i] <- v3$resid
  }

  v <- vol.exp.sm(x$y.train, lambda= mle_y)
  y.train.dev <- v$resid
  
  v <- vol.exp.sm(x$y.valid, lambda= mle_y)
  y.valid.dev <- v$resid
  
  v <- vol.exp.sm(x$y.test,lambda= mle_y)
  y.test.dev <- v$resid
  
  
  list(y.train.dev=y.train.dev, x.train.dev=x.train.dev, y.valid.dev=y.valid.dev, x.valid.dev=x.valid.dev, y.test.dev=y.test.dev, x.test.dev=x.test.dev)
}

w<-pred.r.prepare(max.lag=4)
f<-norm.data.n(w)
f$x.train.dev




thresh.reg <- function(x, y, th, x.pred = NULL) {
  
  # estimation of beta in y = a + x beta + epsilon (linear regression)
  # but only using those covariates in x whose marginal correlation
  # with y exceeds th
  # use th = 0 for full regression
  # note the intercept is added
  # x.pred is a new x for which we wish to make prediction
  
  d <- dim(x)
  
  ind <- (abs(cor(x, y)) > th)
  n <- sum(ind)
  
  new.x <- matrix(c(rep(1, d[1]), x[,ind]), d[1], n+1)    ## Adding intercept term 
  
  gram = t(new.x) %*% new.x
  
  beta <- solve(gram) %*% t(new.x) %*% matrix(y, d[1], 1)
  
  ind.ex <- c(1, as.numeric(ind))
  
  ind.ex[ind.ex == 1] <- beta
  
  condnum = max(svd(gram)$d)/min(svd(gram)$d)
  
  pr <- 0
  
  if (!is.null(x.pred)) pr <- sum(ind.ex * c(1, x.pred))
  
  list(beta = ind.ex, pr=pr, condnum = condnum)
  
}





rolling.thresh.reg <- function(x, th, win, warmup, reg.function = thresh.reg) {

	# performs prediction over a rolling window of size win
	# over the training set
	# x - returned by pred.footsie.prepare
	# lambda - parameter for exponential smoothing
	# th - threshold for thresh.reg
	# warmup - t_0 from the lecture notes


	xx <- norm.data(x)

	n <- length(xx$y.train.dev)

	err <- 0

	condnum <- predi <- truth <- rep(0, n-warmup+1)

	for (i in warmup:n) {

		y <- xx$y.train.dev[(i-win):(i-1)]
		xxx <- xx$x.train.dev[(i-win):(i-1),]

		zz <- reg.function(xxx, y, th, xx$x.train.dev[i,])

		predi[i-warmup+1] <- zz$pr
              condnum[i-warmup+1] <- zz$condnum
		truth[i-warmup+1] <- xx$y.train.dev[i]

	}
	
	
	i_s<-investment_strategy <- if_else (predi>0, 1, -1)
	ret <- i_s*truth
	
	err <- sqrt(250) * mean(ret) / sqrt(var(ret))
	
	list(err=err, predi=predi, truth=truth, condnum=condnum, ret=ret)
	

}
x<-pred.r.prepare(5)
l<-rolling.thresh.reg(x, 0, 250, 250, reg.function = thresh.reg)
l$err



rolling.thresh.reg.valid <- function(x, th, win, warmup, reg.function = thresh.reg) {
  
  # The same as the previous function but for the validation set
  
  xx <- norm.data(x)
  
  n <- length(xx$y.valid.dev)
  
  err <- 0
  
  condnum <- predi <- truth <- rep(0, n-warmup+1)
  
  for (i in warmup:n) {
    
    y <- xx$y.valid.dev[(i-win):(i-1)]
    xxx <- xx$x.valid.dev[(i-win):(i-1),]
    
    zz <- reg.function(xxx, y, th, xx$x.valid.dev[i,])
    
    predi[i-warmup+1] <- zz$pr
    condnum[i-warmup+1] <- zz$condnum
    truth[i-warmup+1] <- xx$y.valid.dev[i]
    
  }
  
  
  i_s<-investment_strategy <- if_else (predi>0, 1, -1)
  ret <- i_s*truth
  
  err <- sqrt(250) * mean(ret) / sqrt(var(ret))
  
  list(err=err, predi=predi, truth=truth, condnum=condnum, ret=ret)
  
}


rolling.thresh.reg.test <- function(x, th, win, warmup, reg.function = thresh.reg) {
  
  # The same as the previous function but for the test set
  
  xx <- norm.data(x)
  
  n <- length(xx$y.test.dev)
  
  err <- 0
  
  condnum <- predi <- truth <- rep(0, n-warmup+1)
  
  for (i in warmup:n) {
    
    y <- xx$y.test.dev[(i-win):(i-1)]
    xxx <- xx$x.test.dev[(i-win):(i-1),]
    zz <- reg.function(xxx, y, th, xx$x.test.dev[i,])
    
    predi[i-warmup+1] <- zz$pr
    condnum[i-warmup+1] <- zz$condnum
    truth[i-warmup+1] <- xx$y.test.dev[i]
    
  }
  
  
  i_s<-investment_strategy <- if_else (predi>0, 1, -1)
  ret <- i_s*truth
  
  err <- sqrt(250) * mean(ret) / sqrt(var(ret))
  
  list(err=err, predi=predi, truth=truth, condnum=condnum,ret=ret)
  
}

sharpe.curves <- function(x, th, warmup, reg.function = thresh.reg, win = seq(from = 10, to = warmup, by = 10)) {
  
  # computes Sharpe ratios for a sequence of rolling windows (D in the lecture notes)
  # for the training, validation and test sets
  
  w <- length(win)
  
  train.curve <- valid.curve <- test.curve <- rep(0, w)
  
  n <- length(x$y.train)
  
  i=1
  rreg <- rolling.thresh.reg(x, th, win[i], warmup, reg.function)
  rreg.valid <- rolling.thresh.reg.valid(x, th, win[i], warmup, reg.function)  
  rreg.test <- rolling.thresh.reg.test(x, th, win[i], warmup, reg.function)
  
  condnum = matrix(0,w,length(rreg$condnum))
  condnum.valid = matrix(0,w,length(rreg.valid$condnum))
  condnum.test = matrix(0,w,length(rreg.test$condnum))
  
  train.curve[i] <- rreg$err
  valid.curve[i] <- rreg.valid$err
  test.curve[i] <- rreg.test$err
  
  condnum[i,] <- rreg$condnum
  condnum.valid[i,] <- rreg.valid$condnum
  condnum.test[i,] <- rreg.test$condnum
  
  for (i in 2:w) {
    rreg <- rolling.thresh.reg(x, th, win[i], warmup, reg.function)
    rreg.valid <- rolling.thresh.reg.valid(x, th, win[i], warmup, reg.function)  
    rreg.test <- rolling.thresh.reg.test(x, th, win[i], warmup, reg.function)
    
    train.curve[i] <- rreg$err
    valid.curve[i] <- rreg.valid$err
    test.curve[i] <- rreg.test$err
    
    condnum[i,] <- rreg$condnum
    condnum.valid[i,] <- rreg.valid$condnum
    condnum.test[i,] <- rreg.test$condnum
  }
  
  list(train.curve = train.curve, valid.curve = valid.curve, test.curve = test.curve, condnum=condnum, condnum.valid = condnum.valid, condnum.test = condnum.test)
}

x<-pred.r.prepare()
sc <- sharpe.curves(x=x, 0, 250, win = seq(from = 50, to = 250, by = 20))

sc$train.curve
sc$valid.curve
sc$test.curve




q=0
data = pred.r.prepare(q)
sc0 = sharpe.curves(x=data, th=0,warmup= 250,
                   win = seq(from = 50, to = 250, by = 20))
sc0$train.curve
sc0$valid.curve
sc0$test.curve
vroll = rolling.thresh.reg.valid(x=data, 0, 250, 250)
plot.ts(vroll$predi,main='0 time lag prediction')
##Some positions are simply ridiculous!
##We will not invest like that in real world
vroll$predi
q=1
data = pred.r.prepare(q)
data$x.train
sc1 = sharpe.curves(x=data,  th=0,warmup= 250,
                   win = seq(from = 50, to = 250, by = 20))
sc1$train.curve
sc1$valid.curve
sc1$test.curve
vroll = rolling.thresh.reg.valid(x=data, 0, 250, 250)
plot.ts(vroll$predi,main='1 time lag prediction')
##Some positions are simply ridiculous!
##We will not invest like that in real world


plot.ts(vroll$ret)



#ret <- if_else ((predi * truth )>0, 1, -1)*


rolling.thresh.reg(x=x, th=0,win= 250,warmup= 250) 
x<- pred.r.prepare(max.lag=1)
data.dev<- norm.data(x=x)
train.pca = prcomp(data.dev$x.train.dev)
valid.pca = prcomp(data.dev$x.valid.dev)
test.pca = prcomp(data.dev$x.test.dev)
names(train.pca)
train.pca$rotation

f2<-t(train.pca$rotation[,c(1,2)])%*%t(train.pca$x)

norm.data()


thresh.reg()
fctr.2 <- function(x,warmup, win,reg.function=thresh.reg) {
  xx <- norm.data(x)
  
  n <- length(xx$y.train.dev)
  err <- 0
  
  condnum <- predi <- truth <- rep(0, n-warmup+1)
  
  for (i in warmup:n) {
    
    y <- xx$y.train.dev[(i-win):(i-1)] - mean(xx$y.train.dev[(i-win):(i-1)])
    c1 <- c(xx$x.train.dev[(i-win):(i-1),1] - mean(xx$x.train.dev[(i-win):(i-1),1]))
    c2<- c(xx$x.train.dev[(i-win):(i-1),2] - mean(xx$x.train.dev[(i-win):(i-1),2]))
    xxx<-cbind(c1,c2)
    train.pca <- prcomp(xxx)
    f2<-t(train.pca$rotation[,c(1,2)])%*%t(train.pca$x)
    zz <- reg.function(t(f2), y, th=0, xx$x.train.dev[i,]-mean(xx$x.train.dev[i:n,]))
    
    predi[i-warmup+1] <- zz$pr
    condnum[i-warmup+1] <- zz$condnum
    truth[i-warmup+1] <- xx$y.train.dev[i]
    
  }
  
  i_s<-investment_strategy <- if_else (predi>0, 1, -1)
  ret <- i_s*truth
  
  err <- sqrt(250) * mean(ret) / sqrt(var(ret))
  
  list(err=err, predi=predi, truth=truth, condnum=condnum, ret=ret)
  
}

x<-pred.r.prepare(1)
fctr.2(x,250,50)
train.pca <- prcomp(x$x.train)

names(train.pca)
dim(train.pca$x)
f2<-t(train.pca$rotation[,c(1,2)])%*%t(train.pca$x)
t(f2)
train.pca<
t(train.pca$rotation[,c(1,2)])%*%t(train.pca$x)

f2<-t(train.pca$rotation[,c(1,2)])%*%t(train.pca$x)
f2

fctr.mdl.train <- function(factor, warmup, win,reg.function=thresh.reg) {
  x<- pred.r.prepare(max.lag=1)
  xx <- norm.data(x)
  
  n <- length(xx$y.train.dev)
  err <- 0
  
  condnum <- predi <- truth <- rep(0, n-warmup+1)
  
  for (i in warmup:n) {
    
    y <- xx$y.train.dev[(i-win):(i-1)] - mean(xx$y.train.dev[(i-win):(i-1)])
    c1 <- c(xx$x.train.dev[(i-win):(i-1),1] - mean(xx$x.train.dev[(i-win):(i-1),1]))
    c2<- c(xx$x.train.dev[(i-win):(i-1),2] - mean(xx$x.train.dev[(i-win):(i-1),2]))
    
    
    if (factor==1){
      xxx<-cbind(c1)
      train.pca <- prcomp(xxx)
      f2<-t(train.pca$rotation[,1])%*%t(train.pca$x)
      zz <- reg.function(t(f2), y, th=0, xx$x.train.dev[i]-mean(xx$x.train.dev[i-win:i]))
      
      predi[i-warmup+1] <- zz$pr
      condnum[i-warmup+1] <- zz$condnum
      truth[i-warmup+1] <- xx$y.train.dev[i]-mean(xx$y.train.dev[n])
    }
    
    
    if (factor==2){
      xxx<-cbind(c1,c2)
      train.pca <- prcomp(xxx)
      f2<-t(train.pca$rotation[,c(1,2)])%*%t(train.pca$x)
      zz <- reg.function(t(f2), y, th=0, xx$x.train.dev[i,]-mean(xx$x.train.dev[i-win:i,]))
      
      predi[i-warmup+1] <- zz$pr
      condnum[i-warmup+1] <- zz$condnum
      truth[i-warmup+1] <- xx$y.train.dev[i]-mean(xx$y.train.dev[n])
      }
  }
  i_s<-investment_strategy <- if_else (predi>0, 1, -1)
  ret <- i_s*truth
  
  err <- sqrt(250) * mean(ret) / sqrt(var(ret))
  
  list(err=err, predi=predi, truth=truth, condnum=condnum, ret=ret)
  
}
y<-fctr.mdl.train(factor=2,250,249)
y$predi
y$err
plot.ts(y$predi)
y$err

sharpe.curves.fctr()






fctr.mdl.valid <- function(factor, warmup, win,reg.function=thresh.reg) {
  x<- pred.r.prepare(max.lag=1)
  xx <- norm.data(x)
  
  n <- length(xx$y.valid.dev)
  err <- 0
  
  condnum <- predi <- truth <- rep(0, n-warmup+1)
  
  for (i in warmup:n) {
    
    y <- xx$y.valid.dev[(i-win):(i-1)] - mean(xx$y.valid.dev[(i-win):(n-1)])
    c1 <- c(xx$x.valid.dev[(i-win):(i-1),1] - mean(xx$x.valid.dev[(i-win):(n-1),1]))
    c2<- c(xx$x.valid.dev[(i-win):(i-1),2] - mean(xx$x.valid.dev[(i-win):(n-1),2]))
    
    
    if (factor==1){
      xxx<-cbind(c1)
      valid.pca <- prcomp(xxx)
      f2<-t(valid.pca$rotation[,1])%*%t(valid.pca$x)
      zz <- reg.function(t(f2), y, th=0, xx$x.valid.dev[i]-mean(xx$x.valid.dev[i-win:i]))
      
      predi[i-warmup+1] <- zz$pr
      condnum[i-warmup+1] <- zz$condnum
      truth[i-warmup+1] <- xx$y.valid.dev[i]
    }
    
    
    if (factor==2){
      xxx<-cbind(c1,c2)
      valid.pca <- prcomp(xxx)
      f2<-t(valid.pca$rotation[,c(1,2)])%*%t(valid.pca$x)
      zz <- reg.function(t(f2), y, th=0, xx$x.valid.dev[i,]-mean(xx$x.valid.dev[i-win:i,]))
      
      predi[i-warmup+1] <- zz$pr
      condnum[i-warmup+1] <- zz$condnum
      truth[i-warmup+1] <- xx$y.valid.dev[i]
    }
  }
  i_s<-investment_strategy <- if_else (predi>0, 1, -1)
  ret <- i_s*truth
  
  err <- sqrt(250) * mean(ret) / sqrt(var(ret))
  
  list(err=err, predi=predi, truth=truth, condnum=condnum, ret=ret)
  
}

fctr.mdl.test <- function(factor, warmup, win,reg.function=thresh.reg) {
  x<- pred.r.prepare(max.lag=1)
  xx <- norm.data(x)
  
  n <- length(xx$y.test.dev)
  err <- 0
  
  condnum <- predi <- truth <- rep(0, n-warmup+1)
  
  for (i in warmup:n) {
    
    y <- xx$y.test.dev[(i-win):(i-1)] - mean(xx$y.test.dev[(i-win):(i-1)])
    c1 <- c(xx$x.test.dev[(i-win):(i-1),1] - mean(xx$x.test.dev[(i-win):(i-1),1]))
    c2<- c(xx$x.test.dev[(i-win):(i-1),2] - mean(xx$x.test.dev[(i-win):(i-1),2]))
    
    
    if (factor==1){
      xxx<-cbind(c1)
      test.pca <- prcomp(xxx)
      f2<-t(test.pca$rotation[,1])%*%t(test.pca$x)
      zz <- reg.function(t(f2), y, th=0, xx$x.test.dev[i]-mean(xx$x.test.dev[i-win:i]))
      
      predi[i-warmup+1] <- zz$pr
      condnum[i-warmup+1] <- zz$condnum
      truth[i-warmup+1] <- xx$y.test.dev[i]
    }
    
    
    if (factor==2){
      xxx<-cbind(c1,c2)
      test.pca <- prcomp(xxx)
      f2<-t(test.pca$rotation[,c(1,2)])%*%t(test.pca$x)
      zz <- reg.function(t(f2), y, th=0, xx$x.test.dev[i,]-mean(xx$x.test.dev[i-win:i,]))
      
      predi[i-warmup+1] <- zz$pr
      condnum[i-warmup+1] <- zz$condnum
      truth[i-warmup+1] <- xx$y.test.dev[i]
    }
  }
  i_s<-investment_strategy <- if_else (predi>0, 1, -1)
  ret <- i_s*truth
  
  err <- sqrt(250) * mean(ret) / sqrt(var(ret))
  
  list(err=err, predi=predi, truth=truth, condnum=condnum, ret=ret)
  
}

y<-fctr.mdl.test(factor=1,100,50)
y$err


sharpe.curves.fctr <- function(factor, warmup, reg.function = thresh.reg, win = seq(from = 10, to = warmup, by = 10)) {
  
  # computes Sharpe ratios for a sequence of rolling windows (D in the lecture notes)
  # for the training, validation and test sets
  
  w <- length(win)
  
  train.curve <- valid.curve <- test.curve <- rep(0, w)
  
  n <- length(x$y.train)
  
  i=1
  rreg <- fctr.mdl.train(factor, warmup, win[i], reg.function)
  rreg.valid <- fctr.mdl.valid(factor, warmup, win[i] , reg.function)  
  rreg.test <- fctr.mdl.test(factor, warmup, win[i] , reg.function)
  
  condnum = matrix(0,w,length(rreg$condnum))
  condnum.valid = matrix(0,w,length(rreg.valid$condnum))
  condnum.test = matrix(0,w,length(rreg.test$condnum))
  
  train.curve[i] <- rreg$err
  valid.curve[i] <- rreg.valid$err
  test.curve[i] <- rreg.test$err
  
  condnum[i,] <- rreg$condnum
  condnum.valid[i,] <- rreg.valid$condnum
  condnum.test[i,] <- rreg.test$condnum
  
  for (i in 2:w) {
    rreg <- fctr.mdl.train(factor, warmup, win[i], reg.function)
    rreg.valid <- fctr.mdl.valid(factor, warmup, win[i] , reg.function)  
    rreg.test <- fctr.mdl.test(factor, warmup, win[i] , reg.function)
    
    train.curve[i] <- rreg$err
    valid.curve[i] <- rreg.valid$err
    test.curve[i] <- rreg.test$err
    
    condnum[i,] <- rreg$condnum
    condnum.valid[i,] <- rreg.valid$condnum
    condnum.test[i,] <- rreg.test$condnum
  }
  
  list(train.curve = train.curve, valid.curve = valid.curve, test.curve = test.curve, condnum=condnum, condnum.valid = condnum.valid, condnum.test = condnum.test)
}







scf1<-sharpe.curves.fctr(factor=1,warmup=250,win = seq(from = 50, to = 250, by = 20))

scf1$train.curve
scf1$valid.curve
scf1$test.curve

scf2<-sharpe.curves.fctr(factor=2,warmup=250,win = seq(from = 50, to = 250, by = 20))

scf2$train.curve
scf2$valid.curve
scf2$test.curve

fctrroll1 = fctr.mdl.valid(factor=1,250, 250)
plot.ts(fctrroll1$predi,main='1 factor prediction')

fctrroll2 = fctr.mdl.valid(factor=2,250, 250)
plot.ts(fctrroll2$predi, main='2 factor prediction')








sc = sharpe.curves.fctr(factor=2, warmup= 250,
                   win = seq(from = 50, to = 250, by = 20))
fctrroll = fctr.mdl.valid(factor=2,250, 250)
plot.ts(fctrroll$predi)
fctrroll$predi ##Check positions in the market each day!
##Remember the position is the predicted
##return in our algorithm
##Do this for the test set as well
plot.ts(vroll$predi) ##Some positions are simply ridiculous!
##We will not invest like that in real world
plot.ts(vroll$ret)



sc = sharpe.curves.fctr(factor=1, warmup= 250,
                        win = seq(from = 50, to = 250, by = 20))
vroll = fctr.mdl.valid(factor=1,250, 250)
vroll$predi ##Check positions in the market each day!



sc = sharpe.curves.fctr(factor=2, warmup= 250,
                        win = seq(from = 50, to = 250, by = 20))
vroll = fctr.mdl.valid(factor=1,250, 250)
vroll$predi ##Check positions in the market each day!
##Remember the position is the predicted
##return in our algorithm
##Do this for the test set as well
plot.ts(vroll$predi) ##Some positions are simply ridiculous!
##We will not invest like that in real world
plot.ts(vroll$ret)
























win=50; warmup=250
x<- pred.r.prepare(max.lag=1)
xx <- norm.data(x)

n <- length(xx$y.test.dev)
err <- 0

condnum <- predi <- truth <- rep(0, n-warmup+1)



for (i in warmup:n) {
  
  y <- xx$y.train.dev[(i-win):(i-1)] - mean(xx$y.train.dev[(i-win):(i-1)])
  c1 <- c(xx$x.train.dev[(i-win):(i-1),1] - mean(xx$x.train.dev[(i-win):(i-1),1]))
  c2<- c(xx$x.train.dev[(i-win):(i-1),2] - mean(xx$x.train.dev[(i-win):(i-1),2]))
  xxx<-cbind(c1,c2)
  train.pca <- prcomp(xxx)
  f2<-t(train.pca$rotation[,c(1,2)])%*%t(train.pca$x)
}
f2





xx$x.train.dev[i]-mean(xx$x.train.dev[i:n]
y<-fctr.mdl(factor=2,250,10)
y$err


win=50; warmup=250
for (i in warmup:n) {
  
  y <- xx$y.train.dev[(i-win):(i-1)] - mean(xx$y.train.dev[(warmup-win):(n-1)])
  c1 <- c(xx$x.train.dev[(i-win):(i-1),1] - mean(xx$x.train.dev[(warmup-win):(n-1),1]))
  c2<- c(xx$x.train.dev[(i-win):(i-1),2] - mean(xx$x.train.dev[(warmup-win):(n-1),2]))
}  

xxx<-cbind(c1,c2)
train.pca <- prcomp(xxx)
f2<-t(train.pca$rotation[,c(1,2)])%*%t(train.pca$x)
f2
  xxx<-cbind(c1)
  xxx
  
  train.pca <- prcomp(xxx)
  train.pca$x
  f1<-t(train.pca$rotation[,1])%*%t(train.pca$x)
  f1
  f2
  zz <- reg.function(t(f2), y, th=0, xx$x.train.dev[i,]-mean(xx$x.train.dev[i:n,]))
  
  predi[i-warmup+1] <- zz$pr
  condnum[i-warmup+1] <- zz$condnum
  truth[i-warmup+1] <- xx$y.train.dev[i]
}





fctr.1 <- function(x,warmup, win){
  xx <- norm.data(x)
  
  n <- length(xx$y.train.dev)
  
  
  condnum <- predi <- truth <- rep(0, n-warmup+1)
  
  for (i in warmup:n) {
    
    y <- xx$y.train.dev[(i-win):(i-1)] - mean(xx$y.train.dev[(warmup-win):(n-1)])
    c1 <- c(xx$x.train.dev[(i-win):(i-1),1] - mean(xx$x.train.dev[(warmup-win):(n-1),1]))
    xxx<-cbind(c1)
    
    
  }
  list(xxx=xxx,y=y)
}

c1 <- c(xx$x.train.dev[(i-win):(i-1),1] - mean(xx$x.train.dev[(warmup-win):(n-1),1]))
c2<- c(xx$x.train.dev[(i-win):(i-1),2] - mean(xx$x.train.dev[(warmup-win):(n-1),2]))

fctr.1(x,250,50)
x<-pred.r.prepare(5)
xx<-norm.data(x)
xx$x.train.dev
fctr.2(x,warmup=250,win=250)
y<-seq(from = 50, to = 250, by = 20)
print(y)



           i=250
win=20
n=400
xxx<-matrix()
xxx <- c(xx$x.train.dev[(i-win):(i-1),1] - mean(xx$x.train.dev[(warmup-win):(n-1),1])

v1<-c(xx$x.train.dev[(i-win):(i-1),1]-mean(xx$x.train.dev[(i-win):(i-1),1]))
class(xxx)
class(mean(xx$x.train.dev[(i-win):(i-1),1]))

xxx<-matrix(v1)
class(xxx)
                                                  
                                                  
xx$x.train.dev
xx$x.train[(warmup-win):(n-1),1]

xxx[1,] <- xx$x.train.dev[(i-win):(i-1),2] - mean(xx$x.train[(warmup-win):(n-1),2])

x=data,  th=0,warmup= 250,
win = seq(from = 50, to = 250, by = 20)
x$x
x$y

dim(t(f2))

dim(x$x.train)
















scf1 <- sharpe.curves.fctr(factor=1,warmup=250,win = seq(from = 50, to = 250, by = 20))
scf1$train.curve
scf1$valid.curve
scf1$test.curve
scf2<-sharpe.curves.fctr(factor=2,warmup=250,win = seq(from = 50, to = 250, by = 20))
scf2$train.curve
scf2$valid.curve
scf2$test.curve
fctrroll1 = fctr.mdl.valid(factor=1,250, 250)
plot.ts(fctrroll1$predi,main='1 factor prediction')
fctrroll2 = fctr.mdl.valid(factor=2,250, 250)
plot.ts(fctrroll2$predi, main='2 factor prediction')