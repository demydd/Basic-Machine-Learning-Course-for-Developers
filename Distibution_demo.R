library(manipulate)


xseq<-seq(-4,4,.01)
y<-2*xseq + rnorm(length(xseq),0,5.5)


hist(y, prob=TRUE, ylim=c(0,.06), breaks=20)
hist(y, breaks=20)
curve(dnorm(x, mean(y), sd(y)), add=TRUE, col="darkblue", lwd=2)


check <- rnorm(1000000)
check <- sample(1:100, 1000000, replace = TRUE)
check <- scale(check, center = FALSE)
check <- scale(check, center = TRUE)
check <- scale(check)


makeHist = function(data, qty, bins, ylim, conf_interval, conf_interval_type, dfcurve){
  
  hist(data, breaks = bins, prob = TRUE, ylim=c(0,ylim)
       , main = paste("Histogram of conf.interval" , conf_interval, "qty =", qty))
  
  conf_position <- round((1 - conf_interval)/2 * qty, 0)
  
  if (conf_interval_type != 'none'){
    
    if(conf_interval_type == 'left') {
      left_limit <- data[order(data)][conf_position]
      abline(v = left_limit, col = "red", lwd = 2)
    }else{ 
      if(conf_interval_type == 'right'){
        right_limit <- data[order(data)][qty - conf_position]
        abline(v = right_limit, col = "red", lwd = 2)
      }else{
        left_limit <- data[order(data)][conf_position]
        right_limit <- data[order(data)][qty - conf_position]
        
        abline(v = left_limit, col = "red", lwd = 2)
        abline(v = right_limit, col = "red", lwd = 2)
      }
    }

    
          
  }
  
  if(dfcurve == 'yes') curve(dnorm(x, mean(data), sd(data)), add=TRUE, col="darkblue", lwd=2)
     
}

#debugonce(makeHist)

manipulate(
  
  qty = slider(1, 1000000),
  bins = slider(1,100),
  ylim = slider(0,1),
  conf_interval_type = picker('none', 'left', 'right', 'both'),
  conf_interval = picker(0.68, 0.9, 0.95, 0.99),
  dfcurve = picker('no', 'yes'),
  
  makeHist(rnorm(qty), qty, bins, ylim, conf_interval, conf_interval_type, dfcurve)
  
)



manipulate(
  
  qty = slider(1, 1000000),
  bins = slider(1,100),
  ylim = slider(0,1),
  conf_interval_type = picker('none', 'left', 'right', 'both'),
  conf_interval = picker(0.68, 0.9, 0.95, 0.99),
  dfcurve = picker('no', 'yes'),
  
  makeHist(rnorm(qty), qty, bins, ylim, conf_interval, conf_interval_type, dfcurve)
  
)


