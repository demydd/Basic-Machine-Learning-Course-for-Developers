library(data.table)
library(manipulate)

location <- 'D:\\Demyd\\Personal\\R\\kaggle\\'
filename <- 'application_test.csv'
data_all <- data.table(read.csv(paste(location, filename, sep = "")))
vars <- c('AMT_INCOME_TOTAL',	'AMT_CREDIT',	'AMT_ANNUITY',	'AMT_GOODS_PRICE',	'REGION_POPULATION_RELATIVE',	'HOUR_APPR_PROCESS_START')
data_vars <- data_all[, ..vars]

data_all$AMT_INCOME_TOTAL
check <- sort(data_all$AMT_INCOME_TOTAL)[1:(length(data_all$AMT_INCOME_TOTAL)-100)]

x <- c(1,2,3)


makeHist = function( data_init
                    ,data
                    ,variable
                    ,top_items
                    ,qty
                    ,bins
                    ,ylim
                    ,conf_interval
                    ,conf_interval_type
                    ,dfcurve
                   ){



  par(mfrow=c(2, 1))
  hist(data_init, breaks = bins, main = paste("Initial histogram of conf.interval", conf_interval, "qty =", ifelse(qty > length(data_init), length(data_init), qty)))

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

  variable = picker(colnames(data_all)),
  top_items = slider(1, 1000),
  qty = slider(1, 1000000),
  bins = slider(1,100),
  ylim = slider(0,1),
  conf_interval_type = picker('none', 'left', 'right', 'both'),
  conf_interval = picker(0.68, 0.9, 0.95, 0.99),
  dfcurve = picker('no', 'yes'),

  makeHist(data_all, rnorm(qty), variable, top_items
           , qty, bins, ylim, conf_interval, conf_interval_type, dfcurve)

)


dev.off()

