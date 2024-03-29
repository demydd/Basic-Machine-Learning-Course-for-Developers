library(data.table)
library(manipulate)

location <- 'D:\\Demyd\\Personal\\R\\kaggle\\'
filename <- 'application_test.csv'
data_all <- data.table(read.csv(paste(location, filename, sep = "")))
vars <- c('AMT_INCOME_TOTAL',	'AMT_CREDIT',	'AMT_ANNUITY',	'AMT_GOODS_PRICE',	'REGION_POPULATION_RELATIVE',	'HOUR_APPR_PROCESS_START')
data_vars <- data_all[, ..vars]

#data_all$AMT_INCOME_TOTAL
#check <- sort(data_all$AMT_INCOME_TOTAL)[1:(length(data_all$AMT_INCOME_TOTAL)-100)]

#x <- c(1,2,3)

makeHistScaled = function( data_init
                     ,data_norm
                     ,variable
                     ,qty
                     ,top_items
                     ,bins
                     ,ylim
                     ,conf_interval
                     ,conf_interval_type
                     ,dfcurve
){

  name <- colnames(data_init)[colnames(data_init) == variable]
  data <- as.vector(unlist(data_init[, ..name]))
  data <- data[order(data)]
  data <- data[1:(length(data) - top_items)]
  #print(length(data) - top_items)

  par(mfrow=c(2, 2))
  hist(data, breaks = bins, main = paste("Initial data, confidence =", conf_interval))
  hist(scale(data, center = TRUE, scale = FALSE), breaks = bins, main = paste("Centered data, confidence =", conf_interval))

  hist(scale(data, center = FALSE, scale = TRUE), breaks = bins, prob = TRUE, ylim=c(0,ylim), main = paste("Scaled data, confidence =", conf_interval))
  if(dfcurve == 'yes') curve(dnorm(x, mean(scale(data, center = FALSE, scale = TRUE)), sd(scale(data, center = FALSE, scale = TRUE))), add=TRUE, col="darkblue", lwd=2)
  hist(scale(data, center = TRUE, scale = TRUE), breaks = bins, prob = TRUE, ylim=c(0,ylim), main = paste("Centered & Scaled data, confidence =", conf_interval))
  if(dfcurve == 'yes') curve(dnorm(x, mean(scale(data, center = TRUE, scale = TRUE)), sd(scale(data, center = TRUE, scale = TRUE))), add=TRUE, col="darkblue", lwd=2)

  conf_position <- round((1 - conf_interval)/2 * qty, 0)

  if (conf_interval_type != 'none'){

    if(conf_interval_type == 'left') {
      left_limit <- data_norm[order(data_norm)][conf_position]
      abline(v = left_limit, col = "red", lwd = 2)
    }else{
      if(conf_interval_type == 'right'){
        right_limit <- data_norm[order(data_norm)][qty - conf_position]
        abline(v = right_limit, col = "red", lwd = 2)
      }else{
        left_limit <- data_norm[order(data_norm)][conf_position]
        right_limit <- data_norm[order(data_norm)][qty - conf_position]

        abline(v = left_limit, col = "red", lwd = 2)
        abline(v = right_limit, col = "red", lwd = 2)
      }
    }

  }

}

makeHist = function( data_init
                    ,data_norm
                    ,variable
                    ,top_items
                    ,qty
                    ,bins
                    ,ylim
                    ,conf_interval
                    ,conf_interval_type
                    ,dfcurve
                   ){

  name <- colnames(data_init)[colnames(data_init) == variable]
  data <- unlist(data_init[, ..name])
  data <- data[order(data)]
  data <- data[1:(length(data) - top_items)]

  par(mfrow=c(3, 1))
  hist(data, breaks = bins, main = paste("Initial histogram of conf.interval", conf_interval, "qty =", ifelse(qty > length(data), length(data), qty)))
  hist(scale(data), breaks = bins, prob = TRUE, ylim=c(0,ylim), main = paste("Scaled histogram of conf.interval", conf_interval, "qty =", ifelse(qty > length(data), length(data), qty)))
  if(dfcurve == 'yes') curve(dnorm(x, mean(data_norm), sd(data_norm)), add=TRUE, col="darkblue", lwd=2)

  hist(data_norm, breaks = bins, prob = TRUE, ylim=c(0,ylim)
       , main = paste("Theoretical histogram of conf.interval" , conf_interval, "qty =", qty))
  if(dfcurve == 'yes') curve(dnorm(x, mean(data_norm), sd(data_norm)), add=TRUE, col="darkblue", lwd=2)

  conf_position <- round((1 - conf_interval)/2 * qty, 0)

  if (conf_interval_type != 'none'){

    if(conf_interval_type == 'left') {
      left_limit <- data_norm[order(data_norm)][conf_position]
      abline(v = left_limit, col = "red", lwd = 2)
    }else{
      if(conf_interval_type == 'right'){
        right_limit <- data_norm[order(data_norm)][qty - conf_position]
        abline(v = right_limit, col = "red", lwd = 2)
      }else{
        left_limit <- data_norm[order(data_norm)][conf_position]
        right_limit <- data_norm[order(data_norm)][qty - conf_position]

        abline(v = left_limit, col = "red", lwd = 2)
        abline(v = right_limit, col = "red", lwd = 2)
      }
    }

  }

}

#debugonce(makeHist)
xxx <- colnames(data_all)
vars <- as.list(c('AMT_INCOME_TOTAL',	'AMT_CREDIT',	'AMT_ANNUITY',	'AMT_GOODS_PRICE',	'REGION_POPULATION_RELATIVE',	'HOUR_APPR_PROCESS_START'))

manipulate(

  variable = picker(vars),
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

#debugonce(makeHistScaled)

manipulate(

  variable = picker(vars),
  top_items = slider(1, 1000),
  qty = slider(1, 1000000),
  bins = slider(1,100),
  ylim = slider(0,1),
  conf_interval_type = picker('none', 'left', 'right', 'both'),
  conf_interval = picker(0.68, 0.9, 0.95, 0.99),
  dfcurve = picker('no', 'yes'),

  makeHistScaled( data_all
                 ,rnorm(qty)
                 ,variable
                 ,qty
                 ,top_items
                 ,bins
                 ,ylim
                 ,conf_interval
                 ,conf_interval_type
                 ,dfcurve
                )
)


variable <- 'AMT_INCOME_TOTAL'
name <- colnames(data_all)[colnames(data_all) == variable]
data_all[,]


dev.off()

