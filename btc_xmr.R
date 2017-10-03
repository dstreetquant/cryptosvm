library("quantmod")
library("PerformanceAnalytics")
library("zoo")
library("gplots")

stockCode <- "BTC_XMR"
polData <- read.csv("BTC_XMR.csv")
txnCostinPerc <- 0.5

dateTime <- paste(polData$Date, polData$Time, sep = ":")
timeIndx <- strptime(dateTime, format = "%d-%m-%Y:%H-%M-%S")
prices <- polData[, c("Open", "High", "Low", "Close", "Volume")]
stockPriceXtsList <- list()
stockPriceXtsList[[stockCode]] <- as.xts(prices, timeIndx)

time_index <- as.POSIXct(index(stockPriceXtsList[[stockCode]]), format = "%d-%m-%Y:%H-%M-%S")
stockPriceXtsList[[stockCode]] <- apply(stockPriceXtsList[[stockCode]], 2, as.numeric)
stockPriceXtsList[[stockCode]] <- xts(stockPriceXtsList[[stockCode]], time_index)
priceList <- stockPriceXtsList[[stockCode]]
priceDF <- priceList

########################################################################
########################### Predictors  ################################
########################################################################

######################## MA 5 #############################
ma_5_t0 <- EMA(Cl(priceList), 5)
ma_5_t1 <- lag(ma_5_t0, 1)
ma_5_t2 <- lag(ma_5_t0, 2)
ma_5_t3 <- lag(ma_5_t0, 3)
ma_5_t4 <- lag(ma_5_t0, 4)
ma_5_t5 <- lag(ma_5_t0, 5)
ma_5_DiffPerc_t0 <- (ma_5_t0 - lag(ma_5_t0, 1))/lag(ma_5_t0, 1) * 100
ma_5_DiffPerc_t1 <- lag(ma_5_DiffPerc_t0, 1)
ma_5_DiffPerc_t2 <- lag(ma_5_DiffPerc_t0, 2)
ma_5_DiffPerc_t3 <- lag(ma_5_DiffPerc_t0, 3)
ma_5_DiffPerc_t4 <- lag(ma_5_DiffPerc_t0, 4)
ma_5_DiffPerc_t5 <- lag(ma_5_DiffPerc_t0, 5)

ma5DF <- ma_5_t0
ma5DF <- merge(ma5DF, ma_5_t1, ma_5_t2, ma_5_t3, ma_5_t4, ma_5_t5)
colnames(ma5DF) <- c("EMA5_t0", "EMA5_t1", "EMA5_t2", "EMA5_t3", "EMA5_t4", "EMA5_t5")
ma5PerDF <- ma_5_DiffPerc_t0
ma5PerDF <- merge(ma5PerDF, ma_5_DiffPerc_t1, ma_5_DiffPerc_t2, ma_5_DiffPerc_t3, ma_5_DiffPerc_t4, ma_5_DiffPerc_t5)
colnames(ma5PerDF) <- c("EMA5_PER_t0", "EMA5_PER_t1", "EMA5_PER_t2", "EMA5_PER_t3", "EMA5_PER_t4", "EMA5_PER_t5")
priceDF <- merge(priceDF, ma5DF)
priceDF <- merge(priceDF, ma5PerDF)

######################## MA 10 #############################
ma_10_t0 <- EMA(Cl(priceList), 10)
ma_10_t1 <- lag(ma_10_t0, 1)
ma_10_t2 <- lag(ma_10_t0, 2)
ma_10_t3 <- lag(ma_10_t0, 3)
ma_10_t4 <- lag(ma_10_t0, 4)
ma_10_t5 <- lag(ma_10_t0, 5)
ma_10_DiffPerc_t0 <- (ma_10_t0 - lag(ma_10_t0, 1))/lag(ma_10_t0, 1) * 100
ma_10_DiffPerc_t1 <- lag(ma_10_DiffPerc_t0, 1)
ma_10_DiffPerc_t2 <- lag(ma_10_DiffPerc_t0, 2)
ma_10_DiffPerc_t3 <- lag(ma_10_DiffPerc_t0, 3)
ma_10_DiffPerc_t4 <- lag(ma_10_DiffPerc_t0, 4)
ma_10_DiffPerc_t5 <- lag(ma_10_DiffPerc_t0, 5)

ma10DF <- ma_10_t0
ma10DF <- merge(ma10DF, ma_10_t1, ma_10_t2, ma_10_t3, ma_10_t4, ma_10_t5)
colnames(ma10DF) <- c("EMA10_t0", "EMA10_t1", "EMA10_t2", "EMA10_t3", "EMA10_t4", "EMA10_t5")
ma10PerDF <- ma_10_DiffPerc_t0
ma10PerDF <- merge(ma10PerDF, ma_10_DiffPerc_t1, ma_10_DiffPerc_t2, ma_10_DiffPerc_t3, ma_10_DiffPerc_t4, ma_10_DiffPerc_t5)
colnames(ma10PerDF) <- c("EMA10_PER_t0", "EMA10_PER_t1", "EMA10_PER_t2", "EMA10_PER_t3", "EMA10_PER_t4", "EMA10_PER_t5")
priceDF <- merge(priceDF, ma10DF)
priceDF <- merge(priceDF, ma10PerDF)

######################## MA 20 #############################
ma_20_t0 <- EMA(Cl(priceList), 20)
ma_20_t1 <- lag(ma_20_t0, 1)
ma_20_t2 <- lag(ma_20_t0, 2)
ma_20_t3 <- lag(ma_20_t0, 3)
ma_20_t4 <- lag(ma_20_t0, 4)
ma_20_t5 <- lag(ma_20_t0, 5)
ma_20_DiffPerc_t0 <- (ma_20_t0 - lag(ma_20_t0, 1))/lag(ma_20_t0, 1) * 100
ma_20_DiffPerc_t1 <- lag(ma_20_DiffPerc_t0, 1)
ma_20_DiffPerc_t2 <- lag(ma_20_DiffPerc_t0, 2)
ma_20_DiffPerc_t3 <- lag(ma_20_DiffPerc_t0, 3)
ma_20_DiffPerc_t4 <- lag(ma_20_DiffPerc_t0, 4)
ma_20_DiffPerc_t5 <- lag(ma_20_DiffPerc_t0, 5)

ma20DF <- ma_20_t0
ma20DF <- merge(ma20DF, ma_20_t1, ma_20_t2, ma_20_t3, ma_20_t4, ma_20_t5)
colnames(ma20DF) <- c("EMA20_t0", "EMA20_t1", "EMA20_t2", "EMA20_t3", "EMA20_t4", "EMA20_t5")
ma20PerDF <- ma_20_DiffPerc_t0
ma20PerDF <- merge(ma20PerDF, ma_20_DiffPerc_t1, ma_20_DiffPerc_t2, ma_20_DiffPerc_t3, ma_20_DiffPerc_t4, ma_20_DiffPerc_t5)
colnames(ma20PerDF) <- c("EMA20_PER_t0", "EMA20_PER_t1", "EMA20_PER_t2", "EMA20_PER_t3", "EMA20_PER_t4", "EMA20_PER_t5")
priceDF <- merge(priceDF, ma20DF)
priceDF <- merge(priceDF, ma20PerDF)

######################## RSI 5 #############################
rsi_5_t0 <- RSI(Cl(priceList), 5)
rsi_5_t1 <- lag(rsi_5_t0, 1)
rsi_5_t2 <- lag(rsi_5_t0, 2)
rsi_5_t3 <- lag(rsi_5_t0, 3)
rsi_5_t4 <- lag(rsi_5_t0, 4)
rsi_5_t5 <- lag(rsi_5_t0, 5)
rsi_5_DiffPerc_t0 <- (rsi_5_t0 - lag(rsi_5_t0, 1))/lag(rsi_5_t0, 1) * 100
rsi_5_DiffPerc_t1 <- lag(rsi_5_DiffPerc_t0, 1)
rsi_5_DiffPerc_t2 <- lag(rsi_5_DiffPerc_t0, 2)
rsi_5_DiffPerc_t3 <- lag(rsi_5_DiffPerc_t0, 3)
rsi_5_DiffPerc_t4 <- lag(rsi_5_DiffPerc_t0, 4)
rsi_5_DiffPerc_t5 <- lag(rsi_5_DiffPerc_t0, 5)

rsi5DF <- rsi_5_t0
rsi5DF <- merge(rsi5DF, rsi_5_t1, rsi_5_t2, rsi_5_t3, rsi_5_t4, rsi_5_t5)
colnames(rsi5DF) <- c("RSI5_t0", "RSI5_t1", "RSI5_t2", "RSI5_t3", "RSI5_t4", "RSI5_t5")
rsi5PerDF <- rsi_5_DiffPerc_t0
rsi5PerDF <- merge(rsi5PerDF, rsi_5_DiffPerc_t1, rsi_5_DiffPerc_t2, rsi_5_DiffPerc_t3, rsi_5_DiffPerc_t4, rsi_5_DiffPerc_t5)
colnames(rsi5PerDF) <- c("RSI5_PER_t0", "RSI5_PER_t1", "RSI5_PER_t2", "RSI5_PER_t3", "RSI5_PER_t4", "RSI5_PER_t5")
priceDF <- merge(priceDF, rsi5DF)
priceDF <- merge(priceDF, rsi5PerDF)

######################## RSI 10 #############################
rsi_10_t0 <- RSI(Cl(priceList), 10)
rsi_10_t1 <- lag(rsi_10_t0, 1)
rsi_10_t2 <- lag(rsi_10_t0, 2)
rsi_10_t3 <- lag(rsi_10_t0, 3)
rsi_10_t4 <- lag(rsi_10_t0, 4)
rsi_10_t5 <- lag(rsi_10_t0, 5)
rsi_10_DiffPerc_t0 <- (rsi_10_t0 - lag(rsi_10_t0, 1))/lag(rsi_10_t0, 1) * 100
rsi_10_DiffPerc_t1 <- lag(rsi_10_DiffPerc_t0, 1)
rsi_10_DiffPerc_t2 <- lag(rsi_10_DiffPerc_t0, 2)
rsi_10_DiffPerc_t3 <- lag(rsi_10_DiffPerc_t0, 3)
rsi_10_DiffPerc_t4 <- lag(rsi_10_DiffPerc_t0, 4)
rsi_10_DiffPerc_t5 <- lag(rsi_10_DiffPerc_t0, 5)

rsi10DF <- rsi_10_t0
rsi10DF <- merge(rsi10DF, rsi_10_t1, rsi_10_t2, rsi_10_t3, rsi_10_t4, rsi_10_t5)
colnames(rsi10DF) <- c("RSI10_t0", "RSI10_t1", "RSI10_t2", "RSI10_t3", "RSI10_t4", "RSI10_t5")
rsi10PerDF <- rsi_10_DiffPerc_t0
rsi10PerDF <- merge(rsi10PerDF, rsi_10_DiffPerc_t1, rsi_10_DiffPerc_t2, rsi_10_DiffPerc_t3, rsi_10_DiffPerc_t4, rsi_10_DiffPerc_t5)
colnames(rsi10PerDF) <- c("RSI10_PER_t0", "RSI10_PER_t1", "RSI10_PER_t2", "RSI10_PER_t3", "RSI10_PER_t4", "RSI10_PER_t5")
priceDF <- merge(priceDF, rsi10DF)
priceDF <- merge(priceDF, rsi10PerDF)

######################## RSI 20 #############################
rsi_20_t0 <- RSI(Cl(priceList), 20)
rsi_20_t1 <- lag(rsi_20_t0, 1)
rsi_20_t2 <- lag(rsi_20_t0, 2)
rsi_20_t3 <- lag(rsi_20_t0, 3)
rsi_20_t4 <- lag(rsi_20_t0, 4)
rsi_20_t5 <- lag(rsi_20_t0, 5)
rsi_20_DiffPerc_t0 <- (rsi_20_t0 - lag(rsi_20_t0, 1))/lag(rsi_20_t0, 1) * 100
rsi_20_DiffPerc_t1 <- lag(rsi_20_DiffPerc_t0, 1)
rsi_20_DiffPerc_t2 <- lag(rsi_20_DiffPerc_t0, 2)
rsi_20_DiffPerc_t3 <- lag(rsi_20_DiffPerc_t0, 3)
rsi_20_DiffPerc_t4 <- lag(rsi_20_DiffPerc_t0, 4)
rsi_20_DiffPerc_t5 <- lag(rsi_20_DiffPerc_t0, 5)

rsi20DF <- rsi_20_t0
rsi20DF <- merge(rsi20DF, rsi_20_t1, rsi_20_t2, rsi_20_t3, rsi_20_t4, rsi_20_t5)
colnames(rsi20DF) <- c("RSI20_t0", "RSI20_t1", "RSI20_t2", "RSI20_t3", "RSI20_t4", "RSI20_t5")
rsi20PerDF <- rsi_20_DiffPerc_t0
rsi20PerDF <- merge(rsi20PerDF, rsi_20_DiffPerc_t1, rsi_20_DiffPerc_t2, rsi_20_DiffPerc_t3, rsi_20_DiffPerc_t4, rsi_20_DiffPerc_t5)
colnames(rsi20PerDF) <- c("RSI20_PER_t0", "RSI20_PER_t1", "RSI20_PER_t2", "RSI20_PER_t3", "RSI20_PER_t4", "RSI20_PER_t5")
priceDF <- merge(priceDF, rsi20DF)
priceDF <- merge(priceDF, rsi20PerDF)

######################## ATR 5 #############################
atr_5 <- ATR(priceList[,c("High","Low","Close")], 5)
atr_5_t0 <- atr_5$atr
atr_5_t1 <- lag(atr_5_t0, 1)
atr_5_t2 <- lag(atr_5_t0, 2)
atr_5_t3 <- lag(atr_5_t0, 3)
atr_5_t4 <- lag(atr_5_t0, 4)
atr_5_t5 <- lag(atr_5_t0, 5)
atr_5_DiffPerc_t0 <- (atr_5_t0 - lag(atr_5_t0, 1))/lag(atr_5_t0, 1) * 100
atr_5_DiffPerc_t1 <- lag(atr_5_DiffPerc_t0, 1)
atr_5_DiffPerc_t2 <- lag(atr_5_DiffPerc_t0, 2)
atr_5_DiffPerc_t3 <- lag(atr_5_DiffPerc_t0, 3)
atr_5_DiffPerc_t4 <- lag(atr_5_DiffPerc_t0, 4)
atr_5_DiffPerc_t5 <- lag(atr_5_DiffPerc_t0, 5)

atr5DF <- atr_5_t0
atr5DF <- merge(atr5DF, atr_5_t1, atr_5_t2, atr_5_t3, atr_5_t4, atr_5_t5)
colnames(atr5DF) <- c("ATR5_t0", "ATR5_t1", "ATR5_t2", "ATR5_t3", "ATR5_t4", "ATR5_t5")
atr5PerDF <- atr_5_DiffPerc_t0
atr5PerDF <- merge(atr5PerDF, atr_5_DiffPerc_t1, atr_5_DiffPerc_t2, atr_5_DiffPerc_t3, atr_5_DiffPerc_t4, atr_5_DiffPerc_t5)
colnames(atr5PerDF) <- c("ATR5_PER_t0", "ATR5_PER_t1", "ATR5_PER_t2", "ATR5_PER_t3", "ATR5_PER_t4", "ATR5_PER_t5")
priceDF <- merge(priceDF, atr5DF)
priceDF <- merge(priceDF, atr5PerDF)

######################## ATR 10 #############################
atr_10 <- ATR(priceList[,c("High","Low","Close")], 10)
atr_10_t0 <- atr_10$atr
atr_10_t1 <- lag(atr_10_t0, 1)
atr_10_t2 <- lag(atr_10_t0, 2)
atr_10_t3 <- lag(atr_10_t0, 3)
atr_10_t4 <- lag(atr_10_t0, 4)
atr_10_t5 <- lag(atr_10_t0, 5)
atr_10_DiffPerc_t0 <- (atr_10_t0 - lag(atr_10_t0, 1))/lag(atr_10_t0, 1) * 100
atr_10_DiffPerc_t1 <- lag(atr_10_DiffPerc_t0, 1)
atr_10_DiffPerc_t2 <- lag(atr_10_DiffPerc_t0, 2)
atr_10_DiffPerc_t3 <- lag(atr_10_DiffPerc_t0, 3)
atr_10_DiffPerc_t4 <- lag(atr_10_DiffPerc_t0, 4)
atr_10_DiffPerc_t5 <- lag(atr_10_DiffPerc_t0, 5)

atr10DF <- atr_10_t0
atr10DF <- merge(atr10DF, atr_10_t1, atr_10_t2, atr_10_t3, atr_10_t4, atr_10_t5)
colnames(atr10DF) <- c("ATR10_t0", "ATR10_t1", "ATR10_t2", "ATR10_t3", "ATR10_t4", "ATR10_t5")
atr10PerDF <- atr_10_DiffPerc_t0
atr10PerDF <- merge(atr10PerDF, atr_10_DiffPerc_t1, atr_10_DiffPerc_t2, atr_10_DiffPerc_t3, atr_10_DiffPerc_t4, atr_10_DiffPerc_t5)
colnames(atr10PerDF) <- c("ATR10_PER_t0", "ATR10_PER_t1", "ATR10_PER_t2", "ATR10_PER_t3", "ATR10_PER_t4", "ATR10_PER_t5")
priceDF <- merge(priceDF, atr10DF)
priceDF <- merge(priceDF, atr10PerDF)

######################## ATR 20 #############################
atr_20 <- ATR(priceList[,c("High","Low","Close")], 20)
atr_20_t0 <- atr_20$atr
atr_20_t1 <- lag(atr_20_t0, 1)
atr_20_t2 <- lag(atr_20_t0, 2)
atr_20_t3 <- lag(atr_20_t0, 3)
atr_20_t4 <- lag(atr_20_t0, 4)
atr_20_t5 <- lag(atr_20_t0, 5)
atr_20_DiffPerc_t0 <- (atr_20_t0 - lag(atr_20_t0, 1))/lag(atr_20_t0, 1) * 100
atr_20_DiffPerc_t1 <- lag(atr_20_DiffPerc_t0, 1)
atr_20_DiffPerc_t2 <- lag(atr_20_DiffPerc_t0, 2)
atr_20_DiffPerc_t3 <- lag(atr_20_DiffPerc_t0, 3)
atr_20_DiffPerc_t4 <- lag(atr_20_DiffPerc_t0, 4)
atr_20_DiffPerc_t5 <- lag(atr_20_DiffPerc_t0, 5)

atr20DF <- atr_20_t0
atr20DF <- merge(atr20DF, atr_20_t1, atr_20_t2, atr_20_t3, atr_20_t4, atr_20_t5)
colnames(atr20DF) <- c("ATR20_t0", "ATR20_t1", "ATR20_t2", "ATR20_t3", "ATR20_t4", "ATR20_t5")
atr20PerDF <- atr_20_DiffPerc_t0
atr20PerDF <- merge(atr20PerDF, atr_20_DiffPerc_t1, atr_20_DiffPerc_t2, atr_20_DiffPerc_t3, atr_20_DiffPerc_t4, atr_20_DiffPerc_t5)
colnames(atr20PerDF) <- c("ATR20_PER_t0", "ATR20_PER_t1", "ATR20_PER_t2", "ATR20_PER_t3", "ATR20_PER_t4", "ATR20_PER_t5")
priceDF <- merge(priceDF, atr20DF)
priceDF <- merge(priceDF, atr20PerDF)

######################## Hi-Lo #############################
hilo_t0 <- priceList$High/priceList$Low
hilo_t1 <- lag(hilo_t0, 1)
hilo_t2 <- lag(hilo_t0, 2)
hilo_t3 <- lag(hilo_t0, 3)
hilo_t4 <- lag(hilo_t0, 4)
hilo_t5 <- lag(hilo_t0, 5)

hiloDF <- hilo_t0
hiloDF <- merge(hiloDF, hilo_t1, hilo_t2, hilo_t3, hilo_t4, hilo_t5)
colnames(hiloDF) <- c("HILO_t0", "HILO_t1", "HILO_t2", "HILO_t3", "HILO_t4", "HILO_t5")
priceDF <- merge(priceDF, hiloDF)

######################## Hi-Op #############################
hiOp_t0 <- priceList$High/priceList$Open
hiOp_t1 <- lag(hiOp_t0, 1)
hiOp_t2 <- lag(hiOp_t0, 2)
hiOp_t3 <- lag(hiOp_t0, 3)
hiOp_t4 <- lag(hiOp_t0, 4)
hiOp_t5 <- lag(hiOp_t0, 5)

hiOpDF <- hiOp_t0
hiOpDF <- merge(hiOpDF, hiOp_t1, hiOp_t2, hiOp_t3, hiOp_t4, hiOp_t5)
colnames(hiOpDF) <- c("HIOP_t0", "HIOP_t1", "HIOP_t2", "HIOP_t3", "HIOP_t4", "HIOP_t5")
priceDF <- merge(priceDF, hiOpDF)

######################## Hi-Cl #############################
hiCl_t0 <- priceList$High/priceList$Close
hiCl_t1 <- lag(hiCl_t0, 1)
hiCl_t2 <- lag(hiCl_t0, 2)
hiCl_t3 <- lag(hiCl_t0, 3)
hiCl_t4 <- lag(hiCl_t0, 4)
hiCl_t5 <- lag(hiCl_t0, 5)

hiClDF <- hiCl_t0
hiClDF <- merge(hiClDF, hiCl_t1, hiCl_t2, hiCl_t3, hiCl_t4, hiCl_t5)
colnames(hiClDF) <- c("HICL_t0", "HICL_t1", "HICL_t2", "HICL_t3", "HICL_t4", "HICL_t5")
priceDF <- merge(priceDF, hiClDF)

######################## Lo-Op #############################
loOp_t0 <- priceList$Low/priceList$Open
loOp_t1 <- lag(loOp_t0, 1)
loOp_t2 <- lag(loOp_t0, 2)
loOp_t3 <- lag(loOp_t0, 3)
loOp_t4 <- lag(loOp_t0, 4)
loOp_t5 <- lag(loOp_t0, 5)

loOpDF <- loOp_t0
loOpDF <- merge(loOpDF, loOp_t1, loOp_t2, loOp_t3, loOp_t4, loOp_t5)
colnames(loOpDF) <- c("LOOP_t0", "LOOP_t1", "LOOP_t2", "LOOP_t3", "LOOP_t4", "LOOP_t5")
priceDF <- merge(priceDF, loOpDF)

######################## Lo-Cl #############################
loCl_t0 <- priceList$Low/priceList$Close
loCl_t1 <- lag(loCl_t0, 1)
loCl_t2 <- lag(loCl_t0, 2)
loCl_t3 <- lag(loCl_t0, 3)
loCl_t4 <- lag(loCl_t0, 4)
loCl_t5 <- lag(loCl_t0, 5)

loClDF <- loCl_t0
loClDF <- merge(loClDF, loCl_t1, loCl_t2, loCl_t3, loCl_t4, loCl_t5)
colnames(loClDF) <- c("LOCL_t0", "LOCL_t1", "LOCL_t2", "LOCL_t3", "LOCL_t4", "LOCL_t5")
priceDF <- merge(priceDF, loClDF)

######################## Cl-Op #############################
ClOp_t0 <- priceList$Close/priceList$Open
ClOp_t1 <- lag(ClOp_t0, 1)
ClOp_t2 <- lag(ClOp_t0, 2)
ClOp_t3 <- lag(ClOp_t0, 3)
ClOp_t4 <- lag(ClOp_t0, 4)
ClOp_t5 <- lag(ClOp_t0, 5)

ClOpDF <- ClOp_t0
ClOpDF <- merge(ClOpDF, ClOp_t1, ClOp_t2, ClOp_t3, ClOp_t4, ClOp_t5)
colnames(ClOpDF) <- c("CLOP_t0", "CLOP_t1", "CLOP_t2", "CLOP_t3", "CLOP_t4", "CLOP_t5")
priceDF <- merge(priceDF, ClOpDF)

######################## Vol #############################
vol_t0 <- priceList$Volume
vol_t1 <- lag(vol_t0, 1)
vol_t2 <- lag(vol_t0, 2)
vol_t3 <- lag(vol_t0, 3)
vol_t4 <- lag(vol_t0, 4)
vol_t5 <- lag(vol_t0, 5)
vol_DiffPerc_t0 <- (vol_t0 - lag(vol_t0, 1))/lag(vol_t0, 1) * 100
vol_DiffPerc_t1 <- lag(vol_DiffPerc_t0, 1)
vol_DiffPerc_t2 <- lag(vol_DiffPerc_t0, 2)
vol_DiffPerc_t3 <- lag(vol_DiffPerc_t0, 3)
vol_DiffPerc_t4 <- lag(vol_DiffPerc_t0, 4)
vol_DiffPerc_t5 <- lag(vol_DiffPerc_t0, 5)

volDF <- vol_t0
volDF <- merge(volDF, vol_t1, vol_t2, vol_t3, vol_t4, vol_t5)
colnames(volDF) <- c("VOL_t0", "VOL_t1", "VOL_t2", "VOL_t3", "VOL_t4", "VOL_t5")
volPerDF <- vol_DiffPerc_t0
volPerDF <- merge(volPerDF, vol_DiffPerc_t1, vol_DiffPerc_t2, vol_DiffPerc_t3, vol_DiffPerc_t4, vol_DiffPerc_t5)
colnames(volPerDF) <- c("VOL_PER_t0", "VOL_PER_t1", "VOL_PER_t2", "VOL_PER_t3", "VOL_PER_t4", "VOL_PER_t5")
priceDF <- merge(priceDF, volDF)
priceDF <- merge(priceDF, volPerDF)

########################################################################
############################# TARGET  ##################################
########################################################################
highDF_3 <- lag(priceList$High, -1)
highDF_3 <- merge(highDF_3, lag(priceList$High, -2), lag(priceList$High, -3))
maxHigh <- apply(highDF_3, 1, max)
maxHighDiffPerc <- (maxHigh - lag(priceList$Open, -1))/lag(priceList$Open, -1) * 100
lowDF_3 <- lag(priceList$Low, -1)
lowDF_3 <- merge(lowDF_3, lag(priceList$Low, -2), lag(priceList$Low, -3))
maxLow <- apply(lowDF_3, 1, min)
maxLowDiffPerc <- (lag(priceList$Open, -1) - maxLow)/lag(priceList$Open, -1) * 100

###################### BUY ONLY #######################################
buyTarget <- ifelse(maxHighDiffPerc$Open > txnCostinPerc, "success", "fail")
#buyTarget <- ifelse(maxHighDiffPerc$Open > txnCostinPerc, 1, -1)
colnames(buyTarget) <- c("TARGET")
buyTarget$TARGET <- as.factor(buyTarget$TARGET)
buyDF <- priceDF[, 6:(ncol(priceDF) - 6)] # remove OHLC raw and vol perc columns
buyDF <- merge(buyDF, buyTarget)

########################################################################
########################### PREDICTION  ################################
########################################################################

###################### LOGIT #######################################
library(caTools)
set.seed(123)
logitDF <- buyDF[, -ncol(buyDF)]
logitTarget <- ifelse(maxHighDiffPerc$Open > txnCostinPerc, 1, 0)
colnames(logitTarget) <- c("TARGET")
logitDF <- merge(logitDF, logitTarget)
dataset <- logitDF
dataset <- na.omit(dataset)
#dataset <- dataset[1000:5000, ]  #testing
dataset <- dataset[300000:nrow(dataset), ]  #testing
split = sample.split(dataset$TARGET, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
training_set[, -ncol(training_set)] = scale(training_set[, -ncol(training_set)])
test_set[, -ncol(test_set)] = scale(test_set[, -ncol(test_set)])

logitClassifier = glm(formula = TARGET ~ .,
                 family = binomial,
                 data = training_set)
logit_pred = predict(logitClassifier, type = 'response', newdata = test_set[, -ncol(test_set)])
y_pred_logit = ifelse(logit_pred > 0.01, 1, 0)
cmLogit = table(test_set[, ncol(test_set)], y_pred_logit > 0.5)
acLogit = (cmLogit[1,1] + cmLogit[2,2])/(cmLogit[1,1] + cmLogit[1,2] + cmLogit[2,1] + cmLogit[2,2]) * 100
sucHitLogit = cmLogit[2,2]/(cmLogit[2,1] + cmLogit[2,2]) * 100
print(acLogit)
print(sucHitLogit)

###################### SVM #######################################
library(caTools)
set.seed(123)
dataset <- buyDF
dataset <- na.omit(dataset)
dataset <- dataset[1000:5000, ]  #testing
split = sample.split(dataset$TARGET, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
training_set[, -ncol(training_set)] = scale(training_set[, -ncol(training_set)])
test_set[, -ncol(test_set)] = scale(test_set[, -ncol(test_set)])

library(e1071)
svmClassifier = svm(formula = TARGET ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial',
                 cross = 10,
                 gamma = 0.000000000000004,
                 cost = 0.00000003)
y_pred_svm = predict(svmClassifier, newdata = test_set[, -ncol(test_set)])
cmSvm = table(test_set[, ncol(test_set)], y_pred_svm)
acSvm = (cmSvm[1,1] + cmSvm[2,2])/(cmSvm[1,1] + cmSvm[1,2] + cmSvm[2,1] + cmSvm[2,2]) * 100
print(acSvm)