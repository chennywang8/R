library(forecast)

# ============ READ RAW DATA =============
raw <- readxl::read_xls("SouvenirSales.xls", skip = 0)
tsf <- ts(log(raw$Sales), start = c(1995,1,1), deltat = 1/12)
plot.ts(tsf)

# ===== HOLT EXPONENTIAL SMOOTHING =====
fc_smooth <- HoltWinters(tsf)
plot(fc_smooth)
print(fc_smooth$fitted)
sse <- fc_smooth$SSE


# ============= PREDICT NEXT 8 YEAR =======
fc_predict <- forecast(fc_smooth, h = 48)
print(fc_predict)
plot(fc_predict)
