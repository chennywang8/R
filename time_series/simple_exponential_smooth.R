library(forecast)

# ============ READ RAW DATA =============
raw <- scan("precip1.dat", skip = 1, sep = " ")
raw <- raw[!is.na(raw)]
tsf <- ts(raw, start = c(1813))
plot.ts(tsf)


# ===== SIMPLE EXPONENTIAL SMOOTHING =====
fc_smooth <- HoltWinters(tsf, beta = FALSE, gamma = FALSE)
plot(fc_smooth)
print(fc_smooth$fitted)
sse <- fc_smooth$SSE


# ============= PREDICT NEXT 8 YEAR =======
fc_predict <- forecast(fc_smooth, h = 8)
print(fc_predict)
plot(fc_predict)

