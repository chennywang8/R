library(forcats)

# ============ READ RAW DATA =============
raw <- scan("skirts.dat", skip = 5)
tsf <- ts(raw, start = c(1866))
plot.ts(tsf)

# ===== HOLT EXPONENTIAL SMOOTHING =====
fc_smooth <- HoltWinters(tsf, gamma = FALSE)
plot(fc_smooth)
print(fc_smooth$fitted)
sse <- fc_smooth$SSE


# ============= PREDICT NEXT 8 YEAR =======
fc_predict <- forecast(fc_smooth, h = 8)
print(fc_predict)
plot(fc_predict)
