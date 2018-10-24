

LatePayments <- c(1,2,3,4,5,6,NA,42, NA)

df <- data.frame(LatePayments)

df$LatePaymentBin <- cut(LatePayments,breaks=c(-Inf,6,Inf), labels=c("CountLatePaymentBinLow","CountLatePaymentBinHigh"))