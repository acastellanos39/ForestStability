model.eval <- function(me) {
  min.pres <- min(me@presence)
  pres10 <- quantile(me@presence, 0.10)
  max.TNPR <- threshold(me, "spec_sens")
  RES <- list(me@auc, abs(sum(me@presence > min.pres)/length(me@presence) - 1), me@TPR[which(me@t > min.pres)[1]] + me@TNR[which(me@t > min.pres)[1]] - 1, abs(sum(me@presence > pres10)/length(me@presence) - 1), me@TPR[which(me@t > pres10)[1]] + me@TNR[which(me@t > pres10)[1]] - 1, abs(sum(me@presence > max.TNPR)/length(me@presence) - 1), me@TPR[which(me@t > max.TNPR)[1]] + me@TNR[which(me@t > max.TNPR)[1]] - 1)
  names(RES) <- c("AUC", "ORate.MPT", "TSS.MPT", "ORate.10PT", "TSS.10PT", "ORate.Max", "TSS.Max")
  RES
}