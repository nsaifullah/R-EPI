library(readxl)
library(dplyr)
library(lubridate)

setwd("C:/Users/nikhi/Dropbox/HustleBustle/Working/TheFuture/RecentApps/IHME")
df = read_excel("PDCDemo_DTA.xlsx")
df = df[order(df$PatUID, df$fill_order),]
df$FillDate = as.Date(df$FillDate)
df$Disenrollment = as.Date(df$Disenrollment)
df$EOFillRangeDate = as.Date(df$EOFillRangeDate)

df$fill_left_bound = df$FillDate - ymd("2021-01-01") + 1
df$fill_right_bound = df$EOFillRangeDate - ymd("2021-01-01") + 1
df$de_bound = df$Disenrollment - ymd("2021-01-01") + 1
X = matrix(rep(0, 365), ncol = 365, nrow = nrow(df))

#I know there's a better way to do this, but here we are
for (i in 1:nrow(X)) {
  X[i, df$fill_left_bound[i]:df$fill_right_bound[i]] = 1
  if (df$de_bound[i] != 365) {
    X[i, df$de_bound[i]:365] = NA
  }
}

#write.csv(X, "PreAggTest.csv")
df_calc = cbind(df, X)
df_calc = df_calc[,c(1, 12:376)]
pdc_calc = group_by(df_calc, PatUID) %>% summarise_all(., sum)

#Given sorting of the input data, we can simply perform list-wise arithmetic to arrive at the final PDCs
daysCovered = rowSums(pdc_calc[,-1], na.rm = T, 1)
for_denom = mapply(is.na, pdc_calc[,-1])
denom = 365-rowSums(for_denom)-unlist(lapply(as.numeric(df[df$fill_order == 1,]$fill_left_bound), function(x) x-1))
pdc = unlist(lapply(daysCovered/denom, function(z) if (z > 1) {z = 100} else {z*100}))
