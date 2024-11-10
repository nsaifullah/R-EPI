library(readxl)

df = read_excel("DataTomfoolery/RTesting.xlsx")
recode_list = list(Yes=1, No=0, Somewhat=0.5)
recode_response = function(x) {
  recode_list[x]
}
output = cbind(df[, 1], sapply(df[, 2:4], recode_response))
