library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)


compras_productos=compras[linea_venta=="product" & total_ventas!=0]

compras_productos=compras_productos[,c("producto","nombre_orden")]

compras_productos=compras_productos[, lapply(.SD, paste0, collapse="|"), by = nombre_orden]


write.csv(compras_productos,osPathJoin(resultadosPath,"market_basket_transactions.csv"), quote = FALSE, row.names = FALSE)


tr <- read.transactions(osPathJoin(resultadosPath,"market_basket_transactions.csv"), format = 'basket', sep='|')


summary(tr)

if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}

itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), main="Frecuencia abosluta",ylab  = "Frecuencia")

association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=5))

inspect(association.rules[1:4])

shorter.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=5))

inspect(shorter.association.rules)

a=as(association.rules, "data.frame")
