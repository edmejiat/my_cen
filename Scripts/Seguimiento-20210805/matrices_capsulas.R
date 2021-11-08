## Tabla de % de participacion de pedidos

capsulas=unique(shopify[product_type=="capsula" & year=="2021",.(product_title,order_name,month)])

cantidad_pedidos_mes=capsulas[,.N,by=c("month")][order(month)]
cantidad_pedidos_mes$product_title="Cantidad total de pedidos"
cantidad_pedidos_mes=dcast(cantidad_pedidos_mes,product_title~month,value.var = "N")

capsulas=capsulas[,.N,by=c("product_title","month")]

capsulas=dcast(capsulas,product_title~month,value.var = "N")

by_presencia=rbind(capsulas,cantidad_pedidos_mes)
by_presencia[is.na(by_presencia)] <- 0

nombres=names(by_presencia)

porcentajes=copy(by_presencia)

div <- mapply('/', porcentajes[,2:dim(porcentajes)[2]], as.vector(porcentajes[product_title=="Cantidad total de pedidos",2:dim(porcentajes)[2]]))
div=setDT(as.data.frame(div))
week <- paste(names(div), " %", sep="")
names(div)=week

porcentajes=cbind(porcentajes[,1],div)

final_by_presencia=merge(by_presencia, porcentajes, all.x=TRUE)

## Tabla de % de cantidades

capsulas=shopify[product_type=="capsula" & year=="2021",.(product_title,order_name,month,net_quantity)]

cantidad_pedidos_mes=capsulas[,.(total=sum(net_quantity)),by=c("month")][order(month)]
cantidad_pedidos_mes$product_title="Cantidad total de productos"
cantidad_pedidos_mes=dcast(cantidad_pedidos_mes,product_title~month,value.var = "total")

capsulas=capsulas[,.(total=sum(net_quantity)),by=c("product_title","month")]

capsulas=dcast(capsulas,product_title~month,value.var = "total")

by_cantidad=rbind(capsulas,cantidad_pedidos_mes)
by_cantidad[is.na(by_cantidad)] <- 0

nombres=names(by_cantidad)
porcentajes=copy(by_cantidad)

div <- mapply('/', porcentajes[,2:dim(porcentajes)[2]], as.vector(porcentajes[product_title=="Cantidad total de productos",2:dim(porcentajes)[2]]))
div=setDT(as.data.frame(div))
week <- paste(names(div), " %", sep="")
names(div)=week

porcentajes=cbind(porcentajes[,1],div)

final_by_cantidad=merge(by_cantidad, porcentajes, all.x=TRUE)

## Tabla de % de valor ventas

capsulas=shopify[product_type=="capsula" & year=="2021",.(product_title,order_name,month,total_sales)]

cantidad_pedidos_mes=capsulas[,.(total=sum(total_sales)),by=c("month")][order(month)]
cantidad_pedidos_mes$product_title="Ventas totales"
cantidad_pedidos_mes=dcast(cantidad_pedidos_mes,product_title~month,value.var = "total")

capsulas=capsulas[,.(total=sum(total_sales)),by=c("product_title","month")]

capsulas=dcast(capsulas,product_title~month,value.var = "total")

by_ventas=rbind(capsulas,cantidad_pedidos_mes)
by_ventas[is.na(by_ventas)] <- 0

nombres=names(by_ventas)
porcentajes=copy(by_ventas)

div <- mapply('/', porcentajes[,2:dim(porcentajes)[2]], as.vector(porcentajes[product_title=="Ventas totales",2:dim(porcentajes)[2]]))
div=setDT(as.data.frame(div))
week <- paste(names(div), " %", sep="")
names(div)=week

porcentajes=cbind(porcentajes[,1],div)

final_by_ventas=merge(by_ventas, porcentajes, all.x=TRUE)

## Escribir En excel

suppressMessages(library(openxlsx))

OUT <- createWorkbook()

addWorksheet(OUT, "final_by_ventas")
addWorksheet(OUT, "final_by_cantidad")
addWorksheet(OUT, "final_by_presencia")

writeData(OUT, sheet = "final_by_ventas", x = final_by_ventas)
writeData(OUT, sheet = "final_by_cantidad", x = final_by_cantidad)
writeData(OUT, sheet = "final_by_presencia", x = final_by_presencia)

saveWorkbook(OUT, osPathJoin(resultadosPath,"segumiento.xlsx"),overwrite=TRUE)


