# Detectando las compras que no fueron maquina

rfm=unique(rfm[,.N,by=c("correo","medalla")])
clientes_club_compras=merge(clientes_club_compras, rfm, all.x=TRUE)

club_all=merge(clientes_club_compras, compras, all=FALSE)

club_all_temporal=unique(club_all[tipo!="",c("nombre_orden","tipo")])
  
club_all_temporal=club_all_temporal[, lapply(.SD, paste0, collapse="-"), by = nombre_orden]



a=club_all_temporal[grepl( "máquina", tipo, fixed = TRUE)==FALSE]
a[,.N,by=tipo]


sin_maquina=club_all[nombre_orden %in% a$nombre_orden & linea_venta=="product"]

cantidad_compras=unique(sin_maquina[linea_venta=="product" & nombre_orden %in% a$nombre_orden,c("nombre_orden","correo")])
cantidad_compras=cantidad_compras[,.N,by="correo"]


percentil=cantidad_compras[,c("N")]
quantile(percentil$N)

cantidad_compras[,.N,by=N]

sin_maquina[cantidad_compras==1]


cantidad_productos=sin_maquina[linea_venta=="product" ,.N,by="correo"]
percentil=cantidad_productos[,c("N")]
quantile(percentil$N)

ticket_prom_productos=merge(cantidad_productos, clientes_club, all=FALSE)

cantidad_productos[,.(ultima_compra=mean(N)),by=year_inscrito]



promedio_compra=sin_maquina[linea_venta=="product",.(promedio_compra=mean(valor_promedio_orden)),by=correo]
percentil=promedio_compra[,c("promedio_compra")]
quantile(percentil$promedio_compra)
