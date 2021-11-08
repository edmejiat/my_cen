########## Funciones Propias ########## 
#Diferencia en meses
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

########## Lectura de datos ########## 

compras=fread(osPathJoin(datosPath,"master sales db.csv"),encoding = "UTF-8")
clientes_club=fread(osPathJoin(datosPath,"inscritos club express limpiada.csv"),encoding = "UTF-8")
rfm=fread(osPathJoin(datosPath,"RFM.csv"),encoding = "UTF-8")


########## Tratamiento de datos ########## 
clientes_club$fecha_registro=as.Date(clientes_club$fecha_registro)

compras$fecha=as.Date(compras$fecha)


########## Calculo de nuevas columnas ########## 
#Meses de inscrito al club
clientes_club[,meses_insrito:=elapsed_months(today(),fecha_registro)]
clientes_club$meses_insrito[is.na(clientes_club$meses_insrito)] <- 0

clientes_club[meses_insrito<13,year_inscrito:="Menos de 1 año"]
clientes_club[meses_insrito>12 & meses_insrito<25,year_inscrito:="2años"]
clientes_club[is.na(year_inscrito),year_inscrito:="mas de 3 años"]

clientes_club[,.N,by="year_inscrito"]

# hist(clientes_club$meses_insrito)

# a=clientes_club[,.N,by=meses_insrito]
# fwrite(a,osPathJoin(resultadosPath,"antiguedad_meses.csv"),sep = "|")
#rm(a)



#Primera y ultima compra
primera_compra=compras[,.(primera_compra=min(fecha)),by=correo]
ultima_compra=compras[,.(ultima_compra=max(fecha)),by=correo]

historico_compras=merge(primera_compra, ultima_compra, all.x=TRUE)
rm(primera_compra, ultima_compra)


clientes_club_compras=merge(historico_compras, clientes_club, all=FALSE)

cantidad_compras=compras[,.N,by=c("nombre_orden","correo")]
cantidad_compras=cantidad_compras[,.N,by=correo]
names(cantidad_compras)=c("correo","cantidad_compras")

clientes_club_compras=merge(clientes_club_compras, cantidad_compras, all.x=TRUE)
rm(cantidad_compras)

clientes_club_compras[,tiempo_ultima_primera:=elapsed_months(ultima_compra,primera_compra)]

# a=(clientes_club_compras[cantidad_compras==2])[,.N,by=tiempo_ultima_primera]
# fwrite(a,osPathJoin(resultadosPath,"ultima_primera_2meses.csv"))

club_all=merge(clientes_club_compras, compras, all=FALSE)


########## Ticket Promedio ##########
#Cantidad de compras
cantidad_compras=unique(compras[linea_venta=="product" ,c("nombre_orden","correo")])
cantidad_compras=cantidad_compras[,.N,by="correo"]

ticket_prom=merge(cantidad_compras, clientes_club, all=FALSE)

precentil=ticket_prom[year_inscrito=="Menos de 1 año" ,c("N")]
quantile(precentil$N)

ticket_prom[,.(ultima_compra=mean(N)),by=year_inscrito]


#Cantidad de productos
cantidad_productos=compras[linea_venta=="product" ,.N,by="correo"]

ticket_prom_productos=merge(cantidad_productos, clientes_club, all=FALSE)

precentil=ticket_prom_productos[year_inscrito=="mas de 3 años",c("N")]
quantile(precentil$N)

ticket_prom_productos[,.(ultima_compra=mean(N)),by=year_inscrito]

#Valor promedio de la compra
promedio_compra=compras[linea_venta=="product" ,.(promedio_compra=mean(valor_promedio_orden)),by=correo]

ticket_prom_valor=merge(promedio_compra, clientes_club, all=FALSE)

precentil=ticket_prom_valor[year_inscrito=="Menos de 1 año",c("promedio_compra")]
quantile(precentil$promedio_compra)

ticket_prom_valor[,.(ultima_compra=mean(promedio_compra)),by=year_inscrito]

#Unicas compras
unica_compra=unique(compras[linea_venta=="product",c("nombre_orden","correo")])
unica_compra=unica_compra[,.N,by="correo"]
unica_compra=unica_compra[N==1]

unica_compra_club=merge(unica_compra, clientes_club, all=FALSE)

unica_compra_club[,.N,by="year_inscrito"]

fwrite(clientes_club_compras,osPathJoin(resultadosPath,"clientes_club_compras.csv"),sep = "|")



