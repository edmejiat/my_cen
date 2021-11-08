###### 

## Lo primero es sacar los clientes que pertenecen al club

bd_360<-function(ruta=""){
  
  seguimiento=osPathJoin(datosPath,ruta)
  
  all_clients=fread(osPathJoin(seguimiento,"all_customers.csv"),encoding="UTF-8")
  
  all_clients$Email=tolower(all_clients$Email)
  all_clients$Email=gsub(" ", "", all_clients$Email, fixed = TRUE)
  
  correos=unique(club_cen[,.(correo)])
  correos_shopify=unique(shopify[,.(customer_email)])
  correos_all_clients=unique(all_clients[,.(Email)])
  
  names(correos_shopify)<-"correo"
  names(correos_all_clients)<-"correo"
  correos=rbind(correos,correos_shopify,correos_all_clients)
  rm(correos_shopify,correos_all_clients)
  correos<-unique(correos)
  
  ## Les anadimos fecha de registro, segmento y subsegmento
  
  correos=merge(correos, club_cen[,.(correo,fecha_registro,segmento,subsegmento)], all.x=TRUE)
  correos[is.na(segmento),segmento:="No inscrito"]
  correos[is.na(subsegmento),subsegmento:="No inscrito - Comprador"]
  names(correos)<-c("correo","fecha_registro_club","segmento_club","subsegmento_club")
  
  
  ## Cantidad y valor total de compras
  
  compras=unique(shopify[,.(customer_email,order_name)])
  compras=compras[,.N,by=customer_email]
  
  ventas=unique(shopify[,.(customer_email,order_name,total_sales)])
  ventas=ventas[,.(venta_total=sum(total_sales)),by=customer_email]
  
  ventas_compras=merge(compras, ventas, all.x=TRUE)
  names(ventas_compras)[2]<-"cantidad_compras_total"
  names(ventas_compras)[1]<-"correo"
  
  correos=merge(correos, ventas_compras, all.x=TRUE)
  
  correos[is.na(cantidad_compras_total),cantidad_compras_total:=0]
  correos[is.na(venta_total),venta_total:=0]
  
  ## Cantidad y valor de las compras por year
  
  
  
  for (i in 1:length(unique(shopify$year))) {
    
    assign(paste("year_",unique(shopify$year)[i],sep = ""), unique(shopify$year)[i])
    my_year=shopify[year==unique(shopify$year)[i]]
    
    compras=unique(my_year[,.(customer_email,order_name)])
    compras=compras[,.N,by=customer_email]
    
    ventas=unique(my_year[,.(customer_email,order_name,total_sales)])
    ventas=ventas[,.(venta_total=sum(total_sales)),by=customer_email]
    
    ventas_compras=merge(compras, ventas, all.x=TRUE)
    names(ventas_compras)[2]<-paste("cantidad_compras",unique(shopify$year)[i],sep = "_")
    names(ventas_compras)[3]<-paste("venta_total",unique(shopify$year)[i],sep = "_")
    names(ventas_compras)[1]<-"correo"
    
    correos=merge(correos, ventas_compras, all.x=TRUE)
    
    
  }
  
  correos[, 5:length(names(correos))][is.na(correos[, 5:length(names(correos))])] <- 0
  
  ## RFM, en cosntruccion
  
  rfm<-fread("C:/Users/Edwin.Mejia/Documents/Danilo Mejia/Proyectos/20210506-Smallville-Capsulas Express Nutresa/Datos/RFm.csv")
  rfm=rfm[,.N,by=c("correo","medalla")]
  rfm[,N:=NULL]
  names(rfm)[2]<-"medalla_rfm"
  
  correos=merge(correos, rfm, all.x=TRUE)
  correos[is.na(medalla_rfm),medalla_rfm:="Sin RFM"]
  
  ### Despues de ejecutar ranking contactabilidad
  
  correos=merge(correos, contactos_correo, all.x=TRUE)
  
  correos[is.na(ranking_contactabilidad),ranking_contactabilidad:=0]
  
  ## Primera y ultima compra
  
  fechas=shopify[,.(customer_email,day)]
  fechas$day<-as.Date(fechas$day)
  
  minimo=fechas[,.(primera_compra=min(day)),by=customer_email]
  maximo=fechas[,.(ultima_compra=max(day)),by=customer_email]
  
  fechas_unidas=merge(minimo, maximo, all.x=TRUE)
  names(fechas_unidas)[1]="correo"
  
  correos=merge(correos, fechas_unidas, all.x=TRUE)
  
  ## Genero (en construccion)
  
  genero<-fread("C:/Users/Edwin.Mejia/Documents/Danilo Mejia/Proyectos/20210506-Smallville-Capsulas Express Nutresa/Datos/inscritos club express limpiada.csv"
                ,encoding = "UTF-8")
  
  genero=genero[,.(correo,Sexo,telefono,contactabilidad,cedula,fecha_nacimiento,edad,direccion,ciudad,departamento)]
  
  correos=merge(correos, genero, all.x=TRUE)
  
  ## tickets promedio  de las compras por year
  
  
  
  for (i in 1:length(unique(shopify$year))) {
    
    assign(paste("prom_productos_",unique(shopify$year)[i],sep = ""), unique(shopify$year)[i])
    my_year=shopify[year==unique(shopify$year)[i]]
    
    compras=my_year[,.N,by=c("customer_email","order_name")]
    compras=compras[,.(promedio=mean(N)),by=(customer_email)]
    
    names(compras)[2]<-paste("promedio_productos",unique(shopify$year)[i],sep = "_")
    names(compras)[1]<-"correo"
    
    correos=merge(correos, compras, all.x=TRUE)
    
    
  }
  
  
  
  
  
  ## compras intermedias primera, ultima
  
  fechas=shopify[,.(customer_email,day)]
  ordenes=shopify[,.(customer_email,day,order_name)]
  ordenes$day<-as.Date(ordenes$day)
  fechas$day<-as.Date(fechas$day)
  
  minimo=fechas[,.(primera_compra=min(day)),by=customer_email]
  minimo$union="Test"
  maximo=fechas[,.(ultima_compra=max(day)),by=customer_email]
  maximo$union2="Test"
  
  f_minimo=copy(fechas)
  
  f_minimo=merge(f_minimo, minimo,by.x = c("customer_email","day"),by.y = c("customer_email","primera_compra"), all.x=TRUE)
  f_minimo=f_minimo[is.na(union)]
  
  f_minimo=merge(f_minimo, maximo,by.x = c("customer_email","day"),by.y = c("customer_email","ultima_compra"), all.x=TRUE)
  f_minimo=f_minimo[is.na(union2)]
  
  f_minimo=merge(f_minimo, ordenes, all.x=TRUE)
  f_minimo=f_minimo[,.N,by=c("customer_email","order_name")]
  
  f_minimo=f_minimo[,.N,by=customer_email]
  names(f_minimo)[2]="Compras intermedias"
  names(f_minimo)[1]="correo"
  
  correos=merge(correos, f_minimo, all.x=TRUE)
  
  ##  Compro maquina en algun momento
  
  maquinas=shopify[product_type=="maquina"]
  maquinas=maquinas[,.N,by=customer_email]
  maquinas$compro_maquina="Si"
  maquinas[,N:=NULL]
  names(maquinas)[1]="correo"
  
  correos=merge(correos, maquinas, all.x=TRUE)
  
  
  ## Promedio de compra, de capsulas y cantidad
  
  capsulas=shopify[product_type=="capsula"]
  
  for (i in 1:length(unique(shopify$year))) {
    
    assign(paste("prom_compras_capsulas_",unique(shopify$year)[i],sep = ""), unique(shopify$year)[i])
    my_year=capsulas[year==unique(shopify$year)[i]]
    
    compras=my_year[,.(compra=sum(total_sales)),by=c("customer_email","order_name")]
    compras=compras[,.(promedio_compra=mean(compra)),by=(customer_email)]
    
    names(compras)[2]<-paste("promedio_compra_capsulas",unique(shopify$year)[i],sep = "_")
    names(compras)[1]<-"correo"
    
    correos=merge(correos, compras, all.x=TRUE)
    
    
  }
  
  
  fwrite(correos,osPathJoin(resultadosPath,"bd_360.csv"),sep = ";")
  
  write_xlsx(correos, osPathJoin(resultadosPath,"bd_360.xlsx"))
  
  write_xlsx(data, "data/cereal-yield-1990-to-1995.xlsx")
  
}







