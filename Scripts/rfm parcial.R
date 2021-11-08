## Funcion que crea el modeo RFM para CEN, este modelo tiene en cuenta 3 matices, 
## RFM signature Pedro, sin modificaciones,
## El modelo RFM con ajuste de consumo promedio
## El All new RFm model Signature Danilo


modelo_rfm<-function(shopify,corte=today(),delta=90){
  
  
  ## Signature Pedro
  corte=as.Date(corte)
  
  fecha_corte=corte-days(delta)
  
  rfm_medalla=fread("C:/Users/Edwin.Mejia/Documents/Danilo Mejia/Proyectos/CEN/Datos/rfm_medalla.csv",encoding="UTF-8")%>%as.data.table()
  rfm_medalla$rfm=as.character(rfm_medalla$rfm)
  
  my_shopify=shopify[,.(order_id,customer_email,customer_id,total_sales,day,week,month,quarter,year,day_of_week,month_of_year)]
  
  ### By factura
  
  my_shopify=my_shopify[,.(total_venta=sum(total_sales)),by=.(order_id,customer_id,customer_email,day,week,month,quarter,year,day_of_week,month_of_year)]
  
  
  ##Calculo de las variables de RFM
  
  #Recencia
  
  rfm=shopify[day>= fecha_corte,.(ultima_compra=max(day)),by=c("customer_id","customer_email")]
  
  rfm[,recencia:=round(as.numeric(difftime(corte,ultima_compra, units="days"), units="days"),0)]
  rfm[,ultima_compra :=NULL]
  
  #Frecuencia
  
  fm=shopify[day>=fecha_corte]
  frecuencia=fm[,.N,by=customer_id]
  names(frecuencia)[2]="frecuencia"
  
  monto=fm[,.(monto=sum(total_sales)),by=customer_id]
  
  rfm=merge(rfm, frecuencia, all.x=TRUE)
  rfm=merge(rfm, monto, all.x=TRUE)
  
  rfm[is.na(rfm)] <- 0
  
  ### Discrtizar las variables
  #Recencia
  
  rfm$tercil_recencia=cut(rfm$recencia, breaks=unique(quantile(rfm$recencia, probs = seq(0, 1, 1/3))), 
                          labels=c("3","2","1"), include.lowest=TRUE)
  
  #Frecuencia
  
  rfm$tercil_frecuencia=cut(rfm$frecuencia, breaks=unique(quantile(rfm$frecuencia, probs = seq(0, 1, 0.25))), 
                            labels=c("1","2","3"), include.lowest=TRUE)
  
  #Monto
  rfm$tercil_monto=cut(rfm$monto, breaks=unique(quantile(rfm$monto, probs = seq(0, 1, 1/3))), 
                       labels=c("1","2","3"), include.lowest=TRUE)
  
  rfm[,rfm:=paste(as.character(tercil_recencia),as.character(tercil_frecuencia),as.character(tercil_monto),sep = "")]
  
  rfm=merge(rfm, rfm_medalla,by.x = "rfm",by.y = "rfm", all.x=TRUE)
  
  rfm[,.N,by=medalla]
  
  ######
  
  final=unique(shopify[,.(customer_id,customer_email)])
  
  final=merge(final, rfm,by.x=c("customer_id","customer_email"),by.y=c("customer_id","customer_email"), all.x=TRUE)
  
  ## Promedio de consumo de capsulas
  
  productos=shopify[product_type =="capsula",.N,by=product_title ]
  productos[product_title %in% c("Pack surtido x 30","Caja surtida x 30","Surtido Perfecto Cápsulas Express"),cantidad_capsulas:=30]
  productos[product_title %in% c("Pack Surtido"),cantidad_capsulas:=10]
  productos[product_title %in% c("Kit Surtido de Cápsulas Express"),cantidad_capsulas:=16]

  productos[is.na(cantidad_capsulas),cantidad_capsulas:=10]
  productos[,N:=NULL]
  
  my_productos=shopify[product_type =="capsula",.(total_Cajas=sum(net_quantity)),by=c("product_title","day","customer_email")]
  
  my_productos=merge(my_productos,productos,by.x="product_title",by.y="product_title", all.x=TRUE)
  my_productos[,total_capsulas:=cantidad_capsulas*total_Cajas]
  
  ## Primera y ultima compra
  
  fechas=shopify[,.(customer_email,day)]
  fechas$day<-as.Date(fechas$day)
  
  minimo=fechas[,.(primera_compra=min(day)),by=customer_email]
  
  my_productos=merge(my_productos,minimo,by.x="customer_email",by.y="customer_email", all.x=TRUE)
  my_productos=my_productos[,.(total_capsulas=sum(total_capsulas)),by=c("primera_compra","customer_email")]
  my_productos[,total_dias:=as.integer(as.Date(today(),"%Y-%m-%d") - as.Date(primera_compra,"%Y-%m-%d"))]
  
  my_productos[,promedio_dia:=total_capsulas/total_dias][order(-total_capsulas)]
  my_productos[,total_dias:=NULL]
  
  final=merge(final, my_productos,by.x=c("customer_email"),by.y=c("customer_email"), all.x=TRUE)
  final[,medalla_ajustado:=ifelse(promedio_dia<1,medalla,"oro")]
  final[medalla!=medalla_ajustado]
  
  ## RFM Signature Danilo
  library(rfm)
  
  my_shopify=shopify[,.(order_id,total_sales,customer_email,day)]
  
  my_shopify=my_shopify[,.(ventas=sum(total_sales)),by=c("order_id","customer_email","day")]
  my_shopify[,order_id:=NULL]
  
  my_shopify$day=as.Date(my_shopify$day)
  
  analysis_date <- today()-delta
  rfm_result <- rfm_table_order(my_shopify, customer_email, day  , ventas, today())
  
  rfm_result=setDT(rfm_result$rfm)
  rfm_result$rfm_score=as.character(rfm_result$rfm_score)
  
  segmentos <- fread("C:/Users/Edwin.Mejia/Documents/Danilo Mejia/Proyectos/CEN/Datos/Ultimate RFM.csv",encoding="UTF-8")%>%as.data.table()
  segmentos[,RFM:=paste(R,F,M,sep = "")]
  
  final_my_rfm=merge(rfm_result, segmentos,by.x="rfm_score",by.y="RFM", all.x=TRUE)
  
  final_my_rfm=final_my_rfm[,.(customer_id,Segmento,Caracteristicas)]
  
  final=merge(final, final_my_rfm,by.x=c("customer_email"),by.y=c("customer_id"), all.x=TRUE)
  
  return(final)
}