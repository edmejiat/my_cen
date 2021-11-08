########## Lectura de los datos ########## 

## Las dos tablas se deben llamar clientes_con_cuenta y shopify

#ruta=""

carga_limpieza<-function(ruta=""){
  
  seguimiento=osPathJoin(datosPath,ruta)
  
  ## Clientes inscritos en el portal
  clientes_con_cuenta=fread(osPathJoin(seguimiento,"clientes_con_cuenta.csv"),encoding="UTF-8")
  clientes_con_cuenta$Email=tolower(clientes_con_cuenta$Email)
  clientes_con_cuenta$Email=gsub(" ", "", clientes_con_cuenta$Email, fixed = TRUE)
  
  ## Las ventas de shopify
  shopify=fread(osPathJoin(seguimiento,"shopify.csv"),encoding="UTF-8")%>%as.data.table()
  ##Con esta linea solo considero los porductos y los pagos confirmados
  shopify<<-shopify[financial_status=="paid" & sale_line_type=="product"]
  shopify$customer_email=tolower(shopify$customer_email)
  shopify$customer_email=gsub(" ", "", shopify$customer_email, fixed = TRUE)
  
  
  ## Los clientes del club
  con <- dbConnect(RMySQL::MySQL(), 
                   dbname = "Fidelización_capsulas_express", 
                   host = "colcaferdspdn.cowvh3qhlapi.us-east-1.rds.amazonaws.com", 
                   port = 3306,
                   user = "colcpdnadmin",
                   password = "NTe04QMM1Flj.")
  
  clientes_club <- dbGetQuery(con, "SELECT * FROM Fidelización_capsulas_express.USUARIOS_INSCRITOS")
  setDT(clientes_club)
  
  clientes_club$FECHA_REGISTRO= as.Date(clientes_club$FECHA_REGISTRO)
  clientes_club$USUARIOS_INSCRITOS_ID=tolower(clientes_club$USUARIOS_INSCRITOS_ID)
  clientes_club$USUARIOS_INSCRITOS_ID=gsub(" ", "", clientes_club$USUARIOS_INSCRITOS_ID, fixed = TRUE)
  
  club_cen<<-create_club_cen(shopify,clientes_con_cuenta,clientes_club)
  
  rm(clientes_club,clientes_con_cuenta,con)
  
  fwrite(club_cen,osPathJoin(seguimiento,"club_cen.csv"),sep = "|")
}

## Clientes club
create_club_cen<-function(compras,con_cuenta,vpn){
  
  bd_compras=copy(compras)
  bd_cuentas=copy(con_cuenta)
  bd_clientes=copy(vpn)
  
  bd_compras$customer_email=gsub("[[:space:]]", "_", bd_compras$customer_email)
  lista_f=tolower(bd_compras$customer_email)
  
  bd_cuentas$Email=gsub("[[:space:]]", "_", bd_cuentas$Email)
  lista_f=tolower(bd_cuentas$Email)
  
  bd_clientes$USUARIOS_INSCRITOS_ID=gsub("[[:space:]]", "_", bd_clientes$USUARIOS_INSCRITOS_ID)
  lista_f=tolower(bd_clientes$USUARIOS_INSCRITOS_ID)
  
  # bd_compras=copy(shopify)
  # bd_cuentas=copy(clientes_con_cuenta)
  # bd_clientes=copy(clientes_club)
  # 
  ### Clientes Incritos en el club
  
  bd_club=unique(bd_clientes[,.(USUARIOS_INSCRITOS_ID,FECHA_REGISTRO,NOMBRES,APELLIDOS,CELULAR,DIRECCION,DETALLE_DIRECCION,CIUDAD,DEPARTAMENTO,CONTACTO)])
  names(bd_club)=c("correo","fecha_registro","nombre","apellido","telefono","direccion","detalle_direccion","ciudad","departamento","contactabilidad")
  bd_club$fecha_registro=as.Date(bd_club$fecha_registro)
  
  bd_club$segmento="Inscritos Club"
  bd_club=bd_club[,subsegmento:=ifelse(correo %in% bd_compras$customer_email,"Inscrito Comprador","Inscrito no Comprador")]
  
  ### Clientes con cuentas creadas
  
  bd_cuentas=bd_cuentas[`Accepts Marketing`=="yes"]
  
  bd_cuentas_pre=unique(bd_cuentas[,.(Email) ])
  names(bd_cuentas_pre)="correo"
  bd_cuentas_pre$fecha_registro='2019-06-01'
  
  bd_cuentas=merge(bd_cuentas_pre, bd_cuentas,by.x="correo",by.y="Email", all.x=TRUE)
  bd_cuentas=bd_cuentas[,.(correo,fecha_registro,`First Name`,`Last Name`,Phone,Address1,Address2,City,Province)]
  names(bd_cuentas)=c("correo","fecha_registro","nombre","apellido","telefono","direccion","detalle_direccion","ciudad","departamento")
  
  bd_cuentas$fecha_registro=as.Date(bd_cuentas$fecha_registro)
  
  bd_cuentas[,contactabilidad:= '| email | sms |']
  
  bd_cuentas$segmento="Inscritos Club"
  bd_cuentas=bd_cuentas[,compran:=ifelse(correo %in% bd_compras$customer_email,1,0)]
  bd_cuentas=bd_cuentas[compran==1]
  bd_cuentas$subsegmento="Inscrito Comprador"
  
  bd_cuentas[,compran:=NULL]
  bd_cuentas=bd_cuentas[,en_club:=ifelse(correo %in% bd_clientes$USUARIOS_INSCRITOS_ID,1,0)]
  bd_cuentas=bd_cuentas[en_club==0]
  bd_cuentas[,en_club:=NULL]
  
  bd_club=rbind(bd_club,bd_cuentas)
  
  bd_club=bd_club[str_count(correo)>5]
  
  bd_club[,comprador:=ifelse(correo %in% bd_compras$customer_email,1,0)]
  
  return(bd_club)
  
  
}


### BD pauta necesita la lista de todos los clientes

pauta<-function(ruta=""){
  seguimiento=osPathJoin(datosPath,ruta)
  
  all_clients=fread(osPathJoin(seguimiento,"all_customers.csv"),encoding="UTF-8")
  
  #Verificaciones y correccion correo
  all_clients$Email=tolower(all_clients$Email)
  all_clients$Email=gsub(" ", "", all_clients$Email, fixed = TRUE)
  all_clients=all_clients[str_count(Email)>5]
  
  all_clients$Phone=tolower(all_clients$Phone )
  all_clients$Phone=gsub(" ", "", all_clients$Phone , fixed = TRUE)
  all_clients$Phone=gsub("+57", "", all_clients$Phone , fixed = TRUE)
  
  all_clients=all_clients[`Accepts Marketing`=="yes"]
  
  all_clients=all_clients[,.(Email,`First Name`,`Last Name`,Phone,`Total Orders`)]
  
  all_clients$Phone=tolower(all_clients$Phone )
  all_clients$Phone=gsub(" ", "", all_clients$Phone , fixed = TRUE)
  
  all_clients[,nombre:=str_to_title(paste(`First Name`,`Last Name`,sep = " "))]
  
  all_clients=all_clients[,.(Email,nombre,Phone,`Total Orders`)]
  all_clients[,contactabilidad:= '| email | sms |']
  
  all_clients[,comprador:=ifelse(`Total Orders`>0,1,0)]
  
  all_clients=all_clients[,.(Email,nombre,Phone,contactabilidad,comprador)]
  
  all_clients[,inscrito:=ifelse(Email %in% club_cen$correo,1,0)]
  all_clients=all_clients[inscrito==0]
  names(all_clients)[1]=c("correo")
  names(all_clients)[3]=c("telefono")
  
  solo_club=club_cen[,.(correo,nombre,apellido,telefono,contactabilidad,comprador)]
  solo_club=solo_club[,nombre:=str_to_title(paste(nombre,apellido,sep = " "))]
  solo_club=solo_club[,.(correo,nombre,telefono,contactabilidad,comprador)]
  solo_club=solo_club[,inscrito:=1]
  
  bdd_pauta=rbind(solo_club,all_clients)
  
  return(bdd_pauta)
}

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

 ##Segunda hoja del archivo KPI CEN

kpi_cen_2021<-function(dataset,club,periodo){
  ###Lectura y filtrado
  
  my_shopify=copy(dataset)
  club_clientes=copy(club)
  
  my_shopify=my_shopify[month==periodo]
  #club_clientes$FECHA_REGISTRO=as.Date(club_clientes$FECHA_REGISTRO)
  
  # fecha=paste(periodo,"01",sep = "-")
  # fecha <- ymd(as.Date(fecha)) %m+% months(1)
  # club_clientes=club_clientes[FECHA_REGISTRO<fecha]
  
  club_clientes=unique(club_clientes[,.(correo)])
  
  my_shopify=my_shopify[,inscrito:=ifelse(customer_email %in% club_clientes$correo,1,0)]
  
  compradores=my_shopify[,.N,by=c("customer_email","month")]
  compradores=compradores[,.N,by=month]
  compradores$tipo="mes"
  names(compradores)[2]="Compradores"
  
  compradores_club=my_shopify[inscrito==1,.N,by=c("customer_email","month")]
  compradores_club=compradores_club[,.N,by=month]
  names(compradores_club)[2]="Compradores_club"
  
  no_compradores_club=my_shopify[inscrito==0,.N,by=c("customer_email","month")]
  no_compradores_club=no_compradores_club[,.N,by=month]
  names(no_compradores_club)[2]="No_Compradores_club"
  
  total_pedidos=my_shopify[,.N,by=c("order_name","month")]
  total_pedidos=total_pedidos[,.N,by=month]
  names(total_pedidos)[2]="Total_pedidos"
  
  total_pedidos_club=my_shopify[inscrito==1,.N,by=c("order_name","month")]
  total_pedidos_club=total_pedidos_club[,.N,by=month]
  names(total_pedidos_club)[2]="Total_pedidos_club"
  
  total_pedidos_no_club=my_shopify[inscrito==0,.N,by=c("order_name","month")]
  total_pedidos_no_club=total_pedidos_no_club[,.N,by=month]
  names(total_pedidos_no_club)[2]="Total_pedidos_no_club"
  
  
  total_monto=my_shopify[,.(total_monto=sum(total_sales)),by=c("month")]
  total_monto_club=my_shopify[inscrito==1,.(total_monto_club=sum(total_sales)),by=c("month")]
  total_monto_no_club=my_shopify[inscrito==0,.(total_monto_no_club=sum(total_sales)),by=c("month")]
  
  #### Tabla final
  
  kpicen_final=merge(compradores, compradores_club, all.x=TRUE)
  kpicen_final=merge(kpicen_final, no_compradores_club, all.x=TRUE)
  kpicen_final=merge(kpicen_final, total_pedidos, all.x=TRUE)
  kpicen_final=merge(kpicen_final, total_pedidos_club, all.x=TRUE)
  kpicen_final=merge(kpicen_final, total_pedidos_no_club, all.x=TRUE)
  kpicen_final=merge(kpicen_final, total_monto, all.x=TRUE)
  kpicen_final=merge(kpicen_final, total_monto_club, all.x=TRUE)
  kpicen_final=merge(kpicen_final, total_monto_no_club, all.x=TRUE)
  
  
  ##Acumulado
  
  periodo_year=substr(periodo,1,4)
  
  my_shopify=dataset[year==periodo_year]
  
  my_shopify=my_shopify[,inscrito:=ifelse(customer_email %in% club_clientes$correo,1,0)]
  
  compradores=my_shopify[,.N,by=c("customer_email","year")]
  compradores=compradores[,.N,by=year]
  compradores$tipo="acumulado"
  names(compradores)[2]="Compradores"
  
  compradores_club=my_shopify[inscrito==1,.N,by=c("customer_email","year")]
  compradores_club=compradores_club[,.N,by=year]
  names(compradores_club)[2]="Compradores_club"
  
  no_compradores_club=my_shopify[inscrito==0,.N,by=c("customer_email","year")]
  no_compradores_club=no_compradores_club[,.N,by=year]
  names(no_compradores_club)[2]="No_Compradores_club"
  
  total_pedidos=my_shopify[,.N,by=c("order_name","year")]
  total_pedidos=total_pedidos[,.N,by=year]
  names(total_pedidos)[2]="Total_pedidos"
  
  total_pedidos_club=my_shopify[inscrito==1,.N,by=c("order_name","year")]
  total_pedidos_club=total_pedidos_club[,.N,by=year]
  names(total_pedidos_club)[2]="Total_pedidos_club"
  
  total_pedidos_no_club=my_shopify[inscrito==0,.N,by=c("order_name","year")]
  total_pedidos_no_club=total_pedidos_no_club[,.N,by=year]
  names(total_pedidos_no_club)[2]="Total_pedidos_no_club"
  
  
  total_monto=my_shopify[,.(total_monto=sum(total_sales)),by=c("year")]
  total_monto_club=my_shopify[inscrito==1,.(total_monto_club=sum(total_sales)),by=c("year")]
  total_monto_no_club=my_shopify[inscrito==0,.(total_monto_no_club=sum(total_sales)),by=c("year")]
  
  
  kpicen_final_acum=merge(compradores, compradores_club, all.x=TRUE)
  kpicen_final_acum=merge(kpicen_final_acum, no_compradores_club, all.x=TRUE)
  kpicen_final_acum=merge(kpicen_final_acum, total_pedidos, all.x=TRUE)
  kpicen_final_acum=merge(kpicen_final_acum, total_pedidos_club, all.x=TRUE)
  kpicen_final_acum=merge(kpicen_final_acum, total_pedidos_no_club, all.x=TRUE)
  kpicen_final_acum=merge(kpicen_final_acum, total_monto, all.x=TRUE)
  kpicen_final_acum=merge(kpicen_final_acum, total_monto_club, all.x=TRUE)
  kpicen_final_acum=merge(kpicen_final_acum, total_monto_no_club, all.x=TRUE)
  
  names(kpicen_final)[1]="periodo"
  names(kpicen_final_acum)[1]="periodo"
  
  final_final=rbind(kpicen_final,kpicen_final_acum)
  
  setcolsfirst(final_final,c("tipo","periodo"))
  
  return(kpicen_final)
}

kpi_cen_acumulado_historico<-function(dataset,club,periodo,corte_rfm){
  ###Lectura y filtrado
  
  corte_rfm=as.Date(corte_rfm)
  
  my_rfm=modelo_rfm_medalla(shopify,corte = corte_rfm)
  my_rfm=my_rfm[,.N,by=medalla]
  
  my_rfm$periodo=periodo
  
  mes=dcast(my_rfm,periodo~medalla,value.var = "N")
  
  inscritos=club[substr(fecha_registro,1,7)==periodo]
  mes$inscritos=dim(inscritos)[1]
  
  inscritos_hist=club[fecha_registro<=corte_rfm]
  mes$inscritos_acum=dim(inscritos_hist)[1]
  
  my_shopify=dataset[month==periodo]
  mes$total_compradores=dim(my_shopify[,.N,by=customer_email])[1]
  
  mes$inscrito_mes=dim(my_shopify[customer_email %in% inscritos$correo,.N,by=customer_email])[1]
  
  mes=mes[,.(periodo,inscritos,inscritos_acum,total_compradores,inscrito_mes,oro,plata,bronce)]
  
  return(mes)
  
}


seguimiento_mes<-function(shopify){
  
  #Cantidad de pedidos por mes,
  pedidos=shopify[,.N,by=c("order_name","month")]
  pedidos=pedidos[,.N,by=c("month")][order(month)]
  names(pedidos)[2]="pedidos"
  
  #Cantidad de productos
  productos=shopify[,.(productos=sum(net_quantity)),by=c("month")][order(month)]
  
  #Cantidad de clientes distintos
  clientes=shopify[,.N,by=c("customer_email","month")]
  clientes=clientes[,.N,by=c("month")][order(month)]
  names(clientes)[2]="clientes"
  
  #Ventas netas
  ventas_netas=shopify[,.(ventas_netas=sum(net_sales)),by=c("month")][order(month)]
  
  #Ventas totales
  ventas_totales=shopify[,.(ventas_totales=sum(total_sales)),by=c("month")][order(month)]
  
  #Ventas por tipo de producto
  ventas_netas_by_p=shopify[product_type!="",.(ventas_netas=sum(net_sales)),by=c("month","product_type")][order(month)]
  ventas_netas_by_p=dcast(ventas_netas_by_p,month~product_type,value.var = "ventas_netas")
  names(ventas_netas_by_p)=c("month","accesorio_netas","capsula_netas","maquina_netas")
  
  ventas_totales_by_p=shopify[product_type!="",.(ventas_netas=sum(total_sales)),by=c("month","product_type")][order(month)]
  ventas_totales_by_p=dcast(ventas_totales_by_p,month~product_type,value.var = "ventas_netas")
  names(ventas_totales_by_p)=c("month","accesorio_total","capsula_total","maquina_total")
  
  ## Ventas sin los pedidos de maquinas
  maquinas=shopify[product_type=="maquina",.N,by=order_name]
  sin_maquinas=shopify[!order_name %in% maquinas$order_name]
  
  #Cantidad de pedidos por mes (sin maquinas),
  pedidos_sm=sin_maquinas[,.N,by=c("order_name","month")]
  pedidos_sm=pedidos_sm[,.N,by=c("month")][order(month)]
  names(pedidos_sm)[2]="pedidos_sin_maquinas"
  
  #Cantidad de productos (sin maquinas)
  productos_sm=sin_maquinas[,.(productos_sin_maquinas=sum(net_quantity)),by=c("month")][order(month)]
  
  #Cantidad de clientes distintos (sin maquinas)
  clientes_sm=sin_maquinas[,.N,by=c("customer_email","month")]
  clientes_sm=clientes_sm[,.N,by=c("month")][order(month)]
  names(clientes_sm)[2]="clientes_sin_maquinas"
  
  #Ventas netas (sin maquinas)
  ventas_netas_sm=sin_maquinas[,.(ventas_netas_sin_maquinas=sum(net_sales)),by=c("month")][order(month)]
  
  #Ventas totales (sin maquinas)
  ventas_totales_sm=sin_maquinas[,.(ventas_totales_sin_maquinas=sum(total_sales)),by=c("month")][order(month)]
  
  
  ## Union 
  
  final=merge(pedidos, productos, all.x=TRUE)
  final=merge(final, clientes, all.x=TRUE)
  final=merge(final, ventas_netas, all.x=TRUE)
  final=merge(final, ventas_totales, all.x=TRUE)
  final=merge(final, ventas_netas_by_p, all.x=TRUE)
  final=merge(final, ventas_totales_by_p, all.x=TRUE)
  final=merge(final, pedidos_sm, all.x=TRUE)
  final=merge(final, productos_sm, all.x=TRUE)
  final=merge(final, clientes_sm, all.x=TRUE)
  final=merge(final, ventas_netas_sm, all.x=TRUE)
  final=merge(final, ventas_totales_sm, all.x=TRUE)
  
  rm(pedidos, productos,clientes,ventas_netas,ventas_totales,ventas_netas_by_p,
     ventas_totales_by_p,pedidos_sm,productos_sm,clientes_sm,ventas_netas_sm,ventas_totales_sm,maquinas,sin_maquinas)
  
  return(final)
  
}

resumen_producto<-function(mes,shopify){
  
  
  my_data=shopify[month==mes]
  productos=my_data[,.(cantidad=sum(net_quantity)),by=product_title][order(-cantidad)]
  
  total=sum(productos$cantidad)
  
  productos[,porcentaje:=cantidad/total]
  
  ## Porcentaje mes anterior
  
  mes_date=as.Date(paste(mes,"-01",sep = ""),"%Y-%m-%d")
  
  mes_date_prev=mes_date %m-% months(1)
  mes_prev=substr(as.character(mes_date_prev),1,7)
  
  my_data_prev=shopify[month==mes_prev]
  productos_prev=my_data_prev[,.(cantidad_mes_anterior=sum(net_quantity)),by=product_title][order(-cantidad_mes_anterior)]
  
  total=sum(productos_prev$cantidad)
  
  productos_prev[,porcentaje_mes_anterior:=cantidad_mes_anterior/total]
  
  ##Cantidades esperadas ultimos seis meses
  
  six=mes_date %m-% months(6)
  
  ultimos_meses=shopify[day>=six]
  
  ultimos_meses=ultimos_meses[,.(cantidad_6_meses=sum(net_quantity)),by=c("product_title","month")][order(-cantidad_6_meses)]
  
  ## Productos con al menos seis meses de historia
  history=ultimos_meses[,.N,by=product_title]
  history=history[N>=6]
  
  ultimos_meses=ultimos_meses[product_title %in% history$product_title]
  
  ultimos_meses=ultimos_meses[,.(promedio_ultimos_6=mean(cantidad_6_meses)),by=c("product_title")][order(-promedio_ultimos_6)]
  
  ##A la fecha de ultima compra, en periodo anterior
  
  ultimo_dia=max(shopify[month==mes]$day)
  ultimo_dia=day(ultimo_dia)
  
  my_data_prev=my_data_prev[day(day)<=ultimo_dia]
  
  productos_prev_parcial=my_data_prev[,.(cantidad_mes_anterior_parcial=sum(net_quantity)),by=product_title][order(-cantidad_mes_anterior_parcial)]
  
  total=sum(productos_prev_parcial$cantidad_mes_anterior_parcial)
  
  productos_prev_parcial[,porcentaje_mes_anterior_parcial:=cantidad_mes_anterior_parcial/total]
  
  
  
  #Union
  
  productos_view=merge(productos, productos_prev_parcial,by.x="product_title",by.y = "product_title", all.x=TRUE)
  
  productos_view=merge(productos_view, productos_prev,by.x="product_title",by.y = "product_title", all.x=TRUE)
  productos_view=merge(productos_view, ultimos_meses,by.x="product_title",by.y = "product_title", all.x=TRUE)
  
  productos_view=productos_view[][order(-cantidad)]
  
  
  return(productos_view)
}