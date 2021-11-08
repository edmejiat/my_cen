lista=c("Cápsulas Matiz Escarlata Intenso","Cápsulas Matiz Ébano Espresso","Cápsulas Matiz Ámbar Espresso Balanceado","Cápsulas Matiz Ámbar Balanceado")

top_tier<-function(ruta,lista=lista){
  
  ## Se filtran solo los productos que se necesitan
  solo_productos=shopify[product_title %in% lista]
  
  ## Correccion de Ambar, ya que salen dos referencias 
  solo_productos=solo_productos[product_title=="Cápsulas Matiz Ámbar Balanceado",product_title:="Cápsulas Matiz Ámbar Espresso Balanceado"]
  
  lista_f=unique(solo_productos$product_title)
  lista_n=unique(solo_productos$product_title)
  
  ## Correccion de los nombres
  lista_f=gsub("[][!#$%()*,.:;<=>@^_`|~.{}?¿]", "",lista_f)
  lista_f=gsub("[[:space:]]", "_", lista_f)
  lista_f=rm_accent(lista_f)
  lista_f=tolower(lista_f)
  
  totales=solo_productos[,.(cantidad=sum(net_quantity)),by=c("customer_email","product_title")]
  totales_cliente=totales[,.(todas=sum(cantidad)),by=customer_email]
  
  ### Creamos la hoja final
  
  OUT <- createWorkbook()
  
  final=dcast(totales,customer_email~product_title,value.var="cantidad")
  final[is.na(final)] <- 0
  marcas=names(final)[2:length(names(final))]
  
  final=merge(totales_cliente,final,by.x = "customer_email", by.y = "customer_email",all.x=TRUE)
  final=final[][order(-todas)]
  
  contactos=final[,.(customer_email)]
  contactos[,es_club:=ifelse(customer_email %in% club_cen$correo,"Si","No")]
  
  ##Si son del club:
  contactos_si_club=contactos[es_club=="Si"]
  contactos_si_club=merge( contactos_si_club,club_cen,by.y="correo",by.x="customer_email", all.x=TRUE)
  
  ## Si no, la informacion la completamos con lo que se tenga de shopify
  
  contactos_no_club=contactos[es_club=="No"]
  contactos_no_club$fecha_registro="No Registrado"
  contactos_no_club$apellido="--"
  contactos_no_club$telefono="--"
  contactos_no_club$direccion="--"
  contactos_no_club$detalle_direccion="--"
  contactos_no_club$segmento="No Inscrito"
  contactos_no_club$subsegmento="No inscrito Comprador"
  
  info_no_club=shopify[,.SD[1],by=.(customer_email,customer_name,shipping_city,shipping_region)]
  info_no_club=info_no_club[,.(customer_email,customer_name,shipping_city,shipping_region)]
  info_no_club=info_no_club[,.SD[1],by=customer_email]
  
  contactos_no_club=merge( contactos_no_club,info_no_club,by.y="customer_email",by.x="customer_email", all.x=TRUE)
  names(contactos_no_club)=c("customer_email","es_club","fecha_registro","apellido","telefono","direccion","detalle_direccion","segmento","subsegmento","nombre","ciudad","departamento")
  
  contactos=smartbind(contactos_si_club, contactos_no_club)
  
  final=merge(final,contactos,by.y="customer_email",by.x="customer_email", all.x=TRUE)%>%as.data.table()
  final=final[][order(-todas)]
  
  addWorksheet(OUT, "final")
  writeData(OUT, sheet = "final", x = final)
  
  for (i in 1:length(lista_f)) {
    a=totales[product_title==lista_n[i]]
    a=a[][order(-cantidad)]
    setDT(assign(lista_f[i],a))
    
    addWorksheet(OUT, substr(lista_f[i],10,str_length(lista_f[i])))
    writeData(OUT, sheet = substr(lista_f[i],10,str_length(lista_f[i])), x = a)
    
    }
  
  seguimiento=osPathJoin(resultadosPath,ruta)
  saveWorkbook(OUT, osPathJoin(seguimiento,paste("tier_",format(today(), "%Y%m%d"),".xlsx",sep = "")),overwrite=TRUE)
  


  
}


