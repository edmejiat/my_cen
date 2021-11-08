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
  
  club_cen<<-club_cen(shopify,clientes_con_cuenta,clientes_club)
  
  rm(clientes_club,clientes_con_cuenta,con)
  
  fwrite(club_cen,osPathJoin(seguimiento,"club_cen.csv"),sep = "|")
}
  
 