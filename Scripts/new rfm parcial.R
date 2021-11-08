library(rfm)

my_shopify=shopify[,.(order_id,total_sales,customer_email,day)]

my_shopify=my_shopify[,.(ventas=sum(total_sales)),by=c("order_id","customer_email","day")]
my_shopify[,order_id:=NULL]

my_shopify$day=as.Date(my_shopify$day)

analysis_date <- today()-delta
rfm_result <- rfm_table_order(my_shopify, customer_email, day  , ventas, today())
rfm_result

rfm_result=setDT(rfm_result$rfm)
rfm_result$rfm_score=as.character(rfm_result$rfm_score)

segmentos <- fread("C:/Users/Edwin.Mejia/Documents/Danilo Mejia/Proyectos/CEN/Datos/Ultimate RFM.csv",encoding="UTF-8")%>%as.data.table()
segmentos[,RFM:=paste(R,F,M,sep = "")]

final_my_rfm=merge(rfm_result, segmentos,by.x="rfm_score",by.y="RFM", all.x=TRUE)

final_my_rfm=final_my_rfm[,.(customer_id,Segmento,Caracteristicas)]

