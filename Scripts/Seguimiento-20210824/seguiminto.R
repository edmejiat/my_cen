library(googlesheets4)

range_clear("1NuicfNhUe4ffvHefo26xHC5T7oXrt5tFNiMxlN2OZyI", "Resumen")
range_clear("1NuicfNhUe4ffvHefo26xHC5T7oXrt5tFNiMxlN2OZyI", "dia_dia")
range_clear("1NuicfNhUe4ffvHefo26xHC5T7oXrt5tFNiMxlN2OZyI", "dia_dia2")
range_clear("1NuicfNhUe4ffvHefo26xHC5T7oXrt5tFNiMxlN2OZyI", "productos")

resumen=seguimiento_mes(shopify)

resumen_mes("2021-08")

write_sheet(resumen,"1NuicfNhUe4ffvHefo26xHC5T7oXrt5tFNiMxlN2OZyI",sheet = "Resumen")

agosto=shopify[month=="2021-08"]

mauinas=agosto[product_type=="maquina"][order(day)]

cantidad_pedidos=agosto[,.N,by=c("day","order_name")]
cantidad_pedidos=cantidad_pedidos[,.N,by=c("day")]

ventas_dia=agosto[,.(ventas=sum(total_sales)),by=c("day")]

dia_dia=merge(cantidad_pedidos, ventas_dia, all.x=TRUE)

write_sheet(dia_dia,"1NuicfNhUe4ffvHefo26xHC5T7oXrt5tFNiMxlN2OZyI",sheet = "dia_dia2")

productos=resumen_producto("2021-08",shopify)

write_sheet(productos,"1NuicfNhUe4ffvHefo26xHC5T7oXrt5tFNiMxlN2OZyI",sheet = "productos")


## Analisis primera compra

primera_compra <- fread("Datos/Seguimiento-20210824/primera_compra.csv")

registros_agostos=club_cen[fecha_registro>="2021-08-01"]


registros_agostos[correo %in% agosto$customer_email]


agosto[order_name %in% primera_compra$order_name,.N,by=c("customer_email","order_name")][order(customer_email)]

ordenes=agosto[,.(promedio=sum(total_sales)),by=order_name]

mean(ordenes$promedio)

no_agosto=shopify[month!="2021-08"]


no_agosto[customer_email %in% registros_agostos$correo,.N,by=customer_email]

sum(agosto[order_name %in% primera_compra$order_name,.(descuentos=sum(discounts)),by=order_name]$descuentos)

rfm=modelo_rfm(shopify = shopify)

my_rfm=rfm[medalla!=medalla_ajustado]
my_rfm=my_rfm[primera_compra<"2021-08-01"]

my_rfm[,.N,by=c("medalla","medalla_ajustado")]

mean(rfm$promedio_dia,na.rm = TRUE)

