### Ranking contactabilidad

contactos_correo$mes<-as.Date(paste(contactos_correo$mes,"01",sep = "-"),"%Y-%m-%d")

ultimas_fechas=unique(contactos_correo[,.(mes)])
ultimas_fechas=ultimas_fechas[][order(-mes)]


calificaciones=c(1,0.9,0.8,0.7,0.6,0.5)
ultimas_fechas=cbind(ultimas_fechas,calificaciones)


contactos_correo$Open<-as.numeric(contactos_correo$Open)
contactos_correo$Click<-as.numeric(contactos_correo$Click)
contactos_correo$Spam<-as.numeric(contactos_correo$Spam)*-1
contactos_correo$Desuscritos<-as.numeric(contactos_correo$Desuscritos)*-1
contactos_correo$Rebotes<-as.numeric(contactos_correo$Rebotes)*-1


## Obtencion de calificacion

contactos_correo[,suma:=(Open+Click+Spam+Desuscritos+Rebotes)]

contactos_correo=contactos_correo[,.(suma=sum(suma)),by=c("Email","mes")]

## Union de calificaciones

contactos_correo=merge(contactos_correo, ultimas_fechas, all.x=TRUE)

contactos_correo[,calificacion_pre:=(suma*calificaciones)]
contactos_correo=contactos_correo[,.(calificacion_final=sum(calificacion_pre)),by=c("Email")]


media=mean(contactos_correo$calificacion_final)
desviacion=sd(contactos_correo$calificacion_final)

library(scales)
contactos_correo[,estandarizacion:=rescale(calificacion_final)]

contactos_correo[,calificacion_final:=NULL]
names(contactos_correo)<-c("correo","ranking_contactabilidad")


