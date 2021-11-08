#Regla 1

regla1=compras_productos[grepl( "cápsulas colcafé cappuccino clásico", producto, fixed = TRUE)==TRUE]
regla1=regla1[grepl( "cápsulas colcafé cappuccino light", producto, fixed = TRUE)==TRUE]
regla1=regla1[grepl( "cápsulas colcafé cappuccino vainilla", producto, fixed = TRUE)==TRUE]

a=club_all[nombre_orden %in% regla1$nombre_orden,.N,by=c("nombre_orden","medalla")]
a[,.N,by=medalla][order(-N)]

a[,.N,by="year_inscrito"]


#Regla 2

regla2=compras_productos[grepl( "cápsulas chai latte", producto, fixed = TRUE)==TRUE]
regla2=regla2[grepl( "cápsulas matiz ébano espresso", producto, fixed = TRUE)==TRUE]
regla2=regla2[grepl( "cápsulas colcafé cappuccino clásico", producto, fixed = TRUE)==TRUE]

a=club_all[nombre_orden %in% regla2$nombre_orden,.N,by=c("nombre_orden","medalla")]
a[,.N,by=medalla][order(-N)]


#Regla 3

regla3=compras_productos[grepl( "cápsulas colcafé cappuccino clásico", producto, fixed = TRUE)==TRUE]
regla3=regla3[grepl( "cápsulas colcafé cappuccino vainilla", producto, fixed = TRUE)==TRUE]
regla3=regla3[grepl( "cápsulas matiz escarlata intenso", producto, fixed = TRUE)==TRUE]

a=club_all[nombre_orden %in% regla3$nombre_orden,.N,by=c("nombre_orden","medalla")]
a[,.N,by=Sexo][order(-N)]

#Regla 4

regla4=compras_productos[grepl( "cápsulas chocolyne", producto, fixed = TRUE)==TRUE]
regla4=regla4[grepl( "cápsulas colcafé cappuccino light", producto, fixed = TRUE)==TRUE]
regla4=regla4[grepl( "cápsulas chai latte", producto, fixed = TRUE)==TRUE]

a=club_all[nombre_orden %in% regla4$nombre_orden,.N,by=c("nombre_orden","medalla")]
a[,.N,by=medalla][order(-N)]

#Regla 5

regla5=compras_productos[grepl( "cápsulas chocolyne", producto, fixed = TRUE)==TRUE]
regla5=regla5[grepl( "cápsulas colcafé cappuccino light", producto, fixed = TRUE)==TRUE]
regla5=regla5[grepl( "kit tazas grandes", producto, fixed = TRUE)==TRUE]

a=club_all[nombre_orden %in% regla5$nombre_orden,.N,by=c("nombre_orden","medalla")]
a[,.N,by=medalla][order(-N)]


clientes_club

rfm=unique(rfm[,.N,by=c("correo","medalla")])[,c("correo","medalla")]
clientes_club=merge(clientes_club, rfm, all.x=TRUE)

sexo_vs_frm=clientes_club[!is.na(medalla),c("year_inscrito","medalla")]
sexo_vs_frm=sexo_vs_frm[,.N,by=c("year_inscrito","medalla")]

dcast(sexo_vs_frm,medalla~year_inscrito,value.name="N")

