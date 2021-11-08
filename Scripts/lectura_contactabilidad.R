## Contactabilidad ##

contactabilidad<-function(ruta){
  #Lectura de datos de la hoja general
  carpetas=list.dirs(path = ruta, full.names = TRUE, recursive = FALSE)
  carpetas_resumen=list.dirs(path = ruta, full.names = FALSE, recursive = FALSE)
  
  all_generales=data.table()
  
  for (i in 1:length(carpetas)) {
    
    nombre_archivo=list.files(carpetas[i])[1]
    nombre_archivo=osPathJoin(carpetas[i],nombre_archivo)
    
    general <- read_excel(enc2native(nombre_archivo), sheet = "General",col_types = "text")%>%as.data.table()
    general$mes=carpetas_resumen[i]
    
    all_generales=rbind(all_generales,general)
    
    all_generales$Id=as.numeric(all_generales$Id)
    
  }
  
  ## Lectura de las Hojas correspondientes
  
  all_campains=data.table()
  
  for (i in 1:dim(all_generales)[1]) {
    linea=all_generales[i]
    nombre_mes=linea$mes
    nombre_archivo=osPathJoin(ruta,linea$mes)
    nombre_archivo=osPathJoin(nombre_archivo,list.files(nombre_archivo)[1])
    nombre_hoja=as.character(linea$Id)
    
    campain=read_excel(enc2native(nombre_archivo), sheet =nombre_hoja,col_types = "text")%>%as.data.table()
    campain$mes=nombre_mes
    
    all_campains=rbind(all_campains,campain)
    
  }
  
  return(all_campains)

  
}


