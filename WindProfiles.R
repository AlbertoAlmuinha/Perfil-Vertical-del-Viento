#-----------------------------------------------------------Wind Profiles--------------------------------------------------------------------------------------------

#Parámetros: 

#data<-data frame o matrix; 
#vel<- Posición de las columnas de velocidad en el df: c(1,2,4..) (AÑADIR COMO VECTOR!)
#filtro_min_vel<-Velocidad mínima para filtrar el nivel inferior
#dir<-Posición de la columna de la direccion seleccionada para binar c(1,1,1..) (AÑADIR COMO VECTOR!) (Debe haber mismas columnas de velocidad que de dirección!)
#alturas<-Se usa si en el nombre de las velocidades no aparece "XXm" ó "XXXm" (X es un número). Si esto no se cumple, utilizar alturas para añadir vector con las alturas de las velocidades seleccionadas: EJEMPLO: c(40,55,70,85)
#filtro<-"Nivel_Inferior" filtra los datos con NAs y velocidad inferior. "Acumulado" elimina los NAs correspondientes a todos los niveles y filtra por la velocidad del nivel inferior.
#alturas.names<-Letras que acompañan a los números en el nombre de la altura, por ejemplo: Para Ane80m, sería "m". Para Ane80Av, sería "Av".

#Comentarios:

#El df debe estar filtrado, por ejemplo, haber pasado ya 99.999 o 9999 a NA.

#Las velocidades deben añadirse en orden creciente de altura!! MUY IMPORTANTE!!

#Almacenar resultado en una variable. Será una lista cuyo primer elemento serán los perfiles, el segundo los perfiles por sector y el tercero los perfiles medios!

#Librerías: "stringr"-"openair"-"data.table"

#WindRoses<-Para filtro de nivel inferior salen las rosas de viento para cada altura sin filtrar. Para filtro acumulada salen para cada altura con el filtro aplicado.

WindProfiles<-function(data, vel, dir, filtro_min_vel=4,alturas=NULL, alturas.names="m", filtro="Nivel_Inferior"){
        
        ncols<-vel; require(stringr); require(openair); require(data.table)
        
        if(is.data.frame(data)==FALSE & is.matrix(data)==FALSE){
                stop("data must be a data frame or a matrix")
        }
        if(sum(apply(data[,ncols],2,is.numeric))!=length(ncols) | sum(apply(data[,dir],2,is.numeric))!=length(dir)){
                stop("Selected columns must be numeric")
        }
        
        if(is.vector(ncols)==FALSE | is.vector(dir)==FALSE){
                stop("'ncols' and 'dir' must be vectors")
        }
        
        if(length(ncols)!=length(dir)){
                stop("'ncols' and 'dir' must have the same length")
        }
        
        if(length(which(data[,ncols]>90))!=0){
                stop("'ncols' must be filtered. Values greater than 90 were finded")
        }
        
        if(length(which(data[,dir]>360))!=0){
                stop("'dir' must be filtered. Values greater than 360 were finded")
        }
        
        for(i in 1:length(ncols)){
          if(sum(is.na(data[,ncols[i]])==TRUE)==length(data[, ncols[i]])){
            stop("A column in 'vel' has only NA's values ")
          }
        }
        
        if(is.vector(alturas)==TRUE){
                alturas<-alturas
        } 
        else{
                names<-str_extract(colnames(data[,ncols]), pattern =paste("[0-9]{2}",alturas.names,"|[0-9]{3}",alturas.names, sep = "")) #Localizo los nombres de las alturas en las columnas dadas
                
                names<-str_replace(names, pattern = paste(alturas.names,"$", sep = ""), replacement = "")
                
                alturas<-as.numeric(names)
        }
        
        
        if(filtro=="Nivel_Inferior"){
                
                filtro_vel<-list(); for(i in 1:length(ncols)){filtro_vel[[i]]<-which(data[,ncols[i]]>filtro_min_vel & is.na(data[,ncols[i]])==FALSE)} #Filtramos las columnas por velocidad
                
                Perfiles1<-list() #Obtenemos los perfiles, cada uno en un elemento de una lista!
                
                iterator<-1
                
                while(iterator<=length(ncols)^2){
                        for(i in 1:length(ncols)){
                                for(j in 1:length(ncols)){
                                        if(i!=j){
                                                Perfiles1[[iterator]]<-log(data[filtro_vel[[i]],ncols[j]]/data[filtro_vel[[i]],ncols[i]])/log(alturas[j]/alturas[i])
                                                iterator<-iterator+1}
                                        else {
                                                Perfiles1[[iterator]]<-NA
                                                iterator<-iterator+1
                                        }
                                }
                        }
                }
                
                longitudes<-vector(); for(i in 1:length(Perfiles1)){longitudes[i]<-max(length(Perfiles1[[i]]))} 
                
                max_long<-max(longitudes) #Calculamos longitud máxima para rellenar NAs, ya que al filtrar distinta longitud entre los vectores
                
                Perfiles<-as.data.frame(matrix(ncol = length(ncols)^2, nrow = max_long)) #Unimos los perfiles en un data frame
                
                for(i in 1:length(Perfiles1)){Perfiles[,i]<-c(Perfiles1[[i]], rep(NA, (max_long-length(Perfiles1[[i]]))))}
                
                names_perfiles<-matrix(nrow = length(ncols), ncol = length(ncols)) #creamos matriz con los nombres de los perfiles para añadir al df
                
                for(i in 1:length(ncols)){
                        for(j in 1:length(ncols)){
                                names_perfiles[j,i]<-paste("Perfil_",alturas[i],"_",alturas[j], sep = "")
                        }
                }
                
                names_union<-list() #pasamos de la matriz de nombres a una lista
                
                for(i in 1:(dim(names_perfiles)[2])){names_union[[i]]<-as.character(names_perfiles[,i])}
                
                names_perfiles<-c(names_union[[1]], names_union[[2]]) #Unimos los nombres de la lista en un vector
                
                if(length(alturas)>=3){
                        for(i in 3:length(names_union)){names_perfiles<-c(names_perfiles, names_union[[i]])}
                }
                
                colnames(Perfiles)<-names_perfiles
                
                prueba<-seq(1,length(ncols)^2,1) #Aquí comienza el filtrado para obtener perfiles únicos (no repetidos)
                
                prueba1<-list()
                
                iterator<-c(seq(1,length(ncols)^2, length(ncols)),length(ncols)^2+1)
                
                for(i in 1:length(ncols)){
                        prueba1[[i]]<-prueba[iterator[i]:(iterator[i+1]-1)]
                }
                
                prueba2<-seq(1,length(ncols)^2,1)
                
                for(i in 1:length(ncols)){prueba1[[i]]<-prueba1[[i]][-prueba2[1:i]]}
                
                del_final<-c(prueba1[[1]], prueba1[[2]]); if(length(alturas)>=3){for(i in 3:length(ncols)){del_final<-c(del_final, prueba1[[i]])}} #Obtenemos perfiles que nos tenemos que quedar!
                
                Perfil_Medio<-apply(Perfiles,2,mean, na.rm=TRUE)
                
                Perfiles_Medios<-as.data.frame(matrix(ncol = length(Perfil_Medio), nrow = 1))
                
                colnames(Perfiles_Medios)<-colnames(Perfiles); Perfiles_Medios[1,]<-Perfil_Medio
                
                Direccion1<-list()
                
                for(i in 1:length(dir)){Direccion1[[i]]<-data[filtro_vel[[i]],dir[i]]}
                for(i in 1:length(dir)){Direccion1[[i]]<-c(Direccion1[[i]],rep(NA,(max_long-length(Direccion1[[i]]))))}
                
                Direccion<-as.data.frame(matrix(nrow = max_long, ncol = length(dir)))
                
                for(i in 1:length(dir)){
                        Direccion[,i]<-as.integer(((Direccion1[[i]]+11.25)/22.5)+1)
                        Direccion[[i]][Direccion[[i]]==17]=1
                }
                
                Sector<-as.data.frame(matrix(nrow = max_long, ncol = length(ncols))); 
                
                for(i in 1:length(ncols)){
                        Sector[,iterator[i]:(iterator[i+1]-1)]<-Direccion[,i]
                }
                
                Perfiles_Sector<-list()
                
                for(i in 1:length(ncols)^2){Perfiles_Sector[[i]]<-tapply(Perfiles[,i], Sector[,i], mean, na.rm=TRUE)}
                
                names(Perfiles_Sector)<-colnames(Perfiles)
                
                
                Perfiles<-Perfiles[,del_final] #Nos quedamos con los perfiles filtrados!
                Perfiles_Sector<-Perfiles_Sector[del_final]
                Perfiles_Medios<-Perfiles_Medios[,del_final]
                
                WindRose_Plots1<-list()

                for(i in 1:length(ncols)){WindRose_Plots1[[i]]<-windRose(data, ws=colnames(data)[ncols[i]], wd=colnames(data)[dir[i]], angle = 22.5, bias.corr = TRUE, paddle = FALSE, cols = "jet", key.header = paste("WindRose_",alturas[i],"m",sep = ""))}

                WindRose_Plots<-list()

                for(i in 1:length(WindRose_Plots1)){WindRose_Plots[[i]]<-WindRose_Plots1[[i]][[1]]}

                perfiles<-list(Perfiles=Perfiles, Perfiles_Sector=Perfiles_Sector, Perfiles_Medios=Perfiles_Medios, WindRose_Plots=WindRose_Plots)
                return(perfiles)
                
                rm(longitudes,Direccion,Perfil_Medio,Sector,alturas,del_final,Direccion1,filtro_vel,iterator,max_long,names,names_perfiles,names_union,Perfiles1,prueba,prueba1,prueba2)
                
        }
        
        else if (filtro=="Acumulado"){
                
                #Filtramos todos los NAs de los niveles para la velocidad: Filtro Acumulado
                
                df<-as.data.frame(matrix(ncol = length(ncols), nrow = dim(data)[1])) #Creamos df con las velocidades seleccionadas
                
                for(i in 1:length(ncols)){df[,i]<-data[,ncols[i]]}
                
                for(i in 1:length(df)){
                        
                        for(j in 1:dim(df)[1]){
                                
                                if(is.na(df[j,i])==TRUE){
                                        
                                        df[j,]<-NA
                                        
                                }
                        }
                }
                
                df<-as.data.frame(df)
                
                for(i in 1:length(ncols)){data[,ncols[i]]<-df[,i]} #Cambiamos las velocidades corregidas en el df original
                
                
                #Filtramos todos los NAs de los niveles para la dirección: Filtro Acumulado
                
                
                df1<-as.data.frame(matrix(ncol = length(ncols), nrow = dim(data)[1])) #Creamos df con las direcciones seleccionadas
                
                for(i in 1:length(dir)){df1[,i]<-data[,dir[i]]}
                
                for(i in 1:length(df1)){
                        
                        for(j in 1:dim(df1)[1]){
                                
                                if(is.na(df1[j,i])==TRUE){
                                        
                                        df1[j,]<-NA
                                        
                                }
                        }
                }
                
                df1<-as.data.frame(df1)
                
                for(i in 1:length(dir)){data[,dir[i]]<-df1[,i]} #Cambiamos las direcciones corregidas en el df original
                
                filtro_vel<-list(); for(i in 1:length(ncols)){filtro_vel[[i]]<-which(data[,ncols[i]]>filtro_min_vel & is.na(data[,ncols[i]])==FALSE)} #Filtramos las columnas por velocidad
                
                Perfiles1<-list() #Obtenemos los perfiles, cada uno en un elemento de una lista!
                
                iterator<-1
                
                while(iterator<=length(ncols)^2){
                        for(i in 1:length(ncols)){
                                for(j in 1:length(ncols)){
                                        if(i!=j){
                                                Perfiles1[[iterator]]<-log(data[filtro_vel[[i]],ncols[j]]/data[filtro_vel[[3]],ncols[i]])/log(alturas[j]/alturas[i])
                                                iterator<-iterator+1}
                                        else {
                                                Perfiles1[[iterator]]<-NA
                                                iterator<-iterator+1
                                        }
                                }
                        }
                }
                
                longitudes<-vector(); for(i in 1:length(Perfiles1)){longitudes[i]<-max(length(Perfiles1[[i]]))} 
                
                max_long<-max(longitudes) #Calculamos longitud máxima para rellenar NAs, ya que al filtrar distinta longitud entre los vectores
                
                Perfiles<-as.data.frame(matrix(ncol = length(ncols)^2, nrow = max_long)) #Unimos los perfiles en un data frame
                
                for(i in 1:length(Perfiles1)){Perfiles[,i]<-c(Perfiles1[[i]], rep(NA, (max_long-length(Perfiles1[[i]]))))}
                
                names_perfiles<-matrix(nrow = length(ncols), ncol = length(ncols)) #creamos matriz con los nombres de los perfiles para añadir al df
                
                for(i in 1:length(ncols)){
                        for(j in 1:length(ncols)){
                                names_perfiles[j,i]<-paste("Perfil_",alturas[i],"_",alturas[j], sep = "")
                        }
                }
                
                names_union<-list() #pasamos de la matriz de nombres a una lista
                
                for(i in 1:(dim(names_perfiles)[2])){names_union[[i]]<-as.character(names_perfiles[,i])}
                
                names_perfiles<-c(names_union[[1]], names_union[[2]]) #Unimos los nombres de la lista en un vector
                
                if(length(alturas)>=3){
                        for(i in 3:length(names_union)){names_perfiles<-c(names_perfiles, names_union[[i]])}
                }
                
                colnames(Perfiles)<-names_perfiles
                
                prueba<-seq(1,length(ncols)^2,1) #Aquí comienza el filtrado para obtener perfiles únicos (no repetidos)
                
                prueba1<-list()
                
                iterator<-c(seq(1,length(ncols)^2, length(ncols)),length(ncols)^2+1)
                
                for(i in 1:length(ncols)){
                        prueba1[[i]]<-prueba[iterator[i]:(iterator[i+1]-1)]
                }
                
                prueba2<-seq(1,length(ncols)^2,1)
                
                for(i in 1:length(ncols)){prueba1[[i]]<-prueba1[[i]][-prueba2[1:i]]}
                
                del_final<-c(prueba1[[1]], prueba1[[2]]); if(length(alturas)>=3){for(i in 3:length(ncols)){del_final<-c(del_final, prueba1[[i]])}} #Obtenemos perfiles que nos tenemos que quedar!
                
                Perfil_Medio<-apply(Perfiles,2,mean, na.rm=TRUE)
                
                Perfiles_Medios<-as.data.frame(matrix(ncol = length(Perfil_Medio), nrow = 1))
                
                colnames(Perfiles_Medios)<-colnames(Perfiles); Perfiles_Medios[1,]<-Perfil_Medio
                
                Direccion1<-list()
                
                for(i in 1:length(dir)){Direccion1[[i]]<-data[filtro_vel[[i]],dir[i]]}
                for(i in 1:length(dir)){Direccion1[[i]]<-c(Direccion1[[i]],rep(NA,(max_long-length(Direccion1[[i]]))))}
                
                Direccion<-as.data.frame(matrix(nrow = max_long, ncol = length(dir)))
                
                for(i in 1:length(dir)){
                        Direccion[,i]<-as.integer(((Direccion1[[i]]+11.25)/22.5)+1)
                        Direccion[[i]][Direccion[[i]]==17]=1
                }
                
                Sector<-as.data.frame(matrix(nrow = max_long, ncol = length(ncols))); 
                
                for(i in 1:length(ncols)){
                        Sector[,iterator[i]:(iterator[i+1]-1)]<-Direccion[,i]
                }
                
                Perfiles_Sector<-list()
                
                for(i in 1:length(ncols)^2){Perfiles_Sector[[i]]<-tapply(Perfiles[,i], Sector[,i], mean, na.rm=TRUE)}
                
                names(Perfiles_Sector)<-colnames(Perfiles)
                
                Perfiles<-Perfiles[,del_final] #Nos quedamos con los perfiles filtrados!
                Perfiles_Sector<-Perfiles_Sector[del_final]
                Perfiles_Medios<-Perfiles_Medios[,del_final]
                
                
                
                WindRose_Plots1<-list()
                
                for(i in 1:length(ncols)){WindRose_Plots1[[i]]<-windRose(data, ws=colnames(data)[ncols[i]], wd=colnames(data)[dir[i]], angle = 22.5, bias.corr = TRUE, paddle = FALSE, cols = "jet", key.header = paste("WindRose_",alturas[i],"m",sep = ""))}
                
                WindRose_Plots<-list()
                
                for(i in 1:length(WindRose_Plots1)){WindRose_Plots[[i]]<-WindRose_Plots1[[i]][[1]]}
                
                perfiles<-list(Perfiles=Perfiles, Perfiles_Sector=Perfiles_Sector, Perfiles_Medios=Perfiles_Medios, WindRose_Plots<-WindRose_Plots)
                return(perfiles)
                
                rm(longitudes,Direccion,Perfil_Medio,Sector,alturas,del_final,Direccion1,filtro_vel,iterator,max_long,names,names_perfiles,names_union,Perfiles1,prueba,prueba1,prueba2, WindRose_Plots1)
                
        }
        
        else{
                stop(" Argument 'filtro' is not valid. Use 'Nivel_Inferior' or 'Acumulado'")
        }
}
