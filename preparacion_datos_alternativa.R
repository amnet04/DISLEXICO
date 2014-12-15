#######################################################################################
#                                                                                     #
#         Carga, revisón y limpieza de datos                                          #
#                                                                                     #
#######################################################################################

# Cargar las rutas de los archivos en una lista
archivos <- list.files("data/respuestas", pattern="*.csv", full.names=TRUE)

# Cargar los nombres de los archivos en una lista para utilizarlos como referencia
archiv_name <- list.files("data/respuestas", pattern="*.csv", full.names=FALSE)

# Unificar los nombres de los archivos
tmpName = archiv_name 
tmpName <- as.vector(sapply(tmpName,  function(x) sub(".csv","",x)))
tmpName <- as.vector(sapply(tmpName,  function(x) sub("Tomo1","TomoI",x)))
tmpName <- as.vector(sapply(tmpName,  function(x) sub("tomo","Tomo",x)))
tmpName <- as.vector(sapply(tmpName,  function(x) sub("mapa","Mapa",x)))
tmpName <- as.vector(sapply(tmpName,  function(x) sub("lamina","Lamina",x)))
archiv_name = tmpName
remove(tmpName)


# Cargar las hojas de variantes y sus datos en una lista
hojas_variantes = sapply(archivos, function(x) read.csv(x, header = TRUE, sep = ";", row.names=1)) 
names(hojas_variantes)=archiv_name
# Corregir posibles espacios en blanco en las hojas de respuestas
for(i in 1:NROW(archivos)){
  hojas_variantes[[archiv_name[i]]][is.na(hojas_variantes[[archiv_name[i]]])] <- 0
}


#######################################################################################
#                                                                                     #
#                     Tratamiento de los datos                                        #
#                                                                                     #
#######################################################################################


# Cargar los Ids de los pueblos en un vector para usarlos como referencia 
Id = row.names(hojas_variantes[[archiv_name[1]]])


########################################
# Tratamiento de las respuestas vacías #
########################################

# Contar las respuestas vacías por municipio
tabla_resp_vacias = 1*sapply(
    hojas_variantes, 
    function(x)  apply(x, 1,function(y) all(y==0))
  )
resp_vacias_loc = apply(tabla_resp_vacias, 1, function(x) sum(x) )

consolidado_vacios = sapply(resp_vacias_loc, 
                            function(x) sapply(resp_vacias_loc,
                                               function(y) x+y)
)

# Se corrige la suma de las diagonales
for(i in 1:NROW(consolidado_vacios)){
  consolidado_vacios[i,i]=resp_vacias_loc[i]
}

# Se calcula el número de respuestas vacías en común por cada pueblo
vacios_comunes <- matrix(0,nrow=NROW(tabla_resp_vacias), ncol=NROW(tabla_resp_vacias))
rownames(vacios_comunes)=Id
colnames(vacios_comunes)=Id
for(i in 1:NCOL(tabla_resp_vacias)){
  for(j in 1:NROW(tabla_resp_vacias)){
    if(tabla_resp_vacias[j,i]==1){
      for(k in 1:NROW(tabla_resp_vacias)){
        if(tabla_resp_vacias[k,i]==1){
          vacios_comunes[j,k]=vacios_comunes[i,j]+1
          print (j)
        }
      }    
    }
  }
}

########################################
#         Calculo de diferencias       #
########################################

#  Función para calcular diferencias 
diferencia <- function (a,b)
{
  if(any(a+b==2)) return(0) # Si las dos localidades tienen 1 para la variable
  else  return(1)           # la función devuelve 0, o sea no hay diferencia.                         # En cualquier otro caso devuelve 1 (diferencia)                          
}

# Función para producir tabla de diferencias
tablaDiferencias <- function (a){
  tabla_diferencias = apply(
    a, 1, 
    function(x) apply(a,1, function(y) {
        diferencia(x,y)
      }
      )
    )
}

# Aplicación de la función de matrices de diferencias a todos los datos
tablas_diferencias=lapply(hojas_variantes, function(x) {
    print (names(x))
    tablaDiferencias(x)
   }  
  )

# Preparación de la variable para guardar el consolidado
consolidado_diferencias =  as.data.frame(tablas_diferencias[1])

# Suma de las 100 matrices de diferencias
for(i in 2:NROW(tablas_diferencias)){
  message(i,"\r",appendLF=FALSE)
  consolidado_diferencias = consolidado_diferencias+as.data.frame(tablas_diferencias[i])
  colnames(consolidado_diferencias) <- Id
}

# Correción de la diagonal, por que el escript considera diferencias si no hay datos
for(i in 1:NROW(consolidado_diferencias)){
  consolidado_diferencias[i,i]=0
}

########################################
#         Calculo de similaridades     #
########################################

# Tabla de similaridades: Como existen respuestas en blanco, 
# la tabla de similaridades se   calcula restando de 100 las 
# diferencias y las respuestas en blanco. Para ello se crea 
# una nueva matriz  cuadrada que calcule la suma de respuestas 
# vacías de cada pareja de lugares


consolidado_similaridades = 100-consolidado_diferencias - resp_vacias_loc
# Se corrige la diagonal para que cada pueblo tenga 100 similaridades con sigo mismo
for(i in 1:NROW(consolidado_similaridades)){
  consolidado_similaridades[i,i]=100
}



################################################
#  Calculo de del indice de identidad relativa #
################################################


# Para calcular el IRI restulta más conveniente tener todas las variantes en la misma
# tabla,  entonces las uno en un solo dataframe
matriz_variantes=as.data.frame(hojas_variantes)
# Eliminar posibles datos totalmente en blanco
columnas_a_borrar<-matrix()
cont=0
for (i in 1:NCOL(matriz_variantes)){
  if (all(matriz_variantes[,i]==0)){
    cont=cont+1
    columnas_a_borrar[cont]=i
  }
}
for (i in 1:NROW(columnas_a_borrar)){
    print(columnas_a_borrar[i])
    matriz_variantes[columnas_a_borrar[i]]<-NULL
}

# Aprovecho para calcular la frecuencia realativa de aparición de cada variante
tbl_feq_var=apply(matriz_variantes, 2, function(x) sum(x))

# Sumarizo las frecuencias para darme una idea de como calcular el peso relativo
sumario_frec_rel=summary(tbl_feq_var)

# Histograma para entender mejor los datos
par(mar=c(2,2.5,2,2))
hist(tbl_feq_var, breaks=485 )
lines(density(tbl_feq_var), col="blue", lwd=3)


#Indice de identidad relativa

#calculo de coodentidades
coi <- function (a,b){
 return(sum(a & b))
}


tabla_coi=matrix(0, nrow=NROW(matriz_variantes), ncol=NROW(matriz_variantes), dimnames= list(Id,Id))
# Matriz de coidentidades
tabla_coi <- apply(matriz_variantes,1, 
             function(x) apply(matriz_variantes, 1,
                function(y) coi(x,y)
                ) 
             )

#calculo de coodiferencias
cod <- function (a,b){
  return(sum(xor(a,b)))
}

tabla_cod=matrix(0, nrow=NROW(matriz_variantes), ncol=NROW(matriz_variantes), dimnames= list(Id,Id))
tabla_cod <- apply(matriz_variantes,1, 
                   function(x) apply(matriz_variantes, 1,
                                     function(y) cod(x,y)
                   ) 
)

# Matriz de identidades relativas

IIR = 100*tabla_coi/(tabla_coi+tabla_cod)

# Clustering
cluster <- function(a){
distancia = as.dist(a)
cluster <- hclust(distancia)
}


# Mapas
colorear_dialectos <- function(nivel){
  llenado=as.data.frame(list(grupo=cutree(cluster,k=nivel)))
  row.names(llenado)=Id
  colnames(llenado)=c("grupo")
  llenado[,"grupo"]=as.vector(cutree(cluster,k=nivel))
  llenado$color=sapply(llenado[,"grupo"], 
                       function (x) RColorBrewer::brewer.pal(12, 'Paired')[round(seq(from = 1, to = 12, by = 11/(nivel-1)))[x]])
  return(llenado$color)
}

#Preparar teselado
teselado <- RJSONIO::fromJSON("data/geo/ALECSTY.json")

#Incluir número de respuestas vacias en los datos del mapa
for(i in 1:NROW(teselado["features"])){
  for(j in 1:NROW(teselado["features"][[i]])){
    teselado["features"][[i]][[j]]$properties$vacios=resp_vacias_loc[j][[1]]
  }
}


llenado_por_dialecto=colorear_dialectos(12)
for(i in 1:NROW(teselado["features"])){
  for(j in 1:NROW(teselado["features"][[i]])){
    teselado["features"][[i]][[j]]$properties$style$fillColor=llenado_por_dialecto[j]
  }
}

save(consolidado_diferencias,tabla_resp_vacias, Id, archiv_name, cluster, distancia, hojas_variantes, resp_vacias_loc, tablas_diferencias,teselado, colorear_dialectos, file="ALEC.RData")


