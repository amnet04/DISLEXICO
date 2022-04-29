#################################################################################
#                                                                               #
#                  Carga, revisón y limpieza de datos                           #
#                                                                               #
#################################################################################

# Cargar variables de entorno
ProjectName <- Sys.getenv("ProjectName")
SiteVariantDataDir <- Sys.getenv("SiteVariantDataDir")
GeoTeselation <- Sys.getenv("GeoTeselation")

# UI form data
kgroups_choices = c("2" = 2,"3" = 3,"4" = 4, "5" = 5, "6" = 6, "7" = 7, "8" = 8,                                                     
                    "9" = 9, "10" = 10, "11" = 11, "12" = 12, "13" = 13 )

# Cargar los nombres de los archivos en una lista para utilizarlos como referencia
fileNames <- list.files(path = SiteVariantDataDir, pattern = "*.csv", full.names = TRUE)

# Cargar las hojas de variantes y sus datos en una lista
listMapData = sapply(fileNames, function(x) read.csv(x, header = TRUE, sep = ";", row.names=1))
names(listMapData)=fileNames

#Corregir posibles espacios en blanco en las hojas de respuestas
for(i in 1:NROW(fileNames)){
  listMapData[[fileNames[i]]][is.na(listMapData[[fileNames[i]]])] <- 0
}

# Cargar los Ids de los pueblos en un vector para usarlos como referencia 
Id = row.names(listMapData[[fileNames[1]]])

########################################
# Tratamiento de las respuestas vacías #
########################################

# Contar las respuestas vacías por municipio
tblVoidAns = 1*sapply(
    listMapData, 
    function(x)  apply(x, 1,function(y) all(y==0))
  )
voidAnsPlace = apply(tblVoidAns, 1, function(x) sum(x) )

# Se convierte voidAnsPlace en un dataframe para poder ordenarlo en las
# tablas de la presentación web
vacios = as.data.frame(voidAnsPlace)
vacios = cbind(Lugar=rownames(vacios), vacios)

comVoid = matrix(0, nrow=NROW(tblVoidAns), ncol=NROW(tblVoidAns),  dimnames = list(Id,Id))
for(i in 1:NCOL(tblVoidAns)){
  print(i)
  for(j in 1:NROW(tblVoidAns)){
    if (tblVoidAns[j,i]==1){
      for(k in 1:NROW(tblVoidAns)){
        if (tblVoidAns[k,i]==1){
          comVoid[j,k]=comVoid[j,k]+1
        }
      }
    }
  }
}
comVoid = as.data.frame(comVoid)


comVoidMaps <- matrix("", nrow=NCOL(tblVoidAns), ncol=1,  dimnames = list(colnames(tblVoidAns),"Localidades"))
countVoidMaps <- matrix(0, nrow=NCOL(tblVoidAns), ncol=1,  dimnames = list(colnames(tblVoidAns),"# Vacios"))
for(i in 1:NCOL(tblVoidAns)){
  print(i)
  for(j in 1:NROW(tblVoidAns)){
    if (tblVoidAns[j,i]==1){
          comVoidMaps[[i]]=paste(comVoidMaps[[i]], rownames(tblVoidAns)[j])
          comVoidMaps[[i]]=paste(comVoidMaps[[i]], ", ")
          countVoidMaps[[i]]=countVoidMaps[[i]]+1
    }
  }
}
comVoidMaps=as.data.frame(comVoidMaps)
comVoidMaps=cbind(countVoidMaps, comVoidMaps)

########################################
#         Calculo de diferencias       #
########################################

#  Función para calcular diferencias  (excluyendo vacios)
diferencia <- function (a,b)
{
  if(any(a+b==2) | sum(a+b)==0) {
    return(0) # Si las dos localidades tienen 1 para la variable
  }
  else {
    return(1)
  }           # la función devuelve 0, o sea no hay diferencia.                         # En cualquier otro caso devuelve 1 (diferencia)
}

diferencia_otr <- function (a,b)
{
  if(any(a+b==2)) {
    return(0) # Si las dos localidades tienen 1 para la variable
  }
  else {
    return(1)
  }           # la función devuelve 0, o sea no hay diferencia.                         # En cualquier otro caso devuelve 1 (diferencia)
}


# Función para producir tabla de diferencias
tablaDiferencias <- function (a){
  tblDiff = apply(
    a, 1,
    function(x) apply(a,1, function(y) {
        diferencia(x,y)
      }
      )
    )
}

# Aplicación de la función de matrices de diferencias a todos los datos
listDiffTbl=lapply(listMapData, function(x) {
    tablaDiferencias(x)
   }
  )

# Preparación de la variable para guardar el consolidado
diferencias =  as.data.frame(listDiffTbl[1])
# Suma de las 100 matrices de diferencias
for(i in 2:NROW(listDiffTbl)){
  message(i,"\r",appendLF=FALSE)
  diferencias = diferencias+as.data.frame(listDiffTbl[i])
  colnames(diferencias) <- Id
}

# Correción de la diagonal, por que el escript considera diferencias si no hay datos
for(i in 1:NROW(diferencias)){
  diferencias[i,i]=0
}

#Normalización
diferencias=diferencias

########################################
#         Calculo de similaridades     #
########################################

# Tabla de similaridades: Como existen respuestas en blanco, 
# la tabla de similaridades se   calcula restando de 100 las 
# diferencias y las respuestas en blanco. Para ello se crea 
# una nueva matriz  cuadrada que calcule la suma de respuestas 
# vacías de cada pareja de lugares

#similaridades normalizadas
similaridades = (1-diferencias-comVoid/100)
# Se corrige la diagonal para que cada pueblo tenga 100 similaridades con sigo mismo
for(i in 1:NROW(similaridades)){
  similaridades[i,i]=1
}



################################################
#  Calculo de del indice de identidad relativa #
################################################


# Para calcular el IRI restulta más conveniente tener todas las variantes en la misma
# tabla,  entonces las uno en un solo dataframe
matriz_variantes=as.data.frame(listMapData)
# Eliminar posibles datos totalmente en blanco
columnas_a_borrar<-matrix()
cont=0
for (i in 1:NCOL(matriz_variantes)){
  if (all(matriz_variantes[,i]==0)){
    print(i)
    cont=cont+1
    columnas_a_borrar[cont]=i
  }
}

if (!is.na(columnas_a_borrar)){
  for (i in 1:NROW(columnas_a_borrar)){
    print(columnas_a_borrar[i])
    matriz_variantes[columnas_a_borrar[i]]<-NULL
  }
}

# Aprovecho para calcular la frecuencia realativa de aparición de cada variante
tbl_feq_var=apply(matriz_variantes, 2, function(x) sum(x))

# Sumarizo las frecuencias para darme una idea de como calcular el peso relativo
sumario_frec_rel=summary(tbl_feq_var)

# Asigno pesos temporalmente
matriz_peso=matrix(1, nrow=NCOL(matriz_variantes), ncol=1, dimnames=list(colnames(matriz_variantes),"peso"))
for(i in 1:NCOL(matriz_variantes)){ 
# Variables presentes entre 1 y 10 lugares valen 1

# Variables presentes entre 11 y 22 lugares valen 0.8
  if (10<sum(matriz_variantes[i]) & sum(matriz_variantes[i])<23){
    matriz_peso[i]=0.8
  }
# Variables presentes entre 23 y 46 lugares valen 0.6
  else if (22<sum(matriz_variantes[i]) & sum(matriz_variantes[i])<47){
    matriz_peso[i]=0.6
  }
# variables presentes entre 47 y  94 lugares valen 0.5
  else if (46<sum(matriz_variantes[i]) & sum(matriz_variantes[i])<95){
    matriz_peso[i]=0.5
  }
# variables presentes entre 95 y 190 lugares valen 0.4
  else if (94<sum(matriz_variantes[i]) & sum(matriz_variantes[i])<191){
    matriz_peso[i]=0.4
  }
# variables presentes entre 191 y 237 lugares valen 0.2
  else if (sum(matriz_variantes[i])>190){
    matriz_peso[i]=0.2
  }
}


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
# Matriz de coidentidades sin peso
tabla_coi <- apply(matriz_variantes,1, 
             function(x) apply(matriz_variantes, 1,
                function(y) coi(x,y)
                ) 
             )

# Matriz de identidades con peso################ TODO


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

# Matriz de identidades relativas normalizada, llevada a diferencias para hcluster

IIR = 1-tabla_coi/(tabla_coi+tabla_cod)

# Matriz de coidentidades llevada a diferencias para hcluster
tabla_coi=1-tabla_coi


#############################################################
#                                                           #
#                     Teselados                             #
#                                                           #
#############################################################

# Clustering
cluster <- function(a, simdis="dist"){
  distancia = as.dist(a)
  cluster <- hclust(distancia)
  return (cluster)
}
################################
# Preparación de los teselados #
################################

colorear_dialectos <- function(nivel, tipo){
  llenado=as.data.frame(list(grupo=cutree(cluster(tipo),k=nivel)))
  row.names(llenado)=Id
  colnames(llenado)=c("grupo")
  llenado[,"grupo"]=as.vector(cutree(cluster(tipo),k=nivel))
  llenado$color=sapply(llenado[,"grupo"], 
                       function (x) RColorBrewer::brewer.pal(12, 'Paired')[round(seq(from = 1, to = 12, by = 11/(nivel-1)))[x]])
  return(llenado$color)
}

#Preparar teselado
teselado <- RJSONIO::fromJSON(GeoTeselation)

#Incluir número de respuestas vacias en los datos del mapa
for(i in 1:NROW(teselado["features"])){
  for(j in 1:NROW(teselado["features"][[i]])){
    teselado["features"][[i]][[j]]$properties$vacios=voidAnsPlace[j][[1]]
  }
}

save(Id,
     kgroups_choices, 
     colorear_dialectos, 
     cluster, 
     teselado, 
     diferencias, 
     similaridades, 
     IIR, 
     vacios, 
     comVoid, 
     comVoidMaps, 
     tabla_cod, 
     tabla_coi, 
     file = paste("/home/shiny/",ProjectName,".RData",sep=""))
