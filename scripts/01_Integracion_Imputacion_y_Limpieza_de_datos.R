# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# 1.1:  Integración y limpieza de los datos======
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Sys.setlocale(locale="es_ES.UTF-8")
pacman::p_load(dplyr,stringr,stringi,readxl,tidyr)
input <- ("G:/Mi unidad/Tesis/")
options(scipen=999)

# ------------------ Se cargan los datos del coneval
setwd(paste0(input,"data/coneval"))
coneval <- read.csv("indicadores de pobreza municipal 2015.csv", encoding = "latin1")



setwd(paste0(input,"data/censo_inegi"))
temp <- list.files(pattern = ".csv")
myfiles <- lapply(temp, read.csv, encoding = "UTF-8")
inegi <- do.call(rbind, myfiles)
rm(temp);rm(myfiles)


# Existen columnas en donde las tasas para las variables del coneval son calculadas
# previamente, se descartan ya que estan redondeadas
coneval <- coneval %>%
  select(-c("pobreza", "pobreza_e","pobreza_m","vul_car",
            "vul_ing","npnv","ic_rezedu","ic_asalud",
            "ic_segsoc","ic_cv","ic_sbv","ic_ali",
            "carencias","carencias3","plb","plbm"))


# Cambiamos el nombre de entidad en el inegi a uno más sencillo
names(inegi)[which(names(inegi)=="X.U.FEFF.ENTIDAD")] = "ENTIDAD"


# Nos interesa unicamente el total por municipio en la base de datos del inegi
# y solo nos quedamos con variables que vamos a ocupar 
inegi <- inegi%>% filter(NOM_LOC=='Total del Municipio')%>%
  select(-c("LOC", "NOM_LOC" ,
            "LONGITUD", "LATITUD", "ALTITUD") )

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# 1.2:  Cambios de los nombres en municipios y estados ======
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
inegi<-inegi%>%
  mutate(NOM_ENT=case_when(
    NOM_ENT=='Michoacán de Ocampo' ~ 'Michoacán',
    NOM_ENT=='Veracruz de Ignacio de la Llave' ~ 'Veracruz',
    NOM_ENT=='Coahuila de Zaragoza' ~ 'Coahuila',
    TRUE ~ NOM_ENT))

coneval<- coneval%>%
  mutate(entidad_federativa=case_when(
    entidad_federativa=='Distrito Federal'~'Ciudad de México',
    TRUE ~ entidad_federativa))%>%
  rename(NOM_ENT=entidad_federativa,
         NOM_MUN=municipio,
         ENTIDAD=clave_entidad,
         MUN=clave_municipio)


# En el INEGI y el coneval las claves de los municipios
# son distintas, así que esas no sirven para hacer un join 
# aparte una base es del 2020 y otra es del 2015, así
# que probablemente hay municipios nuevos
summary(coneval$MUN)
summary(inegi$MUN)
# así que se hace con el nombre de la entidad y el nombre
# del municipio, pero comprobamos que puedan hacer una llave única

inegi%>%
  group_by(NOM_ENT,NOM_MUN)%>%
  summarise(total=n())%>%
  filter(total>1)
# A tibble: 2 x 3
# Groups:   NOM_ENT 
# NOM_ENT    NOM_MUN            total

# Oaxaca     San Juan Mixtepec      2
# Oaxaca     San Pedro Mixtepec     2

coneval%>%
  group_by(NOM_ENT,NOM_MUN)%>%
  summarise(total=n())%>%
  filter(total>1) 

# Con el coneval sí se puede hacer una llave unica uniendo
# los nombres, pero para el inegi se tienen que arreglar 


# En el inegi sí están diferenciados por MUN, pero se
# les tiene que cambiar el nombre para que coincidan con 
# los del coneval

# "San Juan Mixtepec"
inegi%>%
  filter(NOM_MUN=="San Juan Mixtepec")
coneval %>%
  filter(str_detect(NOM_MUN, "San Juan Mixtepec"))


# "San Pedro Mixtepec"
inegi%>%
  filter(NOM_MUN=="San Pedro Mixtepec")
coneval %>%
  filter(str_detect(NOM_MUN, "San Pedro Mixtepec"))

# Se puede inferir por su población cuál es cuál
inegi<-inegi%>%
  mutate(NOM_MUN=case_when(
    MUN=='208' & NOM_ENT== 'Oaxaca'~ 'San Juan Mixtepec -Dto. 08 -',
    MUN=='209' & NOM_ENT== 'Oaxaca'~ 'San Juan Mixtepec -Dto. 26 -',
    MUN=='318' & NOM_ENT== 'Oaxaca'~ 'San Pedro Mixtepec -Dto. 22 -',
    MUN=='319' & NOM_ENT== 'Oaxaca'~ 'San Pedro Mixtepec -Dto. 26 -',
    TRUE ~ NOM_MUN)
    )
# se comprueba que los nombres formen un id unico y se prosigue
# * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# names(inegi)
# claves_ENT_MUN <- inegi%>%
#   select(ENTIDAD,NOM_ENT,MUN,NOM_MUN)
# * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# Quitamos las claves del coneval ya que ahora no serán necesarias
# inegi <- inegi%>% select(-c("ENTIDAD","MUN"))
coneval <- coneval%>% select(-c("ENTIDAD","MUN"))

# Ahora se pueden unir las bases de datos
interseccion_inegi_coneval <- merge(coneval, inegi,
                                    by=c("NOM_ENT","NOM_MUN"))
  # notemos que:
#   en el inegi hay 2469 renglones
#   en el coneval hay 2457 renglones
#   en la intersección hay 2433 renglones


# Vemos aquellos que están en el coneval pero no en la interseccion
dif_coneval <- setdiff(coneval%>%select(NOM_ENT, NOM_MUN), 
                      interseccion_inegi_coneval%>%select(NOM_ENT, NOM_MUN))%>%
                      arrange((NOM_MUN))
# se tienen 24 registros que estan en el coneval pero no en la interseccion

# Vemos aquellos que están en el inegi pero no en la interseccion
dif_inegi <-setdiff(inegi%>%select(NOM_ENT, NOM_MUN), 
                      interseccion_inegi_coneval%>%select(NOM_ENT, NOM_MUN))%>%
                       arrange((NOM_MUN))
# se tienen 36 registros que estan en el inegi pero no en la intersección


# Ahora se tienen que cambiar aquellos que sean los mismos pero el nombre
# este escrito distinto, hacemos que los del coneval coincidan
# con los del inegi 
coneval <- coneval %>%
  mutate(NOM_MUN=
           case_when(
             NOM_MUN=='Acambay' & NOM_ENT=='México' ~ 'Acambay de Ruíz Castañeda',
             NOM_MUN=='Batopilas' & NOM_ENT=='Chihuahua' ~ 'Batopilas de Manuel Gómez Morín',
             NOM_MUN=='Dr. Arroyo'& NOM_ENT=='Nuevo León' ~ 'Doctor Arroyo',
             NOM_MUN=='Dr. Coss'& NOM_ENT=='Nuevo León' ~ 'Doctor Coss',
             NOM_MUN=='Dr. González'& NOM_ENT=='Nuevo León' ~ 'Doctor González',
             NOM_MUN=='Carmen'& NOM_ENT=='Nuevo León' ~ 'El Carmen',
             NOM_MUN=='Gral. Bravo'& NOM_ENT=='Nuevo León' ~ 'General Bravo',
             NOM_MUN=='Gral. Escobedo'& NOM_ENT=='Nuevo León' ~ 'General Escobedo',
             NOM_MUN=='Gral. Terán'& NOM_ENT=='Nuevo León' ~ 'General Terán',
             NOM_MUN=='Gral. Treviño'& NOM_ENT=='Nuevo León' ~ 'General Treviño',
             NOM_MUN=='Gral. Zaragoza'& NOM_ENT=='Nuevo León' ~ 'General Zaragoza',
             NOM_MUN=='Gral. Zuazua'& NOM_ENT=='Nuevo León' ~ 'General Zuazua',
             NOM_MUN=='Heroica Ciudad de Juchitán de Zaragoza'& NOM_ENT=='Oaxaca' ~ 'Juchitán de Zaragoza',
             NOM_MUN=='Jonacatepec'& NOM_ENT=='Morelos'  ~ 'Jonacatepec de Leandro Valle',
             NOM_MUN=='José Joaquin de Herrera'& NOM_ENT=='Guerrero' ~ 'José Joaquín de Herrera',
             NOM_MUN=='Medellín' & NOM_ENT=='Veracruz'~ 'Medellín de Bravo',
             NOM_MUN=='San Mateo Yucutindó' & NOM_ENT=='Oaxaca'~ 'San Mateo Yucutindoo',
             NOM_MUN=='Santiago Chazumba'& NOM_ENT=='Oaxaca' ~ 'Villa de Santiago Chazumba',
             NOM_MUN=='Silao' & NOM_ENT=='Guanajuato' ~ 'Silao de la Victoria',
             NOM_MUN=='Tlaltizapán'& NOM_ENT=='Morelos' ~ 'Tlaltizapán de Zapata',
             NOM_MUN=='Tezoatlán de Segura y Luna'& NOM_ENT=='Oaxaca' ~ 'Heroica Villa Tezoatlán de Segura y Luna, Cuna de',
             NOM_MUN=='Tlaltizapán'& NOM_ENT=='Morelos' ~ 'Tlaltizapán de Zapata',
             NOM_MUN=='Tlaquepaque'& NOM_ENT=='Jalisco'  ~ 'San Pedro Tlaquepaque',
             NOM_MUN=='Villa de Tututepec de Melchor Ocampo'& NOM_ENT=='Oaxaca' ~ 'Villa de Tututepec',
             NOM_MUN=='Zacualpan'& NOM_ENT=='Morelos' ~ 'Zacualpan de Amilpas', 
             TRUE ~ NOM_MUN)
         )

# Si se vuelven a correr las intersecciones en el coneval hay 0

# Existen 12 municipios nuevos, que aparecen en la encuesta del inegi 2020
# pero no aparecen en la encuesta del coneval del 2015:
dim(inegi)-dim(coneval)

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# 2.0: Join entre coneval e inegi=====
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
inegi_y_coneval <- merge(inegi, coneval,
                         by=c("NOM_ENT","NOM_MUN"),
                         all.x=TRUE)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# 2.1: Transformar las variables en tasas del INEGI ======
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

# Los datos del inegi van desde POBTOT hasta TAMLOC, saldrán warnings porque no es númerica
subdata_inegi <- inegi[which(colnames(inegi_y_coneval)=="POBTOT"): which(colnames(inegi_y_coneval)=="TAMLOC")]
#Se convierten los datos a numericos
subdata_inegi <- as.data.frame(sapply(subdata_inegi, as.numeric))

# Se usa un archivo auxiliar para calcular las tasas del inegi
setwd(input)
para_tasas_inegi <- read_excel("Comparacion_Variables.xlsx")%>%
                    select(Variables,dividir_entre)%>%
                    drop_na()
names(para_tasas_inegi) <- c("variable","division")


# Se realiza la conversion 
for(v in 1:dim(para_tasas_inegi)[1]){
  variable <- para_tasas_inegi$variable[v]
  division <- para_tasas_inegi$division[v]
  if(division=="no se divide"){
    subdata_inegi[variable]<-subdata_inegi[variable]
  }else{
    subdata_inegi[variable]<-subdata_inegi[variable]/subdata_inegi[division]
  }
};rm(v)

# ----------------- comprobación:
# elegimos una variable al azar
n <- sample(1:dim(subdata_inegi)[2],1)
var <- names(subdata_inegi)[n]
var

# la buscamos en el csv pra ver entre qué se tiene que dividir y vemos si sí
division <- (para_tasas_inegi%>%
               filter(variable==var))$division
aux_comprobacion <- inegi[,c(var, division)]
aux_comprobacion$tasa <- as.numeric(aux_comprobacion[,1])/as.numeric(aux_comprobacion[,2])
# comprobación: correcto
aux_comprobacion$tasa==subdata_inegi[var]

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#2.2: Transformar las variables en tasas del CONEVAL=======
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# Las variables del coneval se divien entre 'poblacion', a partir de la variable
# pobreza_pob y hasta plbm_pob, se agrega también el estado y municipio
# El coneval tiene 12 na'as y 11 "n.d", en estos renglones no se pueden
# hacer operaciones
subdata_coneval <- inegi_y_coneval[which(colnames(inegi_y_coneval)=="poblacion"): 
                                     which(colnames(inegi_y_coneval)=="plbm_pob")]

# excepcion_renglones <- c(which(subdata_coneval$poblacion=="n.d"),
#                          which(is.na(subdata_coneval$pobreza_e_pob)))

subdata_coneval <- as.data.frame(sapply(subdata_coneval, as.numeric))

# se calculan las tasas desde pobreza_pob hasta la última columna

for (i in names(subdata_coneval)[-1]){
  subdata_coneval[i] <-  (subdata_coneval[i])/(subdata_coneval["poblacion"])
}
rm(i) # Salen warnings por los NA's de los estados que no tienen info para el coneval

# ----------------- comprobación:
# elegimos una variable al azar
var <- sample(names(subdata_coneval)[-1],1)
var

aux <- inegi_y_coneval%>%
  select(NOM_MUN, var,poblacion)%>%
  mutate(poblacion=as.numeric(poblacion))
aux$division<- as.numeric(aux[,2])/aux["poblacion"]

tasa <- subdata_coneval%>%
  select(var)

# comprobación: correcto
tasa==aux$division


# Se agrega la población total, el estado y municipio a las dos subdatas
# y al del inegi también se le agrega las claves 
subdata_coneval <- cbind(inegi_y_coneval[which(colnames(inegi_y_coneval)=="NOM_MUN")],subdata_coneval)
subdata_coneval <- cbind(inegi_y_coneval[which(colnames(inegi_y_coneval)=="NOM_ENT")],subdata_coneval)

subdata_inegi <- cbind(inegi[which(colnames(inegi)=="MUN")],subdata_inegi)
subdata_inegi <- cbind(inegi[which(colnames(inegi)=="ENTIDAD")],subdata_inegi)
subdata_inegi <- cbind(inegi[which(colnames(inegi)=="NOM_MUN")],subdata_inegi)
subdata_inegi <- cbind(inegi[which(colnames(inegi)=="NOM_ENT")],subdata_inegi)

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#3.1: Imputar los datos faltantes=======
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


# Se ve si hay datos faltantes
which(apply(subdata_inegi, 2, function(x) any(is.na(x)))==TRUE) 
# Hay datos faltantes en TAMLOC pero esa se quitará

which(apply(subdata_coneval, 2, function(x) any(is.na(x)))==TRUE)
length(which(apply(subdata_coneval, 2, function(x) any(is.na(x)))==TRUE))
# Existen 17 datos faltantes para todas las columnas en los datos del coneval

# Estos son:
faltantes <- which((rowSums(is.na(subdata_coneval)) > 0) == TRUE) #162 en total
faltantes
length(faltantes) #23 datos faltantes en la base del coneval

# Se van a modificar las variables del coneval, es decir, desde pobreza_pob
# hasta plbm_pob 
i <- which(names(subdata_coneval)=="pobreza_pob")
j <- which(names(subdata_coneval)=="plbm_pob")

# Son 12 los municipios que surgieron entre 2015 y 2020 por lo que
# aparecen en la base de datos del inegi pero no del coneval 
# Esta información se obtuvo de AGEEML_2021682212978.csv



# * * * * * * * * * * * * * * * * * * * * * * San Quintín - Baja California
# Proviene de: (1)
# NOM_MUN: Ensenada
# NOM_ENT: Baja California

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Baja California' & 
                        subdata_coneval['NOM_MUN'] == 'San Quintín'),i:j] <- subdata_coneval[
                          which(subdata_coneval['NOM_ENT'] == 'Baja California' & 
                                  subdata_coneval['NOM_MUN'] == 'Ensenada'), i:j]

# * * * * * * * * * * * * * * * * * * * * * * Seybaplaya - Campeche
# Proviene de: (2)
# NOM_MUN: Champotón
# NOM_ENT: Campeche

imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Campeche' & 
                                   subdata_coneval['NOM_MUN'] == 'Champotón'),i:j]

# NOM_MUN: Campeche
# NOM_ENT: Campeche
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Campeche' & 
                                                  subdata_coneval['NOM_MUN'] == 'Campeche'),i:j])


imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))


subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Campeche' & 
                        subdata_coneval['NOM_MUN'] == 'Seybaplaya'),i:j] <- imputar

rm(imputar)


# * * * * * * * * * * * * * * * * Capitán Luis Ángel Vidal - Chiapas
# Proviene de: (1) 
# NOM_MUN: Siltepec
# NOM_ENT: Chiapas

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                        subdata_coneval['NOM_MUN'] == 'Capitán Luis Ángel Vidal'), i:j] <- subdata_coneval[
                          which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                  subdata_coneval['NOM_MUN'] == 'Siltepec'),i:j]


# * * * * * * * * * * * * * * * * Rincón Chamula San Pedro - Chiapas
# Proviene de: (4) 

# NOM_MUN: Jitotol
# NOM_ENT: Chiapas

imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                   subdata_coneval['NOM_MUN'] == 'Jitotol'),i:j]

# NOM_MUN: Rayón
# NOM_ENT: Chiapas

imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                                  subdata_coneval['NOM_MUN'] == 'Rayón'),i:j])

# NOM_MUN: Tapilula
# NOM_ENT: Chiapas

imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                                  subdata_coneval['NOM_MUN'] == 'Tapilula'),i:j])


# NOM_MUN: Pueblo Nuevo Solistahuacán
# NOM_ENT: Chiapas

imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                                  subdata_coneval['NOM_MUN'] == 'Pueblo Nuevo Solistahuacán'),i:j])

imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))


subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                        subdata_coneval['NOM_MUN'] == 'Rincón Chamula San Pedro'),i:j] <- imputar

rm(imputar)

# * * * * * * * * * * * * * * * * * * * El Parral - Chiapas
# Proviene de: (2)

# NOM_MUN: Villaflores
# NOM_ENT: Chiapas

imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                   subdata_coneval['NOM_MUN'] == 'Villaflores'),i:j]

# NOM_MUN: Villa Corzo
# NOM_ENT: Chiapas

imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                                  subdata_coneval['NOM_MUN'] == 'Villa Corzo'),i:j])


imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))


subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                        subdata_coneval['NOM_MUN'] == 'El Parral'),i:j] <- imputar

rm(imputar)

# * * * * * * * * * * * * * * * * * * * Emiliano Zapata - Chiapas
# Proviene de: (3)

# NOM_MUN: Venustiano Carranza
# NOM_ENT: Chiapas

imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                   subdata_coneval['NOM_MUN'] == 'Venustiano Carranza'),i:j]

# NOM_MUN: Chiapa de Corzo
# NOM_ENT: Chiapas

imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                                  subdata_coneval['NOM_MUN'] == 'Chiapa de Corzo'),i:j])

# NOM_MUN: Acala
# NOM_ENT: Chiapas

imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                                  subdata_coneval['NOM_MUN'] == 'Acala'),i:j])

imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))


subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                        subdata_coneval['NOM_MUN'] == 'Emiliano Zapata'),i:j] <- imputar

rm(imputar)

# * * * * * * * * * * * * * * * * * * * Mezcalapa - Chiapas
# Proviene de: (3)

# NOM_MUN: Ocozocoautla de Espinosa
# NOM_ENT: Chiapas

imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                   subdata_coneval['NOM_MUN'] == 'Ocozocoautla de Espinosa'),i:j]


# NOM_MUN: Tecpatán
# NOM_ENT: Chiapas

imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                                  subdata_coneval['NOM_MUN'] == 'Tecpatán'),i:j])

# NOM_MUN: Ostuacán
# NOM_ENT: Chiapas

imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                                  subdata_coneval['NOM_MUN'] == 'Ostuacán'),i:j])

imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))


subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                        subdata_coneval['NOM_MUN'] == 'Mezcalapa'),i:j] <- imputar

rm(imputar)


# * * * * * * * * * * * * * * * * Honduras de la Sierra - Chiapas
# Proviene de: (1)

# NOM_MUN: Siltepec
# NOM_ENT: Chiapas

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                        subdata_coneval['NOM_MUN'] == 'Honduras de la Sierra'), i:j] <- subdata_coneval[
                          which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                  subdata_coneval['NOM_MUN'] == 'Siltepec'),i:j]


# * * * * * * * * * * * * * * * * * * Coatetelco - Morelos
# Proviene de: (1)

# NOM_MUN: Miacatlán
# NOM_ENT: Morelos

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Morelos' & 
                        subdata_coneval['NOM_MUN'] == 'Coatetelco'),i:j] <- subdata_coneval[
                          which(subdata_coneval['NOM_ENT'] == 'Morelos' & 
                                  subdata_coneval['NOM_MUN'] == 'Miacatlán'),i:j]

# * * * * * * * * * * * * * * * * * * Xoxocotla - Morelos
# Proviene de: (1)

# NOM_MUN: Puente de Ixtla
# NOM_ENT: Morelos

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Morelos' & 
                        subdata_coneval['NOM_MUN'] == 'Xoxocotla'),i:j] <- subdata_coneval[
                          which(subdata_coneval['NOM_ENT'] == 'Morelos' & 
                                  subdata_coneval['NOM_MUN'] == 'Puente de Ixtla'),i:j]


# * * * * * * * * * * * * * * * * * * Hueyapan - Morelos
# Proviene de: (1)

# NOM_MUN: Tetela del Volcán
# NOM_ENT: Morelos

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Morelos' & 
                        subdata_coneval['NOM_MUN'] == 'Hueyapan'),i:j] <- subdata_coneval[
                          which(subdata_coneval['NOM_ENT'] == 'Morelos' & 
                                  subdata_coneval['NOM_MUN'] == 'Tetela del Volcán'),i:j
                        ]

# * * * * * * * * * * * * * * * * * * Puerto Morelos - Quintana Roo
# Proviene de: (1)

# NOM_MUN: Benito Juárez
# NOM_ENT: Quintana Roo

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Quintana Roo' & 
                        subdata_coneval['NOM_MUN'] == 'Puerto Morelos'), i:j] <- subdata_coneval[
                          which(subdata_coneval['NOM_ENT'] == 'Quintana Roo' & 
                                  subdata_coneval['NOM_MUN'] == 'Benito Juárez'),i:j]

rm(i);rm(j)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * otros municipios que no tienen datos en el coneval * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

faltantes <- which(is.na(subdata_coneval$vul_car_pob)==TRUE)
length(faltantes) 

subdata_coneval[faltantes,c("NOM_ENT","NOM_MUN")]
i <- which(names(subdata_coneval)=="pobreza_pob")
j <- which(names(subdata_coneval)=="plbm_pob")

subdata_coneval[faltantes,c("NOM_ENT","NOM_MUN")]


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * Buenaventura - Chihuahua
# Vecinos: (7) LISTO ***
#   Ascensión - Chihuahua
#   Nuevo Casas Grandes - Chihuahua
#   Galeana - Chihuahua
#   Ignacio Zaragoza - Chihuahua
#   Namiquipa - Chihuahua
#   Chihuahua - Chihuahua
#   Ahumada - Chihuahua


#   Ascensión - Chihuahua
imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                   subdata_coneval['NOM_MUN'] == 'Ascensión'),i:j]
#   Nuevo Casas Grandes - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Nuevo Casas Grandes'),i:j])
#   Galeana - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Galeana'),i:j])
#   Ignacio Zaragoza - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Ignacio Zaragoza'),i:j])
#   Namiquipa - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Namiquipa'),i:j])
#   Chihuahua - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Chihuahua'),i:j])
#   Ahumada - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Ahumada'),i:j])

imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))


subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                        subdata_coneval['NOM_MUN'] == 'Buenaventura'),i:j] <- imputar

rm(imputar)

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * Carichí - Chihuahua
# Vecinos: (6) LISTO ***
#   Guerrero - Chihuahua
#   Bocoyna - Chihuahua
#   Guachochi - Chihuahua
#   Nonoava - Chihuahua
#   San Francisco de Borja - Chihuahua
#   Cusihuiriachi - Chihuahua

#   Guerrero - Chihuahua
imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                   subdata_coneval['NOM_MUN'] == 'Guerrero'),i:j]
#   Bocoyna - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Bocoyna'),i:j])
#   Guachochi - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Guachochi'),i:j])
#   Nonoava - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Nonoava'),i:j])
#   San Francisco de Borja - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'San Francisco de Borja'),i:j])
#   Cusihuiriachi - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Cusihuiriachi'),i:j])

imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                        subdata_coneval['NOM_MUN'] == 'Carichí'),i:j] <- imputar

rm(imputar)
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * Santa Isabel - Chihuahua
# Vecinos: (6) LISTO ***
#   Chihuahua - Chihuahua
#   Riva Palacio - Chihuahua
#   Cuauhtémoc - Chihuahua
#   Gran Morelos - Chihuahua
#   Dr. Belisario Domínguez - Chihuahua
#   Satevó - chihuahua


#   Chihuahua - Chihuahua
imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                   subdata_coneval['NOM_MUN'] == 'Chihuahua'),i:j]
#   Riva Palacio - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Riva Palacio'),i:j])
#   Cuauhtémoc - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Cuauhtémoc'),i:j])
#   Gran Morelos - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Gran Morelos'),i:j])
#   Dr. Belisario Domínguez - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Dr. Belisario Domínguez'),i:j])
#   Satevó - chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Satevó'),i:j])

imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                        subdata_coneval['NOM_MUN'] == 'Santa Isabel'),i:j] <- imputar

rm(imputar)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * Temósachic - Chihuahua
# Vecinos: (9) LISTO ***
#   Madera - Chihuahua
#   Moris - Chihuahua
#   Ocampo - Chihuahua
#   Guerrero - Chihuahua
#   Matachí - Chihuahua
#   Namiquipa - Chihuahua
#   Gómez Farías - Chihuahua
#   Sahuaripa - Sonora - 052
#   Yécora - Sonora - 069

#   Madera - Chihuahua 1
imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                   subdata_coneval['NOM_MUN'] == 'Madera'),i:j]
#   Moris - Chihuahua 2
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Moris'),i:j])
#   Ocampo - Chihuahua 3
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Ocampo'),i:j])
#   Guerrero - Chihuahua 4
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Guerrero'),i:j])
#   Matachí - Chihuahua 5
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Matachí'),i:j])
#   Namiquipa - Chihuahua 6
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Namiquipa'),i:j])
#   Gómez Farías - Chihuahua 7
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Gómez Farías'),i:j])
#   Sahuaripa - Sonora 8
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Sonora' & 
                                                  subdata_coneval['NOM_MUN'] == 'Sahuaripa'),i:j])
#   Yécora - Sonora 9
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Sonora' & 
                                                  subdata_coneval['NOM_MUN'] == 'Yécora'),i:j])

imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                        subdata_coneval['NOM_MUN'] == 'Temósachic'),i:j] <- imputar

rm(imputar)

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * Urique - Chihuahua
# Vecinos:(6) LISTO ***
#   Guazapares - Chihuahua
#   Batopilas de Manuel Gómez Morín - Chihuahua
#   Guachochi - Chihuahua
#   Bocoyna - Chihuahua
#   Maguarichi - Chihuahua
#   Choix - Sinaloa


#   Guazapares - Chihuahua
imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                   subdata_coneval['NOM_MUN'] == 'Guazapares'),i:j]
#   Batopilas de Manuel Gómez Morín - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Batopilas de Manuel Gómez Morín'),i:j])
#   Guachochi - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Guachochi'),i:j])
#   Bocoyna - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Bocoyna'),i:j])
#   Maguarichi - Chihuahua
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                                                  subdata_coneval['NOM_MUN'] == 'Maguarichi'),i:j])
#   Choix - Sinaloa
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Sinaloa' & 
                                                  subdata_coneval['NOM_MUN'] == 'Choix'),i:j])

imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chihuahua' & 
                        subdata_coneval['NOM_MUN'] == 'Urique'),i:j] <- imputar

rm(imputar)

# * * * * * * * * * * * * * * * * * * * * * *  Matías Romero Avendaño - Oaxaca
# Vecinos:(7) LISTO*** , en realidad 5 porque 427 no tiene info, ni el 407 *
#   091 - Veracruz - Jesús Carranza
#   190 - Oaxaca - San Juan Cotzocón
#   207 - Oaxaca - 	San Juan Mazatlán
#   198 - Oaxaca - 	San Juan Guichicovi
#   427 - Oaxaca - Santa María Petapa *
#   010 - Oaxaca - El Barrio de la Soledad
#   407 - Oaxaca - Santa María Chimalapa *


#   091 - Veracruz - Jesús Carranza 1
imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Veracruz' & 
                                   subdata_coneval['NOM_MUN'] == 'Jesús Carranza'),i:j]
#   190 - Oaxaca - San Juan Cotzocón 2
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'San Juan Cotzocón'),i:j])
#   207 - Oaxaca - 	San Juan Mazatlán 3
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'San Juan Mazatlán'),i:j])
#   198 - Oaxaca - 	San Juan Guichicovi 4 
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'San Juan Guichicovi'),i:j])

#   427 - Oaxaca - Santa María Petapa  (no tiene datos para imputar así que se descarta)
#imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
#                                             subdata_coneval['NOM_MUN'] == 'Santa María Petapa'),i:j])

#   010 - Oaxaca - El Barrio de la Soledad 5
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'El Barrio de la Soledad'),i:j])

#   407 - Oaxaca - Santa María Chimalapa, tampoco tiene datos
#imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
#                                            subdata_coneval['NOM_MUN'] == 'Santa María Chimalapa'),i:j])


imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                        subdata_coneval['NOM_MUN'] == 'Matías Romero Avendaño'),i:j] <- imputar

rm(imputar) 

# * * * * * * * * * * * * * * * * * * * * * *  San Francisco Chindúa - Oaxaca
# Vecinos: (7) LISTO ***
#   332 - Oaxaca - 	San Pedro Topiltepec
#   479 - Oaxaca - 	Santiago Nejapilla
#   518 - Oaxaca - Santo Domingo Tlatayápam	
#   147 - Oaxaca - San Francisco Nuxaño
#   281 - Oaxaca - 	San Miguel Tecomatlán
#   250 - Oaxaca - San Mateo Etlatongo
#   215 - Oaxaca - San Juan Sayultepec


#   332 - Oaxaca - 	San Pedro Topiltepec 1
imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                   subdata_coneval['NOM_MUN'] == 'San Pedro Topiltepec'),i:j]
#   479 - Oaxaca - 	Santiago Nejapilla 2
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'Santiago Nejapilla'),i:j])
#   518 - Oaxaca - Santo Domingo Tlatayápam	 3
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'Santo Domingo Tlatayápam'),i:j])
#   147 - Oaxaca - San Francisco Nuxaño 4
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'San Francisco Nuxaño'),i:j])
#   281 - Oaxaca - 	San Miguel Tecomatlán 5
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'San Miguel Tecomatlán'),i:j])
#   250 - Oaxaca - San Mateo Etlatongo 6
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'San Mateo Etlatongo'),i:j])
#   215 - Oaxaca - San Juan Sayultepec 7
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'San Juan Sayultepec'),i:j])

imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                        subdata_coneval['NOM_MUN'] == 'San Francisco Chindúa'),i:j] <- imputar

rm(imputar) 
# * * * * * * * * * * * * * * * * * * * * * *  Santa María Chimalapa - Oaxaca
# Vecinos: (8) LISTO *
#   057 - Oaxaca - 	Matías Romero Avendaño
#   010 - Oaxaca - 	El Barrio de la Soledad
#   005 - Oaxaca - 	Asunción Ixtaltepec
#   265 - Oaxaca - 	San Miguel Chimalapa
#   017 - Chiapas - Cintalapa
#   061 - Veracruz - 	Las Choapas
#   210 - Veracruz - Uxpanapa 
#   091 - Veracruz - Jesús Carranza


#   057 - Oaxaca - 	Matías Romero Avendaño 1
imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                   subdata_coneval['NOM_MUN'] == 'Matías Romero Avendaño'),i:j]
#   010 - Oaxaca - 	El Barrio de la Soledad 2
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'El Barrio de la Soledad'),i:j])
#   005 - Oaxaca - 	Asunción Ixtaltepec 3
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'Asunción Ixtaltepec'),i:j])
#   265 - Oaxaca - 	San Miguel Chimalapa 4
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'San Miguel Chimalapa'),i:j])
#   017 - Chiapas - Cintalapa 5
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Chiapas' & 
                                                  subdata_coneval['NOM_MUN'] == 'Cintalapa'),i:j])
#   061 - Veracruz - 	Las Choapas 6
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Veracruz' & 
                                                  subdata_coneval['NOM_MUN'] == 'Las Choapas'),i:j])
#   210 - Veracruz - Uxpanapa 7
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Veracruz' & 
                                                  subdata_coneval['NOM_MUN'] == 'Uxpanapa'),i:j])
#   091 - Veracruz - Jesús Carranza 8
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Veracruz' & 
                                                  subdata_coneval['NOM_MUN'] == 'Jesús Carranza'),i:j])

imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                        subdata_coneval['NOM_MUN'] == 'Santa María Chimalapa'),i:j] <- imputar

rm(imputar) 
# * * * * * * * * * * * * * * * * * * * * * *  Santa María Petapa - Oaxaca
# Vecinos: (4) LISTO *
#   057 - Oaxaca - 	Matías Romero Avendaño
#   198 - Oaxaca - 	San Juan Guichicovi
#   513 - Oaxaca - 	Santo Domingo Petapa
#   010 - Oaxaca - 	El Barrio de la Soledad


#   057 - Oaxaca - 	Matías Romero Avendaño 1
imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                   subdata_coneval['NOM_MUN'] == 'Matías Romero Avendaño'),i:j]
#   010 - Oaxaca - 		San Juan Guichicovi 2
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'San Juan Guichicovi'),i:j])
#   513 - Oaxaca - 	Santo Domingo Petapa 3
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'Santo Domingo Petapa'),i:j])
#   010 - Oaxaca - 	El Barrio de la Soledad 4
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                                                  subdata_coneval['NOM_MUN'] == 'El Barrio de la Soledad'),i:j])

imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Oaxaca' & 
                        subdata_coneval['NOM_MUN'] == 'Santa María Petapa'),i:j] <- imputar
rm(imputar) 

# * * * * * * * * * * * * * * * * * * * * * *  San Nicolás de los Ranchos - Puebla 
# Vecinos: (7) LISTO ***
#   009 - NOM_ENT de México - Amecameca
#   015 - NOM_ENT de México - Atlautla
#   188 - Puebla - Tochimilco
#   175 - Puebla - Tianguismanalco
#   102 - Puebla - Nealtican
#   026 - Puebla - Calpan
#   074 - Puebla - Huejotzingo

#   009 - NOM_ENT de México - Amecameca 1
imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'México' & 
                                   subdata_coneval['NOM_MUN'] == 'Amecameca'),i:j]
#   015 - NOM_ENT de México - Atlautla 2
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'México' & 
                                                  subdata_coneval['NOM_MUN'] == 'Atlautla'),i:j])
#   188 - Puebla - Tochimilco 3
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Puebla' & 
                                                  subdata_coneval['NOM_MUN'] == 'Tochimilco'),i:j])
#   175 - Puebla - Tianguismanalco 4
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Puebla' & 
                                                  subdata_coneval['NOM_MUN'] == 'Tianguismanalco'),i:j])
#   102 - Puebla - Nealtican 5
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Puebla' & 
                                                  subdata_coneval['NOM_MUN'] == 'Nealtican'),i:j])
#   026 - Puebla - Calpan 6
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Puebla' & 
                                                  subdata_coneval['NOM_MUN'] == 'Calpan'),i:j])
#   074 - Puebla - Huejotzingo 7
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Puebla' & 
                                                  subdata_coneval['NOM_MUN'] == 'Huejotzingo'),i:j])

imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Puebla' & 
                        subdata_coneval['NOM_MUN'] == 'San Nicolás de los Ranchos'),i:j] <- imputar
rm(imputar) 


# * * * * * * * * * * * * * * * * * * * * * *  General Plutarco Elías Calles - Sonora
# Vecinos: (2) LISTO ***
#   048 - Sonora - Puerto Peñasco
#   017 - Sonora - Caborca

#   048 - Sonora - Puerto Peñasco
imputar <- subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Sonora' & 
                                   subdata_coneval['NOM_MUN'] == 'Puerto Peñasco'),i:j]
#   017 - Sonora - Caborca
imputar <- rbind(imputar, subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Sonora' & 
                                                  subdata_coneval['NOM_MUN'] == 'Caborca'),i:j])

imputar <- sapply(imputar, as.numeric)
imputar <- as.data.frame(t(apply((imputar),MARGIN=2,FUN=mean)))

subdata_coneval[which(subdata_coneval['NOM_ENT'] == 'Sonora' & 
                        subdata_coneval['NOM_MUN'] == 'General Plutarco Elías Calles'),i:j] <- imputar
rm(imputar) 

rm(i);rm(j)

# ---- Comprobación de que ya no hay NOM_MUNs faltantes de datos:
faltantes <- which(is.na(subdata_coneval$vul_ing_pob)==TRUE)
length(faltantes)
rm(faltantes)

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#3.2: Juntar todos los datos s=======
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

setwd(input)
variables_inegi <- read_excel("Comparacion_Variables.xlsx")

# Se comprueban las dimensiones: 
#   se deben tener  2469 renglones
dim(subdata_coneval)[1]==2469
dim(subdata_inegi)[1]==2469

# las columnas deben de ser las mismas
dim(inegi)[2]==dim(subdata_inegi)[2]
dim(coneval)[2]==dim(subdata_coneval)[2]

# se debe pegar por NOM_ENT y NOM_MUN
names(subdata_inegi)
names(subdata_coneval)

# Ahora se deben de juntar los datos de las tasas en una sola base de nuevo
data_tasas <- merge(subdata_coneval,subdata_inegi, by=c("NOM_ENT","NOM_MUN"))
nombres_limpios <- data_tasas%>%select(NOM_ENT,NOM_MUN, ENTIDAD, MUN)
# Menos dos porque son las keys
dim(inegi)[2]+dim(coneval)[2]-2==dim(data_tasas)[2]


# -------------- Comprobación final de que los datos son correctos
# Para las siguientes funciones se elige un municipio y una variable
# al azar, y se comparan con sacar los resultados para esa variable
# y ese municipio en la data original (inegi y coneval)
# Esto es porque ya se checo por variables 

# - - - - - - comprobación INEGI
comprobacion_inegi <- function(){
  # elegimos un municipio 
  mun <- sample(data_tasas$NOM_MUN,1)
  # elegimos una variable al azar
  resultante <- data_tasas[which(colnames(data_tasas)=="POBTOT"): which(colnames(data_tasas)=="VPH_SINTIC")]
  var_inegi <- sample(names(resultante)[-1],1)
  var_inegi
  aux_inegi <- inegi%>%
    filter(NOM_MUN==mun)
  
  # Debemos de ver entre qué variable dividir 
  divide_inegi <- para_tasas_inegi%>%
    filter(variable==var_inegi)
  divide_inegi <- divide_inegi$division
  # la dividimos desde el inegi 
  var_inegi
  
  x <- as.numeric(aux_inegi[var_inegi]) / as.numeric(aux_inegi[divide_inegi])
  
  y <- data_tasas%>%
    filter(NOM_MUN==mun)%>%
    select(var_inegi)
  
  if(x==y){
    z <- "esta bien :)"
  }else{
    z <- "algo falló :("
  }
  
  o <- paste0(mun, ", ", var_inegi,", ",z)
  return(o)
  
}
comprobacion_inegi()

# - - - - - - comprobación coneval
comprobacion_coneval <- function(){
  # elegimos un municipio 
  mun <- sample(data_tasas$NOM_MUN,1)

  # elegimos una variable al azar
  resultante <- data_tasas[which(colnames(data_tasas)=="poblacion"): which(colnames(data_tasas)=="plbm_pob")]
  var_coneval <- sample(names(resultante)[-1],1)
  var_coneval
  aux_coneval <- coneval%>%
    filter(NOM_MUN==mun)
  
  # Todas se dividieron entre la poblacion
  
  # la dividimos desde el inegi 
  var_coneval
  
  x <- as.numeric(aux_coneval[var_coneval]) / as.numeric(aux_coneval$poblacion)
  y <- data_tasas%>%
    filter(NOM_MUN==mun)%>%
    select(var_coneval)
  

  if(is.na(x)!=TRUE){
    if(x==y){
      z <- "esta bien :)"
    }else{
      z <- "algo falló :("
    }
  }else{
    z <- "El municipio fue imputado"
  }
  
  o <- paste0(mun, ", ", var_coneval,", ",z)
  return(o)
}
comprobacion_coneval()



# El último detalle es hacer que todas las variables queden entre
# 0 y 1, estas son los promedios 
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data_tasas %>% select_if(~any(. > 1))
# identificamos los nombres y buscamos alguna coincidencia con pro (usualmente así se llaman 
#                                                                   los promedios)
mayores_a_1<-names(data_tasas %>% select_if(~any(. > 1)))
mayores_a_1[grep("PRO",mayores_a_1)]

# también se checa a "mano"
mayores_a_1
# y los que son promedios son:
# PROM_HNV: Promedio de hijas e hijos nacidos vivos 
# GRAPROES 
# GRAPROES_F
# GRAPROES_M
# PROM_OCUP
# PRO_OCUP_C 


data_tasas <- data_tasas%>%
  mutate(PROM_HNV=min_max_norm(data_tasas$PROM_HNV),
         GRAPROES=min_max_norm(data_tasas$GRAPROES),
         GRAPROES_F=min_max_norm(data_tasas$GRAPROES_F),
         GRAPROES_M=min_max_norm(data_tasas$GRAPROES_M),
         PROM_OCUP=min_max_norm(data_tasas$PROM_OCUP),
         PRO_OCUP_C=min_max_norm(data_tasas$PRO_OCUP_C))




# de aquí solo vamos a ocupar data_tasas, inegi y coneval 
rm(list=setdiff(ls(), c("inegi","coneval","data_tasas",
                        "inegi_y_coneval","claves_ENT_MUN",
                        "nombres_limpios")))
# setwd(input)
save.image("01_Integracion_Imputacion_y_Limpieza_de_datos.Rdata")

datos<-data_tasas%>%left_join(nombres_limpios)%>%
  mutate(cve=paste0(ENTIDAD,"_",MUN))
getwd()
write.csv(datos,"datos.csv")



datos_inegi <- merge(datos, inegi, by=c("ENTIDAD","MUN"))
