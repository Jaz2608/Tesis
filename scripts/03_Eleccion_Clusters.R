input <- ("G:/Mi unidad/Tesis/")
pacman::p_load(NbClust,ClustGeo,ggplot2,readxl,purrr,
               cluster,factoextra,tidyr,dplyr,stringr)

rm(list=setdiff(ls(),"input"))
setwd(input)
load("02_Creacion_de_Clusters.Rdata")

#  Se cargan los datos del censo con el que se hará el muestreo
setwd(paste0(input,"data/eceg_2020_csv/conjunto_de_datos/"))
# distrito <- read.csv("INE_DISTRITO_2020.csv")
# entidad <- read.csv("INE_ENTIDAD_2020.csv")
seccion <- read.csv("INE_SECCION_2020.csv")%>%
  mutate(across(c(where(is.character)), as.numeric))%>%
  rename(MUN=MUNICIPIO)


# * * * * * * * * * * * * * * * * * * * * * * * * 
# se comprueba que la base de sección sea una partición 
# de la del inegi por municipios 
sum(seccion$POBTOT)
sum(data_tasas$POBTOT)

dim(seccion%>%
      group_by(ENTIDAD,MUN)%>%
      summarise(total=n()))
# pero las claves de los municipios no coinciden por lo que no se 
# podrán unir fácilmente 
summary(seccion$MUN)
summary(data_tasas$MUN)

# por ejemplo:
inegi_y_coneval%>%
  filter(ENTIDAD==3)%>%
  distinct(NOM_ENT, NOM_MUN, ENTIDAD, MUN)

seccion%>%
  filter(ENTIDAD==3)%>%
  distinct(ENTIDAD,MUN)

# Como la clave de municipio no coincide se tiene que hacer una limpieza
# de los nombres para poder hacer el join

# * * * * * * * * * * * * * * * * * * * * * * * * 
# 1.1 Se arreglan los nombres de las bases====
# * * * * * * * * * * * * * * * * * * * * * * * * 

# Como la clave de municipio no coincide en ambas bases se tiene que hacer una limpieza
# de los nombres para poder hacer el join

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# Las bases de datos que se ocuparán 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
obten_secciones<-function(){
  # Se ocupa el catalogo de las secciones para poder tener sus nombres 
  cat_secciones_2020 <- read.csv("G:/Mi unidad/Tesis/data/eceg_2020_csv/catalogos/cat_secciones_2020.csv",
                                 encoding = "latin1")%>%
    distinct(CVE_ENT,CVE_MUN,DESC_MUN)%>%
    rename(ENTIDAD=CVE_ENT, NOM_MUN=DESC_MUN)%>%
    mutate(NOM_MUN=str_to_title(NOM_MUN),
           NOM_MUN=chartr("áéíóú", "aeiou", NOM_MUN),
           NOM_MUN=chartr("ÁÉÍÓÚ", "AEIOU", NOM_MUN))
  
  # * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
  # Se hacen los cambios generales que se hicieron en INEGI - municipios 
  # * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
  
  # Los que dicen Gral. se reemplazan por General y Dr. por Doctor
  cat_secciones_2020$NOM_MUN <- gsub("Gral.", "General", cat_secciones_2020$NOM_MUN)
  cat_secciones_2020$NOM_MUN <- gsub("Dr.", "Doctor", cat_secciones_2020$NOM_MUN)
  
  
  # Los municipios de San Juan Mixtepec y San Pedro Mixtepec vienen repetidos
  cat_secciones_2020 <- cat_secciones_2020%>%
    mutate(NOM_MUN=case_when(
      CVE_MUN=='208' & ENTIDAD== '20'~ 'San Juan Mixtepec -Dto. 08 -',
      CVE_MUN=='209' & ENTIDAD== '20'~ 'San Juan Mixtepec -Dto. 26 -',
      CVE_MUN=='318' & ENTIDAD== '20'~ 'San Pedro Mixtepec -Dto. 22 -',
      CVE_MUN=='319' & ENTIDAD== '20'~ 'San Pedro Mixtepec -Dto. 26 -',
      TRUE ~ NOM_MUN)
    )%>%
    mutate(NOM_MUN=
             case_when(
               ENTIDAD==5 & NOM_MUN=="Cuatrocienegas" ~ "Cuatro Cienegas",
               
               ENTIDAD==7 & NOM_MUN=="Villacomaltitlan" ~ "Villa Comaltitlan",
               
               ENTIDAD==8 & NOM_MUN=="Doctor Belisario Dominguez" ~ "Dr. Belisario Dominguez",
               
               ENTIDAD==10 & NOM_MUN=="Simon Bolivar" ~ "General Simon Bolivar",
               
               ENTIDAD==17 & NOM_MUN=="Jonacatepec" ~ "Jonacatepec De Leandro Valle",
               
               ENTIDAD==20 & NOM_MUN=="H Villa Tezoatlan Segura Y Luna Cuna Ind Oax" ~ "Heroica Villa Tezoatlan De Segura Y Luna, Cuna De",
               ENTIDAD==20 & NOM_MUN=="Heroica Ciudad De Juchitan De Zaragoza" ~ "Juchitan De Zaragoza",
               ENTIDAD==20 & NOM_MUN=="Santiago Chazumba" ~ "Villa De Santiago Chazumba",
               
               ENTIDAD==29 & NOM_MUN=="Zitlaltepec De Trinidad Sanchez Santos" ~ "Ziltlaltepec De Trinidad Sanchez Santos",
               
               
               ENTIDAD==30 & NOM_MUN=="Cosamaloapan" ~ "Cosamaloapan De Carpio",
               ENTIDAD==30 & NOM_MUN=="Ozuluama" ~ "Ozuluama De Mascareñas",
               ENTIDAD==30 & NOM_MUN=="Zontecomatlan" ~ "Zontecomatlan De Lopez Y Fuentes",
               
               ENTIDAD==19 & NOM_MUN=="Carmen" ~ "El Carmen",
               TRUE ~ NOM_MUN
             ))%>%
    rename(MUN=CVE_MUN)
  
  # # secciones es la base que ya tiene ENTIDAD  y NOM_MUN, lo que permitirá unirla con las bases de partición
   secciones <- merge(seccion,cat_secciones_2020,by=c("ENTIDAD","MUN"),all.x = TRUE)%>%
    filter(! ( (ENTIDAD==4 & MUN==12) | (ENTIDAD==20 & MUN==317) |(ENTIDAD==20 & MUN==316) ) ) 
  
   return(secciones)
}
secciones <- obten_secciones()

limpia_nombres<-function(data){
  df <- data%>%
    mutate(NOM_MUN=str_to_title(NOM_MUN),
           NOM_MUN=chartr("áéíóú", "aeiou", NOM_MUN),
           NOM_MUN=chartr("ÁÉÍÓÚ", "AEIOU", NOM_MUN))
  return(df)
}
D0_D1.dis <- limpia_nombres(D0_D1.dis)
D0_D1.geo <- limpia_nombres(D0_D1.geo)


# * * * * * * * * * * * * * * * * * * * * * * * * 
# 1.2 Estimando el tamaño de muestra====
# * * * * * * * * * * * * * * * * * * * * * * * * 

# Los parametros que se tratarán de estimar son:

# 1. Viviendas particulares habitadas sin línea telefónica 
# fija ni teléfono celular: 
# data_tasas | secciones
# VPH_SINCINT | VPH_SINLTC

# 2. Viviendas particulares habitadas que no disponen de energía
# eléctrica, agua entubada, ni drenaje: 
# data_tasas | secciones
# VPH_NDEAED | VPH_NDEAED

# 3. Población con discapacidad 
# data_tasas | secciones
# PCON_DISC | PCON_DISC

# 4. Población de 3 años y más que habla alguna lengua indígena
# data_tasas | secciones
# P3YM_HLI | P3YM_HLI


# funciones para calcular el tamaño de la muestra 
N = dim(secciones)[1] # 68,786
k =  1.96

tamanio_muestra <- function(k,N,S_yu,d){
  numerador <- ((k * N * S_yu) / d)^2
  denominador <- 1 + ( (1/N) * ( ( (k*N*S_yu)/d ) ^2) )
  return(numerador/denominador)
}

# * * * * * * * * * * * * * * * * * * * * * * * * 
# Calculando el total verdadero de VPH_SINCINT 
# * * * * * * * * * * * * * * * * * * * * * * * * 
var <- "VPH_SINLTC"
# el valor verdadero de esta variable es 
sum(secciones[var]) # = 3,168,684
# Calculando la d, con el 5% de error
d <- round(sum(secciones[var])*0.05,0) # =  158,434
d

# Calculando el valor de n 
S_yu = var(secciones[var]) # = 4,961.793
# el tamaño de muestra para este parámetro es: 3,414.615
tm_VPH_SINCINT = tamanio_muestra(k,N,sqrt(S_yu),d)
tm_VPH_SINCINT
# * * * * * * * * * * * * * * * * * * * * * * * * 


# * * * * * * * * * * * * * * * * * * * * * * * * 
# Calculando el total verdadero de VPH_NDEAED
# * * * * * * * * * * * * * * * * * * * * * * * * 
var <- "VPH_NDEAED"
# el valor verdadero es
sum(secciones[var]) # = 79,498
# Calculando la d, con el 5% de error
d <- round(sum(secciones[var])*0.05,0)# = 3,975
d

# Calculando el valor de n 
S_yu = var(secciones[var])
# el tamaño de muestra para este parámetro es:
tm_VPH_NDEAED = tamanio_muestra(k,N,sqrt(S_yu),d)
tm_VPH_NDEAED
# * * * * * * * * * * * * * * * * * * * * * * * * 


# * * * * * * * * * * * * * * * * * * * * * * * * 
# Calculando el total verdadero de PCON_DISC
# * * * * * * * * * * * * * * * * * * * * * * * * 
var <- "PCON_DISC"
# el valor verdadero es
sum(secciones[var]) # = 6,176,477
# Calculando la d, con el 5% de error
d <- round(sum(secciones[var])*0.05,0)# = 308,824
d

# Calculando el valor de n 
S_yu = var(secciones[var])
# el tamaño de muestra para este parámetro es:
tm_PCON_DISC = tamanio_muestra(k,N,sqrt(S_yu),d)
tm_PCON_DISC
# * * * * * * * * * * * * * * * * * * * * * * * * 

# * * * * * * * * * * * * * * * * * * * * * * * * 
# Calculando el total verdadero de PCON_DISC
# * * * * * * * * * * * * * * * * * * * * * * * * 
var <- "P3YM_HLI"
# el valor verdadero es
sum(secciones[var]) # = 7,354,196
# Calculando la d, con el 5% de error
d <- round(sum(secciones[var])*0.05,0)# = 367,710
d

# Calculando el valor de n 
S_yu = var(secciones[var])
# el tamaño de muestra para este parámetro es:
tm_P3YM_HLI = tamanio_muestra(k,N,sqrt(S_yu),d)
tm_P3YM_HLI
# * * * * * * * * * * * * * * * * * * * * * * * * 


# * * * * * * * * * * * * * * * * * * * * * * * * 
# Por lo tanto, el valor de la n debe de ser: 27,109
max(tm_VPH_NDEAED, tm_VPH_SINCINT,
    tm_PCON_DISC,tm_P3YM_HLI)
# * * * * * * * * * * * * * * * * * * * * * * * * 
n <- round(max(tm_VPH_NDEAED, tm_VPH_SINCINT,
               tm_PCON_DISC,tm_P3YM_HLI),0)
n
rm(tm_VPH_NDEAED);rm(tm_VPH_SINCINT)
rm(tm_PCON_DISC);rm(tm_P3YM_HLI)
# * * * * * * * * * * * * * * * * * * * * * * * * 


# * * * * * * * * * * * * * * * * * * * * * * * * 
# 1.3 Obtención de varianzas poblacionales
# * * * * * * * * * * * * * * * * * * * * * * * * 

# Ahora, se debe de calcular la varianza poblacional para cada uno de los 4 parámetros
# en cada una de las bases para cada forma de agrupar, es decir, se tendrán 42*4 varianzas poblacionales

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# Calculo de la varianza poblacional para cada base realizada
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

variables <- c("VPH_SINLTC","VPH_NDEAED",
               "PCON_DISC","P3YM_HLI")

dimension <- 63
obten_df_varianzas_poblacionales<-function(variables,data){
  obten_varianzas_poblacionales <- function(data,variable){
    
    # Lo primero es hacer el join entre secciones y la forma de particionar 
    base <- merge(secciones%>%
                    select(c("ENTIDAD","NOM_MUN","SECCION",variable)),
                  data%>%
                    select(-c(MUN,NOM_ENT)), by=c("ENTIDAD","NOM_MUN"), all.x=TRUE)
    
    # Las primeras columnas estarán ordenadas de esta forma:
    #   ENTIDAD | NOM_MUN | SECCION | Variable 
    # se debe de hacer la agrupación a partir de la columnas 5
    
    obten_VP <- function(base,variable,i){
      base_temp <- base[,c(1:4,i)]
      names(base_temp)[5]<-"Cluster"
      #Asignación del tamaño de muestra en cada estrato
      #Proporcional a Nh, es decir nh ~ n * Nh/N 
      varianza_poblacional <-
        base_temp%>%
        group_by(Cluster)%>%
        summarise(Nh=n(), # total de población por cada grupo
                  nh=round(n*Nh/N,0), # se calcula la n de cada h
                  Syh2=var(eval(parse(text=variable))),  # varianza del conjunto h
                  Vh=(Nh^2/nh)*(1-nh/Nh)*Syh2 ) #calculo de la varianza
      
      
      VP <- sum(varianza_poblacional$Vh)/sum(varianza_poblacional$Nh)^2
      VP
      return(VP)
    }
    
    
    varianzas_poblacionales <- data.frame()
    
    for(i in 5:dim(base)[2]){
      varianzas_poblacionales <- rbind(varianzas_poblacionales,
                                       data.frame(agrupacion = names(base)[i],
                                                  varianza_poblacional = obten_VP(base,variable,i))
      )
    }
    names(varianzas_poblacionales)[2]<-paste0("vp_",variable)
    return(varianzas_poblacionales)
  }
  df_varianzas_poblacionales <- data.frame(matrix(NA, nrow = dimension, ncol = 0))
  
  for(variable in variables){
    
    df_varianzas_poblacionales <- cbind(df_varianzas_poblacionales,
                                        obten_varianzas_poblacionales(data,variable)[,2] )
  }
  names(df_varianzas_poblacionales) <- variables
  df_varianzas_poblacionales
  
  
  nombres_agrupaciones<-data.frame()
  for(i in 5:dim(data)[2]){
    nombres_agrupaciones <- rbind(nombres_agrupaciones,
                                  data.frame(agrupacion = names(data)[i])
    )
  }
  nombres_agrupaciones
  
  df_varianzas_poblacionales <- cbind(nombres_agrupaciones,df_varianzas_poblacionales)
  return(df_varianzas_poblacionales)
}

vp_D0_D1.dis <- obten_df_varianzas_poblacionales(variables,D0_D1.dis)
vp_D0_D1.geo <- obten_df_varianzas_poblacionales(variables,D0_D1.geo)

vp_D0_D1.dis$data_frame <- "vp_D0_D1.dis"
vp_D0_D1.geo$data_frame <- "vp_D0_D1.geo"




# se une toda la información en un único data frame 
data <- rbind(vp_D0_D1.dis,
              vp_D0_D1.geo)

# * * * * * * * * * * * * * * * * * * * * * * * * 
# 1.4 Elección de la partición 
# * * * * * * * * * * * * * * * * * * * * * * * * 
variables
# con esto se ve el minimo para cada variable y a que agrupación
# y base de datos pertenece, vemos que hay más variables en vp_D0_D1.geo
for(i in variables){
 print(data%>%
         filter(eval(parse(text=i))==min(data[i])))
}


df_eleccion<-data.frame()

# con se rankea para cara variable, quién tiene la menor varianza poblacional
for(i in variables){
  df_eleccion<-rbind(df_eleccion, 
                     data%>%
                       select(agrupacion,data_frame,i)%>%
                       mutate(rank = dense_rank(eval(parse(text=i))))%>%
                       select(agrupacion,data_frame,rank)%>%
                       arrange(rank))
  
}

# Se obtiene la varianza con un muestreo aleatorio simple

df_VarMAS <- data.frame()
for(i in variables){
  df_VarMAS <- rbind(df_VarMAS, data.frame(variable = i,
                                           VarMAS = as.numeric((1/n)*(1-(n/N))*var(secciones[i])) ) )
}


# el que tenga menos puntos es el ganador
df_eleccion%>%
  group_by(agrupacion,data_frame)%>%
  summarise(puntos=sum(rank))%>%
  arrange(puntos)




data%>%
  filter(agrupacion=="alfa 0.25, K 5",
         data_frame=="vp_D0_D1.geo")



data%>%
  select(data_frame, agrupacion,VPH_SINLTC, VPH_NDEAED, PCON_DISC,  P3YM_HLI)%>%
  filter((agrupacion == "alfa 0.25, K 5") & (data_frame=="vp_D0_D1.geo"))

# para la variable VPH_SINLTC
i <- "VPH_SINLTC"
data%>%
  select(agrupacion,data_frame,i)%>%
  mutate(rank = dense_rank(eval(parse(text=i))))%>%
  select(agrupacion,data_frame,rank)%>%
  arrange(rank)%>%
  filter((agrupacion == "alfa 0.25, K 5") & (data_frame=="vp_D0_D1.geo"))

# para la variable PCON_DISC
i <- "PCON_DISC"
data%>%
  select(agrupacion,data_frame,i)%>%
  mutate(rank = dense_rank(eval(parse(text=i))))%>%
  select(agrupacion,data_frame,rank)%>%
  arrange(rank)%>%
  filter((agrupacion == "alfa 0.25, K 5") & (data_frame=="vp_D0_D1.geo"))

# para la variable VPH_NDEAED
i <- "VPH_NDEAED"
data%>%
  select(agrupacion,data_frame,i)%>%
  mutate(rank = dense_rank(eval(parse(text=i))))%>%
  select(agrupacion,data_frame,rank)%>%
  arrange(rank)%>%
  filter((agrupacion == "alfa 0.25, K 5") & (data_frame=="vp_D0_D1.geo"))

# para la variable P3YM_HLI
i <- "P3YM_HLI"
data%>%
  select(agrupacion,data_frame,i)%>%
  mutate(rank = dense_rank(eval(parse(text=i))))%>%
  select(agrupacion,data_frame,rank)%>%
  arrange(rank)%>%
  filter((agrupacion == "alfa 0.25, K 5") & (data_frame=="vp_D0_D1.geo"))

a <- "alfa 0.25, K 5"
df <- "vp_D0_D1.geo"


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# Con esto evaluamos qué tanto mejoró 
h <- 2
merge(
  data%>%
    filter(agrupacion==a,
           data_frame==df)%>%
    pivot_longer(!c(agrupacion,data_frame), 
                 names_to="variable", values_to="varianza")%>%
    select(variable,varianza),
  df_VarMAS,
  by="variable"
)%>%
  mutate(mejoro=varianza<VarMAS)%>%
  # mutate(radio = varianza/VarMAS)
  mutate(reduccion=100*((VarMAS-varianza)/VarMAS))%>%
  mutate(varianza=round(varianza,h),
         VarMAS=round(VarMAS,h),
         reduccion=round(reduccion,h))


