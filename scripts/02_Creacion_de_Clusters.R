input <- ("G:/Mi unidad/Tesis/")
pacman::p_load(NbClust,ClustGeo,ggplot2,readxl,purrr,
               cluster,factoextra,tidyr,dplyr,scales, ape)
# library(showtext)
# font_add_google(c("Poppins"))
# showtext_auto()
rm(list=setdiff(ls(),"input"))

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# 4  Obtener la clasificación socioeconomica ======
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
setwd(input)
load("00_Carga_de_datos_demograficos.Rdata")
load("01_Integracion_Imputacion_y_Limpieza_de_datos.Rdata")

output <- "G:/Mi unidad/Tesis/imagenes"
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# 4.1  Se seleccionan las variables======
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
obten_data <- function(var){
  variables_inegi <- read_excel("Comparacion_Variables.xlsx",
                                sheet = "Variables")
  
  var_conservadas_inegi <- variables_inegi%>%
    select(Variables, var)%>%
    drop_na()
  
  
  # necesitamos todas las variables del coneval, excepto poblacion 
  variables_coneval <- names(coneval)[names(coneval) != "poblacion"]
  variables_inegi <- var_conservadas_inegi$Variables
  
  # ahora se conservan únicamente esas variables, menos el nombre de 
  # la entidad y del municipio
  data <- data_tasas%>%
    select(c(variables_coneval,variables_inegi))
  
  return(data)
  
}

# elegimos la combinación de variables que queremos
# (se usa el excel y se marcan con X)
# al final se usaron la de "Muchas" y "Medio"
# datos_op1<-obten_data("Medio")
sociodemo<-obten_data("Nuevo")

# Se quitan algunas variables del coneval que ya estan siendo calculadas
# con inegi 
names(sociodemo)

datos_sociodemo <- sociodemo%>%
  select(-c(vul_car_pob,ic_rezedu_pob,ic_asalud_pob,ic_cv_pob,
            ic_sbv_pob))
rm(sociodemo)
names(datos_sociodemo)
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# 4.2  Se busca una K adecuada ======
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# 
# # - - - - - - - - - Con los dendogramas
# 
# # se calculan las distancias de la base
D0<- dist(datos_sociodemo%>%
                select(- NOM_ENT,- NOM_MUN),
              method="euclidean")

# gráfica del dendrograma
o <- 5
tree <- hclustgeo(D0)
clus4 = cutree(tree, 4)
colores <- c("yellow","green","pink","red")
plot(as.phylo(tree), cex = 0.6,
     edge.color = "black", edge.width = 1.5, 
     tip.color = "black")
rect.hclust(tree, k=o, border=rainbow(o))
setwd(output)

# # - - - - - - - - - Con el método del codo
# elbow <- fviz_nbclust(x = datos_sociodemo%>%
#                           select(- NOM_ENT,- NOM_MUN),
#                         FUNcluster = hcut, linecolor="#91B83F",
#                     method = "wss", k.max = 10) 
# 
# ggplot(data=elbow$data, aes(x=clusters, y=y, group=1))+
#   labs(title = "Número óptimo de clusters con el método del codo",
#        x="Número de clusters (K)", y="WSS")+
#   geom_point(size=3, color="#7CB728")+
#   geom_line(size=1.5, color="#7CB728")+
#   theme_bw()+
#   theme(text = element_text(family="Poppins"),
#         plot.title = element_text(size=20,hjust = 0.5),
#         axis.title = element_text(size=19),
#         axis.text = element_text(size=18))+
#   scale_y_continuous(label=comma,  n.breaks=8)
#   
# 
# setwd(output)
# ggsave(filename = "codo.png",
#        device = "png",width = 8,height = 5, dpi=100)   

# # - - - - - - - - - Con el método silhouette
# silhouette <- fviz_nbclust(x = datos_sociodemo%>%
#                             select(- NOM_ENT,- NOM_MUN),
#                           FUNcluster = hcut, linecolor="#3FB8AF",
#                           method = "silhouette", k.max = 10) 
# 
# ggplot(data=silhouette$data, aes(x=clusters, y=y, group=1))+
#   labs(title = "Número óptimo de clusters con el método de la silueta",
#        x="Número de clusters (K)", y="Promedio del ancho de la silueta")+
#   geom_point(size=3, color="#2A77A4")+
#   geom_line(size=1.5, color="#2A77A4")+
#   theme_bw()+
#   theme(text = element_text(family="Poppins"),
#         plot.title = element_text(size=20,hjust = 0.5),
#         axis.title = element_text(size=19),
#         axis.text = element_text(size=18))+
#   scale_y_continuous(label=comma,  n.breaks=8, limits = c(0,0.4))
# setwd(output)
# ggsave(filename = "silueta.png",
#        device = "png",width = 9,height = 5, dpi=100) 
# # - - - - - - - - - Con el método gap
# gap <- fviz_nbclust(x = datos_sociodemo%>%
#                           select(- NOM_ENT,- NOM_MUN ),
#                         FUNcluster = hcut, linecolor="#F08080",
#                         method = "gap_stat", k.max = 10) 
# 
# 
# ggplot(data=gap$data, aes(x=clusters, y=gap, group=1))+
#   labs(title = "Número óptimo de clusters con el método gap",
#        x="Número de clusters (K)", y="Estadístics Gap (K)")+
#   geom_point(size=3, color="#FFAE4D")+
#   geom_line(size=1.5, color="#FFAE4D")+
#   theme_bw()+
#   theme(text = element_text(family="Poppins"),
#         plot.title = element_text(size=20,hjust = 0.5),
#         axis.title = element_text(size=19),
#         axis.text = element_text(size=18))+
#   scale_y_continuous(label=comma,  n.breaks=8)
# 
# setwd(output)
# ggsave(filename = "gap.png",
#        device = "png",width = 8,height = 5, dpi=100) 

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# 4.3  Creación de clusters ======
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

# se puede usar la función de choicealpha, pero en este caso
# elegiremos usar una malla de distintas alfas 

# datos_sociodemo D1.dis, D1.geo
# Nos dan 3 combinaciones posibles


malla <- seq(0,1,by=0.05)
malla_alfas_ks <- function(d0,d1,Ks){
  data_aux <- data.frame(matrix(nrow = dim(data_tasas)[1]))
  alphas <- malla
  data <- data_tasas%>%
    select(NOM_ENT,NOM_MUN,ENTIDAD,MUN)
  
  for(K in Ks){
    for(i in 1:length(alphas)){
      a <- alphas[i]
      tree <- hclustgeo(d0, d1, alpha=a)
      clusters <- data.frame(cutree(tree, K))
      name <- paste0("alfa ",a,", K ",K)
      names(clusters) <- name
      data <- cbind(data,clusters)
    }
  }
  
  

  return(data)
}

# length(seq(0,1,by=0.05))
# por cada combinación entre los D0 y D1 posibles:
# Vamos a tener 603 nuevas variables
# 3(ks)*21(alphas)
# es decir, 63*3 segmentaciones posibles 

# D0_op1, D0_op2, D1.dis, D1.geo
D0_D1.dis <- malla_alfas_ks(D0,D1.dis,Ks=c(3,4,5))
D0_D1.geo <- malla_alfas_ks(D0,D1.geo,Ks=c(3,4,5))


# se guardan los datos que nos interesan
getwd()
rm(list=setdiff(ls(), c("D0_D1.dis",
                        "D0_D1.geo",
                        "data_tasas",
                        "inegi","coneval",
                        "inegi_y_coneval",
                        "datos_sociodemo",
                        "claves_ENT_MUN")))
setwd("G:/Mi unidad/Tesis")
save.image("02_Creacion_de_Clusters.Rdata")

# Esto se usa para el análisis descriptivo de la base de datos 
library(reshape2)
library(showtext)
font_add_google(c("Special Elite"))
showtext_auto()
heat <- melt( datos_sociodemo%>%
                mutate(id=paste0(NOM_ENT,"_",NOM_MUN))%>%
                select(-c(NOM_ENT,NOM_MUN) ) )
head(heat)



# se preparan los datos 
df<-datos_sociodemo%>%
  mutate(id=paste0(NOM_ENT,"_",NOM_MUN))%>%
  select(-c(NOM_ENT,NOM_MUN) )
row.names(df)<-df$id
cormat <- round(cor(df%>%select(-id)),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  # scale_fill_gradient(low = "yellow", high = "red", na.value = NA)+
  scale_fill_gradient2(low = "#004779", high = "#A50021",mid = "white",
                       na.value = NA,guide = "colourbar",
                       aesthetics = "fill")+
  labs(title = "Correlación entre variables de D0 con el método de pearson")+
  theme_bw()+
  theme(text = element_text(family="Poppins"),
        plot.title = element_text(size=20,hjust = 0.5),
        axis.title = element_blank(),
        axis.text.x = element_text(size=15,
                                   family="Special Elite", 
                                   angle=90, hjust = 1),
        axis.text.y = element_text(size=15,
                                   family="Special Elite"),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

setwd(output)
ggsave(filename = "correlaciones.png",
       device = "png",width = 10,height = 10 , dpi=100) 



corr <- melted_cormat%>%
  mutate(iguales=case_when(Var1==Var2 ~ T, 
                           TRUE ~ F))%>%
  filter(iguales==F)%>%
  arrange(desc(value))
  arrange((value))

  
  z <- corr%>%
    filter(Var1=="PSINDER")
  
  
# ¿cuál es el municipio más pobre y con mayores carencias?
  # ¿cuál es el municipio con más ricos?
# ¿cuáles son los estados donde se habla más indigena?
#     con mayores jefas del hogar?
  
  
x <- datos_sociodemo%>%
  select(NOM_ENT, NOM_MUN, HOGJEF_F)
  arrange(desc(HOGJEF_F))
  
  
  datos_sociodemo%>%
    select(NOM_ENT, NOM_MUN, HOGJEF_F)

  
total_municipios <- datos_sociodemo%>%
    group_by(NOM_ENT)%>%
    summarise(total_mun=n())
  


datos_sociodemo%>%
  left_join(total_municipios)%>%
  group_by(NOM_ENT,total_mun)%>%
  summarise(prom=mean(PSINDER))%>%
  arrange(desc(prom))


datos_sociodemo%>%
  left_join(total_municipios)%>%
  group_by(NOM_ENT,total_mun)%>%
  summarise(prom=mean(P3YM_HLI))%>%
  arrange(desc(prom))

x <- datos_sociodemo%>%
  group_by(NOM_ENT,NOM_MUN)%>%
  select(P3YM_HLI)%>%
  arrange(desc(P3YM_HLI))

x[1:30,]%>%
  group_by(NOM_ENT)%>%
  summarise(total=n())


datos_sociodemo%>%
  group_by(NOM_ENT,NOM_MUN)%>%
  select(npnv_pob)%>%
  arrange(desc(npnv_pob))

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# descripción de los resultados 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
D0_D1.geo
datos_sociodemo

datos_sociodemo
names(D0_D1.geo)
final <- D0_D1.geo[c("alfa 0.25, K 5","NOM_ENT", "NOM_MUN")]

names(datos_sociodemo)


union <- merge(final, datos_sociodemo, by=c("NOM_ENT", "NOM_MUN"))%>%
  rename(grupos="alfa 0.25, K 5")%>%
  mutate(grupos=as.character(grupos))%>%
  mutate(grupos=factor(grupos,levels=c("1","2","3","4","5")))

# Se hace el analisis de pca 
library(ggfortify)
df <- union%>%
  select(-c(NOM_ENT,NOM_MUN,grupos))
pca.obj <- prcomp(df)
autoplot(pca.obj) + theme_minimal() 


colores <- c("#8AB833","#F9D23C","#D55816","#4EB3CF","#E32D91")
dtp <- data.frame('Grupo' = union$grupos, pca.obj$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
  geom_point(aes(x = PC1, y = PC2, col = Grupo)) + 
  theme_minimal()+
  scale_color_manual(values=colores)+
  labs(x="PC1(64.13%)",y="PCw(%)")
  
