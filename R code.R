#Instalar paquetes
install.packages("tidyverse")

#Cargar paquetes
library(tidyverse) # Observar que incluye paqueterías como: ggplot2, readr, y dplyr.

#Fijar directorio de trabajo (depende de cada quien)
setwd("~/Desktop/Marcela/Cursos/Semujeres")

#Importar archivos
## Base 1: Registros de nacimientos 2019
datos_sinac<-read.csv("sinac2019DatosAbiertos.csv") # Línea de código básico para importar el .csv de SINAC
View(datos_sinac) # Línea de código que permite visualizar la base de datos en otra ventana

## Base 2: Catálogo de variables general
catvar_sinac<-read.csv("./sinac_catalogos_2015-2019/CatVariables.csv") #Con este línea se accede a un .csv de un subfolder, recordar el punto (.)
View(catvar_sinac)

## Base 3: Catálogo de municipios
catmpo_sinac<-read.csv("./sinac_catalogos_2015-2019/CATMPO.csv")
View(catvar_sinac)

# Base 1
#Para este sript, se trabajará con las variables ENT_RES, MPO_RES, EDADM, EDOCIVIL, COND_INDM (dicrepancia entre catálogo y base)
#Filtrar base por entidad de residencia de la madre: Yucatán (nomenclatura 31)
datos_sinac_yuc<- datos_sinac %>%
  filter(ENT_RES==31) %>%
  select(ENT_RES, MPO_RES, EDADM, EDOCIVIL, CON_INDM) #Se podría quitar ENT_RES de la selección, una vez que fue filtrado
View(datos_sinac_yuc)

# Base 3
#Filtrar base por entidad: Yucatán (nomenclatura 31)
catmpo_sinac_yuc<- catmpo_sinac %>%
  filter(EDO==31) %>%
  select(MPO, DESCRIP) %>%
  rename(MPO_RES=MPO, Municipio=DESCRIP)

#Unir bases 1 y 3
datos_sinac_yuc_descrip<- left_join(datos_sinac_yuc, catmpo_sinac_yuc, by="MPO_RES")
View(datos_sinac_yuc_descrip)

#Ejercicio para agregar descripción del catálogo de estado civil
# 1. Cargar la base de estado civil
# 2. Renombrar la base y generar ID que permitan unir la base de estado civil con la de datos_sinac_yuc_descrip
# 3. Unir bases :)

#Filtrar por edad y contar
sinac_yuc_edad<- datos_sinac_yuc_descrip %>%
  filter(EDADM<15) %>%
  count(Municipio) %>% #Se realiza un conteo por municipio
  rename(Nacimientos=n) %>% #La línea anterior generar una columna de conteo que se registra por default como n, esta línea renombra n
  arrange(desc(Nacimientos))
View(sinac_yuc_edad)

#Visualización de Lollipop horizontal
sinac_yuc_edad %>%
  arrange(Nacimientos) %>% #se agregan las dos líneas de código, para ordenar de mayor a menor
  mutate(Municipio=factor(Municipio,Municipio)) %>%
  ggplot(aes(x=Municipio, y=Nacimientos)) +
  geom_segment( aes(x=Municipio, xend=Municipio, y=0, yend=Nacimientos), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  labs(title = "Nacimientos registrados en niñas de 10 a 14 años",
       subtitle = "Yucatán, 2019",
       caption = "Dirección General de Información en Salud") +
  coord_flip() +
  theme(
    plot.title=element_text(size=30, 
                            face="bold",
                            hjust=0.5,
                            lineheight=1.2),
    plot.subtitle=element_text(size=20,
                               hjust=0.5),
    plot.caption = element_text(size=15),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


#Guardar archivo (depende el archivo que se deseé guardar)
write.csv(datos_sinac_yuc_descrip, file = "sinac_yuc.csv")
write.csv(sinac_yuc_edad, file = "sinac_yuc_edad.csv")


