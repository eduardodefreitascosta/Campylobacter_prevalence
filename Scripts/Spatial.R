
#################
##Load libraries#
#################

# Package names
packages <- c("ggplot2", "readxl","here", "tidyverse","sp","sf","raster","dplyr","spData","tmap",
              "leaflet","maptools","mapproj","mapview","ggspatial","extrafont")


# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

######################
#Import and tidy data#
######################

#Reading database
Database=read.csv(here("Outputs","Campy2.csv"),sep=";")


#Reading farms
points=read_excel(here("Data","Spatial","campylo_final.xlsx"))
points2<-points%>%
  group_by(farm_ID,nome)%>%
  summarise(N=n())


##############################
#Spatial descriptive analysis#
##############################

# Read in the shapefile
states <- sf::st_read(here("Data","Spatial","UFEBRASIL.shp"))
latin_america<-sf::st_read(here("Data","Spatial","South_America.shp"))
lakes<-sf::st_read(here("Data","Spatial","RS_2001_2002_MASSAS_DAGUA.shp"))
mesors <-sf::st_read(here("Data","Spatial","BR_Mesorregioes_2020.shp"))
mesors<-mesors%>%
  filter(SIGLA_UF %in% c("RS"))

mapars <-sf::st_read(here("Data","Spatial","RS", "43MUE250GC_SIR.shp"))

#Reading municipalities lat and long
municipios=read.csv(file=here("Data","Spatial","municipios.csv"),sep=";",
                    encoding='UTF-8'
                    #Other settings
)

codigo_UF=read_excel(here("Data","Spatial","codigo_UF.xlsx"))

municipios2=left_join(municipios,codigo_UF,by=c("codigo_uf"))


#Join famrs and geometries

points3<-left_join(points2,municipios2,by="nome")



# Filtering southern states
states2<-states%>%
  filter(NM_ESTADO %in%c("RIO GRANDE DO SUL", "SANTA CATARINA") )

#Filtering southern countries
latin_america<-latin_america%>%
  filter(COUNTRY %in% c("Uruguay","Argentina","Paraguay"))


# Manipuating meso names
names(Database)[3]<-"NM_MESO"

Database2<-Database%>%
  group_by(NM_MESO)%>%
  summarise(N=n())

mesors2<-left_join(mesors,Database2,by="NM_MESO")

mesors2$label<-paste0(c("NW ","NE ",
                        "COc ", "COr ", 
                        "PoA ",
                        "SW ","SE " ),"(",round(mesors2$N/99,4)*100,"%",")")


mesors2<- mesors2[order(mesors2$N),]

#Creating labels

  latitude=c(-27.1,-32,-28.5,-27,-32)
  longitude=c(-51,-56,-57,-56.5,-49.5)
  label=c("Santa Catarina \nstate","Uruguay","Argentina","Paraguay","Atlantic ocean")

labels2<-cbind.data.frame(latitude,longitude,label)

# main
main=ggplot()+
  geom_sf(data=states2, fill = "antiquewhite3",alpha=1)+
  geom_sf(data=latin_america,fill = "antiquewhite",alpha=1)+
  geom_text(data=labels2,
            aes(label=label,x = longitude, y = latitude),size=3, colour = "black",alpha=0.5
            )+
  geom_point(data=points3,
                 aes(x=longitude+rnorm(99,0,0.05), y=latitude+rnorm(99,0,0.005)),size = 1,shape=16, 
                 color="black"
             )+
  geom_sf(data=mesors2,aes(fill=factor(N)),alpha=0.5)+
  scale_fill_brewer(palette = "Dark2",name="Region (% out of 99 farms)",labels=mesors2$label)+
  theme_classic()+
  theme(legend.text = element_text(size=10),
        legend.position = "right",
        text = element_text(size = 13, family = "Times New Roman"), 
        axis.text = element_text(size = 11, family = "Times New Roman"),        
        panel.background = element_rect(fill="lightblue1")
        )+
  geom_sf(data=lakes,fill = "lightblue1")+
  coord_sf(xlim = c(-57.5, -48.75), ylim = c(-34, -27.2), expand = T)+
  xlab("Longitude")+
  ylab("Latitude")+
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.15, "in"),
                         style = north_arrow_fancy_orienteering)
  

ggsave(here("Figures","Fig1.tiff"),plot=main, width = 10, height = 10, device='tiff', dpi=600)
