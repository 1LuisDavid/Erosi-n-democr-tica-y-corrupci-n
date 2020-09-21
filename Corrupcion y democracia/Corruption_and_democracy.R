####### Corrupción world bank ------


Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica


library(tidyverse)
library(janitor)
library(ggthemes)
library(readr)



dem <- read_csv("V-Dem-CY-Core-v10.csv")
dem <- clean_names(dem)


dem2019 <-
  dem %>% 
  filter(year == 2019)

dem2019 <-
dem2019 %>% select(1:29, "v2x_corr")


dem2019 <-
dem2019 %>% select(1,2,4,26,30)

demmx <-
dem2019 %>% filter(country_text_id == "MEX")
names(dem2019)



  ggplot(dem2019, aes(x = v2x_corr, y =v2x_libdem, label = country_text_id)) +
  geom_text(size = 5, check_overlap = TRUE) +
  geom_smooth(method = lm)+
  geom_text(data = demmx, aes(x = v2x_corr, y =v2x_libdem, label = country_text_id),
            color = "red", size = 5)+ 
    theme_fivethirtyeight()+
  
    theme(plot.title = element_text(face = "bold", size = 22),
         plot.subtitle = element_text(color = "black", size = 14),
         plot.caption = element_text(face = "italic", hjust = 1, size = 16),
         axis.text.x = element_text(size = 16),
         axis.text.y = element_text(size = 16),
         axis.title.x = element_text(inherit.blank = F, size = 18),
         axis.title.y = element_text(inherit.blank = F, size = 18))+
    
    labs(x = "Índice de corrupción ", y = "Índice de democracia liberal",
         title = "Gráfica 1\nAsociación entre corrupción y democracia en 2019", 
         caption = "Elaboración propia con datos de V-Dem")+
    
    ggsave("G1 Corrupción y democracia.png", width = 12, height = 8, dpi = 400)
    
    
  reg1 <- lm(v2x_libdem ~ v2x_corr, data = dem2019)
  summary(reg1)

  dem2019$v2x_libdem
  
  cor.test(dem2019$v2x_libdem, dem2019$v2x_corr)

###### Gráfica SEA Naty -----
  
  
  Estados <- c("Jalisco","Aguascalientes", "Estado de México", "Quintana Roo", "Sonora",
               "Guanajuato", "Coahuila", "Puebla", "Tabasco", "Veracruz", "Sinaloa", 
               "Baja California", "Zacatecas", "Nayarit", "Oaxaca", "Durango", "Querétaro")
  
Puntaje <- c(.762,.7,.6965,.677,.623,.5955,.553,.551,.5395,.539,.5215,.512,.5,.404,.402,
             .3945,.348)

nat <- cbind.data.frame(Estados, Puntaje)

nat<-
nat %>% mutate(Nivel_coordinación = ifelse(Puntaje >= .66, "Coordinación simbiótica",
                                           ifelse( Puntaje <= .33, "Coordinación formal",
                                                   "Coordinación interactiva")))

# g3 <-
nat %>% ggplot(aes(x = reorder(Estados, Puntaje) , y = Puntaje,
                   fill = Nivel_coordinación, label = Puntaje))+
  geom_col()+ 
  coord_flip()+
  geom_text(color = "white", hjust = 1.1)+
  scale_fill_manual(values = c("grey30", "black"))+
                     
  theme_fivethirtyeight()+
  
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "black"),
        plot.caption = element_text(face = "italic", hjust = 1, size = 11),
        # axis.text.x = element_text(size = 16),
        # axis.text.y = element_text(size = 16),
        axis.title.x = element_text(inherit.blank = F),
        axis.title.y = element_text(inherit.blank = F),
        legend.position = c(0.9, 0.1), legend.direction = "vertical")+
  
  labs(x = " ", y = "Puntaje en el Nivel de coordinación",
       title = "Gráfica 3\nNiveles de coordinación interorganizacional en los SEA",
       caption = "Elaboración propia con datos de Campos (2020)",
       fill = "Tipo de coordinación")+
  
  ggsave("G3 Niveles de coordinación en los SEA.png", width = 9, height = 6, dpi = 400)
  
  
  

  
    
  
  
  
