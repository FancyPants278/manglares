library(tidyverse)
library(lubridate)

source("01_data_wrangling.R")

# plotting ----------------------------------------------------------------

# setting default theme
theme_set(theme_linedraw())


# NDVI --------------------------------------------------------------------


(p1 <- ggplot(ndvi, aes(x=date, y=var, col=region))+
         geom_line(cex=.4)+
         labs(x="", y="NDVI")+
         geom_hline(aes(yintercept=0), col = "black")+
         geom_smooth(col="red")+
         facet_wrap(~region)+
         scale_color_brewer(palette = 2, aesthetics="black")+
         theme(legend.position = "", 
               panel.grid = element_blank()))

(p2 <- ndvi %>% 
                mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
                group_by(region, month) %>% 
                mutate(average = mean(var, na.rm = T), anomaly = var-average) %>% 
                ggplot(aes(x=date, y=anomaly)) +
                geom_col(aes(fill= anomaly>0))+
                geom_hline(yintercept = 0)+
                ylim(-1,.9)+
                scale_fill_manual(labels = c("FALSE" = "Lower than avg", "TRUE"="Higher than avg"), 
                                  values = c("blue", "red"))+
                labs(x ="", y = "NDVI anomaly")+
                facet_wrap(~region)+
                theme_linedraw()+
                theme(legend.title = element_blank(), 
                      panel.grid = element_blank()))

# NDWI --------------------------------------------------------------------


(p1 <- ggplot(ndwi, aes(x=date, y=var, col=region))+
         geom_line(cex=.4)+
         labs(x="", y="NDWI")+
         geom_hline(aes(yintercept=0), col = "black")+
         geom_smooth(col="red")+
         facet_wrap(~region)+
         scale_color_brewer(palette = 2, aesthetics="black")+
         theme(legend.position = "", 
               panel.grid = element_blank()))

(p2 <- ndwi %>% 
                mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
                group_by(region, month) %>% 
                mutate(average = mean(var, na.rm = T), anomaly = var-average) %>% 
                ggplot(aes(x=date, y=anomaly)) +
                geom_col(aes(fill= anomaly>0))+
                geom_hline(yintercept = 0)+
                ylim(-1,.9)+
                scale_fill_manual(labels = c("FALSE" = "Lower than avg", "TRUE"="Higher than avg"), 
                                  values = c("blue", "red"))+
                labs(x ="", y = "NDWI anomaly")+
                facet_wrap(~region)+
                theme_linedraw()+
                theme(legend.title = element_blank(), 
                      panel.grid = element_blank()))


# LSWI --------------------------------------------------------------------


(p1 <- ggplot(lswi, aes(x=date, y=var, col=region))+
         geom_line(cex=.4)+
         labs(x="", y="LSWI")+
         geom_hline(aes(yintercept=0), col = "black")+
         geom_smooth(col="red")+
         facet_wrap(~region)+
         scale_color_brewer(palette = 2, aesthetics="black")+
         theme(legend.position = "", 
               panel.grid = element_blank()))

(p2 <- lswi %>% 
                mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
                group_by(region, month) %>% 
                mutate(average = mean(var, na.rm = T), anomaly = var-average) %>% 
                ggplot(aes(x=date, y=anomaly)) +
                geom_col(aes(fill= anomaly>0))+
                geom_hline(yintercept = 0)+
                ylim(-1,.9)+
                scale_fill_manual(labels = c("FALSE" = "Lower than avg", "TRUE"="Higher than avg"), 
                                  values = c("blue", "red"))+
                labs(x ="", y = "LSWI anomaly")+
                facet_wrap(~region)+
                theme_linedraw()+
                theme(legend.title = element_blank(), 
                      panel.grid = element_blank()))


# rain --------------------------------------------------------------------


(p1 <- ggplot(rain, aes(x=date, y=var, col=region))+
         geom_line(cex=.4)+
         labs(x="", y="Precipitation (mm)")+
         geom_hline(aes(yintercept=0), col = "black")+
         geom_smooth(col="red")+
         facet_wrap(~region)+
         scale_color_brewer(palette = 2, aesthetics="black")+
         theme(legend.position = "", 
               panel.grid = element_blank()))

(p2 <- rain %>% 
                mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
                group_by(region, month) %>% 
                mutate(average = mean(var, na.rm = T), anomaly = var-average) %>% 
                ggplot(aes(x=date, y=anomaly)) +
                geom_col(aes(fill= anomaly>0))+
                geom_hline(yintercept = 0)+
                ylim(-1,.9)+
                scale_fill_manual(labels = c("FALSE" = "Lower than avg", "TRUE"="Higher than avg"), 
                                  values = c("blue", "red"))+
                labs(x ="", y = "Precipitation anomaly (mm)")+
                facet_wrap(~region)+
                theme_linedraw()+
                theme(legend.title = element_blank(), 
                      panel.grid = element_blank()))




# Temperature -------------------------------------------------------------


(p1 <- ggplot(temp, aes(x=date, y=var, col=region))+
         geom_line(cex=.4)+
         labs(x="", y="Temperature (°C)")+
         ylim(5,60)+
         geom_hline(aes(yintercept=32.5), col = "black")+
         geom_smooth(col="red")+
         facet_wrap(~region)+
         scale_color_brewer(palette = 2, aesthetics="black")+
         theme_linedraw()+
         theme(legend.position = "", 
               panel.grid = element_blank()))



## CUIDADO ESTO VA A SER LENTO! p.s. estas anomalias se ven raras...
(p2 <- temp %>% 
                mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
                group_by(region, month) %>% 
                mutate(average = mean(var, na.rm = TRUE), anomaly = var-average) %>% 
                ggplot(aes(x=date, y=anomaly)) +
                geom_col(aes(fill= anomaly>0))+
                scale_fill_manual(labels = c("FALSE" = "Colder than avg", "TRUE"="Warmer than avg"), 
                                  values = c("blue", "red"))+
                labs(x ="", y = "Temperature anomaly (°C)")+
                facet_wrap(~region)+
                theme_linedraw()+
                theme(legend.title = element_blank(), 
                      panel.grid = element_blank()))



## CUIDADO!! ESTO ABAJO VA A SER MUY LENTO!!! si quieres intentar quita el comento #
p1/p2



