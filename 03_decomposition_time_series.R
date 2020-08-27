library(tidyverse)
library(lubridate)
library(ggthemes)
library(zoo)

source("00_load_data.R")

# NDVI ------

p1 <- ggplot(ndvi, aes(x=date, y=ndvi, col=region))+
        geom_line(cex=1.2)+
        labs(x="", y="NDVI")+
        ylim(-1, 1)+
        geom_smooth(col="black")+
        geom_hline(aes(yintercept=0), col = "black")+
        facet_wrap(~region)+
        scale_color_brewer(palette = 2, aesthetics="black")+
        theme_linedraw()+
        theme(legend.title = element_blank(), 
              panel.grid = element_blank())
p1


p2 <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        mutate(average = mean(ndvi), anomaly = ndvi-average) %>% 
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
              panel.grid = element_blank())
p2

cowplot::plot_grid(p1,p2, labels="AUTO", ncol=1)
ggsave("figs/ndvi_variation.png", dpi = 300, width = 10, height = 10)


# NDWI -----
p1 <- ggplot(ndwi, aes(x=date, y=ndwi, col=region))+
        geom_line(cex=1.2)+
        labs(x="", y="NDWI")+
        ylim(-1, 1)+
        geom_hline(aes(yintercept=0), col = "black")+
        facet_wrap(~region)+
        scale_color_brewer(palette = 2, aesthetics="black")+
        theme_linedraw()+
        theme(legend.title = element_blank())
p1


p2 <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        mutate(average = mean(ndwi), anomaly = ndwi-average) %>% 
        ggplot(aes(x=date, y=anomaly)) +
        geom_col(aes(fill= anomaly>0))+
        scale_fill_manual(labels = c("FALSE" = "Lower than avg", "TRUE"="Higher than avg"), 
                          values = c("blue", "red"))+
        labs(x ="", y = "NDWI anomaly")+
        facet_wrap(~region)+
        theme_linedraw()+
        theme(legend.title = element_blank())
p2

cowplot::plot_grid(p1,p2, labels="AUTO", ncol=1)
ggsave("figs/ndwi_variation.png", dpi = 300, width = 10, height = 10)

#LSWI ----

p1 <- ggplot(lswi, aes(x=date, y=lswi, col=region))+
        geom_line(cex=1.2)+
        labs(x="", y="LSWI")+
        ylim(-1, 1)+
        geom_hline(aes(yintercept=16), col = "black")+
        facet_wrap(~region)+
        scale_color_brewer(palette = 2, aesthetics="black")+
        theme_linedraw()+
        theme(legend.title = element_blank())
p1


p2 <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        mutate(average = mean(lswi), anomaly = lswi-average) %>% 
        ggplot(aes(x=date, y=anomaly)) +
        geom_col(aes(fill= anomaly>0))+
        scale_fill_manual(labels = c("FALSE" = "Lower than avg", "TRUE"="Higher than avg"), 
                          values = c("blue", "red"))+
        labs(x ="", y = "LSWI anomaly")+
        facet_wrap(~region)+
        theme_linedraw()+
        theme(legend.title = element_blank())
p2

cowplot::plot_grid(p1,p2, labels="AUTO", ncol=1)
ggsave("figs/lswi_variation.png", dpi = 300, width = 10, height = 10)


# RAIN ------

p1 <- ggplot(rain, aes(x=date, y=rain, col=region))+
        geom_line(cex=1.2)+
        labs(x="", y="Precipitation (mm)")+
        ylim(0,130)+
        geom_hline(aes(yintercept=16), col = "black")+
        facet_wrap(~region)+
        scale_color_brewer(palette = 2, aesthetics="black")+
        theme_linedraw()+
        theme(legend.title = element_blank())
p1


p2 <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        mutate(average = mean(rain), anomaly = rain-average) %>% 
        ggplot(aes(x=date, y=anomaly)) +
        geom_col(aes(fill= anomaly>0))+
        scale_fill_manual(labels = c("FALSE" = "Lower than avg", "TRUE"="Higher than avg"), 
                          values = c("blue", "red"))+
        labs(x ="", y = "Precipitation (mm) anomaly")+
        facet_wrap(~region)+
        theme_linedraw()+
        theme(legend.title = element_blank())
p2

cowplot::plot_grid(p1,p2, labels="AUTO", ncol=1)
ggsave("figs/rain_variation.png", dpi = 300, width = 10, height = 10)




# TEMPERATURE ------

p1 <- ggplot(temp, aes(x=date, y=temp, col=region))+
        geom_line(cex=1.2)+
        labs(x="", y="Temperature (°C)")+
        ylim(5,60)+
        geom_hline(aes(yintercept=32.5), col = "black")+
        facet_wrap(~region)+
        scale_color_brewer(palette = 2, aesthetics="black")+
        theme_linedraw()+
        theme(legend.title = element_blank())
p1


p2 <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        mutate(average = mean(temp, na.rm = TRUE), anomaly = temp-average) %>% 
        ggplot(aes(x=date, y=anomaly)) +
        geom_col(aes(fill= anomaly>0))+
        scale_fill_manual(labels = c("FALSE" = "Colder than avg", "TRUE"="Warmer than avg"), 
                          values = c("blue", "red"))+
        labs(x ="", y = "Temperature anomaly °C")+
        facet_wrap(~region)+
        theme_linedraw()+
        theme(legend.title = element_blank())
p2

cowplot::plot_grid(p1,p2, labels="AUTO", ncol=1)
ggsave("figs/temp_variation.png", dpi = 300, width = 10, height = 10)



# Anomalies ----

#NDVI
ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        mutate(average = mean(ndvi), anomaly = ndvi-average) %>% 
        ggplot(aes(x=month, y=anomaly, fill=anomaly>0))+
        geom_col()+
        facet_grid(year~region)+
        scale_fill_manual(labels = c("FALSE" = "Colder than avg", "TRUE"="Warmer than avg"), 
                          values = c("blue", "red"))

ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        summarise(average = mean(ndvi), sd = sd(ndvi)) %>% 
        ggplot(aes(x=month, y=average))+
        geom_line()+
        geom_errorbar(aes(x=month, ymin=average-sd, ymax=average+sd)) +
        facet_wrap(~region)
#NDWI
ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        mutate(average = mean(ndwi), anomaly = ndwi-average) %>% 
        ggplot(aes(x=month, y=anomaly, fill=anomaly>0))+
        geom_col()+
        facet_grid(year~region)+
        scale_fill_manual(labels = c("FALSE" = "Colder than avg", "TRUE"="Warmer than avg"), 
                          values = c("blue", "red"))

ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        summarise(average = mean(ndwi), sd = sd(ndwi)) %>% 
        ggplot(aes(x=month, y=average))+
        geom_line()+
        geom_errorbar(aes(x=month, ymin=average-sd, ymax=average+sd)) +
        facet_wrap(~region)
#LSWI
lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        mutate(average = mean(lswi), anomaly = lswi-average) %>% 
        ggplot(aes(x=month, y=anomaly, fill=anomaly>0))+
        geom_col()+
        facet_grid(year~region)+
        scale_fill_manual(labels = c("FALSE" = "Colder than avg", "TRUE"="Warmer than avg"), 
                          values = c("blue", "red"))

lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        summarise(average = mean(lswi), sd = sd(lswi)) %>% 
        ggplot(aes(x=month, y=average))+
        geom_line()+
        geom_errorbar(aes(x=month, ymin=average-sd, ymax=average+sd)) +
        facet_wrap(~region)
#Rain
rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        mutate(average = mean(rain), anomaly = rain-average) %>% 
        ggplot(aes(x=month, y=anomaly, fill=anomaly>0))+
        geom_col()+
        facet_grid(year~region)+
        scale_fill_manual(labels = c("FALSE" = "Colder than avg", "TRUE"="Warmer than avg"), 
                          values = c("blue", "red"))

rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        summarise(average = mean(rain), sd = sd(rain)) %>% 
        ggplot(aes(x=month, y=average))+
        geom_line()+
        geom_errorbar(aes(x=month, ymin=average-sd, ymax=average+sd)) +
        facet_wrap(~region)
#Temperature
temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        mutate(average = mean(temp), anomaly = temp-average) %>% 
        ggplot(aes(x=month, y=anomaly, fill=anomaly>0))+
        geom_col()+
        facet_grid(year~region)+
        scale_fill_manual(labels = c("FALSE" = "Colder than avg", "TRUE"="Warmer than avg"), 
                          values = c("blue", "red"))

temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(region, month) %>% 
        summarise(average = mean(temp), sd = sd(temp)) %>% 
        ggplot(aes(x=month, y=average))+
        geom_line()+
        geom_errorbar(aes(x=month, ymin=average-sd, ymax=average+sd)) +
        facet_wrap(~region)



# Decomposition NDVI ----

# Aeropeuerto-Fidepaz
aero_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Aeropuerto-Fidepaz", year < 2020) %>% 
        select(date, ndvi)

seq_along(time(aero_ndvi$date))

TS <- ts(aero_ndvi$ndvi, start= c(2000, 1), frequency = 22)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Aeropuerto.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#La Ardilla
ard_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "La Ardilla", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(ard_ndvi$date))

TS <- ts(ard_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Ardilla.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Balandra

bal_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Balandra", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(bal_ndvi$date))

TS <- ts(bal_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Balandra.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Cajoncito

caj_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Cajoncito", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(caj_ndvi$date))

TS <- ts(caj_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Cajoncito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Centenario

cen_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Centenario", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(cen_ndvi$date))

TS <- ts(cen_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Centenario.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()


#Chametla

cha_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Chametla", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(cha_ndvi$date))

TS <- ts(cha_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Chametla.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Comitan

com_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Comitan", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(com_ndvi$date))

TS <- ts(com_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Comitan.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Conchalito

con_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Conchalito", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(con_ndvi$date))

TS <- ts(con_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Conchalito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Enfermeria

enf_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Enfermeria", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(enf_ndvi$date))

TS <- ts(enf_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Enfermeria.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Erendira
ere_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Erendira", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(ere_ndvi$date))

TS <- ts(ere_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Erendira.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Espiritu Santo
es_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Espiritu Santo", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(es_ndvi$date))

TS <- ts(es_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_EspirituSanto.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Merito
mer_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Merito", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(mer_ndvi$date))

TS <- ts(mer_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Merito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()


#El Mogote
mog_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Mogote", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(mog_ndvi$date))

TS <- ts(mog_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Mogote.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Palmira
pal_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Palmira", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(pal_ndvi$date))

TS <- ts(pal_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Palmira.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Unidad Pichilingue
pic_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Unidad Pichilingue", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(pic_ndvi$date))

TS <- ts(pic_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_UnidadPichilingue.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Tesoro
tes_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Tesoro", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(tes_ndvi$date))

TS <- ts(tes_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Tesoro.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Zacatal
zac_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Zacatal", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(zac_ndvi$date))

TS <- ts(zac_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Zacatal.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Zacatecas
cas_ndvi <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Zacatecas", year < 2019) %>% 
        select(date, ndvi)

seq_along(time(cas_ndvi$date))

TS <- ts(cas_ndvi$ndvi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_NDVI_Zacatecas.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()



# Decomposition NDWI----

# Aeropeuerto-Fidepaz
aero_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Aeropuerto-Fidepaz", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(aero_ndwi$date))

TS <- ts(aero_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Aeropuerto.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#La Ardilla
ard_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "La Ardilla", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(ard_ndwi$date))

TS <- ts(ard_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Ardilla.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Balandra

bal_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Balandra", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(bal_ndwi$date))

TS <- ts(bal_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Balandra.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Cajoncito

caj_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Cajoncito", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(caj_ndwi$date))

TS <- ts(caj_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Cajoncito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Centenario

cen_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Centenario", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(cen_ndwi$date))

TS <- ts(cen_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Centenario.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()


#Chametla

cha_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Chametla", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(cha_ndwi$date))

TS <- ts(cha_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Chametla.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Comitan

com_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Comitan", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(com_ndwi$date))

TS <- ts(com_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Comitan.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Conchalito

con_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Conchalito", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(con_ndwi$date))

TS <- ts(con_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Conchalito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Enfermeria

enf_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Enfermeria", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(enf_ndwi$date))

TS <- ts(enf_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Enfermeria.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Erendira
ere_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Erendira", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(ere_ndwi$date))

TS <- ts(ere_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Erendira.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Espiritu Santo
es_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Espiritu Santo", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(es_ndwi$date))

TS <- ts(es_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_EspirituSanto.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Merito
mer_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Merito", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(mer_ndwi$date))

TS <- ts(mer_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Merito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()


#El Mogote
mog_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Mogote", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(mog_ndwi$date))

TS <- ts(mog_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Mogote.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Palmira
pal_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Palmira", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(pal_ndwi$date))

TS <- ts(pal_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Palmira.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Unidad Pichilingue
pic_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Unidad Pichilingue", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(pic_ndwi$date))

TS <- ts(pic_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_UnidadPichilingue.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Tesoro
tes_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Tesoro", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(tes_ndwi$date))

TS <- ts(tes_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Tesoro.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Zacatal
zac_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Zacatal", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(zac_ndwi$date))

TS <- ts(zac_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Zacatal.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Zacatecas
cas_ndwi <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Zacatecas", year < 2019) %>% 
        select(date, ndwi)

seq_along(time(cas_ndwi$date))

TS <- ts(cas_ndwi$ndwi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_ndwi_Zacatecas.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()



# Decomposition LSWI ----

# Aeropeuerto-Fidepaz
aero_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Aeropuerto-Fidepaz", year < 2019) %>% 
        select(date, lswi)

seq_along(time(aero_lswi$date))

TS <- ts(aero_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Aeropuerto.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#La Ardilla
ard_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "La Ardilla", year < 2019) %>% 
        select(date, lswi)

seq_along(time(ard_lswi$date))

TS <- ts(ard_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Ardilla.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Balandra

bal_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Balandra", year < 2019) %>% 
        select(date, lswi)

seq_along(time(bal_lswi$date))

TS <- ts(bal_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Balandra.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Cajoncito

caj_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Cajoncito", year < 2019) %>% 
        select(date, lswi)

seq_along(time(caj_lswi$date))

TS <- ts(caj_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Cajoncito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Centenario

cen_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Centenario", year < 2019) %>% 
        select(date, lswi)

seq_along(time(cen_lswi$date))

TS <- ts(cen_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Centenario.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()


#Chametla

cha_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Chametla", year < 2019) %>% 
        select(date, lswi)

seq_along(time(cha_lswi$date))

TS <- ts(cha_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Chametla.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Comitan

com_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Comitan", year < 2019) %>% 
        select(date, lswi)

seq_along(time(com_lswi$date))

TS <- ts(com_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Comitan.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Conchalito

con_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Conchalito", year < 2019) %>% 
        select(date, lswi)

seq_along(time(con_lswi$date))

TS <- ts(con_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Conchalito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Enfermeria

enf_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Enfermeria", year < 2019) %>% 
        select(date, lswi)

seq_along(time(enf_lswi$date))

TS <- ts(enf_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Enfermeria.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Erendira
ere_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Erendira", year < 2019) %>% 
        select(date, lswi)

seq_along(time(ere_lswi$date))

TS <- ts(ere_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Erendira.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Espiritu Santo
es_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Espiritu Santo", year < 2019) %>% 
        select(date, lswi)

seq_along(time(es_lswi$date))

TS <- ts(es_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_EspirituSanto.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Merito
mer_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Merito", year < 2019) %>% 
        select(date, lswi)

seq_along(time(mer_lswi$date))

TS <- ts(mer_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Merito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()


#El Mogote
mog_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Mogote", year < 2019) %>% 
        select(date, lswi)

seq_along(time(mog_lswi$date))

TS <- ts(mog_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Mogote.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Palmira
pal_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Palmira", year < 2019) %>% 
        select(date, lswi)

seq_along(time(pal_lswi$date))

TS <- ts(pal_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Palmira.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Unidad Pichilingue
pic_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Unidad Pichilingue", year < 2019) %>% 
        select(date, lswi)

seq_along(time(pic_lswi$date))

TS <- ts(pic_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_UnidadPichilingue.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Tesoro
tes_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Tesoro", year < 2019) %>% 
        select(date, lswi)

seq_along(time(tes_lswi$date))

TS <- ts(tes_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Tesoro.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Zacatal
zac_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Zacatal", year < 2019) %>% 
        select(date, lswi)

seq_along(time(zac_lswi$date))

TS <- ts(zac_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Zacatal.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Zacatecas
cas_lswi <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Zacatecas", year < 2019) %>% 
        select(date, lswi)

seq_along(time(cas_lswi$date))

TS <- ts(cas_lswi$lswi, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(TS)
plot(components.ts)

tiff("figs/decomposition_lswi_Zacatecas.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

# Decomposition RAIN ----

# Aeropeuerto-Fidepaz
aero_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Aeropuerto-Fidepaz", year < 2019) %>% 
        select(date, rain)

seq_along(time(aero_rain$date))

TS <- ts(aero_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Aeropuerto.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#La Ardilla
ard_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "La Ardilla", year < 2019) %>% 
        select(date, rain)

seq_along(time(ard_rain$date))

TS <- ts(ard_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Ardilla.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Balandra

bal_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Balandra", year < 2019) %>% 
        select(date, rain)

seq_along(time(bal_rain$date))

TS <- ts(bal_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Balandra.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Cajoncito

caj_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Cajoncito", year < 2019) %>% 
        select(date, rain)

seq_along(time(caj_rain$date))

TS <- ts(caj_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Cajoncito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Centenario

cen_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Centenario", year < 2019) %>% 
        select(date, rain)

seq_along(time(cen_rain$date))

TS <- ts(cen_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Centenario.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()


#Chametla

cha_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Chametla", year < 2019) %>% 
        select(date, rain)

seq_along(time(cha_rain$date))

TS <- ts(cha_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Chametla.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Comitan

com_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Comitan", year < 2019) %>% 
        select(date, rain)

seq_along(time(com_rain$date))

TS <- ts(com_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Comitan.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Conchalito

con_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Conchalito", year < 2019) %>% 
        select(date, rain)

seq_along(time(con_rain$date))

TS <- ts(con_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Conchalito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Enfermeria

enf_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Enfermeria", year < 2019) %>% 
        select(date, rain)

seq_along(time(enf_rain$date))

TS <- ts(enf_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Enfermeria.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Erendira
ere_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Erendira", year < 2019) %>% 
        select(date, rain)

seq_along(time(ere_rain$date))

TS <- ts(ere_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Erendira.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Espiritu Santo
es_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Espiritu Santo", year < 2019) %>% 
        select(date, rain)

seq_along(time(es_rain$date))

TS <- ts(es_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_EspirituSanto.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Merito
mer_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Merito", year < 2019) %>% 
        select(date, rain)

seq_along(time(mer_rain$date))

TS <- ts(mer_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Merito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()


#El Mogote
mog_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Mogote", year < 2019) %>% 
        select(date, rain)

seq_along(time(mog_rain$date))

TS <- ts(mog_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Mogote.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Palmira
pal_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Palmira", year < 2019) %>% 
        select(date, rain)

seq_along(time(pal_rain$date))

TS <- ts(pal_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Palmira.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Unidad Pichilingue
pic_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Unidad Pichilingue", year < 2019) %>% 
        select(date, rain)

seq_along(time(pic_rain$date))

TS <- ts(pic_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_UnidadPichilingue.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Tesoro
tes_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Tesoro", year < 2019) %>% 
        select(date, rain)

seq_along(time(tes_rain$date))

TS <- ts(tes_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Tesoro.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Zacatal
zac_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Zacatal", year < 2019) %>% 
        select(date, rain)

seq_along(time(zac_rain$date))

TS <- ts(zac_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Zacatal.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Zacatecas
cas_rain <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Zacatecas", year < 2019) %>% 
        select(date, rain)

seq_along(time(cas_rain$date))

TS <- ts(cas_rain$rain, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_rain_Zacatecas.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()


# Decomposition TEMPERATURE ----

# Aeropeuerto-Fidepaz
aero_temp <- temp %>% 
        mutate(na.rm = TRUE, month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Aeropuerto-Fidepaz", year < 2020) %>% 
        select(date, temp)

seq_along(time(aero_temp$date))

TS <- ts(aero_temp$temp, start= c(2017, 1), frequency = 22)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_Aeropuerto.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#La Ardilla
ard_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "La Ardilla", year < 2019) %>% 
        select(date, temp)

seq_along(time(ard_temp$date))

TS <- ts(ard_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
       

plot(components.ts)

tiff("figs/decomposition_temp_Ardilla.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Balandra

bal_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Balandra", year < 2019) %>% 
        select(date, temp)

seq_along(time(bal_temp$date))

TS <- ts(bal_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_Balandra.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Cajoncito

caj_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Cajoncito", year < 2019) %>% 
        select(date, temp)

seq_along(time(caj_temp$date))

TS <- ts(caj_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_Cajoncito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Centenario

cen_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Centenario", year < 2019) %>% 
        select(date, temp)

seq_along(time(cen_temp$date))

TS <- ts(cen_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_Centenario.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()


#Chametla

cha_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Chametla", year < 2019) %>% 
        select(date, temp)

seq_along(time(cha_temp$date))

TS <- ts(cha_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_Chametla.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Comitan

#com_temp <- temp %>% 
      #  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
     #   filter(region == "Comitan", year < 2019) %>% 
      #  select(date, temp)

#seq_along(time(com_temp$date))

#TS <- ts(com_temp$temp, start= c(2000, 1), frequency = 12)
#acf(TS)

#components.ts = decompose(na.StructTS(TS))
#plot(components.ts)

#tiff("figs/decomposition_temp_Comitan.png", res = 300, height = 12, width = 16, units = "cm")
#plot(components.ts)
#dev.off()

#El Conchalito

con_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Conchalito", year < 2019) %>% 
        select(date, temp)

seq_along(time(con_temp$date))

TS <- ts(con_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_Conchalito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Enfermeria

enf_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Enfermeria", year < 2019) %>% 
        select(date, temp)

seq_along(time(enf_temp$date))

TS <- ts(enf_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_Enfermeria.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Erendira
ere_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Erendira", year < 2019) %>% 
        select(date, temp)

seq_along(time(ere_temp$date))

TS <- ts(ere_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_Erendira.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Espiritu Santo
es_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Espiritu Santo", year < 2019) %>% 
        select(date, temp)

seq_along(time(es_temp$date))

TS <- ts(es_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_EspirituSanto.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Merito
mer_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Merito", year < 2019) %>% 
        select(date, temp)

seq_along(time(mer_temp$date))

TS <- ts(mer_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_Merito.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()


#El Mogote
mog_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Mogote", year < 2019) %>% 
        select(date, temp)

seq_along(time(mog_temp$date))

TS <- ts(mog_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_Mogote.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Palmira
pal_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Palmira", year < 2019) %>% 
        select(date, temp)

seq_along(time(pal_temp$date))

TS <- ts(pal_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_Palmira.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Unidad Pichilingue
pic_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Unidad Pichilingue", year < 2019) %>% 
        select(date, temp)

seq_along(time(pic_temp$date))

TS <- ts(pic_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_UnidadPichilingue.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Tesoro
tes_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Tesoro", year < 2019) %>% 
        select(date, temp)

seq_along(time(tes_temp$date))

TS <- ts(tes_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_Tesoro.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#El Zacatal
zac_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "El Zacatal", year < 2019) %>% 
        select(date, temp)

seq_along(time(zac_temp$date))

TS <- ts(zac_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_Zacatal.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()

#Zacatecas
cas_temp <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        filter(region == "Zacatecas", year < 2019) %>% 
        select(date, temp)

seq_along(time(cas_temp$date))

TS <- ts(cas_temp$temp, start= c(2000, 1), frequency = 12)
acf(TS)

components.ts = decompose(na.StructTS(TS))
plot(components.ts)

tiff("figs/decomposition_temp_Zacatecas.png", res = 300, height = 12, width = 16, units = "cm")
plot(components.ts)
dev.off()
