library(tidyverse)
library(lubridate)
library(ggthemes)
library(zoo)





# NDVI -------------------------

#nl= NDVI desde Landsat

nl_aero <- read.csv("data/ndvi/landsat/aeropuerto_nl.csv")
nl_aero$date <- parse_date_time(nl_aero$system.time_start, orders = c("mdy"))
nl_aero$region <- rep("Aeropuerto-Fidepaz", nrow(nl_aero))


nl_ardilla <- read.csv("data/ndvi/landsat/ardilla_nl.csv")
nl_ardilla$date <- parse_date_time(nl_ardilla$system.time_start, orders = c("mdy"))
nl_ardilla$region <- rep("La Ardilla", nrow(nl_ardilla))


nl_balandra <- read.csv("data/ndvi/landsat/balandra_nl.csv")
nl_balandra$date <- parse_date_time(nl_balandra$system.time_start, orders = c("mdy"))
nl_balandra$region <- rep("Balandra", nrow(nl_balandra))


nl_cajoncito <- read.csv("data/ndvi/landsat/cajoncito_nl.csv")
nl_cajoncito$date <- parse_date_time(nl_cajoncito$system.time_start, orders = c("mdy"))
nl_cajoncito$region <- rep("El Cajoncito", nrow(nl_cajoncito))


nl_centenario <- read.csv("data/ndvi/landsat/centenario_nl.csv")
nl_centenario$date <- parse_date_time(nl_centenario$system.time_start, orders = c("mdy"))
nl_centenario$region <- rep("El Centenario", nrow(nl_centenario))


nl_chametla <- read.csv("data/ndvi/landsat/chametla_nl.csv")
nl_chametla$date <- parse_date_time(nl_chametla$system.time_start, orders = c("mdy"))
nl_chametla$region <- rep("Chametla", nrow(nl_chametla))


nl_comitan <- read.csv("data/ndvi/landsat/comitan_nl.csv")
nl_comitan$date <- parse_date_time(nl_comitan$system.time_start, orders = c("mdy"))
nl_comitan$region <- rep("Comitan", nrow(nl_comitan))


nl_conchalito <- read.csv("data/ndvi/landsat/conchalito_nl.csv")
nl_conchalito$date <- parse_date_time(nl_conchalito$system.time_start, orders = c("mdy"))
nl_conchalito$region <- rep("El Conchalito", nrow(nl_conchalito))


nl_enfermeria <- read.csv("data/ndvi/landsat/enfermeria_nl.csv")
nl_enfermeria$date <- parse_date_time(nl_enfermeria$system.time_start, orders = c("mdy"))
nl_enfermeria$region <- rep("Enfermeria", nrow(nl_enfermeria))


nl_erendira <- read.csv("data/ndvi/landsat/erendira_nl.csv")
nl_erendira$date <- parse_date_time(nl_erendira$system.time_start, orders = c("mdy"))
nl_erendira$region <- rep("Erendira", nrow(nl_erendira))


nl_ES <- read.csv("data/ndvi/landsat/ES_nl.csv")
nl_ES$date <- parse_date_time(nl_ES$system.time_start, orders = c("mdy"))
nl_ES$region <- rep("Espiritu Santo", nrow(nl_ES))


nl_merito <- read.csv("data/ndvi/landsat/merito_nl.csv")
nl_merito$date <- parse_date_time(nl_merito$system.time_start, orders = c("mdy"))
nl_merito$region <- rep("El Merito", nrow(nl_merito))


nl_mogote <- read.csv("data/ndvi/landsat/mogote_nl.csv")
nl_mogote$date <- parse_date_time(nl_mogote$system.time_start, orders = c("mdy"))
nl_mogote$region <- rep("El Mogote", nrow(nl_mogote))


nl_palmira <- read.csv("data/ndvi/landsat/palmira_nl.csv")
nl_palmira$date <- parse_date_time(nl_palmira$system.time_start, orders = c("mdy"))
nl_palmira$region <- rep("Palmira", nrow(nl_palmira))


nl_pichilingue <- read.csv("data/ndvi/landsat/pichilingue_nl.csv")
nl_pichilingue$date <- parse_date_time(nl_pichilingue$system.time_start, orders = c("mdy"))
nl_pichilingue$region <- rep("Unidad Pichilingue", nrow(nl_pichilingue))


nl_tesoro <- read.csv("data/ndvi/landsat/tesoro_nl.csv")
nl_tesoro$date <- parse_date_time(nl_tesoro$system.time_start, orders = c("mdy"))
nl_tesoro$region <- rep("El Tesoro", nrow(nl_tesoro))


nl_zacatal <- read.csv("data/ndvi/landsat/zacatal_nl.csv")
nl_zacatal$date <- parse_date_time(nl_zacatal$system.time_start, orders = c("mdy"))
nl_zacatal$region <- rep("El Zacatal", nrow(nl_zacatal))


nl_zacatecas <- read.csv("data/ndvi/landsat/zacatecas_nl.csv")
nl_zacatecas$date <- parse_date_time(nl_zacatecas$system.time_start, orders = c("mdy"))
nl_zacatecas$region <- rep("Zacatecas", nrow(nl_zacatecas))

ndvi <- rbind(nl_aero,nl_ardilla,nl_balandra,nl_cajoncito,nl_centenario,nl_chametla, nl_comitan, nl_conchalito, nl_enfermeria, nl_erendira, nl_ES, nl_merito, nl_mogote, nl_palmira, nl_pichilingue, nl_tesoro, nl_zacatal, nl_zacatecas)
ndvi$region <- factor(ndvi$region, levels = c("Aeropuerto-Fidepaz","La Ardilla", "Balandra", "El Cajoncito", "El Centenario", "Chametla", "Comitan", "El Conchalito", "Enfermeria", "Erendira", "Espiritu Santo", "El Merito", "El Mogote", "Palmira", "Unidad Pichilingue", "El Tesoro", "El Zacatal", "Zacatecas"))

ndvi <- ndvi %>% 
        rename(
                ndvi=X0
        )

# NDWI -------------------------


wl_aero <- read.csv("data/ndwi/aeropuerto_wl.csv")
wl_aero$date <- parse_date_time(wl_aero$system.time_start, orders = c("mdy"))
wl_aero$region <- rep("Aeropuerto-Fidepaz", nrow(wl_aero))


wl_ardilla <- read.csv("data/ndwi/ardilla_wl.csv")
wl_ardilla$date <- parse_date_time(wl_ardilla$system.time_start, orders = c("mdy"))
wl_ardilla$region <- rep("La Ardilla", nrow(wl_ardilla))


wl_balandra <- read.csv("data/ndwi/balandra_wl.csv")
wl_balandra$date <- parse_date_time(wl_balandra$system.time_start, orders = c("mdy"))
wl_balandra$region <- rep("Balandra", nrow(wl_balandra))


wl_cajoncito <- read.csv("data/ndwi/cajoncito_wl.csv")
wl_cajoncito$date <- parse_date_time(wl_cajoncito$system.time_start, orders = c("mdy"))
wl_cajoncito$region <- rep("El Cajoncito", nrow(wl_cajoncito))


wl_centenario <- read.csv("data/ndwi/centenario_wl.csv")
wl_centenario$date <- parse_date_time(wl_centenario$system.time_start, orders = c("mdy"))
wl_centenario$region <- rep("El Centenario", nrow(wl_centenario))


wl_chametla <- read.csv("data/ndwi/chametla_wl.csv")
wl_chametla$date <- parse_date_time(wl_chametla$system.time_start, orders = c("mdy"))
wl_chametla$region <- rep("Chametla", nrow(wl_chametla))


wl_comitan <- read.csv("data/ndwi/comitan_wl.csv")
wl_comitan$date <- parse_date_time(wl_comitan$system.time_start, orders = c("mdy"))
wl_comitan$region <- rep("Comitan", nrow(wl_comitan))


wl_conchalito <- read.csv("data/ndwi/conchalito_wl.csv")
wl_conchalito$date <- parse_date_time(wl_conchalito$system.time_start, orders = c("mdy"))
wl_conchalito$region <- rep("El Conchalito", nrow(wl_conchalito))


wl_enfermeria <- read.csv("data/ndwi/enfermeria_wl.csv")
wl_enfermeria$date <- parse_date_time(wl_enfermeria$system.time_start, orders = c("mdy"))
wl_enfermeria$region <- rep("Enfermeria", nrow(wl_enfermeria))


wl_erendira <- read.csv("data/ndwi/erendira_wl.csv")
wl_erendira$date <- parse_date_time(wl_erendira$system.time_start, orders = c("mdy"))
wl_erendira$region <- rep("Erendira", nrow(wl_erendira))


wl_ES <- read.csv("data/ndwi/ES_wl.csv")
wl_ES$date <- parse_date_time(wl_ES$system.time_start, orders = c("mdy"))
wl_ES$region <- rep("Espiritu Santo", nrow(wl_ES))


wl_merito <- read.csv("data/ndwi/merito_wl.csv")
wl_merito$date <- parse_date_time(wl_merito$system.time_start, orders = c("mdy"))
wl_merito$region <- rep("El Merito", nrow(wl_merito))


wl_mogote <- read.csv("data/ndwi/mogote_wl.csv")
wl_mogote$date <- parse_date_time(wl_mogote$system.time_start, orders = c("mdy"))
wl_mogote$region <- rep("El Mogote", nrow(wl_mogote))


wl_palmira <- read.csv("data/ndwi/palmira_wl.csv")
wl_palmira$date <- parse_date_time(wl_palmira$system.time_start, orders = c("mdy"))
wl_palmira$region <- rep("Palmira", nrow(wl_palmira))


wl_pichilingue <- read.csv("data/ndwi/pichilingue_wl.csv")
wl_pichilingue$date <- parse_date_time(wl_pichilingue$system.time_start, orders = c("mdy"))
wl_pichilingue$region <- rep("Unidad Pichilingue", nrow(wl_pichilingue))


wl_tesoro <- read.csv("data/ndwi/tesoro_wl.csv")
wl_tesoro$date <- parse_date_time(wl_tesoro$system.time_start, orders = c("mdy"))
wl_tesoro$region <- rep("El Tesoro", nrow(wl_tesoro))


wl_zacatal <- read.csv("data/ndwi/zacatal_wl.csv")
wl_zacatal$date <- parse_date_time(wl_zacatal$system.time_start, orders = c("mdy"))
wl_zacatal$region <- rep("El Zacatal", nrow(wl_zacatal))


wl_zacatecas <- read.csv("data/ndwi/zacatecas_wl.csv")
wl_zacatecas$date <- parse_date_time(wl_zacatecas$system.time_start, orders = c("mdy"))
wl_zacatecas$region <- rep("Zacatecas", nrow(wl_zacatecas))

ndwi <- rbind(wl_aero,wl_ardilla,wl_balandra,wl_cajoncito,wl_centenario,wl_chametla, wl_comitan, wl_conchalito, wl_enfermeria, wl_erendira, wl_ES, wl_merito, wl_mogote, wl_palmira, wl_pichilingue, wl_tesoro, wl_zacatal, wl_zacatecas)
ndwi$region <- factor(ndwi$region, levels = c("Aeropuerto-Fidepaz","La Ardilla", "Balandra", "El Cajoncito", "El Centenario", "Chametla", "Comitan", "El Conchalito", "Enfermeria", "Erendira", "Espiritu Santo", "El Merito", "El Mogote", "Palmira", "Unidad Pichilingue", "El Tesoro", "El Zacatal", "Zacatecas"))

ndwi <- ndwi %>% 
        rename(
                ndwi=X0
        )


# LSWI -------------------------


ll_aero <- read.csv("data/lswi/aeropuerto_ll.csv")
ll_aero$date <- parse_date_time(ll_aero$system.time_start, orders = c("mdy"))
ll_aero$region <- rep("Aeropuerto-Fidepaz", nrow(ll_aero))


ll_ardilla <- read.csv("data/lswi/ardilla_ll.csv")
ll_ardilla$date <- parse_date_time(ll_ardilla$system.time_start, orders = c("mdy"))
ll_ardilla$region <- rep("La Ardilla", nrow(ll_ardilla))


ll_balandra <- read.csv("data/lswi/balandra_ll.csv")
ll_balandra$date <- parse_date_time(ll_balandra$system.time_start, orders = c("mdy"))
ll_balandra$region <- rep("Balandra", nrow(ll_balandra))


ll_cajoncito <- read.csv("data/lswi/cajoncito_ll.csv")
ll_cajoncito$date <- parse_date_time(ll_cajoncito$system.time_start, orders = c("mdy"))
ll_cajoncito$region <- rep("El Cajoncito", nrow(ll_cajoncito))


ll_centenario <- read.csv("data/lswi/centenario_ll.csv")
ll_centenario$date <- parse_date_time(ll_centenario$system.time_start, orders = c("mdy"))
ll_centenario$region <- rep("El Centenario", nrow(ll_centenario))


ll_chametla <- read.csv("data/lswi/chametla_ll.csv")
ll_chametla$date <- parse_date_time(ll_chametla$system.time_start, orders = c("mdy"))
ll_chametla$region <- rep("Chametla", nrow(ll_chametla))


ll_comitan <- read.csv("data/lswi/comitan_ll.csv")
ll_comitan$date <- parse_date_time(ll_comitan$system.time_start, orders = c("mdy"))
ll_comitan$region <- rep("Comitan", nrow(ll_comitan))


ll_conchalito <- read.csv("data/lswi/conchalito_ll.csv")
ll_conchalito$date <- parse_date_time(ll_conchalito$system.time_start, orders = c("mdy"))
ll_conchalito$region <- rep("El Conchalito", nrow(ll_conchalito))


ll_enfermeria <- read.csv("data/lswi/enfermeria_ll.csv")
ll_enfermeria$date <- parse_date_time(ll_enfermeria$system.time_start, orders = c("mdy"))
ll_enfermeria$region <- rep("Enfermeria", nrow(ll_enfermeria))


ll_erendira <- read.csv("data/lswi/erendira_ll.csv")
ll_erendira$date <- parse_date_time(ll_erendira$system.time_start, orders = c("mdy"))
ll_erendira$region <- rep("Erendira", nrow(ll_erendira))


ll_ES <- read.csv("data/lswi/ES_ll.csv")
ll_ES$date <- parse_date_time(ll_ES$system.time_start, orders = c("mdy"))
ll_ES$region <- rep("Espiritu Santo", nrow(ll_ES))


ll_merito <- read.csv("data/lswi/merito_ll.csv")
ll_merito$date <- parse_date_time(ll_merito$system.time_start, orders = c("mdy"))
ll_merito$region <- rep("El Merito", nrow(ll_merito))


ll_mogote <- read.csv("data/lswi/mogote_ll.csv")
ll_mogote$date <- parse_date_time(ll_mogote$system.time_start, orders = c("mdy"))
ll_mogote$region <- rep("El Mogote", nrow(ll_mogote))


ll_palmira <- read.csv("data/lswi/palmira_ll.csv")
ll_palmira$date <- parse_date_time(ll_palmira$system.time_start, orders = c("mdy"))
ll_palmira$region <- rep("Palmira", nrow(ll_palmira))


ll_pichilingue <- read.csv("data/lswi/pichilingue_ll.csv")
ll_pichilingue$date <- parse_date_time(ll_pichilingue$system.time_start, orders = c("mdy"))
ll_pichilingue$region <- rep("Unidad Pichilingue", nrow(ll_pichilingue))


ll_tesoro <- read.csv("data/lswi/tesoro_ll.csv")
ll_tesoro$date <- parse_date_time(ll_tesoro$system.time_start, orders = c("mdy"))
ll_tesoro$region <- rep("El Tesoro", nrow(ll_tesoro))


ll_zacatal <- read.csv("data/lswi/zacatal_ll.csv")
ll_zacatal$date <- parse_date_time(ll_zacatal$system.time_start, orders = c("mdy"))
ll_zacatal$region <- rep("El Zacatal", nrow(ll_zacatal))


ll_zacatecas <- read.csv("data/lswi/zacatecas_ll.csv")
ll_zacatecas$date <- parse_date_time(ll_zacatecas$system.time_start, orders = c("mdy"))
ll_zacatecas$region <- rep("Zacatecas", nrow(ll_zacatecas))

lswi <- rbind(ll_aero,ll_ardilla,ll_balandra,ll_cajoncito,ll_centenario,ll_chametla, ll_comitan, ll_conchalito, ll_enfermeria, ll_erendira, ll_ES, ll_merito, ll_mogote, ll_palmira, ll_pichilingue, ll_tesoro, ll_zacatal, ll_zacatecas)
lswi$region <- factor(lswi$region, levels = c("Aeropuerto-Fidepaz","La Ardilla", "Balandra", "El Cajoncito", "El Centenario", "Chametla", "Comitan", "El Conchalito", "Enfermeria", "Erendira", "Espiritu Santo", "El Merito", "El Mogote", "Palmira", "Unidad Pichilingue", "El Tesoro", "El Zacatal", "Zacatecas"))

lswi <- lswi %>% 
        rename(
                lswi=X0
        )


# RAIN -------------------------

p_aero <- read.csv("data/rain/aeropuerto_p.csv")
p_aero$date <- parse_date_time(p_aero$system.time_start, orders = c("mdy"))
p_aero$region <- rep("Aeropuerto-Fidepaz", nrow(p_aero))


p_ardilla <- read.csv("data/rain/ardilla_p.csv")
p_ardilla$date <- parse_date_time(p_ardilla$system.time_start, orders = c("mdy"))
p_ardilla$region <- rep("La Ardilla", nrow(p_ardilla))


p_balandra <- read.csv("data/rain/balandra_p.csv")
p_balandra$date <- parse_date_time(p_balandra$system.time_start, orders = c("mdy"))
p_balandra$region <- rep("Balandra", nrow(p_balandra))


p_cajoncito <- read.csv("data/rain/cajoncito_p.csv")
p_cajoncito$date <- parse_date_time(p_cajoncito$system.time_start, orders = c("mdy"))
p_cajoncito$region <- rep("El Cajoncito", nrow(p_cajoncito))


p_centenario <- read.csv("data/rain/centenario_p.csv")
p_centenario$date <- parse_date_time(p_centenario$system.time_start, orders = c("mdy"))
p_centenario$region <- rep("El Centenario", nrow(p_centenario))


p_chametla <- read.csv("data/rain/chametla_p.csv")
p_chametla$date <- parse_date_time(p_chametla$system.time_start, orders = c("mdy"))
p_chametla$region <- rep("Chametla", nrow(p_chametla))


p_comitan <- read.csv("data/rain/comitan_p.csv")
p_comitan$date <- parse_date_time(p_comitan$system.time_start, orders = c("mdy"))
p_comitan$region <- rep("Comitan", nrow(p_comitan))


p_conchalito <- read.csv("data/rain/conchalito_p.csv")
p_conchalito$date <- parse_date_time(p_conchalito$system.time_start, orders = c("mdy"))
p_conchalito$region <- rep("El Conchalito", nrow(p_conchalito))


p_enfermeria <- read.csv("data/rain/enfermeria_p.csv")
p_enfermeria$date <- parse_date_time(p_enfermeria$system.time_start, orders = c("mdy"))
p_enfermeria$region <- rep("Enfermeria", nrow(p_enfermeria))


p_erendira <- read.csv("data/rain/erendira_p.csv")
p_erendira$date <- parse_date_time(p_erendira$system.time_start, orders = c("mdy"))
p_erendira$region <- rep("Erendira", nrow(p_erendira))


p_ES <- read.csv("data/rain/ES_p.csv")
p_ES$date <- parse_date_time(p_ES$system.time_start, orders = c("mdy"))
p_ES$region <- rep("Espiritu Santo", nrow(p_ES))


p_merito <- read.csv("data/rain/merito_p.csv")
p_merito$date <- parse_date_time(p_merito$system.time_start, orders = c("mdy"))
p_merito$region <- rep("El Merito", nrow(p_merito))


p_mogote <- read.csv("data/rain/mogote_p.csv")
p_mogote$date <- parse_date_time(p_mogote$system.time_start, orders = c("mdy"))
p_mogote$region <- rep("El Mogote", nrow(p_mogote))


p_palmira <- read.csv("data/rain/palmira_p.csv")
p_palmira$date <- parse_date_time(p_palmira$system.time_start, orders = c("mdy"))
p_palmira$region <- rep("Palmira", nrow(p_palmira))


p_pichilingue <- read.csv("data/rain/pichilingue_p.csv")
p_pichilingue$date <- parse_date_time(p_pichilingue$system.time_start, orders = c("mdy"))
p_pichilingue$region <- rep("Unidad Pichilingue", nrow(p_pichilingue))


p_tesoro <- read.csv("data/rain/tesoro_p.csv")
p_tesoro$date <- parse_date_time(p_tesoro$system.time_start, orders = c("mdy"))
p_tesoro$region <- rep("El Tesoro", nrow(p_tesoro))


p_zacatal <- read.csv("data/rain/zacatal_p.csv")
p_zacatal$date <- parse_date_time(p_zacatal$system.time_start, orders = c("mdy"))
p_zacatal$region <- rep("El Zacatal", nrow(p_zacatal))


p_zacatecas <- read.csv("data/rain/zacatecas_p.csv")
p_zacatecas$date <- parse_date_time(p_zacatecas$system.time_start, orders = c("mdy"))
p_zacatecas$region <- rep("Zacatecas", nrow(p_zacatecas))

rain <- rbind(p_aero,p_ardilla,p_balandra,p_cajoncito,p_centenario,p_chametla, p_comitan, p_conchalito, p_enfermeria, p_erendira, p_ES, p_merito, p_mogote, p_palmira, p_pichilingue, p_tesoro, p_zacatal, p_zacatecas)
rain$region <- factor(rain$region, levels = c("Aeropuerto-Fidepaz","La Ardilla", "Balandra", "El Cajoncito", "El Centenario", "Chametla", "Comitan", "El Conchalito", "Enfermeria", "Erendira", "Espiritu Santo", "El Merito", "El Mogote", "Palmira", "Unidad Pichilingue", "El Tesoro", "El Zacatal", "Zacatecas"))

rain <- rain %>% 
        rename(
                rain=precipitation
        )

# TEMPERATURE -------------------------

t_aero <- read.csv("data/temp/aeropuerto_t.csv")
t_aero$date <- parse_date_time(t_aero$system.time_start, orders = c("mdy"))
t_aero$region <- rep("Aeropuerto-Fidepaz", nrow(t_aero))


t_ardilla <- read.csv("data/temp/ardilla_t.csv")
t_ardilla$date <- parse_date_time(t_ardilla$system.time_start, orders = c("mdy"))
t_ardilla$region <- rep("La Ardilla", nrow(t_ardilla))


t_balandra <- read.csv("data/temp/balandra_t.csv")
t_balandra$date <- parse_date_time(t_balandra$system.time_start, orders = c("mdy"))
t_balandra$region <- rep("Balandra", nrow(t_balandra))


t_cajoncito <- read.csv("data/temp/cajoncito_t.csv")
t_cajoncito$date <- parse_date_time(t_cajoncito$system.time_start, orders = c("mdy"))
t_cajoncito$region <- rep("El Cajoncito", nrow(t_cajoncito))


t_centenario <- read.csv("data/temp/centenario_t.csv")
t_centenario$date <- parse_date_time(t_centenario$system.time_start, orders = c("mdy"))
t_centenario$region <- rep("El Centenario", nrow(t_centenario))


t_chametla <- read.csv("data/temp/chametla_t.csv")
t_chametla$date <- parse_date_time(t_chametla$system.time_start, orders = c("mdy"))
t_chametla$region <- rep("Chametla", nrow(t_chametla))


#t_comitan <- read.csv("data/temp/comitan_t.csv")
#t_comitan$date <- parse_date_time(t_comitan$system.time_start, orders = c("mdy"))
#$region <- rep("Comitan", nrow(t_comitan))


t_conchalito <- read.csv("data/temp/conchalito_t.csv")
t_conchalito$date <- parse_date_time(t_conchalito$system.time_start, orders = c("mdy"))
t_conchalito$region <- rep("El Conchalito", nrow(t_conchalito))


t_enfermeria <- read.csv("data/temp/enfermeria_t.csv")
t_enfermeria$date <- parse_date_time(t_enfermeria$system.time_start, orders = c("mdy"))
t_enfermeria$region <- rep("Enfermeria", nrow(t_enfermeria))


t_erendira <- read.csv("data/temp/erendira_t.csv")
t_erendira$date <- parse_date_time(t_erendira$system.time_start, orders = c("mdy"))
t_erendira$region <- rep("Erendira", nrow(t_erendira))


t_ES <- read.csv("data/temp/ES_t.csv")
t_ES$date <- parse_date_time(t_ES$system.time_start, orders = c("mdy"))
t_ES$region <- rep("Espiritu Santo", nrow(t_ES))


t_merito <- read.csv("data/temp/merito_t.csv")
t_merito$date <- parse_date_time(t_merito$system.time_start, orders = c("mdy"))
t_merito$region <- rep("El Merito", nrow(t_merito))


t_mogote <- read.csv("data/temp/mogote_t.csv")
t_mogote$date <- parse_date_time(t_mogote$system.time_start, orders = c("mdy"))
t_mogote$region <- rep("El Mogote", nrow(t_mogote))


t_palmira <- read.csv("data/temp/palmira_t.csv")
t_palmira$date <- parse_date_time(t_palmira$system.time_start, orders = c("mdy"))
t_palmira$region <- rep("Palmira", nrow(t_palmira))


t_pichilingue <- read.csv("data/temp/pichilingue_t.csv")
t_pichilingue$date <- parse_date_time(t_pichilingue$system.time_start, orders = c("mdy"))
t_pichilingue$region <- rep("Unidad Pichilingue", nrow(t_pichilingue))


t_tesoro <- read.csv("data/temp/tesoro_t.csv")
t_tesoro$date <- parse_date_time(t_tesoro$system.time_start, orders = c("mdy"))
t_tesoro$region <- rep("El Tesoro", nrow(t_tesoro))


t_zacatal <- read.csv("data/temp/zacatal_t.csv")
t_zacatal$date <- parse_date_time(t_zacatal$system.time_start, orders = c("mdy"))
t_zacatal$region <- rep("El Zacatal", nrow(t_zacatal))


t_zacatecas <- read.csv("data/temp/zacatecas_t.csv")
t_zacatecas$date <- parse_date_time(t_zacatecas$system.time_start, orders = c("mdy"))
t_zacatecas$region <- rep("Zacatecas", nrow(t_zacatecas))

temp <- rbind(t_aero,t_ardilla,t_balandra,t_cajoncito,t_centenario,t_chametla, t_conchalito, t_enfermeria, t_erendira, t_ES, t_merito, t_mogote, t_palmira, t_pichilingue, t_tesoro, t_zacatal, t_zacatecas)
temp$region <- factor(temp$region, levels = c("Aeropuerto-Fidepaz","La Ardilla", "Balandra", "El Cajoncito", "El Centenario", "Chametla","El Conchalito", "Enfermeria", "Erendira", "Espiritu Santo", "El Merito", "El Mogote", "Palmira", "Unidad Pichilingue", "El Tesoro", "El Zacatal", "Zacatecas"))

temp <- temp %>% 
        rename(
                temp=LST_Day_1km
        )


# Monthly average----
#NDVI
ndvi_ts <- ndvi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(year, month) %>% 
        summarize(ndvi=mean(ndvi))
#NDWI
ndwi_ts <- ndwi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(year, month) %>% 
        summarize(ndwi=mean(ndwi))

#LSWI
lswi_ts <- lswi %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(year, month) %>% 
        summarize(lswi=mean(lswi))

#RAIN
rain_ts <- rain %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
        group_by(year, month) %>% 
        summarize(rain=mean(rain))

#TEMPERATURE
temp_ts <- temp %>% 
        mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%  
        group_by(year, month) %>% 
        summarize(temp=mean(temp, na.rm = TRUE))


#MERGED VARIABLES -----
mangrove_variables <-  merge(ndvi, ndwi, by=c("region","system.time_start", "date")) %>% 
        merge(lswi, by=c("region","system.time_start", "date")) %>% 
        merge(rain, by=c("region","system.time_start", "date")) %>% 
        merge(temp, na.rm=TRUE, by=c("region","system.time_start", "date"))






