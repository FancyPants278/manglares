library(tidyverse)
library(lubridate)
library(ggthemes)
library(zoo)
library(broom)  
library(cowplot)
library(vegan)
library(plotly)
library(xlsx)
source("00_load_data.R")


#DATA MATRIX----
tt <- mangrove_variables %>% 
        group_by(region) %>% 
        summarise(ndvi_mean=mean(ndvi, na.rm = T), 
                  ndvi_min=min(ndvi, na.rm = T),
                  ndvi_max=max(ndvi, na.rm=T),
                  ndwi_mean=mean(ndwi, na.rm = T), 
                  ndwi_min=min(ndwi, na.rm = T),
                  ndwi_max=max(ndwi, na.rm = T),
                  lswi_mean=mean(lswi, na.rm = T),
                  lswi_min=min(lswi, na.rm = T),
                  lswi_max=max(lswi, na.rm = T),
                  rain_mean=mean(rain, na.rm = T), 
                  rain_min=min(rain, na.rm = T),
                  rain_max=max(rain, na.rm = T),
                  temp_mean=mean(temp, na.rm = T),
                  temp_min=min(temp, na.rm = T),
                  temp_max=max(temp, na.rm = T))
write.xlsx(tt,"output/Summary_Mangrove_variables.xlsx")
summary(tt)




# PCA AVERAGE VARIABLES ---

env <- tt %>% 
        ungroup() %>% 
        select(ndvi_mean, ndwi_mean, lswi_mean, rain_mean, temp_mean)



rownames(env) <- tt$region
env_st <- vegan::decostand(env, method = "standardize")

pca_fit <- env %>% 
        prcomp(scale=T) # do PCA on scaled data


p <- pca_fit %>%
        augment(tt) %>% # add original dataset back in
        ggplot(aes(.fittedPC1, .fittedPC2, color = region)) + 
        geom_point() +
        theme_half_open(12) + background_grid()

plotly::ggplotly(p)



arrow_style <- arrow(
        angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

# plot rotation matrix

pca_fit %>%
        tidy(matrix = "rotation") %>%
        pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
        ggplot(aes(PC1, PC2)) +
        geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
        geom_text(
                aes(label = column),
                hjust = 1, nudge_x = -0.02, 
                color = "#904C2F"
        ) +
        xlim(-1.25, .5) + ylim(-.5, 1) +
        coord_fixed() + # fix aspect ratio to 1:1
        theme_minimal_grid(12)
summary(pca_fit)


biplot(pca_fit)

# PCA COMPLETEVARIABLES ---

env_comp <- tt %>% 
        ungroup() %>% 
        select(ndvi_mean:rain_mean, rain_max:temp_max)



rownames(env_comp) <- tt$region
env_st_comp <- vegan::decostand(env_comp, method = "standardize")

pca_fit2 <- env_comp %>% 
        prcomp(scale=T) # do PCA on scaled data


p <- pca_fit2 %>%
        augment(tt) %>% # add original dataset back in
        ggplot(aes(.fittedPC1, .fittedPC2, color = region)) + 
        geom_point(cex=4) +
        theme_half_open(12) + background_grid()

plotly::ggplotly(p)



arrow_style <- arrow(
        angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

# plot rotation matrix

pca_fit2 %>%
        tidy(matrix = "rotation") %>%
        pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
        ggplot(aes(PC1, PC2)) +
        geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
        geom_text(
                aes(label = column),
                hjust = 1, nudge_x = -0.02, 
                color = "#904C2F"
        ) +
        xlim(-1.25, .5) + ylim(-.5, 1) +
        coord_fixed() + # fix aspect ratio to 1:1
        theme_minimal_grid(12)
summary(pca_fit2)


biplot(pca_fit2)

tiff("figs/PCA.png", res = 1000, height = 30, width = 21.6, units = "cm")
biplot(pca_fit2)
dev.off()




# RDA -----

env2 <- tt %>% 
        ungroup() %>% 
        select(ndwi_mean:rain_mean, rain_max:temp_max)



env2_st <- vegan::decostand(env2, method = "standardize", na.RM=TRUE)

ndvi <- tt %>% 
        ungroup() %>% 
        select(ndvi_mean:ndvi_max)
ndvi_st <- vegan::decostand(ndvi, method = "standardize")

rownames(ndvi) <- tt$region

rda1 <- rda(ndvi~.,data=env2_st)
summary(rda1)
coef(rda1)

# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(rda1)$r.squared)
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(rda1)$adj.r.squared)


plot(rda1,
     display= c("sp", "lc", "cn"),
     main= " Triplot RDA NDVI~Environmental Variables Scaling 2- lc scores", type= "text")

rda1_sc2 <-
        scores(rda1,
               choices = 1:2,
               display = "sp"
        )
arrows(0, 0,
       rda1_sc2[, 1] * 0.92,
       rda1_sc2[, 2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)

tiff("figs/RDA.png", res = 1000, height = 21.6, width = 27.3, units = "cm")
plot(rda1,
     display= c("sp", "lc", "cn"),
     main= " Triplot RDA NDVI~Environmental Variables Scaling 2- lc scores", type= "text")
rda1_sc2 <-
        scores(rda1,
               choices = 1:2,
               display = "sp"
        )
arrows(0, 0,
       rda1_sc2[, 1] * 0.92,
       rda1_sc2[, 2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)
dev.off()





# ----

env <- tt %>% 
        ungroup() %>% 
        select(ndvi_mean:temp_max)



rownames(env) <- tt$region
env_st <- vegan::decostand(env, method = "standardize")

pca_fit <- env %>% 
        prcomp(scale=T) # do PCA on scaled data


p <- pca_fit %>%
        augment(tt) %>% # add original dataset back in
        ggplot(aes(.fittedPC1, .fittedPC2, color = region)) + 
        geom_point() +
        theme_half_open(12) + background_grid()

plotly::ggplotly(p)



arrow_style <- arrow(
        angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

