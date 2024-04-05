# install.packages('GeoModels')

library(GeoModels)
library(ggplot2)
library(maps)

data(anomalies)
datos <- as.data.frame(anomalies)

# anomalies 
# Annual precipitation anomalies in U.S.

# Description
# A (7252x3)-matrix containing lon/lat and yearly total precipitation anomalies registered at 7.352 location sites in USA.

# Source
# Kaufman, C.G., Schervish, M.J., Nychka, D.W. (2008) Covariance tapering for likelihood-based estimation in large spatial data sets. 
# Journal of the American Statistical Association, Theory & Methods, 103, 1545–1555.

# https://cran.r-project.org/web/packages/GeoModels/GeoModels.pdf

summary(datos, )
apply(datos, 2, var)


# data
us_boundaries <- map_data("usa")
ggplot(datos, aes(x = lon, y = lat, color = z)) +
  geom_point(size=1) +
  scale_color_distiller(palette = "RdBu") +
  # scale_color_viridis_c() +
  labs(x = "Longitud", y = "Latitud", color = "Z") +
  geom_polygon(data=us_boundaries, aes(x=long, y=lat, group=group), linewidth=0.5, color="black", fill = NA) +
  theme_minimal() +
  theme(legend.key.height = unit(1.5, "cm"))

# scatter lat vs Z
ggplot(datos, aes(x = lat, y = z)) +
  geom_point() +
  labs(x = "Latitud", y = "Z") +
  theme_minimal()

# scatter lon vs Z
ggplot(datos, aes(x = lon, y = z)) +
  geom_point() +
  labs(x = "Longitud", y = "Z") +
  theme_minimal()

# hist Z
ggplot(datos, aes(x = z)) +
  geom_histogram(bins = 16, color = "black") +
  labs(x = "Z", y = "Frecuencia") +
  theme_minimal()


tau <- 0
datos$Y <- ifelse(datos$z >= tau, 1, 0)


us_boundaries <- map_data("usa")
ggplot(datos, aes(x = lon, y = lat, color = factor(Y))) +
  geom_point(size=3) +
  scale_color_manual(values = c("blue", "red")) +
  labs(x = "Longitud", y = "Latitud", color = "Z") +
  geom_polygon(data=us_boundaries, aes(x=long, y=lat, group=group), linewidth=0.5, color="black", fill = NA) +
  theme_minimal()


# subset
datos <- subset(datos, lat >= 31 & lat <= 34 & lon >= -102 & lon <= -99)

# Geary
library(spdep)
w <- as.matrix(dist(datos[,1:2]))
listw <- mat2listw(w)

taus <- seq(min(datos$z) + 1e-1, max(datos$z) - 1e-1, length.out = 100)
gearys <- list()

for (i in 1:length(taus)) {
  Y <- ifelse(datos$z >= taus[i], 1, 0)
  gearys[i] <- geary.test(Y, listw = listw, alternative = 'two.sided')$p.value#estimate[1]
}