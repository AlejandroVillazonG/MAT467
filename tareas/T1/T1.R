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


mantel <- function(s, Y) {
  n <- nrow(s)
  suma <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      diferencia_abs <- abs(Y[i] - Y[j])
      distancia <- sqrt(sum((s[i, ] - s[j, ])^2))
      suma <- suma + diferencia_abs * distancia
    }
  }
  return(suma * 2)
}

resultado <- mantel(datos[, c('lon', 'lat')], datos$Y)
print(resultado)


