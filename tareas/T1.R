install.packages('GeoModels')

library(GeoModels)
library(ggplot2)

data(anomalies)
datos <- as.data.frame(anomalies)

summary(datos)

ggplot(datos, aes(x = lon, y = lat, color = z)) +
  geom_point() +
  scale_color_viridis_c() +
  labs(x = "Longitud", y = "Latitud", color = "Z") +
  theme_minimal()