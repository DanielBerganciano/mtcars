if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("tidyr")) install.packages("tidyr")
library(tidyr)

data(mtcars)
df <- as.data.frame(mtcars)

print("Dataset original:")
print(head(df))

df <- df %>%
  select(mpg, cyl, hp, gear) %>%
  filter(cyl > 4)
print("Tras selección y filtrado:")
print(df)

df <- df %>%
  arrange(desc(hp)) %>%
  rename(consumo = mpg, potencia = hp)
print("Tras ordenar y renombrar:")
print(df)

df <- df %>%
  mutate(eficiencia = consumo / potencia)
grouped_df <- df %>%
  group_by(cyl) %>%
  summarise(
    consumo_medio = mean(consumo, na.rm = TRUE),
    potencia_maxima = max(potencia, na.rm = TRUE)
  )

print("Tras crear columna eficiencia y agrupar:")
print(df)
print("Resultados agrupados:")
print(grouped_df)

transmission_data <- data.frame(
  gear = c(3, 4, 5),
  tipo_transmision = c("Manual", "Automática", "Semiautomática")
)

df <- df %>%
  left_join(transmission_data, by = "gear")

print("Tras realizar el left_join:")
print(df)

long_df <- df %>%
  pivot_longer(cols = c(consumo, potencia, eficiencia), names_to = "medida", values_to = "valor")
print("Formato largo:")
print(long_df)

duplicates <- long_df %>%
  group_by(cyl, gear, tipo_transmision, medida) %>%
  filter(n() > 1)
print("Duplicados identificados:")
print(duplicates)

wide_df <- long_df %>%
  group_by(cyl, gear, tipo_transmision, medida) %>%
  summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = medida, values_from = valor)
print("Formato ancho:")
print(wide_df)