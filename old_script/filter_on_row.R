library(plotly)

## open txt
data <- read.csv("test_filter.txt", header = FALSE, sep = "\t", col.names = c("time", "volt"))

## etablir le debut et la fin de la chronique
time_begin <- 0
time_end <- 2850
data <- data %>%
  subset(time > time_begin & time < time_end)

## calcule de la pente entre 2 points de la médiane
vec <- c()
for(i in 1:length(data$time)-1){
  x <- data$time[i+1]
  x_1 <- data$time[i]
  y <- data$volt[i+1]
  y_1 <- data$volt[i]
  slope <- (y-y_1)/(x-x_1)
  vec <- c(vec,slope)
}
plot(data$time[2:length(data$time)], vec)

# ajout d'une valeur seuil (alpha) pour selectionner les ruptures de pentes | l'ajout du seuil permet de filtrer le bruit
alpha <- 0.070
data$slope[2:length(data$time)] <- abs(vec)
data$slope[data$slope <= alpha] <- 0

# plot les coefficients directeurs avec la médiane mobile
plot_ly(data) %>%
  add_lines(
    x = ~time,
    y = ~volt,
    name = "tension",
    line = list(color = "lightgray")
  ) %>%
  add_lines(
    x = ~time,
    y = ~slope,
    name = "slope",
    line = list(color = "red", width = 2)
  ) %>%
  layout(
    title = "tension et pente",
    xaxis = list(title = "Temps"),
    yaxis = list(title = "Volt")
  )

## Ajout à la chronique de donnée un id plateau
data_sub <- data %>%
  mutate(id = as.numeric(rownames(.)) ) %>%
  subset(slope == 0) %>%
  mutate(id_plateau = cumsum(c(1,diff(id)>1)))

## Pour chaque plateau, identifier le début, la fin et le nombre d'enregistrement
resume_data <- data_sub %>%
  group_by(id_plateau) %>%
  summarise(
    min = min(id),
    max = max(id),
    nb = length(id),
    .groups ="drop"
  )

## Filtrer chaque plateau pour ne selection que ceux qui sont supérieurs à filter
plot_ly(resume_data) %>%
  add_markers(
    x = ~id_plateau,
    y = ~nb,
    name = "Volt",
    line = list(color = "lightgray")
  )

filter = 10
select_plateau <- resume_data$id_plateau[resume_data$nb >= filter]
data_filter <- data_sub %>%
  subset(id_plateau %in% select_plateau)

## Calculer pour chaque plateau la moyenne des dix dernières valeures
data_end <- data_filter %>%
  group_by(id_plateau) %>%
  summarise(
    time = max(time),
    moyenne = mean(tail(volt,10)),
    .groups= "drop"
  )

## Filtrer les fin de plateau pour supprimer ceux mal positionnées on fixe en seuil lié au capteur
seuil <- 0.010
vec <- vector()
for (i in 2:length(data_end$time)){
  x <- data_end$moyenne[i-1]
  x_1 <- data_end$moyenne[i]
  if(x-x_1 < seuil){
  vec <- c(vec,i-1)
  }
}
vec <- c(vec, length(data_end$time)) # on vient enlever le dernier point
data_end <- data_end[-vec,]

# Afficher les valeurs selectionee et les row data
plot_ly(data_end) %>%
  add_lines(
    x = data$time,
    y = data$volt,
    name = "Volt",
    line = list(color = "lightgray")
  ) %>%
  add_markers(
    x = ~time,
    y = ~moyenne,
    name = "Volt_select")

## convertir les volts en hauteur et calculer un débit
inf <- data_end %>%
  mutate(h_tank = (moyenne-0.2)/0.0044) %>%
  mutate(h_ring = h_tank*0.38) %>%
  mutate(h_cumulee = cumsum(c(0, -diff(h_ring)))) %>%
  mutate(q = ((dplyr::lag(h_ring)-h_ring)/(time - dplyr::lag(time)))*3600) %>%
  mutate(h_diff = dplyr::lag(h_ring)-h_ring)

plot(inf$time, inf$q)
plot(inf$time, inf$h_cumulee)
plot(inf$time, inf$h_diff)



write.table(
  data[, c("time", "med")],
  file = "I3_test_seb_med.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)

