###################
#### work flow ####
## to filter ######
### infiltrometer #
####### datas #####
###################


## packages
library(tidyverse)
library(plotly)
library(zoo)

##
setwd("C:/Users/wlefranc/Documents/These_GéHCO/3_Experimentation/Test d'infiltration/5_Traitement/filterwill/filterwil/filterwil")

## chargement des noms des data frame
files <- list.files(
  path = "data/data_row",
  pattern = "\\.txt$",
  full.names = TRUE
)

## choix du fichier
x <- 8

## récupération d'informations
id_infiltro <- gsub(".txt","", str_split(files[x], pattern = "_", simplify = TRUE)[1,6])
id_point <- gsub("row/","", str_split(files[x], pattern = "_", simplify = TRUE)[1,2])
date <- paste(str_split(files[x], pattern = "_", simplify = TRUE)[1,3:5], collapse="_")

## Import du fichier
data_row <- read.csv(files[x], header = FALSE, sep = "\t", col.names = c("time", "volt"))

## fonctions qui vont nous servirent
### création de la médiane mobile
mobile_med <- function(data, k, show_graph){
  data$med <- runmed(data$volt, k = k, endrule = "keep")
  data$q25 <- rollapply(data$volt, k, quantile, probs=0.25, align="center", fill=NA)
  data$q75 <- rollapply(data$volt, k, quantile, probs=0.75, align="center", fill=NA)
  upper <- data$q75
  lower <- data$q25
  # plot rolling median and row data
  options(viewer = NULL)

  plot <- plot_ly(data) %>%
    add_lines(
      x = ~time,
      y = ~volt,
      name = "Volt brut",
      line = list(color = "lightgray")
    ) %>%
    add_lines(
      x = ~time,
      y = ~med,
      name = "Médiane mobile",
      line = list(color = "red", width = 2)
    ) %>%
    add_ribbons(
      x = ~time,
      ymin = lower,
      ymax = upper,
      name = "MAD",
      fillcolor = "rgba(0,0,255,0.2)",
      line = list(color = "transparent")
    ) %>%
    layout(
      title = "Tension et médiane mobile",
      xaxis = list(title = "Temps"),
      yaxis = list(title = "Volt")
    )
  if (show_graph) {
    fichier <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(plot, fichier, selfcontained = TRUE)
    browseURL(fichier)
  }
  return(data)
}
### calculer les pentes
slope <- function(begin, end, data, show_graph){
  # positionner le premier point
  first_point <- data %>%
    subset(time > begin) %>%
    slice(1)
  first_point <- mean(data$volt[((as.numeric(row.names(data)[data$time == first_point$time]))-9) : as.numeric(row.names(data)[data$time == first_point$time])])
  # selection début et fin
  data <- data %>%
    subset(time > begin & time < end) %>%
    mutate(time = time-min(time))
  # calcule des pentes de la médiane
  vec <- c()
  for(i in 1:(length(data$time)-1)){
    x <- data$time[i+1]
    x_1 <- data$time[i]
    y <- data$med[i+1]
    y_1 <- data$med[i]
    slope <- (y-y_1)/(x-x_1)
    vec <- c(vec,slope)
  }
  data$slope[2:length(data$time)] <- vec
  # affichage de la dispertion des pentes
  options(viewer = NULL)

  plot <- plot_ly(data) %>%
    add_markers(
      x = ~time,
      y = ~slope,
      name = "Slope de med"
    )

  if (show_graph) {
    fichier <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(plot, fichier, selfcontained = TRUE)
    browseURL(fichier)
  }
  list <- list(data, first_point)
  return(list)
}
### mise en évidence des plateaux
plate <- function(data, show_graph){
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
  ## afficher la taille des plateaux
  options(viewer = NULL)

  plot<- plot_ly(resume_data) %>%
    add_markers(
      x = ~id_plateau,
      y = ~nb,
      name = "taille du plateau",
      line = list(color = "lightgray")
    )

  if (show_graph) {
    fichier <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(plot, fichier, selfcontained = TRUE)
    browseURL(fichier)
  }
  return(list(resume_data,data_sub))
}
### extraction des fins de plateau
extract_value <- function(row_data, data, seuil, show_graph){
  ## Calculer pour chaque plateau la moyenne des dix dernières valeures
  data_end <- data %>%
    group_by(id_plateau) %>%
    summarise(
      time = max(time),
      moyenne = mean(tail(med,10)),
      .groups= "drop"
    )

  ## Filtrer les fin de plateau pour supprimer ceux mal positionnées on fixe en seuil lié au capteur
  seuil <- seuil
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
  options(viewer = NULL)

  plot <- plot_ly(data_end) %>%
    add_lines(
      x = row_data$time,
      y = row_data$volt,
      name = "Volt",
      line = list(color = "lightgray")
    ) %>%
    add_markers(
      x = ~time,
      y = ~moyenne,
      name = "Volt_select")

  if (show_graph) {
    fichier <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(plot, fichier, selfcontained = TRUE)
    browseURL(fichier)
  }
  return(data_end)
}

## workflow
nb_mobile_med = 15
data <- mobile_med(data_row, nb_mobile_med, show_graph = FALSE)
begin = 45
end = 9144
liste_slope <- slope(begin, end, data, show_graph = FALSE)
data <- liste_slope[[1]]
first_point <- liste_slope[[2]]
alpha = 0.026
data$slope[abs(data$slope) <= alpha] <- 0
liste_plate <- plate(data, show_graph = TRUE)
k = 20
select_plate <- liste_plate[[1]]$id_plateau[liste_plate[[1]]$nb >= k]
data_filter <- liste_plate[[2]] %>%
  subset(id_plateau %in% select_plate)
seuil = 0.020
data_inf <- extract_value(data, data_filter, seuil, show_graph = TRUE)
data_inf <- rbind(data_inf, c(0,0,first_point)) %>%
  arrange(time)

## convertir les volts en hauteur et calculer un débit
data_inf <- data_inf %>%
  mutate(h_tank = (moyenne-0.2)/0.0044) %>%
  mutate(h_ring = h_tank*0.38) %>%
  mutate(h_cumulee = cumsum(c(0, -diff(h_ring)))) %>%
  mutate(q = ((dplyr::lag(h_ring)-h_ring)/(time - dplyr::lag(time)))*3600) %>%
  mutate(h_diff = dplyr::lag(h_ring)-h_ring)

plot(data_inf$time, data_inf$q)
plot(data_inf$time, data_inf$h_cumulee)

## calculer un débit moyen constant
data_inf <- data_inf[c(-56,-60),] # nettoyage des données
nb_steady <- 20
mean_steady_flow <- mean(data_inf$q[(length(data_inf$q)-nb_steady):length(data_inf$q)], na.rm = TRUE)
med_steady_flow <- median(data_inf$q[(length(data_inf$q)-nb_steady):length(data_inf$q)], na.rm = TRUE)

plot(data_inf$time, data_inf$q)
abline(h = mean_steady_flow, col = "red", lwd = 2, lty = 2)
abline(h = med_steady_flow, col = "blue", lwd = 2, lty = 2)
## enregistrer les données
write.table(
  data_inf[, c("time", "h_cumulee")],
  file = paste("data/data_filter/",paste(id_point,date,id_infiltro,"hauteur.txt", sep="_"),sep=""),
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)
write.table(
  data_inf[, c("time", "q")],
  file = paste("data/data_filter/",paste(id_point,date,id_infiltro,"débit.txt", sep="_"),sep=""),
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)

## enregistrer un tableau de suivi filtration
data_resume <- read.csv("data/data_filter/data_resume.csv", sep=";")
my_date = as.character(Sys.Date())
value <- list(my_date,
           paste(id_point,date,id_infiltro, sep="_"),
           nb_mobile_med,
           k,
           alpha,
           seuil,
           begin,
           end,
           nb_steady,
           end-begin,
           mean_steady_flow,
           max(data_row$time)-end)
data_resume <- rbind(data_resume, value)
write.csv(
  data_resume,
  file = paste("data/data_filter/","data_resume.csv", sep=""),
  row.names = FALSE
)

