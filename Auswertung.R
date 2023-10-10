### Auswertungsskript Fallstudie Mikroalgen ##
# benötigte Packages installieren
install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("readr")

#benötigte Packages laden
library("tidyverse")
library("tidyr")
library("ggplot2")
library("readr")

#Daten einlesen
read_csv("daten.csv", header = TRUE)
