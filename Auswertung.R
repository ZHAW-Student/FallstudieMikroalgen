### Auswertungsskript Fallstudie Mikroalgen ###

### benötigte Packages installieren ###
install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("lubridate")

# benötigte Packages laden
library("tidyverse")
library("tidyr")
library("ggplot2")
library("readr")
library("lubridate")


# Daten einlesen
reactor <- read_csv("daten.csv") #Dateinamen anpassen, falls ; als Trennzeichen read_csv2 verwenden
labor <- read_csv("Labordaten.csv") #Dateinamen anpassen, falls ; als Trennzeichen read_csv2 verwenden

labor$Datum_Probenahme <- as.POSIXct(labor$Datum_Probenahme, format = "%d%m%Y")
labor$Uhrzeit_Probenahme <- as.POSIXct(labor$Uhrzeit_Probenahme, format = "%H%M")
labor$DateTime <- paste(labor$Datum_Probenahme, labor$Uhrzeit_Probenahme)
duration()