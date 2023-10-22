# Auswertungsskript Fallstudie Mikroalgen ####
# Vorbereitung ####
## benötigte Packages installieren ####
install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("lubridate")

## benötigte Packages laden ####
library("tidyverse")
library("tidyr")
library("ggplot2")
library("readr")
library("lubridate")

## Daten einlesen ####
reactor <- read_delim("daten.csv", ";") #Dateinamen anpassen
labor <- read_delim("Labordaten.csv", ";") #Dateinamen anpassen

# Daten formatieren und anpassen ####
## Zeitformat anpassen und zusammenfügen ####
labor$Datum_Probenahme <- as.POSIXct(labor$Datum_Probenahme, format = "%d%m%Y", tz = "UTC")
labor$Uhrzeit_Probenahme <- as.POSIXct(labor$Uhrzeit_Probenahme, format = "%H%M", tz = "UTC")
labor$DateTime <- paste(labor$Datum_Probenahme, labor$Uhrzeit_Probenahme)

## Faktoren erstellen ####

