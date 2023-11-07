# Auswertungsskript Fallstudie Mikroalgen ####
# Vorbereitung ####
## benötigte Packages installieren ####
install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggpmisc")
install.packages("readr")
install.packages("lubridate")

## benötigte Packages laden ####
library("tidyverse")
library("tidyr")
library("ggplot2")
library("ggpmisc")
library("readr")
library("lubridate")

## Daten einlesen ####
reactor <- read_delim("daten.csv", ";") #Dateinamen anpassen
labordaten <- read_delim("Labordaten6.csv", ";") #Dateinamen anpassen

# neuen Dataframe erstellen mit den Daten aus dem csv, die uns weiter interessieren
labor <- data.frame(Datum= as.character(labordaten$Datum_Probenahme))

labor <- labor |> 
  mutate(Zeit = as.character(labordaten$Zeitpunkt_Probenahme),
         DatumZeit = as.character(paste(Datum, Zeit)),
         DateTime = as.POSIXct(as.character(DatumZeit), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC"),
         Konzentration = as.numeric(labordaten$Konzentration_Algen_unbestimmt),
         Trockenmasse = as.numeric(labordaten$Trockenmasse_gL),
         log10Konz = log10(Konzentration),
         Tage = labordaten$Tag,
         Glucose = labordaten$Glucose_mml_l)



ggplot(labor, aes(x = Tage,y= Konzentration)) + geom_line()
ggplot(labor, aes(x=Tage, y=log10Konz)) + geom_point() + geom_line() +
  stat_smooth(method = "lm", formula = y~x, geom = "smooth")
  stat_regline_equation

## Wachstum nach Tag plotten mit Regressionsgeraden und Formel ####
ggplot(labor, aes(x=Tage, y=log10Konz)) + geom_point() + geom_line() +  
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
    labs(
      x = "Anzahl Tage seit Experimentbeginn",
      y = "log10 Algenkonzentration",
      title = "Algenkonzentration über Zeit",
      subtitle = "02. Oktober bis 14. November 2023"
    )

ggplot(labor, aes(x=Tage, y=Konzentration)) + geom_point() + geom_line() +  
    geom_point() +
    labs(
      x = "Anzahl Tage seit Experimentbeginn",
      y = "Algenkonzentration",
      title = "Algenkonzentration über Zeit",
      subtitle = "02. Oktober bis 14. November 2023"
    )

## Plot Trockenmasse ####
ggplot(labor, aes(x=Tage, y=Trockenmasse)) + geom_point() + geom_line() +  
  geom_point() +
  labs(
    x = "Anzahl Tage seit Experimentbeginn",
    y = "Trockenmasse in g/l",
    title = "Trockenmasse über Zeit",
    subtitle = "02. Oktober bis 14. November 2023"
  )

## Plot Glucose ####  
ggplot(labor, aes(x=Tage, y=Glucose)) + geom_point() + geom_line() +  
  geom_point() +
  labs(
    x = "Anzahl Tage seit Experimentbeginn",
    y = "Glucose in g",
    title = "Glucose über Zeit",
    subtitle = "02. Oktober bis 14. November 2023"
  )
