# Auswertungsskript Fallstudie Mikroalgen ####
# Vorbereitung ####
## benötigte Packages laden ####
library("tidyverse")
library("tidyr")
library("ggplot2")
library("ggpmisc")
library("readr")
library("lubridate")
library("scales")

## Daten einlesen ####
aktuell <- read_delim("all_data_avg10min.csv", ",") #Dateinamen anpassen
labordaten <- read_delim("Labordaten_final.csv", ";") #Dateinamen anpassen

# neuen Dataframe erstellen mit den Labordaten aus dem csv, die uns weiter interessieren
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

# neuen Dataframe erstellen mit den Reaktordatendaten aus dem csv, die uns weiter interessieren
ps_neu <- data.frame(DateTime = as.POSIXct(as.character(aktuell$time.string), format = "%Y-%m-%d %H:%M:%OS"))

ps_neu <- ps_neu |> 
  mutate(par1 = aktuell$PAR.1, # PAR Angaben in umol/m2/s
         par2 = aktuell$PAR.2,
         pardiff = par2 - par1,
         temp = aktuell$TEMPERATURE,
         pH = aktuell$pH,
         daynight = as.factor(aktuell$daynight),
         kW1 = aktuell$PAR.1*2.19/10000, # Umrechnen PAR in kW nach XX
         kW2 = aktuell$PAR.2*2.19/10000,
         W1 = ps_neu$kW1*1000,
         W2 = ps_neu$kW2*1000,
         truebung = aktuell$TURBIDITY)
ps_neu$DateTime <- as.POSIXct(ps_neu$DateTime, format = "%Y-%m-%d %H:%M:%OS")

# beide Datensätze über den DateTime kombinieren
kombi <- full_join(labor, ps_neu, by = "DateTime")

# Kombitabelle anpassen
kombi <- kombi |> 
  group_by(daynight) |> 
  mutate(kWdiff = kW2 - kW1)

# Werte vor Start und nach Ende des Experiments wegschneiden ####
kombi <- kombi[which(kombi$DateTime >= "2023-10-02 12:00:00"),]
kombi <- kombi[which(kombi$DateTime <= "2023-11-13 12:00:00" ),]


# Plots ####
theme_classic()

## Plot log10 Konzentration über Zeit ####
ggplot(labor, aes(x=DateTime, y=log10Konz)) + geom_point() + geom_line() +  
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  theme_classic() +
    labs(
      x = "Datum",
      y = "log10 Anzahl Algen pro ml",
      title = "Algenkonzentration über Zeit"
    )

## Plot Konzentration über Zeit ####
ggplot(labor, aes(x=DateTime, y=Konzentration)) + geom_point() + geom_line() +  
    geom_point() +
    theme_classic() +
    labs(
      x = "Datum",
      y = "Anzahl Algen pro ml",
      title = "Algenkonzentration über Zeit"
    )

## Plot Konzentration über Zeit mit log Skala ####
ggplot(labor, aes(x=DateTime, y=Konzentration)) + geom_point() + geom_line() +  
  scale_y_continuous(trans = log10_trans()) +
  geom_point() +
  theme_classic() +
  labs(
    x = "Datum",
    y = "Anzahl Algen pro ml",
    title = "Algenkonzentration über Zeit"
    )

## Plot Trockenmasse ####
ggplot(labor, aes(x=DateTime, y=Trockenmasse)) + geom_point() + geom_line() +  
  geom_point() +
  theme_classic() +
  labs(
    x = "Datum",
    y = "Trockenmasse in g/l",
    title = "Trockenmasse über Zeit"
  )

## Plot Glucose ####  
ggplot(labor, aes(x=DateTime, y=Glucose)) + geom_point() + geom_line() +  
  geom_point() +
  theme_classic() +
  labs(
    x = "Datum",
    y = "Glucose mmol/l",
    title = "Glucose über Zeit"
  )

## Plot pH ####
ggplot(kombi, aes(x=DateTime, y=pH)) + geom_point() + geom_line() +  
  geom_point() +
  theme_classic() +
  labs(
    x = "Datum",
    y = "pH",
    title = "pH über Zeit"
  )

## Plot Strahlung ####
ggplot(kombi, aes(temp, kW2, color = daynight)) + 
  geom_point() +
  theme_classic()

## Wachstum ####
ggplot() + 
  geom_line(data = kombi, aes(x=DateTime, y=truebung), color = "black", lwd = 0.5) +
  geom_point(data = kombi, aes(x=DateTime, y=Trockenmasse), color = "blue") +
  scale_y_continuous(trans = log10_trans(),
    name = "Trübung [umgerechnet auf g/l]", 
    sec.axis = sec_axis(~., name = "Trockenmasse [g/l]")) +
  theme_classic() +
  labs(
    x = "Datum",
    title = "Wachstum über Zeit"
  )

## Temp und PAR ####
ggplot() +
  geom_line(data = kombi, aes(x = DateTime, y = temp), color = "red" ) + 
  geom_line(data = kombi, aes(x = DateTime, y = (W2)), color = "blue") +
  scale_y_continuous(
    name = "Temp",  
    sec.axis = sec_axis(~., name = "Watt")) +
  theme_classic() +
  labs(
    x = "Datum",
    title = "Temperatur und Strahlung"
  )
