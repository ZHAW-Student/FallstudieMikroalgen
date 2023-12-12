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
library("plotrix")
library("naniar")


## Daten einlesen ####
aktuell <- read_delim("all_data_avg10min.csv", ",") #Dateinamen anpassen
labordaten <- read_delim("Labordaten_final.csv", ";") #Dateinamen anpassen
mikrobio <- read_delim("Mikrobio_01.csv", ";") #Dateinamen anpassen

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
         Glucose = labordaten$Glucose_mml_l,
         Phase = 0)
labor$Phase[1:12] <- 1
labor$Phase[13:18] <- 2
labor$Phase[19:22] <- 3
labor$Add <- labor$Glucose
labor$Add[labor$Add > 0] <- 0.03

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
         W1 = aktuell$PAR.1*2.19,
         W2 = aktuell$PAR.2*2.19,
         truebung = aktuell$TURBIDITY)
ps_neu$DateTime <- as.POSIXct(ps_neu$DateTime, format = "%Y-%m-%d %H:%M:%OS")

# neuen Dataframe erstellen mit den Mikrobiodaten aus dem csv, die uns weiter interessieren
## noch zu klären: Masseinheit überall gleich? Hier pro L
bakterien <- data.frame(Datum= as.character(mikrobio$Datum_Probenahme))
bakterien <- bakterien |> 
  mutate(Zeit = as.character(mikrobio$Zeitpunkt_Probenahme),
         DatumZeit = as.character(paste(Datum, Zeit)),
         DateTime = as.POSIXct(as.character(DatumZeit), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC"),
         Bakterien_l = as.numeric(mikrobio$Mittelwert_L),
         Bakterien_ml = as.numeric(Bakterien_l/1000),
         log10Bakt = log10(Bakterien_l)
)
bakterien2 <- bakterien[2:10,]
         
# Datensätze labor & pe_neu über den DateTime kombinieren
kombi <- full_join(labor, ps_neu, by = "DateTime")

# Kombitabelle anpassen
kombi <- kombi |> 
  group_by(daynight) |> 
  mutate(kWdiff = kW2 - kW1)

# Werte vor Start und nach Ende des Experiments wegschneiden ####
kombi <- kombi[which(kombi$DateTime >= "2023-10-02 12:00:00"),]
kombi <- kombi[which(kombi$DateTime <= "2023-11-13 12:00:00" ),]

# Datensätze bakterien & kombi über den DateTime kombinieren
kombi2 <- full_join(bakterien2, kombi, by = "DateTime", relationship = "many-to-many")


# Plots ####
Sys.setlocale("LC_TIME", "English")


## Plot Konzentration über Zeit mit log Skala ####
kombi <- kombi |> 
  group_by(Phase)
ggplot(data = labor, aes(x=DateTime, y=Konzentration, col = Phase, group = Phase)) + 
  geom_line(show.legend = FALSE) +  
  stat_poly_line() +
  #stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() + 
  theme(legend.position = "none") +
  scale_y_continuous(trans = log10_trans()) +
  theme_classic() +
  labs(
    y = "Algae cell numbers per ml",
    x = ""
  )

##  Werte grösser als 4.5 als NA ####
kombi$pH2 <- kombi$pH
kombi$pH2[kombi$pH2 >= 4.3] <- NA

## Plot pH ####
ggplot(kombi, aes(x=DateTime, y=pH2)) +
  geom_line(lwd = 0.5) + 
  theme_classic() +
  labs(
    x = "",
    y = "pH",
  )

## Berechnung pH ####
sd_pH <- sd(kombi$pH, na.rm = TRUE)
mw_pH <- mean(kombi$pH, na.rm = TRUE)
sd_pH2 <- sd(kombi$pH2, na.rm = TRUE)
mw_pH2 <- mean(kombi$pH2, na.rm = TRUE)


## Wachstum Trübung und Trockenmasse ####
ggplot() + 
  geom_line(data = kombi, aes(x=DateTime, y=truebung), color = "black", lwd = 0.5) +
  geom_point(data = kombi, aes(x=DateTime, y=Trockenmasse), color = "blue") +
  geom_point(data = kombi, aes(x = DateTime, y = Add), col = "red", shape = 6, size = 3) +
  scale_y_continuous(trans = log10_trans(),
    name = "Turbidity [recalculated to g/l]", 
    sec.axis = sec_axis(~., name = "dry matter [g/l]")) +
  theme_classic() +
  labs(
    x = ""
  )
### 
ggplot(data = kombi, aes(x=DateTime, y=truebung), color = "black", lwd = 0.5) + 
  geom_line() +
  geom_point(data = kombi, aes(x=DateTime, y=Trockenmasse), color = "blue") +
  geom_point(data = kombi, aes(x = DateTime, y = Add), col = "red", shape = 6, size = 3)
  theme_classic() +
  labs(
    x = "",
    y = "g/l"
  )

scale_fill_discrete(name="Experimental\nCondition",
                    breaks=c("ctrl", "trt1", "trt2"),
                    labels=c("Control", "Treatment 1", "Treatment 2"))

## Temp und PAR ####
coeff = 10
ggplot(data = kombi, aes(x = DateTime)) +
  geom_line(aes(y = kombi$temp), color = "red" ) + 
  geom_line(aes(y = W2/coeff), color = "blue") +
  scale_y_continuous(
    name = "Temp",  
    sec.axis = sec_axis(~.*coeff, name = "Watt")) +
  theme_classic() +
  labs(
    x = "Datum",
    title = "Temperatur und Strahlung")


## PAR und Wachstum #### 
ggplot(data = kombi, aes(x = DateTime)) +
  geom_line(aes(y = kW2), color = "red" ) + 
  geom_line(aes(y = truebung), color = "blue") +
  facet_wrap(vars(kW2, truebung))

kombi4 <- kombi[which(kombi$daynight == 1 ),]  
  
coeff = 10
ggplot(data = kombi4, aes(x = DateTime)) +
  geom_line(aes(y = kombi4$kW2), color = "red" ) + 
  geom_line(aes(y = truebung/coeff), color = "blue") +
  scale_y_continuous(
    name = "kW",  
    sec.axis = sec_axis(~.*coeff, name = "Turbidity [recalculated to g/l]")) +
  theme_classic() +
  geom_smooth(aes(y = kombi4$kW2, color = "red")) +
  labs(
    x = "")



## Plot Bakterien und Wachstum über Zeit ##
### zu klären: Plot funktioniert noch nicht
ggplot() +
  geom_path(data = kombi2, aes(x = DateTime, y = log10Bakt), color = "red" ) + 
  geom_path(data = kombi2, aes(x = DateTime, y = log10Konz), color = "blue") +
  theme_classic()

## Berechnung Mittelwert und SEM für Zellzahl und Trockenmasse ####
mw_zz <- mean(kombi$Konzentration, na.rm = TRUE)
mw_tm <- mean(kombi$Trockenmasse, na.rm = TRUE)
sem_zz <- std.error(kombi$Konzentration, na.rm = TRUE)
sem_tm <- std.error(kombi$Trockenmasse, na.rm = TRUE)

## Berechnungen pH ####
