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
library("lubridate")
library("dplyr")
install.packages("timetk")
library("timetk")

## Daten einlesen ####
aktuell <- read_delim("all_data_avg10min.csv", ",") #Dateinamen anpassen
labordaten <- read_delim("Labordaten_final.csv", ";") #Dateinamen anpassen
mikrobio <- read_delim("Mikrobio_01.csv", ";") #Dateinamen anpassen
august <- read_delim("2023-08-15_Kultivierungsdaten.csv", ",")

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
labor$Add[labor$Add > 0] <- 6
labor$Add2 <- labor$Glucose
labor$Add2[labor$Add2 > 0] <- 100000

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
ps_neu$Phase_pH[1:3195] <- 1
ps_neu$Phase_pH[3196:6356] <- 2

# neuer Dataframe mit Daten von August ####
ps_alt <- data.frame(DateTime = as.POSIXct(as.character(august$time.string), format = "%Y-%m-%d %H:%M:%OS"))

ps_alt <- ps_alt |> 
  mutate(par1 = august$PAR.1, # PAR Angaben in umol/m2/s
         par2 = august$PAR.2,
         pardiff = par2 - par1,
         temp = august$TEMPERATURE,
         pH = august$pH,
         daynight = as.factor(august$daynight),
         kW1 = august$PAR.1*2.19/10000, # Umrechnen PAR in kW nach XX
         kW2 = august$PAR.2*2.19/10000,
         W1 = august$PAR.1*2.19,
         W2 = august$PAR.2*2.19,
         truebung = august$TURBIDITY)
ps_alt$DateTime <- as.POSIXct(ps_alt$DateTime, format = "%Y-%m-%d %H:%M:%OS")

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
kombi2 <- full_join(bakterien, kombi, by = "DateTime", relationship = "many-to-many")


# Plots ####
Sys.setlocale("LC_TIME", "English")


## Plot Konzentration über Zeit mit log Skala #### Legende noch entfernen
kombi <- kombi |> 
  group_by(Phase)
ggplot(data = labor, aes(x=DateTime, y=Konzentration, col = Phase, group = Phase)) + 
  geom_line(show.legend = FALSE) +  
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() + 
  theme(legend.position = "none") +
  scale_y_continuous(trans = log10_trans()) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 week") +
  theme_classic() +
  labs(
    y = "Algae cell numbers per ml",
    x = ""
  )
ggsave("Wachstumsphasen_mit_Formel.jpeg", last_plot(), width = 16, height = 10, units = "cm")

##  Werte grösser als 4.5 als NA ####
kombi$pH2 <- kombi$pH
kombi$pH2[kombi$pH2 >= 4.3] <- NA

## Plot pH ####
ggplot(kombi, aes(x=DateTime, y=pH2)) +
  geom_line(lwd = 0.5) + 
  theme_classic() +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 week") +
  labs(
    x = "",
    y = "pH",
  )
ggsave("ph.jpeg", last_plot(), width = 16, height = 10, units = "cm")

## Berechnung pH ####
sd_pH <- sd(kombi$pH, na.rm = TRUE)
mw_pH <- mean(kombi$pH, na.rm = TRUE)
sd_pH2 <- sd(kombi$pH2, na.rm = TRUE)
mw_pH2 <- mean(kombi$pH2, na.rm = TRUE)

phase1 <- ps_neu[which(ps_neu$Phase_pH == 1),]
phase2 <- ps_neu[which(ps_neu$Phase_pH == 2),]
phase1$pH[phase1$pH >= 4.3] <- NA
phase2$pH[phase2$pH >= 4.3] <- NA

sd_pHp1 <- sd(phase1$pH, na.rm = TRUE)
mw_pHp1 <- mean(phase1$pH, na.rm = TRUE)
sd_pHp2 <- sd(phase2$pH, na.rm = TRUE)
mw_pHp2 <- mean(phase2$pH, na.rm = TRUE)

## Wachstum Trübung und Trockenmasse ####
ggplot() + 
  geom_line(data = kombi, aes(x=DateTime, y=truebung), color = "black", lwd = 0.5) +
  geom_point(data = kombi, aes(x=DateTime, y=Trockenmasse), color = "blue") +
  geom_point(data = kombi, aes(x = DateTime, y = Add), col = "red", shape = 6, size = 3) +
  scale_y_continuous(trans = log10_trans(),
                     name = "Turbidity [recalculated to g/l] and dry matter [g/l]") +
  theme_classic() +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 week") +
  labs(
    x = ""
  )
ggsave("Truebung_Trockenmasse.jpeg", last_plot(), width = 16, height = 10, units = "cm")

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

## Berechnung PAR - Tageszeit neue Daten ####
sum_ps <- summarise_by_time(
          ps_neu,
          DateTime,
          by = "day",
          week_start = NULL,
          kW = sum(kW2, na.rm = TRUE)
          )
sum_ps$kWh <- sum_ps$kW
sum_ps$kWh <- sum_ps$kW/24
sum_ps$Wh <- sum_ps$kWh*1000

sum_temp <- summarise_by_time(
          ps_neu,
          DateTime,
          by = "day",
          week_start = NULL,
          mean_temp = mean(temp, na.rm = TRUE)
          )

sum_temp_cut <- sum_temp[1:43,]

## Berechnung PAR - Tageszeit alte Daten ####
sum_ps_alt <- summarise_by_time(
  ps_alt,
  DateTime,
  by = "day",
  week_start = NULL,
  kW = sum(kW2, na.rm = TRUE)
)
sum_ps_alt$kWh <- sum_ps_alt$kW
sum_ps_alt$kWh <- sum_ps_alt$kW/24
sum_ps_alt$Wh <- sum_ps_alt$kWh*1000

sum_ps_alt <- sum_ps_alt[1:29,]

sum_temp_alt <- summarise_by_time(
  ps_alt,
  DateTime,
  by = "day",
  week_start = NULL,
  mean_temp = mean(temp, na.rm = TRUE)
)
sum_temp_alt <- sum_temp_alt[1:29,]
ps_alt$truebung[ps_alt$truebung >= 15] <- NA

## PAR und Wachstum neue Daten #### 
coeff = 0.1
ggplot(data = sum_ps, aes(x = DateTime)) +
  geom_line(aes(y = Wh), color = "blue" ) + 
  geom_line(data = sum_temp_cut, aes(y = mean_temp/coeff), color = "red") +
  scale_y_continuous(
    name = "Wh",  
    sec.axis = sec_axis(~.*coeff, name = "Temperature")) +
  theme_classic() +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 week") +
  labs(
    x = "")

ggsave("PAR_neu.jpeg", last_plot(), width = 16, height = 10, units = "cm")

ggplot() + 
  geom_line(data = kombi, aes(x=DateTime, y=truebung), color = "black", lwd = 0.5) +
  theme_classic() +
  scale_y_continuous(trans = log10_trans()) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 week") +
  labs(
    x = "",
    y = "Turbidity recalculated to g per ml")

ggsave("PAR_neu_Turbidity.jpeg", last_plot(), width = 16, height = 10, units = "cm")

## PAR und Wachstum alte Daten #### 
coeff = 0.1
ggplot(data = sum_ps_alt, aes(x = DateTime)) +
  geom_line(aes(y = Wh), color = "blue" ) + 
  geom_line(data = sum_temp_alt, aes(y = mean_temp/coeff), color = "red") +
  scale_y_continuous(
    name = "Wh",  
    sec.axis = sec_axis(~.*coeff, name = "Temperature")) +
  theme_classic() +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 week") +
  labs(
    x = "")
ggsave("PAR_alt.jpeg", last_plot(), width = 16, height = 10, units = "cm")

ggplot() + 
  geom_line(data = ps_alt, aes(x=DateTime, y=truebung), color = "black", lwd = 0.5) +
  theme_classic() +
  scale_y_continuous(trans = log10_trans()) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 week") +
  labs(
    x = "",
    y = "Turbidity recalculated to g per ml")
ggsave("PAR_alt_Turbidity.jpeg", last_plot(), width = 16, height = 10, units = "cm")

## Plot Bakterien und Wachstum über Zeit ##
### zu klären: Plot funktioniert noch nicht
ggplot() +
  geom_path(data = kombi2, aes(x = DateTime, y = Bakterien_ml), color = "blue" ) + 
  geom_point(data = kombi, aes(x = DateTime, y = Add2), col = "red", shape = 6, size = 3) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 week") +
  scale_y_continuous(trans = log10_trans(), limits = c(1e+05,1e+07)) +
  theme_classic() +
  labs(
    x = "",
    y = "CFU per ml")
ggsave("Bakterien.jpeg", last_plot(), width = 16, height = 10, units = "cm")

## Berechnungen Bakterien ####
mw_bakt <- mean(kombi2$Bakterien_ml, na.rm = TRUE)
sem_bakt <- std.error(kombi2$Bakterien_ml, na.rm = TRUE)


## Berechnung Mittelwert und SEM für Zellzahl und Trockenmasse ####
mw_zz <- mean(kombi$Konzentration, na.rm = TRUE)
mw_tm <- mean(kombi$Trockenmasse, na.rm = TRUE)
sem_zz <- std.error(kombi$Konzentration, na.rm = TRUE)
sem_tm <- std.error(kombi$Trockenmasse, na.rm = TRUE)

