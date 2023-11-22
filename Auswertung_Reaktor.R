# Reaktordaten ####
# Vorbereitung ####
## benötigte Packages installieren ####
install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggpmisc")
install.packages("readr")
install.packages("lubridate")
install.packages("ggpubr")

## benötigte Packages laden ####
library("tidyverse")
library("tidyr")
library("ggplot2")
library("ggpmisc")
library("readr")
library("lubridate")
library("ggpubr")

## Daten einlesen ####
aktuell <- read_delim("2023-10-02_Kultivierungsdaten.csv", ",")
ps <- read_delim("2023-08-15_Kultivierungsdaten.csv", ",")
sb <- read_delim("2023-09-19_Kultivierungsdaten.csv", ",")

ggplot(ps, aes(time.string, pH)) + geom_line()
ggplot(sb, aes(time.string, pH)) + geom_line()
ggplot(aktuell, aes(time.string, pH)) + geom_line()

ggplot(ps, aes(TEMPERATURE, PAR.1)) + 
  geom_point() +
  geom_smooth()

ps_neu <- data.frame(DateTime = as.POSIXct(as.character(aktuell$time.string), format = "%Y-%m-%d %H:%M:%OS"))

ps_neu <- ps_neu |> 
  mutate(par1 = aktuell$PAR.1, # PAR Angaben in umol/m2/s
         par2 = aktuell$PAR.2,
         pardiff = par2 - par1,
         temp = aktuell$TEMPERATURE,
         pH = aktuell$pH,
         daynight = as.factor(aktuell$daynight),
         kW1 = aktuell$PAR.1*2.19/10000, # Umrechnen PAR in kW nach XX
         kW2 = aktuell$PAR.2*2.19/10000)
ps_neu$DateTime <- as.POSIXct(ps_neu$DateTime, format = "%Y-%m-%d %H:%M:%OS")
typeof(ps_neu$DateTime)
typeof(strptime(ps_neu$DateTime, format = "%Y-%m-%d %H:%M:%S"))
labor <- read_delim("Labordaten6.csv")
labor <- labor |> 
  mutate(Zeit = as.character(labordaten$Zeitpunkt_Probenahme),
         Datum = as.character(labordaten$Datum_Probenahme),
         DatumZeit = as.character(paste(Datum, Zeit)),
         DateTime = as.POSIXct(as.character(DatumZeit), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC"))

kombi <- full_join(labor, ps_neu, by = "DateTime")

sum_ps_neu <- ps_neu |> 
  group_by(daynight)
  
ggplot(sum_ps_neu, aes(temp, par2, color = daynight)) + 
  geom_point() + 
  geom_smooth(col = "black") +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) 

ggscatter(sum_ps_neu, x = "temp", y = "par2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson" )

ggplot(sum_ps_neu, aes(temp, par1,  color = daynight)) + 
  geom_point() + 
  geom_smooth(col = "black") +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")))

ggscatter(sum_ps_neu, x = "temp", y = "par1", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson" )


ggplot(sum_ps_neu, aes(temp, pardiff, color = daynight)) + 
  geom_point() + 
  geom_smooth(col = "black") +
  stat_poly_line(geom = "smooth") +
  stat_poly_eq(use_label(c("eq", "R2")))

ggscatter(sum_ps_neu, x = "temp", y = "pardiff", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson" )

  