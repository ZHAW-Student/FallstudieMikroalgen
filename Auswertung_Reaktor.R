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

ps_neu <- data.frame(DateTime = as.POSIXct(as.character(ps$time.string), format = "%Y-%m-%d %H:%M:%OS"))

ps_neu <- ps_neu |> 
  mutate(par1 = ps$PAR.1, 
         par2 = ps$PAR.2,
         pardiff = par2 - par1,
         temp = ps$TEMPERATURE,
         pH = ps$pH,
         daynight = as.factor(ps$daynight))

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

  