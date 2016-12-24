##**************##
### CACHINATOR ###
##**************##

#### postavi radni direktorij ####
setwd("~/Projekti/dev/agenor")

#### učitaj library-ije ####
library(XML)
library(gdata)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readODS)
library(scales)
library(plotly)
library(zoo)
library(tidyr)


#### konstante ####
pathPrometi <- "./prometi"
pathTecaji <- "./tecaji"

#### pozovi obrade prometa ####
source("./skripte/funkcije.R")
source("./skripte/pocetna_stanja.R")
source("./skripte/rba_kune.R")
source("./skripte/rba_eur.R")
source("./skripte/rba_tecaj.R")
source("./skripte/zaba_kune.R")
source("./skripte/zaba_eur.R")
source("./skripte/zaba_tecaj.R")
source("./skripte/rba.R")
source("./skripte/zaba.R")
source("./skripte/erste_kune.R")
source("./skripte/integrator.R")
