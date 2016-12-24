##**************##
### CACHINATOR ###
##**************##

#### postavi radni direktorij ####
setwd("~/Projekti/dev/cashinator")

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
source("./scripts/funkcije.R")
source("./scripts/pocetna_stanja.R")
source("./scripts/rba_kune.R")
source("./scripts/rba_eur.R")
source("./scripts/rba_tecaj.R")
source("./scripts/zaba_kune.R")
source("./scripts/zaba_eur.R")
source("./scripts/zaba_tecaj.R")
source("./scripts/rba.R")
source("./scripts/zaba.R")
source("./scripts/erste_kune.R")
source("./scripts/integrator.R")
# ubaciti kasnije sourcing ostalih skripti
