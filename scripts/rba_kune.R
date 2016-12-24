# Učitaj promete
#### RBA prometi u kunama ####
if (file.exists("./varijable/rba_kune.Rdata")) {
   load("./varijable/rba_kune.Rdata")
} else {
        ### izvuci direktorij sa RBA prometima
        rbaKunePrometi <- pathinator(pathPrometi, "rba")
        
        ### učitaj promete i kreiraj data frame za svaku godinu        
        #rbaKuneLista <- vector("list") 
        rbaKuneLista <- list()
        #rbaKuneGodine <- vector("character", length = length(rbaKunePrometi))
        rbaKuneGodine <- character(length = length(rbaKunePrometi))
        for (i in seq_along(rbaKunePrometi)) {
                rbaKuneLista[[i]] <- read.rba(putanja = rbaKunePrometi[i], valuta = "kune")
                rbaKuneGodine[i] <- gsub("(.*/)(\\d{4}).*$", "\\2", rbaKunePrometi[i])
        }

        names(rbaKuneLista) <- rbaKuneGodine
        
       
        ### počisti podatke
        for (i in seq_along(rbaKuneLista)) {
                rbaKuneLista[[i]] <- rbaKuneCleanator(rbaKuneLista[[i]])
        }
        rbaKune <- do.call("rbind", rbaKuneLista)

        
        ### ubaci početno stanje
        pocetno_stanje <- c("2014-12-31", "2014-12-31", "Početno stanje", "Početno stanje", "pocetno_stanje", NA)
        rbaKune <- rbind(rbaKune, pocetno_stanje)
        rbaKune$iznos[rbaKune$datum_valute == "2014-12-31"] <- 0 
        rbaKune$iznos <- as.numeric(rbaKune$iznos)
        rbaKune <- arrange(rbaKune, datum_valute)
        
        ### dodaj stupac sa stanjem računa
        rbaKune <- mutate(rbaKune, stanje = cumsum(iznos))
        rbaKune$stanje <- rbaKune$stanje + pocetna_stanja$hrk[1]
        
        ### ubacivanje datuma koji fale
        lista_datuma <- seq.Date(rbaKune$datum_valute[1], rbaKune$datum_valute[NROW(rbaKune)], by = "day")
        lista_datuma_df <- data.frame(datum_valute = lista_datuma)
        rbaKune <- full_join(lista_datuma_df, rbaKune)
        
        ### popuni vrijednosti koje fale sa redom prije
        rbaKune$stanje <- na.locf(rbaKune$stanje)
        
        ### za dane bez prometa ubaci 0 u promet
        rbaKune$iznos[is.na(rbaKune$iznos)] <- 0
        
        ### za dan bez prometa označi "bez_transakcije"
        rbaKune$vrsta[is.na(rbaKune$vrsta)] <- "bez_transakcije"
        rbaKune$primatelj_platitelj[is.na(rbaKune$primatelj_platitelj)] <- "Bez transakcije"
        rbaKune$vrsta_prometa[is.na(rbaKune$vrsta_prometa)] <- "Bez transakcije"
        rbaKune$datum_knjizenja[is.na(rbaKune$datum_knjizenja)] <- rbaKune$datum_valute[is.na(rbaKune$datum_knjizenja)]
        
        
        #### Prebaci u local data frame ####
        rbaKune <- tbl_df(rbaKune)
        
        #### Malo uredi
        rbaKune <- select(rbaKune, -(datum_knjizenja))
        save(rbaKune, file = "./varijable/rba_kune.Rdata")
}
