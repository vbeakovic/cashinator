if (file.exists("./varijable/erste.Rdata")) {
        load("./varijable/erste.Rdata")
} else {
        ### izvuci direktorij sa ERSTE prometima
        ersteKunePrometi <- pathinator(pathPrometi, "erste")  
        
        ### učitaj promete i kreiraj data frame za svaku godinu        
        ersteKuneLista <- vector("list") 
        ersteKuneGodine <- vector("character", length = length(ersteKunePrometi))
        for (i in seq_along(ersteKunePrometi)) {
                ersteKuneLista[[i]] <- read.erste(putanja = ersteKunePrometi[i])
                ersteKuneGodine[i] <- gsub("(.*/)(\\d{4}).*$", "\\2", ersteKunePrometi[i])
        }
        names(ersteKuneLista) <- ersteKuneGodine

        ### počisti podatke
        for (i in seq_along(ersteKuneLista)) {
                ersteKuneLista[[i]] <- ersteKuneCleanator(ersteKuneLista[[i]])
        }
        ersteKune <- do.call("rbind", ersteKuneLista)
        
        ### ubaci početno stanje
        pocetno_stanje <- c("2014-12-31", "Početno stanje", "Početno stanje", "pocetno_stanje", NA)
        ersteKune <- rbind(ersteKune, pocetno_stanje)
        ersteKune$iznos[ersteKune$datum_valute == "2014-12-31"] <- 0
        ersteKune$iznos <- as.numeric(ersteKune$iznos)
        ersteKune <- arrange(ersteKune, datum_valute)
        

        
        ### dodaj stupac sa stanjem računa
        ersteKune <- mutate(ersteKune, stanje = cumsum(iznos))
        ersteKune$stanje <- ersteKune$stanje + pocetna_stanja$hrk[3]
        
        ### ubacivanje datuma koji fale
        lista_datuma <- seq.Date(ersteKune$datum_valute[1], 
                                 ersteKune$datum_valute[NROW(ersteKune)], by = "day")
        lista_datuma_df <- data.frame(datum_valute = lista_datuma)
        ersteKune <- full_join(lista_datuma_df, ersteKune)
        
        ### popuni vrijednosti koje fale sa redom prije
        ersteKune$stanje <- na.locf(ersteKune$stanje)
        
        ### za dane bez prometa ubaci 0 u promet
        ersteKune$iznos[is.na(ersteKune$iznos)] <- 0
        
        ### za dan bez prometa označi "bez_transakcije"
        ersteKune$vrsta[is.na(ersteKune$vrsta)] <- "bez_transakcije"
        ersteKune$primatelj_platitelj[is.na(ersteKune$primatelj_platitelj)] <- "Bez transakcije"
        ersteKune$vrsta_prometa[is.na(ersteKune$vrsta_prometa)] <- "Bez transakcije"
        
        
        
        #### Prebaci u local data frame ####
        ersteKune <- tbl_df(ersteKune)
        
        #### Finalni erste data frame ####
        erste <- ersteKune[c(1,2,3,4, 5)]
        names(erste) <- names(rba)
        erste$iznos[1] <- pocetna_stanja$hrk[3]
        save(erste, file = "./varijable/erste.Rdata")
}