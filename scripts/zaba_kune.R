# Učitaj promete
if (file.exists("./varijable/zaba_kune.Rdata")) {
        load("./varijable/zaba_kune.Rdata")
} else {
        ### izvuci direktorij sa ZABA prometima
        zabaKunePrometi <- pathinator(pathPrometi, "zaba")
        
        ### učitaj promete i kreiraj data frame za svaku godinu        
        zabaKuneLista <- vector("list") 
        zabaKuneGodine <- vector("character", length = length(zabaKunePrometi))
        for (i in seq_along(zabaKunePrometi)) {
                zabaKuneLista[[i]] <- read.zaba(putanja = zabaKunePrometi[i], valuta = "kune")
                zabaKuneGodine[i] <- gsub("(.*/)(\\d{4}).*$", "\\2", zabaKunePrometi[i])
        }
        names(zabaKuneLista) <- zabaKuneGodine
        
        ### počisti podatke
        for (i in seq_along(zabaKuneLista)) {
                zabaKuneLista[[i]] <- zabaKuneCleanator(zabaKuneLista[[i]])
        }
        zabaKune <- do.call("rbind", zabaKuneLista)
        
        
        ### ubaci početno stanje
        pocetno_stanje <- c("2014-12-31", "Početno stanje", "Početno stanje", "pocetno_stanje", NA)
        zabaKune <- rbind(zabaKune, pocetno_stanje)
        zabaKune$iznos[zabaKune$datum_valute == "2014-12-31"] <- 0 
        zabaKune$iznos <- as.numeric(zabaKune$iznos)
        zabaKune <- arrange(zabaKune, datum_valute)
        
        ### dodaj stupac sa stanjem računa
        zabaKune <- mutate(zabaKune, stanje = cumsum(iznos))
        zabaKune$stanje <- zabaKune$stanje + pocetna_stanja$hrk[2]
        
        ### ubacivanje datuma koji fale
        lista_datuma <- seq.Date(zabaKune$datum_valute[1], zabaKune$datum_valute[NROW(zabaKune)], by = "day")
        lista_datuma_df <- data.frame(datum_valute = lista_datuma)
        zabaKune <- full_join(lista_datuma_df, zabaKune)
        
        ### popuni vrijednosti koje fale sa redom prije
        zabaKune$stanje <- na.locf(zabaKune$stanje)
        
        ### za dane bez prometa ubaci 0 u promet
        zabaKune$iznos[is.na(zabaKune$iznos)] <- 0
        
        ### za dan bez prometa označi "bez_transakcije"
        zabaKune$vrsta[is.na(zabaKune$vrsta)] <- "bez_transakcije"
        zabaKune$primatelj_platitelj[is.na(zabaKune$primatelj_platitelj)] <- "Bez transakcije"
        zabaKune$vrsta_prometa[is.na(zabaKune$vrsta_prometa)] <- "Bez transakcije"
        
        #### Prebaci u local data frame ####
        zabaKune <- tbl_df(zabaKune)
        save(zabaKune, file = "./varijable/zaba_kune.Rdata")
}