# Učitaj promete
if (file.exists("./varijable/rba_eur.Rdata")) {
        load("./varijable/rba_eur.Rdata")
} else {
        ### izvuci direktorij sa RBA prometima
        rbaEurPrometi <- pathinator(pathPrometi, "rba")
        
        ### učitaj promete i kreiraj data frame za svaku godinu        
        #rbaEurLista <- vector("list") 
        rbaEurLista <- list() 
        #rbaEurGodine <- vector("character", length = length(rbaEurPrometi))
        rbaEurGodine <- character(length = length(rbaEurPrometi))
        for (i in seq_along(rbaEurPrometi)) {
                rbaEurLista[[i]] <- read.rba(putanja = rbaEurPrometi[i], valuta = "eur")
                rbaEurGodine[i] <- gsub("(.*/)(\\d{4}).*$", "\\2", rbaEurPrometi[i])
        }
        names(rbaEurLista) <- rbaEurGodine
        
        ### počisti podatke
        for (i in seq_along(rbaEurLista)) {
                rbaEurLista[[i]] <- rbaEurCleanator(rbaEurLista[[i]])
        }
        rbaEur <- do.call("rbind", rbaEurLista)
        
        
        ### ubaci početno stanje
        pocetno_stanje <- c("2014-12-31", "2014-12-31", "Početno stanje", "Početno stanje", "pocetno_stanje", NA)
        rbaEur <- rbind(rbaEur, pocetno_stanje)
        rbaEur$iznos[rbaEur$datum_valute == "2014-12-31"] <- 0 
        rbaEur$iznos <- as.numeric(rbaEur$iznos)
        rbaEur <- arrange(rbaEur, datum_valute)
        
        ### dodaj stupac sa stanjem računa
        rbaEur <- mutate(rbaEur, stanje = cumsum(iznos))
        rbaEur$stanje <- rbaEur$stanje + pocetna_stanja$eur[1]
        
        ### ubacivanje datuma koji fale
        lista_datuma <- seq.Date(rbaEur$datum_valute[1], rbaEur$datum_valute[NROW(rbaEur)], by = "day")
        lista_datuma_df <- data.frame(datum_valute = lista_datuma)
        rbaEur <- full_join(lista_datuma_df, rbaEur)
        
        ### popuni vrijednosti koje fale sa redom prije
        rbaEur$stanje <- na.locf(rbaEur$stanje)
        
        ### za dane bez prometa ubaci 0 u promet
        rbaEur$iznos[is.na(rbaEur$iznos)] <- 0
        
        ### za dan bez prometa označi "bez_transakcije"
        rbaEur$vrsta[is.na(rbaEur$vrsta)] <- "bez_transakcije"
        rbaEur$primatelj_platitelj[is.na(rbaEur$primatelj_platitelj)] <- "Bez transakcije"
        rbaEur$vrsta_prometa[is.na(rbaEur$vrsta_prometa)] <- "Bez transakcije"
        rbaEur$datum_knjizenja[is.na(rbaEur$datum_knjizenja)] <- rbaEur$datum_valute[is.na(rbaEur$datum_knjizenja)]
        
        
        
        #### Prebaci u local data frame ####
        rbaEur <- tbl_df(rbaEur)
        
        #### Malo uredi
        rbaEur <- select(rbaEur, -(datum_knjizenja))
        save(rbaEur, file = "./varijable/rba_eur.Rdata")
}
