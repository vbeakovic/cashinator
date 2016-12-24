# Učitaj promete
if (file.exists("./varijable/zaba_eur.Rdata")) {
        load("./varijable/zaba_eur.Rdata")
} else {
        ### izvuci direktorij sa ZABA prometima
        zabaEurPrometi <- pathinator(pathPrometi, "zaba")
        
        ### učitaj promete i kreiraj data frame za svaku godinu        
        zabaEurLista <- vector("list") 
        zabaEurGodine <- vector("character", length = length(zabaEurPrometi))
        for (i in seq_along(zabaEurPrometi)) {
                zabaEurLista[[i]] <- read.zaba(putanja = zabaEurPrometi[i], valuta = "eur")
                zabaEurGodine[i] <- gsub("(.*/)(\\d{4}).*$", "\\2", zabaEurPrometi[i])
        }

        
        
        ### počisti podatke
        for (i in seq_along(zabaEurLista)) {
                zabaEurLista[[i]] <- zabaEurCleanator(zabaEurLista[[i]])
        }
        zabaEur <- do.call("rbind", zabaEurLista)        
        
        ### ubaci početno stanje
        pocetno_stanje <- c("2014-12-31", "Početno stanje", "Početno stanje", "pocetno_stanje", NA)
        zabaEur <- rbind(zabaEur, pocetno_stanje)
        zabaEur$iznos[zabaEur$datum_valute == "2014-12-31"] <- 0
        zabaEur$iznos <- as.numeric(zabaEur$iznos)
        zabaEur <- arrange(zabaEur, datum_valute)
        
        ### dodaj stupac sa stanjem računa
        zabaEur <- mutate(zabaEur, stanje = cumsum(iznos))
        zabaEur$stanje <- zabaEur$stanje + pocetna_stanja$eur[2]
        
        ### ubacivanje datuma koji fale
        lista_datuma <- seq.Date(zabaEur$datum_valute[1], zabaEur$datum_valute[NROW(zabaEur)], by = "day")
        lista_datuma_df <- data.frame(datum_valute = lista_datuma)
        zabaEur <- full_join(lista_datuma_df, zabaEur)
        
        ### popuni vrijednosti koje fale sa redom prije
        zabaEur$stanje <- na.locf(zabaEur$stanje)
        
        ### za dane bez prometa ubaci 0 u promet
        zabaEur$iznos[is.na(zabaEur$iznos)] <- 0
        
        ### za dan bez prometa označi "bez_transakcije"
        zabaEur$vrsta[is.na(zabaEur$vrsta)] <- "bez_transakcije"
        zabaEur$primatelj_platitelj[is.na(zabaEur$primatelj_platitelj)] <- "Bez transakcije"
        zabaEur$vrsta_prometa[is.na(zabaEur$vrsta_prometa)] <- "Bez transakcije"
        
        
        
        
        #### Prebaci u local data frame ####
        zabaEur <- tbl_df(zabaEur)
        save(zabaEur, file = "./varijable/zaba_eur.Rdata")
}