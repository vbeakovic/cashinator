if (file.exists("./varijable/cashflow.Rdata")) {
        load("./varijable/cashflow.Rdata")
} else {
        # spajanje prometa sa sve tri banke
        #cashflow <- rbind(rba, zaba)
        #cashflow <- rbind(cashflow, erste)
        cashflow <- do.call(rbind, list(rba, zaba, erste))
        
        # sort po datumu
        cashflow <- arrange(cashflow, datum)
        
        # generiranje stanja
        cashflow$stanje <- cumsum(cashflow$iznos)
        
        # generiranje integriranog opisa
        cashflow$opis <- paste(cashflow$vrsta_prometa, 
                               cashflow$primatelj_platitelj, 
                               sep = " ")
        cashflow$opis <- gsub("\\s+"," ", cashflow$opis)
        cashflow$opis <- toupper(cashflow$opis)
        
        # dodatak varijable za finu i grubu kategorizaciju
        cashflow$subjekti <- as.character("")
        cashflow$kategorija <- as.character("")
        
        # ispis opisa u csv za ručnu obradu
        write.csv(x = unique(cashflow$opis), file = "./pomocni/opisi2016.csv")
        
        
        
        subjekti_kategorija <- read.csv("./pomocni/kategorije.csv", header = FALSE, stringsAsFactors = FALSE)
        
        # pretvorba u factore
        subjekti_kategorija$V3 <- as.factor(subjekti_kategorija$V3)
        subjekti_kategorija$V4 <- as.factor(subjekti_kategorija$V4)
        
        # ispuni finu kategorizaciju
        levels_subjekti <- levels(subjekti_kategorija$V3)
        for (i in seq_along(levels_subjekti)) {
                tmp <- levels_subjekti[i]
                desc <- subjekti_kategorija$V2[subjekti_kategorija$V3 == tmp]
                cashflow$subjekti[cashflow$opis %in% desc] <- tmp
        }
        cashflow$subjekti <- as.factor(cashflow$subjekti)
        
        levels_kategorije <- levels(subjekti_kategorija$V4)
        for (i in seq_along(levels_kategorije)) {
                tmp <- levels_kategorije[i]
                desc <- subjekti_kategorija$V2[subjekti_kategorija$V4 == tmp]
                cashflow$kategorija[cashflow$opis %in% desc] <- tmp
        }
        cashflow$kategorija <- as.factor(cashflow$kategorija)
        cashflow <- cashflow[5:NROW(cashflow), ]
        cashflow$iznos[1] <- 0 
        
        save(cashflow, file = "./varijable/cashflow.Rdata")
}

