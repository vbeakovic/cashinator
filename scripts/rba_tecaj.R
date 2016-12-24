if (file.exists("./varijable/rba_tecaj.Rdata")) {
        load("./varijable/rba_tecaj.Rdata")
} else {
        ### izvuci direktorij sa RBA tečajima
        rbaTecaji <- pathinator(pathTecaji, "rba")
        
        ### učitaj promete i kreiraj data frame za svaku godinu        
        rbaTecajiLista <- vector("list") 
        rbaTecajiGodine <- vector("character", length = length(rbaTecaji))
        for (i in seq_along(rbaTecaji)) {
                rbaTecajiLista[[i]] <- read.tecaj.rba(putanja = rbaTecaji[i])
                rbaTecajiGodine[i] <- gsub("(.*/)(\\d{4}).*$", "\\2", rbaTecaji[i])
        }
        
        names(rbaTecajiLista) <- rbaTecajiGodine
        
        for (i in seq_along(rbaTecajiLista)) {
                rbaTecajiLista[[i]] <- rbaTecajCleanator(rbaTecajiLista[[i]])
        }
        rbaTecaj <- do.call("rbind", rbaTecajiLista)
        save(rbaTecaj, file = "./varijable/rba_tecaj.Rdata")
}


