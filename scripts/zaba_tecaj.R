if (file.exists("./varijable/zaba_tecaj.Rdata")) {
        load("./varijable/zaba_tecaj.Rdata")
} else {
        #### Download tečajnih lista sa ZABA-e ####
        
        ### Izvuci putanje za download
        zabaTecaji <- pathinator(pathTecaji, "zaba")
        
        ### Izvuci godine za koje tražimo tečajne liste
        zabaTecajiGodine <- vector("character", length = length(zabaTecaji))
        zabaTecajiGodine <- sapply(zabaTecaji, function(tecaj) gsub("(.*/)(\\d{4}).*$", "\\2", tecaj))
        
        
        ### Indeks zadnje tečajne liste u godini
        zabaMaxTecajna <- c(251, 167)
        
        ### Petlja za download fileova
        for (i in seq_along(zabaTecaji)) {
                download.tecaj.zaba(zabaTecaji[i], zabaTecajiGodine[i], zabaMaxTecajna[i])
        }
        
        ### Učitaj podatke
        zabaTecajiLista <- vector("list") 
        
        for (i in seq_along(zabaTecaji)) {
                zabaTecajiLista[[i]] <- zabaTecajCleanator(zabaTecaji[i], zabaTecajiGodine[i], zabaMaxTecajna[i])
        }
        
        names(zabaTecajiLista) <- zabaTecajiGodine
        
        zabaTecaj <- do.call("rbind", zabaTecajiLista)
        save(zabaTecaj, file = "./varijable/zabaTecaj.Rdata")
}