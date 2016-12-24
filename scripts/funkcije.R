##************##
##  Funkcije  ##
##************##

#### Generator putanja ####
pathinator <- function(putanja, banka) {
        lista <- list.dirs(putanja)
        indeksi <- grep(banka, lista) 
        lista_banka <- vector("character", length = length(indeksi)) 
        for (i in seq_along(indeksi)) {
             lista_banka[i] <- lista[indeksi[i]]    
        }
        return(lista_banka)
}

#### Listanje direktorija ####
fileFun <- function(theDir) {
        ## Look for files (directories included for now)
        allFiles <- list.files(theDir, no.. = TRUE)
        ## Look for directory names
        allDirs <- list.dirs(theDir, full.names = FALSE, recursive = FALSE)
        ## If there are any directories,
        if(length(allDirs)) {
                ## then call this function again
                moreFiles <- lapply(file.path(theDir, allDirs), fileFun)
                ## Set names for the new list
                names(moreFiles) <- allDirs
                ## Determine files found, excluding directory names
                outFiles <- allFiles[!allFiles %in% allDirs]
                ## Combine appropriate results for current list
                #if(length(outFiles)) {
                #        allFiles <- c(outFiles, moreFiles)
                #} else {
                #        allFiles <- moreFiles
                #}
                return(moreFiles)
        }
        #return(allFiles)
}

#### Read RBA ####
# učitavanje kunskih i eur prometa sa RBA
read.rba <- function(putanja, valuta) {
        # učitaj listu datoteka
        datoteke <- list.files(putanja)
        # izvuci indekse pozicija sa odabranom valutom
        indeksi <- grep(valuta, datoteke)
        # pripremi listu za učitavanje prometa
        lista <- vector("list", length(indeksi))  
        prometi <- data.frame()
        for (i in seq_along(indeksi)) {
                #učitaj xls datoteku
                print(paste(putanja, "/", datoteke[indeksi[i]], sep = ""))
                lista[[i]] <- read.xls(paste(putanja, "/", datoteke[indeksi[i]], sep = ""), 
                                       sheet = 1, method = "csv", pattern = "Datum valute") 
                # izbaci zadnja dva reda
                lista[[i]] <- lista[[i]][-NROW(lista[[i]]), ]
                # sve učitane filove ubaci u jedan data frame
                prometi <- rbind(prometi, lista[[i]])
        }
        return(prometi)
}
# učitavanje RBA tečaja
read.tecaj.rba <- function(putanja) {
        # učitaj listu datoteka
        datoteke <- list.files(putanja)
        # izvuci indekse pozicija sa odabranom valutom
        indeksi <- length(datoteke)
        # pripremi listu za učitavanje prometa
        lista <- vector("list", length(indeksi))  
        tecaji <- data.frame()
        for (i in seq_along(indeksi)) {
                # učitaj XML datoteku sa tečajevima
                lista[[i]] <- xmlParse(paste(putanja, "/", datoteke[indeksi[i]], sep = ""))
                
                # pebaci tečajeve u data frame
                lista[[i]] <- xmlToDataFrame(nodes = xmlChildren(xmlRoot(lista[[i]])[["Tecajevi"]]), 
                                            colClasses = c("character", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric"), 
                                            stringsAsFactors = FALSE)
                # sve učitane filove ubaci u jedan data frame
                tecaji <- rbind(tecaji, lista[[i]])
        }
        return(tecaji)        
}


#### Clean RBA kune ####
rbaKuneCleanator <- function(rba) {
        #### Počisti kolone i imena ####
        rba$X <- NULL
        rba$X.1 <- NULL
        rba$X.2 <- NULL
        rba$X.3 <- NULL
        rba$X.4 <- NULL
        rba$X.5 <- NULL
        
        ### [.]+ pretražuje niz od barem jedne točke!
        names(rba) <- sub("[.]+", "_", names(rba))
        
        ### [.]$ briše točku na kraju riječi
        names(rba) <- sub("[.]$", "", names(rba))
        
        ### izbaci "ž"
        names(rba) <- sub("ž", "z", names(rba))
        
        ### imena varijabli sa malim slovima
        names(rba) <- tolower(names(rba))
        
        ### izbaci "," iz brojeva
        rba$uplata_kn <- gsub(",", "" , rba$uplata_kn, 
                                   ignore.case = FALSE, perl = FALSE, fixed = TRUE, 
                                   useBytes = FALSE)  
        
        rba$isplata_kn <- gsub(",", "" , rba$isplata_kn, 
                                    ignore.case = FALSE, perl = FALSE, fixed = TRUE, 
                                    useBytes = FALSE)  
        
        
        ### pretvori promete u broj i nepostojeće promete popunu sa 0 
        rba$uplata_kn <- as.numeric(rba$uplata_kn)
        rba$uplata_kn[is.na(rba$uplata_kn)] <- 0
        
        rba$isplata_kn <- as.numeric(rba$isplata_kn)
        rba$isplata_kn[is.na(rba$isplata_kn)] <- 0
        
        
        ### formatiraj datume
        rba$datum_valute <- as.Date(as.character(rba$datum_valute), format="%d.%m.%Y")
        rba$datum_knjizenja <- as.Date(as.character(rba$datum_knjizenja), format="%d.%m.%Y")
        
        
        #### Preuredi podatke ####
        ### dodaj negativni predznak isplatama
        rba$isplata_kn <- rba$isplata_kn *  (-1)
        
        ### jedna kolona sa iznosima i jedna kolona sa vrstom prometa
        rba <- gather(rba, vrsta, iznos, uplata_kn:isplata_kn)
        
        ### izvuci samo redove koji imaju promet
        rba <- filter(rba, iznos != 0)
        ### sortiraj po datumu
        rba <- arrange(rba, datum_valute)
        
        ### definiraj vrste prometa
        rba$vrsta <- as.factor(rba$vrsta)
        levels(rba$vrsta) <- c("isplata", "uplata", "pocetno_stanje", "bez_transakcije")
        levels(rba$vrsta_prometa) <- c(levels(rba$vrsta_prometa), "Početno stanje", "Bez transakcije")
        levels(rba$primatelj_platitelj) <- c(levels(rba$primatelj_platitelj), "Početno stanje", "Bez transakcije")
        return(rba)
}

#### Clean RBA eur ####
rbaEurCleanator <- function(rba) {
        #### Počisti kolone i imena ####
        rba$X <- NULL
        rba$X.1 <- NULL
        rba$X.2 <- NULL
        rba$X.3 <- NULL
        rba$X.4 <- NULL
        rba$X.5 <- NULL
        rba$X.6 <- NULL
        ### [.]+ pretražuje niz od barem jedne točke!
        names(rba) <- sub("[.]+", "_", names(rba))
        
        ### [.]$ briše točku na kraju riječi
        names(rba) <- sub("[.]$", "", names(rba))
        
        ### izbaci "ž"
        names(rba) <- sub("ž", "z", names(rba))
        
        ### imena varijabli sa malim slovima
        names(rba) <- tolower(names(rba))
        
        ### izbaci "," iz brojeva
        rba$uplata_eur <- gsub(",", "" , rba$uplata_eur, 
                                   ignore.case = FALSE, perl = FALSE, fixed = TRUE, 
                                   useBytes = FALSE)  
        
        rba$isplata_eur <- gsub(",", "" , rba$isplata_eur, 
                                    ignore.case = FALSE, perl = FALSE, fixed = TRUE, 
                                    useBytes = FALSE)  
        
        
        ### pretvori promete u broj i nepostojeće promete popuni sa 0 
        rba$uplata_eur <- as.numeric(rba$uplata_eur)
        rba$uplata_eur[is.na(rba$uplata_eur)] <- 0
        
        rba$isplata_eur <- as.numeric(rba$isplata_eur)
        rba$isplata_eur[is.na(rba$isplata_eur)] <- 0
        
        
        ### formatiraj datume
        rba$datum_valute <- as.Date(as.character(rba$datum_valute), format="%d.%m.%Y")
        rba$datum_knjizenja <- as.Date(as.character(rba$datum_knjizenja), format="%d.%m.%Y")
        
        #### Preuredi podatke ####
        ### dodaj negativni predznak isplatama
        rba$isplata_eur <- rba$isplata_eur *  (-1)
        
        ### jedna kolona sa iznosima i jedna kolona sa vrstom prometa
        rba <- gather(rba, vrsta, iznos, uplata_eur:isplata_eur)
        
        ### izvuci samo redove koji imaju promet
        rba <- filter(rba, iznos != 0)
        rba <- arrange(rba, datum_valute)
        
        ### definiraj vrste prometa
        rba$vrsta <- as.factor(rba$vrsta)
        levels(rba$vrsta) <- c("isplata", "uplata", "pocetno_stanje", "bez_transakcije")
        levels(rba$vrsta_prometa) <- c(levels(rba$vrsta_prometa), "Početno stanje", "Bez transakcije")
        levels(rba$primatelj_platitelj) <- c(levels(rba$primatelj_platitelj), "Početno stanje", "Bez transakcije")
        return(rba)
}
#### Clean RBA tečaj ####
rbaTecajCleanator <- function(rba) {
        # formatiraj datume
        rba$DatumVrijediOd <- as.Date(rba$DatumVrijediOd, 
                                            format = "%d.%m.%Y")
        
        # uredi nazive varijabli
        names(rba) <- gsub("([a-z])([A-Z])", "\\1_\\2", names(rba),  
                                 perl = TRUE) %>% tolower
        
        
        
        lista_datuma <- seq.Date(rba$datum_vrijedi_od[1], rba$datum_vrijedi_od[NROW(rba)], by = "day")
        lista_datuma_df <- data.frame(datum_vrijedi_od = lista_datuma)
        rba <- full_join(lista_datuma_df, rba)
        
        ### popuni vrijednosti koje fale sa redom prije
        rba <- na.locf(rba)
        rba <- arrange(rba, datum_vrijedi_od)
        rba$datum_vrijedi_od <- as.Date(rba$datum_vrijedi_od, 
                                              format = "%Y-%m-%d")
        
        # vrati u numeric
        rba$kupovni_za_devize <- as.numeric(rba$kupovni_za_devize)
        rba$srednji <- as.numeric(rba$srednji)
        rba$prodajni_za_devize <- as.numeric(rba$prodajni_za_devize)
        rba$kupovni_za_efektivu <- as.numeric(rba$kupovni_za_efektivu)
        rba$prodajni_za_efektivu <- as.numeric(rba$prodajni_za_efektivu)
        return(rba)
}


#### Read ZABA ####
# učitavanje kunkskih i eur prometa sa ZABA-e
read.zaba <- function(putanja, valuta) {
        # učitaj listu datoteka
        datoteke <- list.files(putanja)
        # izvuci indekse pozicija sa odabranom valutom
        indeksi <- grep(valuta, datoteke)
        # pripremi listu za učitavanje prometa
        lista <- vector("list", length(indeksi))  
        prometi <- data.frame()
        for (i in seq_along(indeksi)) {
                #učitaj xls datoteku
                lista[[i]] <- read.xls(paste(putanja, "/", datoteke[indeksi[i]], sep = ""), 
                                       sheet = 1, method = "csv", pattern = "Datum") 
                # izbaci zadnja dva reda
                lista[[i]] <- lista[[i]][-((NROW(lista[[i]])-1):(NROW(lista[[i]]))), ]
                # sve učitane filove ubaci u jedan data frame
                prometi <- rbind(prometi, lista[[i]])
        }
        return(prometi)
}

# učitavanje ZABA tečaja

download.tecaj.zaba <- function(putanja, godina, indeksGodine) {
        for (i in 1:indeksGodine) {
                file_name <- paste(putanja, "/tecajna-", i, "-", godina, ".prn", sep = "")
                download_path <- paste("http://www.zaba.hr/home/ZabaUtilsWeb/utils/tecaj/prn/", i, "/", godina, sep = "")
                if (!file.exists(file_name)) {
                        download.file(download_path, destfile = file_name)
                }        
        }
}

#### Clean ZABA kune ####
zabaKuneCleanator <- function(zaba) {
        
        #### Počisti kolone i imena ####
        ### [.]+ pretražuje niz od barem jedne točke!
        names(zaba) <- gsub("[.]+", "_", names(zaba))
        
        ### imena varijabli sa malim slovima
        names(zaba) <- tolower(names(zaba))
        
        ### izbaci "š"
        names(zaba) <- gsub("š", "s", names(zaba))
        
        ### izbaci "ž"
        names(zaba) <- gsub("ž", "z", names(zaba))
        
        ### izbaci "č"
        names(zaba) <- gsub("č", "c", names(zaba))
        
        ### izbaci "ć"
        names(zaba) <- gsub("ć", "c", names(zaba))
        
        ### izbaci "," iz brojeva
        zaba$dugovni <- gsub(",", "" , zaba$dugovni, 
                                  ignore.case = FALSE, perl = FALSE, fixed = TRUE, 
                                  useBytes = FALSE)  
        
        zaba$potrazni <- gsub(",", "" , zaba$potrazni, 
                                   ignore.case = FALSE, perl = FALSE, fixed = TRUE, 
                                   useBytes = FALSE)  
        
        
        ### pretvori promete u broj i nepostojeće promete popunu sa 0 
        zaba$dugovni <- as.numeric(zaba$dugovni)
        zaba$dugovni[is.na(zaba$dugovni)] <- 0
        
        zaba$potrazni <- as.numeric(zaba$potrazni)
        zaba$potrazni[is.na(zaba$potrazni)] <- 0
        
        ### formatiraj datume
        zaba$datum_izvrsenja <- as.Date(as.character(zaba$datum_izvrsenja), format="%d.%m.%Y")
        zaba$datum_knjizenja <- as.Date(as.character(zaba$datum_knjizenja), format="%d.%m.%Y")
        
        
        #### Preuredi podatke ####
        ### dodaj negativni predznak isplatama
        zaba$dugovni <- zaba$dugovni *  (-1)
        
        ### Reorganiziraj kolone da bi gather prvo uzimao uplate
        
        zaba <- zaba[c(1,2,4,3,5,6,7,8,9,10)]
        
        ### jedna kolona sa iznosima i jedna kolona sa vrstom prometa
        zaba <- gather(zaba, vrsta, iznos, potrazni:dugovni)
        
        ### izvuci samo redove koji imaju promet
        zaba <- filter(zaba, iznos != 0)
        ### sortiraj po datumu
        zaba <- arrange(zaba, datum_izvrsenja)
        
        ### definiraj vrste prometa
        zaba$vrsta <- as.factor(zaba$vrsta)
        levels(zaba$vrsta)[levels(zaba$vrsta) == "dugovni"] <- "isplata"
        levels(zaba$vrsta)[levels(zaba$vrsta) == "potražni"] <- "uplata"
        levels(zaba$vrsta) <- c("isplata", "uplata", "pocetno_stanje", "bez_transakcije")
        levels(zaba$opis_placanja) <- c(levels(zaba$opis_placanja), "Početno stanje", "Bez transakcije")
        levels(zaba$primatelj) <- c(levels(zaba$primatelj), "Početno stanje", "Bez transakcije")
        
        
        ### odaberi samo potrebne kolone
        zaba <- select(zaba, datum_valute = datum_izvrsenja,  
                            vrsta_prometa = opis_placanja,
                            primatelj_platitelj = primatelj, 
                            vrsta, iznos)   
        return(zaba)
}

#### Clean ZABA eur ####
zabaEurCleanator <- function(zaba) {
        if (nrow(zaba) == 0) {
                return(zaba)
        } else {
        #### Počisti kolone i imena ####
        ### [.]+ pretražuje niz od barem jedne točke!
        names(zaba) <- gsub("[.]+", "_", names(zaba))
        
        ### imena varijabli sa malim slovima
        names(zaba) <- tolower(names(zaba))
        
        ### izbaci "š"
        names(zaba) <- gsub("š", "s", names(zaba))
        
        ### izbaci "ž"
        names(zaba) <- gsub("ž", "z", names(zaba))
        
        ### izbaci "č"
        names(zaba) <- gsub("č", "c", names(zaba))
        
        ### izbaci "ć"
        names(zaba) <- gsub("ć", "c", names(zaba))
        
        ### izbaci "," iz brojeva
        zaba$dugovni <- gsub(",", "" , zaba$dugovni, 
                                 ignore.case = FALSE, perl = FALSE, fixed = TRUE, 
                                 useBytes = FALSE)  
        
        zaba$potrazni <- gsub(",", "" , zaba$potrazni, 
                                  ignore.case = FALSE, perl = FALSE, fixed = TRUE, 
                                  useBytes = FALSE)  
        
        
        ### pretvori promete u broj i nepostojeće promete popunu sa 0 
        zaba$dugovni <- as.numeric(zaba$dugovni)
        zaba$dugovni[is.na(zaba$dugovni)] <- 0
        
        zaba$potrazni <- as.numeric(zaba$potrazni)
        zaba$potrazni[is.na(zaba$potrazni)] <- 0
        
        ### formatiraj datume
        zaba$datum_izvrsenja <- as.Date(as.character(zaba$datum_izvrsenja), format="%d.%m.%Y")
        zaba$datum_knjizenja <- as.Date(as.character(zaba$datum_knjizenja), format="%d.%m.%Y")
        
        
        #### Preuredi podatke ####
        ### dodaj negativni predznak isplatama
        zaba$dugovni <- zaba$dugovni *  (-1)
        
        ### Reorganiziraj kolone da bi gather prvo uzimao uplate
        zaba <- zaba[c(1,2,4,3,5,6,7,8,9,10)]
        
        
        ### jedna kolona sa iznosima i jedna kolona sa vrstom prometa
        zaba <- gather(zaba, vrsta, iznos, potrazni:dugovni)
        
        ### izvuci samo redove koji imaju promet
        zaba <- filter(zaba, iznos != 0)
        ### sortiraj po datumu
        zaba <- arrange(zaba, datum_izvrsenja)
        
        ### definiraj vrste prometa
        zaba$vrsta <- as.factor(zaba$vrsta)
        levels(zaba$vrsta)[levels(zaba$vrsta) == "dugovni"] <- "isplata"
        levels(zaba$vrsta)[levels(zaba$vrsta) == "potrazni"] <- "uplata"
        levels(zaba$vrsta) <- c("isplata", "uplata", "pocetno_stanje", "bez_transakcije")
        levels(zaba$opis_placanja) <- c(levels(zaba$opis_placanja), "Početno stanje", "Bez transakcije")
        levels(zaba$primatelj) <- c(levels(zaba$primatelj), "Početno stanje", "Bez transakcije")
        
        
        ### odaberi samo potrebne kolone
        zaba <- select(zaba, datum_valute = datum_izvrsenja, 
                           vrsta_prometa = opis_placanja,
                           primatelj_platitelj = primatelj, 
                           vrsta, iznos)
        return(zaba)
        }
}

#### Clean ZABA tečaj ####
zabaTecajCleanator <- function(putanja, godina, indeksGodine) {
        # počisti pomoćne filove
        if (file.exists("./pomocni/datumi.txt")) {
                file.remove("./pomocni/datumi.txt")
        }
        
        if (file.exists("./pomocni/tecaji.txt")) {
                file.remove("./pomocni/tecaji.txt")
        }
        
        
        for (i in 1:indeksGodine) {
                datumi <- paste("grep 'PRIMJENJUJU SE' ", putanja, "/tecajna-", i, "-", godina, ".prn >> ./pomocni/datumi.txt", sep = "")
                tecaji <- paste("grep '978 EUR' ", putanja, "/tecajna-", i, "-", godina, ".prn >> ./pomocni/tecaji.txt", sep = "")
                system(datumi)
                system(tecaji)
        }
        
        
        zaba_tecaj_tecaj <- read.table("./pomocni/tecaji.txt")
        zaba_tecaj_datumi <- read.table("./pomocni/datumi.txt")
        zaba_tecaj_tecaj <- zaba_tecaj_tecaj[c(4,5,6,7,8)]
        zaba_tecaj_tecaj <- zaba_tecaj_tecaj[c(2, 3, 4, 1, 5)]
        zaba_tecaj_datumi <- zaba_tecaj_datumi[c(10)]
        zaba_tecaj <- cbind(zaba_tecaj_datumi, zaba_tecaj_tecaj)
        names(zaba_tecaj) <- names(rbaTecaj)
        zaba_tecaj$datum_vrijedi_od <- as.Date(as.character(zaba_tecaj$datum_vrijedi_od), 
                                                format = "%d.%m.%Y")
         
         
         
         
        lista_datuma <- seq.Date(zaba_tecaj$datum_vrijedi_od[1], zaba_tecaj$datum_vrijedi_od[NROW(zaba_tecaj)], by = "day")
        lista_datuma_df <- data.frame(datum_vrijedi_od = lista_datuma)
        zaba_tecaj <- full_join(lista_datuma_df, zaba_tecaj)
         
        ### popuni vrijednosti koje fale sa redom prije
        zaba_tecaj <- na.locf(zaba_tecaj)
        zaba_tecaj <- arrange(zaba_tecaj, datum_vrijedi_od)
        zaba_tecaj$datum_vrijedi_od <- as.Date(as.character(zaba_tecaj$datum_vrijedi_od), 
                                                format = "%Y-%m-%d")
        zaba_tecaj$kupovni_za_devize <- as.numeric(zaba_tecaj$kupovni_za_devize)
        zaba_tecaj$srednji <- as.numeric(zaba_tecaj$srednji)
        zaba_tecaj$prodajni_za_devize <- as.numeric(zaba_tecaj$prodajni_za_devize)
        zaba_tecaj$kupovni_za_efektivu <- as.numeric(zaba_tecaj$kupovni_za_efektivu)
        zaba_tecaj$prodajni_za_efektivu <- as.numeric(zaba_tecaj$prodajni_za_efektivu)
        return(zaba_tecaj)
}

#### Read ERSTE ####
# učitavanje kunkskih prometa sa ERSTE-a
read.erste <- function(putanja) {
        # učitaj listu datoteka
        datoteke <- list.files(putanja)
        # izvuci indekse pozicija sa odabranom valutom
        indeksi <- grep(".ods", datoteke)
        # pripremi listu za učitavanje prometa
        lista <- vector("list", length(indeksi))  
        prometi <- data.frame()
        for (i in seq_along(indeksi)) {
                #učitaj xls datoteku
                lista[[i]] <- read.ods(paste(putanja, "/", datoteke[indeksi[i]], sep = ""), sheet = 1) 
                # izbaci prvi red
                lista[[i]] <- lista[[i]][-1, ]
                # sve učitane filove ubaci u jedan data frame
                prometi <- rbind(prometi, lista[[i]])
        }
        return(prometi)
}
#### Clean ERSTE kune ####
ersteKuneCleanator <- function(erste) {
                if (nrow(erste) == 0) {
                        return(zaba)
                } else {
                        imena_kolona <- c("redni_broj",
                                          "datum_valute",
                                          "datum_izvrsenja",
                                          "opis_placanja", 
                                          "broj_racuna_platitelja", 
                                          "isplata", 
                                          "uplata",
                                          "stanje",
                                          "pnb_platitelja", 
                                          "pnb_primatelja", 
                                          "primatelj_platitelj", 
                                          "mjesto", 
                                          "referenca_placanja")
                        names(erste) <- imena_kolona
                        ### izbaci "." iz brojeva
                        erste$isplata <- gsub(".", "" , erste$isplata, 
                                                   ignore.case = FALSE, perl = FALSE, fixed = TRUE, 
                                                   useBytes = FALSE)  
                        
                        erste$uplata <- gsub(".", "" , erste$uplata, 
                                                  ignore.case = FALSE, perl = FALSE, fixed = TRUE, 
                                                  useBytes = FALSE) 
                        
                        erste$stanje <- gsub(".", "" , erste$stanje, 
                                                  ignore.case = FALSE, perl = FALSE, fixed = TRUE, 
                                                  useBytes = FALSE) 
                        
                        ### zamjeni "," sa "."
                        erste$isplata <- gsub(",", "." , erste$isplata, 
                                                   ignore.case = FALSE, perl = FALSE, fixed = TRUE, 
                                                   useBytes = FALSE)  
                        
                        erste$uplata <- gsub(",", "." , erste$uplata, 
                                                  ignore.case = FALSE, perl = FALSE, fixed = TRUE, 
                                                  useBytes = FALSE) 
                        
                        erste$stanje <- gsub(",", "." , erste$stanje, 
                                                  ignore.case = FALSE, perl = FALSE, fixed = TRUE, 
                                                  useBytes = FALSE) 
                        
                        ### nepostojeće promete popuni sa 0 
                        erste$isplata[erste$isplata == ""] <- 0 
                        erste$uplata[erste$uplata == ""] <- 0 
                        
                        ### pretvori promete i stanje u broj 
                        erste$isplata <- as.numeric(erste$isplata)
                        erste$uplata <- as.numeric(erste$uplata)
                        erste$stanje <- as.numeric(erste$stanje)
                        
                        ### formatiraj datume
                        erste$datum_valute <- as.Date(as.character(erste$datum_valute), format="%d.%m.%Y")
                        erste$datum_izvrsenja <- as.Date(as.character(erste$datum_izvrsenja), format="%d.%m.%Y")
                        
                        ### redni broj u integer
                        erste$redni_broj <- as.integer(erste$redni_broj)
                        
                        #### Preuredi podatke ####
                        ### dodaj negativni predznak isplatama
                        erste$isplata<- erste$isplata *  (-1)
                        
                        ### Reorganiziraj kolone da bi gather prvo uzimao uplate
                        erste <- erste[c(1, 2, 3, 4, 5, 7, 6, 8, 9, 10, 11, 12, 13)]
                        
                        
                        ### jedna kolona sa iznosima i jedna kolona sa vrstom prometa
                        erste <- gather(erste, vrsta, iznos, uplata:isplata)
                        
                        ### izvuci samo redove koji imaju promet
                        erste <- filter(erste, iznos != 0)
                        ### sortiraj po datumu
                        erste <- arrange(erste, datum_valute)
                        
                        ### prebaci varijable u factor
                        erste$opis_placanja <- as.factor(erste$opis_placanja)
                        erste$primatelj_platitelj <- as.factor(erste$primatelj_platitelj)
                        
                        ########################################################################################################
                        ### definiraj vrste prometa
                        levels(erste$vrsta) <- c("uplata", "isplata", "pocetno_stanje", "bez_transakcije")
                        levels(erste$opis_placanja) <- c(levels(erste$opis_placanja), "Početno stanje", "Bez transakcije")
                        levels(erste$primatelj_platitelj) <- c(levels(erste$primatelj_platitelj), "Početno stanje", "Bez transakcije")
                        
                        #### odaberi samo potrebne kolone
                        erste <- select(erste, 
                                             datum_valute = datum_valute, 
                                             vrsta_prometa = opis_placanja,
                                             primatelj_platitelj = primatelj_platitelj, 
                                             vrsta = vrsta, 
                                             iznos = iznos)
                }
}