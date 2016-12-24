#### Integracija HRK i EUR RBA ####
# priljeve na kunski računa sa deviznog treba izbaciti
# odljeve sa deviznog računa na kunski treba izbaciti
if (file.exists("./varijable/rba.Rdata")) {
        load("./varijable/rba.Rdata")
} else {
        #### Izbacivanje priljeva na kunski sa deviznog računa ####
        rbaKune <- rbaKune[rbaKune$vrsta_prometa != "Priljev iz dev. pos.", ]
        
        #### izbacivanje odljeva sa deviznog na kunski račun ####
        rbaEur <- rbaEur[rbaEur$vrsta_prometa != "Konverzija/transf", ]
        rbaEur <- rbaEur[rbaEur$vrsta_prometa != "IB Konverzija/transf", ]
        rbaEur$iznos_kn <- 0
        #### prebacivanje EUR u HRK ####
        for (i in 2:NROW(rbaEur)) {
                rbaEur$iznos_kn[i] <- rbaEur$iznos[i] * 
                        rbaTecaj$kupovni_za_devize[rbaTecaj$datum_vrijedi_od == rbaEur$datum_valute[i]]
        }

        
        rbaEur <- mutate(rbaEur, stanje_kn = cumsum(iznos_kn))
        
        rbaEur_spremno <- select(rbaEur, datum = datum_valute, 
                                  vrsta_prometa,
                                  primatelj_platitelj,
                                  vrsta,
                                  iznos = iznos_kn)
        
        rbaKune_spremno <- select(rbaKune, datum = datum_valute, 
                                   vrsta_prometa,
                                   primatelj_platitelj,
                                   vrsta,
                                   iznos)
        
        rba <- rbind(rbaKune_spremno, rbaEur_spremno)
        rba <- arrange(rba, datum)
        rba$iznos[1] <- pocetna_stanja$hrk[1]
        save(rba, file = "./varijable/rba.Rdata")
}        