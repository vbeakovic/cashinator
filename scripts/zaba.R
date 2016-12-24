#### Integracija HRK i EUR ZABA ####
# priljeve na kunski računa sa deviznog treba izbaciti
# odljeve sa deviznog računa na kunski treba izbaciti
if (file.exists("./varijable/zaba.Rdata")) {
        load("./varijable/zaba.Rdata")
} else {
        #### Izbacivanje priljeva na kunski sa deviznog računa ####
        zabaKune <- zabaKune[zabaKune$vrsta_prometa != "Otkup valute", ]
        
        #### izbacivanje odljeva sa deviznog na kunski račun ####
        zabaEur <- zabaEur[zabaEur$vrsta_prometa != "Otkup valute", ]
        zabaEur <- zabaEur[zabaEur$vrsta_prometa != "Konverzija", ]
        
        zabaEur$iznos_kn <- 0
        #### prebacivanje EUR u HRK ####
        for (i in 2:NROW(zabaEur)) {
                zabaEur$iznos_kn[i] <- zabaEur$iznos[i] * 
                        zabaTecaj$kupovni_za_devize[zabaTecaj$datum_vrijedi_od == unique(zabaEur$datum_valute[i])]
        }

        zabaEur <- mutate(zabaEur, stanje_kn = cumsum(iznos_kn))
        
        zabaEur_spremno <- select(zabaEur, datum = datum_valute, 
                                  vrsta_prometa,
                                  primatelj_platitelj,
                                  vrsta,
                                  iznos = iznos_kn)
        
        zabaKune_spremno <- select(zabaKune, datum = datum_valute, 
                                   vrsta_prometa,
                                   primatelj_platitelj,
                                   vrsta,
                                   iznos)
        
        zaba <- rbind(zabaKune_spremno, zabaEur_spremno)
        zaba <- arrange(zaba, datum)
        zaba$iznos[1] <- pocetna_stanja$hrk[2]
        save(zaba, file = "./varijable/zaba.Rdata")
}