#### Početna stanja ####
if (file.exists("./varijable/pocetna_stanja.Rdata")) {
   load("./varijable/pocetna_stanja.Rdata")
} else {
        pocetna_stanja_path <- "./pocetna_stanja/pocetna_stanja.ods" 
        pocetna_stanja <- read.ods(pocetna_stanja_path, sheet = 1)
        names(pocetna_stanja) <- pocetna_stanja[1, ]
        pocetna_stanja <- filter(pocetna_stanja, banka != "banka")
        pocetna_stanja$hrk <- as.numeric(pocetna_stanja$hrk)
        pocetna_stanja$eur <- as.numeric(pocetna_stanja$eur)
        save(pocetna_stanja, file = "./varijable/pocetna_stanja.Rdata")
}
