# import packages
library(AMR)
library(sf)
library(tidyverse)
#options(repos = c(CerteMedEpi = "https://certe-medical-epidemiology.r-universe.dev",options()$repos))

# import all data from files
certe <- readRDS("~/OneDrive - UMCG/ABRZNN/data/Isolaten/certe_isolaten_2016_2021.rds")
izore <- readRDS("~/OneDrive - UMCG/ABRZNN/data/Isolaten/izore_isolaten_2016_2021.rds")
umcg16 <- read.csv("~/OneDrive - UMCG/ABRZNN/data/Isolaten/UMCGdata/RegDataAnalysev4-2016.csv", sep=';')
umcg17 <- read.csv("~/OneDrive - UMCG/ABRZNN/data/Isolaten/UMCGdata/RegDataAnalysev4-2017.csv", sep=';')
umcg18 <- read.csv("~/OneDrive - UMCG/ABRZNN/data/Isolaten/UMCGdata/RegDataAnalysev4-2018.csv", sep=';')
umcg19 <- read.csv("~/OneDrive - UMCG/ABRZNN/data/Isolaten/UMCGdata/RegDataAnalysev4-2019.csv", sep=';')
umcg20 <- read.csv("~/OneDrive - UMCG/ABRZNN/data/Isolaten/UMCGdata/RegDataAnalysev4-2020.csv", sep=';')
umcg21 <- read.csv("~/OneDrive - UMCG/ABRZNN/data/Isolaten/UMCGdata/RegDataAnalysev4-2021.csv", sep=';')

# merge all umcg and preprocess
umcg18$Ordernr = as.double(umcg18$Ordernr)
umcg <- bind_rows(umcg16, umcg17,umcg18,umcg19,umcg20,umcg21)
umcg <- umcg[ -c(76,85,77,78,97:101,104,105) ]


# add lab
certe <- certe %>% mutate(lab = 'Certe')
izore <- izore %>% mutate(lab = 'Izore')
umcg <- umcg %>% mutate(lab = 'UMCG')


# combine all files to one file
names(umcg) <- tolower(names(umcg))
colnames(certe)[37:79] <- ab_name(colnames(certe[37:79]), 'nl')
colnames(izore)[42:116] <- ab_name(colnames(izore[42:116]),'nl')
colnames(umcg)[21:86] <- ab_name(colnames(umcg[21:86]),'nl')
colnames(umcg)[92:101] <- ab_name(colnames(umcg[92:101]),'nl')
izore$bacteriecode <- NULL
umcg <- umcg %>% mutate(bacteriecode = as.mo(uitslag))
names(izore)[names(izore) == 'ziekenhuis'] <- 'zkhlocatie'
names(izore)[names(izore) == 'afdeling'] <- 'zkhafdeling'
names(izore)[names(izore) == 'materiaalcode'] <- 'mtrlcode'
names(izore)[names(izore) == 'micro_organisme'] <- 'bacteriecode'
names(izore)[names(izore) == 'pos_neg'] <- 'uitslag_int'
names(izore)[names(izore) == 'beoordeling'] <- 'uitslag_commentaar'
names(izore)[names(izore) == 'uitvoerende_afdeling'] <- 'uitvafd'
names(izore)[names(izore) == 'pat_id'] <- 'patidnb'
names(umcg)[names(umcg) == 'zkhafd'] <- 'zkhafdeling'
names(umcg)[names(umcg) == 'groei'] <- 'groeidichtheid'
izore$patidnb = as.character(izore$patidnb)
umcg$ordernr = as.character(umcg$ordernr)
umcg$ontvangstdatum = as.Date(umcg$ontvangstdatum, format = "%d/%m/%Y")
umcg$stam = as.character(umcg$stam)
umcg$patidnb = as.character(umcg$patidnb)
certe_izore_umcg <- bind_rows(certe, izore, umcg)

# first isolate and cleaning
data_1st <- certe_izore_umcg %>% mutate(first = first_isolate(info = TRUE), type = mo_type())
data_1st <- data_1st %>% filter(first == TRUE, type == 'Bacteria', uitslag_int == 'Positief' | uitslag_int == 'pos')
data_1st <- data_1st %>% filter(geslacht == 'M' | geslacht == 'V')
data_1st <- data_1st %>% mutate(jaar = format(as.Date(ontvangstdatum, format="%Y-%m-%d"),"%Y"))

# add reference data
reference <- readRDS("~/OneDrive - UMCG/ABRZNN/Data/Referentiedata/postcodes.rds")
data_1st$postcode = as.numeric(data_1st$postcode)
data_1st <- data_1st %>% left_join(select(reference,plaats,gemeente,provincie,nuts3,postcode),by='postcode')
data_1st <- data_1st %>% mutate(postcode2 = substr(postcode, start = 1, stop = 2))
data_1st$postcode2 = as.numeric(data_1st$postcode2)
data_1st <- data_1st %>% filter(provincie == 'Groningen' | provincie == 'Friesland' | provincie == 'Drenthe')

# add material groups
mtrlgroepen <- read_csv2("~/OneDrive - UMCG/ABRZNN/data/Materiaalgroepen/mtrlgroepen.csv")
data_1st <- data_1st %>% left_join(select(mtrlgroepen,mtrlgroep,Materiaalcode),by=c('mtrlcode'='Materiaalcode'))

# add bacteria info
data_1st <- data_1st %>% mutate(genus = mo_genus(bacteriecode))
data_1st <- data_1st %>% mutate(species = mo_species(bacteriecode))
data_1st <- data_1st %>% mutate(gram = mo_gramstain(bacteriecode))
data_1st <- data_1st %>% mutate(bacteriesoort = mo_name(bacteriecode))

# age groups
data_1st <- data_1st %>% mutate(leeftijdgroep = case_when(
  leeftijd > 90 ~ 'overig',
  leeftijd >= 81  & leeftijd <= 90 ~ '81-90',
  leeftijd >= 71  & leeftijd <= 80 ~ '71-80',
  leeftijd >= 61  & leeftijd <= 70 ~ '61-70',
  leeftijd >= 51  & leeftijd <= 60 ~ '51-60',
  leeftijd >= 41  & leeftijd <= 50 ~ '41-50',
  leeftijd >= 31  & leeftijd <= 40 ~ '31-40',
  leeftijd >= 21  & leeftijd <= 30 ~ '21-30',
  leeftijd >= 11 & leeftijd <= 20 ~ '11-20',
  leeftijd >= 0 & leeftijd <= 10 ~ '0-10'))

# cleaning
data_1st$is_esbl = as.logical(data_1st$is_esbl)
data_1st$is_mrsa = as.logical(data_1st$is_mrsa)
data_1st$is_vre = as.logical(data_1st$is_vre)

data_1st$zorglijn[data_1st$zorglijn == "1elijn"] <- "1e lijn"  
data_1st$zorglijn[data_1st$zorglijn == "2elijn"] <- "2e lijn"  
data_1st$zorglijn[data_1st$zorglijn == "3e lijns"] <- "3e lijn"

data_1st <- data_1st %>% select('ordernr','ontvangstdatum','jaar','zorglijn','lab','zkhlocatie','zkhafdeling','specialisme','patidnb','geslacht','leeftijd','leeftijdgroep','postcode','postcode2','plaats','gemeente','nuts3','provincie','mtrlgroep','bacteriecode','bacteriesoort','genus','species','gram','Benzylpenicilline':'Rifampicine','Ceftaroline':'Flucloxacilline','Isoniazide','Levofloxacine','Framycetin':'Polymyxine B','Prothionamide':'Streptomycine','Temocilline':'Cefpodoxim/clavulaanzuur','Cefaloridine','Cefetrizole','Florfenicol','bevat_brmo','is_esbl':'is_prsp')

data_1st <- data_1st %>% mutate(across(Benzylpenicilline:Florfenicol, as.rsi))





#export to shiny app
write.csv(data_1st,"dashboard/data_1st.csv", row.names = FALSE)

#publish app
rsconnect::deployApp('dashboard', appName = 'ResistentieProfiel')
rsconnect::configureApp('ResistentieProfiel', size="xxxlarge")
