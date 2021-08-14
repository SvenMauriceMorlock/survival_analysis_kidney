library(data.table)
library(lubridate)

path1 <- "~/06_data/iga_sheet2.csv"
path2 <- "~/06_data/ntx.csv" 
path3 <- "~/06_data/IgA_sheet_1.csv"
################################################################################
tmp_data <- fread(path3, na.strings = "")
tmp_columns <- c("last-name", "first-name", "Date last seen")
tmp_data <- tmp_data[, ..tmp_columns]
tmp_names <- c("last-name", "first-name", "date_last_seen")
colnames(tmp_data) <- tmp_names
tmp_data$date_last_seen <- ymd(dmy(tmp_data$date_last_seen))
# Datenaufbereitung IGA Patienten
daten_iga <- fread(path1,
                   colClasses = c("graft loss (0=functial, 1=loss)" = "factor",
                                  "Pat death (0=alive, 1= dead)" =  "factor",
                                  "biopsy proven recurrence (0=no, 1=yes)" = "factor",
                                  "D-type" = "factor",
                                  "D-age" = "numeric",
                                  "D-abo" = "factor",
                                  "D-sex" = "factor",
                                  "R-abo" = "factor",
                                  "R Full Phenotype" = "factor",
                                  "D-pheno" = "factor",
                                  "Cold ischaemic period hours" = "numeric",
                                  "Cold ischaemic period minutes" = "numeric",
                                  "mm-B" = "factor",
                                  "mm-DR" = "factor",
                                  "Current PRA%" = "numeric",
                                  "Highest PRA%" = "numeric"
                   ), na.strings = "")
names_date_type <- c("Date of birth",
                     "T-date", "Todesdatum")
daten_iga[, (names_date_type) := lapply(.SD, function(x){ymd(dmy(x))}), .SDcols = names_date_type]
#### Teilnehmer unter 18 Jahren zur Zeit der Operation entfernen  
daten_iga <- daten_iga[(daten_iga$`T-date` - daten_iga$`Date of birth`) >= 18 * 365, ]
# Daten aus Sheet 1 hinzufügen
daten_iga <- data.table::merge.data.table(daten_iga,tmp_data, by = c("last-name", "first-name"), all.y = FALSE)
# Namen nicht relevant
daten_iga <- daten_iga[,-c(1,2)] 
names(daten_iga) <- c(
  "graft_loss", "graft_loss_date", "max_FUP_graft", "death", 
  "death_date", "max_FUP_surv", "biopsy_proven_recurrence",
  "date_of_birth", "T_date","D_type", "D_age", "D_abo", "D_sex",
  "R_abo", "R_full_phenotype", "D_pheno","cold_ischaemic_period_h", 
  "cold_ischaemic_period_m", "mm_B", "mm_DR", "current_PRA", 
  "highest_PRA", "date_last_seen")
daten_iga <- cbind(daten_iga,daten_iga$T_date + years(10))
names(daten_iga)[length(names(daten_iga))] <- "follow_up"
daten_iga$max_FUP_graft <- as.numeric(gsub(",",".",daten_iga$max_FUP_graft))
daten_iga$max_FUP_surv <- as.numeric(gsub(",",".",daten_iga$max_FUP_surv))
daten_iga_0 <- daten_iga[biopsy_proven_recurrence == 0]
daten_iga_1 <- daten_iga[biopsy_proven_recurrence == 1]
#################### Datenaufbereitung NTX Patienten ###########################
daten_ntx <- fread(path2, colClasses = c("Patienten Status[NTX PatientenInformation]" = "factor",
                                         "TX Status[NTX PatientenInformation]" = "factor",
                                         "Geschlecht" = "factor",
                                         "Bezeichnung" = "factor"),na.strings = "")
colnames_ntx <- c("Datum", "Bezeichnung", "Patienten_ID", "Familienname", "Vorname", "Geburtsdatum",
                  "Geschlecht", "NTXNR", "TX_FU_Ende_Liv1",
                  "T_Ende_1", "T_Ende_2", "T_Ende_3", "T_Ende_4", "T_Ende_5", "T_Ende_6",
                  "TX_Status", "P_Status", "Last_Seen", "Todesdatum")
colnames(daten_ntx) <- colnames_ntx

#### Factor levels 
daten_ntx$TX_Status<-  fct_collapse(daten_ntx$TX_Status,
                                    "mit" = c("1 - mit Transplantatfunktion"),
                                    "ohne" = c("2 - ohne Transplantatfunktion", "2- ohne Transplantatfunktion"))
daten_ntx$P_Status <- fct_collapse(daten_ntx$P_Status,
                                   "lebt" = c("1 - lebt"),
                                   "verstorben" = c("2 - verstorben"))
levels(daten_ntx$Geschlecht) <- c("m", "w")
#### Uhrzeiten aus Datum Variable entfernen
daten_ntx[, ("Datum") := lapply(.SD, function(x){substr(x, 1, 10)}), .SDcols = "Datum"]

#### Zeit/Datum Datentyp erstellen
namen_ntx_sheet1 <- c("Datum", "Geburtsdatum",
                      "T_Ende_1",
                      "T_Ende_2",
                      "T_Ende_3",
                      "T_Ende_4",
                      "T_Ende_5",
                      "T_Ende_6",
                      "Last_Seen", 
                      "Todesdatum")
daten_ntx[, (namen_ntx_sheet1) := lapply(.SD, function(x){ymd(dmy(x))}), .SDcols = namen_ntx_sheet1]
#### Teilnehmer unter 18 Jahren zur Zeit der Operation entfernen  
daten_ntx <- daten_ntx[interval(daten_ntx$Geburtsdatum, daten_ntx$Datum) / years(1) >= 18, ]

# follow_up time
daten_ntx <- cbind(daten_ntx, 
                   daten_ntx$Datum + years(10)
)
names(daten_ntx)[length(names(daten_ntx))] <- "follow_up"
# Irrelevante Spalten löschen
daten_ntx$Patienten_ID <- NULL
daten_ntx$Familienname <- NULL
daten_ntx$Vorname <- NULL
daten_ntx$Bezeichnung <- NULL
daten_ntx$TX_Status <- relevel(daten_ntx$TX_Status, ref = "ohne")
# T_Ende_x können in eine Variable subsumiert werden, weil jeder Patient
# innerhalb des follow-up, wenn er ein Transplantatverlust hatte, genau ein 
# Transplantat verlor.
daten_ntx <- daten_ntx %>% 
  mutate(Transplantatversagen = case_when(
    !is.na(daten_ntx$T_Ende_1) ~ daten_ntx$T_Ende_1,
    !is.na(daten_ntx$T_Ende_2) ~ daten_ntx$T_Ende_2,
    !is.na(daten_ntx$T_Ende_3) ~ daten_ntx$T_Ende_3,
    !is.na(daten_ntx$T_Ende_4) ~ daten_ntx$T_Ende_4,
    !is.na(daten_ntx$T_Ende_5) ~ daten_ntx$T_Ende_5,
    !is.na(daten_ntx$T_Ende_6) ~ daten_ntx$T_Ende_6
    
  ))
daten_ntx$T_Ende_1 <- NULL
daten_ntx$T_Ende_2 <- NULL
daten_ntx$T_Ende_3 <- NULL
daten_ntx$T_Ende_4 <- NULL
daten_ntx$T_Ende_5 <- NULL
daten_ntx$T_Ende_6 <- NULL
############################ speichern und bereinigen #########################
# remove all tmp_* data and variables
rm(list = ls()[grep(pattern = "tmp_[\\w\\W]*", ls())])
rm(list = ls()[grep(pattern = "path[\\w\\W]*", ls())])
rm(list = ls()[grep(pattern = "names[\\w\\W]*", ls())])
rm(list = ls()[grep(pattern = "colnames[\\w\\W]*", ls())])
rm(list = ls()[grep(pattern = "namen[\\w\\W]*", ls())])
# save data as R
save(daten_iga, file = "06_data/daten_IgA.Rdata")
save(daten_ntx, file = "06_data/daten_NTX.Rdata")
