## Final Dataset Creation

#Import Metabolite Dataset (batch-normalized, imputation)
library(readr)
CLSA_COMBINED_NORMDATAALL_v1 <- read_csv("CLSA COMBINED_NORMDATAALL_v1.CSV")
View(CLSA_COMBINED_NORMDATAALL_v1)

# Create final metabolites dataset with only 2 metabolites of interest, o methyl ascorbate and ascorbic acid 2 sulfate
## 100009329- Ascorbic Acid 2 Sulfate, 100003258- O Methyl Ascorbate, ADM_METABOLON_COM- linking key with phenotype data

library(dplyr)
metabvars <- c("100003258", "100009329", "ADM_METABOLON_COM")
metab_final<-CLSA_COMBINED_NORMDATAALL_v1[metabvars]

#Load environment data 
CLSA_Full<- read_csv("180911_UOttawa_MHRoyGagnon_Baseline_CoPv7_DRU_March2022.csv")
clsa_final<-subset(CLSA_Full, select=c("AGE_NMBR_COM", "ICQ_GLAUC_COM", "ED_HIGH_COM", "ED_OTED_COM", "SDC_CULT_WH_COM", "SDC_CULT_ZH_COM", 
                                       "SDC_CULT_SA_COM", "SDC_CULT_BL_COM", "SDC_CULT_FP_COM", "BLD_FD24_HR_COM",
                                       "SDC_CULT_LA_COM", "SDC_CULT_SE_COM", "SDC_CULT_AR_COM",
                                       "SDC_CULT_WA_COM", "SDC_CULT_JA_COM", "SDC_CULT_KO_COM","SDC_CULT_OT_COM", "SDC_CULT_DK_NA_COM",
                                       "SDC_CULT_REFUSED_COM", "SMK_100CG_COM", "SMK_CURRCG_COM", "ALC_EVER_COM", 
                                       "ALC_FREQ_COM", "ALC_RDWE_NB_COM", "ALC_RDWD_NB_COM", "ALC_WHWD_NB_COM",
                                       "ALC_WHWE_NB_COM", "ALC_BRWD_NB_COM", "ALC_BRWE_NB_COM", "ALC_LQWD_NB_COM", "ALC_LQWE_NB_COM",
                                       "ALC_OTWD_NB_COM", "ALC_OTWE_NB_COM", "TON_IOPCC_R_COM", "TON_IOPCC_L_COM", 
                                       "MEDI_ID_DIN_SP2_1_COM","MEDI_ID_DIN_SP2_2_COM","MEDI_ID_DIN_SP2_3_COM", "WGHTS_PROV_COM",
                                       "MEDI_ID_DIN_SP2_4_COM","MEDI_ID_DIN_SP2_5_COM","MEDI_ID_DIN_SP2_6_COM","MEDI_ID_DIN_SP2_7_COM",
                                       "MEDI_ID_DIN_SP2_8_COM","MEDI_ID_DIN_SP2_9_COM","MEDI_ID_DIN_SP2_10_COM","MEDI_ID_DIN_SP2_11_COM",
                                       "MEDI_ID_DIN_SP2_12_COM","MEDI_ID_DIN_SP2_13_COM", "MEDI_ID_DIN_SP2_14_COM","MEDI_ID_DIN_SP2_15_COM",
                                       "MEDI_ID_DIN_SP2_16_COM","MEDI_ID_DIN_SP2_17_COM","MEDI_ID_DIN_SP2_18_COM","MEDI_ID_DIN_SP2_19_COM",
                                       "MEDI_ID_DIN_SP2_20_COM","MEDI_ID_DIN_SP2_21_COM","MEDI_ID_DIN_SP2_22_COM","MEDI_ID_DIN_SP2_23_COM",
                                       "MEDI_ID_DIN_SP2_24_COM","MEDI_ID_DIN_SP2_25_COM","MEDI_ID_DIN_SP2_26_COM","MEDI_ID_DIN_SP2_27_COM",
                                       "MEDI_ID_DIN_SP2_28_COM","MEDI_ID_DIN_SP2_29_COM","MEDI_ID_DIN_SP2_30_COM","MEDI_ID_DIN_SP2_31_COM",
                                       "MEDI_ID_DIN_SP2_32_COM","MEDI_ID_DIN_SP2_33_COM","MEDI_ID_DIN_SP2_34_COM","MEDI_ID_DIN_SP2_35_COM",
                                       "MEDI_ID_DIN_SP2_36_COM","MEDI_ID_DIN_SP2_37_COM","MEDI_ID_DIN_SP2_38_COM","MEDI_ID_DIN_SP2_39_COM",
                                       "MEDI_ID_DIN_SP2_40_COM", "NUT_FRUT_NB_COM", "NUT_PURE_NB_COM", "NUT_GREEN_NB_COM", "NUT_CRRT_NB_COM", 
                                       "NUT_VGOT_NB_COM", "SEX_ASK_COM", "BLD_CAFF24_COM", "ADM_METABOLON_COM", "ADM_GWAS3_COM", "INC_TOT_COM", "HWT_DBMI_COM", "BLD_FD24_HR_COM"))

#Merge final environment dataset with metabolite dataset to create final dataset], total sample is 9992

vitc_final<-merge(metab_final, clsa_final, by="ADM_METABOLON_COM")
colnames(vitc_final)[2]= 'O-METHYL'
colnames(vitc_final)[3]= 'Ascorbic'

## Create Environmental Variables 

#Low Education 
## 0- higher than a Bachelor's degree 1- Bachelor's degree 2- less than a bachelor's degree 

as.numeric(vitc_final$ED_HIGH_COM)
vitc_final$lowed = with(vitc_final, 
                        ifelse( ED_HIGH_COM== "01" | ED_HIGH_COM== "02" | ED_HIGH_COM== "03" |
                                  ED_HIGH_COM== "04" |  ED_OTED_COM == 2, 2,
                                ifelse (ED_HIGH_COM== "05", 1,
                                        ifelse (ED_HIGH_COM== "06", 0, 
                                                ifelse (ED_HIGH_COM=="97" | ED_HIGH_COM== "98" | ED_OTED_COM == 8|
                                                          ED_OTED_COM == 9, NA, NA)))))
head(select(vitc_final, lowed, ED_HIGH_COM, ED_OTED_COM), 150)

#Creation of Overall Consumption Alcohol Variable: 
## 0- Never drinker, 1- Occasional 2- Weekly 3- Daily 

vitc_final$ALC_FREQ_COM<-as.numeric(vitc_final$ALC_FREQ_COM)
is.numeric(vitc_final$ALC_FREQ_COM)
vitc_final$altalc = with(vitc_final, 
                         ifelse(ALC_EVER_COM==2, 0,
                                ifelse(ALC_FREQ_COM==96| ALC_FREQ_COM==07| ALC_FREQ_COM==06
                                       | ALC_FREQ_COM==05, 1,
                                       ifelse(ALC_FREQ_COM==04 | ALC_FREQ_COM==03| ALC_FREQ_COM==02, 2,
                                              ifelse(ALC_FREQ_COM==01, 3,
                                                     ifelse(ALC_FREQ_COM==98 | ALC_FREQ_COM==99| ALC_EVER_COM==8, NA, NA))))))
head(select(vitc_final, altalc, ALC_FREQ_COM, ALC_EVER_COM), 50)

#Creation of the IOP Variable 

##If information available for both eyes, take the mean of both IOPs recorded, if only one available, use that as measurement
vitc_final$meaniop = with(vitc_final, 
                          ifelse(is.na(TON_IOPCC_R_COM), TON_IOPCC_L_COM,
                                 ifelse(is.na(TON_IOPCC_L_COM), TON_IOPCC_R_COM,
                                        (TON_IOPCC_L_COM+TON_IOPCC_R_COM)/2)))

head(select(vitc_final, meaniop, TON_IOPCC_L_COM, TON_IOPCC_R_COM), 50)

#Adjusting IOP for whether people were on IOP lowering drugs or not 

##create variable list with all IOP Meds

iopmedlistn<-c(02356279, 02402505, 02296055, 02315858, 02489775, 02458691, 02404052, 02249456, 
               02246285, 02522373, 02489260, 02395398, 02395142, 02522020, 02449226, 02400162, 02457210, 02421127, 
               02444666, 02428121, 02404591, 02504189, 02425149, 02413817, 02202492, 02202506, 02414287, 00545015, 02358328,
               02404389, 02436256, 02236876, 02248151, 02299615, 02296527, 00755834, 02415305, 02331624, 02238873, 
               01908448, 02301334, 02515377, 02248347, 02240113, 02258692, 02489635, 02278251, 02373041, 02373068, 00888354,
               02076306, 00000868, 00000884, 02457997, 02457539, 02453355, 02453347, 02453770, 02447800, 02489368, 02489570,
               02350939, 02324997, 02513285, 02514516, 02507811, 02437686, 02426935, 02454505, 02245882, 02148463, 02133326, 
               00042544, 02456230, 02515350, 02441659, 02341085, 02305429, 02316307, 02344351, 02367335, 02166712, 02166720,
               02413167, 02435411, 02254786, 02242275, 02242276, 00451207, 02171899, 02318008, 02216205, 02269090, 02429063,
               02484218, 02246619, 02231493)

##Create a flag variable where if any of the medication DIN columns contain any of the IOP medications, flag as 1 
vitc_final$IOP_meds<-0
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_1_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_2_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_3_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_4_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_5_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_6_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_7_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_8_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_9_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_10_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_11_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_12_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_13_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_14_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_15_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_16_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_17_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_18_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_19_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_20_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_21_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_22_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_23_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_24_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_25_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_26_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_27_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_28_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_29_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_30_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_31_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_32_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_33_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_34_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_35_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_36_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_37_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_38_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_39_COM %in% iopmedlistn] <- 1
vitc_final$IOP_meds[vitc_final$MEDI_ID_DIN_SP2_40_COM %in% iopmedlistn] <- 1

table(vitc_final$IOP_meds)

##Adjust IOP measurement for those who are taking IOP lowering meds by dividing by 0.7 
vitc_final$IOP_adj = ifelse(vitc_final$IOP_meds== 1, vitc_final$meaniop/0.7, vitc_final$meaniop) 

#Creation of Fruit Data

vitc_final$frut = with(vitc_final,
                       ifelse(NUT_FRUT_NB_COM== 9996, 0, 
                              ifelse(NUT_FRUT_NB_COM== 7777 | NUT_FRUT_NB_COM==9998| NUT_FRUT_NB_COM==9999, NA, NUT_FRUT_NB_COM)))

vitc_final$frut_juice = with(vitc_final,
                             ifelse(NUT_PURE_NB_COM== 9996, 0, 
                                    ifelse(NUT_PURE_NB_COM== 7777 | NUT_PURE_NB_COM==9998| NUT_PURE_NB_COM==9999, NA, NUT_PURE_NB_COM)))

vitc_final$total_frut = (vitc_final$frut + vitc_final$frut_juice)

#Creation of Vegetable Data, measurement is times/day consumed 
##Created by combining the number of greens, carrots and other vegetables consumed 

vitc_final$greens = with(vitc_final,
                         ifelse(NUT_GREEN_NB_COM== 9996, 0, 
                                ifelse(NUT_GREEN_NB_COM== 7777 | NUT_GREEN_NB_COM==9998| NUT_GREEN_NB_COM==9999, NA, NUT_GREEN_NB_COM)))

vitc_final$carrots = with(vitc_final,
                          ifelse(NUT_CRRT_NB_COM== 9996, 0, 
                                 ifelse(NUT_CRRT_NB_COM== 7777 | NUT_CRRT_NB_COM==9998| NUT_CRRT_NB_COM==9999, NA, NUT_CRRT_NB_COM)))
vitc_final$vgot = with(vitc_final,
                       ifelse(NUT_VGOT_NB_COM== 9996, 0, 
                              ifelse(NUT_VGOT_NB_COM== 7777 | NUT_VGOT_NB_COM==9998| NUT_VGOT_NB_COM==9999, NA, NUT_VGOT_NB_COM)))

vitc_final$gen_veg= (with(vitc_final, vgot+carrots+greens))

#Creation of Smoking Variable:
## 0- Never smoker, 1- Former smoker, 2- Current smoker 

vitc_final$newsmoke = with (vitc_final,
                            ifelse(SMK_100CG_COM==2, 0,
                                   ifelse(SMK_100CG_COM==1& SMK_CURRCG_COM==3, 1,
                                          ifelse(SMK_100CG_COM==1 & (SMK_CURRCG_COM==1 | SMK_CURRCG_COM==2), 2,
                                                 ifelse(SMK_100CG_COM==8| SMK_100CG_COM==9| SMK_CURRCG_COM==8| 
                                                          SMK_CURRCG_COM==9, NA,NA)))))
head(select(vitc_final, newsmoke, SMK_100CG_COM, SMK_CURRCG_COM), 50)


#Export phenotype dataset to cluster for storing 
write.csv(vitc_final, "rough_vitc.csv")

## create final dataset for Gxe and merge with SNP data and create additional covariates 
### Import Datasets with Environmental Factors and Genetic Factors ###

ASC_Chr10_SNP<-read.table(file="Asc_SNPs_Chr10.raw", header=T)
ASC_Chr16_SNP<-read.table(file="Asc_SNPs_Chr16.raw", header=T)
OMethyl_SNP<-read.table(file="O-Methyl_SNPs.raw", header=T)


#Merge Accounts

##Variables to keep from Environmental Exposures:

Enviro_Exposure<-subset(vitc_final, select=c("ADM_GWAS3_COM", "O-METHYL", "Ascorbic", "AGE_NMBR_COM", "lowed", "newsmoke", 
                                             "altalc", "gen_veg", "IOP_adj", "total_frut", "INC_TOT_COM", "HWT_DBMI_COM", "WGHTS_PROV_COM", "BLD_FD24_HR_COM"))

#SNP Datasets, keep linker and SNP info
## Only extracted individuals SNP data that had both genetic
## and metabolic data, were of European ancestry and had no missing information on last meal or drink n= 9004
ASC_Chr10_SNP2<-subset(ASC_Chr10_SNP, select=c("FID", "chr10.87718088.G.A_A"))
ASC_Chr16_SNP2<-subset(ASC_Chr16_SNP, select=c("FID", "chr16.30343759.C.T_T","chr16.30374516.T.C_T"))
OMethyl_SNP2<-subset(OMethyl_SNP, select=c("FID", "chr22.19953481.A.G_A", "chr22.19959676.CTCT.C_C", "chr22.19963748.G.A_G"))

#Other covariates
sqc_info<-read.delim("clsa_sqc_v3.txt",sep=" ",header=TRUE)
Metab_Variables<-subset(sqc_info, select=c("ADM_GWAS_COM", "chromosomal.sex","batch","in.kinship",
                                                       "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10" ))

#Rename linking columns to all have FID 
names(Enviro_Exposure)[names(Enviro_Exposure)== 'ADM_GWAS3_COM'] <-'FID'
names(Metab_Variables)[names(Metab_Variables)== 'ADM_GWAS_COM'] <-'FID'

#Merge all datasets
Final_Dataset<-merge(Enviro_Exposure, Metab_Variables, by="FID")
Final_Dataset<-merge(Final_Dataset, ASC_Chr10_SNP2, by="FID")    
Final_Dataset<-merge(Final_Dataset, ASC_Chr16_SNP2, by="FID") 
Gxe<-merge(Final_Dataset, OMethyl_SNP2, by="FID")    


#Creation of Income Variable 
## 1- <20 000, 2- 20 000- 50 000, 3- 50 000-100 000, 4- 100 000-150 000, 5- >:150 000, 6- Don't know/refused 
Gxe$Income<- ifelse(Gxe$INC_TOT_COM == 8 | Gxe$INC_TOT_COM == 9 | Gxe$INC_TOT_COM == 7, 6, Gxe$INC_TOT_COM)

#Creation of BMI Variable 
Gxe$BMI_Cat2<- ifelse(Gxe$HWT_DBMI_COM < 20.0, 1, 
                      ifelse(Gxe$HWT_DBMI_COM >= 20.0 & Gxe$HWT_DBMI_COM < 25.0, 2,
                             ifelse(Gxe$HWT_DBMI_COM >= 25.0 & Gxe$HWT_DBMI_COM <30, 3, 
                                    ifelse (Gxe$HWT_DBMI_COM >= 30, 4, NA))))

#Remove in Kinship
Gxe<-Gxe[!Gxe$in.kinship == 1,]
#8551 obs
#Remove missing or outlier IOP values
Gxe<-Gxe[!is.na(Gxe$IOP_adj),]
#8189 obs
## IOP Extreme outlier= larger than 60 (3 outliers)
Gxe<-Gxe[!Gxe$IOP_adj >60,]
#8186 obs

### Extreme Outliers Fruit/Vegetable (for fruit, 1 participant with 20, extreme outlier?)
## Make outlier variables NA ##
Gxe$total_frut<- 
  ifelse(Gxe$total_frut >15, NA, Gxe$total_frut)

#Remove_Missing_Sex
Gxe<- Gxe[!Gxe$chromosomal.sex== 0,]
#8179 obs 

Gxe$altalc_cat<- as.factor(Gxe$altalc)
Gxe$newsmoke_cat<- as.factor(Gxe$newsmoke)

### Code Additonal Covariates ###

clsa_final<-subset(CLSA_Full, select=c( "DIA_DIAB_COM", "CCC_HBP_COM", "DIA_TYPE_COM", "BP_SYSTOLIC_AVG_COM", "BP_DIASTOLIC_AVG_COM", "NUT_BRD_NB_COM", 
                                        "NUT_FRUT_NB_COM","NUT_PURE_NB_COM", "NUT_FBR_NB_COM", "NUT_MEAT_NB_COM", "NUT_MTOT_NB_COM", "NUT_CHCK_NB_COM",
                                        "NUT_FISH_NB_COM", "NUT_SASG_NB_COM", "NUT_PATE_NB_COM", "NUT_SAUC_NB_COM", "NUT_O3EG_NB_COM", "NUT_EGGS_NB_COM",
                                        "NUT_LEGM_NB_COM", "NUT_NUTS_NB_COM", "NUT_GREEN_NB_COM", "NUT_CRRT_NB_COM", "NUT_VGOT_NB_COM", "NUT_PTTO_NB_COM", 
                                        "NUT_FRIE_NB_COM", "NUT_LWCS_NB_COM", "NUT_CHSE_NB_COM", "NUT_LWYG_NB_COM", "NUT_YOGR_NB_COM", "NUT_CALC_NB_COM",
                                        "NUT_DAIR_NB_COM", "NUT_SALT_NB_COM", "NUT_DSRT_NB_COM", "NUT_CHOC_NB_COM", "NUT_BTTR_NB_COM", "NUT_DRSG_NB_COM",
                                        "NUT_CAJC_NB_COM", "NUT_CAML_NB_COM", "NUT_WHML_NB_COM", "NUT_LFML_NB_COM", "NUT_CADR_NB_COM",
                                        "ADM_GWAS3_COM"))
Gxe$ADM_GWAS3_COM <- Gxe$FID
Gxe<- merge(Gxe,clsa_final, by="ADM_GWAS3_COM")

### Total caloric intake #### 

# Bread(*), cereal(*), red(*), other_meat(*), chicken(*), fish(*), sasg(*), pate(*), sauce(*), o3eg(*), eggs(*), legm(*), nuts(*), frut(*), greens(*), ptto(*), frie*, carrots(*), 
# vgot(*), lwcs*, chse*, lwyg*, yogr*, calc*, dairydsrt*, salty*, dsrt*, choc*, butter*, drsg*, cajc*, fruit_juice(*), caml*, whml*, ifml*, cadr, 

#Code Diet Frequency Variables based on Dr. Freeman's Code 

# Bread 
Gxe$bread = with (Gxe, 
                   ifelse(NUT_BRD_NB_COM == 9996, 0, 
                          ifelse(NUT_BRD_NB_COM== 7777 | NUT_BRD_NB_COM == 9998 | NUT_BRD_NB_COM == 9999, NA, NUT_BRD_NB_COM)))
# Cereal 
Gxe$cereal = with (Gxe, 
                    ifelse(NUT_FBR_NB_COM == 9996, 0, 
                           ifelse(NUT_FBR_NB_COM== 7777 | NUT_FBR_NB_COM == 9998 | NUT_FBR_NB_COM == 9999, NA, NUT_FBR_NB_COM)))

# Red Meat  
Gxe$red = with (Gxe, 
                 ifelse(NUT_MEAT_NB_COM == 9996, 0, 
                        ifelse(NUT_MEAT_NB_COM== 7777 | NUT_MEAT_NB_COM == 9998 | NUT_MEAT_NB_COM == 9999, NA, NUT_MEAT_NB_COM)))

# Other Meat  
Gxe$other_mt = with (Gxe, 
                      ifelse(NUT_MTOT_NB_COM == 9996, 0, 
                             ifelse(NUT_MTOT_NB_COM== 7777 | NUT_MTOT_NB_COM == 9998 | NUT_MTOT_NB_COM == 9999, NA, NUT_MTOT_NB_COM)))

# Chicken  
Gxe$chck = with (Gxe, 
                  ifelse(NUT_CHCK_NB_COM == 9996, 0, 
                         ifelse(NUT_CHCK_NB_COM== 7777 | NUT_CHCK_NB_COM == 9998 | NUT_CHCK_NB_COM == 9999, NA, NUT_CHCK_NB_COM)))
# Fish  
Gxe$fish = with (Gxe, 
                  ifelse(NUT_FISH_NB_COM == 9996, 0, 
                         ifelse(NUT_FISH_NB_COM== 7777 | NUT_FISH_NB_COM == 9998 | NUT_FISH_NB_COM == 9999, NA, NUT_FISH_NB_COM)))
# Sausage  
Gxe$sasg = with (Gxe, 
                  ifelse(NUT_SASG_NB_COM == 9996, 0, 
                         ifelse(NUT_SASG_NB_COM== 7777 | NUT_SASG_NB_COM == 9998 | NUT_SASG_NB_COM == 9999, NA, NUT_SASG_NB_COM)))
# Pate 
Gxe$pate = with (Gxe, 
                  ifelse(NUT_PATE_NB_COM == 9996, 0, 
                         ifelse(NUT_PATE_NB_COM== 7777 | NUT_PATE_NB_COM == 9998 | NUT_PATE_NB_COM == 9999, NA, NUT_PATE_NB_COM)))
# Sauce
Gxe$sauce = with (Gxe, 
                   ifelse(NUT_SAUC_NB_COM == 9996, 0, 
                          ifelse(NUT_SAUC_NB_COM== 7777 | NUT_SAUC_NB_COM == 9998 | NUT_SAUC_NB_COM == 9999, NA, NUT_SAUC_NB_COM)))
# O3 Egg
Gxe$o3eg = with (Gxe, 
                  ifelse(NUT_O3EG_NB_COM == 9996, 0, 
                         ifelse(NUT_O3EG_NB_COM== 7777 | NUT_O3EG_NB_COM == 9998 | NUT_O3EG_NB_COM == 9999, NA, NUT_O3EG_NB_COM)))
# Egg
Gxe$eggs = with (Gxe, 
                  ifelse(NUT_EGGS_NB_COM == 9996, 0, 
                         ifelse(NUT_EGGS_NB_COM== 7777 | NUT_EGGS_NB_COM == 9998 | NUT_EGGS_NB_COM == 9999, NA, NUT_EGGS_NB_COM)))
# Legumes
Gxe$legm = with (Gxe, 
                  ifelse(NUT_LEGM_NB_COM == 9996, 0, 
                         ifelse(NUT_LEGM_NB_COM== 7777 | NUT_LEGM_NB_COM == 9998 | NUT_LEGM_NB_COM == 9999, NA, NUT_LEGM_NB_COM)))
# Nuts
Gxe$nuts = with (Gxe, 
                  ifelse(NUT_NUTS_NB_COM == 9996, 0, 
                         ifelse(NUT_NUTS_NB_COM== 7777 | NUT_NUTS_NB_COM == 9998 | NUT_NUTS_NB_COM == 9999, NA, NUT_NUTS_NB_COM)))

# Fruit
Gxe$frut = with(Gxe,
                 ifelse(NUT_FRUT_NB_COM== 9996, 0, 
                        ifelse(NUT_FRUT_NB_COM== 7777 | NUT_FRUT_NB_COM==9998| NUT_FRUT_NB_COM==9999, NA, NUT_FRUT_NB_COM)))
# Greens
Gxe$greens = with(Gxe,
                   ifelse(NUT_GREEN_NB_COM== 9996, 0, 
                          ifelse(NUT_GREEN_NB_COM== 7777 | NUT_GREEN_NB_COM==9998| NUT_GREEN_NB_COM==9999, NA, NUT_GREEN_NB_COM)))
# Potatoes
Gxe$ptto = with(Gxe,
                 ifelse(NUT_PTTO_NB_COM== 9996, 0, 
                        ifelse(NUT_PTTO_NB_COM== 7777 | NUT_PTTO_NB_COM==9998| NUT_PTTO_NB_COM==9999, NA, NUT_PTTO_NB_COM)))
# Carrots
Gxe$carrots = with(Gxe,
                    ifelse(NUT_CRRT_NB_COM== 9996, 0, 
                           ifelse(NUT_CRRT_NB_COM== 7777 | NUT_CRRT_NB_COM==9998| NUT_CRRT_NB_COM==9999, NA, NUT_CRRT_NB_COM)))
# Fries 
Gxe$frie = with(Gxe,
                 ifelse(NUT_FRIE_NB_COM== 9996, 0, 
                        ifelse(NUT_FRIE_NB_COM== 7777 | NUT_FRIE_NB_COM==9998| NUT_FRIE_NB_COM==9999, NA, NUT_FRIE_NB_COM)))
# Other Vegetables
Gxe$vgot = with(Gxe,
                 ifelse(NUT_VGOT_NB_COM== 9996, 0, 
                        ifelse(NUT_VGOT_NB_COM== 7777 | NUT_VGOT_NB_COM==9998| NUT_VGOT_NB_COM==9999, NA, NUT_VGOT_NB_COM)))
# Lw CS 
Gxe$lwcs = with(Gxe,
                 ifelse(NUT_LWCS_NB_COM== 9996, 0, 
                        ifelse(NUT_LWCS_NB_COM== 7777 | NUT_LWCS_NB_COM==9998| NUT_LWCS_NB_COM==9999, NA, NUT_LWCS_NB_COM)))
# Cheese
Gxe$chse = with(Gxe,
                 ifelse(NUT_CHSE_NB_COM== 9996, 0, 
                        ifelse(NUT_CHSE_NB_COM== 7777 | NUT_CHSE_NB_COM==9998| NUT_CHSE_NB_COM==9999, NA, NUT_CHSE_NB_COM)))
# LWYG
Gxe$lwyg = with(Gxe,
                 ifelse(NUT_LWYG_NB_COM== 9996, 0, 
                        ifelse(NUT_LWYG_NB_COM== 7777 | NUT_LWYG_NB_COM==9998| NUT_LWYG_NB_COM==9999, NA, NUT_LWYG_NB_COM)))
# Yogurt
Gxe$yogr = with(Gxe,
                 ifelse(NUT_YOGR_NB_COM== 9996, 0, 
                        ifelse(NUT_YOGR_NB_COM== 7777 | NUT_YOGR_NB_COM==9998| NUT_YOGR_NB_COM==9999, NA, NUT_YOGR_NB_COM)))
# Calcium 
Gxe$calc = with(Gxe,
                 ifelse(NUT_CALC_NB_COM== 9996, 0, 
                        ifelse(NUT_CALC_NB_COM== 7777 | NUT_CALC_NB_COM==9998| NUT_CALC_NB_COM==9999, NA, NUT_CALC_NB_COM)))
# Dairy Desert  
Gxe$dairydsrt = with(Gxe,
                      ifelse(NUT_DAIR_NB_COM== 9996, 0, 
                             ifelse(NUT_DAIR_NB_COM== 7777 | NUT_DAIR_NB_COM==9998| NUT_DAIR_NB_COM==9999, NA, NUT_DAIR_NB_COM)))
# Salty Snacks
Gxe$salty = with(Gxe,
                  ifelse(NUT_SALT_NB_COM== 9996, 0, 
                         ifelse(NUT_SALT_NB_COM== 7777 | NUT_SALT_NB_COM==9998| NUT_SALT_NB_COM==9999, NA, NUT_SALT_NB_COM)))
# Desert
Gxe$dsrt = with(Gxe,
                 ifelse(NUT_DSRT_NB_COM== 9996, 0, 
                        ifelse(NUT_DSRT_NB_COM== 7777 | NUT_DSRT_NB_COM==9998| NUT_DSRT_NB_COM==9999, NA, NUT_DSRT_NB_COM)))
# Chocolate 
Gxe$choc = with(Gxe,
                 ifelse(NUT_CHOC_NB_COM== 9996, 0, 
                        ifelse(NUT_CHOC_NB_COM== 7777 | NUT_CHOC_NB_COM==9998| NUT_CHOC_NB_COM==9999, NA, NUT_CHOC_NB_COM)))
# Butter 
Gxe$butter = with(Gxe,
                   ifelse(NUT_BTTR_NB_COM== 9996, 0, 
                          ifelse(NUT_BTTR_NB_COM== 7777 | NUT_BTTR_NB_COM==9998| NUT_BTTR_NB_COM==9999, NA, NUT_BTTR_NB_COM)))
# Dressing 
Gxe$drsg = with(Gxe,
                 ifelse(NUT_DRSG_NB_COM== 9996, 0, 
                        ifelse(NUT_DRSG_NB_COM== 7777 | NUT_DRSG_NB_COM==9998| NUT_DRSG_NB_COM==9999, NA, NUT_DRSG_NB_COM)))
# Dressing 
Gxe$drsg = with(Gxe,
                 ifelse(NUT_DRSG_NB_COM== 9996, 0, 
                        ifelse(NUT_DRSG_NB_COM== 7777 | NUT_DRSG_NB_COM==9998| NUT_DRSG_NB_COM==9999, NA, NUT_DRSG_NB_COM)))
# CAJC 
Gxe$cajc = with(Gxe,
                 ifelse(NUT_CAJC_NB_COM== 9996, 0, 
                        ifelse(NUT_CAJC_NB_COM== 7777 | NUT_CAJC_NB_COM==9998| NUT_CAJC_NB_COM==9999, NA, NUT_CAJC_NB_COM)))
#Fruit Juice 
Gxe$fruit_juice = with(Gxe,
                        ifelse(NUT_PURE_NB_COM== 9996, 0, 
                               ifelse(NUT_PURE_NB_COM== 7777 | NUT_PURE_NB_COM==9998| NUT_PURE_NB_COM==9999, NA, NUT_PURE_NB_COM)))
# CAML 
Gxe$caml = with(Gxe,
                 ifelse(NUT_CAML_NB_COM== 9996, 0, 
                        ifelse(NUT_CAML_NB_COM== 7777 | NUT_CAML_NB_COM==9998| NUT_CAML_NB_COM==9999, NA, NUT_CAML_NB_COM)))
# Whole Milk 
Gxe$whml = with(Gxe,
                 ifelse(NUT_WHML_NB_COM== 9996, 0, 
                        ifelse(NUT_WHML_NB_COM== 7777 | NUT_WHML_NB_COM==9998| NUT_WHML_NB_COM==9999, NA, NUT_WHML_NB_COM)))

# Low fat Milk  
Gxe$lfml = with(Gxe,
                 ifelse(NUT_LFML_NB_COM== 9996, 0, 
                        ifelse(NUT_LFML_NB_COM== 7777 | NUT_LFML_NB_COM==9998| NUT_LFML_NB_COM==9999, NA, NUT_LFML_NB_COM)))
# CADR
Gxe$cadr = with(Gxe,
                 ifelse(NUT_CADR_NB_COM== 9996, 0, 
                        ifelse(NUT_CADR_NB_COM== 7777 | NUT_CADR_NB_COM==9998| NUT_CADR_NB_COM==9999, NA, NUT_CADR_NB_COM)))


# Calculat energy intake of Each Variable 

Gxe$bread_e = Gxe$bread*127.07
Gxe$cereal_e = 435.07*Gxe$cereal 
Gxe$red_e = Gxe$red*284.2
Gxe$other_mt_e = Gxe$other_mt*266.88
Gxe$chck_e = Gxe$chck*334.64
Gxe$fish_e = Gxe$fish*223.3
Gxe$sasg_e= Gxe$sasg*249.71
Gxe$pate_e = Gxe$pate*176
Gxe$sauce_e = Gxe$sauce*36.27
Gxe$o3eg_e = Gxe$o3eg*150
Gxe$eggs_e = Gxe$eggs*187
Gxe$legm_e = Gxe$legm*141.3
Gxe$nuts_e = Gxe$nuts*173.38
Gxe$fruit_e = Gxe$frut*95
Gxe$greens_e = Gxe$greens*13
Gxe$ptto_e= Gxe$ptto*137.25 
Gxe$frie_e = Gxe$frie*350.63
Gxe$carrots_e = Gxe$carrots*18.48
Gxe$vgot_e = Gxe$vgot*11.29
Gxe$lwcs_e = Gxe$lwcs*66.04
Gxe$chse_e = Gxe$chse*151.13
Gxe$lwyg_e = Gxe$lwyg*124.5
Gxe$yogr_e = Gxe$yogr*145.5
Gxe$calc_e = Gxe$calc*124.5
Gxe$dair_e = Gxe$dairydsrt*208
Gxe$salty_e = Gxe$salty*186.19
Gxe$dsrt_e = Gxe$dsrt*285.75
Gxe$choc_e = Gxe$choc*104
Gxe$butter_e = Gxe$butter*52.75
Gxe$drsg_e = Gxe$drsg*51.2
Gxe$cajc_e = Gxe$cajc*86.48
Gxe$fruit_juice_e = Gxe$fruit_juice*72.69
Gxe$caml_e = Gxe$caml*81.47
Gxe$whml_e = Gxe$whml*118.44
Gxe$lfml_e = Gxe$lfml*81.47 
Gxe$cadr_e = Gxe$cadr*60.16 

# Sum Total Daily Energy Intake 

Gxe$ total_energy = (with(Gxe, bread_e  + cereal_e + red_e + other_mt_e + chck_e + fish_e  + sasg_e + pate_e + sauce_e + o3eg_e + eggs_e + legm_e + nuts_e +  fruit_e + 
                             greens_e + ptto_e + frie_e + carrots_e + vgot_e + lwcs_e + chse_e + lwyg_e+ yogr_e + calc_e + dair_e  + salty_e + dsrt_e + choc_e + butter_e + drsg_e  + 
                             cajc_e + fruit_juice_e + caml_e  + whml_e + lfml_e + cadr_e, na.rm=TRUE))

Gxe$total_energy<- rowSums(Gxe[, 116:151], na.rm=TRUE)

mean(Gxe$total_energy)


# Diabetes 
## 0 is no, 1 is yes 
Gxe$Diab = (with(Gxe, 
                  ifelse(Gxe$DIA_DIAB_COM == 2, 0, 
                         ifelse(Gxe$DIA_DIAB_COM == 1, 1,
                                ifelse(Gxe$DIA_DIAB_COM== 8 | Gxe$DIA_DIAB_COM== 9, NA, Gxe$DIA_DIAB_COM)))))

# Diabetes Type 
# 0- none, 1- type 1, 2- type 2, 3- neither 

Gxe$Diab_Type = (with(Gxe, 
                       ifelse(Gxe$Diab== 0, 0, 
                              ifelse(Gxe$DIA_TYPE_COM== 1, 1, 
                                     ifelse(Gxe$DIA_TYPE_COM== 2, 2, 
                                            ifelse(Gxe$DIA_TYPE_COM== 3, 3,
                                                   ifelse(Gxe$DIA_TYPE_COM== 8 | Gxe$DIA_TYPE_COM== 9, NA, Gxe$DIA_TYPE_COM)))))))

# Hypertension
# 0- no, 1- yes

Gxe$Hyper_doc = (with(Gxe,
                       ifelse(Gxe$CCC_HBP_COM == 2, 0, 
                              ifelse(Gxe$CCC_HBP_COM == 1, 1,
                                     ifelse(Gxe$CCC_HBP_COM== 8 | Gxe$CCC_HBP_COM== 9, NA, Gxe$CCC_HBP_COM)))))

Gxe$Hyper = (with(Gxe, 
                   ifelse(Gxe$Hyper_doc==1 | Gxe$BP_SYSTOLIC_AVG_COM >= 130 | Gxe$BP_DIASTOLIC_AVG_COM >= 80, 1,
                          ifelse(Gxe$Hyper_doc == 0 & Gxe$BP_SYSTOLIC_AVG_COM < 130 & Gxe$BP_DIASTOLIC_AVG_COM < 80, 0, NA))))


##Create Metabolite Values ##

#run loop for Ascorbic acid  
Gxe$log_ascorbic = log(Gxe$Ascorbic)
mean_asc<-mean(Gxe$log_ascorbic, na.rm = TRUE)
sd_asc<-sd(Gxe$log_ascorbic, na.rm = TRUE)


#Looking and quantifying the number of outliers in the log-transformed metabolite data: 
Tmina = mean_asc- (3*sd_asc)
Tmaxa= mean_asc+ (3*sd_asc)
Gxe$log_ascorbic<-  ifelse(Gxe$log_ascorbic< Tmina| Gxe$log_ascorbic > Tmaxa, NA,  
                            ifelse(is.na(Gxe$log_ascorbic), NA, Gxe$log_ascorbic))

#Standardize the variable without outliers to a mean of 0 and an sd of 1 
Gxe$ascorbic_standard= ((Gxe$log_ascorbic - mean(Gxe$log_ascorbic, na.rm=TRUE))/sd(Gxe$log_ascorbic, na.rm=TRUE))


#Repeat with O-methyl Ascorbate   
Gxe$log_omet = log(Gxe$`O-METHYL`)
mean_omet<-mean(Gxe$log_omet, na.rm = TRUE)
sd_omet<-sd(Gxe$log_omet, na.rm = TRUE)


#Looking and quantifying the number of outliers in the log-transformed metabolite data: 
Tmino = mean_omet- (3*sd_omet)
Tmaxo= mean_omet+ (3*sd_omet)
Gxe$log_omet<-  ifelse(Gxe$log_omet< Tmino| Gxe$log_omet > Tmaxo, NA,  
                        ifelse(is.na(Gxe$log_omet), NA, Gxe$log_omet))

#Standardize the variable without outliers to a mean of 0 and an sd of 1 
Gxe$omet_standard= ((Gxe$log_omet - mean(Gxe$log_omet, na.rm=TRUE))/sd(Gxe$log_omet, na.rm=TRUE))


#remove missing values of confounder variables for final dataset
Gxe4<- Gxe[!is.na(Gxe$BMI_Cat2),]
Gxe4<- Gxe4[!is.na(Gxe4$altalc_cat),]
Gxe4<- Gxe4[!is.na(Gxe4$newsmoke_cat),]
Gxe4<- Gxe4[!is.na(Gxe4$Hyper),]
Gxe4<- Gxe4[!is.na(Gxe4$Diab),]
Gxe4<- Gxe4[!is.na(Gxe4$total_energy),]
Gxe4<- Gxe4[!is.na(Gxe4$lowed),]







