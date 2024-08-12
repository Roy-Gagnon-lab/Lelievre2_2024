# Run linear Models that are adjusted for all covariates


#### Main Effect Models ####

#Fruit and IOP #
Gxe43<- Gxe4[!is.na(Gxe4$total_frut),]
M<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + total_energy,  Gxe43)
M3<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + total_energy + total_frut,  Gxe43)
summary(M3)
anova(M, M3, test="Chisq")

#Vegetable and IOP #
Gxe43<- Gxe4[!is.na(Gxe4$gen_veg),]
M<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + total_energy,  Gxe43)
M3<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + total_energy + gen_veg,  Gxe43)
summary(M3)
anova(M, M3, test="Chisq")

#Alcohol Category #
Gxe43<- Gxe4[!is.na(Gxe4$altalc_cat),]
M<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + total_energy,  Gxe43)
M3<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + total_energy,  Gxe43)
summary(M3)
anova(M, M3, test="Chisq")

#Smoking Category #
Gxe43<- Gxe4[!is.na(Gxe4$newsmoke_cat),]
M<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab + altalc_cat + total_energy,  Gxe43)
M3<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + total_energy,  Gxe43)
summary(M3)
anova(M, M3, test="Chisq")

#Chr22.19963748 #
Gxe43<- Gxe4[!is.na(Gxe4$chr22.19963748.G.A_G),]
M<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + total_energy
       + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,  Gxe43)
M3<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + chr22.19963748.G.A_G + total_energy
        + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,  Gxe43)
summary(M3)
anova(M, M3, test="Chisq")

#Chr10.8771088 #
Gxe43<- Gxe4[!is.na(Gxe4$chr10.87718088.G.A_A),]
M<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + total_energy
       + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,  Gxe43)
M3<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + chr10.87718088.G.A_A + total_energy
        + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,  Gxe43)
summary(M3)
anova(M, M3, test="Chisq")

#Chr16.30374516 #
Gxe43<- Gxe4[!is.na(Gxe4$chr16.30374516.T.C_T),]
M<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + total_energy
       + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,  Gxe43)
M3<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + chr16.30374516.T.C_T + total_energy
        + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,  Gxe43)
summary(M3)
anova(M, M3, test="Chisq")

#Chr22.19953481 #
Gxe43<- Gxe4[!is.na(Gxe4$chr22.19953481.A.G_A),]
M<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + total_energy
       + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,  Gxe43)
M3<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + chr22.19953481.A.G_A + total_energy
        + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,  Gxe43)
summary(M3)
anova(M, M3, test="Chisq")

#Chr16.3034759 #
Gxe43<- Gxe4[!is.na(Gxe4$chr16.30343759.C.T_T),]
M<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + total_energy
       + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,  Gxe43)
M3<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + chr16.30343759.C.T_T + total_energy
        + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,  Gxe43)
summary(M3)
anova(M, M3, test="Chisq")

#Chr22.19959676 #
Gxe43<- Gxe4[!is.na(Gxe4$chr22.19959676.CTCT.C_C),]
M<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + total_energy
       + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,  Gxe43)
M3<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + chr22.19959676.CTCT.C_C + total_energy
        + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,  Gxe43)
summary(M3)
anova(M, M3, test="Chisq")

#O-Methyl #
Gxe43<- Gxe4[!is.na(Gxe4$omet_standard),]
M<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab + newsmoke_cat + altalc_cat + total_energy + BLD_FD24_HR_COM,  Gxe43)
M3<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + BLD_FD24_HR_COM + total_energy + omet_standard,  Gxe43)
summary(M3)
anova(M, M3, test="Chisq")

#Ascorbic Acid #
Gxe43<- Gxe4[!is.na(Gxe4$ascorbic_standard),]
M<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab + newsmoke_cat + altalc_cat + total_energy + BLD_FD24_HR_COM,  Gxe43)
M3<- lm(IOP_adj ~ AGE_NMBR_COM + chromosomal.sex + lowed + Income + WGHTS_PROV_COM +  BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + BLD_FD24_HR_COM  + total_energy + ascorbic_standard,  Gxe43)
summary(M3)
anova(M, M3, test="Chisq")


######### Linear Models to test for associations with ascorbic acid and o-methylascorbate #############

#### O-Methyl Ascorbate ########
##Fruit and O-Methyl ##
Gxe4_Fruit_Omet<- Gxe4[!is.na(Gxe4$total_frut) & !is.na(Gxe4$omet_standard), ]
Model_frut_O <- lm(IOP_adj ~ AGE_NMBR_COM + omet_standard + chromosomal.sex + total_energy +
                     BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat 
                   +lowed + Income + BLD_FD24_HR_COM + WGHTS_PROV_COM + total_frut, Gxe4_Fruit_Omet)
summary(Model_frut_O)
Model_frut_Omet <- lm(IOP_adj ~ omet_standard + AGE_NMBR_COM + chromosomal.sex +
                        BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat+  total_energy 
                      +lowed + Income + BLD_FD24_HR_COM + WGHTS_PROV_COM+ total_frut+ total_frut*omet_standard, Gxe4_Fruit_Omet)
summary(Model_frut_Omet)

anova(Model_frut_O, Model_frut_Omet, test="Chisq")

## Vegetable and O-Methyl ##

Gxe4_Veg_Omet<- Gxe4[!is.na(Gxe4$gen_veg) & !is.na(Gxe4$omet_standard), ]
Model_Veg_O <- lm(IOP_adj ~ AGE_NMBR_COM + omet_standard + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat 
                  +lowed + Income + BLD_FD24_HR_COM +WGHTS_PROV_COM + gen_veg, Gxe4_Veg_Omet)
summary(Model_Veg_O)
Model_Veg_Omet <- lm(IOP_adj ~ omet_standard + AGE_NMBR_COM + chromosomal.sex +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + total_energy 
                       +lowed + Income + BLD_FD24_HR_COM + WGHTS_PROV_COM + gen_veg+ gen_veg*omet_standard, Gxe4_Veg_Omet)
summary(Model_Veg_Omet)

anova(Model_Veg_O, Model_Veg_Omet, test="Chisq")


## Alcohol and O-Methyl ##

Gxe4_Alc_<- Gxe4[!is.na(Gxe4$altalc) & !is.na(Gxe4$omet_standard), ]

Model_alc_O <- lm(IOP_adj ~ AGE_NMBR_COM + omet_standard + chromosomal.sex +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + total_energy 
                  +lowed + Income + BLD_FD24_HR_COM +  WGHTS_PROV_COM + altalc_cat, Gxe4_Alc_)
summary(Model_alc_O)

Model_Alc_Omet <- lm(IOP_adj ~ omet_standard + AGE_NMBR_COM + chromosomal.sex + 
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + total_energy 
                     +lowed + Income + BLD_FD24_HR_COM + WGHTS_PROV_COM + altalc_cat*omet_standard, Gxe4_Alc_)
summary(Model_Alc_Omet)

anova(Model_Alc_Omet, Model_alc_O, test="Chisq")


## Smoking and O-Methyl ##

Gxe4_Smk_<- Gxe4[!is.na(Gxe4$newsmoke) & !is.na(Gxe4$omet_standard), ]
Model_smk_o <- lm(IOP_adj ~ AGE_NMBR_COM + omet_standard + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + altalc_cat 
                  +lowed + Income + BLD_FD24_HR_COM +  WGHTS_PROV_COM + newsmoke_cat, Gxe4_Smk_)
summary(Model_smk_o)
Model_Smk_Omet <- lm(IOP_adj ~ omet_standard + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab + altalc_cat +
                       +lowed + Income + BLD_FD24_HR_COM +  WGHTS_PROV_COM  + newsmoke_cat*omet_standard, Gxe4_Smk_)
summary(Model_Smk_Omet)
anova(Model_smk_o, Model_Smk_Omet, test="Chisq")

#### Ascorbic Acid ########

##Fruit and Ascorbic Acid ##

Gxe4_Fruit_Omet<- Gxe4[!is.na(Gxe4$total_frut) & !is.na(Gxe4$ascorbic_standard), ]
Model_frut_A <- lm(IOP_adj ~ AGE_NMBR_COM + ascorbic_standard + chromosomal.sex + total_energy +
                     BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat 
                   +lowed + Income + BLD_FD24_HR_COM +  WGHTS_PROV_COM  + total_frut, Gxe4_Fruit_Omet)
summary(Model_frut_A)
Model_frut_Asc <- lm(IOP_adj ~ ascorbic_standard + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat 
                     +lowed + Income + BLD_FD24_HR_COM +  WGHTS_PROV_COM  + total_frut+ total_frut*ascorbic_standard, Gxe4_Fruit_Omet)
summary(Model_frut_Asc)
anova(Model_frut_A, Model_frut_Asc, test="Chisq")

## Vegetable and Ascorbic ##

Gxe4_Veg_Asc<- Gxe4[!is.na(Gxe4$gen_veg) & !is.na(Gxe4$ascorbic_standard), ]
Model_Veg_O <- lm(IOP_adj ~ AGE_NMBR_COM + ascorbic_standard + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat 
                  +lowed + Income + BLD_FD24_HR_COM +  WGHTS_PROV_COM + gen_veg, Gxe4_Veg_Asc)
summary(Model_Veg_O)
Model_Veg_Omet <- lm(IOP_adj ~ ascorbic_standard + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat +
                       +lowed + Income + BLD_FD24_HR_COM +  WGHTS_PROV_COM + gen_veg+ gen_veg*ascorbic_standard, Gxe4_Veg_Asc)
summary(Model_Veg_Omet)
anova(Model_Veg_O, Model_Veg_Omet, test="Chisq")

## Alcohol and Ascorbic ##

Gxe4_Alc_<- Gxe4[!is.na(Gxe4$altalc_cat) & !is.na(Gxe4$ascorbic_standard), ]
Model_Alc_a <- lm(IOP_adj ~ AGE_NMBR_COM + ascorbic_standard + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat 
                  +lowed + Income +  WGHTS_PROV_COM +  BLD_FD24_HR_COM + altalc_cat, Gxe4_Alc_)
summary(Model_Alc_a )
Model_Alc_asc <- lm(IOP_adj ~ ascorbic_standard + AGE_NMBR_COM + chromosomal.sex + total_energy +
                      BMI_Cat2 + Hyper + Diab  + newsmoke_cat 
                    +lowed + Income +  WGHTS_PROV_COM + BLD_FD24_HR_COM  + altalc_cat*ascorbic_standard, Gxe4_Alc_)
summary(Model_Alc_asc)
anova(Model_Alc_a, Model_Alc_asc, test="Chisq")

## Smoking and Ascorbic ##

Gxe4_Smk_<- Gxe4[!is.na(Gxe4$newsmoke) & !is.na(Gxe4$ascorbic_standard), ]
Model_smk_a <- lm(IOP_adj ~ AGE_NMBR_COM + ascorbic_standard + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + altalc_cat +
                    +lowed + Income +  WGHTS_PROV_COM + BLD_FD24_HR_COM + newsmoke_cat, Gxe4_Smk_)
summary(Model_smk_a)
Model_smk_asc <- lm(IOP_adj ~ ascorbic_standard + AGE_NMBR_COM + chromosomal.sex + total_energy +
                      BMI_Cat2 + Hyper + Diab  + altalc_cat +
                      +lowed + Income +  WGHTS_PROV_COM + BLD_FD24_HR_COM  + newsmoke_cat*ascorbic_standard, Gxe4_Smk_)
summary(Model_smk_asc)
anova(Model_smk_a, Model_smk_asc, test="Chisq")

######### Linear Models for Gxe Interactions #############
#### chr22.19963748.G.A_G ########

##Fruit and chr22.19963748.G.A_G ##

Gxe4_Fruit_chr22G<- Gxe4[!is.na( Gxe4$total_frut) & !is.na( Gxe4$chr22.19963748.G.A_G), ]
Model_frut_A <- lm(IOP_adj ~ AGE_NMBR_COM + chr22.19963748.G.A_G + chromosomal.sex + total_energy +
                     BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                   +lowed + Income +   WGHTS_PROV_COM  + total_frut, Gxe4_Fruit_chr22G)
summary(Model_frut_A)
Model_frut_Asc <- lm(IOP_adj ~ chr22.19963748.G.A_G + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                     +lowed + Income +   WGHTS_PROV_COM  + total_frut+ total_frut*chr22.19963748.G.A_G, Gxe4_Fruit_chr22G)
summary(Model_frut_Asc)
anova(Model_frut_A, Model_frut_Asc, test="LRT")


Gxe4_Fruit_chr22G<- Gxe4[!is.na( Gxe4$total_frut) & !is.na( Gxe4$chr22.19963748.G.A_G), ]
Model_frut_A <- lm(IOP_adj ~ AGE_NMBR_COM + chr22.19963748.G.A_G + chromosomal.sex + total_energy +
                     BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat 
                   +lowed + Income +   WGHTS_PROV_COM  + total_frut, Gxe4_Fruit_chr22G)
summary(Model_frut_A)
Model_frut_Asc <- lm(IOP_adj ~ chr22.19963748.G.A_G + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat 
                     +lowed + Income +   WGHTS_PROV_COM  + total_frut+ total_frut*chr22.19963748.G.A_G, Gxe4_Fruit_chr22G)
summary(Model_frut_Asc)
anova(Model_frut_A, Model_frut_Asc, test="LRT")

## Vegetable and chr22.19963748.G.A_G ##

Gxe4_Veg_chr22G<- Gxe4[!is.na(Gxe4$gen_veg) & !is.na(Gxe4$chr22.19963748.G.A_G), ]
Model_Veg_O <- lm(IOP_adj ~ AGE_NMBR_COM + chr22.19963748.G.A_G + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM + gen_veg, Gxe4_Veg_chr22G)
summary(Model_Veg_O)
Model_Veg_Omet <- lm(IOP_adj ~ chr22.19963748.G.A_G + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                       +lowed + Income +  WGHTS_PROV_COM + gen_veg+ gen_veg*chr22.19963748.G.A_G, Gxe4_Veg_chr22G)
summary(Model_Veg_Omet)
anova(Model_Veg_O, Model_Veg_Omet, test="LRT")


Gxe4_Veg_chr22G<- Gxe4[!is.na(Gxe4$gen_veg) & !is.na(Gxe4$chr22.19963748.G.A_G), ]
Model_Veg_O <- lm(IOP_adj ~ AGE_NMBR_COM + chr22.19963748.G.A_G + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + 
                  +lowed + Income +  WGHTS_PROV_COM + gen_veg, Gxe4_Veg_chr22G)
summary(Model_Veg_O)
Model_Veg_Omet <- lm(IOP_adj ~ chr22.19963748.G.A_G + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + 
                     +lowed + Income +  WGHTS_PROV_COM + gen_veg+ gen_veg*chr22.19963748.G.A_G, Gxe4_Veg_chr22G)
summary(Model_Veg_Omet)
anova(Model_Veg_O, Model_Veg_Omet, test="LRT")


## Alcohol and chr22.19963748.G.A_G ##

Gxe4_Alc_chr22G<- Gxe4[!is.na(Gxe4$altalc_cat) & !is.na(Gxe4$chr22.19963748.G.A_G), ]
Model_Alc_a <- lm(IOP_adj ~ AGE_NMBR_COM + chr22.19963748.G.A_G + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM +  altalc_cat, Gxe4_Alc_chr22G)
summary(Model_Alc_a )
Model_Alc_asc <- lm(IOP_adj ~ chr22.19963748.G.A_G + AGE_NMBR_COM + chromosomal.sex + total_energy +
                      BMI_Cat2 + Hyper + Diab  + newsmoke_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                    +lowed + Income +  WGHTS_PROV_COM + altalc_cat*chr22.19963748.G.A_G, Gxe4_Alc_chr22G)
summary(Model_Alc_asc)
anova(Model_Alc_a, Model_Alc_asc, test="Chisq")

## Smoking and chr22.19963748.G.A_G ##

Gxe4_Smk_chr22G<- Gxe4[!is.na(Gxe4$newsmoke) & !is.na(Gxe4$chr22.19963748.G.A_G), ]
Model_smk_a <- lm(IOP_adj ~ AGE_NMBR_COM + chr22.19963748.G.A_G + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                    +lowed + Income +  WGHTS_PROV_COM + newsmoke_cat, Gxe4_Smk_chr22G)
summary(Model_smk_a)
Model_smk_asc <- lm(IOP_adj ~ chr22.19963748.G.A_G + AGE_NMBR_COM + chromosomal.sex + total_energy +
                      BMI_Cat2 + Hyper + Diab  + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                      +lowed + Income +  WGHTS_PROV_COM +  newsmoke_cat*chr22.19963748.G.A_G, Gxe4_Smk_chr22G)
summary(Model_smk_asc)
anova(Model_smk_a, Model_smk_asc, test="Chisq")


#### chr16.30374516.T.C_T ########

##Fruit and chr16.30374516.T.C_T ##

Gxe4_Fruit_chr22G<- Gxe4[!is.na(Gxe4$total_frut) & !is.na(Gxe4$chr16.30374516.T.C_T), ]
Model_frut_A <- lm(IOP_adj ~ AGE_NMBR_COM + chr16.30374516.T.C_T + chromosomal.sex + total_energy +
                     BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                   +lowed + Income +   WGHTS_PROV_COM  + total_frut, Gxe4_Fruit_chr22G)
summary(Model_frut_A)
Model_frut_Asc <- lm(IOP_adj ~ chr16.30374516.T.C_T + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                     +lowed + Income +   WGHTS_PROV_COM  + total_frut+ total_frut*chr16.30374516.T.C_T, Gxe4_Fruit_chr22G)
summary(Model_frut_Asc)
anova(Model_frut_A, Model_frut_Asc, test="Chisq")

## Vegetable and chr16.30374516.T.C_T ##

Gxe4_Veg_chr22G<- Gxe4[!is.na(Gxe4$gen_veg) & !is.na(Gxe4$chr16.30374516.T.C_T), ]
Model_Veg_O <- lm(IOP_adj ~ AGE_NMBR_COM + chr16.30374516.T.C_T + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM + gen_veg, Gxe4_Veg_chr22G)
summary(Model_Veg_O)
Model_Veg_Omet <- lm(IOP_adj ~ chr16.30374516.T.C_T + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                     +lowed + Income +  WGHTS_PROV_COM + gen_veg+ gen_veg*chr16.30374516.T.C_T, Gxe4_Veg_chr22G)
summary(Model_Veg_Omet)
anova(Model_Veg_O, Model_Veg_Omet, test="Chisq")

## Alcohol and chr16.30374516.T.C_T ##

Gxe4_Alc_chr22G<- Gxe4[!is.na(Gxe4$altalc_cat) & !is.na(Gxe4$chr16.30374516.T.C_T), ]
Model_Alc_a <- lm(IOP_adj ~ AGE_NMBR_COM + chr16.30374516.T.C_T + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM +  altalc_cat, Gxe4_Alc_chr22G)
summary(Model_Alc_a )
Model_Alc_asc <- lm(IOP_adj ~ chr16.30374516.T.C_T + AGE_NMBR_COM + chromosomal.sex + total_energy +
                      BMI_Cat2 + Hyper + Diab  + newsmoke_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                    +lowed + Income +  WGHTS_PROV_COM + altalc_cat*chr16.30374516.T.C_T, Gxe4_Alc_chr22G)
summary(Model_Alc_asc)
anova(Model_Alc_a, Model_Alc_asc, test="Chisq")

## Smoking and chr16.30374516.T.C_T ##

Gxe4_Smk_chr22G<- Gxe4[!is.na(Gxe4$newsmoke) & !is.na(Gxe4$chr16.30374516.T.C_T), ]
Model_smk_a <- lm(IOP_adj ~ AGE_NMBR_COM + chr16.30374516.T.C_T + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM + newsmoke_cat, Gxe4_Smk_chr22G)
summary(Model_smk_a)
Model_smk_asc <- lm(IOP_adj ~ chr16.30374516.T.C_T + AGE_NMBR_COM + chromosomal.sex + total_energy +
                      BMI_Cat2 + Hyper + Diab  + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                    +lowed + Income +  WGHTS_PROV_COM +  newsmoke_cat*chr16.30374516.T.C_T, Gxe4_Smk_chr22G)
summary(Model_smk_asc)
anova(Model_smk_a, Model_smk_asc, test="Chisq")


#### chr22.19953481.A.G_A ########

##Fruit and chr22.19953481.A.G_A ##

Gxe4_Fruit_chr22G<- Gxe4[!is.na(Gxe4$total_frut) & !is.na(Gxe4$chr22.19953481.A.G_A), ]
Model_frut_A <- lm(IOP_adj ~ AGE_NMBR_COM + chr22.19953481.A.G_A + chromosomal.sex + total_energy +
                     BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                   +lowed + Income +   WGHTS_PROV_COM  + total_frut, Gxe4_Fruit_chr22G)
summary(Model_frut_A)
Model_frut_Asc <- lm(IOP_adj ~ chr22.19953481.A.G_A + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                     +lowed + Income +   WGHTS_PROV_COM  + total_frut+ total_frut*chr22.19953481.A.G_A, Gxe4_Fruit_chr22G)
summary(Model_frut_Asc)
anova(Model_frut_A, Model_frut_Asc, test="Chisq")

## Vegetable and chr22.19953481.A.G_A ##

Gxe4_Veg_chr22G<- Gxe4[!is.na(Gxe4$gen_veg) & !is.na(Gxe4$chr22.19953481.A.G_A), ]
Model_Veg_O <- lm(IOP_adj ~ AGE_NMBR_COM + chr22.19953481.A.G_A + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM + gen_veg, Gxe4_Veg_chr22G)
summary(Model_Veg_O)
Model_Veg_Omet <- lm(IOP_adj ~ chr22.19953481.A.G_A + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                     +lowed + Income +  WGHTS_PROV_COM + gen_veg+ gen_veg*chr22.19953481.A.G_A, Gxe4_Veg_chr22G)
summary(Model_Veg_Omet)
anova(Model_Veg_O, Model_Veg_Omet, test="Chisq")

## Alcohol and chr22.19953481.A.G_A ##

Gxe4_Alc_chr22G<- Gxe4[!is.na(Gxe4$altalc_cat) & !is.na(Gxe4$chr22.19953481.A.G_A), ]
Model_Alc_a <- lm(IOP_adj ~ AGE_NMBR_COM + chr22.19953481.A.G_A + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM +  altalc_cat, Gxe4_Alc_chr22G)
summary(Model_Alc_a )
Model_Alc_asc <- lm(IOP_adj ~ chr22.19953481.A.G_A + AGE_NMBR_COM + chromosomal.sex + total_energy +
                      BMI_Cat2 + Hyper + Diab  + newsmoke_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                    +lowed + Income +  WGHTS_PROV_COM + altalc_cat*chr22.19953481.A.G_A, Gxe4_Alc_chr22G)
summary(Model_Alc_asc)
anova(Model_Alc_a, Model_Alc_asc, test="Chisq")

## Smoking and chr22.19953481.A.G_A ##

Gxe4_Smk_chr22G<- Gxe4[!is.na(Gxe4$newsmoke) & !is.na(Gxe4$chr22.19953481.A.G_A), ]
Model_smk_a <- lm(IOP_adj ~ AGE_NMBR_COM + chr22.19953481.A.G_A + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM + newsmoke_cat, Gxe4_Smk_chr22G)
summary(Model_smk_a)
Model_smk_asc <- lm(IOP_adj ~ chr22.19953481.A.G_A + AGE_NMBR_COM + chromosomal.sex + total_energy +
                      BMI_Cat2 + Hyper + Diab  + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                    +lowed + Income +  WGHTS_PROV_COM +  newsmoke_cat*chr22.19953481.A.G_A, Gxe4_Smk_chr22G)
summary(Model_smk_asc)
anova(Model_smk_a, Model_smk_asc, test="Chisq")

#### chr16.30343759.C.T_T ########

##Fruit and chr16.30343759.C.T_T ##

Gxe4_Fruit_chr22G<- Gxe4[!is.na(Gxe4$total_frut) & !is.na(Gxe4$chr16.30343759.C.T_T), ]
Model_frut_A <- lm(IOP_adj ~ AGE_NMBR_COM + chr16.30343759.C.T_T + chromosomal.sex + total_energy +
                     BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                   +lowed + Income +   WGHTS_PROV_COM  + total_frut, Gxe4_Fruit_chr22G)
summary(Model_frut_A)
Model_frut_Asc <- lm(IOP_adj ~ chr16.30343759.C.T_T + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                     +lowed + Income +   WGHTS_PROV_COM  + total_frut+ total_frut*chr16.30343759.C.T_T, Gxe4_Fruit_chr22G)
summary(Model_frut_Asc)
anova(Model_frut_A, Model_frut_Asc, test="Chisq")

## Vegetable and chr16.30343759.C.T_T ##

Gxe4_Veg_chr22G<- Gxe4[!is.na(Gxe4$gen_veg) & !is.na(Gxe4$chr16.30343759.C.T_T), ]
Model_Veg_O <- lm(IOP_adj ~ AGE_NMBR_COM + chr16.30343759.C.T_T + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM + gen_veg, Gxe4_Veg_chr22G)
summary(Model_Veg_O)
Model_Veg_Omet <- lm(IOP_adj ~ chr16.30343759.C.T_T + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                     +lowed + Income +  WGHTS_PROV_COM + gen_veg+ gen_veg*chr16.30343759.C.T_T, Gxe4_Veg_chr22G)
summary(Model_Veg_Omet)
anova(Model_Veg_O, Model_Veg_Omet, test="Chisq")

## Alcohol and chr16.30343759.C.T_T ##

Gxe4_Alc_chr22G<- Gxe4[!is.na(Gxe4$altalc_cat) & !is.na(Gxe4$chr16.30343759.C.T_T), ]
Model_Alc_a <- lm(IOP_adj ~ AGE_NMBR_COM + chr16.30343759.C.T_T + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM +  altalc_cat, Gxe4_Alc_chr22G)
summary(Model_Alc_a )
Model_Alc_asc <- lm(IOP_adj ~ chr16.30343759.C.T_T + AGE_NMBR_COM + chromosomal.sex + total_energy +
                      BMI_Cat2 + Hyper + Diab  + newsmoke_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                    +lowed + Income +  WGHTS_PROV_COM + altalc_cat*chr16.30343759.C.T_T, Gxe4_Alc_chr22G)
summary(Model_Alc_asc)
anova(Model_Alc_a, Model_Alc_asc, test="Chisq")

## Smoking and chr16.30343759.C.T_T ##

Gxe4_Smk_chr22G<- Gxe4[!is.na(Gxe4$newsmoke) & !is.na(Gxe4$chr16.30343759.C.T_T), ]
Model_smk_a <- lm(IOP_adj ~ AGE_NMBR_COM + chr16.30343759.C.T_T + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM + newsmoke_cat, Gxe4_Smk_chr22G)
summary(Model_smk_a)
Model_smk_asc <- lm(IOP_adj ~ chr16.30343759.C.T_T + AGE_NMBR_COM + chromosomal.sex + total_energy +
                      BMI_Cat2 + Hyper + Diab  + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                    +lowed + Income +  WGHTS_PROV_COM +  newsmoke_cat*chr16.30343759.C.T_T, Gxe4_Smk_chr22G)
summary(Model_smk_asc)
anova(Model_smk_a, Model_smk_asc, test="Chisq")

#### chr22.19959676.CTCT.C_C ########

##Fruit and chr22.19959676.CTCT.C_C ##

Gxe4_Fruit_chr22G<- Gxe4[!is.na(Gxe4$total_frut) & !is.na(Gxe4$chr22.19959676.CTCT.C_C), ]
Model_frut_A <- lm(IOP_adj ~ AGE_NMBR_COM + chr22.19959676.CTCT.C_C + chromosomal.sex + total_energy +
                     BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                   +lowed + Income +   WGHTS_PROV_COM  + total_frut, Gxe4_Fruit_chr22G)
summary(Model_frut_A)
Model_frut_Asc <- lm(IOP_adj ~ chr22.19959676.CTCT.C_C + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                     +lowed + Income +   WGHTS_PROV_COM  + total_frut+ total_frut*chr22.19959676.CTCT.C_C, Gxe4_Fruit_chr22G)
summary(Model_frut_Asc)
anova(Model_frut_A, Model_frut_Asc, test="Chisq")

## Vegetable and chr22.19959676.CTCT.C_C ##

Gxe4_Veg_chr22G<- Gxe4[!is.na(Gxe4$gen_veg) & !is.na(Gxe4$chr22.19959676.CTCT.C_C), ]
Model_Veg_O <- lm(IOP_adj ~ AGE_NMBR_COM + chr22.19959676.CTCT.C_C + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM + gen_veg, Gxe4_Veg_chr22G)
summary(Model_Veg_O)
Model_Veg_Omet <- lm(IOP_adj ~ chr22.19959676.CTCT.C_C + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                     +lowed + Income +  WGHTS_PROV_COM + gen_veg+ gen_veg*chr22.19959676.CTCT.C_C, Gxe4_Veg_chr22G)
summary(Model_Veg_Omet)
anova(Model_Veg_O, Model_Veg_Omet, test="Chisq")

## Alcohol and chr22.19959676.CTCT.C_C ##

Gxe4_Alc_chr22G<- Gxe4[!is.na(Gxe4$altalc_cat) & !is.na(Gxe4$chr22.19959676.CTCT.C_C), ]
Model_Alc_a <- lm(IOP_adj ~ AGE_NMBR_COM + chr22.19959676.CTCT.C_C + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM +  altalc_cat, Gxe4_Alc_chr22G)
summary(Model_Alc_a )
Model_Alc_asc <- lm(IOP_adj ~ chr22.19959676.CTCT.C_C + AGE_NMBR_COM + chromosomal.sex + total_energy +
                      BMI_Cat2 + Hyper + Diab  + newsmoke_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                    +lowed + Income +  WGHTS_PROV_COM + altalc_cat*chr22.19959676.CTCT.C_C, Gxe4_Alc_chr22G)
summary(Model_Alc_asc)
anova(Model_Alc_a, Model_Alc_asc, test="Chisq")

## Smoking and chr22.19959676.CTCT.C_C ##

Gxe4_Smk_chr22G<- Gxe4[!is.na(Gxe4$newsmoke) & !is.na(Gxe4$chr22.19959676.CTCT.C_C), ]
Model_smk_a <- lm(IOP_adj ~ AGE_NMBR_COM + chr22.19959676.CTCT.C_C + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM + newsmoke_cat, Gxe4_Smk_chr22G)
summary(Model_smk_a)
Model_smk_asc <- lm(IOP_adj ~ chr22.19959676.CTCT.C_C + AGE_NMBR_COM + chromosomal.sex + total_energy +
                      BMI_Cat2 + Hyper + Diab  + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                    +lowed + Income +  WGHTS_PROV_COM +  newsmoke_cat*chr22.19959676.CTCT.C_C, Gxe4_Smk_chr22G)
summary(Model_smk_asc)
anova(Model_smk_a, Model_smk_asc, test="Chisq")

#### chr10.87718088.G.A_A ########

##Fruit and chr10.87718088.G.A_A ##

Gxe4_Fruit_chr22G<- Gxe4[!is.na(Gxe4$total_frut) & !is.na(Gxe4$chr10.87718088.G.A_A), ]
Model_frut_A <- lm(IOP_adj ~ AGE_NMBR_COM + chr10.87718088.G.A_A + chromosomal.sex + total_energy +
                     BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                   +lowed + Income +   WGHTS_PROV_COM  + total_frut, Gxe4_Fruit_chr22G)
summary(Model_frut_A)
Model_frut_Asc <- lm(IOP_adj ~ chr10.87718088.G.A_A + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                     +lowed + Income +   WGHTS_PROV_COM  + total_frut+ total_frut*chr10.87718088.G.A_A, Gxe4_Fruit_chr22G)
summary(Model_frut_Asc)
anova(Model_frut_A, Model_frut_Asc, test="Chisq")

## Vegetable and chr10.87718088.G.A_A ##

Gxe4_Veg_chr22G<- Gxe4[!is.na(Gxe4$gen_veg) & !is.na(Gxe4$chr10.87718088.G.A_A), ]
Model_Veg_O <- lm(IOP_adj ~ AGE_NMBR_COM + chr10.87718088.G.A_A + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM + gen_veg, Gxe4_Veg_chr22G)
summary(Model_Veg_O)
Model_Veg_Omet <- lm(IOP_adj ~ chr10.87718088.G.A_A + AGE_NMBR_COM + chromosomal.sex + total_energy +
                       BMI_Cat2 + Hyper + Diab  + newsmoke_cat + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                     +lowed + Income +  WGHTS_PROV_COM + gen_veg+ gen_veg*chr10.87718088.G.A_A, Gxe4_Veg_chr22G)
summary(Model_Veg_Omet)
anova(Model_Veg_O, Model_Veg_Omet, test="Chisq")

## Alcohol and chr10.87718088.G.A_A ##

Gxe4_Alc_chr22G<- Gxe4[!is.na(Gxe4$altalc_cat) & !is.na(Gxe4$chr10.87718088.G.A_A), ]
Model_Alc_a <- lm(IOP_adj ~ AGE_NMBR_COM + chr10.87718088.G.A_A + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM +  altalc_cat, Gxe4_Alc_chr22G)
summary(Model_Alc_a )
Model_Alc_asc <- lm(IOP_adj ~ chr10.87718088.G.A_A + AGE_NMBR_COM + chromosomal.sex + total_energy +
                      BMI_Cat2 + Hyper + Diab  + newsmoke_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                    +lowed + Income +  WGHTS_PROV_COM + altalc_cat*chr10.87718088.G.A_A, Gxe4_Alc_chr22G)
summary(Model_Alc_asc)
anova(Model_Alc_a, Model_Alc_asc, test="Chisq")


## Smoking and chr10.87718088.G.A_A ##

Gxe4_Smk_chr22G<- Gxe4[!is.na(Gxe4$newsmoke) & !is.na(Gxe4$chr10.87718088.G.A_A), ]
Model_smk_a <- lm(IOP_adj ~ AGE_NMBR_COM + chr10.87718088.G.A_A + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM + newsmoke_cat, Gxe4_Smk_chr22G)
summary(Model_smk_a)
Model_smk_asc <- lm(IOP_adj ~ chr10.87718088.G.A_A + AGE_NMBR_COM + chromosomal.sex + total_energy +
                      BMI_Cat2 + Hyper + Diab  + altalc_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                    +lowed + Income +  WGHTS_PROV_COM +  newsmoke_cat*chr10.87718088.G.A_A, Gxe4_Smk_chr22G)
summary(Model_smk_asc)
anova(Model_smk_a, Model_smk_asc, test="Chisq")

