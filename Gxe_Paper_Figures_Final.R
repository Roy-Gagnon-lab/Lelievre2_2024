#Creation of Figure 1 

############# Create Main Effects Forest Plot ########################
library(readxl)

#Load dataset with beta for each main effects model and 95% confidence interval 
## Models are found in "Gxe_Paper_Models.R" file, values are from models looking at the 
## association between all the factors (environmental, genetic, metabolic) and IOP
# Appears like this: 
#\Variable Level\Variable\Beta\Lower CI\ Upper CI\

Forestplot <- read_excel("Forestplot.xlsx")
library(ggplot2)
Forestplot$Variable_Level<- factor(Forestplot$Variable_Level, levels= c("Total Veg", "Total Fruit",
                                                                        "Daily Drinker", "Weekly Drinker","Occasional Drinker", "Never Drinker", 
                                                                        "Current Smoker","Former Smoker", "Never Smoker", 
                                                                        "O-Methylascorbate", "Ascorbic Acid 2 Sulfate", "rs4680",
                                                                        "rs12414734", "rs8050812", "rs165879","rs144009214", "rs61484427"))
Forestplot$Variable <-factor(Forestplot$Variable, levels=c("Genetics", "Metabolites", "Alcohol", "Smoking", "Fruit", "Veg") )

Main_Plot<-ggplot(Forestplot, aes(x=Beta, y=Variable_Level, xmin=Lower_CI, xmax=Upper_CI)) +
  geom_point(size=3, shape = 18, position= position_dodge(width=0.5)) +
  geom_linerange(position= position_dodge(width=0.5), linewidth=1) +
  geom_vline(xintercept=0, linewidth=0.5) +
  facet_grid(Variable ~ ., scales="free_y", space="free_y")+
  scale_y_discrete(name= "Variable") + 
  scale_x_continuous(name= "Beta", limits= c(-1,1)) +
  theme_gray(base_size = 12)

Main_Plot

ggsave("Figure1_IJE.jpeg", dpi=300, width=300, units="mm")

#Creation of Figure 2
library(sjPlot)

#Run models to be used to create interaction plots
M_alc_chr10 <- lm(IOP_adj ~ chr10.87718088.G.A_A + AGE_NMBR_COM + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM + altalc_cat*chr10.87718088.G.A_A, Gxe4)

M_alc_chr16 <- lm(IOP_adj ~ chr16.30374516.T.C_T + AGE_NMBR_COM + chromosomal.sex + total_energy +
                    BMI_Cat2 + Hyper + Diab  + newsmoke_cat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                  +lowed + Income +  WGHTS_PROV_COM + altalc_cat*chr16.30374516.T.C_T, Gxe4)

#Create interaction plots using SjPlot

P_1<- plot_model(M_alc_chr10, type="pred", terms=c("chr10.87718088.G.A_A", "altalc_cat"), ci.lvl= NA,
                 title = "", axis.title = c("rs12414734 # of A alleles", "IOP (mmHg)")) + theme(legend.position = 'bottom')
P_1_2<- P_1 + 
  scale_color_discrete(name= "Alcohol Consumption",
                       labels = c("Never", "Occasionally", "Weekly", "Daily")) 

P_2<- plot_model(M_alc_chr16, type="pred", terms=c("chr16.30374516.T.C_T","altalc_cat"), ci.lvl= NA, title="",
                 axis.title=c("rs8050812 # of T alleles", "IOP (mmHg)")) +theme(legend.position = 'bottom')
P_2_2<- P_2 +
  scale_color_discrete(name= "Alcohol Consumption",
                       labels = c("Never", "Occasionally", "Weekly", "Daily")) 

#Combine both plots and add letter labels 
library(patchwork)
patchwork <- (P_2_2 + P_1_2)
patchwork + plot_annotation(tag_levels = 'a') + plot_layout(guides="collect") & theme(legend.position = 'bottom')+
  theme(panel.background = element_blank(), axis.line = element_line(color="Black"))
ggsave("Figure2_IJE.jpeg", dpi=300, width=300, units="mm")
