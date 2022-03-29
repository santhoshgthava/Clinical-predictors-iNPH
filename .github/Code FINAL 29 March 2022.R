#Install relevant packages
install.packages(c("robumeta", "metafor", "dplyr"))
install.packages("meta")
install.packages("dmetar")
install.packages("readxl")
#into library load
library("robumeta")
library("metafor")
library("dplyr")
library("mada")
library("meta")
library("dmetar")
library("readxl")






# Load TTAU dataset from dmetar (or download and open manually)
library(readxl)
iNPH_Biochem_TTau_Excel <- read_excel("~/Desktop/iNPH Biochem - TTau.xlsx")
View(iNPH_Biochem_TTau_Excel)

# Use metcont to pool results.
meta_ttau <- metacont(n.e = iNPH_Biochem_TTau_Excel$n.SR,
                      mean.e = iNPH_Biochem_TTau_Excel$mean.SR,
                      sd.e = iNPH_Biochem_TTau_Excel$sd.SR,
                      n.c = iNPH_Biochem_TTau_Excel$n.SNR,
                      mean.c = iNPH_Biochem_TTau_Excel$mean.SNR,
                      sd.c = iNPH_Biochem_TTau_Excel$sd.SNR,
                      studlab = iNPH_Biochem_TTau_Excel$Author,
                      data = iNPH_Biochem_TTau_Excel,
                      sm = "SMD",
                      method.smd = "Cohen",
                      fixed = FALSE,
                      random = TRUE,
                      method.tau = "REML",
                      hakn = TRUE,
                      title = "T-Tau levels in SR vs N-SR")

#numerical visualisation of meta_ttau (note that SR are the experimental group so reference group, so minus result means it is less in them)

print(meta_ttau)

#make the plot

forest.meta(meta_ttau, 
            sortvar = TE,
            predict = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"),
            lab.e = "S-R",
            lab.c = "S-NR",
            JAMA.pval = FALSE,
            test.overall.random = TRUE,
            label.test.overall.random = "Overall statistical result of model: ")


#meta-regression metareg: single ones (all)

metareg_ttau_females <- metareg(meta_ttau,
        ~ females)
print(metareg_ttau_females)


metareg_ttau_age <- metareg(meta_ttau,
                                ~ age)
print(metareg_ttau_age)


metareg_ttau_sample <- metareg(meta_ttau,
                            ~ sample)
print(metareg_ttau_sample)


metareg_ttau_neuro <- metareg(meta_ttau,
                               ~ neuro)
print(metareg_ttau_neuro)


metareg_ttau_dropout<- metareg(meta_ttau,
                              ~ dropout)
print(metareg_ttau_dropout)


metareg_ttau_srm<- metareg(meta_ttau,
                               ~ srm)
print(metareg_ttau_srm)

metareg_ttau_date <- metareg(meta_ttau,
                           ~ date)
print(metareg_ttau_date)
# meta regression step 2: neuro plus others

metareg_ttau_neuroagefemales <- metareg(meta_ttau,
                              ~ neuro + age + females)
print(metareg_ttau_neuroagefemales)

metareg_ttau_neurosampledate <- metareg(meta_ttau,
                                        ~  neuro + sample + date)
print(metareg_ttau_neurosampledate)

metareg_ttau_neurofemalesdate <- metareg(meta_ttau,
                                        ~  neuro + females + date)
print(metareg_ttau_neuroagefemales)

metareg_ttau_neuroagedate <- metareg(meta_ttau,
                                         ~  neuro + age + date)
print(metareg_ttau_neuroagedate)

metareg_ttau_neurosamplefemales <- metareg(meta_ttau,
                                     ~  neuro + sample + females)

print(metareg_ttau_neurosamplefemales)

#other models were tried, no significance

#plot the metaregression neuro

bubble(metareg_ttau_neuro, studlab = TRUE, scale = TRUE)

#meta-analysis without the study positive for "neuro" (explicitly included neuro patients)

iNPH_Biochem_TTau_Excel2 <- read_excel("~/Desktop/iNPH Biochem - TTau2.xlsx")
View(iNPH_Biochem_TTau_Excel2)

# Use metcont to pool results.
meta_ttau2 <- metacont(n.e = iNPH_Biochem_TTau_Excel2$n.SR,
                      mean.e = iNPH_Biochem_TTau_Excel2$mean.SR,
                      sd.e = iNPH_Biochem_TTau_Excel2$sd.SR,
                      n.c = iNPH_Biochem_TTau_Excel2$n.SNR,
                      mean.c = iNPH_Biochem_TTau_Excel2$mean.SNR,
                      sd.c = iNPH_Biochem_TTau_Excel2$sd.SNR,
                      studlab = iNPH_Biochem_TTau_Excel2$Author,
                      data = iNPH_Biochem_TTau_Excel2,
                      sm = "SMD",
                      method.smd = "Cohen",
                      fixed = FALSE,
                      random = TRUE,
                      method.tau = "REML",
                      hakn = TRUE,
                      title = "T-Tau levels in SR vs N-SR")

#numerical visualisation of meta_ttau (note that SR are the experimental group so reference group, so minus result means it is less in them)

print(meta_ttau2)

#make the plot

forest.meta(meta_ttau2, 
            sortvar = TE,
            predict = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"),
            lab.e = "S-R",
            lab.c = "S-NR",
            JAMA.pval = FALSE,
            test.overall.random = TRUE,
            label.test.overall.random = "Overall statistical result of model: ")


####################################################################################################################



# Load P-TAU dataset from dmetar (or download and open manually)

library(readxl)
iNPH_Biochem_PTau_Excel <- read_excel("~/Desktop/iNPH Biochem - PTau.xlsx")
View(iNPH_Biochem_PTau_Excel)

# Use metcont to pool results.
meta_ptau <- metacont(n.e = iNPH_Biochem_PTau_Excel$n.SR,
                      mean.e = iNPH_Biochem_PTau_Excel$mean.SR,
                      sd.e = iNPH_Biochem_PTau_Excel$sd.SR,
                      n.c = iNPH_Biochem_PTau_Excel$n.SNR,
                      mean.c = iNPH_Biochem_PTau_Excel$mean.SNR,
                      sd.c = iNPH_Biochem_PTau_Excel$sd.SNR,
                      studlab = iNPH_Biochem_PTau_Excel$Author,
                      data = iNPH_Biochem_PTau_Excel,
                      sm = "SMD",
                      method.smd = "Cohen",
                      fixed = FALSE,
                      random = TRUE,
                      method.tau = "REML",
                      hakn = TRUE,
                      title = "P-Tau levels in SR vs N-SR")

#numerical visualisation of meta_ttau (note that SR are the experimental group so reference group, so minus result means it is less in them)

print(meta_ptau)

#make the plot

forest.meta(meta_ptau, 
            sortvar = TE,
            predict = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"),
            lab.e = "S-R",
            lab.c = "S-NR",
            JAMA.pval = FALSE,
            test.overall.random = TRUE,
            label.test.overall.random = "Overall statistical result of model: ")



#meta-regression metareg: single ones (all)

metareg_ptau_females <- metareg(meta_ptau,
                                ~ females)
print(metareg_ptau_females)


metareg_ptau_age <- metareg(meta_ptau,
                            ~ age)
print(metareg_ptau_age)


metareg_ptau_sample <- metareg(meta_ptau,
                               ~ sample)
print(metareg_ptau_sample)


metareg_ptau_neuro <- metareg(meta_ptau,
                              ~ neuro)
print(metareg_ptau_neuro)


metareg_ptau_dropout<- metareg(meta_ptau,
                               ~ dropout)
print(metareg_ptau_dropout)


metareg_ptau_srm<- metareg(meta_ptau,
                           ~ srm)
print(metareg_ptau_srm)

metareg_ptau_date <- metareg(meta_ptau,
                             ~ date)
print(metareg_ptau_date)


####################################################################################################################



# Load Abeta dataset from dmetar (or download and open manually)

library(readxl)
iNPH_Biochem_Abeta_Excel <- read_excel("~/Desktop/iNPH Biochem - Abeta.xlsx")
View(iNPH_Biochem_Abeta_Excel)

# Use metcont to pool results.
meta_Abeta <- metacont(n.e = iNPH_Biochem_Abeta_Excel$n.SR,
                       mean.e = iNPH_Biochem_Abeta_Excel$mean.SR,
                       sd.e = iNPH_Biochem_Abeta_Excel$sd.SR,
                       n.c = iNPH_Biochem_Abeta_Excel$n.SNR,
                       mean.c = iNPH_Biochem_Abeta_Excel$mean.SNR,
                       sd.c = iNPH_Biochem_Abeta_Excel$sd.SNR,
                       studlab = iNPH_Biochem_Abeta_Excel$Author,
                       data = iNPH_Biochem_Abeta_Excel,
                       sm = "SMD",
                       method.smd = "Cohen",
                       fixed = FALSE,
                       random = TRUE,
                       method.tau = "REML",
                       hakn = TRUE,
                       title = "Abeta levels in SR vs N-SR")

#numerical visualisation of meta_ttau (note that SR are the experimental group so reference group, so minus result means it is less in them)

print(meta_Abeta)

#make the plot

forest.meta(meta_Abeta, 
            sortvar = TE,
            predict = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"),
            lab.e = "S-R",
            lab.c = "S-NR",
            JAMA.pval = FALSE,
            test.overall.random = TRUE,
            label.test.overall.random = "Overall statistical result of model: ")


#meta-regression metareg: single ones (all)

metareg_Abeta_females <- metareg(meta_Abeta,
                                ~ females)
print(metareg_Abeta_females)


metareg_Abeta_age <- metareg(meta_Abeta,
                            ~ age)
print(metareg_Abeta_age)


metareg_Abeta_sample <- metareg(meta_Abeta,
                               ~ sample)
print(metareg_Abeta_sample)


metareg_Abeta_neuro <- metareg(meta_Abeta,
                              ~ neuro)
print(metareg_Abeta_neuro)


metareg_Abeta_dropout<- metareg(meta_Abeta,
                               ~ dropout)
print(metareg_Abeta_dropout)


metareg_Abeta_srm<- metareg(meta_Abeta,
                           ~ srm)
print(metareg_Abeta_srm)

metareg_Abeta_date <- metareg(meta_Abeta,
                             ~ date)
print(metareg_Abeta_date)

# meta regression step 2: neuro plus others

metareg_ttau_neuroagefemales <- metareg(meta_ttau,
                                        ~ neuro + age + females)
print(metareg_ttau_neuroagefemales)

metareg_ttau_neurosampledate <- metareg(meta_ttau,
                                        ~  neuro + sample + date)
print(metareg_ttau_neurosampledate)

#other models were tried, no significance

#plot the metaregression neuro

bubble(metareg_ttau_neuro, studlab = TRUE, scale = TRUE)

#meta-analysis without the study positive for "neuro" (explicitly included neuro patients)

iNPH_Biochem_TTau_Excel2 <- read_excel("~/Desktop/iNPH Biochem - TTau2.xlsx")
View(iNPH_Biochem_TTau_Excel2)

# Use metcont to pool results.
meta_ttau2 <- metacont(n.e = iNPH_Biochem_TTau_Excel2$n.SR,
                       mean.e = iNPH_Biochem_TTau_Excel2$mean.SR,
                       sd.e = iNPH_Biochem_TTau_Excel2$sd.SR,
                       n.c = iNPH_Biochem_TTau_Excel2$n.SNR,
                       mean.c = iNPH_Biochem_TTau_Excel2$mean.SNR,
                       sd.c = iNPH_Biochem_TTau_Excel2$sd.SNR,
                       studlab = iNPH_Biochem_TTau_Excel2$Author,
                       data = iNPH_Biochem_TTau_Excel2,
                       sm = "SMD",
                       method.smd = "Cohen",
                       fixed = FALSE,
                       random = TRUE,
                       method.tau = "REML",
                       hakn = TRUE,
                       title = "T-Tau levels in SR vs N-SR")

#numerical visualisation of meta_ttau (note that SR are the experimental group so reference group, so minus result means it is less in them)

print(meta_ttau2)

#make the plot

forest.meta(meta_ttau2, 
            sortvar = TE,
            predict = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"),
            lab.e = "S-R",
            lab.c = "S-NR",
            JAMA.pval = FALSE,
            test.overall.random = TRUE,
            label.test.overall.random = "Overall statistical result of model: ")


####################################################################################################################



# Load NFL dataset from dmetar (or download and open manually)

library(readxl)
iNPH_Biochem_NFL_Excel <- read_excel("~/Desktop/iNPH Biochem - NFL.xlsx")
View(iNPH_Biochem_NFL_Excel)

# Use metcont to pool results.
meta_NFL <- metacont(n.e = iNPH_Biochem_NFL_Excel$n.SR,
                     mean.e = iNPH_Biochem_NFL_Excel$mean.SR,
                     sd.e = iNPH_Biochem_NFL_Excel$sd.SR,
                     n.c = iNPH_Biochem_NFL_Excel$n.SNR,
                     mean.c = iNPH_Biochem_NFL_Excel$mean.SNR,
                     sd.c = iNPH_Biochem_NFL_Excel$sd.SNR,
                     studlab = iNPH_Biochem_NFL_Excel$Author,
                     data = iNPH_Biochem_NFL_Excel,
                     sm = "SMD",
                     method.smd = "Cohen",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     hakn = TRUE,
                     title = "NFL levels in SR vs N-SR")

#numerical visualisation of meta_ttau (note that SR are the experimental group so reference group, so minus result means it is less in them)

print(meta_NFL)

#make the plot

forest.meta(meta_NFL, 
            sortvar = TE,
            predict = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"),
            lab.e = "S-R",
            lab.c = "S-NR",
            JAMA.pval = FALSE,
            test.overall.random = TRUE,
            label.test.overall.random = "Overall statistical result of model: ")


####################################################################################################################



# Load Sulfatide dataset from dmetar (or download and open manually)

library(readxl)
iNPH_Biochem_Sulfatide_Excel <- read_excel("~/Desktop/iNPH Biochem - Sulfatide.xlsx")
View(iNPH_Biochem_Sulfatide_Excel)

# Use metcont to pool results.
meta_Sulfatide <- metacont(n.e = iNPH_Biochem_Sulfatide_Excel$n.SR,
                           mean.e = iNPH_Biochem_Sulfatide_Excel$mean.SR,
                           sd.e = iNPH_Biochem_Sulfatide_Excel$sd.SR,
                           n.c = iNPH_Biochem_Sulfatide_Excel$n.SNR,
                           mean.c = iNPH_Biochem_Sulfatide_Excel$mean.SNR,
                           sd.c = iNPH_Biochem_Sulfatide_Excel$sd.SNR,
                           studlab = iNPH_Biochem_Sulfatide_Excel$Author,
                           data = iNPH_Biochem_Sulfatide_Excel,
                           sm = "SMD",
                           method.smd = "Cohen",
                           fixed = FALSE,
                           random = TRUE,
                           method.tau = "REML",
                           hakn = TRUE,
                           title = "Sulfatide levels in SR vs N-SR")

#numerical visualisation of meta_ttau (note that SR are the experimental group so reference group, so minus result means it is less in them)

print(meta_Sulfatide)

#make the plot

forest.meta(meta_Sulfatide, 
            sortvar = TE,
            predict = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"),
            lab.e = "S-R",
            lab.c = "S-NR",
            JAMA.pval = FALSE,
            test.overall.random = TRUE,
            label.test.overall.random = "Overall statistical result of model: ")



####################################################################################################################



# Load Total set dataset from dmetar (or download and open manually)

library(readxl)
iNPH_Biochem_Total_Excel <- read_excel("~/Desktop/iNPH Biochem - Total.xlsx")
View(iNPH_Biochem_Total_Excel)

# Use metcont to pool results.
meta_Total <- metacont(n.e = iNPH_Biochem_Total_Excel$n.SR,
                       mean.e = iNPH_Biochem_Total_Excel$mean.SR,
                       sd.e = iNPH_Biochem_Total_Excel$sd.SR,
                       n.c = iNPH_Biochem_Total_Excel$n.SNR,
                       mean.c = iNPH_Biochem_Total_Excel$mean.SNR,
                       sd.c = iNPH_Biochem_Total_Excel$sd.SNR,
                       studlab = iNPH_Biochem_Total_Excel$Author,
                       data = iNPH_Biochem_Total_Excel,
                       sm = "SMD",
                       method.smd = "Cohen",
                       fixed = FALSE,
                       random = TRUE,
                       method.tau = "REML",
                       hakn = TRUE,
                       title = "Total levels in SR vs N-SR")

#numerical visualisation of meta_ttau (note that SR are the experimental group so reference group, so minus result means it is less in them)

print(meta_Total)

#make the plot

forest.meta(meta_Total, 
            sortvar = TE,
            predict = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"),
            lab.e = "S-R",
            lab.c = "S-NR",
            JAMA.pval = FALSE,
            test.overall.random = TRUE,
            label.test.overall.random = "Overall statistical result of model: ")


#Eggers (Publication bias) calculatiom

eggersplot <-metabias(
  meta_Total,
  method.bias = meta_Total$method.bias,
  plotit = FALSE,
  correct = FALSE,
  k.min = 10 
)

print(eggersplot,
      digits = gs("digits"),
      digits.stat = gs("digits.stat"),
      digits.pval = max(gs("digits.pval"), 2),
      digits.se = gs("digits.se"),
      digits.tau2 = gs("digits.tau2"),
      scientific.pval = gs("scientific.pval"),
      big.mark = gs("big.mark"),
      zero.pval = gs("zero.pval"),
      JAMA.pval = gs("JAMA.pval"),
      text.tau2 = gs("text.tau2"))




#Eggers (Publication bias) plot


eggersplot <-metabias(
  meta_Total,
  method.bias = meta_Total$method.bias,
  plotit = TRUE,
  correct = FALSE,
  k.min = 14 
)

funnel.meta(meta_Total,
            xlim = c(-2, 1),
            studlab = FALSE,
            method.bias = "linreg")



####################### Study characteristics graphs ##################

library(readxl)
Heatmap <- read_excel("Desktop/iNPH Radio New/Tables/Qualitative graphs/Heatmap.xlsx")
View(Heatmap)


####### 1. Make bar plot for study design

studydesigntable <- table(Heatmap$`Study design`)

# Load ggplot2
library(ggplot2)

# Create data


data1 <- data.frame(
  name=c("Prospective", "Retrospective") ,  
  value=c(21, 7)
)




barplot(height = data1$value,
        names.arg = data1$name,
        xlab = "Study design",
        ylab = "Number of studies",
        ylim = NULL, xpd = TRUE,
        col="black", angle = c(45, 90), density = c(10,30))




###### 2. Make bar plot for study sample size

studysamplesizetable <- table(Heatmap$Sample)

data2 <- data.frame(
  name=Heatmap$Number,  
  value=Heatmap$Sample
)

barplot(height = data2$value,
        names.arg = data2$name,
        xlab = "Study",
        ylab = "Sample size",
        col = "black",
        ylim = NULL, xpd = TRUE)



###### 3. Make bar plot for study year


studysamplesizetable <- table(Heatmap$date)

data3 <- data.frame(
  name=c("1980", "2002", "2004", "2005", "2006", "2007", "2008",
         "2011", "2013",  "2014",  "2016","2017", "2018", "2019", "2020", "2021"),  
  value=c( 1,1,1,1,1,1,1,3,
           3,3,2,1,2,2,3,2) 
 )


barplot(height = data3$value,
        names.arg = data3$name,
        xlab = "Year of study publication",
        ylab = "Number of studies",
        col = c("gray0", "gray10", "gray20", "gray30", "gray35", "gray40",  "gray45", 
                "gray50", "gray55", "gray60", "gray65", "gray70", "gray75",
                "gray80", "gray85", "gray90", "gray95"),
        ylim = NULL, xpd = TRUE)



library(RColorBrewer)
coul3 <- brewer.pal(9, "Spectral") 

####################### Imaging characteristics graphs ##################

#Imaging modality

studyradiotable <- table(Heatmap$`Imaging modality`)

data4 <- data.frame(
  name=c( "1.5T MRI", "1.5T or 0.5T MRI", "3T MRI",
                    "0.5-3T MRI", "Undefined MRI", "CTC",
                    "CT or MRI", "SPECT", "PEG"),
  value=c(8, 1,5,2,1,2,4,2,3)
)

library(RColorBrewer)
coul <- brewer.pal(11, "PiYG") 


barplot(height = data4$value,
        names.arg = data4$name,
        xlab = "Imaging modality",
        ylab = "Number of studies",
        col = c("palegreen", "seagreen1", "seagreen2", "seagreen3", "seagreen4",
                "slateblue1", "plum2", "blue", "salmon"),
        ylim = NULL, xpd = TRUE)


####################### Patient characteristics: Percentage ##################

# Mean patient  characteristics in percentages 
data5 <- data.frame(
  name=c("Female", "Stroke",	"MI",	"DM",	"HTN",
         "Depression", "Gait-", "Cognition-",
         "Urine-",	"DESH+", 
         "HTriad+", "S-R", 	"Lost FU",  "Complic."),
value=c(0.411804467,	0.187096774,	0.326515152,	0.278907555,	0.478842884,	0.22259433,
        0.944366243,	0.77092637,	0.568090168,	
        0.57325641,	0.638368687,	0.721949932,	0.127401705,	0.147431461))

library(RColorBrewer)

coul <- brewer.pal(9, "Set1") 


barplot(height = data5$value,
        names.arg = data5$name,
        xlab = "Patient characteristics",
        ylab = "Mean proportion",
        col = c("steelblue", "purple3", "red2", "royalblue4", "red4",
                "snow3", "gold", "gold3",  "goldenrod4", "darkolivegreen1", 
                "darkolivegreen3", "forestgreen", "gray40", "gray0"), 
        ylim = NULL, xpd = TRUE)


# Mean clinical score absolute values

data6 <- data.frame(
  name=c("MMSE",	"mRS",	"iNPHGS:T",
         "iNPHGS:C",	"iNPHGS:U",	"iNPHGS:G", "Continence",
         "TUG",	"Hellstrom NPH", 	"EI", "CA"),
value=c(21.91591176,	2.528,	6.18,
        2.2,	1.94,	2.16,	3,	18.37507843,	55.5,	0.373083333,	71.64657143))



library(RColorBrewer)

coul <- brewer.pal(9, "Greys") 


 my_bar <- barplot(height = data6$value,
        names.arg = data6$name,
        xlab = "Patient characteristics",
        ylab = "Mean value",
        col = coul, 
        ylim = NULL, xpd = TRUE)


 
 ####################### Complex heatmap ##################
 
 library(readxl)
 Heatmap2 <- read_excel("Desktop/iNPH Radio New/Tables/Qualitative graphs/Heatmap2.xlsx")
 View(Heatmap2)
 
 # transform into matrix
 HeatmapCool2 <- data.matrix(Heatmap2)
 
 #replace NA by 0
 is.na(HeatmapCool2) <- sapply(HeatmapCool2, is.infinite)
 HeatmapCool2[is.na(HeatmapCool2)] <- 0

 
 rownames(HeatmapCool2) <- c("Agerskov et al. (2019)",
                            "Agerskov et al. (2020)",
                            "Aoki et al. (2020)",
                            "Chen et al. (2008)",
                            "Garcia-Armengol et al. (2016)",
                            "Grahnke et al. (2018)",
                            "Hong et al. (2018)",
                            "Ishii et al. (2011)",
                            "Jurcoane et al. (2013)",
                            "Kawaguchi et al. (2011)",
                            "Kazui et al. (2013)",
                            "Kuchcinski et al. (2019)",
                            "Mantovani et al. (2021) ",
                            "McGirt et al. (2005)",
                            "Murakami et al. (2007)",
                            "Narita et al. (2016)",
                            "Palm et al. (2006)",
                            "PM Black (1980)",
                            "Poca et al. (2002)",
                            "Poca et at. (2004)",
                            "Shinoda et al. (2017)",
                            "Stecco et al. (2020)",
                            "Virhammar et al. (2014) CA",
                            "Virhammar et al. (2014)",
                            "Wu et al. (2021)",
                            "Yamada et al. (2013)",
                            "Yamamoto et al. (2013)",
                            "Ziegelitz et al. (2014)")
 

 
 print(HeatmapCool2)
 
 

 
 
 
 #print heatmap: THE MAGIC
heatmap(HeatmapCool2,
             col = topo.colors(10000),
             scale = "column",
             margins = c(5,5)
            )
 
legend(x=0.98, y=0.1, bty = "n", cex = 0.6, border = "white", legend=c("min", "min-med","NA", "med-max", "max"),fill=topo.colors(5))


#throwaway 
 rownames(HeatmapCool) <- c("Agerskov et al. (2019)",
                  "Agerskov et al. (2020)",
                  "Aoki et al. (2020)",
                 "Chen et al. (2008)",
                 "Garcia-Armengol et al. (2016)",
                 "Grahnke et al. (2018)",
                  "Hong et al. (2018)",
                  "Ishii et al. (2011)",
                 "Jurcoane et al. (2013)",
                 "Kawaguchi et al. (2011)",
                 "Kazui et al. (2013)",
                  "Kuchcinski et al. (2019)",
                  "Mantovani et al. (2021) ",
                  "McGirt et al. (2005)",
                  "Murakami et al. (2007)",
                 "Narita et al. (2016)",
                  "Palm et al. (2006)",
                "PM Black (1980)",
                 "Poca et al. (2002)",
                 "Poca et at. (2004)",
                 "Shinoda et al. (2017)",
                  "Stecco et al. (2020)",
                 "Virhammar et al. (2014) CA",
               "Virhammar et al. (2014)",
                "Wu et al. (2021)",
                  "Yamada et al. (2013)",
                  "Yamamoto et al. (2013)",
                  "Ziegelitz et al. (2014)")
 
 colnames(HeatmapCool) <- c("Sample",	"Age (y)", "Females", 
                  "Stroke",	"MI", "DM",	"HTN",	"Depression",
                  "Gait-",	"Cognition-", "Urine-", "MMSE", 
                  "mRS",	"iNPHGS:T",	"iNPHGS:C",	"iNPHGS: U",
                  "iNPHGS: G",	"Continence",	"TUG",	"Hellstrom NPH",
                  "EI",	"CA",	"DESH+", "HT+", "S-R", "Lost FU",	"Complic.")


 
 # Default Heatmap
 heatmap(as.matrix(HeatmapCool2))
 
 d3heatmap(HeatmapCool2,Rowv = FALSE, Colv=FALSE)
 
 
 # Default Heatmap
 heatmap(data)
 
 
