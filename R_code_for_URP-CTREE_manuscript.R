#### >>>>>>>> URP-CTREE anaylsis <<<<<<<< ####
## author: Adrian Cathomen, adrian.cathomen@balgrist.ch

rm(list=ls())

library(party)     ## for recursive partitioning

setwd("/Users/Measurements/Desktop/EMSCI_data")

imageDirectory<- "/Users/Measurements/Desktop/EMSCI_data/Graphiken/tetra"
saveInImageDirectory<-function(filename, width, height){imageFile <- file.path(imageDirectory, filename); ggsave(imageFile, width = width, height = height)}

#### Load data & structure ####
## Load:
data_EMSCI <- read.csv("data_EMSCI.csv", header=T, sep=",")

## Structure:
str(data_EMSCI)
# ID as factor:
data_EMSCI$PatientNumber <- factor(data_EMSCI$PatientNumber) 
# Date of injury as date:
data_EMSCI$DOI <- as.Date(data_EMSCI$DOI, "%d/%m/%Y")
# code for cause as factor:
data_EMSCI$code_Cause <- factor(data_EMSCI$code_Cause)
# code for exam stage as factor:
data_EMSCI$code_ExamStage <- factor(data_EMSCI$code_ExamStage)
# ISNSCI test date as date:
data_EMSCI$ISNCSCI_TestDate <- as.Date(data_EMSCI$ISNCSCI_TestDate, "%d/%m/%Y")
# code AIS as factor:
data_EMSCI$code_AIS <- factor(data_EMSCI$code_AIS)
# code NLI as factor:
data_EMSCI$code_NLI <- factor(data_EMSCI$code_NLI)
# code PT (Para/Tetra) as factor:
data_EMSCI$code_PT <- factor(data_EMSCI$code_PT)
# SCIM23 test date as date:
data_EMSCI$SCIM23_TestDate <- as.Date(data_EMSCI$SCIM23_TestDate, "%d/%m/%Y")
# 6mWT test date as date:
data_EMSCI$X6min_TestDate <- as.Date(data_EMSCI$X6min_TestDate, "%d/%m/%Y")
data_EMSCI$X10m_TestDate <- as.Date(data_EMSCI$X10m_TestDate, "%d/%m/%Y")
data_EMSCI$WISCI_TestDate <- as.Date(data_EMSCI$WISCI_TestDate, "%d/%m/%Y")



str(data_EMSCI) # NLI has 29 levels, because nobody is assigned to S4/S5. 

#remove columns which are not needed:
data_EMSCI$Center <- NULL #encoded in Country
data_EMSCI$Cause <- NULL #encoded in code_cause
data_EMSCI$ExamStage <- NULL #encoded in code_ExamStage
data_EMSCI$AIS <- NULL #encoded in code_AIS
data_EMSCI$NLI <- NULL #encoded in code_NLI
data_EMSCI$RUEMS <- NULL # not needed for analysis
data_EMSCI$LUEMS <- NULL 
data_EMSCI$RLEMS <- NULL
data_EMSCI$LLEMS <- NULL
data_EMSCI$RMS <- NULL
data_EMSCI$LMS <- NULL
data_EMSCI$RPP <- NULL
data_EMSCI$LPP <- NULL
data_EMSCI$RLT <- NULL
data_EMSCI$LLT <- NULL
data_EMSCI[,c(21:26, 31:36 )] <- NULL

#assign reasonable names to codes and columns 
levels(data_EMSCI$code_AIS) <- c("A", "B", "C", "D", "E")
levels(data_EMSCI$code_AIS)
levels(data_EMSCI$code_NLI) <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "T1", "T2", "T3", "T4","T5", "T6", "T7", "T8", "T9","T10", "T11", "T12", "L1", "L2", "L3", "L4", "L5", "S1", "S2", "S3", "Int")
levels(data_EMSCI$code_NLI)
levels(data_EMSCI$code_PT) <- c("Tetra", "Para", "Int")
levels(data_EMSCI$code_PT)
colnames(data_EMSCI)[25] <- "min6_TestDate"
colnames(data_EMSCI)[26] <- "min6"
colnames(data_EMSCI)[27] <- "m10_TestDate"
colnames(data_EMSCI)[28] <- "m10"

# change PatientNumber to ID
names(data_EMSCI)[1] <- "ID"

str(data_EMSCI)

#### >>>> Subsetting ####
## patients with complete dataset ####
# means: 
# information about tetra/para has to be present at veryacute
# predictors (UEMS,LEMS,LT,PP,age,NLI) have to be present at veryacute, 1month and 3months
# outcomes LEMS, SCIM 12-14, speed_10mWT, 6MWT & UEMS and AIS have to be present at veryacute, 1month, 3months, and 6months

names(data_EMSCI)
data.complete <- data_EMSCI[,c(1,6,9,11,13,14,15,17,18,24,31,32,26)]
names(data.complete) <- c("ID", "Age", "ExamStage", "AIS", "PT", "UEMS",
                          "LEMS", "PP", "LT", "SCIM_Mob","speed6min", "speed10m", "min6")

# reshape to wide format
data.complete_wide <- reshape(data.complete, v.names = c("AIS", "PT", "UEMS", 
                                                         "LEMS", "PP","LT", "SCIM_Mob","speed6min", "speed10m", "min6"), timevar="ExamStage", idvar="ID", direction = "wide")   ## reshape to wide format

## excluding measure @12months
names(data.complete_wide)


m.complete_wide <- data.complete_wide[,c("ID","Age", "AIS.1","PT.1","UEMS.1","LEMS.1","PP.1","LT.1", "SCIM_Mob.1", "speed6min.1", "speed10m.1", "min6.1",
                                         "AIS.2", "UEMS.2","LEMS.2", "PP.2", "LT.2", "SCIM_Mob.2","speed6min.2","speed10m.2", "min6.2", 
                                         "AIS.3", "UEMS.3","LEMS.3","PP.3","LT.3","SCIM_Mob.3","speed6min.3","speed10m.3","min6.3",
                                         "AIS.4", "UEMS.4", "LEMS.4", "SCIM_Mob.4", "speed6min.4","speed10m.4", "min6.4")]
names(m.complete_wide)

names(m.complete_wide) <- c("ID", "Age", "AIS_va", "PT_va", "UEMS_va", "LEMS_va", "PP_va", "LT_va", "SCIM_va","speed6min_va", "speed10m_va", "min6_va",
                            "AIS_1month", "UEMS_1month", "LEMS_1month", "PP_1month", "LT_1month", "SCIM_1month","speed6min_1month", "speed10m_1month", "min6_1month",
                            "AIS_3months", "UEMS_3months", "LEMS_3months", "PP_3months", "LT_3months", "SCIM_3months","speed6min_3months", "speed10m_3months", "min6_3months",
                            "AIS_6months", "UEMS_6months","LEMS_6months", "SCIM_6months","speed6min_6months", "speed10m_6months", "min6_6months")

m.tetra.complete_wide <- subset(m.complete_wide, PT_va == "Tetra" & !is.na(PT_va)) # change to "Para" if needed
table(m.tetra.complete_wide$PT_va)

#### Patient Selection: Step #1 ####
# Age 18-70y
m.tetra.complete_wide <- subset(m.tetra.complete_wide, Age >= 18 & Age<=70)

m.tetra.data_va.6 <- na.omit(m.tetra.complete_wide)

## assign rownames to default 
rownames(m.tetra.data_va.6) <- NULL

str(m.tetra.data_va.6)


#### Patient Selection: Step #3 ####
## exclude patients with SCIM>9 and speed6min/speed10m ==0
m.tetra.data_va.6$speed6min_6months <- ifelse(m.tetra.data_va.6$SCIM_6months > 9 & m.tetra.data_va.6$speed6min_6months ==0 , NA, m.tetra.data_va.6$speed6min_6months)
m.tetra.data_va.6$speed10m_6months <- ifelse(m.tetra.data_va.6$SCIM_6months > 9 & m.tetra.data_va.6$speed10m_6months ==0 , NA, m.tetra.data_va.6$speed10m_6months)
m.tetra.data_va.6$speed6min_6months <- ifelse(m.tetra.data_va.6$speed6min_6months ==0 & m.tetra.data_va.6$speed10m_6months >0, NA, m.tetra.data_va.6$speed6min_6months)

which(is.na(m.tetra.data_va.6$speed6min_6months))
which(is.na(m.tetra.data_va.6$speed10m_6months))

patients.excluded <- subset(m.tetra.data_va.6, is.na(m.tetra.data_va.6$speed6min_6months) | is.na(m.tetra.data_va.6$speed10m_6months))

m.tetra.data_va.6 <- na.omit(m.tetra.data_va.6)

data.age <- data_EMSCI[,c("ID","code_ExamStage", "code_NLI", "Sex","code_Cause","DOI","ISNCSCI_TestDate","TLT","TPP")]
data.age_wide <- reshape(data.age, timevar="code_ExamStage", idvar="ID", direction = "wide")
data.age_wide <- data.age_wide[,c(1:6,23,28,29)]
m.tetra.data_va.6 <- merge(data.age_wide,  m.tetra.data_va.6, by="ID")

data.age1 <- data_EMSCI1[,c("ID","code_ExamStage", "Center", "LOS")]
data.age_wide1 <- reshape(data.age1, timevar="code_ExamStage", idvar="ID", direction = "wide")
data.age_wide1 <- data.age_wide1[,c(1:3)]
m.tetra.data_va.6 <- merge(data.age_wide1,  m.tetra.data_va.6, by="ID")


#### recursive partitioning 6MWT ####
## outcome 6MWT at 6 months, predictors LEMS, UEMS, LT, PP, age, and NLI  


tree_tetra_va.6_6MWT<-ctree(min6_6months ~ Age + code_NLI.1 + LEMS_va + UEMS_va + 
                                  LT_va + PP_va , 
                                data= m.tetra.data_va.6, controls=ctree_control(maxdepth = 5))

# change N in title according to dataset
plot(tree_tetra_va.6_6MWT, terminal_panel= node_boxplot(tree_tetra_va.6_6MWT), main = "Tetra: 6MWT @ 6 months, predictors @ acute, N=309")


# inspect nodes
m.tetra.data_va.6$node6MWT<-where(tree_tetra_va.6_6MWT) ## create column with node info in separate column 


tetra_va.6_6MWT_Node4<-subset(m.tetra.data_va.6, node6MWT==4)
write.csv(tetra_va.6_6MWT_Node4, "6MWT_URPtreeN309_va.6_Node4.csv", row.names = F)

tetra_va.6_6MWT_Node5<-subset(m.tetra.data_va.6, node6MWT==5)
write.csv(tetra_va.6_6MWT_Node5, "6MWT_URPtreeN309_va.6_Node5.csv", row.names = F)

tetra_va.6_6MWT_Node7<-subset(m.tetra.data_va.6, node6MWT==7)
write.csv(tetra_va.6_6MWT_Node7, "6MWT_URPtreeN309_va.6_Node7.csv", row.names = F)

tetra_va.6_6MWT_Node8<-subset(m.tetra.data_va.6, node6MWT==8)
write.csv(tetra_va.6_6MWT_Node8, "6MWT_URPtreeN309_va.6_Node8.csv", row.names = F)

tetra_va.6_6MWT_Node11<-subset(m.tetra.data_va.6, node6MWT==11)
write.csv(tetra_va.6_6MWT_Node11, "6MWT_URPtreeN309_va.6_Node11.csv", row.names = F)

tetra_va.6_6MWT_Node12<-subset(m.tetra.data_va.6, node6MWT==12)
write.csv(tetra_va.6_6MWT_Node12, "6MWT_URPtreeN309_va.6_Node12.csv", row.names = F)

tetra_va.6_6MWT_Node13<-subset(m.tetra.data_va.6, node6MWT==13)
write.csv(tetra_va.6_6MWT_Node13, "6MWT_URPtreeN309_va.6_Node13.csv", row.names = F)