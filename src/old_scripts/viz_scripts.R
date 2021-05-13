#rerunning models for visualizing the base model plus example extreme models for two species

library(MigClim)

setwd("D:/OneDrive/PhD/CSF_ABM_Project/MigClim/MigClimRFiles/85_cc")

# 85 ----------------------------------------------------------------------
MigClim.migrate (iniDist = "ABMA_Presence",
                 hsMap="ABMA_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ABMA_base_viz", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "ANHE_Presence",
                 hsMap="ANHE_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ANHE_base_viz", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "ANBO_Presence",
                 hsMap="ANBO_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ANBO_base_viz", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "PSMA_Presence",
                 hsMap="PSMA_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="PSMA_base_viz", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "RALU_Presence",
                 hsMap="RALU_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="RALU_base_viz", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "RASY_Presence",
                 hsMap="RASY_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="RASY_base_viz", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)


# 45 ----------------------------------------------------------------------

setwd("D:/OneDrive/PhD/CSF_ABM_Project/MigClim/MigClimRFiles/45_cc")

MigClim.migrate (iniDist = "ABMA_Presence",
                 hsMap="ABMA_c45_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ABMA_base_viz4", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "ANHE_Presence",
                 hsMap="ANHE_c45_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ANHE_base_viz4", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "ANBO_Presence",
                 hsMap="ANBO_c45_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ANBO_base_viz4", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "PSMA_Presence",
                 hsMap="PSMA_c45_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="PSMA_base_viz4", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "RALU_Presence",
                 hsMap="RALU_c45_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="RALU_base_viz4", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "RASY_Presence",
                 hsMap="RASY_c45_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="RASY_base_viz4", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

# 45south -----------------------------------------------------------------

setwd("D:/OneDrive/PhD/CSF_ABM_Project/MigClim/MigClimRFiles/New SRange/New Buffer Work/4.5/Aligned/ASC")

MigClim.migrate (iniDist = "ABMA_Presence",
                 hsMap="ABMA_c45_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ABMA_base_viz4S", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "ANHE_Presence",
                 hsMap="ANHE_c45_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ANHE_base_viz4S", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "ANBO_Presence",
                 hsMap="ANBO_c45_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ANBO_base_viz4S", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "PSMA_Presence",
                 hsMap="PSMA_c45_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="PSMA_base_viz4S", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "RALU_Presence",
                 hsMap="RALU_c45_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="RALU_base_viz4S", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "RASY_Presence",
                 hsMap="RASY_c45_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="RASY_base_viz4S", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)



# 85south -----------------------------------------------------------------

setwd("D:/OneDrive/PhD/CSF_ABM_Project/MigClim/MigClimRFiles/New SRange/New Buffer Work/8.5")

MigClim.migrate (iniDist = "ABMA_Presence",
                 hsMap="ABMA_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ABMA_base_vizS", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "ANHE_Presence",
                 hsMap="ANHE_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ANHE_base_vizS", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "ANBO_Presence",
                 hsMap="ANBO_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ANBO_base_vizS", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "PSMA_Presence",
                 hsMap="PSMA_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="PSMA_base_vizS", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "RALU_Presence",
                 hsMap="RALU_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="RALU_base_vizS", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "RASY_Presence",
                 hsMap="RASY_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="RASY_base_vizS", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)




# 85 viz sensitivity ------------------------------------------------------

#movement for ANHE is highest, and curve was lowest for important
MigClim.migrate (iniDist = "ANHE_Presence",
                 hsMap="ANHE_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(0.83, 0.68, 0.57, 0.47, 0.39, 0.32, 0.26, 0.22, 0.18, 0.15, 0.12, 0.10),
                 iniMatAge=1, propaguleProd=c(1),
                 #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ANHE_distance11_viz", replicateNb=5, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

#ABMA LDD
MigClim.migrate (iniDist = "ABMA_Presence",
                 hsMap="ABMA_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 lddFreq=0.05, lddMinDist=10, lddMaxDist=11,
                 simulName="ABMA_ldddist10_viz", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "ABMA_Presence",
                 hsMap="ABMA_c85_",
                 rcThreshold = 200,
                 envChgSteps=4,
                 dispSteps=20,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 lddFreq=0.05, lddMinDist=2, lddMaxDist=3,
                 simulName="ABMA_ldddist2_viz", replicateNb=1, overWrite=TRUE,
                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)