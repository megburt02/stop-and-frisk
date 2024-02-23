##################################
## MEG BURT ######################
## PORTFOLIO PROJ. ###############
## INQUIRE: megburt02@gmail.com ##
##################################

rm(list=ls(all=TRUE))


# OJBECT "saf" IS A DATASET OF ALL STOP AND FRISK INCIDENTS IN NYC IN 2006.
# New York (N.Y.). Police Department. New York Police Department (NYPD) Stop, Question, and Frisk Database, 2006. Inter-university Consortium for Political and Social Research [distributor], 2008-06-09. https://doi.org/10.3886/ICPSR21660.v1
# ACCESS FOR FREE: https://www.icpsr.umich.edu/web/ICPSR/studies/21660
saf <- read.delim("C:/Users/megbu/OneDrive/Desktop/Work/Portfolio/ICPSR_21660/DS0001/21660-0001-Data.tsv")

# PRE-PROCESSING
saf <- saf[,c("CASEID","PCT","DATESTOP", "SEX", "RACE", "HT_TOTAL", "OFFCODE","ARSTMADE", "OFFUNIF")]
saf <- replace(saf, saf == -9 | saf == "Z", NA) 

# CREATE NEW RACE VARIABLES: 0/1 BLACK, 0/1 WHITE
saf$RACE <- as.factor(saf$RACE) 
saf$BLACK <- ifelse((saf$RACE == "B" | saf$RACE == "P"),1,0)
saf$WHITE <- ifelse(saf$RACE == "W",1,0)

# SUBSET ON OFFENSE CODE "CRIMINAL POSESSION OF A WEAPON"
# AS IN S. GOEL ET AL. 2016
saf_cpw <- subset(saf, saf$OFFCODE == 20)

# LOGIT MODELING
# IF AN ARREST RESULT INDICATES A "LEGITIMATE" STOP AND FRISK, WHAT ARE THE DETERMINANTS OF ARREST?
model <- glm(ARSTMADE ~ BLACK + WHITE + HT_TOTAL + SEX,family=binomial(link='logit'),data=saf_cpw)
summary(model)

# CONCLUSIONS:
# Black people were more likely to be stopped without a subsequent arrest, supporting the hypothesis that they are often stopped because of racial bias rather than "legitimate" suspicion.
# Men were more likely than women to be stopped without a subsequent arrest.
# Height did not influence whether the subject was arrested or not, indicating that race and gender had more effect than stature in the stop and frisk policy.

