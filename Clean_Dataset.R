survey_orginal <- read_csv("survey.csv")
colnames(survey_orginal)

#Removing unneccessary columns
survey_orginal <- survey_orginal[,-c(1,4,5,27)]

#Convert Gender into 3 categories
Male_List <- c("male", "guy (-ish) ^_^","m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man","cis male")
Female_List <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail")
Trans_List <- c("a little about you","p","trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means" )

survey_orginal$Gender <- sapply(as.factor(survey_orginal$Gender), function(x) ifelse(x %in% Male_List, "Male",
                                    ifelse(x %in% Female_List, "Female", "Trans")))
#unique(survey$Gender)


#Only consider participants with age 18-100, assuming others providing unreliable answers
survey_orginal <- survey_orginal[!(survey_orginal$Age>100|survey_orginal$Age<18),]

#Check NA values in each columns (only 'work_interfere' and 'self_employed')
colSums(is.na(survey_orginal))

#Check most occurrances
table(survey_orginal$self_employed) #No
table(survey_orginal$work_interfere) #Sometimes

#replace the NA columns with mode
survey_orginal$work_interfere[is.na(survey_orginal$work_interfere)] <- "Sometimes"
survey_orginal$self_employed[is.na(survey_orginal$self_employed)] <- "No"

#Check NA values again
colSums(is.na(survey_orginal))


#write_csv(survey_orginal,"survey_cleaned.csv", col_names = TRUE)

