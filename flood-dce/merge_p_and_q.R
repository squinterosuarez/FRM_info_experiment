# merge Prolific demographics across pilot and full first
# then merge with Qualtrics export
library("tidyr")
library("dplyr")
# several respondents have revoked consent
getwd()
p_pilot <- read.csv("data/prolific_demographic_export_pilot.csv", header = T, row.names = 1)
p_full <- read.csv("data/prolific_demographic_export_full.csv", header = T, row.names = 1)

p <- full_join(p_full, p_pilot)

# drop returned
p <- p[!p$Status=="RETURNED", ]

# drop_consent revoked
p <- p[!p$Age=="CONSENT_REVOKED", ]

str(p)
p$Age <- as.integer(p$Age)
hist(p$Time.taken[p$Time.taken<1000])
table(p$Current.uk.area.of.residence)
p$Current.uk.area.of.residence <- ifelse(grepl("East Anglia", p$Current.uk.area.of.residence), 
           "East of England (East Anglia, Bedfordshire and Hertfordshire, Essex)",
           p$Current.uk.area.of.residence)

# merge with Qualtrics
d_full <- read.csv("data/FRM_experiment_full.csv", header = T)

# remove preview fill
d_full <- d_full[!d_full$Status == "Survey Preview", ]

# remove row 1
d_full <- d_full[-1,]

colnames(p)[1] <- "Prolific.ID"
colnames(d_full)

# merge
pd <- full_join(p, d_full, by = "Prolific.ID")

pd <- pd[, grepl("task7|task8|Create.a.New.Field|Create.New.Field", colnames(pd))==F]

pd <- pd %>% select(Prolific.ID, Time.taken, Current.uk.area.of.residence,
                    Age, Sex, Ethnicity.simplified, Student.status, Employment.status,
                    Duration..in.seconds., K2, T2, X1_ChoiceTask:Q49, 
                    treatment_arm: task6_choice)
# remove speeders
#d_full <- d_full[d_full$Duration..in.seconds. > 180, ]
pd <- pd[grepl("ImportId", pd$Prolific.ID)==F, ]
pd <- pd[!pd$T2=="", ]

table(is.na(pd$Time.taken))

write.csv(pd, "data/full_results.csv")
