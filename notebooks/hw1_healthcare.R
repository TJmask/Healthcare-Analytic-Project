library(data.table)
install.packages('bit64')
data_npi = fread('/Users/tjmask/Desktop/npidata_1000.csv', check.names = T)
View(data_npi)
select = c("Entity.Type.Code", "Provider.Business.Practice_0001","Provider.Gender.Code","Healthcare.Provider.Taxonomy.Cod","Provider.License.Number.State.Co","Is.Sole.Proprietor")
data_npi_new = data_npi[, ..select]
head(data_npi_new)
state = data_npi_new[Provider.Business.Practice_0001
             %in% c('TX','NC','IN','MD','LA','UT','NE','MT','VT')]
male_sole = state[Is.Sole.Proprietor =='Y'& Provider.Gender.Code =='M']
male_sole[,.(Provider.Gender.Code, Is.Sole.Proprietor)]
head(male_sole)
nrow(male_sole)
female_sole = state[Is.Sole.Proprietor =='Y'& Provider.Gender.Code =='F']
nrow(female_sole)

male_not_sole = state[Is.Sole.Proprietor =='N'& Provider.Gender.Code =='M']
nrow(male_not_sole)
female_not_sole = state[Is.Sole.Proprietor =='N'& Provider.Gender.Code =='F']
nrow(female_not_sole)

fisher.test()
female = state[Is.Sole.Proprietor =='N']
head(female)


#setnames(a, c("Entity Type Code","Provider Business Practice_0001"), c("code", "provider1"))
install.packages("stats")

fisher.test(matrix(c(nrow(male_sole),nrow(female_sole),nrow(male_not_sole),nrow(female_not_sole)), nrow=2))
View(data_npi_new)


## Low  risk/reward -- “Obstetrics & Gynecology”: 207V00000X; “Pediatrics”         : 208000000X
## High risk/reward -- “Surgery”                : 208600000X; “Orthopaedic Surgery”: 207X00000X



risk_gender = state[Provider.Gender.Code %in% c('M','F') &
                                 Healthcare.Provider.Taxonomy.Cod %in% c('207V00000X','208000000X','208600000X','207X00000X'),
                               .(Provider.Gender.Code, Healthcare.Provider.Taxonomy.Cod)]
head(risk_gender)

High_risk_male = risk_gender[Healthcare.Provider.Taxonomy.Cod %in% c('208600000X','207X00000X')& Provider.Gender.Code =='M']
nrow(High_risk_male)

High_risk_female = risk_gender[Healthcare.Provider.Taxonomy.Cod %in% c('208600000X','207X00000X')& Provider.Gender.Code =='F']
nrow(High_risk_female)


low_risk_male = risk_gender[Healthcare.Provider.Taxonomy.Cod %in% c('207V00000X','208000000X')& Provider.Gender.Code =='M']
nrow(low_risk_male)

low_risk_female = risk_gender[Healthcare.Provider.Taxonomy.Cod %in% c('207V00000X','208000000X')& Provider.Gender.Code =='F']
nrow(low_risk_female)

fisher.test(matrix(c(nrow(High_risk_male),nrow(High_risk_female),nrow(low_risk_male),nrow(low_risk_female)), nrow=2))


matrix(c(1,2,3,4),nrow = 2)
