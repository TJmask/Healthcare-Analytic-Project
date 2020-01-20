setwd("/Users/tjmask/Desktop/Courses/health care/hw3")
library(data.table)

dt_ip = fread("VTINP16_upd.csv")
head(dt_ip)
dt_op = fread("VTOUTP16.csv")
head(dt_op)
dt_em =fread("VTED16.csv")
head(dt_em)
dt_revenue = fread("VTREVCODE16.csv")
icd = fread('REVCODE_FILE_LAYOUT_and_CODES.xlsx')
head(dt_revenue)
"507033, 40436, 859382, 1585831, 200760, 3692, 690326  "

dt_ip[UNIQ==507033]
dt_revenue[Uniq==507033]
dt_ip[UNIQ ==507033 & ERFLAG==1]

diagnosis = dt_ip[UNIQ == 859382, .(DX1, DX2, DX3, DX4, DX5, DX6, DX7, DX8, DX9, DX10, 
                                 DX11, DX12, DX13, DX14, DX15, DX16, DX17, DX18, DX19, DX20)]
icd = data.table(icd)
for (i in diagnosis[1,]) {
  print(icd[`ICD-10` == i])
}




## 1.patient 507033 is admitted by Northwestern Medical Center, admission type is Elective, admission source is NON-HEALTH CARE FACILITY POINT OF ORIGIN,
## between 25-29 years old, lives in ESSEX JUNCTION. a female, discharge status is HOME - OWN OR FAMILY CARE. principle 
## payment is BLUE CROSS, charge is $3233.29, she has problems Encounter for full-term uncomplicated delivery, and  40 weeks gestation of pregnancy.
### as well as Single live birth.  She was treated with Delivery of Products of Conception, External Approach, and Drainage of Amniotic Fluid, Therapeutic from Products of Conception,
## Via Natural or Artificial Opening. St. Albans, DISCHARGE DATE EQUALS ADMISSION DATE, she is Normal pregnancy and/or delivery
### it's Neoplasms. Other procedures to assist delivery, and Operations on the eye; day of year 2016, record num is 13363,
### bill tyoe is Hospital based,  Inpatient Final Bill. Did not have an associated Emergency Room revenue record, Not a critical access hospital
### No associated observation room revenue code, Has procedure in ICD-9-CM Range.Admit Month Jan-Mar, Discharge Month Jan-Mar
### DRG:Malignancy, female reproductive system w CC. PREGNANCY, CHILDBIRTH AND THE PUERPERIUM; Effective for discharges on Oct. 1, 2015 - Sept 30, 2016 MS - DRG

###2. Northwestern Medical Center; Hospital based,  Inpatient Final Bill $1002.13;Pharmac, $83.44,Drugs Charged to Patients;Pharmacy: IV solutions $ 92.10,Drugs Charged to Patients;
## Pharmacy: Other $31.31, Drugs Charged to Patients; Medical/Surgical Supplies, $75.22, Med Supplies Charged to Patient;Medical/Surgical Supplies: Sterile supplies,$334.33,Med Supplies Charged to Patient;
## Laboratory - Clinical Diagnostic,341.00, Laboratory - Clinical; Labor Room, 1273.76, Delivery Room & Labor Room;

dt_demographics = dt_ip[PPAY %in% c(1,2,6,7)][,.(UNIQ, intage, sex, PPAY, CHRGS, MDC)]

xtabs(CHRGS ~ MDC + PPAY + sex, dt_demographics)

### filter the useful columns from inpatient table
head(dt_ip)
dt_info = dt_ip[PPAY %in% c(1,2,6,7)][,.(UNIQ, intage, sex, PPAY, CHRGS, MDC)]
# dt_info = na.omit(dt_info)

tabulation  = xtabs(CHRGS ~ MDC + PPAY, dt_info)
rownames(tabulation) = c('BRAIN AND CNS', 'EYE', "EAR, NOSE & THROAT", "RESPIRATORY", "HEART & CIRCULATORY",
                "DIGESTIVE","LIVER & PANCREAS", "MUSCULOSKELETAL", "SKIN AND BREAST", "ENDOCRINE", "KIDNEY & URINARY",
                "MALE REPRODUCTIVE", "FEMALE REPRODUCTIVE", "PREGNANCY, CHILDBIRTH AND THE PUERPERIUM","NEONATAL",
                "SPLEEN & BLOOD", "LYMPHATIC", "INFECTION", "MENTAL ILLNESS", "SUBSTANCE ABUSE", "INJURY, TOXIC EFFECTS", 
                "BURNS", "ALL OTHER","TRAUMA", "HIV")

colnames(tabulation) = c("Medicare","Medicaid","Commercial_Payers")


### group “BLUE CROSS” + “COMMERCIAL INSURANCE” 
dt_info$PPAY = ifelse(dt_info$PPAY==6|dt_info$PPAY==7, 3, dt_info$PPAY)
nrow(dt_info)

### aggregate charges by groups
dt_group = dt_info[,.(total_charge = sum(CHRGS, na.rm=TRUE)), by =. (sex, MDC, PPAY)][order(MDC, PPAY)]
head(dt_group)



### rotate and rename columns and rows
dt_Medicare = dt_group[PPAY==1][,.(MDC,Medicare=total_charge)]
dt_Medicaid = dt_group[PPAY==2][,.(MDC, Medicaid=total_charge)]
dt_Commercial_Payers = dt_group[PPAY==3][,.(MDC, Commercial_Payers=total_charge)]
major_payers = merge(merge(dt_Medicare, dt_Medicaid, by = "MDC"), dt_Commercial_Payers, by = "MDC")
head(major_payers)
major_payers$MDC= c('BRAIN AND CNS', 'EYE', "EAR, NOSE & THROAT", "RESPIRATORY", "HEART & CIRCULATORY",
                          "DIGESTIVE","LIVER & PANCREAS", "MUSCULOSKELETAL", "SKIN AND BREAST", "ENDOCRINE", "KIDNEY & URINARY",
                          "MALE REPRODUCTIVE", "FEMALE REPRODUCTIVE", "PREGNANCY, CHILDBIRTH AND THE PUERPERIUM","NEONATAL",
                          "SPLEEN & BLOOD", "LYMPHATIC", "INFECTION", "MENTAL ILLNESS", "SUBSTANCE ABUSE", "INJURY, TOXIC EFFECTS", 
                          "BURNS", "ALL OTHER","TRAUMA", "HIV")
major_payers


### calculating the percentage of each MDC in each insurance
dt_Medicaid = dt_Medicaid[,.(MDC, Medicaid,
                             share = round(Medicaid/sum(Medicaid),4))][order(share, decreasing = TRUE)]

dt_Medicare = dt_Medicare[,.(MDC, Medicare, 
                             share = round(Medicare/sum(Medicare),4))][order(share, decreasing = TRUE)]

dt_Commercial_Payers = dt_Commercial_Payers[,.(MDC, Commercial_Payers, 
                             share = round(Commercial_Payers/sum(Commercial_Payers),4))][order(share, decreasing = TRUE)]


### rename the rows of Medicaid table
dt_Medicaid$MDC = c('BRAIN AND CNS', 'EYE', "EAR, NOSE & THROAT", "RESPIRATORY", "HEART & CIRCULATORY",
                    "DIGESTIVE","LIVER & PANCREAS", "MUSCULOSKELETAL", "SKIN AND BREAST", "ENDOCRINE", "KIDNEY & URINARY",
                    "MALE REPRODUCTIVE", "FEMALE REPRODUCTIVE", "PREGNANCY, CHILDBIRTH AND THE PUERPERIUM","NEONATAL",
                    "SPLEEN & BLOOD", "LYMPHATIC", "INFECTION", "MENTAL ILLNESS", "SUBSTANCE ABUSE", "INJURY, TOXIC EFFECTS", 
                    "BURNS", "ALL OTHER","TRAUMA", "HIV")

### rename the rows of Medicare table
dt_Medicare$MDC = c('BRAIN AND CNS', 'EYE', "EAR, NOSE & THROAT", "RESPIRATORY", "HEART & CIRCULATORY",
                    "DIGESTIVE","LIVER & PANCREAS", "MUSCULOSKELETAL", "SKIN AND BREAST", "ENDOCRINE", "KIDNEY & URINARY",
                    "MALE REPRODUCTIVE", "FEMALE REPRODUCTIVE", "PREGNANCY, CHILDBIRTH AND THE PUERPERIUM","NEONATAL",
                    "SPLEEN & BLOOD", "LYMPHATIC", "INFECTION", "MENTAL ILLNESS", "SUBSTANCE ABUSE", "INJURY, TOXIC EFFECTS", 
                    "BURNS", "ALL OTHER","TRAUMA", "HIV")

### rename the rows of Commercial_Payers table
dt_Commercial_Payers$MDC  = c('BRAIN AND CNS', 'EYE', "EAR, NOSE & THROAT", "RESPIRATORY", "HEART & CIRCULATORY",
                          "DIGESTIVE","LIVER & PANCREAS", "MUSCULOSKELETAL", "SKIN AND BREAST", "ENDOCRINE", "KIDNEY & URINARY",
                          "MALE REPRODUCTIVE", "FEMALE REPRODUCTIVE", "PREGNANCY, CHILDBIRTH AND THE PUERPERIUM","NEONATAL",
                          "SPLEEN & BLOOD", "LYMPHATIC", "INFECTION", "MENTAL ILLNESS", "SUBSTANCE ABUSE", "INJURY, TOXIC EFFECTS", 
                          "BURNS", "ALL OTHER","TRAUMA", "HIV")

### plot pie charts
library(plotly)
p_medicaid <- plot_ly(dt_Medicaid, labels = ~MDC, values = ~share, type = 'pie') %>%
  layout(title = 'Medicaid MDC Share',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p_medicare <- plot_ly(dt_Medicare, labels = ~MDC, values = ~share, type = 'pie') %>%
  layout(title = 'Medicare MDC Share',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p_Commercial_Payers <- plot_ly(dt_Commercial_Payers, labels = ~MDC, values = ~share, type = 'pie') %>%
  layout(title = 'Commercial Payers MDC Share',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))





### Question 3 
head(dt_em)
dt_em[DX1=="T401X1A"]
library(stringr)

a = which((substr(dt_em[,DX1],1,3) %in% c('T40', "T41", "T42", "T43")))
b = which(substr(dt_em[,DX2],1,3) %in% c('T40', "T41", "T42", "T43"))
c = which(substr(dt_em[,DX3],1,3) %in% c('T40', "T41", "T42", "T43"))
e = which(substr(dt_em[,DX4],1,3) %in% c('T40', "T41", "T42", "T43"))
f = which(substr(dt_em[,DX5],1,3) %in% c('T40', "T41", "T42", "T43"))
g = which(substr(dt_em[,DX6],1,3) %in% c('T40', "T41", "T42", "T43"))
h = which(substr(dt_em[,DX7],1,3) %in% c('T40', "T41", "T42", "T43"))
i = which(substr(dt_em[,DX8],1,3) %in% c('T40', "T41", "T42", "T43"))
j = which(substr(dt_em[,DX9],1,3) %in% c('T40', "T41", "T42", "T43"))
k = which(substr(dt_em[,DX10],1,3) %in% c('T40', "T41", "T42", "T43"))
l = which(substr(dt_em[,DX11],1,3) %in% c('T40', "T41", "T42", "T43"))
m = which(substr(dt_em[,DX12],1,3) %in% c('T40', "T41", "T42", "T43"))
n = which(substr(dt_em[,DX13],1,3) %in% c('T40', "T41", "T42", "T43"))
o = which(substr(dt_em[,DX14],1,3) %in% c('T40', "T41", "T42", "T43"))
p = which(substr(dt_em[,DX15],1,3) %in% c('T40', "T41", "T42", "T43"))
q = which(substr(dt_em[,DX16],1,3) %in% c('T40', "T41", "T42", "T43"))
r = which(substr(dt_em[,DX17],1,3) %in% c('T40', "T41", "T42", "T43"))
s = which(substr(dt_em[,DX18],1,3) %in% c('T40', "T41", "T42", "T43"))
t = which(substr(dt_em[,DX19],1,3) %in% c('T40', "T41", "T42", "T43"))
u = which(substr(dt_em[,DX20],1,3) %in% c('T40', "T41", "T42", "T43"))

## get unique positions
unique_position = unique(c(a,b,c,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u))
abuse_unique = dt_em[unique_position]
head(abuse_unique)
nrow(abuse_unique) ##2151

## abuse for male
nrow(abuse_unique[sex==1]) ##1009
## abuse for male
nrow(abuse_unique[sex==2]) ##1141


## spent on identified patients
total_spent = abuse_unique[,.(spent = sum(CHRGS))]


## abuse_unique[UNIQ %in% c(507033, 40436, 859382, 1585831, 200760, 3692, 690326)]
abuse_major_insurance = abuse_unique[PPAY %in% c(1,2,6,7)][,.(PPAY, CHRGS)]
abuse_major_insurance$PPAY = 
  ifelse(abuse_major_insurance$PPAY==6|abuse_major_insurance$PPAY==7, 3, abuse_major_insurance$PPAY)
abuse_major_insurance_each =abuse_major_insurance[,.(total_abuse = sum(CHRGS)), by = PPAY][order(PPAY)]
total_payments = x[,.(total = sum(CHRGS, na.rm=TRUE)), by = PPAY][order(PPAY)]

df_abuse_share = merge(abuse_major_insurance_each, total_payments, by="PPAY")[,.(PPAY, share=total_abuse/total)]
df_abuse_share$PPAY = c("Medicare","Medicaid",'Commercial Payers')
df_abuse_share


abuse_major_insurance = abuse_unique[PPAY %in% c(1,2,6,7)][,.(PPAY, CHRGS)]
abuse_major_insurance = na.omit(abuse_major_insurance)
nrow(abuse_major_insurance) ##2000





x = dt_em[PPAY %in% c(1,2,6,7)][,.(PPAY, CHRGS)]
x$PPAY = ifelse(x$PPAY==6|x$PPAY==7, 3, x$PPAY)







abuse_major_insurance$PPAY = ifelse(abuse_major_insurance$PPAY==6|abuse_major_insurance$PPAY==7, 3, x$PPAY)
abuse_major_insurance_share = abuse_major_insurance[,.(total_abuse = sum(CHRGS)), by = PPAY][order(PPAY)]
a =abuse_major_insurance_share[,.(PPAY, share= total_abuse/sum(total_abuse))]
a$PPAY =   c("Medicare","Medicaid",'Commercial Payers')
a


total_payments = dt_info[,.(total = sum(CHRGS, na.rm=TRUE)), by = PPAY][order(PPAY)]

### share 
df_abuse_share = merge(abuse_major_insurance_share, total_payments, by="PPAY")[,.(PPAY, share=total_abuse/total)]
df_abuse_share$PPAY = c("Medicare","Medicaid",'Commercial Payers')
df_abuse_share
write.csv(df_abuse_share, "/Users/tjmask/Desktop/Courses/health care/hw3/abuse_share.csv")

a1 = which(substr(dt_em[,DX1],1,4) == "T404"| substr(dt_em[,DX1],1,5) =="T4362")
a2 = which(substr(dt_em[,DX2],1,4) == "T404"| substr(dt_em[,DX2],1,5) =="T4362")
a3 = which(substr(dt_em[,DX3],1,4) == "T404"| substr(dt_em[,DX3],1,5) =="T4362")
a4 = which(substr(dt_em[,DX4],1,4) == "T404"| substr(dt_em[,DX4],1,5) =="T4362")
a5 = which(substr(dt_em[,DX5],1,4) == "T404"| substr(dt_em[,DX5],1,5) =="T4362")
a6 = which(substr(dt_em[,DX6],1,4) == "T404"| substr(dt_em[,DX6],1,5) =="T4362")
a7 = which(substr(dt_em[,DX7],1,4) == "T404"| substr(dt_em[,DX7],1,5) =="T4362")
a8 = which(substr(dt_em[,DX8],1,4) == "T404"| substr(dt_em[,DX8],1,5) =="T4362")
a9 = which(substr(dt_em[,DX9],1,4) == "T404"| substr(dt_em[,DX9],1,5) =="T4362")
a10 = which(substr(dt_em[,DX10],1,4) == "T404"| substr(dt_em[,DX10],1,5) =="T4362")
a11 = which(substr(dt_em[,DX11],1,4) == "T404"| substr(dt_em[,DX11],1,5) =="T4362")
a12 = which(substr(dt_em[,DX12],1,4) == "T404"| substr(dt_em[,DX12],1,5) =="T4362")
a13 = which(substr(dt_em[,DX13],1,4) == "T404"| substr(dt_em[,DX13],1,5) =="T4362")
a14 = which(substr(dt_em[,DX14],1,4) == "T404"| substr(dt_em[,DX14],1,5) =="T4362")
a15 = which(substr(dt_em[,DX15],1,4) == "T404"| substr(dt_em[,DX15],1,5) =="T4362")
a16 = which(substr(dt_em[,DX16],1,4) == "T404"| substr(dt_em[,DX16],1,5) =="T4362")
a17 = which(substr(dt_em[,DX17],1,4) == "T404"| substr(dt_em[,DX17],1,5) =="T4362")
a18 = which(substr(dt_em[,DX18],1,4) == "T404"| substr(dt_em[,DX18],1,5) =="T4362")
a19 = which(substr(dt_em[,DX19],1,4) == "T404"| substr(dt_em[,DX19],1,5) =="T4362")
a20 = which(substr(dt_em[,DX20],1,4) == "T404"| substr(dt_em[,DX20],1,5) =="T4362")

abuse_postion = unique(c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,
                         a11,a12,a13,a14,a15,a16,a17,a18,a19,a20))

dt_em[abuse_postion]
nrow(dt_em[abuse_postion]) ### 156

### zipcode 
abuse_unique[,.(abuse_total = .N), by = TXTZIP][order(abuse_total, decreasing = TRUE)]
write.csv(df_abuse_share, "/Users/tjmask/Desktop/Courses/health care/hw3/abuse_share.csv")



### 10 common
head(abuse_unique)

diagnosis = as.vector(as.matrix((abuse_unique[, 10:29])))
length(diagnosis)
diagnosis_abuse = diagnosis[substr(diagnosis,1,3) %in% c('T40', "T41", "T42", "T43")]
length(diagnosis_abuse)
times = as.data.table(table(diagnosis_abuse))[order(N, decreasing = TRUE)]
top_10 = head(times,10)
top_10


ed$newcol <- paste(ed$DX1, ed$DX2, ed$DX3, ed$DX4, ed$DX5,ed$DX6, 
                   ed$DX7, ed$DX8, ed$DX9, ed$DX10, ed$DX11, ed$DX12,
                   ed$DX13, ed$DX14, ed$DX15, ed$DX16, ed$DX17, 
                   ed$DX18, ed$DX19, ed$DX20, sep='-')

ed[, newcol]

ed_drug <- ed[grepl("T40", ed[["newcol"]]) | grepl("T41", ed[["newcol"]]) | grepl("T42", ed[["newcol"]]) | grepl("T43", ed[["newcol"]]), ]

ed_drug$newcol = NULL

fwrite(ed_drug, file = "ed_drug.csv")





repeated_positions = c(a,b,c,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)
length(repeated_positions)

times = as.data.table(table(repeated_positions))[order(N, decreasing = TRUE)]
position_10 = as.numeric((head(times,10))$repeated_positions)
position_10 = c(21686, 5935,  9136, 10555, 15051, 19564,  962, 10792, 15248, 15355)
top_10  = dt_em[position_10]


dt_em[reapted_positions][,.(.N), by = .(DX1)]
dt_em[reapted_positions][,.(.N), by = .(DX2)]

write.csv(top_10, "/Users/tjmask/Desktop/Courses/health care/hw3/top_10.csv")



abuse_unique[UNIQ == 19314]








for (i in length(dt_em[,DX1])){
  
}
disease = c("DX1","DX2")
disease = c("DX1", "DX2", "DX3", "DX4", "DX5", "DX6", "DX7", "DX8", "DX9", "DX10",
            "DX11", "DX12", "DX13", "DX14 ", "DX15", "DX16", "DX17", "DX18", "DX19","DX20")
for ( i 1:length(disease)){
  dt_em[,dt_em[,disease[i]]]
  print(dt_em[,disease[i]])
}

dt_em[,disease[1]]
names1 = as.vector(colnames(dt_em)[colnames(dt_em) %in% disease])

class(names1)

disease[2]


dt_em[which((substr(dt_em[,DX1],1,3) %in% c('T40', "T41", "T42", "T43")))]
head(dt_em)
dt_em[]

length(which((substr(dt_em[,DX1],1,3) %in% c('T40', "T41", "T42", "T43"))))

for (i in 1:20){
  
}



length(b)
c = which(substr(dt_em[,DX3],1,3) %in% c('T40', "T41", "T42", "T43"))
length(c)

which(dt_em[,DX2] %in% dt_em[,DX2])

h = c(a, b,c)

length(unique(h))

h = union(c,union(a,b))
length(h)

dt_em[d]
d= union(a,b,c)
length(union(a,b))
1074+214+185


length(which(substr(dt_em[,DX2],1,3) %in% c('T40', "T41", "T42", "T43")))

dt_em[]












donuts <- function(x, group = 1, labels = NA, col = NULL, radius = c(.7,1)) {
  group <- rep_len(group, length(x))
  ug  <- unique(group)
  tbl <- table(group)[order(ug)]
  col <- if (is.null(col))
  seq_along(ug) else rep_len(col, length(ug))
  col.main <- Map(rep, col[seq_along(tbl)], tbl)
  col.sub  <- lapply(col.main, function(x) {
  al <- head(seq(0, 1, length.out = length(x) + 2L)[-1L], -1L)
  Vectorize(adjustcolor)(x, alpha.f = al)
  })
  plot.new()
  par(new = TRUE)
  pie(x, border = NA, radius = radius[2L],
      col = unlist(col.sub), labels = labels)
  
  par(new = TRUE)
  pie(x, border = NA, radius = radius[1L],
      col = unlist(col.main), labels = NA)
}

with(dt_Medicaid,
     donuts(dt_Medicaid$share,as.vector(dt_Medicaid$MDC), sprintf('%s: %s%%', dt_Medicaid$MDC, dt_Medicaid$share),
           )
)


par(mfrow = c(1,2), mar = c(0,4,0,4))

with(browsers,
     donuts(share, browser, sprintf('%s: %s%%', version, share),
            col = c('cyan2','red','orange','green','dodgerblue2'))
)

with(mtcars,
     donuts(mpg, interaction(gear, cyl), rownames(mtcars))
)


install.packages("plotrix")
library(ggplot2)
library(plotrix)
pie3D(dt_Medicaid$Medicaid, labels = dt_Medicaid$MDC, 
      main = "An exploded 3D pie chart", explode=0.1, radius=.9, labelcex = 1.2,  start=0.7)


