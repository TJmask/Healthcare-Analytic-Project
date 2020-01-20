setwd('/Users/tjmask/Desktop/CPSC_Enrollment_2019_10')
library(data.table)
library(stringr)


# Question 1

## geting the data 
enrollment  = fread('CPSC_Enrollment_Info_2019_10.csv')
contract = fread('CPSC_Contract_Info_2019_10.csv')
major_insurance = fread('MajorInsuranceOrgName.csv')
monthly_report_by_plan = fread('Monthly_Report_By_Plan_2019_10.csv')


## removing the rows that contain '*'
enrollment= enrollment[Enrollment!='*'] 

## keeping all keep all the Hxxxx and Rxxxx and Exxxx contracts
enrollment_new =  enrollment[substr(enrollment$`Contract Number`, 1, 1) != 'S']
monthly_report_by_plan_new = monthly_report_by_plan[substr(monthly_report_by_plan$`Contract Number`, 1, 1) != 'S']

## merge table major_insurance with monthly_report_by_plan_new by 'Organization Marketing Name'
df_merge = merge(major_insurance, monthly_report_by_plan_new, 
                 by = 'Organization Marketing Name')

## merge enrollment_new with newly merged table to get all the infomation by 'Contract Number' and 'Plan ID'
df_all_info = merge(x= enrollment_new, y=df_merge, by =c('Contract Number','Plan ID'), x.all = TRUE)

## getting our group's assigned states
states_group1 = c('CA','OH','WA','MD','LA','IA','NM','ME','AK' )

## aggregate enrollments by assigned states and MajorInsuranceOrgName
df_company_state = df_all_info[,.(company_total = sum(as.numeric(Enrollment.x))), 
                               by = .(State, MajorInsuranceOrgName)]
df_company_state_group1 =  df_company_state[(State %in% states_group1)]

## aggregate enrollments by assigned states
df_state = df_all_info[,.(total = sum(as.numeric(Enrollment.x))), by = .(State)]
df_state_group1 = df_state[(State %in% states_group1)]

## calculating the shares of different companies in different states
df_ratio = merge(df_company_state_group1, df_state_group1, by = 'State')
df_share = df_ratio[,.(MajorInsuranceOrgName,State,share= company_total/total)]
df_share = df_share[with(df_share,order(State,share, decreasing = TRUE)),]
write_csv(df_share, '/Users/tjmask/Desktop/CPSC_Enrollment_2019_10/shares.csv')

## getting HHIs
df_share$share_100 = df_share$share*100
df_HHIs = df_share[,.(HHIs=round(sum(share_100 ^2), 2)), by = State]
df_HHIs = df_HHIs[with(df_HHIs,order(HHIs, decreasing = TRUE)),]

setkey(df_HHIs, HHIs)
write_csv(df_HHIs, '/Users/tjmask/Desktop/CPSC_Enrollment_2019_10/HHIs.csv')



# Question 3
eoc_report = fread('EOC170.csv')
head(eoc_report)

## cleaning data 
eoc_report = eoc_report[substr(`Contract Number`,1,1)!='S']
eoc_report$`EOC170-0010`= as.numeric(eoc_report$`EOC170-0010`)
eoc_report = na.omit(eoc_report)

## get the top-10 biggest market share insurance companies
df_share_10 = df_share[,.(order = order(share), 
                          share, MajorInsuranceOrgName), by=State][order<=10]

## merge tables 
df_all_share = merge(df_share_10, df_all_info, 
                     by = c('MajorInsuranceOrgName','State'))[,.(`Contract Number`, 
                                                                 State, MajorInsuranceOrgName,Enrollment.x)]
df_eoc_all =merge(df_all_info, eoc_report, 
                by = 'Contract Number')[,.(`Contract Number`,State,  `MajorInsuranceOrgName`,`EOC170-0010`)]

df_eoc_all = unique(df_eco_all)
df_uod = merge(df_eoc_all, df_all_share, by =c("Contract Number","State"))

## calculating the average weight
df_weight  = df_uod[,.(weighted_avg = as.numeric(`EOC170-0010`)/1000*as.numeric(Enrollment.x)/sum(as.numeric(Enrollment.x))),
            by =.(`Contract Number`,State, MajorInsuranceOrgName.y)]

df_avg_weight = df_weight[,.(avg_weight = sum(weighted_avg)), by =.(`Contract Number`,State, MajorInsuranceOrgName.y)]

## ranking 
df_rank = df_avg_weight[with(df_avg_weight,order(State,avg_weight, decreasing = TRUE)),]




# Question 3
eoc_report = fread('EOC170.csv')
head(eoc_report)

## cleaning data 
eoc_report = eoc_report[substr(`Contract Number`,1,1)!='S']
eoc_report$`EOC170-0010`= as.numeric(eoc_report$`EOC170-0010`)
eoc_report = na.omit(eoc_report)
View(eoc_report)
nrow(eoc_report)
head(eoc_report)
head(df_all_info)
df_all_info[df_all_info$`Contract Number`=='H0028']



## get the top-10 biggest market share insurance companies
df_share_10 = df_share[,.(order = order(share), 
                          share, MajorInsuranceOrgName), by=State][order<=10]
head(df_share_10)
nrow(df_share_10)
head(df_all_info)
head(eoc_report)
head(df_all_info)
df_all_share = merge(df_share_10, df_all_info, 
                     by = c('MajorInsuranceOrgName','State'))[,.(`Contract Number`, 
                       State, MajorInsuranceOrgName,Enrollment.x)]
head(df_all_share)

df_eco_all =merge(df_all_info, eoc_report, 
                  by = 'Contract Number')[,.(`Contract Number`,
                      State,  `MajorInsuranceOrgName`,`EOC170-0010`)]
head(df_eco_all)
df_eco_all = unique(df_eco_all)

df_uod = merge(df_eco_all, df_all_share, by =c("Contract Number","State"))
                                     
head(df_uod)

a  = df_uod[,.(weighted_avg = as.numeric(`EOC170-0010`)/1000*as.numeric(Enrollment.x)/sum(as.numeric(Enrollment.x))),
       by =.(`Contract Number`,State, MajorInsuranceOrgName.y)]
setkey(a, State )
tail(a,50)

b = a[,.(avg_weight = sum(weighted_avg)), by =.(`Contract Number`,State, MajorInsuranceOrgName.y)]
b 
nrow(b)





length(unique(df_share_10$MajorInsuranceOrgName))

length(unique(df_all_share$MajorInsuranceOrgName))

?merge





merge(df_all_info, eoc_report, by = 'Contract Number', y.all=TRUE)
length(unique(eoc_report$`Contract Number`))
head(a)
nrow(a)







df_share$HHIs = calculate_HHIs()
HHIs = 0
i=0
j=0
calculate_HHIs = function(x){
  for (i in c(unique(df_share$State))){
    for (j in (df_share$share_100)){
      HHIs = HHIs + j*j
    }
  }
}
HHI = calculate_HHIs()
HHI
df_share[,.(HHIs=calculate_HHIs())]
class(calculate_HHIs())

states = rbind('CA','OH','WA','MD','LA','IA','NM','ME','AK')

HHIs = c(266652, 293317.2, 319982.4,346647.6, 373312.8, 399978, 426643.2,453308.4, 479973.6)
df_HHIs = c(states, HHI)
df_HHIs = data.table(df_HHIs)
setkey(df_HHIs, 'HHIs',physical= FALSE)



write_csv(df_HHIs,'/Users/tjmask/Desktop/CPSC_Enrollment_2019_10/HHIs.csv' )

head(enrollment_new)
head(contract)
head(monthly_report_by_plan_new)
nrow(monthly_report_by_plan)
nrow(monthly_report_by_plan_new)
head(major_insurance)

df_merge = merge(major_insurance, monthly_report_by_plan_new, 
                 by = 'Organization Marketing Name')
head(df_merge)
nrow(df_merge)
nrow(df_merge[substr(df_merge$`Contract Number`, 1, 1) != 'S'])
df_merge = df_merge[Enrollment!='*']
major_insurance$`Organization Marketing Name`

head(df_merge)
head(enrollment_new)

df_all_info = merge(x= enrollment_new, y=df_merge, by =c('Contract Number','Plan ID'), x.all = TRUE)

head(enrollment_new)
length(unique(df_merge$`Contract Number`))


nrow(df_all_info)

head(df_all_info)
states_group1 = c('CA','OH','WA','MD','LA','IA','NM','ME','AK' )

head(df_company_state)
df_company_state = df_all_info[,.(company_total = sum(as.numeric(Enrollment.x))), by = .(State, MajorInsuranceOrgName)]
df_company_state_group1 =  df_company_state[(State %in% states_group1)]
head(df_company_state_group1 )  

df_state = df_all_info[,.(total = sum(as.numeric(Enrollment.x))), by = .(State)]
df_state_group1 = df_state[(State %in% states_group1)]
head(df_state_group1)

df_ratio = merge(df_company_state_group1, df_state_group1, by = 'State')
head(df_ratio)

df_share = df_ratio[,.(MajorInsuranceOrgName,State,share= company_total/total)]
head(df_share)

write_csv(df_share, '/Users/tjmask/Desktop/CPSC_Enrollment_2019_10/shares.csv')
