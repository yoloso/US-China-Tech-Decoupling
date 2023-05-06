#Set working directory
setwd ()   #insert folder path


#Import data
clean = read.csv("data_raw.csv")

#Install package: Uncomment if you haven't install these packages:
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("fastDummies")
# install.packages("standardize")
# install.packages("dplyr")
# install.packages ("anytime")

#Load package
library ("tidyverse")
library("fastDummies")
library("standardize")
library ("ggplot2")
library ("dplyr")
library ("anytime")


#Change all column name into lower case
names (clean) = tolower(names(clean))

#remove unnecessary columns
clean = select (clean, -starts_with("time"))
clean = select (clean, -ends_with("nps_group"))
clean = select (clean, -contains("timer"))
clean = select (clean, -c("feed":"tr_us"))
clean = select (clean, -c("recordeddate":"externalreference"))
clean = select (clean, -c("term":"ls"))



#filter data based on pre-treatment attention checkers: check if completed
# consent, etc. 
#is.character(raw$main_q)
clean = subset (clean, clean$progress==100 &
                 clean$finished==1 &
                  clean$consent==1&
                  clean$s1_citizenship==1&
                  clean$tech_7==999 &
                  clean$s3_birth_year == clean$d3_birth_year &
                  clean$main_q==4)

clean = subset (clean, clean$gc ==1)


#clean$control_q = as.numeric (clean$control_q)
#clean$treat1_q= as.numeric (clean$treat1_q)
#clean$treat2_q= as.numeric (clean$treat2_q)
#clean$treat3_q= as.numeric (clean$treat3_q)

#control attention checker: only treated group (NA) or control group + choose 
#correct answer (3) are selected. Others drop
clean$control_q[is.na(clean$control_q)] <- 0
clean = subset (clean, clean$control_q==3 | clean$control_q==0)

#treatment 1: treat 1 answer correct (=2) OR treat2|3 (NA)
clean$treat1_q [is.na(clean$treat1_q)]=0
clean = subset(clean,clean$treat1_q==2 | clean$treat1_q==0)

#treatment 2: treat 2 answer correct(=1) or treat1|3 (NA)
clean$treat2_q [is.na(clean$treat2_q)]=0
clean = subset(clean, clean$treat2_q==1 | clean$treat2_q==0)

#treatment 3: treat 3 answer correct (=4) or treat 1|2 (NA)
clean$treat3_q [is.na(clean$treat3_q)]=0
clean = subset(clean, clean$treat3_q==4 | clean$treat3_q==0)

#create treatment group
#control group: neutral paragraph
#treatment1: China-US competition
#treatment2: Sanction
#treatment3: Gov't use of data

# mutate = add new variables + keep other variables. !=0 --> easy to code and avoid mistake
clean = clean%>%
  mutate(treatment = case_when (control_q!=0 ~ 'control',
                                treat1_q!=0 ~ 'treat1',
                                treat2_q!=0 ~ 'treat2',
                                treat3_q!=0 ~ 'treat3'))
# unique (clean$treatment)
# table (clean$treatment)
# 689 + 608 + 524 + 621
# 686 + 606 + 523 + 615
# 3817 - 2430

#1375 NA in this dataset!!!

#remove NAs in the treatment column
clean= clean[!(is.na(clean$treatment)), ]

#dummify the treatment column: from 1 column named "treatment" --> create 4 columns
clean = dummy_cols(clean, select_columns = 'treatment')

#head(clean)

#removing the NAs in the outcome columns
#remove NAs in outcome 12.1 and 13.1 (because the above command can't work on these 2 columns)
for (i in 74:108) {
  clean = clean[!(is.na(clean[i])), ]
}

clean = clean[!is.na(clean$out12.1), ]
clean = clean[!is.na(clean$out13.1), ]

###Control variables instrument (X)
#create age variable
clean$age = 2021 - clean$d3_birth_year
#female dummy (female = 1): unnecessary  
clean$female = clean$s2_female
clean$male = 1-clean$s2_female

#minority dummy (minority=1): Unnecessary
clean$minority = clean$s6_minority
#rural dummy (rural = 1, other = 0)
clean = clean %>%
  mutate(rural_hukou = case_when (d2_rural_hukou ==1 ~ 1, TRUE~0))

clean = clean %>%
  mutate (rural_growup = case_when (d1_rural_region==1 ~ 1, TRUE ~0))

#currently employed/working dummy (employed =1)
clean$employed = clean$d8_work_fulltime

#sets of education dummy (below HS, HS or equivalent, college or equivalent, above college)
clean = clean %>%
  mutate (edu_below_hs = case_when (s7_edu==1 | s7_edu==2 ~1, TRUE ~0))

#(Melody uses the case_when command)
clean = clean %>%
  mutate (edu_hs=if_else(s7_edu ==3 | s7_edu==4, 1, 0))

clean = clean %>%
  mutate(edu_college = case_when(s7_edu==5 | s7_edu ==6 ~ 1, TRUE ~ 0))

clean = clean %>%
  mutate (edu_above_college = case_when (s7_edu==7 | s7_edu==8 ~1, TRUE ~0))

#province dummy: change name so that no mistakes after dummny_cols --> create multiple dummy cols
clean = rename(clean, s4_province = s4_province_1)
clean = dummy_cols(clean, select_columns = 's4_province')

#income dummy
clean = clean %>%
  mutate (income_below_8k=case_when (d4_income_level <6 ~1, TRUE ~0))
clean = dummy_cols (clean, select_columns = 'd4_income_level')

#nature of job/work type
clean = clean %>%
  mutate (job_public = case_when(d9_work_type==1 | d9_work_type==2 ~1, TRUE ~0))

clean = clean %>%
  mutate (job_private = case_when (d9_work_type==3 ~1, TRUE ~0))

clean = clean %>%
  mutate (job_none = case_when (d9_work_type ==5 | d9_work_type==6 ~1, TRUE ~0))

clean = clean %>%
  mutate (student=case_when(d9_work_type==4 ~ 1, TRUE ~0))

#party affiliated 
clean = clean %>%
  mutate (party_aff = case_when (d6_party==1 ~ 1, TRUE ~ 0))

## ideology 1
# Nationalistic:
# H(4): most nationalistic
# L(1): least nationalistic

#step1: change the H/L scale of the raw data of the ide1 question
#clean$ide1_1 = as.numeric (clean$ide1_1)
#clean$ide1_2 = as.numeric (clean$ide1_2)
#clean$ide1_3 = as.numeric (clean$ide1_3)
#clean$ide1_4 = as.numeric (clean$ide1_4)
#clean$ide1_5 = as.numeric (clean$ide1_5)


clean$nationalistic_1 = clean$ide1_1
clean$nationalistic_2 = 5- clean$ide1_2
clean$nationalistic_3 = clean$ide1_3
clean$nationalistic_4 = 5 - clean$ide1_4
clean$nationalistic_5 = clean$ide1_5

#step2: standardized (use the standardize package): "scale" command is to get z-score
# https://cran.r-project.org/web/packages/standardize/vignettes/using-standardize.html
clean$nationalistic_scaled_1 = scale (clean$nationalistic_1) [,1]
clean$nationalistic_scaled_2 = scale (clean$nationalistic_2) [,1]
clean$nationalistic_scaled_3 = scale (clean$nationalistic_3) [,1]
clean$nationalistic_scaled_4 = scale (clean$nationalistic_4) [,1]
clean$nationalistic_scaled_5 = scale (clean$nationalistic_5) [,1]

#step3: sum the normalized scores to create an index
clean = clean %>%
  rowwise() %>%
  mutate (nationalistic_scaled_score = sum (c_across(starts_with("nationalistic_scaled_")), na.rm=T))

#step4: standardize again 
clean$nationalistic_index = scale (clean$nationalistic_scaled_score) [,1]

##ideology 2
# H(4): most conservative
# H(1): most liberal

#step1: change the H/L raw data 
#clean$ide2_1 = as.numeric (clean$ide2_1)
#clean$ide2_2 = as.numeric (clean$ide2_2)
#clean$ide2_3 = as.numeric (clean$ide2_3)
#clean$ide2_4 = as.numeric (clean$ide2_4)
#clean$ide2_5 = as.numeric (clean$ide2_5)


clean$conservative_1 = clean$ide2_1
clean$conservative_2 = clean$ide2_2
clean$conservative_3 = 5- clean$ide2_3
clean$conservative_4 = clean$ide2_4
clean$conservative_5 = 5- clean$ide2_5

#step2: standardize 
clean$conservative_scaled_1 = scale (clean$conservative_1) [,1]
clean$conservative_scaled_2 = scale (clean$conservative_2) [,1]
clean$conservative_scaled_3 = scale (clean$conservative_3) [,1]
clean$conservative_scaled_4 = scale (clean$conservative_4) [,1]
clean$conservative_scaled_5 = scale (clean$conservative_5) [,1]

#step3: sum the normalized scores to create an index
clean = clean %>%
  rowwise() %>%
  mutate (conservative_scaled_score = sum(c_across(starts_with("conservative_scaled_")), na.rm=T))

#step4: standardize again
clean$conservative_index = scale (clean$conservative_scaled_score) [,1]

## ideology 3
#Antimarket/promarket
# H(4): more antimarket
# L(1): more promarket

#step1: change the H/L scale of the raw data
#clean$ide3_1 = as.numeric (clean$ide3_1)
#clean$ide3_2 = as.numeric (clean$ide3_2)
#clean$ide3_3 = as.numeric (clean$ide3_3)
#clean$ide3_4 = as.numeric (clean$ide3_4)
#clean$ide3_5 = as.numeric (clean$ide3_5)

clean$antimarket_1 = clean$ide3_1
clean$antimarket_2 = 5- clean$ide3_2
clean$antimarket_3 = clean$ide3_3
clean$antimarket_4 = clean$ide3_4
clean$antimarket_5 = 5- clean$ide3_5

#step2: standardize
clean$antimarket_scaled_1 = scale(clean$antimarket_1) [,1]
clean$antimarket_scaled_2 = scale(clean$antimarket_2) [,1]
clean$antimarket_scaled_3 = scale(clean$antimarket_3) [,1]
clean$antimarket_scaled_4 = scale(clean$antimarket_4) [,1]
clean$antimarket_scaled_5 = scale(clean$antimarket_5) [,1]

#step3: sum the normalized score to create an index
clean = clean %>%
  rowwise() %>%
  mutate (antimarket_scaled_score = sum(c_across(starts_with("antimarket_scaled_")), na.rm=T))

#step4: standardize again
clean$antimarket_index = scale(clean$antimarket_scaled_score) [,1]

##ideology index 4
#H(4): most patriotic
#L(1): least patriotic

#step1: change the H/L scale of the raw data
#clean$nat_1 = as.numeric (clean$nat_1)
#clean$nat_2 = as.numeric (clean$nat_2)
#clean$nat_3 = as.numeric (clean$nat_3)
#clean$nat_4 = as.numeric (clean$nat_4)
#clean$nat_5 = as.numeric (clean$nat_5)
#clean$nat_6 = as.numeric (clean$nat_6)

clean$patriotic_1 = clean$nat_1
clean$patriotic_2 = 5- clean$nat_2
clean$patriotic_3 = clean$nat_3
clean$patriotic_4 = 5- clean$nat_4
clean$patriotic_5 = clean$nat_5
clean$patriotic_6 = clean$nat_6

#step2: standardize
clean$patriotic_scaled_1 = scale (clean$patriotic_1) [,1]
clean$patriotic_scaled_2 = scale (clean$patriotic_2) [,1]
clean$patriotic_scaled_3 = scale (clean$patriotic_3) [,1]
clean$patriotic_scaled_4 = scale (clean$patriotic_4) [,1]
clean$patriotic_scaled_5 = scale (clean$patriotic_5) [,1]
clean$patriotic_scaled_6 = scale (clean$patriotic_6) [,1]

#step4: sum the normalized score to create an index
clean = clean %>%
  rowwise() %>%
  mutate(patriotic_scaled_score = sum (c_across(starts_with("patriotic_scaled_")), na.rm = T))

#step4: standardize 
clean$patriotic_index = scale(clean$patriotic_scaled_score) [,1]

#tech index
#yes =1
clean = clean %>%
  mutate (tech_dummy_1 = case_when (tech_1==1 ~ 1, TRUE ~0))

clean = clean %>%
  mutate (tech_dummy_2 = case_when (tech_2==1 ~ 1, TRUE ~0))

clean = clean %>%
  mutate (tech_dummy_3 = case_when (tech_3==1 ~ 1, TRUE ~0))

clean = clean %>%
  mutate (tech_dummy_4 = case_when (tech_4==1 ~ 1, TRUE ~0))

clean = clean %>%
  mutate (tech_dummy_5 = case_when (tech_5==1 ~ 1, TRUE ~0))

clean = clean %>%
  mutate (tech_dummy_6 = case_when (tech_6==1 ~ 1, TRUE ~0))

#sum all the "yes" answers
clean = clean %>%
  rowwise() %>%
  mutate(tech_dummy_score = sum(c_across(starts_with("tech_dummy_")), na.rm=T))

#standardize
clean$tech_dummy_index = scale (clean$tech_dummy_score) [,1]

## Oversea/globalization index
# sum all the "yes" answer
#clean$oversea_1 = as.numeric (clean$oversea_1)
#clean$oversea_2 = as.numeric (clean$oversea_2)
#clean$oversea_3 = as.numeric (clean$oversea_3)
#clean$oversea_4 = as.numeric (clean$oversea_4)
#clean$oversea_5 = as.numeric (clean$oversea_5)
#clean$oversea_6 = as.numeric (clean$oversea_6)

clean = clean %>%
  rowwise() %>%
  mutate(globalization_dummy_score=sum(c_across(starts_with("oversea_")),na.rm=T))

#standardize
clean$globalization_dummy_index = scale (clean$globalization_dummy_score) [,1]

### Opinion towards large tech company
# H(4): most favorable
# L(1): least favorable

#step1: change the H/L scale of the raw data
#clean$tcom_1 = as.numeric (clean$tcom_1)
#clean$tcom_2 = as.numeric (clean$tcom_2)
#clean$tcom_3 = as.numeric (clean$tcom_3)

clean$tcom_favorable_1 = clean$tcom_1
clean$tcom_favorable_2 = 5- clean$tcom_2
clean$tcom_favorable_3 = 5- clean$tcom_3

#step2: standardize
clean$tcom_favorable_scale_1 = scale(clean$tcom_favorable_1) [,1]
clean$tcom_favorable_scale_2 = scale(clean$tcom_favorable_2) [,1]
clean$tcom_favorable_scale_3 = scale(clean$tcom_favorable_3) [,1]

#step3: sum rows across the columns
clean = clean %>%
  rowwise() %>%
  mutate (tcom_favorable_score = sum(c_across(starts_with("tcom_favorable_scale_")), na.rm=T))

#step4: standardize
clean$tcom_favorable_index = scale (clean$tcom_favorable_score) [,1]

### data privacy perception
# H(4): most concern
# L(1): least concern

#step1: change the H/L scale in the raw data
clean$data_concern_1 = clean$data_view_1
clean$data_concern_2 = 5- clean$data_view_2
clean$data_concern_3 = 5- clean$data_view_3
clean$data_concern_4 = 5- clean$data_view_4
clean$data_concern_5 = 5- clean$data_view_5

#step2: standardize
clean$data_concern_scaled_1 = scale (clean$data_concern_1) [,1]
clean$data_concern_scaled_2 = scale (clean$data_concern_2) [,1]
clean$data_concern_scaled_3 = scale (clean$data_concern_3) [,1]
clean$data_concern_scaled_4 = scale (clean$data_concern_4) [,1]
clean$data_concern_scaled_5 = scale (clean$data_concern_5) [,1]

#step3: sum 
clean = clean %>%
  rowwise() %>%
  mutate(data_concern_score = sum(c_across(starts_with("data_concern_scaled_")),na.rm=T))

#step4: standardize
clean$data_concern_index = scale (clean$data_concern_score) [,1]

###Outcome variable
#outcome 1
clean$out1_scaled_1 = scale(clean$out1_1) [,1]
clean$out1_scaled_2 = scale(clean$out1_2) [,1]
clean$out1_scaled_3 = scale(clean$out1_3) [,1]
clean$out1_scaled_4 = scale(clean$out1_4) [,1]
clean$out1_scaled_5 = scale(clean$out1_5) [,1]
clean$out1_scaled_6 = scale(clean$out1_6) [,1]
clean$out1_scaled_7 = scale(clean$out1_7) [,1]

clean = clean %>%
  rowwise() %>%
  mutate (out1_scaled_score = sum(c_across(starts_with("out1_scaled_")), na.rm=T))

clean$out1_scaled_index = scale (clean$out1_scaled_score) [,1]

#outcome 2
clean$out2_scaled_1 = scale(clean$out2_1) [,1]
clean$out2_scaled_2 = scale(clean$out2_2) [,1]
clean$out2_scaled_3 = scale(clean$out2_3) [,1]


clean = clean %>%
  rowwise() %>%
  mutate (out2_scaled_score = sum(c_across(starts_with("out2_scaled_")), na.rm=T))

clean$out2_scaled_index = scale (clean$out2_scaled_score) [,1]

#outcome 3
clean$out3_scaled_1 = scale(clean$out3_1) [,1]
clean$out3_scaled_2 = scale(clean$out3_2) [,1]
clean$out3_scaled_3 = scale(clean$out3_3) [,1]
clean$out3_scaled_4 = scale(clean$out3_4) [,1]
clean$out3_scaled_5 = scale(clean$out3_5) [,1]
clean$out3_scaled_6 = scale(clean$out3_6) [,1]
clean$out3_scaled_7 = scale(clean$out3_7) [,1]

clean = clean %>%
  rowwise() %>%
  mutate (out3_scaled_score = sum(c_across(starts_with("out3_scaled_")), na.rm=T))

clean$out3_scaled_index = scale (clean$out3_scaled_score) [,1]

#outcome 4
clean$out4_scaled_1 = scale(clean$out4_1) [,1]
clean$out4_scaled_2 = scale(clean$out4_2) [,1]
clean$out4_scaled_3 = scale(clean$out4_3) [,1]


clean = clean %>%
  rowwise() %>%
  mutate (out4_scaled_score = sum(c_across(starts_with("out4_scaled_")), na.rm=T))

clean$out4_scaled_index = scale (clean$out4_scaled_score) [,1]

#outcome 5
clean$out5_scaled_1 = scale(clean$out5_1) [,1]
clean$out5_scaled_2 = scale(clean$out5_2) [,1]
clean$out5_scaled_3 = scale(clean$out5_3) [,1]
clean$out5_scaled_4 = scale(clean$out5_4) [,1]
clean$out5_scaled_5 = scale(clean$out5_5) [,1]
clean$out5_scaled_6 = scale(clean$out5_6) [,1]
clean$out5_scaled_7 = scale(clean$out5_7) [,1]

clean = clean %>%
  rowwise() %>%
  mutate (out5_scaled_score = sum(c_across(starts_with("out5_scaled_")), na.rm=T))

clean$out5_scaled_index = scale (clean$out5_scaled_score) [,1]

#outcome 6
clean$out6_scaled_1 = scale(clean$out6_1) [,1]
clean$out6_scaled_2 = scale(clean$out6_2) [,1]
clean$out6_scaled_3 = scale(clean$out6_3) [,1]


clean = clean %>%
  rowwise() %>%
  mutate (out6_scaled_score = sum(c_across(starts_with("out6_scaled_")), na.rm=T))

clean$out6_scaled_index = scale (clean$out6_scaled_score) [,1]

#outcome 7,8,9,10,11
clean$out7_scaled = scale (clean$out7) [,1]
clean$out8_scaled = scale (clean$out8) [,1]
clean$out9_scaled = scale (clean$out9) [,1]
clean$out10_scaled = scale (clean$out10) [,1]
clean$out11_scaled = scale (clean$out11) [,1]

#Categorize the geographical region based on the provincial code
eastern = c(1,2,3,6,9, 10,11,13,15,19,21,32,33,34)
western = c(5,20,22,23,24,25,26,27,28,29,30,31)
central = c(4,7,8,12,14,16,17,18)

clean$region = 
  as.factor(ifelse(clean$s4_province %in% eastern, "Eastern",
                   ifelse(clean$s4_province %in% western, "Western",
                          ifelse(clean$s4_province %in% central, "Central",
                                 NA))))
#Create the dummy variables for "region"
clean = dummy_cols(clean, select_columns = "region")
names(clean)= tolower(names(clean))

### PART 3: Personal Information Protection Law
#PIPL activated date: 2021-11-01

clean$newdate = anydate (clean$enddate)
cutoff.date = as.Date ("2021-11-01")
clean$ppil = case_when(clean$newdate >= cutoff.date ~ "pre", TRUE~"post")
clean = dummy_cols(clean, select_columns = "ppil")

### PART 4: Generation: before and after 1990
clean$gen_1990 = as.factor (ifelse(clean$s3_birth_year <1990, "before1990",
                        ifelse(clean$s3_birth_year >=1990, "after1990",
                               NA)))   
  
clean$gen_1980 = as.factor (ifelse(clean$s3_birth_year <1980, "before1980",
                                          ifelse(clean$s3_birth_year >=1980, "after1980",
                                                 NA))) 
clean = dummy_cols (clean, select_columns = "gen_1990")
clean = dummy_cols(clean,select_columns = "gen_1980")

### MERGE edu_college and edu_above_college to avoid warnings when running regression model
clean$edu_college_total = as.factor (ifelse (clean$edu_college==1,1,
                                             ifelse (clean$edu_above_college==1,1,
                                                     0)))

### BIG CITIES DUMMY
# Provinces
clean$province_name = as.factor (ifelse(clean$s4_province==1, "Beijing",
                                 ifelse(clean$s4_province==2, "Tianjin",
                                 ifelse(clean$s4_province==3, "Hebei",
                                 ifelse(clean$s4_province==4, "Shanxi",
                                 ifelse(clean$s4_province==5, "Inner Mongolia",
                                 ifelse(clean$s4_province==6, "Liaoning",
                                 ifelse(clean$s4_province==7, "Jilin",
                                 ifelse(clean$s4_province==8, "Heilongjiang",
                                 ifelse(clean$s4_province==9, "Shanghai",
                                 ifelse(clean$s4_province==10, "Jiangsu",
                                 ifelse(clean$s4_province==11, "Zhejiang",
                                 ifelse(clean$s4_province==12, "Anhui",
                                 ifelse(clean$s4_province==13, "Fujian",
                                 ifelse(clean$s4_province==14, "Jiangxi",
                                 ifelse(clean$s4_province==15, "Shandong",
                                 ifelse(clean$s4_province==16, "Henan",
                                 ifelse(clean$s4_province==17, "Hubei",
                                 ifelse(clean$s4_province==18, "Hunan",
                                 ifelse(clean$s4_province==19, "Guangdong",
                                 ifelse(clean$s4_province==20, "Guangxi Zhuang",
                                 ifelse(clean$s4_province==21, "Hainan",
                                 ifelse(clean$s4_province==22, "Chongqing",
                                 ifelse(clean$s4_province==23, "Sichuan",
                                 ifelse(clean$s4_province==24, "Guizhou",
                                 ifelse(clean$s4_province==25, "Yunnan",
                                 ifelse(clean$s4_province==26, "Tibet",
                                 ifelse(clean$s4_province==27, "Shaanxi",
                                 ifelse(clean$s4_province==28, "Gansu",
                                 ifelse(clean$s4_province==29, "Qinghai",
                                 ifelse(clean$s4_province==30, "Ningxia Hui",
                                 ifelse(clean$s4_province==31, "Xinjiang Uygur",
                                 ifelse(clean$s4_province==32, "Taiwan",
                                 ifelse(clean$s4_province==33, "Hong Kong",
                                 ifelse(clean$s4_province==34, "Macao",
                                       NA)))))))))))))))))))))))))))))))))))

#Dummy variables for big cities: Beijing, Shanghai, Guangdong
clean$city = as.factor (ifelse(clean$s4_province==1, "big",
                        ifelse(clean$s4_province==9, "big",
                        ifelse(clean$s4_province==19,"big",
                        "small"))))

clean=dummy_cols(clean, select_columns = "city")

  write.csv(clean, "gc1_clean_data_16jun.csv")
