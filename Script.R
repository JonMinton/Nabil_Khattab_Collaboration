
rm(list=ls())
setwd("X:/PROJECTS/Nabil Khattab Collaboration/")

require(foreign)

D <- read.spss(
  "LFS_UK_2013__shorter_list_of_variables_2.sav", 
  to.data.frame=T, use.value.labels=T)

# What variables are there?
# Which am I interested in?

order(names(D))
summary(D)

#want to establish ethnic penalty conditioning on

# age 25 to 55

# match on
# age
# befor
# 2013 - cameyr
# country
# dep_chld
# discurr
# easteu
# edage
# ftpt
# Generation
# gorwkr
# hdpch19 : number of children in houshold aged under 19
# health
# highqual
# ilodefr
# marchk
# marsta
# NS_SE4c
# numhhld
# publicr
# regwkr
# relig
# samelad
# sex
# sumhrs

# net99

require("MatchIt")

# First (simplest) comparison: 
# control if ETHEWEUL== English/Welsh/Scottish/Northern Irish/British
# treatment iF not

require("car")

treat_binary <- rep(NA, dim(D)[1])

treat_binary[!is.na(D$ETHEWEUL)] <- 0
treat_binary[D$ETHEWEUL=="English/Welsh/Scottish/Northern Irish/British"] <- 1

#remove rows from data where treat_binary is undefined
D2 <- D[!is.na(treat_binary),]
D2 <- subset(D2, 
             select=c(
               "age", "befor", "cameyr", "country", "dep_chld", 
               "discurr", "easteu", "edage", "ftpt", "Generation", 
               "gorwkr", "hdpch19", "health", "highqual", "ilodefr", 
               "marchk", "marsta", "NS_SE4C", "numhhld", "publicr", 
               "regwkr", "relig", "samelad", "sex", "sumhrs", "net99")
  )

# someone who's always been here has been here since they were born
# otherwise they've been here since they arrived here
yearshere <- ifelse (is.na(D2$cameyr), D2$age, 2013 - D$cameyr)

# need to fix edage so 96 and 97 are missing
D2$edage[D2$edage > 95] <- NA

require(Amelia)

# nominal : 
# befor
# country
# discurr
# easteu
# ftpt
# gorwkr
# healthprob
# ilodefr
# marchk
# marsta
# publicr
# regwkr
# relig
# sex

# 


fourclass <- recode(D2$NS_SE4C,
                    recodes="'Salariat'=3; 'Intermediate occupations'=2; 'Routine and manual occupations'=1; 'Never worked'=0",
                    as.factor.result=F
                      )

#making marchk binary
D2$marchk <- as.numeric(D2$marchk=="Yes")

D2$samelad <- as.numeric(D2$samelad=="Yes")


# ordinal:
# gen
# fourclass


gen <- recode(
  D2$Generation, 
  recodes="'1st generation' = 1; '1.5 generation' = 1.5; '2nd generation'= 2; 'British natives' = 0",
  as.factor.result=F           
  )

# converting health to binary 

healthprob <- rep(0, dim(D2)[1])
healthprob[!is.na(D2$health)] <- 1

# convert highqual to ordinalish

quallvl <- recode(D2$highqual, 
                  recodes="'No qualification'=0; 'Higher education'=1; 'GCSE grades A-C or equiv'=2; 'GCE A Level or equiv'=3; 'Degree or equivalent'=4; else=NA",
                  as.factor.result=F
                  )



# cardinal : 
# age
# yearshere
# dep_chld
# edage
# hdpch19
# numhhld
# sumhrs
# net99


D3 <- D2[,c("age","befor", "country", "dep_chld", 
  "discurr", "easteu", "edage", "ftpt",  
  "gorwkr", "hdpch19", "ilodefr", 
  "marchk", "marsta", "numhhld", "publicr", 
  "regwkr", "relig", "samelad", "sex", "sumhrs", "net99")
  ]
D3 <- cbind(D3, yearshere, fourclass, gen, healthprob, quallvl)


D4 <- D2[,c("age","befor", "dep_chld", 
            "discurr", "easteu", "edage", "ftpt",  
            "gorwkr", "hdpch19", "ilodefr", 
            "marchk", "marsta", "numhhld", "publicr", 
            "regwkr", "relig", "samelad", "sex", "sumhrs", "net99")
         ]
D4 <- cbind(D4, yearshere, fourclass, gen, healthprob, quallvl)

D5 <- D2[,c("age","befor", "dep_chld", 
            "discurr", "easteu", "edage", "ftpt",  
            "gorwkr", "hdpch19", "ilodefr", 
            "marchk", "marsta", "numhhld", "publicr", 
            "relig", "samelad", "sex", "sumhrs", "net99")
         ]
D5 <- cbind(D5, yearshere, fourclass, gen, healthprob, quallvl)

D5.imputed <- amelia(D5, 
                     noms=c("befor", "discurr", "easteu", "ftpt",
                            "gorwkr", "healthprob", "ilodefr", "marchk", "marsta", 
                            "publicr", "relig", "sex"))


m.out <- matchit(treat ~ age + gorwkr + ftpt + highqual + sumhrs, data=DataSS, method="exact")


# part 1: 
# use variable ETHEWEUL as grouping
# control group is category 'English/Welsh/Scottish/Northern Irish/ British'
# all other categories are treatments

# part 2: use ethnrel_18 as grouping
# control group is Christian White British
# other groups are treatments


# actgr
# age
# agearrive

# cameyr''

# Key to variables:
#http://www.ons.gov.uk/ons/guide-method/method-quality/specific/labour-market/labour-market-statistics/index.html

# SEX
# 1: male
# 2: female

# AGE
# value

# AGEDFE *** - use this for consistency?!??
# AGE before previous 31 August
# value

# AGES
# Age in five year groups

# MF5964
# Male age 16 to 64 or female age 16 to 59
#  # NOT in this dataset

# marsta : Marital Status
# (1) single, never married
# (2) married, living with spouse
# (3) married, separated from spouse
# (4) divorced
# (5) widowed
# (6) civil partnered
# (7) civil partnered but separated from spouse
# (8) formerly civil partnered
# (9) civil partner - widow

# irend2 : religious denomination
# (1) : catholic
# (2) : presbyterian
# (3) : church of ireland
# (4) : methodist
# (5) : other protestant
# (6) : other religion
# (7) : no denomination
# (8) : under 16 years
# (9) : unwilling to answer

# relbup: religion brought up in (NORTHERN IRELAND ONLY) - NOT IN THIS DATA
# (1) : catholic
# (2) : Presbyterian
# (3) : Church of ireland
# (4) : methodist
# (5) : other protestant
# (6) : other religion
# (7) : no denomination
# (8) : unwilling to answer

# nation: nationality
# MANY categories

# cryo_rec : country brought up in 
# MANY categories
# match on character value (removing whitespace, lowercase)
#   with nation

# cameyr: year came to UK
# numeric :: convert to factor

# ethn_rel: ethnoreligious identity - added by Nadil

# also ethnrel_18 : one more category

# uresmc: region
# factors: regions within UK

# health : 
# range of health conditions : NAs = no health problem reported?

# ilodefr: ILO definition of economic activity status
# (1) in employment
# (2) ILO unemployed
# (3) Inactive
# (4) under 16

# acthr: numbers of hours worked

# Generation: NOTE UPPERCASE FIRST CHARACTER
# (1) 1st generation
# (2) 1.5 generation
# (3) 2nd generation
# (4) British natives

# jb: has a job?
# (1) yes
# (2) no
# (3) waiting to take up new job


# everwk
# (1) yes
# (2) no



# use sumhrs 
# see p 133 of following:

# SUMHOURS SHOULD BE USED IN PREFERENCE TO TOTHRS

#file:///C:/Users/Jon%20Minton/Downloads/volume32011_tcm77-240876.pdf





require(ggplot2)


qplot()

# What do I want? 

# 'treated' if British Pakistani male, aged between 25 and 60
# 'untreated' if British White male

#sex==1

treat <- rep(NA, dim(Data)[1])

require(car)

attach(Data)

#treated: 
#ethn_rel == 10

#untreated
#etn_rel == 16 | 14
# 16: christian white british
# 14: no religion white british


treat[sex==1 & age >=25 & age <= 60 & ethn_rel==10]  <- 1 # common to both treated and untreated

treat[sex==1 & age >=25 & age <= 60 & (ethn_rel==16 | ethn_rel==14)] <- 0

detach(Data)

Data <- data.frame(Data, treat=treat)
DataSS <- Data[!is.na(treat),]
DataSS <- subset(DataSS, select=c(treat, age, gorwkr, ftpt, highqual, sumhrs, NS_SE4C))
is_salariat <- DataSS$NS_SE4C==1
is_salariat[DataSS$NS_SE4C < 0] <- NA
DataSS <- data.frame(DataSS, is_salariat=is_salariat)




require("Zelig")


m.data <- match.data(m.out)

z.out <- zelig(is_salariat ~ treat + age + as.factor(ftpt) + as.factor(highqual) + sumhrs, 
                model="ls", data=m.data)
                
x.out <- setx(z.out, treat=0)
x1.out <- setx(z.out, treat=1)

s.out <- sim(z.out, x=x.out, x1=x1.out)

# outcome: Prob(NS_SE4C==1 ) 
#  salariat occupation

# matching on:
# age: age
# gorwkr: region of place of work
# ftpt :whether fulltime or part time
# highqual: highest educational qualification
# sumhrs: total number of hours in main and second job (-9, -8 == missing)




