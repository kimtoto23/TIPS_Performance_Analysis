############################## -*- Mode: Ess-R -*- ############################
## 
## Filename: estimate.r
## 
## Title     :
## 
## Created   : <2020-10-30 (금) 15:17:18 by Hyo-Jun Kim>
## 
## URL: 
## Keywords: 
## 
######################################################################
## 
### Commentary: 
## 
## 
######################################################################
## 
### Change Log:
## 
## 
######################################################################
## 
### Code:


########################################################################
########################################################################
### A. two input, one output model
########################################################################
########################################################################

lab.dat <- read.csv("C:/Users/INHA/Desktop/Archive/data/lab_company_info.txt", sep = "\t")
tips.dat <- read.csv("C:/Users/INHA/Desktop/Archive/data/tips_company_info.txt", sep = "\t")

lab.control.dat <-
    read.csv("C:/Users/INHA/Desktop/Archive/data/lab_matching_company_info.txt", sep = "\t")
tips.control.dat <-
    read.csv("C:/Users/INHA/Desktop/Archive/data/tips_matching_company_info.txt", sep = "\t")

lab.matching.table <-
    read.csv("C:/Users/INHA/Desktop/Archive/data/lab_matching_table.txt", sep = "\t")
tips.matching.table <-
    read.csv("C:/Users/INHA/Desktop/Archive/data/tips_matching_table.txt", sep = "\t")


lab <- lab.dat[, c("사업자등록번호", "설립일자", "매출액.2019.",
                   "종업원수.2019.", "산업분류코드.10차.")]
tips <- tips.dat[, c("사업자번호", "설립연월", "매출액.2019.",
                     "종업원수.2019.", "산업분류코드.10차.")]

lab.control <- lab.control.dat[, c("매핑된사업자번호", "설립일자",
                                   "매출액.2019.", "종업원수.2019.",
                                   "산업분류코드.10차.")]
tips.control <- tips.control.dat[, c("매핑된사업자번호", "설립일자",
                                     "매출액.2019.", "종업원수.2019.",
                                     "산업분류코드.10차.")]

names(lab) <- c("bzid", "foundation.date", "sales", "employee", "sic")
names(tips) <- c("bzid", "foundation.date", "sales", "employee", "sic" )
names(lab.control) <- c("bzid", "foundation.date", "sales", "employee", "sic")
names(tips.control) <- c("bzid", "foundation.date", "sales", "employee", "sic")
names(lab.matching.table) <- c("bzid.in.lab", "bzid.in.lab.control")
names(tips.matching.table) <- c("bzid.in.tips", "bzid.in.tips.control")
lab$bzid <- gsub("-", "", lab$bzid)
tips$bzid <- gsub("-", "", tips$bzid)

lab <- na.omit(lab)
tips <- na.omit(tips)
lab.control <- na.omit(lab.control)
tips.control <- na.omit(tips.control)

lab$birth.year <- as.numeric(substr(lab$foundation.date, 1, 4))
tips$birth.year <- as.numeric(substr(tips$foundation.date, 1, 4))
lab$foundation.date <- NULL
tips$foundation.date <- NULL
lab.control$birth.year <-
    as.numeric(substr(lab.control$foundation.date, 1, 4))
tips.control$birth.year <-
    as.numeric(substr(tips.control$foundation.date, 1, 4))
lab.control$foundation.date <- NULL
tips.control$foundation.date <- NULL

lab$age <- 2019 - lab$birth.year
tips$age <- 2019 - tips$birth.year
lab.control$age <- 2019 - lab.control$birth.year
tips.control$age <- 2019 - tips.control$birth.year

lab.control <- unique(lab.control)
tips.control <- unique(tips.control)

## 연구소 기업과 살아남은 대조군 기업의 쌍이 많이 깨졌다.
## 그럼에도 불구하고, 두 군 간의 효율성 차이를 살펴본다는 점에서,
## 그냥 비교한다.
## bzid.lab <- unique(lab$bzid)
## matching.id.lab <-
##     with(lab.matching.table, bzid.in.lab.control[bzid.in.lab %in%
## bzid.lab])

lab$type <- "lab"
tips$type <- "tips"
lab.control$type <- "lab.control"
tips.control$type <- "tips.control"

lab <- lab[lab$sales > 0 & lab$employee > 0 & lab$age > 0, ]
tips <- tips[tips$sales > 0 & tips$employee > 0 & tips$age > 0, ]
lab.control <- lab.control[lab.control$sales > 0 & lab.control$employee
                           > 0 & lab.control$age > 0, ]
tips.control <- tips.control[tips.control$sales > 0 & tips.control$employee
                           > 0 & tips.control$age > 0, ]

lab.control <- unique(lab.control)
tips.control <- unique(tips.control)

my.dat3 <- as.data.frame(rbind(lab, lab.control, tips, tips.control))

## 연구소 기업과 살아남은 대조군 기업의 쌍이 많이 깨졌다.
## 그럼에도 불구하고, 두 군 간의 효율성 차이를 살펴본다는 점에서,
## 그냥 비교한다.
## bzid.lab <- unique(lab$bzid)
## matching.id.lab <-
##     with(lab.matching.table, bzid.in.lab.control[bzid.in.lab %in%
## bzid.lab])

lab$type <- "lab"
tips$type <- "tips"
lab.control$type <- "lab.control"
tips.control$type <- "tips.control"

lab <- lab[lab$sales > 0 & lab$employee > 0 & lab$age > 0, ]
tips <- tips[tips$sales > 0 & tips$employee > 0 & tips$age > 0, ]
lab.control <- lab.control[lab.control$sales > 0 & lab.control$employee
                           > 0 & lab.control$age > 0, ]
tips.control <- tips.control[tips.control$sales > 0 & tips.control$employee
                           > 0 & tips.control$age > 0, ]

lab.control <- unique(lab.control)
tips.control <- unique(tips.control)

my.dat3 <- as.data.frame(rbind(lab, lab.control, tips, tips.control))

########################################################################
########################################################################
### B. one input, one output model
########################################################################
########################################################################
lab.dat <- read.csv("C:/Users/INHA/Desktop/Archive/data/lab_company_info.txt", sep = "\t")
tips.dat <- read.csv("C:/Users/INHA/Desktop/Archive/data/tips_company_info.txt", sep = "\t")

lab.control.dat <-
    read.csv("C:/Users/INHA/Desktop/Archive/data/lab_matching_company_info.txt", sep = "\t")
tips.control.dat <-
    read.csv("C:/Users/INHA/Desktop/Archive/data/tips_matching_company_info.txt", sep = "\t")

lab.matching.table <-
    read.csv("C:/Users/INHA/Desktop/Archive/data/lab_matching_table.txt", sep = "\t")
tips.matching.table <-
    read.csv("C:/Users/INHA/Desktop/Archive/data/tips_matching_table.txt", sep = "\t")


lab <- lab.dat[, c("사업자등록번호", "설립일자", "매출액.2019.",
                   "산업분류코드.10차.")]
tips <- tips.dat[, c("사업자번호", "설립연월", "매출액.2019.",
                     "산업분류코드.10차.")]

lab.control <- lab.control.dat[, c("매핑된사업자번호", "설립일자",
                                   "매출액.2019.", 
                                   "산업분류코드.10차.")]
tips.control <- tips.control.dat[, c("매핑된사업자번호", "설립일자",
                                     "매출액.2019.", 
                                     "산업분류코드.10차.")]

names(lab) <- c("bzid", "foundation.date", "sales", "sic")
names(tips) <- c("bzid", "foundation.date", "sales", "sic")
names(lab.control) <- c("bzid", "foundation.date", "sales", "sic")
names(tips.control) <- c("bzid", "foundation.date", "sales", "sic")
names(lab.matching.table) <- c("bzid.in.lab", "bzid.in.lab.control")
names(tips.matching.table) <- c("bzid.in.tips", "bzid.in.tips.control")
lab$bzid <- gsub("-", "", lab$bzid)
tips$bzid <- gsub("-", "", tips$bzid)

lab <- na.omit(lab)
tips <- na.omit(tips)
lab.control <- na.omit(lab.control)
tips.control <- na.omit(tips.control)

lab$birth.year <- as.numeric(substr(lab$foundation.date, 1, 4))
tips$birth.year <- as.numeric(substr(tips$foundation.date, 1, 4))
lab$foundation.date <- NULL
tips$foundation.date <- NULL
lab.control$birth.year <-
    as.numeric(substr(lab.control$foundation.date, 1, 4))
tips.control$birth.year <-
    as.numeric(substr(tips.control$foundation.date, 1, 4))
lab.control$foundation.date <- NULL
tips.control$foundation.date <- NULL

lab$age <- 2019 - lab$birth.year
tips$age <- 2019 - tips$birth.year
lab.control$age <- 2019 - lab.control$birth.year
tips.control$age <- 2019 - tips.control$birth.year

lab.control <- unique(lab.control)
tips.control <- unique(tips.control)

## 연구소 기업과 살아남은 대조군 기업의 쌍이 많이 깨졌다.
## 그럼에도 불구하고, 두 군 간의 효율성 차이를 살펴본다는 점에서,
## 그냥 비교한다.
## bzid.lab <- unique(lab$bzid)
## matching.id.lab <-
##     with(lab.matching.table, bzid.in.lab.control[bzid.in.lab %in%
## bzid.lab])

lab$type <- "lab"
tips$type <- "tips"
lab.control$type <- "lab.control"
tips.control$type <- "tips.control"

lab <- lab[lab$sales > 0 & lab$age > 0, ]
tips <- tips[tips$sales > 0 & tips$age > 0, ]
lab.control <- lab.control[lab.control$sales > 0 & lab.control$age > 0, ]
tips.control <- tips.control[tips.control$sales > 0 & tips.control$age > 0, ]

lab.control <- unique(lab.control)
tips.control <- unique(tips.control)

my.dat2 <- as.data.frame(rbind(lab, tips, lab.control, tips.control))


######################################################################
## Preparation for SFA
my.dat3$y <- log(my.dat3$sales)
my.dat3$g <- log(my.dat3$age)
my.dat3$l <- log(my.dat3$employee)
my.dat3$gg <- 1/2*(my.dat3$g)^2
my.dat3$ll <- 1/2*(my.dat3$l)^2
my.dat3$gl <- my.dat3$g * my.dat3$l

my.dat2$y <- log(my.dat2$sales)
my.dat2$g <- log(my.dat2$age)


#####################################################################
## Industry classification
my.dat3$sic1 <- substr(my.dat3$sic, 1, 1)
my.dat2$sic1 <- substr(my.dat2$sic, 1, 1)


######################################################################
### estimate.r ends here

