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

library(nonparaeff)
library(frontier)
library(poweRlaw)

rm(list = ls())

source("data_management.r")
source("function.r")

system("mkdir -p img")
system("mkdir -p tables")

desc <- list()
re <- list()
fit <- list()
model3 <- list()
model2 <- list()

########################################################################
########################################################################
### A. Estimation of Efficiency
########################################################################
########################################################################

########################################################################
### A.10. Simple SFA
########################################################################
## use my.dat3
model3$cd3 <- y ~ l + g + factor(sic1)
model3$tr3 <- y ~ l + g + ll + gg + gl + factor(sic1)
model3$cd3d <- y ~ l + g + factor(type) + factor(sic1)
model3$tr3d <- y ~ l + g + ll + gg + gl + factor(type) + factor(sic1)

fit3 <- lapply(model3, function(x) sfa(x, data = my.dat3))
eff3 <- lapply(fit3, efficiencies)

names(fit3) <- c("CD", "TR", "CD-d", "TR-d")
names(eff3) <- c("CD", "TR", "CD-d", "TR-d")

tapply(eff3[[1]], my.dat3$type, mean)
tapply(eff3[[2]], my.dat3$type, mean)
tapply(eff3[[3]], my.dat3$type, mean)
tapply(eff3[[4]], my.dat3$type, mean)

## use my.dat2
model2$cd2 <- y ~ g + factor(sic1)
model2$cd2d <- y ~ g + factor(type) + factor(sic1)

fit2 <- lapply(model2, function(x) sfa(x, data = my.dat2))
eff2 <- lapply(fit2, efficiencies)

names(fit2) <- c("CD", "CD-d")
names(eff2) <- c("CD", "CD-d")

tapply(eff2[[1]], my.dat2$type, mean)
tapply(eff2[[2]], my.dat2$type, mean)

########################################################################
### A.20. Metafrontier SFA (Battese et al., 2004)
########################################################################
## meta sfa fit: two var C-D
set.seed(1234)
meta.fit.cd <- meta.sfa.static(model = update(model2$cd2, . ~ . - factor(sic1)),
                            data = my.dat3, group = "type", B = 2000)
meta.fit.cd[[2]]$type <- my.dat3$type

with(meta.fit.cd[[2]], tapply(within.eff, type, mean))
with(meta.fit.cd[[2]], tapply(tgr, type, mean))
with(meta.fit.cd[[2]], tapply(meta.eff, type, mean))

## meta sfa fit: two var transllog
set.seed(1234)
meta.fit.tr <- meta.sfa.static(model = update(model3$tr3, . ~ . - factor(sic1)), 
                            data = my.dat3, group = "type", B = 2000)
meta.fit.tr[[2]]$type <- my.dat3$type

with(meta.fit.tr[[2]], tapply(within.eff, type, mean))
with(meta.fit.tr[[2]], tapply(tgr, type, mean))
with(meta.fit.tr[[2]], tapply(meta.eff, type, mean))

########################################################################
########################################################################
### B. Descriptive Statistics of Variables
########################################################################
########################################################################

attach(my.dat3)
desc$five.var.total <-
    t(data.frame(sales = five.var(sales)/1e6,
                             age = five.var(age),
                             employee = five.var(employee)))

desc$five.var.by.type <-
    t(data.frame(sales.lab = five.var(sales[type == "lab"])/1e6,
                 sales.lab.control = five.var(sales[type ==
                                                    "lab.control"])/1e6,
                 sales.tips = five.var(sales[type == "tips"])/1e6,
                 sales.tips.control = five.var(sales[type ==
                                                     "tips.control"])/1e6
                 ,
                 employee.lab = five.var(employee[type == "lab"]),
                 employee.lab.control = five.var(employee[type ==
                                                    "lab.control"]),
                 employee.tips = five.var(employee[type == "tips"]),
                 employee.tips.control = five.var(employee[type ==
                                                           "tips.control"])
                 ,
                 age.lab = five.var(age[type == "lab"]),
                 age.lab.control = five.var(age[type ==
                                                    "lab.control"]),
                 age.tips = five.var(age[type == "tips"]),
                 age.tips.control = five.var(age[type ==
                                                     "tips.control"])))

png("./img/total_sample_input_output_log_plot.png", width = 960, height = 500, res = 130)
par(mfrow = c(1, 3))
x <- round(sales)/1e6
m.pl <- conpl$new(x)
est <- estimate_xmin(m.pl)
m.pl <- conlnorm$new(x)
m.pl$setXmin(est)
plot(m.pl, xlab = "", ylab = "", yaxt = "n", main = "")
title(main = "(a) 매출액")
title(xlab = "매출액")
title(ylab = "CDF")
axis(2, las = 2)
x <- round(employee)
m.pl <- conlnorm$new(x)
est <- estimate_xmin(m.pl)
m.pl$setXmin(est)
plot(m.pl, xlab = "", ylab = "", yaxt = "n", main = "")
title(main = "(b) 고용인원")
title(xlab = "고용인원")
title(ylab = "CDF")
axis(2, las = 2)
x <- round(age)
m.pl <- conlnorm$new(x)
est <- estimate_xmin(m.pl)
m.pl$setXmin(est)
plot(m.pl, xlab = "", ylab = "", yaxt = "n", main = "")
title(main = "(c) 업력")
title(xlab = "업력")
title(ylab = "CDF")
axis(2, las = 2)
dev.off()

detach(my.dat3)
########################################################################
########################################################################
### C. Efficiency measurement result
########################################################################
########################################################################

########################################################################
### C.10. Model estimation result
########################################################################

tmp <- lapply(fit3, function(x) as.data.frame(report.sfa(x)))
kk <- merge(tmp[[1]], tmp[[2]], by.x = "row.names", by.y = "row.names",
            all.x = T, all.y = T)
names(kk) <- c("Row.names", "CD", "TR")
kk <- merge(kk, tmp[[3]], by.x = "Row.names", by.y = "row.names",
            all.x = T, all.y = T)
names(kk) <- c("Row.names", "CD", "TR", "CD-d")
kk <- merge(kk, tmp[[4]], by.x = "Row.names", by.y = "row.names",
            all.x = T, all.y = T)
names(kk) <- c("Row.names", "CD", "TR", "CD-d", "TR-d")
row.names(kk) <- kk$Row.names
kk$Row.names <- NULL
kk <- rbind(kk, logLik = sapply(fit3, function(x) round(logLik(x), 2)))
re$estimation.result.3.var <- kk
rm(tmp, kk)

tmp <- lapply(fit2, function(x) as.data.frame(report.sfa(x)))
kk <- merge(tmp[[1]], tmp[[2]], by.x = "row.names", by.y = "row.names",
            all.x = T, all.y = T)
names(kk) <- c("Row.names", "CD", "CD-d")
row.names(kk) <- kk$Row.names
kk$Row.names <- NULL
kk <- rbind(kk, logLik = sapply(fit2, function(x) round(logLik(x), 2)))
re$estimation.result.2.var <- kk
rm(tmp, kk)

tmp <- matrix(NA, nrow = 3, ncol = 3)
row.names(tmp) <- c("CobbDouglas", "CobbDouglas with dummy", "Trnaslog")
colnames(tmp) <- c("CobbDouglas with dummy", "Trnaslog",
                   "Translog with dummy")
tmp[1, 1] <- lr.test(fit3, 1, 2, degree = 3)
tmp[1, 2] <- lr.test(fit3, 1, 3, degree = 3)
tmp[1, 3] <- lr.test(fit3, 1, 4, degree = 6)
tmp[2, 3] <- lr.test(fit3, 2, 4, degree = 3)
tmp[3, 3] <- lr.test(fit3, 3, 4, degree = 3)
re$lr.test.result3 <- tmp
comment(re$lr.test.result3) <-
    "The J test result signifies that we cannot determine which one is better between CD-d and TR."
rm(tmp)

re$lr.test.result2 <- lr.test(fit2, 1, 2, degree = 3)

## add efficiencies for selected two models
my.dat3$eff <- eff3[[4]]
my.dat2$eff <- eff2[[2]]

## Note: 'epdf' denotes the empirical probability density function.
## epdf of efficiency: 3 variables 
png("./img/efficiency_3_density_all_sample.png", width = 960,
    height = 500, res = 130)
plot(density(my.dat3$eff), lwd = 2, col = "red", xlab = "효율성",
     main = "효율성 분포(샘플, n = 991)")
abline(v = mean(my.dat3$eff), lwd = 4, col = "gray70")
text(0.6, 0.5, paste("평균 = ", round(mean(my.dat3$eff), 2), sep = ""))
arrows(0.5, 0.5, mean(my.dat3$eff), 0.5, length = 0.1, )
dev.off()

## epdf of efficiency: 2 variables 
png("./img/efficiency_2_density_all_sample.png", width = 960,
    height = 500, res = 130)
plot(density(my.dat2$eff), lwd = 2, col = "red", xlab = "효율성",
     main = "효율성 분포(샘플, n = 4,780)")
abline(v = mean(my.dat2$eff), lwd = 4, col = "gray70")
text(0.6, 0.5, paste("평균 = ", round(mean(my.dat2$eff, na.rm = T), 2), sep = ""))
arrows(0.5, 0.5, mean(my.dat2$eff), 0.5, length = 0.1, )
dev.off()

## Efficiency epdf by type: 3 variables
png("./img/efficiency_3_density_by_type.png", width = 960,
    height = 500, res = 130)
par(mfrow = c(1, 4), cex.main = 0.9)
x <- my.dat3$eff[my.dat3$type == "lab"]
plot(density(x), lwd = 2, col = "red", xlab = "효율성",
     main = "(a) 연구소 기업(n = 38)")
abline(v = mean(x), lwd = 4, col = "gray70")
text(0.6, 0.5, paste("평균 = ", round(mean(x), 2), sep = ""))
x <- my.dat3$eff[my.dat3$type == "lab.control"]
plot(density(x), lwd = 2, col = "red", xlab = "효율성",
     main = "(b) 연구소 기업 대조군(n = 371)")
abline(v = mean(x), lwd = 4, col = "gray70")
text(0.6, 0.5, paste("평균 = ", round(mean(x), 2), sep = ""))
x <- my.dat3$eff[my.dat3$type == "tips"]
plot(density(x), lwd = 2, col = "red", xlab = "효율성",
     main = "(c) TIPS기업(n = 53)")
abline(v = mean(x), lwd = 4, col = "gray70")
text(0.6, 0.5, paste("평균 = ", round(mean(x), 2), sep = ""))
x <- my.dat3$eff[my.dat3$type == "tips.control"]
plot(density(x), lwd = 2, col = "red", xlab = "효율성",
     main = "(d) TIPS기업 대조군(n = 529)")
abline(v = mean(x), lwd = 4, col = "gray70")
text(0.6, 0.5, paste("평균 = ", round(mean(x), 2), sep = ""))
dev.off()

## Efficiency epdf by type: 2 variables
png("./img/efficiency_2_density_by_type.png", width = 960,
    height = 500, res = 130)
par(mfrow = c(1, 4), cex.main = 0.9)
x <- my.dat2$eff[my.dat2$type == "lab"]
plot(density(x), lwd = 2, col = "red", xlab = "효율성",
     main = "(a) 연구소 기업(n = 273)")
abline(v = mean(x), lwd = 4, col = "gray70")
text(0.6, 0.5, paste("평균 = ", round(mean(x), 2), sep = ""))
x <- my.dat2$eff[my.dat2$type == "lab.control"]
plot(density(x), lwd = 2, col = "red", xlab = "효율성",
     main = "(b) 연구소 기업 대조군(n = 1,901)")
abline(v = mean(x), lwd = 4, col = "gray70")
text(0.6, 0.5, paste("평균 = ", round(mean(x), 2), sep = ""))
x <- my.dat2$eff[my.dat2$type == "tips"]
plot(density(x), lwd = 2, col = "red", xlab = "효율성",
     main = "(c) TIPS기업(n = 265)")
abline(v = mean(x), lwd = 4, col = "gray70")
text(0.6, 0.5, paste("평균 = ", round(mean(x), 2), sep = ""))
x <- my.dat2$eff[my.dat2$type == "tips.control"]
plot(density(x), lwd = 2, col = "red", xlab = "효율성",
     main = "(d) TIPS기업 대조군(n = 2,341)")
abline(v = mean(x), lwd = 4, col = "gray70")
text(0.6, 0.5, paste("평균 = ", round(mean(x), 2), sep = ""))
dev.off()

## t-test across types: 3 variables
lab.eff <- my.dat3$eff[my.dat3$type == "lab"]
lab.control.eff <- my.dat3$eff[my.dat3$type == "lab.control"]
tips.eff <- my.dat3$eff[my.dat3$type == "tips"]
tips.control.eff <- my.dat3$eff[my.dat3$type == "tips.control"]
tmp <- matrix(NA, nrow = 3, ncol = 3)
row.names(tmp) <- c("Lab", "Lab.control", "TIPS")
colnames(tmp) <- c("Lab.control", "TIPS", "TIPS.control")
tmp[1, 1] <- t.test.report(lab.eff, lab.control.eff)
tmp[1, 2] <- t.test.report(lab.eff, tips.eff)
tmp[1, 3] <- t.test.report(lab.eff, tips.control.eff)
tmp[2, 2] <- t.test.report(lab.control.eff, tips.eff)
tmp[2, 3] <- t.test.report(lab.control.eff, tips.control.eff)
tmp[3, 3] <- t.test.report(tips.eff, tips.control.eff)
re$eff.3.t.test.lab.lab.control.tips.tips.control <- tmp
rm(tmp)

lab.eff <- my.dat2$eff[my.dat2$type == "lab"]
lab.control.eff <- my.dat2$eff[my.dat2$type == "lab.control"]
tips.eff <- my.dat2$eff[my.dat2$type == "tips"]
tips.control.eff <- my.dat2$eff[my.dat2$type == "tips.control"]
tmp <- matrix(NA, nrow = 3, ncol = 3)
row.names(tmp) <- c("Lab", "Lab.control", "TIPS")
colnames(tmp) <- c("Lab.control", "TIPS", "TIPS.control")
tmp[1, 1] <- t.test.report(lab.eff, lab.control.eff)
tmp[1, 2] <- t.test.report(lab.eff, tips.eff)
tmp[1, 3] <- t.test.report(lab.eff, tips.control.eff)
tmp[2, 2] <- t.test.report(lab.control.eff, tips.eff)
tmp[2, 3] <- t.test.report(lab.control.eff, tips.control.eff)
tmp[3, 3] <- t.test.report(tips.eff, tips.control.eff)
re$eff.2.t.test.lab.lab.control.tips.tips.control <- tmp
rm(tmp)

## t-test of lab and lab.control by age: 3 variables
age.crit <- 5
lab.young <- my.dat3$eff[my.dat3$type == "lab" & my.dat3$age <
                         age.crit]
lab.old <-  my.dat3$eff[my.dat3$type == "lab" & my.dat3$age >=
                        age.crit]
lab.control.young <- my.dat3$eff[my.dat3$type == "lab.control"
                                 & my.dat3$age < age.crit]
lab.control.old <-  my.dat3$eff[my.dat3$type == "lab.control"
                                & my.dat3$age >= age.crit]
tmp <- matrix(NA, nrow = 3, ncol = 3)
row.names(tmp) <- c("Lab(Young)", "Lab(Old)", "Lab Control(Young)")
colnames(tmp) <- c("Lab(Old)", "Lab Control(Young)", "Lab Control(Old)")
tmp[1, 1] <- t.test.report(lab.young, lab.old)
tmp[1, 2] <- t.test.report(lab.young, lab.control.young)
tmp[1, 3] <- t.test.report(lab.young, lab.control.old)
tmp[2, 2] <- t.test.report(lab.old, lab.control.young)
tmp[2, 3] <- t.test.report(lab.old, lab.control.old)
tmp[3, 3] <- t.test.report(lab.control.young, lab.control.old)
re$eff.3.t.test.lab.age.category <- tmp
rm(tmp)

## t-test of lab and lab.control by age: 3 variables
age.crit <- 5
lab.young <- my.dat2$eff[my.dat2$type == "lab" & my.dat2$age <
                         age.crit]
lab.old <-  my.dat2$eff[my.dat2$type == "lab" & my.dat2$age >=
                        age.crit]
lab.control.young <- my.dat2$eff[my.dat2$type == "lab.control"
                                 & my.dat2$age < age.crit]
lab.control.old <-  my.dat2$eff[my.dat2$type == "lab.control"
                                & my.dat2$age >= age.crit]
tmp <- matrix(NA, nrow = 3, ncol = 3)
row.names(tmp) <- c("Lab(Young)", "Lab(Old)", "Lab Control(Young)")
colnames(tmp) <- c("Lab(Old)", "Lab Control(Young)", "Lab Control(Old)")
tmp[1, 1] <- t.test.report(lab.young, lab.old)
tmp[1, 2] <- t.test.report(lab.young, lab.control.young)
tmp[1, 3] <- t.test.report(lab.young, lab.control.old)
tmp[2, 2] <- t.test.report(lab.old, lab.control.young)
tmp[2, 3] <- t.test.report(lab.old, lab.control.old)
tmp[3, 3] <- t.test.report(lab.control.young, lab.control.old)
re$eff.2.t.test.lab.age.category <- tmp
rm(tmp)


########################################################################
########################################################################
### D. Export results to files
########################################################################
########################################################################
for(i in 1:length(desc)){
    file.name <-
        paste("./tables/DESC_", i, "_", gsub("\\.", "_", names(desc)[i]),
              ".txt", sep = "")
    write.table(desc[[i]], file.name, sep = "\t")
}

for(i in 1:length(re)){
    file.name <-
        paste("./tables/RE_", i, "_", gsub("\\.", "_", names(re)[i]),
              ".txt", sep = "")
    write.table(re[[i]], file.name, sep = "\t")
}



## 
######################################################################
### estimate.r ends here
