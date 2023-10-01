############################## -*- Mode: Ess-R -*- ############################
## 
## Filename: function.r
## 
## Title     :
## 
## Created   : <2020-10-31 (토) 20:48:51 by Hyo-Jun Kim>
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


meta.sfa.static <- function(model = NULL, data = NULL, group = NULL, B =
                            2000, ...){
    library(frontier)

    meta.dat <- data[order(data[[group]]), ]
    
    if(is.null(model) & is.null(meta.dat) & is.null(group)){
        cat("model, meta.dat and group must be specified. Exiting")
        re <- NULL
    }

    ng <- length(unique(meta.dat[[group]]))

    meta.dat$idx <- 1:nrow(meta.dat)

    within.meta.dat <- list()
    within.eff <- list()
    my.fit <- list()
    unq.group <- unique(meta.dat[[group]])
    
    for(i in 1:ng){
        ith.group <- unq.group[i]
        within.meta.dat[[i]] <- meta.dat[which(meta.dat[[group]] ==
                                          unq.group[i]), ]
        my.fit[[i]] <-
            sfa(model, data = within.meta.dat[[i]])
        within.eff[[i]] <- as.vector(efficiencies(my.fit[[i]]))
    }

    ## estimate metafrontier beta using LP. See Battese et al., (2004,
    ## JPA). I used eq (13) and (14) for estimating betas, and used
    ## bootstrapping technique of standard errors of betas.
    
    within.x.beta <- lapply(my.fit, function(x) x$fitted)
    within.x.beta <- unlist(within.x.beta)

    tmp <- meta.est.beta.star(model = model, data = meta.dat, B = B,
                              within.x.beta = within.x.beta)
    
    my.fit[[ng + 1]] <- tmp
    names(my.fit) <- c(paste("group = ", unq.group, sep = ""),
                       "meta.fit")

    meta.dat$within.eff <- unlist(within.eff)
    fitted.meta <-
        as.matrix(cbind(1, meta.dat[, all.vars(model)[-1]])) %*%
        matrix(tmp[, 1], ncol = 1)
    meta.dat$tgr <- exp(within.x.beta)/exp(fitted.meta)
    meta.dat$meta.eff <- with(meta.dat, within.eff * tgr)

    meta.dat <- meta.dat[order(meta.dat$idx), ]
    re <- list(my.fit, data = meta.dat)

    return(re)
  }
    
        
meta.est.beta.star <- function(model = NULL,  data = NULL, B = 2000,
                               within.x.beta = NULL){

    lp.dat <- cbind(1, data[, all.vars(model)[-1]])

    boot.re <- list()
    for(i in 1:B){
        sampled.obs <- sample(1:nrow(lp.dat), nrow(lp.dat), replace = T)
        boot.dat <- lp.dat[sampled.obs, ]
        
        obj <- apply(boot.dat, 2, sum)
        const.lhs <- boot.dat
        const.dir <- rep(">=", nrow(boot.dat))
        const.rhs <- within.x.beta[sampled.obs]
        
        try(boot.re[[i]] <-
            lp2(direction = "min", objective.in = obj,
                const.mat = const.lhs, const.dir = const.dir,
                const.rhs = const.rhs,
                free.var = 1:ncol(boot.dat))$solution.free)
    }
    my.beta <- apply(do.call(rbind, boot.re), 2, mean)
    my.se <- apply(do.call(rbind, boot.re), 2, sd)
    re <- data.frame(beta = my.beta, se = my.se)
    return(re)
}

five.var <- function(x){
    mean.x <- mean(x, na.rm = T)
    sd.x <- sd(x, na.rm = T)
    median.x <- median(x, na.rm = T)
    max.x  <- max(x, na.rm = T)
    min.x <- min(x, na.rm = T)

    re <- c(mean = mean.x, sd = sd.x, median = median.x,
            max = max.x, min = min.x)

    return(re)
}

report.p.val <- function(x){
    y <- x
    y[x < 0.01] <- "a"
    y[x >= 0.01 & x < 0.05] <- "b"
    y[x >= 0.05 & x < 0.10] <- "c"
    y[x >= 0.10] <- ""
    return(y)
    }

report.sfa <- function(x){
    re.est <- summary(x)$mleParam
    est <- format(re.est[, 1], digits = 3, nsmall = 3)
    se <- format(re.est[, 2], digits = 3, nsmall = 3)
    p.val <- report.p.val(re.est[, 4])
    re <- paste(est, p.val, " (", se, ")", sep = "")
    names(re) <- row.names(re.est)
    return(re)
    }

lr.test <- function(x, i, j, degree = NULL, crit = 0.01){
    statistic <- -2*(logLik(x[[i]])[1] - logLik(x[[j]])[1])
    p.val <- 1 - pchisq(statistic, df = degree)
    crit.val <- qchisq(1 - crit, df = degree)
    my.star <- report.p.val(p.val)
    re <-
        paste(round(statistic, 2), my.star,
              " (", round(crit.val, 2), ", ",  degree, ")", sep = "")
    return(re)
    }

t.test.report <- function(x, y){
    my.test <- t.test(x, y)
    statistic <- my.test$statistic
    p.val <- my.test$p.value
    my.star <- report.p.val(p.val)
    
    re <- paste(round(my.test$statistic, 2),
                " (", round(p.val, 2), ")", sep = "")
    return(re)
    }
    
######################################################################
### function.r ends here
