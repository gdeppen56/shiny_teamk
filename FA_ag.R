#rm(list=ls())
library(shiny)
library(data.table)
library(DT) # datatable()
library(modeest) # mlv function for mode;
library(dplyr)
library(parallel)
library(xts) # rollapply()
library(e1071) # svm()
library(stats) # loess()
library(highcharter)
library(cumstats) # cummedian
library(smooth)
require("RPostgreSQL")
# IMPORTANT: ex data format for Shiny : "normalized.index"  "index" "HOK" "HOM" "HON" "spread" "spread.yr" "n.days.to.exp" "name" "int.normalized.index.for.plot"
library(readr)
library(shinyjs)
library(shinyWidgets)
library(rhandsontable)

#=================================================== This stays the same, but ignore for now
# 
# 
# #=================================================== PROFILE SETTING
# all.trader.names <- c("AG","Jeff","Joe","Micah","Rob") #this should be checked
# this.trader <- "AG" # used for directory
# base.fa.address <- "/home/ted/ShinyApps/futures_analysis_"
# this.trader.dir <- paste0(base.fa.address,tolower(this.trader))
# 
# #------------------------------ get links
# strsplit_ <- function(x, by="") {
#   strsplit(x, split = by)[[1]]
# } # this is for length 1 vector string split
# 
# base_link_to_fa <- paste0("http://10.10.100.48:1212/ted/futures_analysis_", tolower(this.trader), "?") # to write in googlesheet notes
# base_link_to_seasonality <- paste0("http://10.10.100.48:1212/ted/seasonality_", tolower(this.trader), "?")
# mother_address_complication <- "qwrwnkjdfgnlag"
# mother_address <- paste0("http://10.10.100.48:1212/ted/mother_", tolower(this.trader), ifelse(mother_address_complication!="",paste0("_", mother_address_complication),""))
# 
# #=================================================== PROFILE SETTING ENDS
# 
# #==================dir setting
# root.dir <- "/home/ted/ShinyApps/futures_analysis"
# inputs_dir <- paste0(root.dir, "/Inputs")
# spread_objects.dir <- paste0(inputs_dir, "/spread_objects")
# setwd(spread_objects.dir)
# s.list <- gsub(".RDS", "", list.files())


#==================dir setting over


sMlt <- function(s) { # get spread multiplier
  
  unit_s_usd_vec <- (s@o.ratio * s@o.mlt / s@o.pos)
  unit_s_usd <- mean(unit_s_usd_vec[!is.na(unit_s_usd_vec)]) # brent crack, hogo, ..
  return(unit_s_usd)
}

M2N <- function(months) {
  
  # input vec: "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"
  match(tolower(months), tolower(month.abb))
}

getHL <- function(input.spread) {
  hl <-
    tryCatch({
      # Half-Life of Mean Reversion: How long it takes to revert back to mean
      
      lag_spread <- c(NA, input.spread)
      lag_spread <- lag_spread[-length(lag_spread)]
      
      delta_spread <- input.spread - lag_spread
      regress_result <- lm(delta_spread ~ lag_spread) # intercept is assumed to be alive in Mean Reverting Process
      
      round(-log(2) / regress_result$coefficients[2], 1)
    }, error = function(e) {
      NA
    })
  
  return(hl)
}


getSummary <- function(input.spread, HL_include, round_by) {
  # HL_include is a logical vec
  
  # get Descriptive Statistics and Half Life with get HL function
  
  
  distilled.spread <- input.spread[!is.na(input.spread)]
  
  summ <- as.numeric(summary(distilled.spread))
  MODE <- suppressWarnings({
    mlv(as.double(distilled.spread), method = "venter", type = "shorth")[[1]]
  })
  SD <- sd(distilled.spread)
  
  # SKEW = skewness(as.numeric(distilled.spread))
  # KURTOSIS = kurtosis(as.numeric(distilled.spread))
  
  if (HL_include == TRUE) {
    HL <- getHL(distilled.spread)
    out <- round(data.table(HL = HL, MIN = summ[1], Q1 = summ[2], MED = summ[3], MEAN = summ[4], MODE = MODE, Q3 = summ[5], MAX = summ[6], SD = SD, IQR = summ[5] - summ[2], RNG = summ[6] - summ[1]), round_by)
    # out <- round(data.table( HL=HL, MIN=summ[1],MED=summ[3],MEAN=summ[4],MODE= MODE,  MAX = summ[6],SD=SD,RNG=summ[6]-summ[1]),round_by)
    
    return(out)
  } else {
    out <- round(data.table(MIN = summ[1], Q1 = summ[2], MED = summ[3], MEAN = summ[4], MODE = MODE, Q3 = summ[5], MAX = summ[6], SD = SD, IQR = summ[5] - summ[2], RNG = summ[6] - summ[1]), round_by)
    # out <- round(data.table(MIN=summ[1],MED=summ[3],MEAN=summ[4],MODE= MODE,  MAX = summ[6],SD=SD,RNG=summ[6]-summ[1]),round_by)
    return(out)
  }
}


rollAnalysis <-
  
  function(in_data, n_d, window_width, looking="backward", period_method) {
    
    # in_data for chosen small data set
    # n_d: number of deviation away
    # window_width
    # looking= "back", "forwad", "center" ; backward-looking, forward-looking, centered-looking
    # period_method = "n.days.to.exp","normalized.index"
    
    Y <- ifelse(period_method == "n.days.to.exp", "n.days.to.exp", "int.normalized.index.for.plot")
    
    window_by <- 1 # default: window moves by 1
    
    in_data_unmelt <- data.table::dcast(in_data[, c(Y, "spread.yr", "spread"), with = FALSE], as.formula(paste0(Y, "~spread.yr")))
    in_data_unmelt_mat <- as.matrix(in_data_unmelt[, -1]) # n.days.to.exp gone
    
    roll.mat <- rollapply(in_data_unmelt_mat,
                          width = window_width,
                          FUN = function(z) {
                            m <- mean(z, na.rm = TRUE)
                            sd <- sd(z, na.rm = TRUE)
                            return(c(m, m - n_d * sd, m + n_d * sd))
                          }, by.column = FALSE, fill = NA, align = "right"
    )
    
    if (looking == "backward") {
      mean.type <- paste0(window_width, "b.look.m")
      finall.roll.mat <- roll.mat
    } else if (looking == "forward") {
      mean.type <- paste0(window_width, "f.look.m")
      finall.roll.mat <- rbind(roll.mat[window_width:nrow(roll.mat), ], matrix(rep(NA, ncol(roll.mat) * (window_width - 1)), ncol = ncol(roll.mat)))
    } else if (looking == "centered") {
      mean.type <- paste0(window_width, "c.look.m")
      divisor <- ceiling(window_width / 2)
      ref_n_block <- window_width - divisor # referebce number of blocks
      finall.roll.mat <- rbind(roll.mat[-(1:ref_n_block), ], matrix(rep(NA, ncol(roll.mat) * ref_n_block), ncol = ncol(roll.mat)))
    }
    
    
    roll.table <- setNames(cbind(in_data_unmelt[, Y, with = FALSE], finall.roll.mat), c(Y, mean.type, "l.band", "u.band"))
    roll.table.melted <- melt(roll.table, id = Y)
    
    if (period_method == "n.days.to.exp") {
      roll.table.melted$name <- roll.table.melted[[Y]]
      out <- roll.table.melted
    } else {
      to.be.merged <- unique(in_data[, c("int.normalized.index.for.plot", "normalized.index"), with = FALSE])
      out <- merge(roll.table.melted, to.be.merged)[order(variable, int.normalized.index.for.plot)]
      out$name <- out[["normalized.index"]]
    }
    out
  }



#----------------------- Relative Price related

RP_Score <-
  function(sd_wide_except_ongoing.yr, today, today.spread) {
    
    # ex) today = -211, today.spread = -.5; today relates to ongoing yr
    # sd_wide_except_ongoing.yr: wide data without ongoing yr and n.days.to.exp; starts from today+1
    
    density <- getKDE(as.numeric(as.matrix(sd_wide_except_ongoing.yr)))
    cumulative_probability <- density[[2]](today.spread)
    
    return(Score2(cumulative_probability))
  }


getKDE <- function(prices) { # Get Kernel Density Estimate (with CDF)
  
  prices <- prices[!is.na(prices)] # no NAs
  
  hT <- bw.nrd0(prices)
  kde_ <- Vectorize(function(x) mean(dnorm((x - prices) / hT) / hT)) # returns PDF at x
  cdf_ <- Vectorize(function(x) mean(pnorm((x - prices) / hT))) # returns CDF at x
  
  c(kde_, cdf_) #
}

Score2 <- function(p, n=10) { # takes a single cumulative probability x and return BuySell score (-10,-9,...,+9,+10)
  
  if (!is.na(p)) {
    score <- 2 * n * (p - 1) + n
  } else {
    score <- p
  }
  
  
  return(score)
}

#----------------------- Seasonality related 

# sd_wide_except_ongoing.yr <- dw[1:50,]

Seasonality_tslice_dailymed <- # take care of data of 1 row
  
  function(sd_wide_except_ongoing.yr, M="median") { # 1st column is n.days.to.exp
    
    #----------- returns vector
    prices <- sd_wide_except_ongoing.yr[, -1]
    # daily.moves <- rbind(matrix(0,nrow=1,ncol=ncol(prices)), apply(prices,2,diff)) #daily moves
    
    daily.moves <- apply(prices, 2, diff) # daily moves
    
    
    if (M == "median") {
      M.daily.moves <- apply(daily.moves, 1, median, na.rm = TRUE) # median daily moves
    } else {
      M.daily.moves <- apply(daily.moves, 1, mean, na.rm = TRUE) # median daily moves
    }
    # M.daily.moves <- apply(daily.moves,1,median,na.rm=TRUE) # median daily moves
    # M.daily.moves <- M.daily.moves[1:5]
    
    cumsum.M.daily.moves <- cumsum(M.daily.moves)
    
    seasonality <- rev(cummean(rev(cumsum.M.daily.moves)) - c(rev(cumsum.M.daily.moves)[-1], 0))
    seasonality <- c(seasonality, NA_real_)
    
    return(seasonality)
  }

Seasonality_roll_dailymed <-
  
  function(sd_wide_except_ongoing.yr, M="median", lookforward=25) { #
    
    prices <- sd_wide_except_ongoing.yr[, -1] # [1:20]
    seasonality_vec <- zoo::rollapply(prices,
                                      width = lookforward, by = 1, partial = FALSE,
                                      FUN = function(z) {
                                        
                                        # print(z)
                                        roll_prices <- as.data.table(z) # data w/o n.days.period
                                        diff_roll_prices <- apply(roll_prices, 2, diff)
                                        daily_med_moves <- apply(diff_roll_prices, 1, median, na.rm = TRUE)
                                        
                                        if (M == "median") {
                                          seasonality <- median(cumsum(daily_med_moves), na.rm = TRUE)
                                        } else {
                                          seasonality <- mean(cumsum(daily_med_moves), na.rm = TRUE)
                                        }
                                        
                                        return(seasonality) # result in rolling window
                                      }, by.column = FALSE, fill = NA, align = "left"
    )
    
    
    
    
    
    return(seasonality_vec) #----------- returns vector
  }


Seasonality_roll_prob <-
  
  function(sd_wide_except_ongoing.yr, M="median", lookforward=5) { #
    
    prices <- sd_wide_except_ongoing.yr[, -1] # [1:20]
    
    seasonality_vec <- zoo::rollapply(prices,
                                      width = lookforward, by = 1, partial = FALSE,
                                      FUN = function(z) {
                                        
                                        # print(z)
                                        roll_prices <- as.data.table(z) # data w/o n.days.period
                                        moves <- sweep(roll_prices, 2, as.numeric(roll_prices[1, ])) # moves from today
                                        mean.moves <- apply(moves[-1, ], 2, mean, na.rm = TRUE)
                                        
                                        return(mean.moves) # result in rolling window
                                      }, by.column = FALSE, fill = NA, align = "left"
    )
    
    
    seasonality_vec <- apply(seasonality_vec, 1, function(x) {
      20 * (sum(x > 0, na.rm = TRUE) / length(x[!is.na(x)])) - 10
    })
    
    return(seasonality_vec) #----------- returns vector
  }

seasonality_ef <-
  
  function(moves, entire.start, entire.end, lf) { # IMPORTANT: use base.left.tbl
    # moves: moves tbl
    # in.begin.vec (legitimate)
    # in.end.vec   (legitimate)
    # return seasonality E, F ( as a data table)
    target_period <- entire.start:entire.end
    
    in.begin.vec <- target_period
    in.end.vec <- in.begin.vec + lf
    
    legit.index <- which(in.end.vec <= 0)
    
    in.begin.vec <- in.begin.vec[legit.index]
    in.end.vec <- in.end.vec[legit.index]
    
    seasonality_updn.move <- rep(NA_real_, length(in.begin.vec))
    seasonality_move <- rep(NA_real_, length(in.begin.vec))
    
    for (i in seq(in.begin.vec)) {
      # print(i)
      in.begin <- in.begin.vec[i]
      in.end <- in.end.vec[i]
      
      target.moves.index <- base.left.tbl[begin >= in.begin & end <= in.end]$index
      target.moves <- moves[target.moves.index]
      
      yrly.seasonality_updn.move <- apply(target.moves, 2, function(this.yr.s) {
        # this.yr.s <- moves[target.index][[1]]
        mean(sign(this.yr.s), na.rm = TRUE) # up=1,dn=-1
      })
      
      yrly.seasonality_move <- apply(target.moves, 2, function(this.yr.s) { # could divide target.moves to make the moves into speed:  / ( base.left.tbl[target.moves.index]$ww-1)
        # this.yr.s <- moves[target.index][[1]]
        mean(this.yr.s, na.rm = TRUE)
      })
      
      seasonality_updn.move[i] <- -mean(yrly.seasonality_updn.move, na.rm = TRUE) # MINUS!
      seasonality_move[i] <- -mean(yrly.seasonality_move, na.rm = TRUE)
    }
    
    merge(data.table(n.days.to.exp = target_period), data.table(n.days.to.exp = in.begin.vec, seasonality_updn.move, seasonality_move), all.x = TRUE, by = "n.days.to.exp")
  }

seasonality_d <-
  
  function(moves, entire.start, entire.end, lf) { # IMPORTANT: use base.left.tbl
    # moves: moves tbl
    # in.begin.vec (legitimate)
    # in.end.vec   (legitimate)
    # return seasonality E, F ( as a data table)
    target_period <- entire.start:entire.end
    
    base <- base.left.tbl[ww == (lf + 1) & begin >= entire.start & begin <= entire.end ]
    
    # in.begin.vec <-  base$begin
    # in.end.vec <-    base$end
    
    # seasonality_prob <- rep(NA_real_,length(in.begin.vec))
    # seasonality_move <- rep(NA_real_,length(in.begin.vec))
    
    seasonality <-
      apply(moves[base$index], 1, function(yrly.diff) {
        M.move <- mean(yrly.diff, na.rm = TRUE)
        
        # M.move <- mean(yrly.diff,na.rm=TRUE)
        prob <- ifelse(mean(yrly.diff > 0, na.rm = TRUE) > 0.5, mean(yrly.diff > 0, na.rm = TRUE), mean(yrly.diff < 0, na.rm = TRUE)) # think well
        M.move * prob
      })
    
    merge(data.table(n.days.to.exp = target_period), data.table(n.days.to.exp = base$begin, score = -seasonality), all.x = TRUE, by = "n.days.to.exp")
  }



seasonality_d_versatile <-
  
  function(moves, entire.start, entire.end) { # IMPORTANT: use base.left.tbl
    
    # setwd(inputs_dir)
    # s <- s.list[[150]]
    # d <- readRDS(paste0(s,".RDS"))
    # d <- d$d
    #
    # possible.spread.yrs <- unique(d$spread.yr)
    #
    #
    #   temp <-
    #     lapply(possible.spread.yrs,function(this.yr){
    #
    #       #this.yr <- "2006"
    #       this.yr.char <- as.character(this.yr)
    #       d.this.yr <- d[spread.yr==this.yr.char]
    #       d.this.yr.simple<- merge(data.table(n.days.to.exp=base.T.rng),d.this.yr[,c("n.days.to.exp","spread")],all.x=TRUE,by="n.days.to.exp")
    #       z <- d.this.yr.simple$spread
    #       zz <- outer(z,z,'-') #outer product -> outer subtraction
    #       y <- data.table(diff = zz[lower.tri(zz)]) # long table
    #       setNames(y,this.yr.char)
    #
    #     })
    #
    #   moves <- dplyr::bind_cols(temp)
    
    # moves: moves tbl
    # entire.start =-150
    # entire.end  = -30
    
    lf.vec <- c(10, 25)
    
    target_period <- entire.start:entire.end
    
    score_by_lf <-
      lapply(lf.vec, function(lf) {
        base <- base.left.tbl[ww == (lf + 1) & begin >= entire.start & begin <= entire.end ]
        
        # in.begin.vec <-  base$begin
        # in.end.vec <-    base$end
        
        # seasonality_prob <- rep(NA_real_,length(in.begin.vec))
        # seasonality_move <- rep(NA_real_,length(in.begin.vec))
        
        seasonality <-
          apply(moves[base$index], 1, function(yrly.diff) {
            M.move <- mean(yrly.diff, na.rm = TRUE)
            
            # M.move <- mean(yrly.diff,na.rm=TRUE)
            prob <- ifelse(mean(yrly.diff > 0, na.rm = TRUE) > 0.5, mean(yrly.diff > 0, na.rm = TRUE), mean(yrly.diff < 0, na.rm = TRUE)) # think well
            M.move * prob
          })
        
        merge(data.table(n.days.to.exp = target_period), data.table(n.days.to.exp = base$begin, score = -seasonality), all.x = TRUE, by = "n.days.to.exp")[, "score"]
      })
    
    z <- data.table(n.days.to.exp = target_period, setNames(dplyr::bind_cols(score_by_lf), as.character(lf.vec)))
  }
#----------------------- Range related 

Range_Risk_roll <-
  
  function(sd_wide_except_ongoing.yr, lookforward=25) { #
    
    
    #### important: I exclude yrs with any NA data in the rolling method
    
    
    # sd_wide_except_ongoing.yr <- dw
    prices <- sd_wide_except_ongoing.yr # [,-1]#[1:20]
    # z <- prices[200:224]
    excluded.yrs <- names(which(apply(prices, 2, function(x) {
      all(is.na(x))
    })))
    if (length(excluded.yrs) != 0) {
      prices <- prices[, !names(prices) %in% excluded.yrs, with = FALSE]
    }
    prices <- prices[, -1]
    # roll_prices <- as.matrix(prices[1:25])
    
    result <- zoo::rollapply(prices,
                             width = lookforward + 1, by = 1, partial = FALSE,
                             FUN = function(roll_prices) {
                               
                               # ### risk
                               # moves <- sweep(roll_prices,2,as.numeric(roll_prices[1,]))
                               #
                               # max_risk_each.yr <-
                               #   apply(moves,2,function(x){
                               #
                               #     short_risks <- x[x>0 ] #candidate of max short risk for a given yr
                               #     long_risks <- x[x<0 ]
                               #
                               #
                               #     max_short_risk <- ifelse(length(short_risks)!=0,max(short_risks),0) # data has to be complete; otherwise NA
                               #     max_long_risk <- ifelse(length(long_risks)!=0,abs(min(long_risks)),0)
                               #     #mean_short_risk <- ifelse(length(short_risks)!=0,mean(short_risks),0) # data has to be complete; otherwise NA
                               #     #mean_long_risk <- ifelse(length(long_risks)!=0,abs(mean(long_risks)),0) # data has to be complete; otherwise NA
                               #
                               #
                               #     c(max_long_risk,max_short_risk)
                               #     #0.10000000 0.03000000 0.03500000 0.01666667
                               #
                               #   })
                               #
                               # max_risk <- apply(max_risk_each.yr,1,max,na.rm=TRUE)
                               # mean_risk <- apply(max_risk_each.yr,1,mean,na.rm=TRUE)
                               # n.yrs.risks <- sum(!is.na(max_risk_each.yr[1,]))
                               
                               #
                               ranges <- apply(apply(roll_prices, 2, range), 2, diff) # only for yrs with complete data
                               mean_range <- mean(ranges, na.rm = TRUE)
                               median_range <- median(ranges, na.rm = TRUE)
                               # min_range <- min(ranges,na.rm=TRUE)
                               # max_range <- max(ranges,na.rm=TRUE)
                               # n.yrs.range <- sum(!is.na(max_risk_each.yr[1,])) #assume same as n.yrs.risks
                               #
                               
                               out <- c( # max_risk,
                                 # mean_risk,
                                 # n.yrs.risks,
                                 mean_range,
                                 median_range
                                 # min_range,
                                 # max_range
                               )
                               
                               return(out) # result in rolling window
                             }, by.column = FALSE, fill = NA_real_, align = "left"
    )
    
    result <- setNames(as.data.table(result), c("mean.rng", "med.rng"))
    # result <- setNames(as.data.table(result),c("max.L.risk","max.S.risk","mean.L.risk","mean.S.risk","n.yrs.used","mean.rng","med.rng","min.rng","max.rng"))
    
    
    return(result) #----------- returns vector
  }


#----------------------- Scalpability related 


getTheta <- function(input.spread) {
  theta <-
    tryCatch({
      regress_result <- lm(diff(input.spread) ~ input.spread[-length(input.spread)])
      
      
      
      # round(-log(2)/regress_result$coefficients[2],1)
      round(-regress_result$coefficients[2], 1)
    }, error = function(e) {
      NA
    })
  
  return(theta)
}


Scalpability_HL <-
  
  function(sd_wide_except_ongoing.yr, lookforward=25) { #
    
    # sd_wide_except_ongoing.yr <- dw
    prices <- sd_wide_except_ongoing.yr # [,-1]#[1:20]
    excluded.yrs <- names(which(apply(prices, 2, function(x) {
      all(is.na(x))
    })))
    if (length(excluded.yrs) != 0) {
      prices <- prices[, !names(prices) %in% excluded.yrs, with = FALSE]
    }
    prices <- prices[, -1]
    
    # roll_prices <- as.matrix(prices[1:25])
    scalpability_vec <- zoo::rollapply(prices,
                                       width = lookforward + 1, by = 1, partial = FALSE,
                                       FUN = function(roll_prices) {
                                         # print(roll_prices)
                                         excluded.yrs <- names(which(apply(roll_prices, 2, function(x) {
                                           any(is.na(x))
                                         })))
                                         if (length(excluded.yrs) != 0) {
                                           roll_prices <- roll_prices[, !colnames(roll_prices) %in% excluded.yrs]
                                         }
                                         hl_each.yr <- apply(roll_prices, 2, getHL)
                                         theta_each.yr <- apply(roll_prices, 2, getTheta)
                                         
                                         n_pos.yr <- sum(hl_each.yr > 0, na.rm = TRUE) # good
                                         n_neg.yr <- sum(hl_each.yr < 0, na.rm = TRUE) # bad
                                         
                                         # ratio_pos.yr <- n_pos.yr/(n_pos.yr+n_neg.yr)
                                         
                                         # mean_hl_pos.yr <- mean(hl_each.yr[hl_each.yr>0],na.rm=TRUE)     #good
                                         # mean_hl_neg.yr <- mean(hl_each.yr[hl_each.yr<0],na.rm=TRUE)     #good
                                         
                                         # mean_theta_pos.yr <- mean(theta_each.yr[theta_each.yr>0],na.rm=TRUE)     #good
                                         # mean_theta_neg.yr <- mean(theta_each.yr[theta_each.yr<0],na.rm=TRUE)     #good
                                         
                                         mean_mean.theta <- mean(theta_each.yr, na.rm = TRUE)
                                         
                                         out <- c(n_pos.yr, n_neg.yr, mean_mean.theta)
                                         
                                         return(out) # result in rolling window
                                       }, by.column = FALSE, fill = NA_real_, align = "left"
    )
    
    result <- as.data.table(setNames(as.data.frame(scalpability_vec), c("n_pos.yr", "n_neg.yr", "mean.theta")))
    # seasonality_vec <- apply(seasonality_vec,1,function(x){ 20*(sum(x>0,na.rm=TRUE)/length(x[!is.na(x)]))-10 })
    
    return(result) #----------- returns vector
  }
#----------------------- Stability related 

Stability_SD <-
  
  function(sd_wide_except_ongoing.yr, lookforward=25) { #
    
    # sd_wide_except_ongoing.yr <- dw
    prices <- sd_wide_except_ongoing.yr # [,-1]#[1:20]
    excluded.yrs <- names(which(apply(prices, 2, function(x) {
      all(is.na(x))
    })))
    if (length(excluded.yrs) != 0) {
      prices <- prices[, !names(prices) %in% excluded.yrs, with = FALSE]
    }
    prices <- prices[, -1]
    
    # roll_prices <- as.matrix(prices[1:25])
    stability_vec <- zoo::rollapply(prices,
                                    width = lookforward + 1, by = 1, partial = FALSE,
                                    FUN = function(roll_prices) {
                                      # print(roll_prices)
                                      excluded.yrs <- names(which(apply(roll_prices, 2, function(x) {
                                        any(is.na(x))
                                      })))
                                      if (length(excluded.yrs) != 0) {
                                        roll_prices <- roll_prices[, !colnames(roll_prices) %in% excluded.yrs]
                                      }
                                      sd_roll <- sd(roll_prices, na.rm = TRUE)
                                      mean_roll <- mean(roll_prices, na.rm = TRUE)
                                      
                                      out <- c(mean_roll, -sd_roll)
                                      
                                      return(out) # result in rolling window
                                    }, by.column = FALSE, fill = NA_real_, align = "left"
    )
    
    result <- as.data.table(setNames(as.data.frame(stability_vec), c("mean", "stability")))
    # seasonality_vec <- apply(seasonality_vec,1,function(x){ 20*(sum(x>0,na.rm=TRUE)/length(x[!is.na(x)]))-10 })
    
    return(result) #----------- returns vector
  }

#--------------------------------------------- Break Spreads' names 


monthN <-
  function(month) {
    out <- which(month.abb == month)
    if (length(out) == 0) {
      out <- month
    }
    return(out)
    # switch(month,"Jan"=1, "Feb"=2, "Mar"=3, "Apr"=4, "May"=5, "Jun"=6, "Jul"=7, "Aug"=8, "Sep"=9, "Oct"=10, "Nov"=11, "Dec"=12)
  }

nM_to_n <-
  function(nM) { # nM= "12M"; not vectorized
    
    
    nM_exist <- grepl("[[:digit:]]+M", nM)
    
    if (sum(nM_exist) > 0) {
      nM <- as.numeric(gsub("M", "", nM[nM_exist]))
    }
    
    
    return(nM)
    # switch(month,"Jan"=1, "Feb"=2, "Mar"=3, "Apr"=4, "May"=5, "Jun"=6, "Jul"=7, "Aug"=8, "Sep"=9, "Oct"=10, "Nov"=11, "Dec"=12)
  }

spreadNameBr <- function(this.name) { # this.name is vector
  
  temp <- rbindlist(lapply(strsplit(this.name, "_"), function(this.vec) {
    data.table(spread_under_type = paste(this.vec[c(1, 4)], collapse = "___"), month = this.vec[3], int = this.vec[2])
  }))
  
  temp$month <- factor(temp$month, levels = c(month.abb, sort(setdiff(temp$month, month.abb))))
  
  int_essential_levels <- c("consecutive", "1M", "2M", "3M", "6M", "9M", "12M")
  temp$int <- factor(temp$int, levels = c(int_essential_levels, sort(setdiff(temp$int, int_essential_levels))))
  
  
  temp <- temp[order(spread_under_type, int, month)]
  
  temp <- tidyr::separate(temp, col = "spread_under_type", into = c("product", "relationship"), sep = "___")
  temp$spread_under_type <- paste(temp$product, temp$relationship)
  temp
}

#-------------------------------------- slack 
library(slackr)
#api_token_teamKnuepfer <- "" #is api token forever?

slackit <- 
  function(channel="#master", message ,api_token=api_token_teamKnuepfer ){
    
    slackr_setup(channel = "#master", username = "busterbaram", icon_emoji = "",
                 incoming_webhook_url = "", api_token = api_token_teamKnuepfer, config_file = "~/.slackr",
                 echo = FALSE)
    text_slackr(message)
    
  }
#--------------------------------------- googlesheets related
library(googlesheets)
setwd(this.trader.dir)
gs_auth(token = "googlesheets_token.rds")

# setwd("/home/ted/ShinyApps/futures_analysis_rob/")
#--------------------------------------- break spread names

spread.names.br <- spreadNameBr(s.list)



pool <- pool::dbPool(
  drv= DBI::dbDriver("PostgreSQL"), dbname = "fa_notes",
  host = "localhost", port = 5432,
  user = "postgres", password = "Akqthtk0911"
)
onStop(function() {
  poolClose(pool)
})
# poolClose(pool)
# 
# lapply(dbListConnections(pool, dbDisconnect))


getQuery_specific <- 
  function(in.product,in.interval,in.month,in.relationship){
    #this.product="BRENT"
    #this.interval="12M"
    #this.month="Dec"
    #this.relationship="2X"
    out <- pool %>% tbl("whole") %>% filter(product==in.product & interval==in.interval & month==in.month & relationship==in.relationship) %>% collect()
    #command <- paste0("SELECT * FROM whole WHERE product=",quot(product), " AND interval=",quot(interval)," AND month =", quot(month)," AND relationship=",quot(relationship),";")
    out <- as.data.table(out)
    
    out
    
  }


#--------------------------------------- tab Mapping

tabMap <- 
  function(spread.names.br){
    
    ### Mapping from spread_under_type to tab
    tab_mapping <- unique(spread.names.br[,c("product","spread_under_type")])
    tab_mapping$tab = NA_character_
    
    #-- take care of individual tabs
    products_on_individual_tabs <- c("BRENT","LIGHTSWEETCRUDEOIL","HO","GO")
    tab_mapping[product %in% products_on_individual_tabs]$tab <- tab_mapping[product %in% products_on_individual_tabs]$spread_under_type
    #-- take care of individual HO-GO tabs
    tab_mapping[grep("HO-GO",product)]$tab <- "HO-GO"
    #-- take care of individual MEAT tabs
    meat_products <- c("LEANHOGS","LIVECATTLE","FEEDERCATTLE")
    tab_mapping[product %in% meat_products]$tab  <- "MEAT"
    #-- take care of individual AGRICULTURE tabs
    agriculture_products <- c("COCOA-COCOALIFFE",    "COCOALIFFE",  "COCOA" ,"CORN" ,"KCWHEAT-WHEAT",
                              "KCWHEAT" , "MSPWHEAT-WHEAT" ,"MSPWHEAT" ,"ROBUSTACOFFEE"       ,     "SOYBEANOIL" ,
                              "SUGARNO.11", "WHEAT"          ,          "WHITESUGAR" 
    )
    tab_mapping[product %in% agriculture_products]$tab  <- "AGRICULTURE"
    
    #-- take care of individual STIR(=Short Term Interest Rates) tabs
    stir_products <- c("EURIBOR"  ,                "EURO-DOLLAR"         ,     "EUROSWISS"            ,    "FEDFUNDS","STERLING")
    tab_mapping[product %in% stir_products]$tab  <- "STIR"
    #-- take care of individual OTHER tabs
    tab_mapping[is.na(tab)]$tab <- "OTHERS"
    tab_mapping
  }
tabmap <- tabMap(spread.names.br)
#--------------------------------------------- Rounding criteria

spread_round_by <- 5
summary_round_by <- 3
rotation_text <- 30 # 45 degree
#------------------------------ get base.left.tbl ( for moves table (-500~0 base))
base.T.rng <- -500:0
setwd(root.dir)
base.left.tbl <- readRDS("base.left.tbl.RDS")

#--------------------------------------------- color setting

setwd(this.trader.dir)

hist_height <- 450

mean_color <- "red"
median_color <- "green"
Q_color <- "green"
sd_color <- "navy"
minmax_color <- "green"
ongoing.yr_color <- "lime"
# mean_align="left"
# median_align = "right"
# Q_align=
# sd_align="center"
# minmax_align = "left"

mean_vertical_align <- "bottom"
median_vertical_align <- "top"
Q_vertical_align <- "top"
sd_vertical_align <- "middle"
minmax_vertical_align <- "middle"





ui <- function(request){ #request for bookmarking
  
  fluidPage(
    headerPanel(title = HTML(""), windowTitle = "FUTURES ANALYSIS"),
    #span(textOutput("display_username"), style = "color:grey"),
    useShinyjs(), # Set up shinyjs; ex) toggle()
    
    uiOutput("ui_text"),
    uiOutput("ui_note"),
    # fluidRow(column(width=12,
    
    # )),
    br(), br(), br(),br(),
    # uiOutput("note_dropdown_absolute_panel"),
    
    fluidRow(style = "background-color:#f8fff2;",column(offset=2,width = 8, align="center",uiOutput("ui_note_tbl")),
             column(width=2,align="center",uiOutput("ui_note_filter"))
             
             
             
    ),br(),
    
    fluidRow(
      column(
        width = 2,
        fluidRow(
          column(
            width = 12,
            tags$head(
              tags$link(rel = "shortcut icon", href = "favicon.ico")
            ),
            a("back to Mother", href = mother_address),
            uiOutput("spread_part1_ui"), tags$style(type = "text/css", ".selectize-dropdown-content {max-height: 850px; }")
          )
        ),
        fluidRow(
          column(width = 6, uiOutput("spread_part2_ui")),
          column(width = 6, uiOutput("spread_part3_ui"))
        )
      ),
      column(
        width = 1, align = "center",
        #column(width = 1, offset = 0, align = "center", uiOutput("ui_setting_dropdown"))
        uiOutput("ui_today_text"),
        br(),
        
        
        # actionButton(
        #   inputId = "ready_button", label = "Ready", icon("refresh"),
        #   style = "color: #fff; background-color: #9988f7; border-color: #9988f7"
        # ),
        
        # actionBttn(
        #   inputId = "ready_button", label = "Ready", icon("refresh"),
        #   # style="color: #fff; background-color: #9988f7; border-color: #9988f7"
        #   style = "material-circle", color = "royal", size = "sm", no_outline = TRUE
        # ),
        
        #br(), #br(),
        selectInput(
          "period_method",
          label = "", width = "70%",#h6("x-axis")
          choices = c("n.to.exp" = "n.days.to.exp", "date" = "normalized.index")
        ),        
        uiOutput("ui_setting_dropdown")
      ),
      column(
        width = 6, offset = 0, align = "center",
        fluidRow(column(width = 12, align = "center", uiOutput("ui_select_period"))),
        fluidRow(column(width = 10, align = "center", uiOutput("ui_select_years")),
                 column(width=2,align="right",
                        radioGroupButtons(inputId = "select_yrs_refined", 
                                          label = "", 
                                          choices = c("X",  "15+", "O"), 
                                          selected="O",
                                          status = "info")
                 )
        )

      ),
      
      column(
        width = 1, align = "center",
        br(),
        uiOutput("ui_go_button"),
        br(), br(),
        uiOutput("ui_ongoing_yr")
      )
      ,column(width=2,#align="center",   
              uiOutput("ui_note_read"),br(),br(),
              rHandsontableOutput(outputId = "yrs_to_T") #, width = "100%", height = "100%") 
              
              #tags$style(type = "text/css", "#speed_tbl th {font-weight:bold;}")
      )
     
      
    ),
    fluidRow(column(width = 3, textOutput("test_text"))),
    #fluidRow(column(width=3,bookmarkButton())),
    
    fluidRow(column(offset=2,width = 8, align="center",uiOutput("ui_test_tbl"))),
    
    tabsetPanel(
      tabPanel(
        "analysis",
        
        fluidRow(
          column(
            width = 1,
            # h6("Group Analysis"),
            
            # wellPanel(style = "background-color: #ffffff;",
            #           uiOutput("ui_sd_mult_gr")
            # ),
            wellPanel(
              # style = "background-color: #ffffff;",
              #  uiOutput("ui_sd_mult_gr"),
              uiOutput("ui_technical_analysis_start_day"),
              wellPanel(
                style = "background-color: #ffffff;",
                
                # uiOutput("ui_help_regression_text"),
                uiOutput("ui_check_moving_analysis"),
                uiOutput("ui_roll_window_width"),
                uiOutput("ui_roll_N_std"),
                uiOutput("ui_roll_looking"),
                uiOutput("ui_loess"),
                uiOutput("ui_svm")
              ),
              # wellPanel(
              #   style = "background-color: #ffffff;",
              #
              #
              # ),
              uiOutput(
                # style = "background-color: #ffffff;",
                "ui_go_button_moving_analysis"
              )
            ),
            
            absolutePanel(
              fixed = TRUE,
              
              wellPanel(
                
                # style = "background-color: #ffffff;",
                
                #TODO:
                #uiOutput("ui_notes_new"),
                uiOutput("ui_relative_price"),
                uiOutput("ui_seasonality"),
                uiOutput("ui_range.risk"),
                uiOutput("ui_scalpability"),
                uiOutput("ui_stability"),
                uiOutput("ui_indicator_lookforward"),
                
                uiOutput("ui_seasonality_link")
              )
              
              
              #  uiOutput("ui_indicator_lookforward")
            )
          ),
          column(width = 11, class = "panel", uiOutput("ui1"))
        ),
        
        
        fluidRow(
          column(offset = 1, width = 11, uiOutput("ui_RPchart")),
          column(offset = 1, width = 11, uiOutput("ui_Schart_tslice_dailymed")), # seasonality chart
          # column(offset=1,width = 11,class="panel",uiOutput("ui_Schart2")), #seasonality chart
          column(offset = 1, width = 11, uiOutput("ui_Schart_roll_dailymed")),
          # column(offset=1,width = 11,class="panel",uiOutput("ui_Schart4")), #seasonality chart
          column(offset = 1, width = 11, uiOutput("ui_Schart_roll_prob")), # seasonality chart
          column(offset = 1, width = 11, uiOutput("ui_Schart_D")), # seasonality chart
          column(offset = 1, width = 11, uiOutput("ui_Schart_D_versatile")), # seasonality chart
          
          column(offset = 1, width = 11, uiOutput("ui_Schart_E")), # seasonality chart
          column(offset = 1, width = 11, uiOutput("ui_Schart_F")), # seasonality chart
          
          
          
          column(offset = 1, width = 11, uiOutput("ui_RRchart_roll")), # Range Risk Chartchart
          column(offset = 1, width = 11, uiOutput("ui_SCALPchart")), # scalpability
          column(offset = 1, width = 11, uiOutput("ui_STABLEchart"))
          
          
          # column(offset=3,width = 3,class="panel",uiOutput("ui_Risk_circle")),
          #  column(offset=0,width = 3,class="panel",uiOutput("ui_Risk_circle_all"))
        ),
        fluidRow(
          column(offset = 1, width = 3, align = "center", uiOutput("ui_summary.stat")),
          column(offset = 0, width = 3, align = "center", highchartOutput("hc_boxplot", height = hist_height)),
          column(offset = 0, width = 1, align = "center", uiOutput("ui_summary.stat2")),
          column(offset = 0, width = 4, align = "center", highchartOutput("hc_group_hist", height = hist_height))
        )
      ),
      
      tabPanel(
        "meta",
        
        fluidRow(
          column(
            width = 2, offset = 1,
            h6("definition"),
            uiOutput("ui_meta")
          ),
          column(
            width = 3, offset = 1,
            h6("delivYr"),
            uiOutput("ui_meta_yr")
          ),
          column(
            width = 3, offset = 1,
            h6("Expiry"),
            uiOutput("ui_meta_exp")
          )
        )
      ),
      tabPanel(
        "data",
        
        fluidRow(column(
          width = 10, offset = 1,
          uiOutput("ui_pure_spread_data_table")
        ))
      )
    )
  )
  
}




server <- function(input, output, session) {
  
  session$onSessionEnded(function(){
    lapply(dbListConnections(drv), dbDisconnect)
  })
  
  
  # onBookmark(function(state) {
  #   state$values$ready_first <- check$ready_first
  #   state$values$ready_first <- check$go_first
  # 
  # })
  
  # onRestore(function(state) {
  # 
  #   check$ready_first <- state$values$ready_first
  #   check$go_first <- state$values$go_first
  # 
  #   X <- state$input$spread_part1
  #   Y <- state$input$spread_part2
  #   Z <- state$input$spread_part3
  #   a <- state$input$ndays_period
  #   b <- state$input$selected_years
  #   updateSelectizeInput(session, "spread_part1", choices=X,selected = X)
  #   updateSelectizeInput(session, "spread_part2", choices=Y,selected = Y)
  #   updateSelectizeInput(session, "spread_part3", choices=Z,selected = Z)
  #   updateSliderInput(session,"ndays_period",value=a,min = -500, max = 0, step = 1)
  #   # checkboxGroupInput("selected_years", "",
  #   #                    choiceNames = as.character(possible.spread.years()),
  #   #                    choiceValues = possible.spread.years(),
  #   #                    selected = selected,
  #   #                    inline = TRUE
  #   # )
  # 
  #   updateCheckboxGroupInput(session, "selected_years",  inline = TRUE,selected =b #choiceNames = b,
  #                            #choiceValues = b
  #                            )
  # 
  # })
  # 
  
  
  
  output$ui_text <- renderUI({
    textOutput("text")
  })
  # output$text <- renderText({   gsub("\\?","",session$clientData$url_search)==""    })
  # output$text <- renderText({   session_input_pure()   })
  
  output$ui_seasonality_link <- renderUI({
    #if (check$ready_first) {
    link <- paste0(base_link_to_seasonality, gsub(" ", "@", chosen_spread()), "__", input$ndays_period[1], "_", input$ndays_period[2]) # @: to take care of "2X CONDOR", "2X 2X" relationships -> space become weird in link
    
    
    tags$a(
      href = link,
      "Seasonality Shiny", target = "_blank"
    )
    #}
  })
  
  fa_note_link <- reactive({
    link <- paste0(base_link_to_fa, gsub(" ", "@", chosen_spread()), "__", 
                   input$ndays_period[1], "_", input$ndays_period[2],"__",
                   input$ongoing_yr,"__",
                   paste(input$selected_years,collapse="_")
    ) # @: to take care of "2X CONDOR", "2X 2X" relationships -> space become weird in link
    
  })
  
  session_input_pure <- reactive({
    gsub("@", " ", gsub("\\?", "", session$clientData$url_search))
    
    # basically three parts: profile__period__ongoing.yr
    # Preprocess: first $ should be deleted. then @ should be replaced with space (ex) 2X@CONDOR -> 2X CONDOR)
    # also note that this part is "" when there's no session input, leading all the subsequent reactive data to be NA
    # we desire the following reactive data to impact once only in the beginning
  })
  
  
  # this part could be changed by reactiveValues
  profile_session_input <- reactive({
    out <- strsplit_(session_input_pure(), "__")[1]
    # ifelse(!check$go_first, out, "")
  })
  begin.end_session_input <- reactive({
    # begin.end_session_input <- as.integer(strsplit(strsplit(session_input_pure(),"__")[[1]][2],"_")[[1]])
    out <- as.integer(strsplit_(strsplit_(session_input_pure(), "__")[2], "_"))
    # ifelse(!check$go_first, out, "")
  })
  
  ongoing.yr_session_input <- reactive({
    out <- strsplit_(session_input_pure(), "__")[3]
    # ifelse(!check$go_first, out, "")
  })
  
  chosen.yrs_session_input <- reactive({
    out <- strsplit_(session_input_pure(), "__")[4]
    strsplit_(out,"_")
    
    # ifelse(!check$go_first, out, "")
  })
  
  ################################################### REACTIVE DATA PART #########################################
  
  check <- reactiveValues(go_first = FALSE, ready_first = FALSE, roll_first = FALSE, remind_first = FALSE) # first= has there ever been any go button click? ready_first = has there ever been any ready button click? roll_first = has there ever been check in check_moving_analysis?
  
  # observeEvent(profile_session_input(),if(!is.na(profile_session_input())){ # if there is any session input
  #   
  #   check$ready_first<- TRUE
  #   #check$go_first<- TRUE
  #   
  # } )
  
  
  observeEvent(input$go_button, {
    check$go_first <- TRUE
  })
  
  # observeEvent(input$ready_button, {
  #   check$ready_first <- TRUE
  # })
  observeEvent(input$check_moving_analysis, {
    check$roll_first <- TRUE
  })
  
  observeEvent(input$remind, {
    check$remind_first <- TRUE
  })
  
  
  chosen_spread <- reactive({ # purest form data when you choose the spread
    alpha <- unlist(strsplit(input$spread_part1, " ")) # spread underlier+type
    paste(alpha[1], input$spread_part2, input$spread_part3, paste(alpha[2:length(alpha)], collapse = " "), sep = "_")
  })
  
  chosen_product <- reactive({ # purest form data when you choose the spread
    alpha <- unlist(strsplit(input$spread_part1, " ")) # spread underlier+type
    alpha[1]
  })
  chosen_interval <- reactive({input$spread_part2})
  chosen_month <- reactive({ input$spread_part3})
  chosen_relationship <- reactive({
    alpha <- unlist(strsplit(input$spread_part1, " ")) # spread underlier+type
    paste(alpha[2:length(alpha)], collapse = " ")
    
  })
  
  
  possible.spread.years <- reactive({
    begin.point <- -500 # max(furthest.day.from.exp(), -500)
    sort(unique(spread_data()$d[n.days.to.exp >= begin.point]$spread.yr))
  }) # inputs
  
  unexpired_yrs <- reactive({
    
    pure_spread_meta_yr()$spread[as.Date(pure_spread_meta_exp()$spread) > Sys.Date()]
    
  })
  ongoing_yr_to_T <- reactive({
    
    v <-as.data.table(t(pure_spread_data()[spread.yr %in% unexpired_yrs(),.(T=tail(n.days.to.exp,1)),by=spread.yr]))
    setNames(v[-1,],as.character(v[1,]))
    
    
  })
  
  # furthest.day.from.exp <- reactive({
  #   min(spread_data()$d$n.days.to.exp)
  # }) # for n.days.to.exp slider input
  
  furthest.date <- reactive({
    min(spread_data()$d$normalized.index)
  }) # for date_period input
  nearest.date <- reactive({
    max(spread_data()$d$normalized.index)
  }) # for date_period input
  beginning.date <- reactive({
    beginning_int.normalized.index.for.plot <- unique(pure_spread_data()[normalized.index == nearest.date()]$int.normalized.index.for.plot)[1] - 250 # [1] to double check, 250 to make 1 business year back
    beginning_normalized.index <- unique(pure_spread_data()[int.normalized.index.for.plot == beginning_int.normalized.index.for.plot]$normalized.index)[1]
  })
  
  spread_data <- reactive({ # root data
    #setwd(inputs_dir)
    out <- readRDS(paste0(inputs_dir,"/",chosen_spread(), ".RDS"))
    #setwd(this.trader.dir)
    out
  })
  
  pure_spread_meta <- reactive({
    #setwd(inputs_dir)
    #setwd(spread_objects.dir)
    out <- readRDS(paste0(spread_objects.dir,"/",chosen_spread(), ".RDS"))
    #setwd(this.trader.dir)
    out
  })
  
  pure_spread_data <- reactive({
    spread_data()$d
  }) # purest form data when you choose the spread
  
  semi_pure_spread_data <- reactive({
    spread_data()$d[spread.yr %in% input$selected_years]
  }) # purest form data when you choose the spread
  
  
  #--- Meta 
  pure_spread_meta_yr <- reactive({
    spread_data()$yr
  })
  pure_spread_meta_exp <- reactive({
    spread_data()$exp
  })
  
  pure_spread_meta_yr_exp <- reactive({
    
    data.table(yr=spread_data()$yr$spread,exp=spread_data()$exp$spread)
    
  })
  
  pure_spread_meta <- reactive({
    #setwd(inputs_dir)
    #setwd(spread_objects.dir)
    out <- readRDS(paste0(spread_objects.dir,"/",chosen_spread(), ".RDS"))
    #setwd(this.trader.dir)
    out
  })
  #--- Meta Ends
  
  spread.data <- reactive({ # refined pure_spread_data()
    temp <- pure_spread_data()
    temp <- temp[spread.yr %in% input$selected_years]
    # add _1, _2, _3, ... to outright names; to avoid unique names
    outright_start_last_index <- which(names(temp) %in% c("index", "spread"))[1:2] + c(1, -1) # vector of size 2
    names(temp)[outright_start_last_index[1]:outright_start_last_index[2]] <- paste(names(temp)[outright_start_last_index[1]:outright_start_last_index[2]], (outright_start_last_index[1]:outright_start_last_index[2] - 1), sep = "_")
    
    if (input$period_method == "n.days.to.exp") {
      out <- temp[input$ndays_period[1] <= n.days.to.exp & n.days.to.exp <= input$ndays_period[2]]
    } else {
      out <- temp[toupper(input$date_period_begin) <= normalized.index & normalized.index <= toupper(input$date_period_end)]
    }
    out
  })
  
  spread.data_ndays_period <- reactive({ # refined pure_spread_data()
    input$ndays_period[1]:input$ndays_period[2]
  })
  
  
  moves <- reactive({
    d <- semi_pure_spread_data()
    temp <-
      lapply(input$selected_years, function(this.yr) {
        
        # this.yr <- "2006"
        this.yr.char <- as.character(this.yr)
        d.this.yr <- d[spread.yr == this.yr.char]
        d.this.yr.simple <- merge(data.table(n.days.to.exp = base.T.rng), d.this.yr[, c("n.days.to.exp", "spread")], all.x = TRUE, by = "n.days.to.exp")
        z <- d.this.yr.simple$spread
        zz <- outer(z, z, "-") # outer product -> outer subtraction
        y <- data.table(diff = zz[lower.tri(zz)]) # long table
        setNames(y, this.yr.char)
      })
    
    moves <- dplyr::bind_cols(temp)
    
    # cbind(base.left.tbl,moves)
  })
  
  seasonality_EF <- reactive({
    seasonality_ef(
      moves = moves(),
      entire.start = input$ndays_period[1],
      entire.end = input$ndays_period[2],
      lf = as.numeric(input$indicator_lookforward)
    )
  })
  
  seasonality_D <- reactive({
    seasonality_d(
      moves = moves(),
      entire.start = input$ndays_period[1],
      entire.end = input$ndays_period[2],
      lf = as.numeric(input$indicator_lookforward)
    )
  })
  
  seasonality_D_versatile_long <- reactive({
    y <-
      seasonality_d_versatile(
        moves = moves(),
        entire.start = input$ndays_period[1],
        entire.end = input$ndays_period[2]
      )
    
    data.table::melt(y, id.var = "n.days.to.exp")
  })
  
  
  #--- --------------------------- Part required for 'Relative Price' based Buy/Sell signal part -> not implemented for 'date' x-axis yet
  
  spread.data_wide <- reactive({ # for seasonality analysis; multiplier is applied at seasonality_tbl_filtered()
    
    data.table::dcast(spread.data()[, c("n.days.to.exp", "spread.yr", "spread"), with = FALSE], as.formula(paste0("n.days.to.exp", "~spread.yr")))
  })
  
  spread.data_wide_wo_ongoing_yr <- reactive({ # for seasonality analysis; multiplier is applied at seasonality_tbl_filtered()
    
    dw <- spread.data_wide()
    dw <- dw[, names(dw) != as.character(input$ongoing_yr), with = FALSE] # exclude ongoing spread
    # target_cols <- names(dw)[names(dw)!="n.days.to.exp"]
    # dw[, (target_cols) := lapply(.SD, function(x) {x * input$sMlt}), .SDcols = target_cols] # multiply spread by sMlt
    dw
  })
  
  
  spread.data_wide_wo_ongoing_yr_mltplied <- reactive({ # for seasonality analysis; multiplier is applied at seasonality_tbl_filtered()
    
    dw <- spread.data_wide()
    dw <- dw[, names(dw) != as.character(input$ongoing_yr), with = FALSE] # exclude ongoing spread
    target_cols <- names(dw)[names(dw) != "n.days.to.exp"]
    dw[, (target_cols) := lapply(.SD, function(x) {
      x * input$sMlt
    }), .SDcols = target_cols] # multiply spread by sMlt
    dw
  })
  
  
  
  today <- reactive({
    tail(pure_spread_data()[spread.yr == input$ongoing_yr]$n.days.to.exp, 1)
  })
  
  today_date <- reactive({
    original.ongoing.yr <- head(pure_spread_meta_yr()$spread[as.Date(pure_spread_meta_exp()$spread) > Sys.Date()], 1)
    
    format(as.Date(tail(pure_spread_data()[spread.yr == original.ongoing.yr]$index, 1)),"%d%b%y")
  })
  
  today_spread <- reactive({
    pure_spread_data()[spread.yr == input$ongoing_yr & n.days.to.exp == today()]$spread
  })
  
  today_in_date_range <- reactive({ # logical
    
    today() %in% spread.data_ndays_period()
  })
  
  seasonality.wide.tslice_dailymed <- reactive({
    dw <- spread.data_wide_wo_ongoing_yr_mltplied()
    # target_cols <- names(dw)[names(dw)!="n.days.to.exp"]
    # dw[, (target_cols) := lapply(.SD, function(x) {x * input$sMlt}), .SDcols = target_cols] # multiply spread by sMlt
    
    seasonality.wide <- data.table(
      n.days.to.exp = spread.data_ndays_period(),
      score = -Seasonality_tslice_dailymed(dw, M = "median")
    )
  })
  
  # seasonality.wide.data2 <- reactive({
  #
  #   dw <- spread.data_wide_wo_ongoing_yr_mltplied()
  #   # target_cols <- names(dw)[names(dw)!="n.days.to.exp"]
  #   # dw[, (target_cols) := lapply(.SD, function(x) {x * input$sMlt}), .SDcols = target_cols] # multiply spread by sMlt
  #
  #   seasonality.wide <- data.table(n.days.to.exp = input$ndays_period[1]:input$ndays_period[2],
  #                                  score = -Seasonality2(dw,M="median"))
  #
  # })
  seasonality.wide.roll_dailymed <- reactive({
    dw <- spread.data_wide_wo_ongoing_yr_mltplied()
    # target_cols <- names(dw)[names(dw)!="n.days.to.exp"]
    # dw[, (target_cols) := lapply(.SD, function(x) {x * input$sMlt}), .SDcols = target_cols] # multiply spread by sMlt
    
    
    
    seasonality.wide <- data.table(
      n.days.to.exp = spread.data_ndays_period(),
      score = -Seasonality_roll_dailymed(dw, lookforward = as.numeric(input$indicator_lookforward) + 1, M = "mean")
    )
  })
  
  
  seasonality.wide.roll_prob <- reactive({
    dw <- spread.data_wide_wo_ongoing_yr_mltplied()
    # target_cols <- names(dw)[names(dw)!="n.days.to.exp"]
    # dw[, (target_cols) := lapply(.SD, function(x) {x * input$sMlt}), .SDcols = target_cols] # multiply spread by sMlt
    score <- -Seasonality_roll_prob(dw, lookforward = as.numeric(input$indicator_lookforward) + 1)
    
    # non.na.score <- score[!is.na(score)]
    # smooth.non.na.score <- smooth(non.na.score)
    # smooth.score <- c(smooth.non.na.score,rep(NA_real_,(length(score)-length(non.na.score)) ))
    #
    seasonality.wide <- data.table(
      n.days.to.exp = spread.data_ndays_period(),
      score = score
    )
  })
  
  RR.wide <- reactive({
    dw <- spread.data_wide_wo_ongoing_yr()
    # target_cols <- names(dw)[names(dw)!="n.days.to.exp"]
    # dw[, (target_cols) := lapply(.SD, function(x) {x * input$sMlt}), .SDcols = target_cols] # multiply spread by sMlt
    score <- Range_Risk_roll(dw, lookforward = as.numeric(input$indicator_lookforward))
    
    # non.na.score <- score[!is.na(score)]
    # smooth.non.na.score <- smooth(non.na.score)
    # smooth.score <- c(smooth.non.na.score,rep(NA_real_,(length(score)-length(non.na.score)) ))
    #
    out <- cbind(data.table(n.days.to.exp = spread.data_ndays_period()), score)
  })
  
  scalpability.wide <- reactive({
    dw <- spread.data_wide_wo_ongoing_yr()
    # target_cols <- names(dw)[names(dw)!="n.days.to.exp"]
    # dw[, (target_cols) := lapply(.SD, function(x) {x * input$sMlt}), .SDcols = target_cols] # multiply spread by sMlt
    score <- Scalpability_HL(dw, lookforward = as.numeric(input$indicator_lookforward))
    
    # non.na.score <- score[!is.na(score)]
    # smooth.non.na.score <- smooth(non.na.score)
    # smooth.score <- c(smooth.non.na.score,rep(NA_real_,(length(score)-length(non.na.score)) ))
    #
    out <- cbind(data.table(n.days.to.exp = spread.data_ndays_period()), score)
  })
  
  
  stability.wide <- reactive({
    dw <- spread.data_wide_wo_ongoing_yr()
    # target_cols <- names(dw)[names(dw)!="n.days.to.exp"]
    # dw[, (target_cols) := lapply(.SD, function(x) {x * input$sMlt}), .SDcols = target_cols] # multiply spread by sMlt
    score <- Stability_SD(dw, lookforward = as.numeric(input$indicator_lookforward))
    
    # non.na.score <- score[!is.na(score)]
    # smooth.non.na.score <- smooth(non.na.score)
    # smooth.score <- c(smooth.non.na.score,rep(NA_real_,(length(score)-length(non.na.score)) ))
    #
    out <- cbind(data.table(n.days.to.exp = spread.data_ndays_period()), score)
  })
  
  # risk.summary.today <- reactive({
  #
  #   dw <- spread.data_wide_wo_ongoing_yr()
  #   # target_cols <- names(dw)[names(dw)!="n.days.to.exp"]
  #   # dw[, (target_cols) := lapply(.SD, function(x) {x * input$sMlt}), .SDcols = target_cols] # multiply spread by sMlt
  #   score = Risk_summary_today(dw,today(),lookforward_vec=c(5,10,15)  )
  #
  #   # non.na.score <- score[!is.na(score)]
  #   # smooth.non.na.score <- smooth(non.na.score)
  #   # smooth.score <- c(smooth.non.na.score,rep(NA_real_,(length(score)-length(non.na.score)) ))
  #   #
  # score
  # })
  #
  # risk.all <- reactive({
  #
  #   dw <- spread.data_wide_wo_ongoing_yr()
  #   # target_cols <- names(dw)[names(dw)!="n.days.to.exp"]
  #   # dw[, (target_cols) := lapply(.SD, function(x) {x * input$sMlt}), .SDcols = target_cols] # multiply spread by sMlt
  #   score = Risk_all(dw,today(),lookforward= as.numeric(input$indicator_lookforward))
  #
  #   # non.na.score <- score[!is.na(score)]
  #   # smooth.non.na.score <- smooth(non.na.score)
  #   # smooth.score <- c(smooth.non.na.score,rep(NA_real_,(length(score)-length(non.na.score)) ))
  #   #
  #   score
  # })
  
  RP.data <- reactive({ # relative price data
    
    
    unexpired.yr.data <- spread.data()[spread.yr == input$ongoing_yr]
    target.period <- unexpired.yr.data$n.days.to.exp
    
    
    scores <- rep(NA_real_, length(target.period))
    
    for (i in seq(length(target.period))) {
      today <- target.period[i]
      today.spread <- unexpired.yr.data[n.days.to.exp == today]$spread
      # print(today)
      
      # d <- sd[n.days.to.exp> today & n.days.to.exp<= d.end] # > today !!
      #
      # dw <- data.table::dcast(d[,c("n.days.to.exp","spread.yr","spread"),with=FALSE],as.formula(paste0("n.days.to.exp","~spread.yr"))) # wide format
      # dw <- dw[,names(dw)!=as.character(on.going.yr),with=FALSE] #exclude ongoing spread
      #
      #
      
      if (today < input$ndays_period[2]) {
        dw <- spread.data_wide()[n.days.to.exp > today]
        dw <- dw[, names(dw) != as.character(input$ongoing_yr), with = FALSE] # exclude ongoing spread
        
        scores[i] <- RP_Score(dw[, -1], today, today.spread)
      } else {
        scores[i] <- NA
      }
    }
    
    out <- merge(data.table(n.days.to.exp = unique(spread.data()$n.days.to.exp)), data.table(n.days.to.exp = target.period, scores = scores), all = TRUE)
    
    reshape2::melt(out,
                   id.vars = "n.days.to.exp", variable.name = "variable",
                   value.name = "scores"
    ) # make it long
  })
  
  
  # latest.unexpired.yr <- reactive({  #if 2018, 2019, ... are unexpired, then 2018 chosen
  #   unexpired.tbl <- pure_spread_data()[,.(min.nday = max(n.days.to.exp)!=0),by=spread.yr] #if max n.days.to.exp is not 0, then TRUE
  #   #unexpired.tbl[which(unexpired.tbl$min.nday==TRUE)[1],]$spread.yr
  #   unexpired.tbl[min.nday==TRUE & spread.yr >= year(Sys.Date())]$spread.yr[1]
  # })
  
  
  #--- --------------------------- Part required for 'Relative Price' based Buy/Sell signal part end
  
  
  spread.data.for.technical.analysis <- reactive({ # for technical analysis(=rolling analysis + regression) for multiple spread yrs
    temp <- pure_spread_data()
    temp <- temp[spread.yr %in% input$selected_years]
    
    temp[temp[[input$period_method]] >= input$technical_analysis_start_day]
  })
  
  
  svm_untuned_data <- reactive({ #-------------- SVM Regression: Un-Tuned
    
    X <- ifelse(input$period_method == "n.days.to.exp", "n.days.to.exp", "int.normalized.index.for.plot")
    untuned_svm_model <- svm(as.formula(paste0("spread~", X)), data = spread.data.for.technical.analysis())
    untuned_svm_model_pred <- predict(untuned_svm_model, spread.data.for.technical.analysis())
    
    out <- setNames(as.data.table(cbind(sort(unique(spread.data.for.technical.analysis()[[X]])), unique(untuned_svm_model_pred))), c(X, "SVM"))
    out_melt <- data.table::melt(out, X)
    
    if (input$period_method == "n.days.to.exp") {
      out_melt$name <- out_melt[[input$period_method]]
      out_melt
    } else {
      to.be.merged <- unique(spread.data.for.technical.analysis()[, c("int.normalized.index.for.plot", "normalized.index"), with = FALSE])
      out_melt <- merge(out_melt, to.be.merged)[order(variable, int.normalized.index.for.plot)]
      out_melt$name <- out_melt[["normalized.index"]]
    }
    
    out_melt
  })
  
  loess_data <- reactive({ #-------------- LOESS regression
    
    X <- ifelse(input$period_method == "n.days.to.exp", "n.days.to.exp", "int.normalized.index.for.plot")
    
    loess_model <- loess(as.formula(paste0("spread~", X)), spread.data.for.technical.analysis(), span = 0.75, degree = 2)
    loess_model_pred <- loess_model$fitted
    
    out <- setNames(as.data.table(cbind(sort(unique(spread.data.for.technical.analysis()[[X]])), unique(loess_model_pred))), c(X, "LOESS"))
    out_melt <- data.table::melt(out, X)
    
    if (input$period_method == "n.days.to.exp") {
      out_melt$name <- out_melt[[input$period_method]]
      out_melt
    } else {
      to.be.merged <- unique(spread.data.for.technical.analysis()[, c("int.normalized.index.for.plot", "normalized.index"), with = FALSE])
      out_melt <- merge(out_melt, to.be.merged)[order(variable, int.normalized.index.for.plot)]
      out_melt$name <- out_melt[["normalized.index"]]
    }
    
    out_melt
  })
  
  rolling_analysis_data <- reactive({ #-------------- rolling analysis (moving average, moving std)
    
    roll_data <- rollAnalysis(spread.data.for.technical.analysis(), input$n_std, input$roll_window_width, looking = input$looking, period_method = input$period_method)
    roll_data
  })
  
  
  
  summary.spread.data <- reactive({ # yearly
    this.spread.data <- spread.data()
    summary.table <- rbindlist(lapply(sort(unique(this.spread.data$spread.yr), decreasing = TRUE), function(this.spread.yr) {
      getSummary(this.spread.data[spread.yr == this.spread.yr]$spread, TRUE, summary_round_by)
    }))
    summary.table <- cbind(data.table(Yr = sort(unique(this.spread.data$spread.yr), decreasing = TRUE)), summary.table)[, c("Yr", "HL", "MIN", "MED", "MEAN", "MODE", "MAX", "SD")]
    out <- rbind(round(summary.table[, c("HL", "MIN", "MED", "MEAN", "MODE", "MAX", "SD")][, lapply(.SD, mean)], summary_round_by), summary.table, fill = TRUE) # add average info on top
    out[, c("Yr", "HL", "MIN", "MED", "MEAN", "MODE", "MAX", "SD")]
  })
  
  
  summary.spread.data2 <- reactive({ # as a group
    this.spread.data <- spread.data()
    summary.table <- getSummary(this.spread.data$spread, FALSE, summary_round_by)
  })
  
  summary.spread.this.year.data <- reactive({
    #  tryCatch({
    this.spread.data <- spread.this.year.data()
    
    summary.table <- rbindlist(lapply(sort(unique(this.spread.data$spread.yr), decreasing = TRUE), function(this.spread.yr) {
      getSummary(this.spread.data[spread.yr == this.spread.yr]$spread, TRUE, summary_round_by)
    }))
    summary.table <- cbind(data.table(Yr = sort(unique(this.spread.data$spread.yr), decreasing = TRUE)), summary.table)
    #  },error=function(e){summary.table <- data.table()    })
  })
  
  
  ################################################### renderUI PART #########################################
  
  # outputOptions(output, "ui_chart_height", suspendWhenHidden = FALSE)
  
  
  
  
  output$ui_setting_dropdown <-
    renderUI({
      #if (check$ready_first == TRUE) {
      dropdown( # width="600px",right=TRUE,
        fluidRow(column(width = 12, align = "center", htmlOutput("guidance_doc"))), br(),
        
        
        fluidRow(column(
          width = 12, align = "center", # uiOutput("ui_chart_height")
          numericInput("chart_height", label = "Chart Height", value = 700, min = 0, max = NA, step = 50, width = "100%")
        )),
        fluidRow(column(width = 12, align = "center", selectInput("theme",
                                                                  label = "Theme", width = "100%",
                                                                  choices = c(
                                                                    "default", "fivethirtyeight", "economist",
                                                                    "darkunica", "gridlight", "sandsignika", "null"
                                                                  )
        ))),
        
        fluidRow(
          column(width = 12, align = "center", numericInput("sMlt", label = "Mltplier", value = 1, min = NA, max = NA, step = 1, width = "70%")) # spread multiplier
        ), # sMlt(pure_spread_meta())
        # fluidRow(
        #   column(width = 12,align="center",  textInput("text_practice", label = "hi",width="100%")) # spread multiplier
        # ), #sMlt(pure_spread_meta())
        circle = TRUE, status = "danger",
        icon = icon("gear"), size = "sm", style = "unite"
        # animate = animateOptions(
        #   enter = animations$fading_entrances$fadeInRightBig,
        #   exit = animations$fading_exits$fadeOutRightBig
        # )
        
        # tooltip = tooltipOptions(title = "")
      )
      #}
    })
  
  output$ui_today_text <-
    renderUI({
      #  if (check$ready_first == TRUE) {
      textOutput("today_text")
      #  }
    })
  
  output$today_text <- renderText(paste0("Data: ",today_date()))
  
  # ========================================================= Google sheet related ends
  
  output$ui_note <- renderUI({
    
    absolutePanel(
      left = "0%", top = "0%", right = "0%", width = "100%", fixed = TRUE, draggable = FALSE, style = "z-index:1000;",
      # wellPanel(style = "opacity: 1",
      # googleAuthUI("gauth_login")
      div(
        style = "padding: 0.1px; border-bottom: 1px solid #CCC; background: #FFFFEE;", # 
        fluidRow(
          column(width = 1,offset=1, align = "center", selectInput("type_gs", label = "GoogleSheet", choices = c("master"))),
          #column(width = 1, align = "center", selectInput("type_ws", label = "WorkSheet", choices = c("specific","general"))),
          column(offset=1,width = 3, textInput("note", "note",width='100%')),
          column(width = 1, align="center",br(),uiOutput("ui_go_button_write")),
          column(width=1, align="center",  br(),  tags$a(
            href = "https://docs.google.com/spreadsheets/d/1qvnyQA2GM8Hxs9qSpRA-KVU2A5t9IeeOaEmQwU3N2lM/edit?usp=sharing",
            "Master Sheet", target = "_blank"), br(), tags$a(href = "https://teamknuepfer3.slack.com", "Slack, ", target = "_blank"),
            tags$a(href = "http://10.10.100.48/phpPgAdmin/","SQL db", target = "_blank")
            
          ),
          column(offset=0, width = 1, align="center",br(),awesomeCheckbox("remind",label="remind",value = FALSE,status="success")),
          column(offset = 0, width = 1, uiOutput("ui_remind_on")),column(width=2,uiOutput("ui_remind_to"))
        )
      )
      
      
    )
    
  })
  
  output$ui_go_button_write <- renderUI({
    
    actionBttn(
      inputId = "go_button_write", label = "Write", icon("paper-plane"),
      # style="color: #fff; background-color: #9988f7; border-color: #9988f7"
      style = "unite", color = "success", size = "sm")
    
    
  })
  
  output$ui_remind_on <- renderUI({
    if(check$remind_first){
      
      if(input$remind){
        dateInput("remind_on","on",value=Sys.Date())
        
      }
    }
  })
  output$ui_remind_to <- renderUI({
    if(check$remind_first){
      
      if(input$remind){
        if(input$type_gs=="master"){
          
          selectInput("remind_to","to",choices=all.trader.names,selected=c("AG","Joe","Micah"),multiple=TRUE)
          
        }else{
          selectInput("remind_to","to",choices=all.trader.names,selected=c("AG","Joe","Micah"),multiple=TRUE)
          
        }
        
      }
      
    }
  })
  
  note_tbl <- reactive({ # the actual input to googlesheet and slack
    
    
    if(input$remind){
      # note.tbl <- data.table(name=this.trader,
      #                        timestamp=Sys.time(),
      #                        note= input$note, 
      #                        link=fa_note_link(),
      #                        profile=chosen_spread(),
      #                        story="",
      #                        #reminder=ifelse(input$remind,"T",""),
      #                        remind_on = input$remind_on , 
      #                        remind_to =  paste(input$remind_to ,collapse="|")
      # )
      
      this.time <- Sys.time()
      
      note.tbl <- data.table(name=this.trader,
                             timestamp=this.time,
                             note= input$note, 
                             link=fa_note_link(),
                             profile=chosen_spread(),
                             background="",
                             #reminder=ifelse(input$remind,"T",""),
                             remind_on = input$remind_on , 
                             remind_to =  paste(input$remind_to ,collapse="|"),
                             'reminded?' = "",
                             ongoing_yr = input$ongoing_yr,
                             'T' = as.integer(ongoing_yr_to_T()[[as.character(input$ongoing_yr)]]),
                             expiration = pure_spread_meta_yr_exp()[yr==input$ongoing_yr]$exp,
                             product = chosen_product(),
                             relationship= chosen_relationship(),
                             interval= chosen_interval(),
                             month=chosen_month(),
                             id = paste(this.trader,this.time,sep="_")
      )
      
    }else{ #no reminder
      this.time <- Sys.time()
      note.tbl <- data.table(name=this.trader,
                             timestamp=Sys.time(),
                             note= input$note, 
                             link=fa_note_link(),
                             profile=chosen_spread(),
                             background="",
                             #reminder=ifelse(input$remind,"T",""),
                             remind_on = "" , 
                             remind_to =  "",
                             'reminded?' = "",
                             ongoing_yr = input$ongoing_yr,
                             'T' = as.integer(ongoing_yr_to_T()[[as.character(input$ongoing_yr)]]),
                             expiration = pure_spread_meta_yr_exp()[yr==input$ongoing_yr]$exp,
                             product = chosen_product(),
                             relationship= chosen_relationship(),
                             interval= chosen_interval(),
                             month=chosen_month(),
                             id = paste(this.trader,this.time,sep="_")
      )
    }
    
    
    note.tbl
  })
  
  
  
  
  
  slack_message <- reactive({
    
    paste0("[",note_tbl()$profile,"] ",note_tbl()$name," notes ", "'",note_tbl()$note,"'"," \n\n@ ", note_tbl()$link )
    
  })
  
  
  observeEvent(input$go_button_write,{
    withProgress(message = 'Writing now', value = 0, {
      
      dbWriteTable(pool,  c("public", "whole"), 
                   value = note_tbl(),append = TRUE, row.names = FALSE)
      
      incProgress(1/4, detail = paste("SQL database"))
      
      if(input$type_gs=="master"){
        gs_title <-     paste0("master_sheet")
        gs <- gs_key(gs_ls(gs_title)$sheet_key[1]) #first identify sheet_key of sheet_title master_sheet. Then call google sheet object by sheet_key
      }else{
        
      }
      
      incProgress(2/4, detail = paste("Googlesheet loaded"))
      
      
      target.ws <- tabmap[spread_under_type==input$spread_part1]$tab
      gs %>% gs_add_row(ws=target.ws,input= note_tbl() )
      
      incProgress(3/4, detail = paste("Written"))
      
      slackit(channel="#master",message=slack_message())
      incProgress(3/3, detail = paste(""))
      
    })
    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "All in order",
      type = "success"
    )
    
  })
  
  # observeEvent(input$go_button_write,{
  #   
  #   slackit(channel="#master",message=slack_message())
  #   
  #   
  # })
  
  
  
  output$ui_go_button <-
    renderUI({
      #if (check$ready_first == TRUE) {
      # actionButton(
      #   inputId = "go_button", label = "Go", icon("paper-plane"),
      #   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      # )
      actionBttn(
        inputId = "go_button", label = "Go", icon("paper-plane"),
        # style="color: #fff; background-color: #9988f7; border-color: #9988f7"
        style = "jelly", color = "primary", size = "sm"
      )
      #}
    })
  
  output$spread_part1_ui <-
    renderUI({
      condition <-!is.na(profile_session_input()) # if there's any profile session input
      session.input.part1 <- spreadNameBr(profile_session_input())$spread_under_type # it is "NA NA" if not available
      
      
      if(condition){
        choices <- session.input.part1
        selected <- choices
      }else{
        choices <- as.character(unique(spread.names.br$spread_under_type))
        selected <- choices[1]
      }
      selectizeInput(
        "spread_part1",
        label = "", width = "100%",
        choices = choices,
        selected = selected
      ) # , tags$style(type = "text/css", ".selectize-dropdown-content {max-height: 850px; }")
    })
  
  
  
  
  
  output$spread_part2_ui <-
    renderUI({
      
      condition <-!is.na(profile_session_input())
      session.input.part2 <- as.character(spreadNameBr(profile_session_input())$int) # it is na if not available
      
      if(condition){
        choices <- session.input.part2
        selected <- choices
      }else{
        choices <- as.character(unique(spread.names.br[spread_under_type == input$spread_part1]$int))
        selected <- choices[1]
      }
      
      
      selectInput("spread_part2",
                  label = "", width = "100%",
                  choices = choices, 
                  selected = selected
      )
    })
  
  output$spread_part3_ui <-
    renderUI({
      
      condition <-!is.na(profile_session_input())
      session.input.part3 <-  as.character(spreadNameBr(profile_session_input())$month) # it is na if not available
      
      if(condition){
        choices <- session.input.part3
        selected <- choices
      }else{
        choices <- as.character(unique(spread.names.br[spread_under_type == input$spread_part1 & int == input$spread_part2]$month))
        selected <- choices[1]
      }
      
      selectInput("spread_part3",
                  label = "", width = "100%",
                  choices = choices, 
                  selected = selected
      )
    })
  
  output$ui_go_button_moving_analysis <-
    renderUI({
      #if (check$ready_first == TRUE) {
      # actionButton(
      #   inputId = "go_button_moving_analysis", label = "Go", icon("paper-plane"),
      #   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      # )
      actionBttn(
        inputId = "go_button_moving_analysis", label = "Go", icon("paper-plane"),
        # style="color: #fff; background-color: #9988f7; border-color: #9988f7"
        style = "material-circle", color = "primary", size = "sm"
      )
      #}
    })
  
  output$ui_select_period <-
    renderUI({
      #if (check$ready_first == TRUE) {
      #  input$ready_button
      #  isolate({
      if (input$period_method == "n.days.to.exp") {
        # dateRangeInput("date_period", label="",start=seq(nearest.date(), length=2,by="-1 years")[2], end=nearest.date(), min = furthest.date(), max = nearest.date())
        
        min.input <- -500 #max(furthest.day.from.exp(), -500)
        
        # condition <- sum(!is.na(begin.end_session_input())) > 0 # if there is some begin & end input
        # begin <- ifelse(condition & !check$go_first, begin.end_session_input()[1], -253) # if it's not empty and go button was not clicked for the first
        # end <- ifelse(condition & !check$go_first, begin.end_session_input()[2], 0)
        
        condition <- sum(!is.na(begin.end_session_input())) > 0 # if there is some begin & end input
        begin <- ifelse(condition , begin.end_session_input()[1], -253) # if it's not empty and go button was not clicked for the first
        end <- ifelse(condition, begin.end_session_input()[2], 0)
        
        sliderInput("ndays_period", "", value = c(begin, end), width = "100%", min = min.input, max = 0, step = 1)
      } else {
        L <- vector("list", 2) # create NULL list
        
        
        L[[1]] <- textInput("date_period_begin", label = "begin", value = beginning.date(), width = "10%")
        L[[2]] <- textInput("date_period_end", label = "end", value = nearest.date(), width = "10%")
        return(L)
      }
      #  })
      #}
    })
  
  # output$ui_from_2015 <- renderUI({
  #   #if (check$ready_first == TRUE) {
  #   #  input$ready_button
  #   #  isolate({
  #       #condition <- sum(!is.na(chosen.yrs_session_input())) > 0 & !check$go_first # if there is some input, and there was go_first is false
  #       #original.candidate <- as.character(possible.spread.years())
  #       #if(condition){selected <- chosen.yrs_session_input()}else{selected <- original.candidate}
  #       awesomeCheckbox("from_2015",label="Recent",value = FALSE,status="info")
  #   #  })
  #   #}
  # })
  # output$ui_deselect_all <- renderUI({
  #   #if (check$ready_first == TRUE) {
  #   #  input$ready_button
  #   #  isolate({
  #   #condition <- sum(!is.na(chosen.yrs_session_input())) > 0 & !check$go_first # if there is some input, and there was go_first is false
  #   #original.candidate <- as.character(possible.spread.years())
  #   #if(condition){selected <- chosen.yrs_session_input()}else{selected <- original.candidate}
  #   awesomeCheckbox("deselect_all",label="None",value = FALSE,status="danger")
  #   #  })
  #   #}
  # })
  
  # output$test_text <- renderText({
  #   as.integer(ongoing_yr_to_T()[[1]])
  #     #is.object(possible.spread.years())
  #       #!is.na(profile_session_input())
  #   #all(is.character(input$selected_years))
  #   #check$ready_first
  #       #profile_session_input()
  #       #spreadNameBr(profile_session_input())$spread_under_type
  # 
  # 
  # })
  
  # output$ui_theme <-
  #   renderUI({
  #     if (check$ready_first == TRUE) {
  #       selectInput("theme",
  #         label = "Theme", width = "80%",
  #         choices = c(
  #           "default", "fivethirtyeight", "economist",
  #           "darkunica", "gridlight", "sandsignika", "null"
  #         )
  #       )
  #     }
  #   })
  
  # output$ui_chart_height <-
  #   renderUI({
  #     if (check$ready_first == TRUE) {
  #       numericInput("chart_height", label = "Chart Height", value = 700, min = 0, max = NA, step = 50, width = "80%")
  #     }
  #   })
  
  
  
  output$ui_select_years <- renderUI({
    
    #reactiveTimer(20000)
    #possible.spread.years()
    #if (check$ready_first == TRUE) {
    #input$ready_button
    # # input$from_2015
    # # input$deselect_all
    #input$select_yrs_refined
    #isolate({
    #condition <- sum(!is.na(chosen.yrs_session_input())) > 0 & !check$go_first # if there is some input, and there was go_first is false
    
    #default.from.spread.profile <- spread.names.br[profile==chosen_spread()]$begin.yr:spread.names.br[profile==chosen_spread()]$end.yr
    condition <- sum(!is.na(chosen.yrs_session_input())) > 0 
    if(condition){default <- chosen.yrs_session_input()}else{default <- as.character(possible.spread.years())}
    #if(condition){default <- chosen.yrs_session_input()}else{default <-default.from.spread.profile}
    
    #if(input$from_2015){ selected <- possible.spread.years()[possible.spread.years() >=2015]}
    #if(input$deselect_all){ selected <- NULL}
    
    if(input$select_yrs_refined=="O"){
      selected <- default
    }else if(input$select_yrs_refined=="15+"){
      selected <- as.character(possible.spread.years()[possible.spread.years() >=2015])
    }else if(input$select_yrs_refined=="X"){
      
      selected <- NULL
    }else{
      
    }
    checkboxGroupInput("selected_years", "",
                       choiceNames = as.character(possible.spread.years()),
                       choiceValues =possible.spread.years(),
                       selected = selected,
                       inline = TRUE
    )
    #})
    # }
    
  })
  
  
  output$ui_ongoing_yr <- renderUI({
    #if (check$ready_first == TRUE) {
    #  input$ready_button
    #  isolate({
    #invalidateLater(10000, session)
    #Sys.sleep(3)
    condition <- !is.na(ongoing.yr_session_input()) 
    
    if(condition){
      choices <- ongoing.yr_session_input()
      selected <- choices
    }else{ 
      
      choices <- rev(possible.spread.years())
      selected <- head(unexpired_yrs(), 1)
    }
    
    selectInput("ongoing_yr", "on.going yr",
                choices = choices, width = "80%",
                selected = selected
    )
    #  })
    #}
  })
  
  
  
  # output$ui_sMlt <- renderUI({
  #   if (check$ready_first == TRUE) {
  #     input$ready_button
  #     isolate({
  #       numericInput("sMlt", label = "Mltplier", value = sMlt(pure_spread_meta()), min = NA, max = NA, step = 1, width = "70%")
  #     })
  #   }
  # })
  
  #------------- meta+data tab begins
  output$ui_meta <- renderUI({
    if (check$go_first == TRUE) {
      dataTableOutput("meta")
    }
  })
  output$ui_meta_yr <- renderUI({
    if (check$go_first == TRUE) {
      dataTableOutput("meta_yr")
    }
  })
  output$ui_meta_exp <- renderUI({
    if (check$go_first == TRUE) {
      dataTableOutput("meta_exp")
    }
  })
  #------------- meta tab ends
  #------------- data tab begins
  output$ui_pure_spread_data_table <- renderUI({
    if (check$go_first == TRUE) {
      dataTableOutput("pure_spread_data_table")
    }
  })
  #------------- data tab ends
  
  output$ui_indicator_lookforward <-
    renderUI({
      #if (check$ready_first == TRUE) {
      selectInput(
        "indicator_lookforward",
        label = h6("Look Forward"), width = "70%", selected = "10",
        choices = c("5" = "5", "10" = "10", "15" = "15", "20" = "20", "25" = "25", "50" = "50", "100" = "100")
      )
      # }
    })
  
  output$ui_note_read <-
    renderUI({
      #if (check$ready_first == TRUE) {
      # checkboxInput("relative_price", label="Relative Price", value=FALSE)
      
      
      # shinyWidgets::switchInput(
      #   inputId = "note_read",
      #   label = "Note",
      #   #right = TRUE,
      #   value = FALSE,
      #   onStatus = "success"
      # )
      
      shinyWidgets::materialSwitch(
        inputId = "note_read",
        label = "Note",
        right = TRUE,
        value = TRUE,
        status = "success"
      )
      
      # }
    })
  
  output$ui_relative_price <-
    renderUI({
      #if (check$ready_first == TRUE) {
      # checkboxInput("relative_price", label="Relative Price", value=FALSE)
      
      shinyWidgets::materialSwitch(
        inputId = "relative_price",
        label = "RP",
        right = TRUE,
        value = TRUE,
        status = "info"
      )
      # }
    })
  
  output$ui_seasonality <-
    renderUI({
      #if (check$ready_first == TRUE) {
      # checkboxInput("seasonality", label="Seasonality", value=FALSE)
      shinyWidgets::materialSwitch(
        inputId = "seasonality",
        label = "Seasonal",
        right = TRUE,
        value = TRUE,
        status = "danger"
      )
      #}
    })
  
  
  
  
  
  
  output$ui_range.risk <-
    renderUI({
      #if (check$ready_first == TRUE) {
      # checkboxInput("seasonality", label="Seasonality", value=FALSE)
      shinyWidgets::materialSwitch(
        inputId = "range.risk",
        label = "Range",
        right = TRUE,
        value = TRUE,
        status = "default"
      )
      #}
    })
  
  output$ui_scalpability <-
    renderUI({
      #if (check$ready_first == TRUE) {
      # checkboxInput("seasonality", label="Seasonality", value=FALSE)
      shinyWidgets::materialSwitch(
        inputId = "scalpability",
        label = "Scalp",
        right = TRUE,
        value = TRUE,
        status = "success"
      )
      #}
    })
  
  output$ui_stability <-
    renderUI({
      #if (check$ready_first == TRUE) {
      # checkboxInput("seasonality", label="Seasonality", value=FALSE)
      shinyWidgets::materialSwitch(
        inputId = "stability",
        label = "Stable",
        right = TRUE,
        value = TRUE,
        status = "primary"
      )
      #}
    })
  
  # output$ui_indicator_lookforward <-
  #   renderUI({
  #     if(check$ready_first==TRUE){
  #
  #       if(length(input$range.risk)!=0 ){
  #
  #         if(input$range.risk==TRUE){
  #
  #           selectInput(
  #             "indicator_lookforward", label = h6("look forward"), width = "70%",selected="10",
  #             choices = c("5"="5","10"="10","15"="15","25"="25","50"="50","100"="100")
  #           )
  #
  #         }
  #       }
  #     }
  #   })
  
  # output$ui_sd_mult_gr <-
  #   renderUI({
  #     if(check$ready_first==TRUE){
  #       numericInput("sd_mult_gr", label="N * SD (hist)", value=1, min = 0, max = NA, step = .2,width="100%")
  #       #sliderInput("Nstd","Period", value=c(1),width='20%', min = .01 , max = 10, step = .2)
  #     }
  #   })
  
  output$ui_check_moving_analysis <-
    renderUI({
      #if (check$ready_first == TRUE) {
      checkboxInput("check_moving_analysis", "Bollinger", value = FALSE)
      #}
    })
  
  
  output$ui_technical_analysis_start_day <-
    renderUI({
      #if (check$ready_first == TRUE) {
      #  input$ready_button
      #  isolate({
      if (input$period_method == "n.days.to.exp") {
        numericInput("technical_analysis_start_day", label = "Fit line from", value = -150, min = NA, max = -1, step = 1, width = "100%")
      } else {
        textInput("technical_analysis_start_day", label = "Fit line from", value = beginning.date(), width = "100%")
      }
      #  })
      # }
    })
  
  output$ui_roll_window_width <-
    renderUI({
      if (check$roll_first == TRUE) {
        if (input$check_moving_analysis == TRUE) {
          numericInput("roll_window_width", label = "window width", value = 10, min = 1, max = NA, step = 1, width = "100%")
        }
      }
    })
  output$ui_roll_N_std <-
    renderUI({
      if (check$roll_first == TRUE) {
        if (input$check_moving_analysis == TRUE) {
          numericInput("n_std", "n * std", value = 1, min = 0.1, step = .2, width = "100%")
        }
      }
    })
  output$ui_roll_looking <-
    renderUI({
      if (check$roll_first == TRUE) {
        if (input$check_moving_analysis == TRUE) {
          radioButtons("looking", "looking", choices = c("backward", "centered", "forward"), selected = "centered")
        }
      }
    })
  
  output$ui_help_regression_text <-
    renderUI({
      #if (check$ready_first == TRUE) {
      helpText("regression")
      #}
    })
  
  
  output$ui_loess <-
    renderUI({
      #if (check$ready_first == TRUE) {
      checkboxInput("check_loess", "LOESS", value = FALSE)
      # }
    })
  
  output$ui_svm <-
    renderUI({
      # if (check$ready_first == TRUE) {
      checkboxInput("check_svm", "SVM", value = FALSE)
      #}
    })
  
  
  
  output$ui1 <- renderUI({
    if (check$go_first == TRUE) {
      highchartOutput("hcontainer", height = input$chart_height)
    }
  })
  output$ui_RPchart <- renderUI({
    if (check$go_first == TRUE & input$period_method == "n.days.to.exp") {
      
      # if(input$relative_price==TRUE){
      highchartOutput("RPchart", height = input$chart_height * 2 / 3)
      
      # }else{}
    }
  })
  
  #
  observe({
    # hide("RPchart")
    toggle(id = "ui_RPchart", anim = TRUE, animType = "slide", time = .7, condition = (input$relative_price))
  })
  
  
  
  output$ui_Schart_tslice_dailymed <- renderUI({
    if (check$go_first == TRUE & input$period_method == "n.days.to.exp") {
      
      # if(input$seasonality==TRUE){
      highchartOutput("Schart_tslice_dailymed", height = input$chart_height * 2 / 3)
      
      # }
    }
  })
  
  
  output$ui_Schart_roll_dailymed <- renderUI({
    if (check$go_first == TRUE & input$period_method == "n.days.to.exp") {
      
      # if(input$seasonality==TRUE){
      highchartOutput("Schart_roll_dailymed", height = input$chart_height * 2 / 3)
      
      # }
    }
  })
  
  output$ui_Schart_roll_prob <- renderUI({
    if (check$go_first == TRUE & input$period_method == "n.days.to.exp") {
      
      # if(input$seasonality==TRUE){
      highchartOutput("Schart_roll_prob", height = input$chart_height * 2 / 3)
      
      # }
    }
  })
  
  
  output$ui_Schart_D <- renderUI({
    if (check$go_first == TRUE & input$period_method == "n.days.to.exp") {
      
      # if(input$seasonality==TRUE){
      highchartOutput("Schart_D", height = input$chart_height * 2 / 3)
      
      # }
    }
  })
  
  output$ui_Schart_D_versatile <- renderUI({
    if (check$go_first == TRUE & input$period_method == "n.days.to.exp") {
      
      # if(input$seasonality==TRUE){
      highchartOutput("Schart_D_versatile", height = input$chart_height * 2 / 3)
      
      # }
    }
  })
  
  output$ui_Schart_E <- renderUI({
    if (check$go_first == TRUE & input$period_method == "n.days.to.exp") {
      
      # if(input$seasonality==TRUE){
      highchartOutput("Schart_E", height = input$chart_height * 2 / 3)
      
      # }
    }
  })
  output$ui_Schart_F <- renderUI({
    if (check$go_first == TRUE & input$period_method == "n.days.to.exp") {
      
      # if(input$seasonality==TRUE){
      highchartOutput("Schart_F", height = input$chart_height * 2 / 3)
      
      # }
    }
  })
  
  
  observe({
    # hide("RPchart")
    toggle(id = "ui_Schart_tslice_dailymed", anim = TRUE, animType = "slide", time = .7, condition = (input$seasonality))
    toggle(id = "ui_Schart_roll_dailymed", anim = TRUE, animType = "slide", time = .7, condition = (input$seasonality))
    toggle(id = "ui_Schart_roll_prob", anim = TRUE, animType = "slide", time = .7, condition = (input$seasonality))
    toggle(id = "ui_Schart_D", anim = TRUE, animType = "slide", time = .7, condition = (input$seasonality))
    toggle(id = "ui_Schart_D_versatile", anim = TRUE, animType = "slide", time = .7, condition = (input$seasonality))
    toggle(id = "ui_Schart_E", anim = TRUE, animType = "slide", time = .7, condition = (input$seasonality))
    toggle(id = "ui_Schart_F", anim = TRUE, animType = "slide", time = .7, condition = (input$seasonality))
  })
  
  output$ui_RRchart_roll <- renderUI({
    if (check$go_first == TRUE & input$period_method == "n.days.to.exp") {
      
      # if(input$seasonality==TRUE){
      highchartOutput("RRchart_roll", height = input$chart_height * 2 / 3)
      
      # }
    }
  })
  
  # output$ui_Risk_circle <- renderUI({
  #   if(check$go_first==TRUE & input$period_method=="n.days.to.exp"){
  #
  #     #if(input$seasonality==TRUE){
  #     highchartOutput("Risk_circle",height = input$chart_height * 2/3)
  #
  #     #}
  #   }
  # })
  # output$ui_Risk_circle_all <- renderUI({
  #   if(check$go_first==TRUE & input$period_method=="n.days.to.exp"){
  #
  #     #if(input$seasonality==TRUE){
  #     highchartOutput("Risk_circle_all",height = input$chart_height* 2/3  )
  #
  #     #}
  #   }
  # })
  
  
  observe({
    # hide("RPchart")
    toggle(id = "ui_RRchart_roll", anim = TRUE, animType = "slide", time = .7, condition = (input$range.risk))
    # toggle(id="ui_Risk_circle",anim=TRUE,animType = "slide",time=.7,condition = (input$range.risk) )
    # toggle(id="ui_Risk_circle_all",anim=TRUE,animType = "slide",time=.7,condition = (input$range.risk) )
  })
  
  
  output$ui_SCALPchart <- renderUI({
    if (check$go_first == TRUE & input$period_method == "n.days.to.exp") {
      
      # if(input$seasonality==TRUE){
      highchartOutput("SCALPchart", height = input$chart_height * 2 / 3)
      
      # }
    }
  })
  
  observe({
    toggle(id = "ui_SCALPchart", anim = TRUE, animType = "slide", time = .7, condition = (input$scalpability))
  })
  
  output$ui_STABLEchart <- renderUI({
    if (check$go_first == TRUE & input$period_method == "n.days.to.exp") {
      
      # if(input$seasonality==TRUE){
      highchartOutput("STABLEchart", height = input$chart_height * 2 / 3)
      
      # }
    }
  })
  
  observe({
    toggle(id = "ui_STABLEchart", anim = TRUE, animType = "slide", time = .7, condition = (input$stability))
  })
  
  observe({
    toggle(id = "ui_note_tbl", anim = TRUE, animType = "slide", time = .7, condition = (input$note_read))
  })
  
  observe({
    toggle(id = "ui_note_filter", anim = TRUE, animType = "slide", time = .7, condition = (input$note_read))
  })
  
  output$ui_summary.stat <- renderUI({
    if (check$go_first == TRUE) {
      dataTableOutput("summary.stat")
    }
  })
  
  output$ui_summary.stat2 <- renderUI({
    if (check$go_first == TRUE) {
      tableOutput("summary.stat2")
    }
  })
  
  
  
  output$ui_test_tbl <- renderUI({
    if (check$go_first == TRUE) {
      
      dataTableOutput("test_tbl")
    }
  })
  
  output$ui_note_tbl <- renderUI({
    if (check$go_first == TRUE ) {
      
      dataTableOutput("note_tbl")
    }
  })
  
  output$ui_note_filter <- renderUI({
    if (check$go_first == TRUE ) {
      
      checkboxGroupButtons(
        inputId = "note_filter",
        label = "Filter",
        choices = c("OnlyMe", 
                    "NotExpired", "NotReminded"),
        #selected = c("OnlyMe","NotExpired"),
        #status = "success",
        
        checkIcon = list(
          yes = tags$i(class = "fa fa-check-square", 
                       style = "color: green"),
          no = tags$i(class = "fa fa-square-o", 
                      style = "color: green"))
        
        # checkIcon = list(
        #   yes = icon("ok", 
        #              lib = "glyphicon"),
        #   no = icon("remove",
        #             lib = "glyphicon"))
      )
    }
  })
  # output$ui_hc_boxplot <- renderUI({
  #   if(check$go_first==TRUE){highchartOutput("hc_boxplot",height= hist_height)
  #   }
  # })
  #
  # output$ui_hc_group_hist <- renderUI({
  #   if(check$go_first==TRUE){highchartOutput("hc_group_hist",height= hist_height)
  #   }
  # })
  
  ################################################### Analysis PANEL Part #########################################
  
  output$guidance_doc <- renderUI({
    tags$a(
      href = "https://drive.google.com/open?id=1P1fcHn23GBxUCqt0NUbWeRxy0D9GyfxG",
      "GUIDANCE", target = "_blank"
      # ,
      # style = "color:#FFF;"
      # icon("question"),
      # title = "Help"
    )
  })
  
  output$meta <- renderDataTable({
    input$go_button
    # input$go_button_moving_analysis
    isolate({
      if (check$go_first == TRUE) {
        datatable(pure_spread_meta()@o.summary, options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
      }
    })
  })
  output$meta_yr <- renderDataTable({
    input$go_button
    # input$go_button_moving_analysis
    isolate({
      if (check$go_first == TRUE) {
        datatable(pure_spread_meta_yr(), options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
      }
    })
  })
  output$meta_exp <- renderDataTable({
    input$go_button
    # input$go_button_moving_analysis
    isolate({
      if (check$go_first == TRUE) {
        datatable(pure_spread_meta_exp(), options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
      }
    })
  })
  output$pure_spread_data_table <- renderDataTable({
    input$go_button
    # input$go_button_moving_analysis
    isolate({
      if (check$go_first == TRUE) {
        this.table <- pure_spread_data()[, index := as.character(index)]
        this.table <- this.table[, spread := round(spread, spread_round_by)]
        
        datatable(this.table, options = list(pageLength = 25), rownames = FALSE)
      }
    })
  })
  
  output$summary.stat <- renderDataTable({
    input$go_button
    # input$go_button_moving_analysis
    # input$button_3
    isolate({
      if (check$go_first == TRUE) {
        # tryCatch({
        datatable(summary.spread.data(), options = list(sDom = '<"top">rt<"bottom"flp><"clear">'), rownames = FALSE) # options = list(dom = "t")
        # }, error=function(e){})
      }
    })
  })
  
  
  # output$test_tbl <- renderDataTable({
  #   input$go_button
  #   #input$go_button_moving_analysis
  #   #input$button_3
  #   isolate({
  #     #if(check$go_first==TRUE){
  #       #tryCatch({
  #     
  #     a <- getQuery_specific(product=chosen_product(),interval=chosen_interval(),month=chosen_month(),relationship=chosen_relationship())
  #     if(nrow(a)>0){
  #       b <- a[,c("name","timestamp","note","profile","ongoing_yr","expiration")]
  #       
  #     }else{ b<- data.table()}
  #       datatable(        b, rownames= FALSE)
  #       #}, error=function(e){})
  #     #}
  #   })
  # })
  queried_note <- 
    
    reactive({
      # this part will be called only if output$note_tbl is refreshed. 
      # suppose there was first call, and you wrote something. 
      # without input$go_button write, this part will not update
      
      input$go_button_write
      
      getQuery_specific(in.product=chosen_product(),
                        in.interval=chosen_interval(),
                        in.month=chosen_month(),
                        in.relationship=chosen_relationship())
    })
  
  output$note_tbl <- renderDataTable({
    input$go_button
    input$note_filter
    input$go_button_write
    #input$go_button_moving_analysis
    #input$button_3
    isolate({
      #if(check$go_first==TRUE){
      #tryCatch({
      
      a <- queried_note()
      
      if(nrow(a)>0){
        b <- a[,c("name","timestamp","note","ongoing_yr","T","expiration","remind_on","reminded?")]
        
        if("OnlyMe" %in% input$note_filter){
          
          b <- b[name==this.trader]
        }
        if("NotExpired" %in% input$note_filter){
          
          b <- b[expiration>=Sys.Date()]
          
        }
        if("NotReminded" %in% input$note_filter){
          
          b <- b[`reminded?`=="" | is.na(`reminded?`)]
          
        }
        b <- b[order(-timestamp)]
        datatable(  b,selection = 'none',rownames= FALSE,extensions = c('Buttons','ColReorder','FixedHeader','KeyTable','Scroller') ,#filter = 'top'
                    options=list(#pageLength=5,
                      buttons = c('excel','print'),
                      dom = 'Bfrtip',# w/o this, buttons don't show
                      colReorder = TRUE,
                      fixedHeader = TRUE,
                      keys=TRUE,
                      scroller = TRUE, #scroller
                      
                      deferRender = TRUE, #scroller
                      scrollY = 200 #scroller
                    )
                    
        ) %>% formatDate("timestamp",method='toLocaleString')
        
      }else{ b<- data.table()
      datatable(b)
      }
      
      #}, error=function(e){})
      #}
    })
  })
  
  output$yrs_to_T <-  renderRHandsontable({
    
    rhandsontable(ongoing_yr_to_T(),rowHeaders = NULL)%>% 
      #hot_table(highlightCol = T, highlightRow = T)  %>%
      hot_col(col = names(ongoing_yr_to_T()), format = "0")
    #     hot_table(highlightCol = TRUE) %>%
    # hot_col(col = c("ongoing.yr", "today", "ww", "begin", "end", "D", "n.all.yr", "n.pos.yr", "n.neg.yr"), format = rhandsonFormat(0)) %>%
    #
    #   hot_col(col = c( "Med", "M", "score"), format = rhandsonFormat(input_round_n)) %>%
    #  hot_col(col = c( "link"), renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
    
    #   hot_cols(columnSorting = TRUE)
  })
  
  
  
  output$summary.stat2 <- renderTable({
    input$go_button
    # input$go_button_moving_analysis
    # input$button_3
    isolate({
      if (check$go_first == TRUE) {
        # tryCatch({
        t(summary.spread.data2())
        # }, error=function(e){})
      }
    })
  }, rownames = TRUE, colnames = FALSE, digits = 3)
  
  output$RPchart <- renderHighchart({
    input$go_button
    
    isolate({
      if (length(tail(spread.data()[spread.yr == input$ongoing_yr]$n.days.to.exp, n = 1)) != 0) {
        RPscores <- RP.data()
        
        # blue <- "#0000FF"
        # red <- "#FF0000"
        
        blue <- "#0025BF"
        red <- "#BF0085"
        
        
        RPscores[, col := ifelse(scores > 0, red, blue)]
        RPscores$col[ is.na(RPscores$col)] <- blue
        RPscores[, scores := round(scores, 2) ]
        
        hc <-
          highchart() %>%
          hc_add_series(RPscores, type = "column", hcaes(x = n.days.to.exp, y = scores, color = col), name = input$ongoing_yr) %>% #
          hc_title(text = paste0("RP @ ", RPscores[n.days.to.exp == today()]$scores), style = list(fontWeight = "bold")) %>% # ,fontSize="30px"
          hc_yAxis(max = 10, min = -10)
        
        # b6<- list(label=list(text="-6"),color="black",width=1,value=-6)
        b7 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = -7)
        b8 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = -8)
        b9 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = -9)
        b10 <- list(color = "black", width = 2, value = -10)
        
        s7 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = 7)
        s8 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = 8)
        s9 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = 9)
        s10 <- list(color = "black", width = 2, value = 10)
        
        hc <-
          hc %>%
          hc_xAxis(title = list(text = "n.days.to.exp"), allowDecimals = FALSE)
        
        if (today_in_date_range()) {
          hc <- hc %>%
            hc_xAxis(
              plotLines = list(list(
                color = "grey",
                dashStyle = "shortdash",
                value = today(),
                width = 1
              ))
            )
        }
        
        
        hc <-
          hc %>%
          hc_yAxis(
            max = 10, min = -10,
            title = list(text = "RP score"),
            opposite = FALSE,
            plotLines = list(b7, b8, b9, b10, s7, s8, s9, s10)
          )
        
        hc <- hc %>% hc_chart(zoomType = "xy", animation = TRUE)
        
        
        if (input$theme != "default") {
          theme <- switch(input$theme,
                          null = hc_theme_null(),
                          darkunica = hc_theme_darkunica(),
                          gridlight = hc_theme_gridlight(),
                          sandsignika = hc_theme_sandsignika(),
                          fivethirtyeight = hc_theme_538(),
                          economist = hc_theme_economist()
          )
          hc <- hc %>% hc_add_theme(theme)
        }
        
        hc
      } else {
        highchart()
      }
    })
  })
  
  
  output$Schart_tslice_dailymed <- renderHighchart({
    input$go_button
    
    isolate({
      blue <- "#0025BF"
      red <- "#BF0085"
      
      seasonality.wide <- seasonality.wide.tslice_dailymed()
      seasonality.wide[, col := ifelse(score > 0, red, blue)]
      seasonality.wide$col[ is.na(seasonality.wide$col)] <- blue
      seasonality.wide[, score := round(score, 2)]
      
      
      
      hc <-
        highchart() %>%
        hc_add_series(seasonality.wide, type = "column", hcaes(x = n.days.to.exp, y = score, color = col), name = "median (look forward all)")
      hc <- hc %>%
        hc_title(text = paste0("Seasonality A", " @ ", seasonality.wide[n.days.to.exp == today()]$score), style = list(fontWeight = "bold"), fontSize = "55px") %>% # ,fontSize="30px"
        # hc_title(text=paste0(gsub("_"," ",chosen_spread())," @" ,round(today_spread(),spread_round_by))
        #          ,style = list( fontWeight = "bold",fontSize="30px"))%>%
        hc_xAxis(title = list(text = "n.days.to.exp"), allowDecimals = FALSE) %>%
        hc_yAxis(
          # min=-500,max=500,
          title = list(text = "Seasonality score"),
          opposite = FALSE
        ) %>%
        hc_chart(zoomType = "xy", animation = TRUE)
      
      if (today_in_date_range()) {
        hc <- hc %>%
          hc_xAxis(
            plotLines = list(list(
              color = "grey",
              dashStyle = "shortdash",
              value = today(),
              width = 1
              # label=list(
              #   rotation=rotation_text,
              #   text=paste0(
              #     ifelse(!is.na(seasonality.wide[n.days.to.exp==today()]$score),
              #            seasonality.wide[n.days.to.exp==today()]$score,"")
              #
              #   ),
              #   style = list( color = "navy", fontWeight = "bold",fontSize="25px" )
            ))
          )
      }
      
      
      hc
      
      if (input$theme != "default") {
        theme <- switch(input$theme,
                        null = hc_theme_null(),
                        darkunica = hc_theme_darkunica(),
                        gridlight = hc_theme_gridlight(),
                        sandsignika = hc_theme_sandsignika(),
                        fivethirtyeight = hc_theme_538(),
                        economist = hc_theme_economist()
        )
        hc <- hc %>% hc_add_theme(theme)
      }
      
      hc
    })
  })
  
  
  output$Schart_roll_dailymed <- renderHighchart({
    input$go_button
    input$indicator_lookforward
    isolate({
      blue <- "#0025BF"
      red <- "#BF0085"
      
      seasonality.wide <- seasonality.wide.roll_dailymed()
      seasonality.wide[, col := ifelse(score > 0, red, blue)]
      seasonality.wide$col[ is.na(seasonality.wide$col)] <- blue
      seasonality.wide[, score := round(score, 2)]
      
      
      
      seasonality_score_today <- seasonality.wide[n.days.to.exp == today()]$score
      
      if (length(seasonality_score_today) == 0) {
        seasonality_score_today <- ""
      }
      
      hc <-
        highchart() %>%
        hc_add_series(seasonality.wide, type = "column", hcaes(x = n.days.to.exp, y = score, color = col), name = paste0("median (LF=", input$indicator_lookforward, ")"))
      
      hc <- hc %>%
        hc_title(text = paste0("Seasonality B", " @ ", seasonality.wide[n.days.to.exp == today()]$score), style = list(fontWeight = "bold"), fontSize = "55px") %>% # ,fontSize="30px"
        
        hc_xAxis(title = list(text = "n.days.to.exp"), allowDecimals = FALSE) %>%
        hc_yAxis(
          # min=-500,max=500,
          title = list(text = "Seasonality score"),
          opposite = FALSE
        ) %>%
        hc_chart(zoomType = "xy", animation = TRUE)
      
      if (today_in_date_range()) {
        hc <- hc %>%
          hc_xAxis(
            plotLines = list(list(
              color = "grey",
              dashStyle = "shortdash",
              value = today(),
              width = 1
            ))
          )
      }
      
      hc
      
      if (input$theme != "default") {
        theme <- switch(input$theme,
                        null = hc_theme_null(),
                        darkunica = hc_theme_darkunica(),
                        gridlight = hc_theme_gridlight(),
                        sandsignika = hc_theme_sandsignika(),
                        fivethirtyeight = hc_theme_538(),
                        economist = hc_theme_economist()
        )
        hc <- hc %>% hc_add_theme(theme)
      }
      
      hc
    })
  })
  
  output$Schart_roll_prob <- renderHighchart({
    input$go_button
    input$indicator_lookforward
    
    isolate({
      blue <- "#0025BF"
      red <- "#BF0085"
      
      seasonality.wide <- seasonality.wide.roll_prob()
      seasonality.wide[, col := ifelse(score > 0, red, blue)]
      seasonality.wide$col[ is.na(seasonality.wide$col)] <- blue
      seasonality.wide[, score := round(score, 2)]
      
      
      seasonality_score_today <- seasonality.wide[n.days.to.exp == today()]$score
      
      prob <- ((-seasonality_score_today + 10) / 20) * 100
      
      if (length(prob) != 0) {
        prob_text <- ifelse(prob >= 50, paste0(round(prob, 1), "% UP"), paste0(round(100 - prob, 1), "% DOWN"))
      } else {
        prob_text <- ""
      }
      
      hc <-
        highchart() %>%
        hc_add_series(seasonality.wide, type = "column", hcaes(x = n.days.to.exp, y = score, color = col), name = paste0("probability (LF=", input$indicator_lookforward, ")"))
      hc <- hc %>%
        hc_title(text = paste0("Seasonality C", " @ ", seasonality.wide[n.days.to.exp == today()]$score, " = ", prob_text), style = list(fontWeight = "bold"), fontSize = "55px") %>% # ,fontSize="30px"
        
        hc_xAxis(title = list(text = "n.days.to.exp"), allowDecimals = FALSE) %>%
        hc_yAxis(
          # min=-500,max=500,
          # title = list(text = "Seasonality score"),
          opposite = FALSE
        ) %>%
        hc_chart(zoomType = "xy", animation = TRUE)
      
      # b6<- list(label=list(text="-6"),color="black",width=1,value=-6)
      b7 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = -7)
      b8 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = -8)
      b9 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = -9)
      b10 <- list(color = "black", width = 2, value = -10)
      
      s7 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = 7)
      s8 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = 8)
      s9 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = 9)
      s10 <- list(color = "black", width = 2, value = 10)
      
      hc <-
        hc %>%
        hc_yAxis(
          max = 10, min = -10,
          title = list(text = "Seasonality score"),
          opposite = FALSE,
          plotLines = list(b7, b8, b9, b10, s7, s8, s9, s10)
        )
      
      
      
      
      if (today_in_date_range()) {
        hc <- hc %>%
          hc_xAxis(
            plotLines = list(list(
              color = "grey",
              dashStyle = "shortdash",
              value = today(),
              width = 1
            ))
          )
      }
      
      hc
      
      if (input$theme != "default") {
        theme <- switch(input$theme,
                        null = hc_theme_null(),
                        darkunica = hc_theme_darkunica(),
                        gridlight = hc_theme_gridlight(),
                        sandsignika = hc_theme_sandsignika(),
                        fivethirtyeight = hc_theme_538(),
                        economist = hc_theme_economist()
        )
        hc <- hc %>% hc_add_theme(theme)
      }
      
      hc
    })
  })
  
  output$Schart_D <- renderHighchart({
    input$go_button
    input$indicator_lookforward
    
    isolate({
      blue <- "#2E86C1"
      red <- "#FF5733"
      
      seasonality.wide <- seasonality_D()
      seasonality.wide[, col := ifelse(score > 0, red, blue)]
      seasonality.wide$col[ is.na(seasonality.wide$col)] <- blue
      seasonality.wide[, score := round(score, spread_round_by)]
      
      
      seasonality_score_today <- seasonality.wide[n.days.to.exp == today()]$score
      
      # prob <- ((-seasonality_score_today + 10) / 20) * 100
      #
      # if (length(prob) != 0) {
      #   prob_text <- ifelse(prob >= 50, paste0(round(prob, 1), "% UP"), paste0(round(100 - prob, 1), "% DOWN"))
      # } else {
      #   prob_text <- ""
      # }
      
      hc <-
        highchart() %>%
        hc_add_series(seasonality.wide, type = "column", hcaes(x = n.days.to.exp, y = score, color = col), name = paste0("prob*mean.move (LF=", input$indicator_lookforward, ")"))
      hc <- hc %>%
        hc_title(text = paste0("Seasonality D", " @ ", seasonality.wide[n.days.to.exp == today()]$score), style = list(fontWeight = "bold"), fontSize = "55px") %>% # ,fontSize="30px"
        
        hc_xAxis(title = list(text = "n.days.to.exp"), allowDecimals = FALSE) %>%
        hc_yAxis(
          # min=-500,max=500,
          # title = list(text = "Seasonality score"),
          opposite = FALSE
        ) %>%
        hc_chart(zoomType = "xy", animation = TRUE)
      
      if (today_in_date_range()) {
        hc <- hc %>%
          hc_xAxis(
            plotLines = list(list(
              color = "grey",
              dashStyle = "shortdash",
              value = today(),
              width = 1
            ))
          )
      }
      
      hc
      
      if (input$theme != "default") {
        theme <- switch(input$theme,
                        null = hc_theme_null(),
                        darkunica = hc_theme_darkunica(),
                        gridlight = hc_theme_gridlight(),
                        sandsignika = hc_theme_sandsignika(),
                        fivethirtyeight = hc_theme_538(),
                        economist = hc_theme_economist()
        )
        hc <- hc %>% hc_add_theme(theme)
      }
      
      hc
    })
  })
  
  output$Schart_D_versatile <- renderHighchart({
    input$go_button
    # input$indicator_lookforward
    
    isolate({
      # blue <- "#2E86C1"
      # red <- "#FF5733"
      
      seasonality.long <- seasonality_D_versatile_long()
      # seasonality.wide[, col := ifelse(score > 0, red, blue)]
      # seasonality.wide$col[ is.na(seasonality.wide$col)] <- blue
      # seasonality.wide[, score := round(score, spread_round_by)]
      
      
      seasonality_score_today <- seasonality.long[n.days.to.exp == today()]$score
      
      # prob <- ((-seasonality_score_today + 10) / 20) * 100
      #
      # if (length(prob) != 0) {
      #   prob_text <- ifelse(prob >= 50, paste0(round(prob, 1), "% UP"), paste0(round(100 - prob, 1), "% DOWN"))
      # } else {
      #   prob_text <- ""
      # }
      
      hc <-
        highchart() %>%
        hc_add_series(type = "column", data = seasonality.long, hcaes(x = n.days.to.exp, y = value, group = variable))
      
      hc <- hc %>%
        hc_title(text = paste0("Seasonality D versatile"), style = list(fontWeight = "bold"), fontSize = "55px") %>% # ,fontSize="30px"
        
        hc_xAxis(title = list(text = "n.days.to.exp"), allowDecimals = FALSE) %>%
        hc_yAxis(
          # min=-500,max=500,
          # title = list(text = "Seasonality score"),
          opposite = FALSE
        ) %>%
        hc_plotOptions(
          series = list(marker = list(enabled = FALSE), states = list(hover = list(radiusPlus = 5, lineWidthPlus = 5))),
          line = list(allowPointSelect = TRUE, findNearestPointBy = "xy")
        ) %>%
        hc_chart(zoomType = "xy", animation = TRUE)
      
      if (today_in_date_range()) {
        hc <- hc %>%
          hc_xAxis(
            plotLines = list(list(
              color = "grey",
              dashStyle = "shortdash",
              value = today(),
              width = 1
            ))
          )
      }
      
      hc
      
      if (input$theme != "default") {
        theme <- switch(input$theme,
                        null = hc_theme_null(),
                        darkunica = hc_theme_darkunica(),
                        gridlight = hc_theme_gridlight(),
                        sandsignika = hc_theme_sandsignika(),
                        fivethirtyeight = hc_theme_538(),
                        economist = hc_theme_economist()
        )
        hc <- hc %>% hc_add_theme(theme)
      }
      
      hc
    })
  })
  
  output$Schart_E <- renderHighchart({
    input$go_button
    input$indicator_lookforward
    
    isolate({
      blue <- "#2E86C1"
      red <- "#FF5733"
      
      seasonality.wide <- seasonality_EF()
      seasonality.wide[, col := ifelse(seasonality_updn.move > 0, red, blue)]
      seasonality.wide$col[ is.na(seasonality.wide$col)] <- blue
      seasonality.wide[, seasonality_updn.move := round(seasonality_updn.move, 2)]
      
      
      seasonality_score_today <- seasonality.wide[n.days.to.exp == today()]$seasonality_updn.move
      
      # prob <- ((-seasonality_score_today + 10) / 20) * 100
      #
      # if (length(prob) != 0) {
      #   prob_text <- ifelse(prob >= 50, paste0(round(prob, 1), "% UP"), paste0(round(100 - prob, 1), "% DOWN"))
      # } else {
      #   prob_text <- ""
      # }
      
      hc <-
        highchart() %>%
        hc_add_series(seasonality.wide, type = "column", hcaes(x = n.days.to.exp, y = seasonality_updn.move, color = col), name = paste0("updn_mean (LF=", input$indicator_lookforward, ")"))
      hc <- hc %>%
        hc_title(text = paste0("Seasonality E", " @ ", seasonality.wide[n.days.to.exp == today()]$seasonality_updn.move), style = list(fontWeight = "bold"), fontSize = "55px") %>% # ,fontSize="30px"
        
        hc_xAxis(title = list(text = "n.days.to.exp"), allowDecimals = FALSE) %>%
        hc_yAxis(
          # min=-500,max=500,
          # title = list(text = "Seasonality score"),
          opposite = FALSE
        ) %>%
        hc_chart(zoomType = "xy", animation = TRUE)
      
      ## b6<- list(label=list(text="-6"),color="black",width=1,value=-6)
      # b7 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = -7)
      # b8 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = -8)
      # b9 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = -9)
      # b10 <- list(color = "black", width = 2, value = -10)
      #
      # s7 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = 7)
      # s8 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = 8)
      # s9 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = 9)
      # s10 <- list(color = "black", width = 2, value = 10)
      #
      # hc <-
      #   hc %>%
      #   hc_yAxis(
      #     max = 10, min = -10,
      #     title = list(text = "Seasonality score"),
      #     opposite = FALSE,
      #     plotLines = list(b7, b8, b9, b10, s7, s8, s9, s10)
      #   )
      
      
      
      
      if (today_in_date_range()) {
        hc <- hc %>%
          hc_xAxis(
            plotLines = list(list(
              color = "grey",
              dashStyle = "shortdash",
              value = today(),
              width = 1
            ))
          )
      }
      
      hc
      
      if (input$theme != "default") {
        theme <- switch(input$theme,
                        null = hc_theme_null(),
                        darkunica = hc_theme_darkunica(),
                        gridlight = hc_theme_gridlight(),
                        sandsignika = hc_theme_sandsignika(),
                        fivethirtyeight = hc_theme_538(),
                        economist = hc_theme_economist()
        )
        hc <- hc %>% hc_add_theme(theme)
      }
      
      hc
    })
  })
  
  output$Schart_F <- renderHighchart({
    input$go_button
    input$indicator_lookforward
    
    isolate({
      blue <- "#2E86C1"
      red <- "#FF5733"
      
      seasonality.wide <- seasonality_EF()
      seasonality.wide[, col := ifelse(seasonality_move > 0, red, blue)]
      seasonality.wide$col[ is.na(seasonality.wide$col)] <- blue
      seasonality.wide[, seasonality_move := round(seasonality_move, spread_round_by)]
      
      
      seasonality_score_today <- seasonality.wide[n.days.to.exp == today()]$seasonality_move
      
      # prob <- ((-seasonality_score_today + 10) / 20) * 100
      #
      # if (length(prob) != 0) {
      #   prob_text <- ifelse(prob >= 50, paste0(round(prob, 1), "% UP"), paste0(round(100 - prob, 1), "% DOWN"))
      # } else {
      #   prob_text <- ""
      # }
      
      hc <-
        highchart() %>%
        hc_add_series(seasonality.wide, type = "column", hcaes(x = n.days.to.exp, y = seasonality_move, color = col), name = paste0("moves_mean (LF=", input$indicator_lookforward, ")"))
      hc <- hc %>%
        hc_title(text = paste0("Seasonality F", " @ ", seasonality.wide[n.days.to.exp == today()]$seasonality_move), style = list(fontWeight = "bold"), fontSize = "55px") %>% # ,fontSize="30px"
        
        hc_xAxis(title = list(text = "n.days.to.exp"), allowDecimals = FALSE) %>%
        hc_yAxis(
          # min=-500,max=500,
          # title = list(text = "Seasonality score"),
          opposite = FALSE
        ) %>%
        hc_chart(zoomType = "xy", animation = TRUE)
      
      ## b6<- list(label=list(text="-6"),color="black",width=1,value=-6)
      # b7 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = -7)
      # b8 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = -8)
      # b9 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = -9)
      # b10 <- list(color = "black", width = 2, value = -10)
      #
      # s7 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = 7)
      # s8 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = 8)
      # s9 <- list(color = "green", dashStyle = "shortdash", width = 1.5, value = 9)
      # s10 <- list(color = "black", width = 2, value = 10)
      #
      # hc <-
      #   hc %>%
      #   hc_yAxis(
      #     max = 10, min = -10,
      #     title = list(text = "Seasonality score"),
      #     opposite = FALSE,
      #     plotLines = list(b7, b8, b9, b10, s7, s8, s9, s10)
      #   )
      
      
      
      
      if (today_in_date_range()) {
        hc <- hc %>%
          hc_xAxis(
            plotLines = list(list(
              color = "grey",
              dashStyle = "shortdash",
              value = today(),
              width = 1
            ))
          )
      }
      
      hc
      
      if (input$theme != "default") {
        theme <- switch(input$theme,
                        null = hc_theme_null(),
                        darkunica = hc_theme_darkunica(),
                        gridlight = hc_theme_gridlight(),
                        sandsignika = hc_theme_sandsignika(),
                        fivethirtyeight = hc_theme_538(),
                        economist = hc_theme_economist()
        )
        hc <- hc %>% hc_add_theme(theme)
      }
      
      hc
    })
  })
  
  
  
  output$RRchart_roll <- renderHighchart({
    input$go_button
    input$indicator_lookforward
    
    isolate({
      range.wide <- RR.wide()
      
      cols <- names(range.wide)[-1]
      range.wide[, (cols) := round(.SD, summary_round_by), .SDcols = cols] # vectorized round
      
      range.long <- melt(range.wide, id.vars = "n.days.to.exp")
      
      mean_range_today <- range.wide[n.days.to.exp == today()]$mean.rng
      median_range_today <- range.wide[n.days.to.exp == today()]$med.rng
      
      
      
      hc <-
        highchart() %>%
        hc_add_series(range.long, type = "line", hcaes(x = n.days.to.exp, y = value, group = variable))
      
      hc <- hc %>%
        hc_title(text = paste0("Range", " (LF=", input$indicator_lookforward, ") : M=", mean_range_today, "; Med=", median_range_today), style = list(fontWeight = "bold"), fontSize = "55px") %>% # ,fontSize="30px"
        # hc_title(text=paste0(gsub("_"," ",chosen_spread())," @" ,round(today_spread(),spread_round_by))
        #          ,style = list( fontWeight = "bold",fontSize="30px"))%>%
        hc_xAxis(title = list(text = "n.days.to.exp"), allowDecimals = FALSE) %>%
        hc_yAxis(
          # min=-500,max=500,
          title = list(text = "Range"),
          opposite = FALSE
        ) %>%
        hc_plotOptions(
          series = list(marker = list(enabled = FALSE), states = list(hover = list(radiusPlus = 5, lineWidthPlus = 5))),
          line = list(allowPointSelect = TRUE, findNearestPointBy = "xy")
        ) %>%
        hc_chart(zoomType = "xy", animation = TRUE)
      
      if (today_in_date_range()) {
        hc <- hc %>%
          hc_xAxis(
            plotLines = list(list(
              color = "grey",
              dashStyle = "shortdash",
              value = today(),
              width = 1
              # label=list(
              #   rotation=rotation_text,
              #   text=paste0(
              #     ifelse(!is.na(seasonality.wide[n.days.to.exp==today()]$score),
              #            seasonality.wide[n.days.to.exp==today()]$score,"")
              #
              #   ),
              #   style = list( color = "navy", fontWeight = "bold",fontSize="25px" )
            ))
          )
      }
      
      
      if (input$theme != "default") {
        theme <- switch(input$theme,
                        null = hc_theme_null(),
                        darkunica = hc_theme_darkunica(),
                        gridlight = hc_theme_gridlight(),
                        sandsignika = hc_theme_sandsignika(),
                        fivethirtyeight = hc_theme_538(),
                        economist = hc_theme_economist()
        )
        hc <- hc %>% hc_add_theme(theme)
      }
      
      hc
    })
  })
  
  output$SCALPchart <- renderHighchart({
    input$go_button
    input$indicator_lookforward
    
    isolate({
      scalpability.wide <- round(scalpability.wide(), summary_round_by)
      scalpability_today <- scalpability.wide[n.days.to.exp == today()]$mean.theta
      
      
      scalpability.long <- reshape2::melt(scalpability.wide[, c("n.days.to.exp", "mean.theta")], id.vars = "n.days.to.exp")
      scalpability.long$name <- paste0(scalpability.wide$n.days.to.exp, ": ", "n.pos.yrs=", scalpability.wide$n_pos.yr, " n.neg.yrs =", scalpability.wide$n_neg.yr)
      
      # hc <-
      #   highchart() %>%
      #   hc_add_series(scalpability.wide ,type="area",hcaes(x=n.days.to.exp,y=mean.theta),color="lightgreen" )
      
      hc <-
        highchart() %>%
        hc_add_series(scalpability.long, type = "line", hcaes(x = n.days.to.exp, y = value, group = variable), color = "green") # "#b2ffb2" )
      
      hc <- hc %>%
        hc_title(text = paste0("Scalpability", " (LF=", input$indicator_lookforward, ") : ", scalpability_today), style = list(fontWeight = "bold"), fontSize = "55px") %>% # ,fontSize="30px"
        # hc_title(text=paste0(gsub("_"," ",chosen_spread())," @" ,round(today_spread(),spread_round_by))
        #          ,style = list( fontWeight = "bold",fontSize="30px"))%>%
        hc_xAxis(title = list(text = "n.days.to.exp"), allowDecimals = FALSE) %>%
        hc_yAxis(
          # min=-500,max=500,
          title = list(text = "Scalpability"),
          opposite = FALSE
        ) %>%
        hc_plotOptions(
          series = list(marker = list(enabled = FALSE), states = list(hover = list(radiusPlus = 5, lineWidthPlus = 5))),
          line = list(allowPointSelect = TRUE, findNearestPointBy = "xy")
        ) %>%
        hc_chart(zoomType = "xy", animation = TRUE)
      
      if (today_in_date_range()) {
        hc <- hc %>%
          hc_xAxis(
            plotLines = list(list(
              color = "grey",
              dashStyle = "shortdash",
              value = today(),
              width = 1
              # label=list(
              #   rotation=rotation_text,
              #   text=paste0(
              #     ifelse(!is.na(seasonality.wide[n.days.to.exp==today()]$score),
              #            seasonality.wide[n.days.to.exp==today()]$score,"")
              #
              #   ),
              #   style = list( color = "navy", fontWeight = "bold",fontSize="25px" )
            ))
          )
      }
      
      
      if (input$theme != "default") {
        theme <- switch(input$theme,
                        null = hc_theme_null(),
                        darkunica = hc_theme_darkunica(),
                        gridlight = hc_theme_gridlight(),
                        sandsignika = hc_theme_sandsignika(),
                        fivethirtyeight = hc_theme_538(),
                        economist = hc_theme_economist()
        )
        hc <- hc %>% hc_add_theme(theme)
      }
      
      hc
    })
  })
  
  
  output$STABLEchart <- renderHighchart({
    input$go_button
    input$indicator_lookforward
    
    isolate({
      stability.wide <- round(stability.wide(), summary_round_by)
      stability_today <- stability.wide[n.days.to.exp == today()]$stability
      
      
      
      hc <-
        highchart() %>%
        hc_add_series(stability.wide, type = "line", hcaes(x = n.days.to.exp, y = stability), color = "navy", name = "minus.SD")
      
      hc <- hc %>%
        hc_title(text = paste0("Stability", " (LF=", input$indicator_lookforward, ") : ", stability_today), style = list(fontWeight = "bold"), fontSize = "55px") %>% # ,fontSize="30px"
        # hc_title(text=paste0(gsub("_"," ",chosen_spread())," @" ,round(today_spread(),spread_round_by))
        #          ,style = list( fontWeight = "bold",fontSize="30px"))%>%
        hc_xAxis(title = list(text = "n.days.to.exp"), allowDecimals = FALSE) %>%
        hc_yAxis(
          # min=-500,max=500,
          title = list(text = "Stability"),
          opposite = FALSE
        ) %>%
        hc_plotOptions(
          series = list(marker = list(enabled = FALSE), states = list(hover = list(radiusPlus = 5, lineWidthPlus = 5))),
          line = list(allowPointSelect = TRUE, findNearestPointBy = "xy")
        ) %>%
        hc_chart(zoomType = "xy", animation = TRUE)
      
      if (today_in_date_range()) {
        hc <- hc %>%
          hc_xAxis(
            plotLines = list(list(
              color = "grey",
              dashStyle = "shortdash",
              value = today(),
              width = 1
            ))
          )
      }
      
      
      if (input$theme != "default") {
        theme <- switch(input$theme,
                        null = hc_theme_null(),
                        darkunica = hc_theme_darkunica(),
                        gridlight = hc_theme_gridlight(),
                        sandsignika = hc_theme_sandsignika(),
                        fivethirtyeight = hc_theme_538(),
                        economist = hc_theme_economist()
        )
        hc <- hc %>% hc_add_theme(theme)
      }
      
      hc
    })
  })
  
  
  output$hcontainer <- renderHighchart({
    
    
    # if(check$go_first==TRUE){
    input$go_button
    input$go_button_moving_analysis
    # input$button_3
    
    
    isolate({
      if (input$period_method == "n.days.to.exp") {
        # hc <-
        #   hchart(spread.data(), type="line",  hcaes(x = n.days.to.exp, y = round(spread,5), group = spread.yr)) %>%
        #   hc_xAxis(title = list(text = "n.days.to.exp"),allowDecimals=FALSE)
        
        on.going.yr.n.days.to.exp <- tail(spread.data()[spread.yr == input$ongoing_yr]$n.days.to.exp, 1)
        on.going.yr.n.days.to.exp <- ifelse(length(on.going.yr.n.days.to.exp) != 0, on.going.yr.n.days.to.exp, NA_real_)
        
        sd <- setNames(spread.data()[, c("n.days.to.exp", "spread", "name", "spread.yr")], c("x", "y", "caption", "name"))
        # sd[,y:=round(y,5)]
        
        
        sdd <- sd %>%
          # use `name` to name  series according the value of `cat` avariable
          group_by(name) %>%
          do(data = list_parse(.)) %>%
          # add type of series
          mutate(type = "line")
        
        hc <-
          highchart() %>%
          hc_add_series_list(sdd) %>%
          hc_xAxis(title = list(text = "n.days.to.exp"), allowDecimals = FALSE)
        
        if (today_in_date_range()) {
          hc <- hc %>%
            hc_xAxis(
              plotLines = list(list(
                color = "grey",
                dashStyle = "shortdash",
                value = today(),
                width = 1
              ))
            ) %>%
            hc_yAxis(
              plotLines = list(list(
                color = "grey",
                dashStyle = "shortdash",
                value = today_spread(),
                width = 1
              ))
            )
        }
      } else {
        # hc <-
        #   hchart(spread.data(), type="line",  hcaes(x = int.normalized.index.for.plot, y = round(spread,5), group = spread.yr)) %>%
        #   hc_xAxis(title = list(text = "date"), categories = spread.data()$normalized.index, labels=list(enabled=FALSE) )  #enabled=FALSE means you want to delete it. 'categories' are to make each x-tick to shine
        #
        sd <- setNames(spread.data()[, c("int.normalized.index.for.plot", "spread", "name", "spread.yr", "normalized.index")], c("x", "y", "caption", "name", "normalized.index"))
        # sd[,y:=round(y,5)]
        
        sdd <- sd %>%
          # use `name` to name  series according the value of `cat` avariable
          group_by(name) %>%
          do(data = list_parse(.)) %>%
          # add type of series
          mutate(type = "line")
        
        hc <-
          highchart() %>%
          hc_add_series_list(sdd) %>%
          hc_xAxis(title = list(text = "date"), categories = spread.data()$normalized.index, labels = list(enabled = FALSE))
      }
      
      # why numbers show up to 2 digits?
      # hc_tooltip(formatter = JS("function () { return '<b>' + this.series.name + '</b><br /> ' + Highcharts.numberFormat(this.point.y, -1) + ' <br />' + this.x;}"))
      # hc_tooltip(formatter = JS("function () { return '<b>' + this.series.name + '</b><br /> ' + Highcharts.numberFormat(this.point.y, 5) + ' <br />' + this.x;}"))
      
      # hc_tooltip(formatter = JS("function () { return '<b>' + this.series.name + '</b><br /> ' + Highcharts.numberFormat(this.point.y, 3) + ' <br />' + this.point.caption;}"),
      
      hc <- hc %>%
        hc_title(
          text = paste0(gsub("_", " ", chosen_spread()), " @ ", round(today_spread(), spread_round_by))
          , style = list(fontWeight = "bold", fontSize = "30px")
        ) %>%
        hc_chart(zoomType = "xy", animation = FALSE) %>%
        hc_tooltip(formatter = JS("function () { return '<b>' + this.series.name + '</b><br /> ' + Highcharts.numberFormat(this.point.y, 3) + ' <br />' + this.point.caption;}"), shared = FALSE, crosshairs = TRUE) %>%
        # hc_tooltip(pointFormat = "{point.y} <br> {point.caption}",shared=FALSE, crosshairs=TRUE)%>%
        hc_yAxis(title = list(text = "spread")) %>%
        hc_plotOptions(
          series = list(marker = list(enabled = FALSE), states = list(hover = list(radiusPlus = 5, lineWidthPlus = 8))),
          line = list(allowPointSelect = TRUE, findNearestPointBy = "xy")
        )
      
      if (input$theme != "default") {
        theme <- switch(input$theme,
                        null = hc_theme_null(),
                        darkunica = hc_theme_darkunica(),
                        gridlight = hc_theme_gridlight(),
                        sandsignika = hc_theme_sandsignika(),
                        fivethirtyeight = hc_theme_538(),
                        economist = hc_theme_economist()
        )
        hc <- hc %>% hc_add_theme(theme)
      }
      
      
      # x_col <- ifelse(input$period_method =="n.days.to.exp","n.days.to.exp","int.normalized.index.for.plot") # this approach doesn't work
      if (input$check_moving_analysis == TRUE) {
        if (input$period_method == "n.days.to.exp") {
          focused_data <- rolling_analysis_data()[input$ndays_period[1] <= n.days.to.exp & n.days.to.exp <= input$ndays_period[2]]
          focused_data[, value := round(value, summary_round_by) ]
          hc <- hc %>% hc_add_series(type = "line", dashStyle = "shortdash", focused_data, hcaes(x = "n.days.to.exp", y = value, group = variable), color = "grey")
        } else {
          focused_data <- rolling_analysis_data()[input$date_period_begin <= normalized.index & normalized.index <= input$date_period_end]
          focused_data[, value := round(value, summary_round_by) ]
          hc <- hc %>% hc_add_series(type = "line", dashStyle = "shortdash", focused_data, hcaes(x = "int.normalized.index.for.plot", y = value, group = variable), color = "grey")
        }
      }
      if (input$check_svm == TRUE) {
        if (input$period_method == "n.days.to.exp") {
          focused_data <- svm_untuned_data()[input$ndays_period[1] <= n.days.to.exp & n.days.to.exp <= input$ndays_period[2]]
          focused_data[, value := round(value, summary_round_by) ]
          hc <- hc %>% hc_add_series(type = "line", dashStyle = "shortdash", focused_data, hcaes(x = "n.days.to.exp", y = value, group = variable), color = "navy")
        } else {
          focused_data <- svm_untuned_data()[input$date_period_begin <= normalized.index & normalized.index <= input$date_period_end]
          hc <- hc %>% hc_add_series(type = "line", dashStyle = "shortdash", focused_data, hcaes(x = "int.normalized.index.for.plot", y = value, group = variable), color = "navy")
        }
      }
      if (input$check_loess == TRUE) {
        if (input$period_method == "n.days.to.exp") {
          focused_data <- loess_data()[input$ndays_period[1] <= n.days.to.exp & n.days.to.exp <= input$ndays_period[2]]
          focused_data[, value := round(value, summary_round_by) ]
          
          hc <- hc %>% hc_add_series(type = "line", dashStyle = "shortdash", focused_data, hcaes(x = "n.days.to.exp", y = value, group = variable), color = "#086887")
        } else {
          focused_data <- loess_data()[input$date_period_begin <= normalized.index & normalized.index <= input$date_period_end]
          focused_data[, value := round(value, summary_round_by) ]
          hc <- hc %>% hc_add_series(type = "line", dashStyle = "shortdash", focused_data, hcaes(x = "int.normalized.index.for.plot", y = value, group = variable), color = "#086887")
        }
      }
      
      hc
    })
    # }
  })
  
  output$hc_boxplot <- renderHighchart({
    if (check$go_first == TRUE) {
      input$go_button
      # input$go_button_moving_analysis
      # input$button_3
      isolate({
        # hc <-
        # hcboxplot(x = round(spread.data()$spread,5),
        #           var = spread.data()$spread.yr,
        #           name="Summary"
        #           #color = "#2980b9"
        # )%>%
        # hc_chart(zoomType="xy", animation=TRUE)%>%
        # hc_chart(type = "column")
        # factor( spread.data()$spread.yr , rev(sort(unique( spread.data()$spread.yr ))))
        
        sd <- spread.data()
        
        sd$spread.yr <- factor(spread.data()$spread.yr, levels = rev(sort(unique(sd$spread.yr))))
        
        suppressWarnings({
          hc <- highchart() %>%
            hc_add_series_boxplot(x = round(sd$spread, spread_round_by), by = sd$spread.yr, name = "Summary", outliers = TRUE) %>%
            hc_chart(zoomType = "xy", animation = TRUE) %>%
            hc_chart(type = "column", inverted = TRUE) # suppress warning for 'deprecated' issue
          # hc <-
          # hcboxplot(x = round(spread.data()$spread,5), var = spread.data()$spread.yr, name="Summary",outliers = TRUE) %>%
          #   hc_chart(zoomType="xy", animation=TRUE,type = "column")
        })
        
        ongoing.yr.data <- sd[spread.yr == input$ongoing_yr]
        
        hc <- hc %>%
          # hc_xAxis(reversed = TRUE) %>%
          
          hc_yAxis(
            plotLines = list(
              list(value = tail(ongoing.yr.data$spread, 1), width = 2, color = ongoing.yr_color)
            )
          )
        
        if (input$theme != "default") {
          theme <- switch(input$theme,
                          null = hc_theme_null(),
                          darkunica = hc_theme_darkunica(),
                          gridlight = hc_theme_gridlight(),
                          sandsignika = hc_theme_sandsignika(),
                          fivethirtyeight = hc_theme_538(),
                          economist = hc_theme_economist()
          )
          
          hc <- hc %>% hc_add_theme(theme)
        }
        
        hc
      })
    }
  })
  # #  #
  output$hc_group_hist <- renderHighchart({
    if (check$go_first == TRUE) {
      input$go_button
      # input$go_button_moving_analysis
      # input$button_3
      isolate({
        sd_mult_gr <- 1
        spread.data <- spread.data()
        ongoing.yr.data <- spread.data[spread.yr == input$ongoing_yr]
        
        summary.spread.data <- summary.spread.data2()
        # ongoing.yr.spread.txt = paste0(input$ongoing_yr, " spread=",  round(tail(ongoing.yr.data$spread,1),3), " (",  format(tail(ongoing.yr.data$index,1),"%d%b%y"),")" )
        ongoing.yr.spread.txt <- ""
        
        hc <-
          hchart(round(spread.data$spread, spread_round_by), type = "area") %>%
          hc_chart(zoomType = "x", animation = TRUE, marginBottom = 75) %>%
          hc_xAxis(title = list(text = "")) %>%
          hc_plotOptions(series = list(showInLegend = FALSE)) %>%
          hc_xAxis(
            plotLines = list(
              list(zIndex = 5, value = summary.spread.data$MEAN, dashStyle = "shortdash", width = 2, color = mean_color, label = list(verticalAlign = mean_vertical_align, text = paste0("m=", summary.spread.data$MEAN), style = list(color = mean_color))),
              list(zIndex = 5, value = summary.spread.data$MEAN + sd_mult_gr * summary.spread.data$SD, dashStyle = "shortdash", width = 2, color = sd_color, label = list(verticalAlign = sd_vertical_align, text = paste0("m+", sd_mult_gr, "*sd=", round(summary.spread.data$MEAN + sd_mult_gr * summary.spread.data$SD, summary_round_by)), style = list(color = sd_color))),
              list(zIndex = 5, value = summary.spread.data$MEAN - sd_mult_gr * summary.spread.data$SD, dashStyle = "shortdash", width = 2, color = sd_color, label = list(verticalAlign = sd_vertical_align, text = paste0("m-", sd_mult_gr, "*sd=", round(summary.spread.data$MEAN - sd_mult_gr * summary.spread.data$SD, summary_round_by)), style = list(color = sd_color))),
              
              list(zIndex = 5, value = summary.spread.data$Q1, dashStyle = "shortdash", width = 2, color = Q_color, label = list(verticalAlign = Q_vertical_align, text = paste0("Q1=", round(summary.spread.data$Q1, summary_round_by)), style = list(color = Q_color))),
              list(zIndex = 5, value = summary.spread.data$Q3, dashStyle = "shortdash", width = 2, color = Q_color, label = list(verticalAlign = Q_vertical_align, text = paste0("Q3=", round(summary.spread.data$Q3, summary_round_by)), style = list(color = Q_color))),
              
              list(zIndex = 5, value = tail(ongoing.yr.data$spread, 1), width = 2, color = ongoing.yr_color, label = list(verticalAlign = mean_vertical_align, text = ongoing.yr.spread.txt, style = list(color = ongoing.yr_color))),
              
              list(zIndex = 5, value = summary.spread.data$MED, dashStyle = "shortdash", width = 1, color = median_color, label = list(verticalAlign = median_vertical_align, text = paste0("med=", summary.spread.data$MED), style = list(color = median_color))),
              list(zIndex = 5, value = summary.spread.data$MAX, dashStyle = "shortdash", width = 1, color = minmax_color, label = list(verticalAlign = minmax_vertical_align, text = paste0("max=", summary.spread.data$MAX), style = list(color = minmax_color))),
              list(zIndex = 5, value = summary.spread.data$MIN, dashStyle = "shortdash", width = 1, color = minmax_color, label = list(verticalAlign = minmax_vertical_align, text = paste0("min=", summary.spread.data$MIN), style = list(color = minmax_color)))
            )
          )
        
        if (input$theme != "default") {
          theme <- switch(input$theme,
                          null = hc_theme_null(),
                          darkunica = hc_theme_darkunica(),
                          gridlight = hc_theme_gridlight(),
                          sandsignika = hc_theme_sandsignika(),
                          fivethirtyeight = hc_theme_538(),
                          economist = hc_theme_economist()
          )
          
          hc <- hc %>% hc_add_theme(theme)
        }
        hc
      })
    }
  })
}

shinyApp(ui = ui, server = server, enableBookmarking = "server")

