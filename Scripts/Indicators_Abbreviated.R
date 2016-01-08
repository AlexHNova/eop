# Project: EOP
# Date: 14 December 2015

# ---------------------------------------- #

doTheMeltCast <- function(df = NULL) {
  
    df$dist_var <- rownames(df)
    df <- melt(data = df, id.vars = "dist_var")
    names(df) <- c("variable", "type", "value")
    df$prepost <- NA
    df[grep(pattern = "_pre", x = df$type, fixed = TRUE), "prepost"] <- "pre"
    df[grep(pattern = "_post", x = df$type, fixed = TRUE), "prepost"] <- "post"
    df <- move.cols(df = df, 
                       colNames = c("variable", "type", "value", "prepost"),
                       colIdxx = c(3, 2, 4, 1))
    df$type <- gsub(pattern = "_pre|_post", replacement = "", x = df$type)
    
    df <- dcast(data = df, formula = prepost + type ~ variable)
    df$prepost <- factor(x = df$prepost, levels = c("pre", "post"), ordered = TRUE)
    
    return(df)
}

prettify <- function(df = NULL) {
  
  CI <- data.frame(matrix(nrow = 1, ncol = ncol(df)), stringsAsFactors = FALSE)
  colnames(CI) <- colnames(df)
  rownames(CI) <- "CI"

  df <- rbind(df, CI) ; rm(CI)
  
  for (c in 1:ncol(df)) {
    df["CI", c] <- paste("(", df["Lower", c], ",", df["Upper", c], ")", sep = "")
  }
  df <- df[c("PointEst", "CI"),]
  
  names(df) <- gsub(pattern = "_pre|_post", replacement = "", x = names(df))
  return(df)
}

# Group C
{
# REG
indic_C1i <- function(capt = "")
{

    des.energy.seasonal.winter <- des[, grep(pattern = "_seasonal_winter", x = names(des), value = TRUE)]
    names(des.energy.seasonal.winter)[grep("coal", names(des.energy.seasonal.winter))] <- "coal"
    names(des.energy.seasonal.winter)[grep("wood", names(des.energy.seasonal.winter))] <- "wood"
    des.energy.seasonal.winter$coal <- tolower(as.character(des.energy.seasonal.winter$coal))
    des.energy.seasonal.winter$wood <- tolower(as.character(des.energy.seasonal.winter$wood))
    des.energy.seasonal.winter$sf.user <- "No"
    des.energy.seasonal.winter[which((des.energy.seasonal.winter$coal == "yes") | 
                                     (des.energy.seasonal.winter$wood == "yes")), "sf.user"] <- "Yes"
    des$sf.user.winter <- des.energy.seasonal.winter$sf.user
    rm(des.energy.seasonal.winter)
  
    sf.prop.winter <- as.data.frame(binCI.by(x = des, 
                                              by = "type", 
                                              vrr = "sf.user.winter", 
                                              opsie = "Yes"))
    
    sf.prop.winter.pre <- as.data.frame(binCI.by(x = des[which(des$prepost == "pre"),], 
                                              by = "type", 
                                              vrr = "sf.user.winter", 
                                              opsie = "Yes"))
    sf.prop.winter.pre <- prettify(sf.prop.winter.pre)
    
    sf.prop.winter.post <- as.data.frame(binCI.by(x = des[which(des$prepost == "post"),], 
                                              by = "type", 
                                              vrr = "sf.user.winter", 
                                              opsie = "Yes"))
    sf.prop.winter.post <- prettify(sf.prop.winter.post)
    
    graphdf <- doTheMeltCast(sf.prop.winter)
    rp <- ggplot(data = graphdf, mapping = aes(x = prepost, group = type, colour = type)) + 
      geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = type), alpha = 0.2) +
      ggtitle(capt) +
      ylim(0,100) +
      xlab("Pre/post \n (Source: DES)") + 
      ylab("Percent. of hhs \n (CI = 95%)")
    print(rp)
    
    addWorksheet(wb = tempWorkbook, sheetName = "C1i")
    insertPlot(wb = tempWorkbook, sheet = "C1i")
    
    lst <- list(sf.prop.winter.pre, sf.prop.winter.post)
    names(lst) <- c("pre", "post")
    tempWorkbook <- updateWorkbook(dfList = lst, 
                                   wb = tempWorkbook, 
                                   sn = "C1i", 
                                   cn = capt, 
                                   rowNames = TRUE,
                                   startRow = 23)
    
    assign("sf.prop.winter", sf.prop.winter, .GlobalEnv)
    
    return(0)
}

# REG
indic_C1ii <- function(capt = "")
{

    des.energy.seasonal.summer <- des[, grep(pattern = "_seasonal_summer", x = names(des), value = TRUE)]
    names(des.energy.seasonal.summer)[grep("coal", names(des.energy.seasonal.summer))] <- "coal"
    names(des.energy.seasonal.summer)[grep("wood", names(des.energy.seasonal.summer))] <- "wood"
    des.energy.seasonal.summer$coal <- tolower(as.character(des.energy.seasonal.summer$coal))
    des.energy.seasonal.summer$wood <- tolower(as.character(des.energy.seasonal.summer$wood))
    des.energy.seasonal.summer$sf.user <- "No"
    des.energy.seasonal.summer[which((des.energy.seasonal.summer$coal == "yes") | 
                                     (des.energy.seasonal.summer$wood == "yes")), "sf.user"] <- "Yes"
    des$sf.user.summer <- des.energy.seasonal.summer$sf.user
    rm(des.energy.seasonal.summer)
  
    sf.prop.summer <- as.data.frame(binCI.by(x = des, 
                                              by = "type", 
                                              vrr = "sf.user.summer", 
                                              opsie = "Yes"))
    
    sf.prop.summer.pre <- as.data.frame(binCI.by(x = des[which(des$prepost == "pre"),], 
                                              by = "type", 
                                              vrr = "sf.user.summer", 
                                              opsie = "Yes"))
    sf.prop.summer.pre <- prettify(sf.prop.summer.pre)
    
    sf.prop.summer.post <- as.data.frame(binCI.by(x = des[which(des$prepost == "post"),], 
                                              by = "type", 
                                              vrr = "sf.user.summer", 
                                              opsie = "Yes"))
    sf.prop.summer.post <- prettify(sf.prop.summer.post)
    
    graphdf <- doTheMeltCast(sf.prop.summer)
    rp <- ggplot(data = graphdf, mapping = aes(x = prepost, group = type, colour = type)) + 
      geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = type), alpha = 0.2) +
      ggtitle(capt) +
      ylim(0,100) +
      xlab("Pre/post \n (Source: DES)") + 
      ylab("Percent. of hhs \n (CI = 95%)")
    print(rp)
    
    addWorksheet(wb = tempWorkbook, sheetName = "C1ii")
    insertPlot(wb = tempWorkbook, sheet = "C1ii")
    
    lst <- list(sf.prop.summer.pre, sf.prop.summer.post)
    names(lst) <- c("pre", "post")
    tempWorkbook <- updateWorkbook(dfList = lst, 
                                   wb = tempWorkbook, 
                                   sn = "C1ii", 
                                   cn = capt, 
                                   rowNames = TRUE,
                                   startRow = 23)
    
    assign("sf.prop.summer", sf.prop.summer, .GlobalEnv)

}

# REG
indic_C1iii <- function(capt = "")
{
    des$energy_coal_seasonal_winter <- tolower(as.character(des$energy_coal_seasonal_winter))
    coal.winter.prop <- binCI.by(x = des, 
                                 by = "type", 
                                 vr = "energy_coal_seasonal_winter", 
                                 opsie = "yes")
    
    coal.winter.prop.pre <- binCI.by(x = des[which(des$prepost == "pre"),], 
                                  by = "type", 
                                  vr = "energy_coal_seasonal_winter", 
                                  opsie = "yes")
    coal.winter.prop.pre <- prettify(coal.winter.prop.pre)
    
    coal.winter.prop.post <- binCI.by(x = des[which(des$prepost == "post"),], 
                                         by = "type", 
                                         vr = "energy_coal_seasonal_winter", 
                                         opsie = "yes")
    coal.winter.prop.post <- prettify(coal.winter.prop.post)
    
    coal.winter.prop <- data.frame(rbind(coal.winter.prop), stringsAsFactors = FALSE)
    graphdf <- doTheMeltCast(coal.winter.prop)
    rp <- ggplot(data = graphdf, mapping = aes(x = prepost, group = type, colour = type)) + 
      geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = type), alpha = 0.2) +
      ggtitle(capt) +
      ylim(0,100) +
      xlab("Pre/post \n (Source: DES)") + 
      ylab("Percent. of hhs \n (CI = 95%)")
    print(rp)
    
    addWorksheet(wb = tempWorkbook, sheetName = "C1iii")
    insertPlot(wb = tempWorkbook, sheet = "C1iii")
    
    lst <- list(coal.winter.prop.pre, coal.winter.prop.post)
    names(lst) <- c("pre", "post")
    tempWorkbook <- updateWorkbook(dfList = lst, 
                                   wb = tempWorkbook, 
                                   sn = "C1iii", 
                                   cn = capt, 
                                   rowNames = TRUE,
                                   startRow = 23)
    
    assign("coal.winter.prop", coal.winter.prop, .GlobalEnv)

}

# REG
indic_C1iv <- function(capt = "")
{

    des$energy_coal_seasonal_summer <- tolower(as.character(des$energy_coal_seasonal_summer))
    
    coal.summer.prop <- binCI.by(x = des, 
                               by = "type", 
                               vr = "energy_coal_seasonal_summer", 
                               opsie = "yes")
    
    coal.summer.prop.pre <- binCI.by(x = des[which(des$prepost == "pre"),], 
                                  by = "type", 
                                  vr = "energy_coal_seasonal_summer", 
                                  opsie = "yes")
    coal.summer.prop.pre <- prettify(coal.summer.prop.pre)
    
    coal.summer.prop.post <- binCI.by(x = des[which(des$prepost == "post"),], 
                                         by = "type", 
                                         vr = "energy_coal_seasonal_summer", 
                                         opsie = "yes")
    coal.summer.prop.post <- prettify(coal.summer.prop.post)
    
    coal.summer.prop <- data.frame(rbind(coal.summer.prop), stringsAsFactors = FALSE)
    graphdf <- doTheMeltCast(coal.summer.prop)
    rp <- ggplot(data = graphdf, mapping = aes(x = prepost, group = type, colour = type)) + 
      geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = type), alpha = 0.2) +
      ggtitle(capt) +
      ylim(0,100) +
      xlab("Pre/post \n (Source: DES)") + 
      ylab("Percent. of hhs \n (CI = 95%)")
    print(rp)
    
    addWorksheet(wb = tempWorkbook, sheetName = "C1iv")
    insertPlot(wb = tempWorkbook, sheet = "C1iv")
    
    lst <- list(coal.summer.prop.pre, coal.summer.prop.post)
    names(lst) <- c("pre", "post")
    tempWorkbook <- updateWorkbook(dfList = lst,
                                   wb = tempWorkbook, 
                                   sn = "C1iv", 
                                   cn = capt, 
                                   rowNames = TRUE,
                                   startRow = 23)
    
    assign("coal.summer.prop", coal.summer.prop, .GlobalEnv)

}

# REG
indic_C1v <- function(capt = "")
{
  popmat <- popmat[,c(match(names(sf.prop.winter), colnames(popmat)))]
  popmat3 <- popmat
  sf_nw <- (sf.prop.winter/100)*popmat3
  
  popv2 <- rbind(popv2[1,], popv2[1,], popv2[1,])
  popv2 <- popv2[,c(match(colnames(sf.prop.winter2), colnames(popv2)))]
  sf_nw2 <- (sf.prop.winter2/100)*popv2
  
  popv3 <- rbind(popv3[1,], popv3[1,], popv3[1,])
  popv3 <- popv3[,c(match(colnames(sf.prop.winter.prepost), colnames(popv3)))]
  sf_nw_pp <- (sf.prop.winter.prepost/100)*popv3
  
  tempWorkbook <- updateWorkbook(dfList = list(sf_nw, sf_nw2, sf_nw_pp), 
                                 wb = tempWorkbook, 
                                 sn = paste("C1v - ", desname, sep = ""), 
                                 cn = capt, 
                                 rowNames = TRUE)
  assign("sf_nw", sf_nw, .GlobalEnv)
  assign("sf_nw2", sf_nw2, .GlobalEnv)
  assign("sf_nw_pp", sf_nw_pp, .GlobalEnv)
}

# REG
indic_C1vi <- function(capt = "")
{
  popmat <- popmat[,c(match(names(sf.prop.summer), colnames(popmat)))]
  popmat3 <- popmat
  sf_ns <- (sf.prop.summer/100)*popmat3

  popv2 <- rbind(popv2[1,], popv2[1,], popv2[1,])
  popv2 <- popv2[,c(match(colnames(sf.prop.summer2), colnames(popv2)))]
  sf_ns2 <- (sf.prop.summer2/100)*popv2
  
  popv3 <- rbind(popv3[1,], popv3[1,], popv3[1,])
  popv3 <- popv3[,c(match(colnames(sf.prop.summer.prepost), colnames(popv3)))]
  sf_ns_pp <- (sf.prop.summer.prepost/100)*popv3

  tempWorkbook <- updateWorkbook(dfList = list(sf_ns, sf_ns2, sf_ns_pp), 
                                 wb = tempWorkbook, 
                                 sn = paste("C1vi - ", desname, sep = ""), 
                                 cn = capt, 
                                 rowNames = TRUE)
  assign("sf_ns", sf_ns, .GlobalEnv)
  assign("sf_ns2", sf_ns2, .GlobalEnv)
  assign("sf_ns_pp", sf_ns_pp, .GlobalEnv)
}

# REG
indic_C1vii <- function(capt = "")
{
  popmat <- popmat[,c(match(colnames(coal.winter.prop), colnames(popmat)))]
  popmat3 <- popmat
  coal.winter_n <- (coal.winter.prop/100)*popmat3
  
  popv2 <- rbind(popv2[1,], popv2[1,], popv2[1,])
  popv2 <- popv2[,c(match(colnames(coal.winter.prop2), colnames(popv2)))]
  coal.winter_n2 <- (coal.winter.prop2/100)*popv2
  
  popv3 <- rbind(popv3[1,], popv3[1,], popv3[1,])
  popv3 <- popv3[,c(match(colnames(coal.winter.prop.prepost), colnames(popv3)))]
  coal.winter_n_pp <- (coal.winter.prop.prepost/100)*popv3

  tempWorkbook <- updateWorkbook(dfList = list(coal.winter_n, 
                                               coal.winter_n2, 
                                               coal.winter_n_pp), 
                                 wb = tempWorkbook, 
                                 sn = paste("C1vii - ", desname, sep = ""), 
                                 cn = capt, 
                                 rowNames = TRUE)
  
  assign("coal.winter_n", coal.winter_n, .GlobalEnv)
  assign("coal.winter_n2", coal.winter_n2, .GlobalEnv)
  assign("coal.winter_n_pp", coal.winter_n_pp, .GlobalEnv)
}

# REG
indic_C1viii <- function(capt = "")
{
  popmat <- popmat[,c(match(colnames(coal.summer.prop), colnames(popmat)))]
  popmat3 <- popmat
  coal.summer_n <- (coal.summer.prop/100)*popmat3
 
  popv2 <- rbind(popv2[1,], popv2[1,], popv2[1,])
  popv2 <- popv2[,c(match(colnames(coal.summer.prop2), colnames(popv2)))]
  coal.summer_n2 <- (coal.summer.prop2/100)*popv2
  
  popv3 <- rbind(popv3[1,], popv3[1,], popv3[1,])
  popv3 <- popv3[,c(match(colnames(coal.summer.prop.prepost), colnames(popv3)))]
  coal.summer_n_pp <- (coal.summer.prop.prepost/100)*popv3
  
  tempWorkbook <- updateWorkbook(dfList = list(coal.summer_n, 
                                               coal.summer_n2, 
                                               coal.summer_n_pp), 
                                 wb = tempWorkbook, 
                                 sn = paste("C1viii - ", desname, sep = ""), 
                                 cn = capt, 
                                 rowNames = TRUE)
  
  assign("coal.summer_n", coal.summer_n, .GlobalEnv)
  assign("coal.summer_n2", coal.summer_n2, .GlobalEnv)
  assign("coal.summer_n_pp", coal.summer_n_pp, .GlobalEnv)
}

# REG
indic_C2i <- function(capt = "")
{
    hh.coal.used.winter.month <- sumfun(x = des, 
                                      tp = "type", 
                                      vr =  "energy_coal_consumption_wintercurrentkg")
    
    hh.coal.used.winter.month.pre <- sumfun(x = des[which(des$prepost == "pre"),], 
                                         tp = "type", 
                                         vr =  "energy_coal_consumption_wintercurrentkg")
    hh.coal.used.winter.month.pre <- prettify(hh.coal.used.winter.month.pre)
    
    hh.coal.used.winter.month.post <- sumfun(x = des[which(des$prepost == "post"),], 
                                                tp = "type", 
                                                vr =  "energy_coal_consumption_wintercurrentkg")
    hh.coal.used.winter.month.post <- prettify(hh.coal.used.winter.month.post)
    
    hh.coal.used.winter.month <- data.frame(rbind(hh.coal.used.winter.month), stringsAsFactors = FALSE)
    graphdf <- doTheMeltCast(hh.coal.used.winter.month)
    rp <- ggplot(data = graphdf, mapping = aes(x = prepost, group = type, colour = type)) + 
      geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = type), alpha = 0.2) +
      ggtitle(capt) +
      ylim(0,100) +
      xlab("Pre/post \n (Source: DES)") + 
      ylab("Perc. hhs")
    print(rp)
    
    addWorksheet(wb = tempWorkbook, sheetName = "C2i")
    insertPlot(wb = tempWorkbook, sheet = "C2i")
    
    lst <- list(hh.coal.used.winter.month.pre, hh.coal.used.winter.month.post)
    names(lst) <- c("pre", "post")
    tempWorkbook <- updateWorkbook(dfList = lst, 
                                   wb = tempWorkbook, 
                                   sn = "C2i", 
                                   cn = capt)
    
    assign("hh.coal.used.winter.month", hh.coal.used.winter.month, envir = .GlobalEnv)
}

# REG
indic_C2ii <- function(capt = "")
{
    hh.coal.used.summer.month <- sumfun(x = des, 
                                        tp = "type", 
                                        vr = "energy_coal_consumption_summercurrentkg")
    
    hh.coal.used.summer.month.pre <- sumfun(x = des[which(des$prepost == "pre"),], 
                                         tp = "type", 
                                         vr =  "energy_coal_consumption_summercurrentkg")
    hh.coal.used.summer.month.pre <- prettify(hh.coal.used.summer.month.pre)
    
    hh.coal.used.summer.month.post <- sumfun(x = des[which(des$prepost == "post"),], 
                                                tp = "type", 
                                                vr = "energy_coal_consumption_summercurrentkg")
    hh.coal.used.summer.month.post <- prettify(hh.coal.used.summer.month.post)
      
    hh.coal.used.summer.month <- data.frame(rbind(hh.coal.used.summer.month), stringsAsFactors = FALSE)
    graphdf <- doTheMeltCast(hh.coal.used.summer.month)
    rp <- ggplot(data = graphdf, mapping = aes(x = prepost, group = type, colour = type)) + 
      geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = type), alpha = 0.2) +
      ggtitle(capt) +
      ylim(0,100) +
      xlab("Pre/post \n (Source: DES)") + 
      ylab("Perc. hhs")
    print(rp)

    addWorksheet(wb = tempWorkbook, sheetName = "C2ii")
    insertPlot(wb = tempWorkbook, sheet = "C2ii")
    
    lst <- list(hh.coal.used.summer.month.pre, hh.coal.used.summer.month.post)
    names(lst) <- c("pre", "post")
    tempWorkbook <- updateWorkbook(dfList = lst, 
                                   wb = tempWorkbook, 
                                   sn = "C2ii", 
                                   cn = capt)
    
    assign("hh.coal.used.summer.month", hh.coal.used.summer.month, envir = .GlobalEnv)

}

# REG
indic_C2v <- function(capt = "")
{

    # why not a prepost here?
  
    des$energy_coal_consumption_annual <- (des$energy_coal_consumption_wintercurrentkg)*4 + 
                                        (des$energy_coal_consumption_summercurrentkg)*8
    hh.coal.used.annualised <- sumfun(x = des, 
                                      tp = "type", 
                                      vr =  "energy_coal_consumption_annual")
    
    lst <- list(hh.coal.used.annualised)
    names(lst) <- c("Annualised")
    tempWorkbook <- updateWorkbook(dfList = lst, 
                                   sn = "C2v", 
                                   cn = capt, 
                                   rowNames = TRUE)
    
    assign("hh.coal.used.annualised", hh.coal.used.annualised, .GlobalEnv)             

}
}




# ---------------------------------------- #
# Group D
{
indic_D1i <- function(capt = "")
{
  vyfuur <- indoor_ambient_delta(df = houseTS, hours = 5, months = c(5,6,7,8))
  vyfuur[[1]]$date <- as.POSIXct(vyfuur[[1]]$date)
  
  a_i_delta <- sumfun(x = vyfuur[[1]], tp = "prepost", vr = "d_outd_avgIndoor")
  names(a_i_delta)[2] <- "mean Ambient-Indoor delta"
  year.delta = sumfun(x = selectFireEps(x = vyfuur[[1]], 
                                        na.rm = TRUE, 
                                        returnAll = TRUE), 
                      tp = "prepost", 
                      vr =  "thermal.comfort.delta")
  names(year.delta)[2] <- "mean thermal comfort deficit"

  # do the same, but now with tp = "Type"
  a_i_deltas <- sumfun(x = vyfuur[[1]], tp = "Type", vr = "d_outd_avgIndoor")
  names(a_i_deltas)[2] <- "mean Ambient-Indoor delta"
  year.deltas = sumfun(x = selectFireEps(x = vyfuur[[1]], 
                                         na.rm = TRUE, 
                                         returnAll = TRUE), 
                       tp = "Type", 
                       vr =  "thermal.comfort.delta")
  names(year.deltas)[2] <- "mean thermal comfort deficit"
  
  tempWorkbook <- try(updateWorkbook(wb = tempWorkbook, 
                                     dfList = list(a_i_delta, 
                                                   year.delta, 
                                                   a_i_deltas, 
                                                   year.deltas), 
                                     sn = "D1i", 
                                     cn = capt))
  
#   assign("a_i_delta1", a_i_delta, .GlobalEnv)
#   assign("year.delta1", year.delta, .GlobalEnv)
#   assign("a_i_deltas1", a_i_deltas, .GlobalEnv)
#   assign("year.deltas1", year.deltas, .GlobalEnv)
}

indic_D1ii <- function(capt = "")
{
  a_i_delta <- NULL
  year.delta <- NULL
  a_i_deltas <- NULL
  year.deltas <- NULL
  
  tweeuur <- indoor_ambient_delta(df = houseTS, hours = 14, months = c(1:4, 9:12)) 
  
  if (!is.null(tweeuur))
  {
    tweeuur[[1]]$date <- as.POSIXct(tweeuur[[1]]$date)
    
    a_i_delta <- sumfun(x = tweeuur[[1]], tp = "prepost", vr =  "d_outd_avgIndoor")
    names(a_i_delta)[2] <- "mean Ambient-Indoor delta"
    year.delta = sumfun(x = selectFireEps(x = tweeuur[[1]], 
                                          na.rm = TRUE, 
                                          returnAll = TRUE), 
                        tp = "prepost", 
                        vr =  "thermal.comfort.delta")
    if (!is.null(year.delta)) {names(year.delta)[2] <- "mean thermal comfort deficit"}
    
    # doen nou dieselfde, maar met tp = "Type"
    a_i_deltas <- sumfun(x = tweeuur[[1]], tp = "Type", vr =  "d_outd_avgIndoor")
    names(a_i_deltas)[2] <- "mean Ambient-Indoor delta"
    year.deltas = sumfun(x = selectFireEps(x = tweeuur[[1]], 
                                           na.rm = TRUE, 
                                           returnAll = TRUE), 
                         tp = "Type", 
                         vr =  "thermal.comfort.delta")
    if (!is.null(year.deltas)) {names(year.deltas)[2] <- "mean thermal comfort deficit"}
  }
  
  tempWorkbook <- updateWorkbook(wb = tempWorkbook, 
                                 dfList = list(a_i_delta, 
                                               year.delta, 
                                               a_i_deltas, 
                                               year.deltas), 
                                 sn = "D1ii", cn = capt)
  
  assign("a_i_delta2", a_i_delta, .GlobalEnv)
  assign("year.delta2", year.delta, .GlobalEnv)
  assign("a_i_deltas2", a_i_deltas, .GlobalEnv)
  assign("year.deltas2", year.deltas, .GlobalEnv) 
}

indic_D2i <- function(capt = "")
{
  load(paste(reportdir, "D2i.Rda", sep = ""))
  tempWorkbook <- updateWorkbook(wb = tempWorkbook, 
                                 dfList = list(days.comfort.prop.ym1, 
                                               days.comfort.prop.ys1, 
                                               days.comfort.prop.yt1), 
                                 sn = "D2i", cn = capt)
  
  assign("days.comfort.prop.ym1", days.comfort.prop.ym1, .GlobalEnv)
  assign("days.comfort.prop.ys1", days.comfort.prop.ys1, .GlobalEnv)
  assign("days.comfort.prop.yt1", days.comfort.prop.yt1, .GlobalEnv)
}

indic_D2ii <- function(capt = "")
{
  load(paste(reportdir, "D2ii.Rda", sep = ""))
  tempWorkbook <- updateWorkbook(wb = tempWorkbook, 
                                 dfList = list(days.comfort.prop.ym2, 
                                               days.comfort.prop.ys2, 
                                               days.comfort.prop.yt2), 
                                 sn = "D2ii", cn = capt)
  
  assign("days.comfort.prop.ym2", days.comfort.prop.ym2, .GlobalEnv)
  assign("days.comfort.prop.ys2", days.comfort.prop.ys2, .GlobalEnv)
  assign("days.comfort.prop.yt2", days.comfort.prop.yt2, .GlobalEnv)
}

indic_D3i <- function(capt = "", dta = houseTS)
{
  load(file = paste(reportdir, "D3i_res.Rda", sep = ""))
  tempWorkbook <- updateWorkbook(wb = tempWorkbook, 
                                 dfList = list(res), 
                                 sn = "D3i", 
                                 cn = capt, rowNames = TRUE)
  
  assign("secunda.pred", res, envir = .GlobalEnv)
}

indic_D3ii <- function(capt = "")
{
  d3ii <- data.frame(Annual.comfort.community = na.omit(secunda.pred) /100 * 1000 * 24 * 365)
  tempWorkbook <- updateWorkbook(wb = tempWorkbook, 
                                 dfList = list(d3ii), 
                                 sn = "D3ii", 
                                 cn = capt, 
                                 rowNames = TRUE)
  
  assign("d3ii", d3ii, envir = .GlobalEnv) 
}
}




# ---------------------------------------- #
# Group E
{
# satisfied
indic_E1i <- function(capt = "")
{
  #E1i Satisfaction with house: % hh agreeing with "I am satisfied with my house"

    des$piqola_satisfied <- gsub(pattern = "strongly_", 
                                 replacement = "", 
                                 x = des$piqola_satisfied, 
                                 fixed = TRUE)
    
    piqola_satisfied = binCI.by(x = des, 
                                by = "type", 
                                vrr = "piqola_satisfied", 
                                opsie = "agree")
    
    piqola_satisfied_pre = binCI.by(x = des[which(des$prepost == "pre"),], 
                                   by = "type", 
                                   vrr = "piqola_satisfied", 
                                   opsie = "agree")
    piqola_satisfied_pre <- prettify(piqola_satisfied_pre)
    
    piqola_satisfied_post = binCI.by(x = des[which(des$prepost == "post"),], 
                                        by = "type", 
                                        vrr = "piqola_satisfied", 
                                        opsie = "agree")
    piqola_satisfied_post <- prettify(piqola_satisfied_post)
    
    piqola_satisfied <- data.frame(rbind(piqola_satisfied), stringsAsFactors = FALSE)
    graphdf <- doTheMeltCast(piqola_satisfied)
    rp <- ggplot(data = graphdf, mapping = aes(x = prepost, group = type, colour = type)) + 
      geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = type), alpha = 0.2) +
      ggtitle(capt) +
      ylim(0,100) +
      xlab("Pre/post \n (Source: DES)") +
      ylab("Percent. of hhs \n (CI = 95%)")
    print(rp)
    
    addWorksheet(wb = tempWorkbook, sheetName = "E1i")
    insertPlot(wb = tempWorkbook, sheet = "E1i")
    
    lst <- list(piqola_satisfied_pre, piqola_satisfied_post)
    names(lst) <- c("pre", "post")
    tempWorkbook <- updateWorkbook(dfList = lst, 
                                   wb = tempWorkbook,
                                   sn = "E1i", 
                                   cn = capt, 
                                   rowNames = TRUE,
                                   startRow = 23)
    
    assign("piqola_satisfied", piqola_satisfied, envir = .GlobalEnv)

}

# thermal comfort
indic_E1ii <- function(capt = "")
{
    des$piqola_thermalcomfort <- gsub(pattern = "strongly_", 
                                 replacement = "", 
                                 x = des$piqola_thermalcomfort, 
                                 fixed = TRUE)
    
    piqola_comfort <- binCI.by(x = des, 
                              by = "type", 
                              vrr = "piqola_thermalcomfort", 
                              opsie = "agree")
    
    piqola_comfort_pre <- binCI.by(x = des[which(des$prepost == "pre"),], 
                                  by = "type", 
                                 vrr = "piqola_thermalcomfort", 
                                 opsie = "agree")
    piqola_comfort_pre <- prettify(piqola_comfort_pre)
    
    piqola_comfort_post <- binCI.by(x = des[which(des$prepost == "post"),], 
                                      by = "type", 
                                      vrr = "piqola_thermalcomfort", 
                                      opsie = "agree")
    piqola_comfort_post <- prettify(piqola_comfort_post)
    
    piqola_comfort <- data.frame(rbind(piqola_comfort), stringsAsFactors = FALSE)
    graphdf <- doTheMeltCast(piqola_comfort)
    rp <- ggplot(data = graphdf, mapping = aes(x = prepost, group = type, colour = type)) + 
      geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = type), alpha = 0.2) +
      ggtitle(capt) +
      ylim(0,100) +
      xlab("Pre/post \n (Source: DES)") +
      ylab("Percent. of hhs \n (CI = 95%)")
    print(rp)
    
    addWorksheet(wb = tempWorkbook, sheetName = "E1ii")
    insertPlot(wb = tempWorkbook, sheet = "E1ii")
    
    lst <- list(piqola_comfort_pre, piqola_comfort_post)
    names(lst) <- c("pre", "post")
    tempWorkbook <- updateWorkbook(dfList = lst,
                                   wb = tempWorkbook,
                                   sn = "E1ii", 
                                   cn = capt, 
                                   rowNames = TRUE,
                                   startRow = 23)
    
    assign("piqola_comfort", piqola_comfort, envir = .GlobalEnv)

}

# smoke
indic_E1iii <- function(capt = "")
{

    des$piqola_smoke <- gsub(pattern = "strongly_", 
                             replacement = "", 
                             x = des$piqola_smoke, 
                             fixed = TRUE)
    
    piqola_smoke <- binCI.by(x = des, 
                            by = "type", 
                            vrr = "piqola_smoke", 
                            opsie = "disagree")
    
    piqola_smoke_pre <- binCI.by(x = des[which(des$prepost == "pre"),], 
                               by = "type", 
                               vrr = "piqola_smoke", 
                               opsie = "disagree")
    piqola_smoke_pre <- prettify(piqola_smoke_pre)
    
    piqola_smoke_post <-  binCI.by(x = des[which(des$prepost == "post"),], 
                                    by = "type", 
                                    vrr = "piqola_smoke", 
                                    opsie = "disagree")
    piqola_smoke_post <- prettify(piqola_smoke_post)
    
    piqola_smoke <- data.frame(rbind(piqola_smoke), stringsAsFactors = FALSE)
    graphdf <- doTheMeltCast(piqola_smoke)
    rp <- ggplot(data = graphdf, mapping = aes(x = prepost, group = type, colour = type)) + 
      geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = type), alpha = 0.2) +
      ggtitle(capt) +
      ylim(0,100) +
      xlab("Pre/post \n (Source: DES)") +
      ylab("Percent. of hhs \n (CI = 95%)")
    print(rp)
    
    addWorksheet(wb = tempWorkbook, sheetName = "E1iii")
    insertPlot(wb = tempWorkbook, sheet = "E1iii")
    
    lst <- list(piqola_smoke_pre, piqola_smoke_post)
    names(lst) <- c("pre", "post")
    tempWorkbook <- updateWorkbook(dfList = lst, 
                                   wb = tempWorkbook,
                                   sn = "E1iii", 
                                   cn = capt, 
                                   rowNames = TRUE,
                                   startRow = 23)
    
    assign("piqola_smoke", piqola_smoke, envir = .GlobalEnv)

}

# value
indic_E1iv <- function(capt = "")
{
    des$piqola_value <- gsub(pattern = "strongly_", 
                             replacement = "", 
                             x = des$piqola_value, 
                             fixed = TRUE)
    
    piqola_value <- binCI.by(x = des, 
                            by = "type", 
                            vrr = "piqola_value", 
                            opsie = "agree")
    
    piqola_value_pre <- binCI.by(x = des[which(des$prepost == "pre"),], 
                               by = "type", 
                               vrr = "piqola_value", 
                               opsie = "agree")
    piqola_value_pre <- prettify(piqola_value_pre)
    
    piqola_value_post <- binCI.by(x = des[which(des$prepost == "post"),], 
                                    by = "type", 
                                    vrr = "piqola_value", 
                                    opsie = "agree")
    piqola_value_post <- prettify(piqola_value_post)
    
    piqola_value <- data.frame(rbind(piqola_value), stringsAsFactors = FALSE)
    graphdf <- doTheMeltCast(piqola_value)
    rp <- ggplot(data = graphdf, mapping = aes(x = prepost, group = type, colour = type)) + 
      geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = type), alpha = 0.2) +
      ggtitle(capt) +
      ylim(0,100) +
      xlab("Pre/post \n (Source: DES)") +
      ylab("Percent. of hhs \n (CI = 95%)")
    print(rp)
    
    addWorksheet(wb = tempWorkbook, sheetName = "E1iv")
    insertPlot(wb = tempWorkbook, sheet = "E1iv")
    
    lst <- list(piqola_value_pre, piqola_value_post)
    names(lst) <- c("pre", "post")
    tempWorkbook <- updateWorkbook(dfList = lst,
                                   wb = tempWorkbook,
                                   sn = "E1iv", 
                                   cn = capt, 
                                   rowNames = TRUE,
                                   startRow = 23)
    
    assign("piqola_value", piqola_value, envir = .GlobalEnv)

}

# power
indic_E1v <- function(capt = "")
{
    des$piqola_power <- gsub(pattern = "strongly_", 
                             replacement = "", 
                             x = des$piqola_power, 
                             fixed = TRUE)
    
    piqola_power <- binCI.by(x = des, 
                            by = "type", 
                            vrr = "piqola_power", 
                            opsie = "agree")
    
    piqola_power_pre <- binCI.by(x = des[which(des$prepost == "pre"),], 
                               by = "type", 
                               vrr = "piqola_power", 
                               opsie = "agree")
    piqola_power_pre <- prettify(piqola_power_pre)
    
    piqola_power_post <- binCI.by(x = des[which(des$prepost == "post"),], 
                                    by = "type", 
                                    vrr = "piqola_power", 
                                    opsie = "agree")
    piqola_power_post <- prettify(piqola_power_post)
    
    piqola_power <- data.frame(rbind(piqola_power), stringsAsFactors = FALSE)
    graphdf <- doTheMeltCast(piqola_power)
    rp <- ggplot(data = graphdf, mapping = aes(x = prepost, group = type, colour = type)) + 
      geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = type), alpha = 0.2) +
      ggtitle(capt) +
      ylim(0,100) +
      xlab("Pre/post \n (Source: DES)") +
      ylab("Percent. of hhs \n (CI = 95%)")
    print(rp)
    
    addWorksheet(wb = tempWorkbook, sheetName = "E1v")
    insertPlot(wb = tempWorkbook, sheet = "E1v")
    
    lst <- list(piqola_power_pre, piqola_power_post)
    names(lst) <- c("pre", "post")
    tempWorkbook <- updateWorkbook(dfList = lst,
                                   wb = tempWorkbook,
                                   sn = "E1v", 
                                   cn = capt, 
                                   rowNames = TRUE,
                                   startRow = 23)
    
    assign("piqola_power", piqola_power, envir = .GlobalEnv)

}

# cooking
indic_E1vi <- function(capt = "")
{

    des$piqola_cooking <- gsub(pattern = "strongly_", 
                               replacement = "", 
                               x = des$piqola_cooking, 
                               fixed = TRUE)
    
    piqola_cooking <- binCI.by(x = des, 
                              by = "type", 
                              vrr = "piqola_cooking", 
                              opsie = "disagree")
    
    piqola_cooking_pre <- binCI.by(x = des[which(des$prepost == "pre"),], 
                                 by = "type", 
                                 vrr = "piqola_cooking", 
                                 opsie = "disagree")
    piqola_cooking_pre <- prettify(piqola_cooking_pre)
    
    piqola_cooking_post <- binCI.by(x = des[which(des$prepost == "post"),], 
                                      by = "type", 
                                      vrr = "piqola_cooking", 
                                      opsie = "disagree")
    piqola_cooking_post <- prettify(piqola_cooking_post)
    
    piqola_cooking <- data.frame(rbind(piqola_cooking), stringsAsFactors = FALSE)
    graphdf <- doTheMeltCast(piqola_cooking)
    rp <- ggplot(data = graphdf, mapping = aes(x = prepost, group = type, colour = type)) + 
      geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = type), alpha = 0.2) +
      ggtitle(capt) +
      ylim(0,100) +
      xlab("Pre/post \n (Source: DES)") +
      ylab("Percent. of hhs \n (CI = 95%)")
    print(rp)
    
    addWorksheet(wb = tempWorkbook, sheetName = "E1vi")
    insertPlot(wb = tempWorkbook, sheet = "E1vi")
    
    lst <- list(piqola_cooking_pre, piqola_cooking_post)
    names(lst) <- c("pre", "post")
    tempWorkbook <- updateWorkbook(dfList = lst, 
                                   wb = tempWorkbook,
                                   sn = "E1vi", 
                                   cn = capt, 
                                   rowNames = TRUE,
                                   startRow = 23)
    
    assign("piqola_cooking", piqola_cooking, envir = .GlobalEnv)
}
}




# ---------------------------------------- #

    






# REG
# indic_C1iii <- function(capt = "")
{
    capt <- "% of households using wood in winter \n pre- and post-intervention"
    des$energy_wood_seasonal_winter <- tolower(as.character(des$energy_wood_seasonal_winter))
    wood.winter.prop <- binCI.by(x = des, 
                                 by = "type", 
                                 vr = "energy_wood_seasonal_winter", 
                                 opsie = "yes")
    
    wood.winter.prop.pre <- binCI.by(x = des[which(des$prepost == "pre"),], 
                                  by = "type", 
                                  vr = "energy_wood_seasonal_winter", 
                                  opsie = "yes")
    wood.winter.prop.pre <- prettify(wood.winter.prop.pre)
    
    wood.winter.prop.post <- binCI.by(x = des[which(des$prepost == "post"),], 
                                         by = "type", 
                                         vr = "energy_wood_seasonal_winter", 
                                         opsie = "yes")
    wood.winter.prop.post <- prettify(wood.winter.prop.post)
    
    wood.winter.prop <- data.frame(rbind(wood.winter.prop), stringsAsFactors = FALSE)
    graphdf <- doTheMeltCast(wood.winter.prop)
    rp <- ggplot(data = graphdf, mapping = aes(x = prepost, group = type, colour = type)) + 
      geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = type), alpha = 0.2) +
      ggtitle(capt) +
      ylim(0,100) +
      xlab("Pre/post \n (Source: DES)") + 
      ylab("Percent. of hhs \n (CI = 95%)")
    print(rp)
    
    addWorksheet(wb = tempWorkbook, sheetName = "C1iii")
    insertPlot(wb = tempWorkbook, sheet = "C1iii")
    
    lst <- list(wood.winter.prop.pre, wood.winter.prop.post)
    names(lst) <- c("pre", "post")
    tempWorkbook <- updateWorkbook(dfList = lst, 
                                   wb = tempWorkbook, 
                                   sn = "C1iii", 
                                   cn = capt, 
                                   rowNames = TRUE,
                                   startRow = 23)
    
    assign("wood.winter.prop", wood.winter.prop, .GlobalEnv)

}

# REG
# indic_C1iv <- function(capt = "")
{
    capt <- "% of households using wood in summer \n pre- and post-intervention"
    des$energy_wood_seasonal_summer <- tolower(as.character(des$energy_wood_seasonal_summer))
    
    wood.summer.prop <- binCI.by(x = des, 
                               by = "type", 
                               vr = "energy_wood_seasonal_summer", 
                               opsie = "yes")
    
    wood.summer.prop.pre <- binCI.by(x = des[which(des$prepost == "pre"),], 
                                  by = "type", 
                                  vr = "energy_wood_seasonal_summer", 
                                  opsie = "yes")
    wood.summer.prop.pre <- prettify(wood.summer.prop.pre)
    
    wood.summer.prop.post <- binCI.by(x = des[which(des$prepost == "post"),], 
                                         by = "type", 
                                         vr = "energy_wood_seasonal_summer", 
                                         opsie = "yes")
    wood.summer.prop.post <- prettify(wood.summer.prop.post)
    
    wood.summer.prop <- data.frame(rbind(wood.summer.prop), stringsAsFactors = FALSE)
    graphdf <- doTheMeltCast(wood.summer.prop)
    rp <- ggplot(data = graphdf, mapping = aes(x = prepost, group = type, colour = type)) + 
      geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = type), alpha = 0.2) +
      ggtitle(capt) +
      ylim(0,100) +
      xlab("Pre/post \n (Source: DES)") + 
      ylab("Percent. of hhs \n (CI = 95%)")
    print(rp)
    
    addWorksheet(wb = tempWorkbook, sheetName = "C1iv")
    insertPlot(wb = tempWorkbook, sheet = "C1iv")
    
    lst <- list(wood.summer.prop.pre, wood.summer.prop.post)
    names(lst) <- c("pre", "post")
    tempWorkbook <- updateWorkbook(dfList = lst,
                                   wb = tempWorkbook, 
                                   sn = "C1iv", 
                                   cn = capt, 
                                   rowNames = TRUE,
                                   startRow = 23)
    
    assign("wood.summer.prop", wood.summer.prop, .GlobalEnv)

}