###############################################################
# PCA huislys en maak en fyn-verstel vuur veranderlike
###############################################################








# vuur.diagnoseer.plot
 vuur.diagnoseer.plot <- function(dpca = get("dpca", envir = xx), code = zz$stt, sstf = "S", debug = TRUE, grafdir = paste(getwd(), "/Figs/", sep = ""), naam = get("naam", environment(vuur.diagnoseer.plot)), verbose = get("verbose")){
   message("Jy's nou binne in vuur.diagnoseer.plot")
   message(code, "\n")
   message("naam is ", naam)
   message("sstf is ", sstf)
   message("code is ", code)
   if (debug == TRUE) assign("dpca", dpca, envir = .GlobalEnv)
   
   if (require(reshape2) == FALSE){
    install.packages("reshape2", dependencies = TRUE)
    require(melt)}

   if (verbose) {print(names(dpca))}
   if (verbose) {print(names(dpca$call))}
   if (verbose) {print(names(dpca$call$X))}
   if (verbose) {print(names(dpca$ind$contrib))}
   if (verbose) {print(names(dpca$ind$cos2))}
   print("cp1")
   data <- dpca$call$X 
   print("cp2")
   data$vuur <- eval(parse(text = code))
   print("cp3")
   data$date <- as.character(rownames(data))
   print("cp4")
   dm <- melt(data) 
   print("cp5")
   dev.off()
   p1  <- qplot(data = dm, x = as.POSIXlt(date), y = value,  color=variable==sstf, geom="line", facet = variable~., group=variable, xlab = "date", ylab = "temperature", main = naam)
   p3 <- qplot(data=dm, x = vuur, y = value, geom = "boxplot", fill=variable)
   p2 <- qplot(data = dm, x = as.POSIXlt(date), y = value,  color=vuur, geom="line", group=variable, xlab = "date", ylab = "temperature")
   p4 <- qplot(data = dm[which(dm$variable==sstf),], 
               x = as.integer(format(as.POSIXlt(dm$date[which(dm$variable==sstf)]), format = "%H")), 
               y = value,  color=vuur, geom="jitter", group=variable, xlab = "hour", ylab = "temperature", aplha=I(1/7), size = I(0.6))

   #p1 geom smooth, geom path per dag , boxplot, smooth + temp x tod , col=vuur, alle sensors lyne faset = vuur
   multiplot(p1, p2, p3, p4 + geom_smooth(aes(color=vuur, group=vuur)), layout = matrix(c(1,1,2,2,3,4),nrow=3, byrow=TRUE))
   datateller = max(as.integer(grep("data", ls(envir = .GlobalEnv))))
   if (debug == TRUE) assign(paste("data", datateller,sep=""), data, envir = .GlobalEnv)
   dev.copy2pdf(file=paste(grafdir,"diagnostiese.vuurplot", naam,".pdf", sep=""))
   
   message("jy verlaat vuur.diagnoseer.plot")
}
 
 
 
 
 
 
 
 
###################################################################################
# stipfunksie
###################################################################################
pl <- function(x = 1:nrow(dpca$ind$contrib), y = dpca$call$X[,sstf], tp = "p", sdif = "sdif", sdifval = 3, 
               pcadf = cluscont, cl, clII, mn , nm , nmII , stfnaam = "S",
               envr = zz, qplot=TRUE, verbose = TRUE, naam = "naam") {
  code = sprintf("ifelse(%s$call$X[,%s] > %s & %s$ind$contrib[ ,'%s']  > %s & %s$ind$cos2[,'%s']  > %s & %s > %s , 'vuur', 'nie')", 
                 deparse(substitute(pcadf)), deparse(substitute(sdif)), sdifval,
                 deparse(substitute(pcadf)), nm, cl, 
                 deparse(substitute(pcadf)), nmII, clII, 
                 paste("dpca$call$X[,'",stfnaam, "']", sep=""), mn)  # moontlik nie meer 'n morsige hack !!!!!!!!!!!!!!!
  if (verbose == TRUE) cat(code)
  kex = ifelse(pcadf$call$X[ ,sdif] > sdifval & pcadf$ind$contrib[,nm]  > cl & pcadf$ind$cos2[,nmII] > clII & y > mn, "vuur", "nie")
  if (qplot != TRUE) {
    message("base")
    plot(x, y, type = tp, col=factor(kex), 
       pch = 19, cex=0.25, 
       main = assign("stt", code, envir = envr))
  }
  
  if (qplot == TRUE) {
    require(ggplot2)
    if (!exists("week", envir = environment(pl))) qplot(x = x, y = y, geom = "smooth", color = kex, main = assign("stt", code, envir = envr))
    if (exists("week", envir = environment(pl))) qplot(x = x, y = y, geom = "smooth", facets = week~ ., color = kex, main = assign("stt", code, envir = envr))
  }
}










###################################################################################
# EKSPERIMENTEEL !!!
###################################################################################

pl.pca <- function(x = dpca$ind$coord[,1], y = ddpca$ind$coord[,2], tp = "p", sdif = "sdif", sdifval = 3, 
               pcadf = cluscont, cl, clII, mn , nm , nmII , 
               envr = zz, qplot=TRUE, verbose = TRUE, naam = "naam") {
  code = sprintf("ifelse(%s$call$X[,%s] > %s & %s$ind$contrib[ ,'%s']  > %s & %s$ind$cos2[,'%s']  > %s & %s > %s , 'vuur', 'nie')", 
                 deparse(substitute(pcadf)), deparse(substitute(sdif)), sdifval,
                 deparse(substitute(pcadf)), nm, cl, 
                 deparse(substitute(pcadf)), nmII, clII, 
                 "dpca$call$X[,'S']", mn)  # morsige hack !!!!!!!!!!!!!!!
  if (verbose == TRUE) cat(code)
  kex = ifelse(pcadf$call$X[ ,sdif] > sdifval & pcadf$ind$contrib[,nm]  > cl & pcadf$ind$cos2[,nmII] > clII & y > mn, "vuur", "nie")
  if (qplot != TRUE) {
    message("base")
    plot(x, y, type = tp, col=factor(kex), 
         pch = 19, cex=0.25, 
         main = assign("stt", code, envir = envr))
  }
  
  if (qplot == TRUE) {
    require(ggplot2)
    if (!exists("week", envir = environment(pl))) qplot(x = x, y = y, geom = "smooth", color = kex, main = assign("stt", code, envir = envr))
    if (exists("week", envir = environment(pl))) qplot(x = x, y = y, geom = "smooth", facets = week~ ., color = kex, main = assign("stt", code, envir = envr))
  }
}











###################################################################################
# hoof-funksie
# gebruik in die vorm df = maak.vuur(df) OF res = lapply(kortLYS, maak.vuur, aggr = TRUE, verbose=T, qp = F)
###################################################################################

maak.vuur <- function(df, aggr = FALSE, 
                      drops = c("vuur","E"), 
                      datum.naam = "date", 
                      naam.var = "naam",
                      skalleer = TRUE, 
                      stoofnaam = "S", 
                      verbose = FALSE, debug = FALSE,
                      tpp = "p", 
                      qp = FALSE,
                      stoordir = getwd()){
  if(suppressMessages(require(FactoMineR))) {if (verbose ==TRUE) {cat("FactoMineR gelaai ")}} else {install.packages("FactoMineR")}
  if(suppressMessages(require(ggplot2))) {if (verbose ==TRUE) {cat("ggplot gelaai ")}} else {install.packages("ggplot2")}
  if(require(data.table)) {if (verbose ==TRUE) {cat("data.table gelaai ")}} else {install.packages("data.table")}
  if(require(openair)) {if (verbose ==TRUE) {cat("openair gelaai ")}} else {install.packages("openair")}
  if(require(manipulate)) {if (verbose ==TRUE) {cat("manipulate gelaai ")}} else {install.packages("manipulate")}
  
  if ((stoofnaam %in% names(df)) == FALSE) {
    warning(paste("Nee, ou , hierdie datastel bevat nie 'n kolom met die naam '", stoofnaam, "' nie. Gee vir my die regte inligting en dan kan ons weer besigheid doen. ", sep = "")); 
    return(df)}
  
  if (all(is.na(df[, stoofnaam]))) {
    warning(paste("Hierdie datastel bevat geen waardes in die '", stoofnaam, "' kolom nie, so ek gaan nie enige waardes buiten NA vir die vuurkolom probeer insit nie. ", sep = ""))
    df$vuur <- NA
    return(df)}
  
  # maak seker daar is ander numeriese veranderlikes ook buiten die stoofkolom, want anders gaan die res van die funksie nie werk nie
  check.numeric.cols <- sapply(as.data.frame(df[, - match(c(datum.naam, stoofnaam), names(df))]), FUN = is.numeric)
  if (any(check.numeric.cols) == FALSE) {
    warning(paste("Hierdie datastel bevat geen ander numeriese veranderlikes buiten die stoofkolom nie, so ek gaan nie enige waardes buiten NA vir die vuurkolom probeer insit nie. ", sep = ""))
    df$vuur <- NA
    return(df)}
  
  # as daar wel ander numeriese kolomme is, maak net seker hulle is ook darem nou nie heeltemal net vol NAs nie...
  subdf <- as.data.frame(df[, - match(c(datum.naam, stoofnaam), names(df))])
  subdf <- as.data.frame(subdf[, check.numeric.cols])
  het.geen.ander.waardes <- all(sapply(as.data.frame(sapply(subdf, FUN = is.na)), FUN = all))
  if (het.geen.ander.waardes) {
    warning("Al die ander numeriese kolomme is leeg, so kan geen vure klassifiseer nie.")
    df$vuur <- NA
    return(df)}
  
  # maak ook seker dat daar darem 'n paar complete cases onder die numeriese kolomme is; indien nie, return met 'n NA vuur-kolom 
  numeric <- sapply(X = df, FUN = is.numeric)
  is_complete <- complete.cases(as.data.frame(df[, numeric]))
  if (!any(is_complete)) {
    warning("Geen volledige gevalle in die data stel nie. Kan nie vure klassifiseer nie.")
    df$vuur <- NA
    return(df)
  }
  
  if (any(!is.na(match(drops, names(df))))) df = df[ ,-na.omit(match(drops, names(df)))]
  if (verbose == TRUE) message("Drops gedoen ")
  if (aggr == TRUE) {
    week = as.integer(format(df[ ,match(datum.naam, names(df))], "%W")) # oorweeg om as.POSIXlt te gebruik
    xvals = as.integer(format(df[ ,match(datum.naam, names(df))], "%H"))
    if (verbose == TRUE) message("xvals is ", paste(head(xvals), " "), "\n week is ", paste(names(table(week)), " "))
    } else {xvals = 1:nrow(df)}
  
  naam = ifelse(!is.na(match(naam.var, names(df))), unique(df[, naam.var]), "naam")
  if (!is.na(match("naam", names(df)) ==FALSE)) df = df[ ,-grep("naam", names(df))]
  if (verbose) {message("Naam", naam)}
  rownames(df) <- df[,match(datum.naam, names(df))]
  df[,match(datum.naam, names(df))] = NULL # nou is die rynaam die datum en die datum kolom is weg (vereis deur PCA)
  names(df) <- gsub('[[:digit:]]+_+', "",names(df))
  if (verbose == TRUE) message("Names df: ", paste(names(df), " "))

  df$sdif <- NA_real_
  numeric_cols <- names(df)[sapply(df, FUN = is.numeric)]
  for (i in 1:nrow(df))
  {
    stooftemp <- df[i,stoofnaam]
    gemtemp_ander <- rowMeans(x = as.data.frame(df[i, numeric_cols[-match(c(stoofnaam, "sdif"), numeric_cols)]], stringsAsFactors = FALSE), dims = , na.rm = TRUE)
    sdif <- ifelse(is.na(stooftemp) | is.na(gemtemp_ander), NA, stooftemp - gemtemp_ander)
    df[i, "sdif"] <- sdif
  }
  
  # remove all the non-numeric columns (but NB! keep the rownames (because that is the date))
  numeric <- sapply(X = df, FUN = is.numeric)
  removed_cols <- NULL
  if (!all(numeric)) {
    removed_cols <- as.data.frame(df[, !numeric], stringsAsFactors = FALSE)
    names(removed_cols) <- names(df)[!numeric]
    rownames(removed_cols) <- rownames(df) }
  df <- df[, numeric]
  
  # now remove all the incomplete cases
  removed_rows <- NULL
  is_complete <- complete.cases(df)
  if (!all(is_complete)) {
    removed_rows <- df[!is_complete,]
    rownames(removed_rows) <- rownames(df[!is_complete,])
    colnames(removed_rows) <- colnames(df)
    df <- df[is_complete,]
  }
  
  if (verbose  == TRUE) message("Names df: ", paste(names(df), " "), "\nEn sy dimensies is: ", paste(dim(df), collapse = " by "))
  if (verbose  == TRUE) message("Aantal NAs moet nul wees en dis  ", paste(sapply(df, function(x) table(is.na(x))["TRUE"]), " "), " NA is goed hier")
  if (verbose  == TRUE) message("Aantal Inf s moet nul wees en dis  ", paste(sapply(df, function(x) table(is.infinite(x))["TRUE"]), " " ), " NA is goed hier") 
  
                               
  dpca <- PCA(df, scale.unit=skalleer)
  if (verbose == TRUE) message(paste(names(df), " "), "\nHoofkomponent-analise voltooi")
  ig = readline(prompt=cat("Kyk na die dimensies. Tik iets hier wanneer jy klaar is:"))
  if (exists("zz")) rm(zz)
  zz = new.env()
  xx = new.env()
  assign("dpca", dpca, envir = xx)
  if (verbose == TRUE) message("zz geskep", ls(zz))
  if (verbose == TRUE) message("xx geskep", ls(xx))
  klaar = FALSE
 
  if (verbose) {message("cp zz")}
  
  while(klaar == FALSE) {
     manipulate(pl(x = xvals, y = df[,stoofnaam], naam = naam, tp = tpp, pcadf = dpca, cl=cll, clII = cllII, mn = mnn, nm = nnm, nmII = nnmm, envr = zz, qplot = qp, sdifval = ssdifval, stfnaam = stoofnaam),
                ssdifval = slider(label = "Stoof diff", 0, 10, step=0.1, initial = 3), 
                cll = slider(label = "bydrae tot", 0, 1, step=0.0001, initial = 0.0375), 
                mnn = slider(0,30, step = 0.5, initial = 15),
                cllII = slider(label = "cos2 > ", 0, 1, step=0.005, initial = 0.135), 
                nnm = picker(label = "Bydrae dimensie", "Dim.1", "Dim.2", "Dim.3", "Dim.4", initial="Dim.2"),
                nnmm = picker(label = "cos2 dimensie", "Dim.1", "Dim.2", "Dim.3", "Dim.4", initial="Dim.2")
                  )
  
  ig = readline(prompt=cat("Tik iets hier om te kyk hoe dit werk"))
  # plot hier
  if (verbose) {print(names(dpca))}
  if (verbose) {print(names(dpca$call))}
  if (verbose) {print(zz$stt)}
  if (verbose) {print(zz$stt)}
  if (verbose) {message("stoordir: ", stoordir)}
  stm = gsub(":|[[:space:]]", "_", Sys.time())
  dput(x = zz$stt, file = paste(stoordir, stm, "_", naam, ".txt", sep = ""))
  vuur.diagnoseer.plot(dpca, zz$stt, sstf = stoofnaam, naam = naam, verbose = verbose)
  #
  kl = readline(prompt = cat("Tik JA as jy teverede is, anders enigiets anders"))
  if (kl == "JA") klaar = TRUE
  }
  
  if (verbose == TRUE) message("zz$st is: " , zz$stt)
  kolom = eval(parse(text = zz$stt))
  df[,"vuur"] = kolom
  if (verbose == TRUE) message("zz$stt geskryf")
  
  # sit nou die datum-kolom terug asook die ander nie-numeriese kolomme wat ons vroeer uitgehaal het
  if (!is.null(removed_cols)) {
    df$date <- rownames(df)
    rownames(df) <- NULL
    removed_cols$date <- rownames(removed_cols)
    rownames(removed_cols) <- NULL
    df <- merge(x = df, y = removed_cols, all.x = TRUE, all.y = TRUE)
  }
  
  # sit nou die rye terug wat ons vroeer uitgehaal het (indien enige)
  if (!is.null(removed_rows)) {
    removed_rows$date <- rownames(removed_rows)
    rownames(removed_rows) <- NULL
    
    for (c in 1:ncol(removed_rows)) {
      col <- (names(removed_rows)[c])
      if (col == "date") {next}
  
      for (r in 1:nrow(removed_rows)) {
        df[which(df$date == removed_rows[r,"date"]), col] <- removed_rows[r,col]
      }
    }
  }

  if (verbose == TRUE) message(paste(table(df$vuur), " "))
  if (debug == TRUE) assign("df", df, envir= .GlobalEnv)
  dm = melt(df)
  if (verbose == TRUE) message("df is: ", dim(df), "\n en dm is ", dim(dm))
  if (verbose == TRUE) message("ons stip nou " , paste(names(dm), " "), "\n", paste(table(dm$variable), " "), "\n", paste(table(dm$value), " "))
  qplot(data=dm, x=date, y=value, geom="point", size = I(0.7), main="", color = vuur) + facet_grid(facets = variable~.)
  if (verbose == TRUE) message(paste(ls(), " "))
  if (verbose == TRUE) message(paste(str(df), " "))
  return(df)
}








#############################################################################

# vuur.publikasie.plot
vuur.publikasie.plot <- function(x, vuurnaam = "vuur", sdif = "sdif", sstf = "S", naam = "", grafdir="Grafieke/",  verbose = FALSE, save = TRUE){
  naam = gsub("Huis", "House ", naam)
  stopifnot(class(x)=="data.frame")
  require(reshape2)
  require(ggplot2)
  names(x)[match(vuurnaam,names(x))] <- "vuur"
  if (verbose == TRUE) names(x)
  dm = melt(x, id.vars = c("date", "vuur"))
  p1  <- qplot(data = dm, x = as.POSIXlt(date), y = value,  color=variable == sstf, geom="line", facet = variable~., group=variable, xlab = "date", ylab = "temperature", main = naam)
  p3 <- qplot(data=dm, x = vuur, y = value, geom = "boxplot", fill=variable, xlab = "fire on?", ylab="temperature")
  p2 <- qplot(data = dm, x = as.POSIXlt(date), y = value,  color=vuur, geom="line", group=variable, xlab = "date", ylab = "temperature", main = naam)
  p4 <- qplot(data = dm[which(dm$variable==sstf),], 
              x = as.integer(format(as.POSIXlt(dm$date[which(dm$variable==sstf)]), format = "%H")), 
              y = value,  color=vuur, geom="jitter", group=variable, xlab = "hour", ylab = "temperature", aplha=I(1/2), size = I(0.7))
  p5 <- qplot(data= dm[which(dm$variable==sstf),], x = factor(format(as.POSIXlt(date), format = "%H")), fill=vuur, xlab = "hour")
  dc <- dcast(dm, formula = date + vuur ~ variable)
  dx = names(dc[which(sapply(dc, class)=="numeric")])
  dx = dx[-na.omit(match(c(sstf,"sdif"), dx))]
  dc$coldest.room = dc[,grep(names(which.min(sapply(dc[dx], mean, na.rm = TRUE))), names(dc))]
  p6 <- qplot(data = dc , x = coldest.room, y=S, geom="jitter", color=vuur, alpha = I(0.5), size = I(0.7)) 
  
  #p1 geom smooth, geom path per dag , boxplot, smooth + temp x tod , col=vuur, alle sensors lyne faset = vuur
  multiplot(p2, p3, p4 + geom_smooth(aes(color=vuur, group=vuur)), p5, p6, layout = matrix(c(1,1,2,3,4,5),nrow=3, byrow=TRUE))
  if (save == TRUE) dev.copy2pdf(file=paste(grafdir,"vuurplot", naam,".pdf", sep=""))
}









##############################################################################
# hulpfunksie multiplot van  Winston Chang: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

