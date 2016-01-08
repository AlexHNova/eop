library(openxlsx)

tl <- lapply(EOPdf_list, FUN = function(h) {
  t <- as.data.frame(table(is.na(h$C), is.na(h$W)))
  names(t) <- c("is.na(h$C)", "is.na(h$W)", "Freq")
  t
})

wb <- createWorkbook()
addWorksheet(wb = wb, sheetName = 1)
nextWritingRow <- 1
for (i in 1:length(tl))
{
  writeData(wb = wb, sheet = 1, x = names(tl)[[i]], startCol = 1, startRow = nextWritingRow, colNames = FALSE, rowNames = FALSE)
  nextWritingRow <<- nextWritingRow +1
  writeData(wb = wb,sheet = 1, x = tl[[i]], startCol = 1, startRow = nextWritingRow, colNames = TRUE, rowNames = FALSE)
  nextWritingRow <<- nextWritingRow + nrow(tl[[i]]) + 2
}

addWorksheet(wb = wb, sheetName = 2)





saveWorkbook(wb = wb, file = "QC_NAsinC_vs_NAsInW.xlsx", overwrite = TRUE)
