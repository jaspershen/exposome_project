mz_match <- function(ms1.table,
                     database,
                     mz.error.tol = 25){
  
  mz <- ms1.table$mz
  
  result <- 
  lapply(mz, function(x){
    mz.error <- abs(x - database$mz) * 10^6 /ifelse(x < 400, 400, x)
    idx <- which(mz.error < mz.error.tol)
    if(length(idx) == 0){
      return(
        data.frame(MS2.spectrum.name = NA,
                   Candidate.number =NA, Compound.name = NA, CAS.ID = NA,
                   HMDB.ID = NA, KEGG.ID = NA, Lab.ID = NA, 
                   Adduct = NA, mz.error = NA,
                   mz.match.score = NA, RT.error = NA, 
                   RT.match.score = NA,
                   CE = NA, SS = NA, Total.score = NA, Database = NA, 
                   stringsAsFactors = FALSE)
      )
    }
    idx <- which.min(mz.error)
    data.frame(MS2.spectrum.name = NA,
               Candidate.number =1, 
               Compound.name = database$Compound.name[idx], 
               CAS.ID = NA,
               HMDB.ID = NA, 
               KEGG.ID = NA, 
               Lab.ID = NA, 
               Adduct = NA, 
               mz.error = mz.error[idx],
               mz.match.score = (mz.error.tol - mz.error[idx])/mz.error.tol, 
               RT.error = NA, 
               RT.match.score = NA,
               CE = NA, 
               SS = NA, 
               Total.score = (mz.error.tol - mz.error[idx])/mz.error.tol, 
               Database = "select_exposome", 
               stringsAsFactors = FALSE)
  })
  
  result <- do.call(rbind, result)
  result <- data.frame(ms1.table, result, stringsAsFactors = FALSE)
  return(result)
}