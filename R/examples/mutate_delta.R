
 # Example ------------------------------------------------------------------

 # dplyr must be in NAMESPACE
 if (requireNamespace("dplyr", quietly = TRUE)) {

   # checkout data
   head(nmr)

   # add delta variables with 1 time lagged differences and fill with 0
   nmr  <- mutate_delta(
     data = nmr,
     cols = c("bw", "fat", "lean", "fluid"),
     id = "idno",
     time = "date"
   )

   # add delta variable with 2 time lagged differences and fill with 0
   nmr <- mutate_delta(
     data = nmr,
     cols = c("bw", "fat", "lean", "fluid"),
     id = "idno",
     time = "date",
     n = 2
   )
 }
