df_imp <- readRDS("manuscript/df_imp.RData")

df_imp[] <- lapply(df_imp, function(i){
  ordered(round(i))
})

# Make data long for multilevel CFA
df_long <- reshape(as.data.frame(df_imp), direction = "long", varying = names(df_imp), sep = "_")
# Multilevel CFA
mod1 <- paste0("F =~ ", paste0(grep(
  "^SCL", names(df_long), value = T
), collapse = "+"))
res1 <-
  cfa(
    model = mod1,
    data = df_long[, -1],
    ordered = grep("^SCL", names(df_long), value = T),
    cluster = "id",
    std.lv = TRUE,
    auto.fix.first = FALSE,
    estimator ="WLSMV"
  )

saveRDS(res1, "res1.RData")
# Configural invariance model
wavs <- LETTERS[1:7][-6]
mod2 <- paste0(sapply(wavs, function(w){
  paste0("F", w, " =~ ", paste0(grep(
    paste0("^SCL.+_", w), names(df_imp), value = T
  ), collapse = "+"))}), collapse = "\n")

res2 <-
  cfa(
    model = mod2,
    data = df_imp,
    ordered = names(df_imp),
    std.lv = TRUE,
    auto.fix.first = FALSE,
    estimator ="WLSMV" 
  )
saveRDS(res2, "res2.RData")

# Metric invariance model
mod3 <- paste0(sapply(wavs, function(w){
  paste0("F", w, " =~ ", paste0(paste0("l", 1:16, " * "), paste0(grep(
    paste0("^SCL.+_", w), names(df_imp), value = T
  )), collapse = "+"))}), collapse = "\n")

res3 <-
  cfa(
    model = mod3,
    data = df_imp,
    ordered = names(df_imp),
    std.lv = TRUE,
    auto.fix.first = FALSE,
    estimator ="WLSMV" 
  )
saveRDS(res3, "res3.RData")

res_list <- list.files(pattern = "^res\\d+\\.RData")
res_list <- lapply(res_list, readRDS)
tab <- lapply(res_list, function(i)table_fit(i)[c("Name", "Parameters", "chisq", "df", "cfi", "tli", "rmsea", "srmr")
])
tab <- tidySEM:::bind_list(tab)
tab$Name <- c("CFA", "Configural", "Metrix")
write.csv(tab, "measurement_invariance.csv", row.names = FALSE)