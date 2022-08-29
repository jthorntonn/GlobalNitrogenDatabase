library(readxl)

library(writexl)

library(magrittr) 

df0 <- read_xlsx("43016_2021_318_MOESM3_ESM.xlsx", sheet="Benchmark_median_Area_km2")

df1 <- read_xlsx("43016_2021_318_MOESM3_ESM.xlsx", sheet="Benchmark_median_Fertilizer")
df2 <- read_xlsx("43016_2021_318_MOESM3_ESM.xlsx", sheet="Benchmark_median_Manure")
df3 <- read_xlsx("43016_2021_318_MOESM3_ESM.xlsx", sheet="Benchmark_median_Fixation")
df4 <- read_xlsx("43016_2021_318_MOESM3_ESM.xlsx", sheet="Benchmark_median_Deposition")
df5 <- read_xlsx("43016_2021_318_MOESM3_ESM.xlsx", sheet="Benchmark_median_Harvest")

rm_df0 <- df0[,-1]
rm_df1 <- df1[,-1]
rm_df2 <- df2[,-1]
rm_df3 <- df3[,-1]
rm_df4 <- df4[,-1]
rm_df5 <- df5[,-1]

rm_df0$`1961` %<>% as.double
rm_df1$`1961` %<>% as.double
rm_df2$`1961` %<>% as.double
rm_df3$`1961` %<>% as.double
rm_df4$`1961` %<>% as.double
rm_df5$`1961` %<>% as.double

#surplus <- data.frame(matrix(ncol = 55, nrow = 116))
surplus <- rm_df1 + rm_df2 + rm_df3 + rm_df4 -rm_df5

surplus

n_input <- rm_df1 + rm_df2 + rm_df3 + rm_df4

n_input

nue <- rm_df5/n_input
nue

# ISO_codes <- read.csv('ISO.csv') # https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv
# 
# Nue_c <- data.frame(matrix(ncol = 3, nrow = 116))
# #surplus_c <- data.frame(matrix(ncol = 3, nrow = 116))
# 
# Nue_c['ISO3'] <- ISO_codes$alpha.3[match(Nue_c$...1, ISO_codes$name)]
# names(Nue_c) <- c("Country", as.character(c(2015)), 'ISO3')

#surplus_c['ISO3'] <- ISO_codes$alpha.3[match(Nue_c$...1, ISO_codes$name)]
#names(surplus_c) <- c("Country", as.character(c(2015)), 'ISO3')

nue_c <-cbind(df5[1], nue)
nue_c




df0_hec <-rm_df0*100

df1_kg <- rm_df1*907185
df2_kg <-rm_df2*907185
df3_kg <-rm_df3*907185
df4_kg <-rm_df4*907185
df5_kg <-rm_df5*907185

surplus_kg <- surplus*907185

df0_hecc <- cbind(df1[1], df0_hec)

df1_kgc <- cbind(df1[1], df1_kg)
df2_kgc <- cbind(df1[1], df2_kg)
df3_kgc <- cbind(df1[1], df3_kg)
df4_kgc <- cbind(df1[1], df4_kg)
df5_kgc <- cbind(df1[1], df5_kg)

surplus_kgc <-cbind(df1[1], surplus_kg)

write_xlsx(list(Benchmark_median_Area_Hectare = df0_hecc,
  Benchmark_median_Fertilizer = df1_kgc,
  Benchmark_median_Manure = df2_kgc,
  Benchmark_median_Fixation =df3_kgc,
  Benchmark_median_Deposition =df4_kgc,
  Benchmark_median_Harvest =df5_kgc,
                NUE = nue_c,
  Nsurplus = surplus_kgc),
           "nitrigenNUEsurplus.xlsx")
