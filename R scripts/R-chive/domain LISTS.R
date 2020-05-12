

Month_list <- list("January"="1","February"="2","March" = "3",           # Name months for plotting
  "April"="4","May"="5","June"="6","July"="7","August"="8",
  "September"="9","October"="10","November"="11","December"="12")

Variables <- c("Salinity", "Temperature", "Ice", "DIN", "Chlorophyll", "Turbocline", "Mixed", "Zonal", "Meridional")
depth_levels <- list(Shallow = c(paste("S", Variables, sep = "_"), "S"), # Rename depth factor levels
  Deep = c(paste("D", Variables, sep = "_"), "D")) 

strip.s <- list(Salinity = c(paste(c("S","D"), "Salinity", sep = "_")),  # Groups of columns to convert into long dataframe
  Temperature = c(paste(c("S","D"), "Temperature", sep = "_")),
  Ice = "S_Ice", Turbocline = "S_Turbocline", Mixed = "S_Mixed") 

strip.p <- list(DIN = c(paste(c("S","D"), "DIN", sep = "_")),            # Groups of columns to convert into long dataframe
  Chlorophyll = c(paste(c("S","D"), "Chlorophyll", sep = "_")))
    