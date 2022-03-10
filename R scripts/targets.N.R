
##**## extract mean winter and summer, shallow, and deep, ammonia and nitrate concentrations as target data

#### Setup ####

rm(list=ls(all.names = TRUE))                                               # Wipe the brain

library(MiMeMo.tools)

domains <- readRDS("./Objects/Domains.rds")                                 # Import model domain

#### CAO - Original manually corrected file ####

headers <- read.csv("./Data/BODC/JR016-006_CTD_nutrient_data_summary_BODC_fixed.csv", nrows=2, header=FALSE)
headers_names <- sapply(headers,paste,collapse="_")                         # Rebuild column names because they're split over 2 rows

manual_data <- read.csv(file="./Data/BODC/JR016-006_CTD_nutrient_data_summary_BODC_fixed.csv", skip = 2, 
                 header=FALSE) %>% 
  setNames(headers_names) %>%                                               # Fix awkward header names
  select(`Longitude (E)_`, `Latitude (N)_`, `CTD cast_`,                    # Keep interesting columnd
         `Date_`, `Depth (m)_`, `Nitrate+nitrite_mM l-1`,
         `Ammonium_mM l-1`) %>% 
  drop_na(`CTD cast_`) %>%                                  # Drop rows which are all NA
  mutate(Date_ = as.Date(Date_, format = "%d/%m/%Y")) %>%                   # Simplify date to character string for binding with new data
  mutate(Season = case_when(lubridate::month(Date_) %in% 5:8 ~ "summer",    # Get seasons for target data to be averaged in
                            lubridate::month(Date_) %in% c(1,2,11,12) ~ "winter",
                            T ~ "drop"))
#### CAO New raw data ####

all_data <- list.files("./Data/BODC/CAO_Nutrients/", recursive = T, full.names = T) %>% # List files
  .[!grepl("metadata", .)] %>%                                              # Drop metadata files
  map(~{
    headers <- readxl::read_excel(.x, col_names = F, n_max = 2)             # Read in first 2 rows
    headers_names <- sapply(headers, paste, collapse = "_")                 # Rebuild column names because they're split over 2 rows

    data <- readxl::read_excel(.x, skip = 2, col_names = F) %>%             # Read in data
      .[rowSums(is.na(.)) != ncol(.),] %>%                                  # Drop rows which are all NA
      setNames(headers_names)                                               # Fix awkward header names
    
    ## The names can't be hard-coded here because they change over files.
    data <- cbind(data[,1:8], select(data, starts_with(c("Ammonium", "Nitrate")))) # Select interesting columns 
    }) %>% 
  map(setNames, names(.[[1]])) %>%                                          # Overwrite the column names to match the first file (I checked the columns contain the same data)
  reduce(rbind) %>% 
  transmute("Longitude (E)_" = fill_in(Longitude_NA),                       # Fill in empty cells
            "Latitude (N)_" = fill_in(Latitude_NA),
            "CTD cast_" = fill_in(CTD_NA),
            Date_ = fill_in(as.character(`Date/time at bottom_NA`)),        # Change to character for easy binding to old data
            "Depth (m)_" = as.numeric(Depth_m),
            "Nitrate+nitrite_mM l-1" = as.numeric(`Nitrate+nitrite_mM l-1`),
            "Ammonium_mM l-1" = as.numeric(`Ammonium_mM l-1`)) %>% 
  mutate(Date_ = as.Date(Date_, format = "%Y-%m-%d")) %>%                   # Simplify date to character string for binding with new data
  mutate(Season = case_when(lubridate::month(Date_) %in% 5:8 ~ "summer",
                            lubridate::month(Date_) %in% c(1,2,11,12) ~ "winter",
                            T ~ "drop")) %>% 
rbind(manual_data) %>%                                                    # Add in first data file
  group_by(`CTD cast_`, Date_) %>%                                          # Per cast
  arrange(`Depth (m)_`, .by_group = TRUE) %>%                               # Order depths ascending
  drop_na(`Depth (m)_`) %>% 
  st_as_sf(coords = c("Longitude (E)_","Latitude (N)_"), crs = 4326) %>%    # Convert to sf for spatial filter later
  st_transform(crs = 3035)  

CAO_shallow_obs <- all_data %>% 
  mutate(weights = calculate_depth_share(`Depth (m)_`, min_depth = 0, max_depth = 60), # Calculate share of the depth column per sample
         Depth_layer = "Shallow")                                           # Label depth layer 

CAO_deep_obs <- all_data %>% 
  mutate(weights = calculate_depth_share(`Depth (m)_`, min_depth = 60, max_depth = 400), # Calculate share of the depth column per sample
         Depth_layer = "Deep")                                              # Label depth layer 

CAO_final <- rbind(CAO_shallow_obs, CAO_deep_obs) %>%                                   # Combine estimates
  filter(weights > 0) %>%                                                   # Drop samples outside the depth window
  group_by(`CTD cast_`, Date_, Season, Depth_layer) %>% 
  summarise(Ammonium = weighted.mean(`Ammonium_mM l-1`, weights, na.rm = T),           # Weighted averages
            N =  weighted.mean(`Nitrate+nitrite_mM l-1`, weights, na.rm = T),
            Samples = n()) %>%                                              # Number of samples contributing to each estimate
  ungroup() %>% 
  st_join(domains) %>%                                                      # Check which are in the model domain
  st_drop_geometry() %>%                                                    # Simplify the output
  drop_na(Shore) %>% 
  filter(Season != "drop") 

#### Norwegian polar institute data ####

Nor_data <- read.csv("./Data/Target/N-ICE2015_water_column_biogeochemistry_v1.csv") %>% 
  st_as_sf(coords = c(4, 5), crs = 4326) %>%    # Convert to sf for spatial filter later
  st_transform(crs = 3035) #%>%    
  #st_join(domains) %>%                                                      # Check which are in the model domain
  #drop_na(Shore)

ggplot() +
  geom_sf(data = domains) +
  geom_sf(data = Nor_data)

Nor_data2 <- read.csv("./Data/Target/2010-2013-nutrients-api-v1.tsv", sep = "\t") %>% 
  st_as_sf(coords = c(7, 8), crs = 4326) %>%    # Convert to sf for spatial filter later
  st_transform(crs = 3035) %>%    
  st_join(domains) %>%                                                      # Check which are in the model domain
  drop_na(Shore) %>% 
  st_drop_geometry() %>%                                                    # Simplify the output
  mutate(Date = as.Date(eventDate, format = "%Y-%m-%d")) %>%                   # Simplify date to character string for binding with new data
  mutate(Season = case_when(lubridate::month(Date) %in% 5:8 ~ "summer",
                            lubridate::month(Date) %in% c(1,2,11,12) ~ "winter",
                            T ~ "drop")) %>% 
  select(Depth = maximumDepthInMeters, ctdnr, Date, nitrate, ammonium, Season) %>% 
  group_by(ctdnr, Date) %>%                                                 # Per cast
  arrange(Depth, .by_group = TRUE) %>%                               # Order depths ascending
  drop_na(Depth) 

Nor_shallow_obs <- Nor_data2 %>% 
  mutate(weights = calculate_depth_share(Depth, min_depth = 0, max_depth = 60), # Calculate share of the depth column per sample
         Depth_layer = "Shallow")                                           # Label depth layer 

Nor_deep_obs <- Nor_data2 %>% 
  mutate(weights = calculate_depth_share(Depth, min_depth = 60, max_depth = 400), # Calculate share of the depth column per sample
         Depth_layer = "Deep")                                              # Label depth layer 

Nor_final <- rbind(Nor_shallow_obs, Nor_deep_obs) %>%                                   # Combine estimates
  filter(weights > 0) %>%                                                   # Drop samples outside the depth window
  group_by(ctdnr, Date, Season, Depth_layer) %>% 
  summarise(Ammonium = weighted.mean(ammonium, weights, na.rm = T),           # Weighted averages
            N =  weighted.mean(nitrate, weights, na.rm = T),
            Samples = n()) %>%                                              # Number of samples contributing to each estimate
  ungroup() %>% 
  filter(Season != "drop") 

ggplot() +
  geom_sf(data = domains) +
  geom_sf(data = Nor_data2)

#### ICES data ####

ICES <- read.csv("./Data/Target/ICES_N.csv") %>% 
  dplyr::select(Date = yyyy.mm.ddThh.mm,
         Depth = PRES..db.,
         N = NTRA..umol.l.,
         Station) %>% 
  filter(N != "<0.1") %>% 
  full_join(read.csv("./Data/Target/ICES_ammonia.csv") %>% 
              select(Date = yyyy.mm.ddThh.mm,
                     Depth = PRES..db.,
                     ammonium = AMON..umol.l.,
                     Station)) %>% 
  mutate(N = as.numeric(N),
         Date = as.Date(Date, format = "%Y-%m-%d")) %>%                   # Simplify date to character string for binding with new data
  mutate(Season = case_when(lubridate::month(Date) %in% 5:8 ~ "summer",
                            lubridate::month(Date) %in% c(1,2,11,12) ~ "winter",
                            T ~ "drop")) %>% 
  filter(Season != "drop") %>% 
  group_by(Station, Date) %>%                                          # Per cast
  arrange(Depth, .by_group = TRUE)                               # Order depths ascending

ICES_shallow_obs <- ICES %>% 
  mutate(weights = calculate_depth_share(Depth, min_depth = 0, max_depth = 60), # Calculate share of the depth column per sample
         Depth_layer = "Shallow")                                           # Label depth layer 

ICES_deep_obs <- ICES %>% 
  mutate(weights = calculate_depth_share(Depth, min_depth = 60, max_depth = 400), # Calculate share of the depth column per sample
         Depth_layer = "Deep")                                              # Label depth layer 

ICES_final <- rbind(ICES_shallow_obs, ICES_deep_obs) %>%                                   # Combine estimates
  filter(weights > 0) %>%                                                   # Drop samples outside the depth window
  group_by(Station, Date, Season, Depth_layer) %>% 
  summarise(Ammonium = weighted.mean(ammonium, weights, na.rm = T),           # Weighted averages
            N =  weighted.mean(N, weights, na.rm = T),
            Samples = n()) %>%                                              # Number of samples contributing to each estimate
  ungroup() %>% 
  filter(Season != "drop")

#### Combine ####

Summary <- bind_rows(CAO_final,
                 ICES_final,
                 Nor_final) %>% 
  group_by(Depth_layer, Season) %>%                                         # Decided not to group by shore because there were few inshore samples   
  summarise(Ammonium_casts = sum(!is.na(Ammonium)),                         # Number of CTD casts contributing to each estimate
            Nitrate_casts = sum(!is.na(N)),
            Ammonium_SD = radiant.data::weighted.sd(Ammonium, Samples, na.rm= T),                    # Calculate average, weighting by the number of samples
            Nitrate_SD = radiant.data::weighted.sd(N, Samples, na.rm = T),                                  # Calculate average, weighting by the number of samples
            Ammonium_umol.l = weighted.mean(Ammonium, Samples, na.rm= T),                    # Calculate average, weighting by the number of samples
            Nitrate_umol.l = weighted.mean(N, Samples, na.rm = T))                                  # Calculate average, weighting by the number of samples

write.csv(Summary, "./Objects/Nutrient target data.csv")

