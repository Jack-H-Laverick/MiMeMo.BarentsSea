
#### Setup ####

rm(list=ls(all.names = TRUE))                                               # Wipe the brain

library(MiMeMo.tools)

domains <- readRDS("./Objects/Domains.rds")                                 # Import model domain

#### Original manually corrected file ####

headers <- read.csv("./Data/BODC/JR016-006_CTD_nutrient_data_summary_BODC_fixed.csv", nrows=2, header=FALSE)
headers_names <- sapply(headers,paste,collapse="_")                         # Rebuild column names because they're split over 2 rows

manual_data <- read.csv(file="./Data/BODC/JR016-006_CTD_nutrient_data_summary_BODC_fixed.csv", skip = 2, 
                 header=FALSE) %>% 
  setNames(headers_names) %>%                                               # Fix awkward header names
  select(`Longitude (E)_`, `Latitude (N)_`, `CTD cast_`,                    # Keep interesting columnd
         `Date_`, `Depth (m)_`, `Nitrate+nitrite_mM l-1`,
         `Ammonium_mM l-1`) %>% 
  mutate(Date_ = as.character(Date_))                                       # Simplify date to character string for binding with new data

#### New raw data ####

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
  rbind(manual_data) %>%                                                    # Add in first data file
  drop_na() %>%                                                             # Only use complete cases
  st_as_sf(coords = c("Longitude (E)_","Latitude (N)_"), crs = 4326) %>%    # Convert to sf for spatial filter later
  st_transform(crs = 3035) %>%  
  group_by(`CTD cast_`, Date_) %>%                                          # Per cast
  arrange(`Depth (m)_`, .by_group = TRUE)                                   # Order depths ascending

#### Calculate proportion ####

shallow_proportion <- all_data %>% 
  mutate(weights = calculate_depth_share(`Depth (m)_`, min_depth = 0, max_depth = 60), # Calculate share of the depth column per sample
         Depth_layer = "Shallow")                                           # Label depth layer 

deep_proportion <- all_data %>% 
  mutate(weights = calculate_depth_share(`Depth (m)_`, min_depth = 60, max_depth = 400), # Calculate share of the depth column per sample
         Depth_layer = "Deep")                                              # Label depth layer 

final <- rbind(shallow_proportion, deep_proportion) %>%                     # Combine estimates
  filter(weights > 0) %>%                                                   # Drop samples outside the depth window
  group_by(`CTD cast_`, Date_, Depth_layer) %>% 
  summarise(Ammonium = weighted.mean(`Ammonium_mM l-1`, weights),           # Weighted averages
            DIN =  weighted.mean(`Nitrate+nitrite_mM l-1` +`Ammonium_mM l-1`, weights),
            Samples = n()) %>%                                              # Number of samples contributing to each estimate
  ungroup() %>% 
  mutate(Proportion = Ammonium/DIN) %>%                                     # Get a proportion of ammonium to total DIN
  st_join(domains) %>%                                                      # Check which are in the model domain
  st_drop_geometry() %>%                                                    # Simplify the output
  drop_na() %>% 
  group_by(Depth_layer) %>%                                                 # Decided not to group by shore because there were few inshore samples   
  summarise(Proportion = weighted.mean(Proportion, Samples),                # Calculate average, weighting by the number of samples
            Casts = n())                                                    # Number of CTD casts contributing to each estimate

saveRDS(final, "./Objects/Ammonia to DIN.rds")
