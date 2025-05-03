rm(list = ls())

library(readxl)
library(dplyr)


traits_output = "data/02_b_traits-clean_data.RData"

names_of_traits = c(
  "Family3",
  "Order3",
  "Mass", # Min.=5.54, Median=16.06, Mean=261.85, Max.=2716.61
  "Wing.Length", # Min.=47.70, Median=101.20, Mean=155.60, Max.=481.50
  "Hand.Wing.Index", # Min.=16.20, Median=30.30, Mean=31.31, Max.=71.60
  "Habitat", # = Woodland, Forest, Grassland, Human Modified, Rock, Shrubland, Wetland, Riverine
  "Trophic.Level", # = Herbivore, Omnivore, Carnivore
  "Trophic.Niche", # = Omnivore, Invertivore, Vertivore, Granivore, Herbivore terrestrial, Aquatic predator, Herbivore aquatic
  "Primary.Lifestyle" # = Terrestrial, Insessorial, Generalist, Aerial, Aquatic
)




# Import AVONET Database --------------------------------------------------

# We use the AVONET database for the functional traits of the species: 
# AVONET - Tobias 2022 supplementary material 1
# -> AVONET: morphological, ecological and geographical data for all birds
#    (Tobias et al 2022 Ecology Letters doi: https://doi.org/10.111/ele.13898)
# Available at https://figshare.com/s/b990722d72a26b5bfead?file=34480856

# To avoid storing the entire Excel, we only store the useful information in a csv
# (the sheet "AVONET3_BirdTree" of the Excel file)

# # ________________________________________
# # You can run the following lines to download AVONET supplementary material 1
# tryCatch({
#   url_excel_download = "https://figshare.com/ndownloader/files/34480856?private_link=b990722d72a26b5bfead"
#   destination_file = "data/02_a_traits-AVONET_supplementary_dataeset_1.xlsx"
#   download.file(url_excel_download, destfile = destination_file)
#   cat("File downloaded successfully!\n")
# }, error = function(e) {
#   cat("Error downloading file:\n", e$message, "\n")
# })
# # And these lines to extract the sheet "AVONET3_BirdTree" from the Excel file
# # and save it as a csv file
# data_avonet = read_excel("data/02_a_traits-AVONET_supplementary_dataeset_1.xlsx",
#                          sheet = "AVONET3_BirdTree")
# write.csv(data_avonet, "data/02_a_traits-AVONET_supplementary_dataeset_1_AVONET3_BirdTree.csv",
#           row.names=FALSE)
# # ________________________________________


# If you already have the file, you can use the following lines to import it
# data_avonet = read.csv("data/02_a_AVONET_supplementary_dataeset_1_AVONET3_BirdTree.csv")
data_avonet = read.csv("../20230412_freiburg_mscthesis_data_analysis/data/species_data/1-AVONET_suppl1_AVONET3_BirdTree.csv")



# Match the species -------------------------------------------------------


# Get the species names expected in the region
# 02_b_species_expected_in_surveys.csv is a csv file, where each row is a species, 
# and the columns are "english.name", "scientific.name" and "rarety.in.CFB" 
# (a description of how rare the species is in the region of ConFoBi plots)
expected_species = read.csv("data/02_a_species_expected_in_surveys.csv", sep = ";")

# Check the species not found in AVONET
species_not_found <- setdiff(expected_species$scientific.name, data_avonet$Species3)

expected_species %>% 
  filter(scientific.name %in% species_not_found)


# NB: For each species, we check if names in AVONET could match our species by
# Looking at family or similar species names. 


# * * * Coal Tit - Periparus ater * * *

# Our scienctific name
expected_species[expected_species$english.name == "Coal Tit", 2:3]
# Species in AVONET containing 'Paridae' in the family name, and 'ater' in the species name
data_avonet %>%
  filter( grepl('Paridae', Family3) & grepl('ater', Species3) ) %>%
  select(Species3, Family3, Order3)

# We replace 'Parus ater' by 'Periparus ater' in data_avonet
data_avonet[data_avonet$Species3 == "Parus ater", "Species3"] = "Periparus ater"


# * * * Blue Tit - Cyanistes caeruleus * * *

# Our scienctific name
expected_species[expected_species$english.name == "Blue Tit", 2:3]
# Species in AVONET containing 'Paridae' in the family name, and 'caer' or 'yan' in the species name
data_avonet %>% 
  filter( grepl('Paridae', Family3) & ( grepl('caer', Species3) | # blue tit
                                        grepl( 'yan', Species3)  ) ) %>% # azure tit
  select(Species3, Family3, Order3)

# We replace 'Parus caeruleus' by 'Cyanistes caeruleus' in data_avonet
data_avonet[data_avonet$Species3 == "Parus caeruleus", "Species3"] = "Cyanistes caeruleus"


# * * * Crested Tit - Lophophanes cristatus * * *

# Our scienctific name
expected_species[expected_species$english.name == "Crested Tit", 2:3]
# Species in AVONET containing 'Paridae' in the family name, and 'crist' or 'ophoph' in the species name
data_avonet %>% 
  filter( grepl('Paridae', Family3) & ( grepl( 'crist', Species3) | 
                                        grepl('ophoph', Species3)  ) ) %>%
  select(Species3, Family3, Order3)

# We replace 'Parus cristatus' by 'Lophophanes cristatus' in data_avonet
data_avonet[data_avonet$Species3 == "Parus cristatus", "Species3"] = "Lophophanes cristatus"


# * * * Marsh Tit - Poecile palustris * * *

# Our scienctific name
expected_species[expected_species$english.name == "Marsh Tit", 2:3]
# Species in AVONET containing 'Paridae' in the family name, and 'oeci' or 'alust' in the species name
data_avonet %>% 
  filter( grepl('Paridae', Family3) & ( grepl( 'oeci', Species3) |
                                        grepl('alust', Species3)  ) ) %>%
  select(Species3, Family3, Order3)

# We replace 'Parus palustris' by 'Poecile palustris' in data_avonet
data_avonet[data_avonet$Species3 == "Parus palustris", "Species3"] = "Poecile palustris"


# * * * Middle Spotted Woodpecker - Dendrocoptes medius * * *

# Our scienctific name
expected_species[expected_species$english.name == "Middle Spotted Woodpecker", 2:3]
# Species in AVONET containing 'Picidae' in the family name, and 'medi' in the species name
data_avonet %>% 
  filter( grepl('Picidae', Family3) & grepl('medi', Species3) ) %>%
  select(Species3, Family3, Order3)

# We replace 'Dendrocopos medius' by 'Dendrocoptes medius' in data_avonet
data_avonet[data_avonet$Species3 == "Dendrocopos medius", "Species3"] = "Dendrocoptes medius"


# * * * Eurasian Siskin - Spinus spinus * * *

# Our scienctific name
expected_species[expected_species$english.name == "Eurasian Siskin", 2:3]
# Species in AVONET containing 'Fringillidae' in the family name, and 'pinus' in the species name
data_avonet %>% 
  filter( grepl('Fringillidae', Family3) & grepl('pinus', Species3) ) %>% # Pine siskin = Carduelis pinus
  select(Species3, Family3, Order3)

# We replace 'Carduelis spinus' by 'Spinus spinus' in data_avonet
data_avonet[data_avonet$Species3 == "Carduelis spinus", "Species3"] = "Spinus spinus"


# * * * Lesser Spotted Woodpecker - Dryobates minor * * *

# Our scienctific name
expected_species[expected_species$english.name == "Lesser Spotted Woodpecker", 2:3]
# Species in AVONET containing 'Picidae' in the family name, and 'minor' in the species name
data_avonet %>% 
  filter( grepl('Picidae', Family3) & grepl('minor', Species3) ) %>%
  select(Species3, Family3, Order3)

# We replace 'Dendrocopos minor' by 'Dryobates minor' in data_avonet
data_avonet[data_avonet$Species3 == "Dendrocopos minor", "Species3"] = "Dryobates minor"


# * * * Willow Tit - Poecile montanus * * *

# Our scienctific name
expected_species[expected_species$english.name == "Willow Tit", 2:3]
# Species in AVONET containing 'Paridae' in the family name, and 'monta' in the species name
data_avonet %>% 
  filter( grepl('Paridae', Family3) & grepl('monta', Species3) ) %>%
  select(Species3, Family3, Order3)

# We replace 'Parus montanus' by 'Poecile montanus' in data_avonet
data_avonet[data_avonet$Species3 == "Parus montanus", "Species3"] = "Poecile montanus"


# * * * Linnet - Linaria cannabina * * *

# Our scienctific name
expected_species[expected_species$english.name == "Linnet", 2:3]
# Species in AVONET containing 'Fringillidae' in the family name, and 'canna' in the species name
data_avonet %>% 
  filter( grepl('Fringillidae', Family3) & grepl('canna', Species3) ) %>%
  select(Species3, Family3, Order3)

# We replace 'Carduelis cannabina' by 'Linaria cannabina' in data_avonet
data_avonet[data_avonet$Species3 == "Carduelis cannabina", "Species3"] = "Linaria cannabina"


# * * * Greenfinch - Chloris chloris * * *

# Our scienctific name
expected_species[expected_species$english.name == "Greenfinch", 2:3]
# Species in AVONET containing 'Fringillidae' in the family name, and 'chlo' in the species name
data_avonet %>% 
  filter( grepl('Fringillidae', Family3) & grepl('chlo', Species3) ) %>%
  select(Species3, Family3, Order3)

# We replace 'Carduelis chloris' by 'Chloris chloris' in data_avonet
data_avonet[data_avonet$Species3 == "Carduelis chloris", "Species3"] = "Chloris chloris"


# * * * Hooded Crow - Corvus cornix * * *

# Our scienctific name
expected_species[expected_species$english.name == "Hooded Crow", 2:3]
# Species in AVONET containing 'Fringillidae' in the family name, and 'chlo' in the species name
data_avonet %>% 
  filter( grepl('Corvidae', Family3) & grepl('Corvus', Species3) ) %>%
  select(Species3, Family3, Order3) # We take corvus corone...

# We create a new row for 'Corvus cornix' in data_avonet based on 'Corvus corone'
new_row_for_cornix = data_avonet[data_avonet$Species3 == "Corvus corone", ]
new_row_for_cornix["Species3"] = "Corvus cornix"
data_avonet = rbind(data_avonet, new_row_for_cornix)




# Clean the dataset -------------------------------------------------------

traits_data <- expected_species %>%  left_join(
  data_avonet, by=c('scientific.name'='Species3')
)

# * * * Convert some columns to factors * * *

# Family3
traits_data$Family3 = as.factor(traits_data$Family3)

# Order3
traits_data$Order3 = as.factor(traits_data$Order3)

# Habitat
traits_data$Habitat = factor(
  traits_data$Habitat, 
  levels=c("Forest", "Woodland", "Shrubland", "Grassland", "Rock", "Human Modified")
)

# Trophic Level
traits_data$Trophic.Level = factor(
  traits_data$Trophic.Level, 
  levels=c("Herbivore", "Omnivore", "Carnivore")
)

# Trophic Niche
traits_data$Trophic.Niche = factor(
  traits_data$Trophic.Level, 
  levels=c("Omnivore", "Invertivore", "Vertivore", "Granivore")
)

# Primaru Lifestyle
traits_data$Primary.Lifestyle = factor(
  traits_data$Primary.Lifestyle, 
  levels=c("Terrestrial", "Insessorial", "Generalist")
)



# * * * Select the columns we want to keep * * *

traits_data = traits_data[, c("english.name", "scientific.name", names_of_traits) ]

# * * * Rename the columns * * *

traits_data = traits_data %>% 
  rename(Family = Family3, Order = Order3)

# * * * Give rownames to the dataframe * * *

rownames(traits_data) = traits_data$english.name


# * * * Check and export the dataframe * * *
str(traits_data)
# View(traits_data)

save(traits_data, file = traits_output) 


