## ---------------------------
##
## Purpose of script: Raw data exploration, and processing
##
## Author: Dr. Benedict Hogan, Princeton University 
##
## Date Created: 2023
##
## ---------------------------
##
## Notes: This script loads raw spectral data collected by Pawel Podkowa, and results in ./processed_spec.RDS,
## 	which stores the spectra, some extracted details about each spectrum (specimen, population), and human RGB
##  colors for each spectrum for plotting
## 
## ---------------------------

library(pavo)
library(dplyr)
library(tidyverse)

############# read raw data #############

specs <- pavo::getspec('../dat/specs_PABU/', lim = c(300, 700), decimal = ",")

# save as rds to save time in future
# saveRDS(specs, '../dat/spectra.rds') 
# specs <- readRDS('../dat/spectra.rds') # if one saved, load that

# explorespec(specs, by = 3) # take a look

############# Function to extract names/groups of spectra #############
# This allows us to easily connect a table of metadata to spectra by name

getnames <- function(specs){
  
  # Lookup encoding for paper from data encoding
  rename <- tibble(patch =c('neck', 'abdo', 'rump', 'side', 'back', 'head'),
                    patch_re = c('Upper breast', 'Belly', 'Rump', 'Coverts', 'Mantle', 'Nape'))
  
  # Lets functionalize this, since we also want to use it later on, after averaging spectra
  spec_names <- names(specs) 
  spec_names <- data.frame(spec_names) %>% 
    mutate(id = str_extract(spec_names,  '^[:alnum:]*')) %>%
    mutate(location = str_extract(spec_names, '(?<=[:punct:])[:alnum:]*(?=[:punct:])')) %>%
    mutate(patch = str_extract(spec_names, '(?<=[:punct:])[:alnum:]*(?=[:punct:]*[0-9]*$)')) %>%
    mutate(patch = str_extract(patch, '[A-z]*$')) %>%
    mutate(number = str_extract(spec_names, '[0-9]*$')) %>%
    mutate(group = paste0(id, ".", location, ".", patch)) %>%
    left_join(rename) # add non-encoded patch names
}

spec_names <- getnames(specs)

############## 08_16_23 Pawel suggested we omit:

# LSU14747.WEST.1neck
# CAS80251.EAST.4side
specs <- specs[,!str_detect(spec_names$group, 'LSU14747.WEST.1neck|CAS80251.EAST.4side')]
spec_names <- getnames(specs)

############# Avoiding assumptions about tidy data, check it #############

# check that there are indeed sets of 3 consecutive spectra for each 'group'
check <- data.frame(unclass(rle(spec_names$group))) %>%
  rename(group = values)

# Also check that there are -only- 3 for each group, no accidental duplication
check <- spec_names %>% 
  group_by(group) %>% 
  summarize(n = n()) %>%
  left_join(check) %>%
  filter(group != 'wl.NA.NA') # omit wl from checking, only expect 1 of these

# should both be true
all(check$lengths == 3)
all(check$n == 3)

rm(check)

############## Okay, given checks pass, lets process and aggregate #############

specs <- procspec(specs, fixneg = 'zero')
specs <- aggspec(specs, by = 3)

# lets re-extract names (not sure if guaranteed that pavo keeps order of original)
spec_names <- getnames(specs)

# explore those on what should correspond to id basis
# explorespec(specs, by = 6)

# summarize sample size
spec_names %>% 
  filter(spec_names != 'wl') %>%
  group_by(id) %>% 
  summarize(location = first(location)) %>% 
  group_by(location) %>% 
  summarize(n = n())
# okay, 39 east, 29 mid, 20 west - 88 individuals overall, matches expectations

# Get RGB representation of these points for plotting
specrgb <- data.frame(pavo::spec2rgb(specs)) %>%
  rownames_to_column(var = 'spec_names') %>%
  rename(specrgb = "pavo..spec2rgb.specs.") 

# Lets save this processed data out for use elsewhere
saveRDS(list(specs, spec_names, specrgb), 'processed_spec.RDS')





