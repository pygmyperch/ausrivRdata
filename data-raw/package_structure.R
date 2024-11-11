# Package structure:
#
# ausrivRdata/              # Root directory
# ├── data-raw/            # Scripts to process raw data (not included in package)
# │   ├── create_data.R    # Main data processing script
# │   └── README.md        # Documentation of data processing
# ├── R/                   # R functions
# ├── data/                # Processed .rda files
# ├── inst/                # Installed files
# │   └── extdata/         # External data files
# ├── man/                 # Documentation
# ├── DESCRIPTION
# └── NAMESPACE

setwd("/Users/brau0037/Library/CloudStorage/GoogleDrive-pygmyperch@gmail.com/My Drive/git_repos/ausrivRdata")

# Create the structure using usethis:
usethis::use_data_raw()  # This creates data-raw/ and adds it to .Rbuildignore

# pre-process the geofabric data:
# ~/CloudStorage/GoogleDrive-pygmyperch@gmail.com/My Drive/git_repos/ausrivRdata/data-raw/01_preprocess_raw_data.R

setwd("/Users/brau0037/Library/CloudStorage/GoogleDrive-pygmyperch@gmail.com/My Drive/git_repos/ausrivRdata")


# Create directory for split data
dir.create("inst/extdata/drainage_divisions", recursive = TRUE, showWarnings = FALSE)

# Process drainage divisions
drainage_divisions <- unique(AWRADrainageDivision$Division)
division_metadata <- data.frame(
  division = drainage_divisions,
  processed_date = processing_date,
  river_regions = sapply(drainage_divisions, function(div) {
    sum(RiverRegion$Division == div)
  }),
  bbox = sapply(drainage_divisions, function(div) {
    div_data <- AWRADrainageDivision[AWRADrainageDivision$Division == div,]
    paste(round(st_bbox(div_data), 3), collapse = ",")
  })
)

# Save metadata
saveRDS(division_metadata, 
        "inst/extdata/division_metadata.rds", 
        compress = "xz")

# Process and save data by division
for (div in drainage_divisions) {
  message("Processing division: ", div)
  
  # Create safe filename
  safe_name <- gsub("[^[:alnum:]]", "_", div)
  div_dir <- file.path("inst/extdata/drainage_divisions", safe_name)
  dir.create(div_dir, showWarnings = FALSE)
  
  # Save division boundary
  div_boundary <- AWRADrainageDivision[AWRADrainageDivision$Division == div,]
  saveRDS(div_boundary, 
          file.path(div_dir, "boundary.rds"), 
          compress = "xz")
  
  # Save river regions
  div_regions <- RiverRegion[RiverRegion$Division == div,]
  saveRDS(div_regions, 
          file.path(div_dir, "river_regions.rds"), 
          compress = "xz")
  
  # Save network if applicable
  if ("Division" %in% names(SH_Network)) {
    div_network <- SH_Network[SH_Network$Division == div,]
    saveRDS(div_network, 
            file.path(div_dir, "network.rds"), 
            compress = "xz")
  }
  
  # Create division-specific metadata
  writeLines(
    sprintf("Data processed on: %s\nNumber of river regions: %d",
            processing_date, nrow(div_regions)),
    file.path(div_dir, "README.txt")
  )
}

# Create simplified overview data for package loading
division_overview <- AWRADrainageDivision %>%
  select(Division, DivisionNo) %>%
  st_simplify(dTolerance = 0.01)

# Save using usethis to properly set up lazy loading
usethis::use_data(division_overview, overwrite = TRUE)

# Document the process
writeLines(
  sprintf("# Data Processing Log\n\nProcessed on: %s\n\nDrainage Divisions:\n%s",
          processing_date,
          paste("*", drainage_divisions, collapse = "\n")),
  "data-raw/README.md"
)