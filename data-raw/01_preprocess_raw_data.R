# data-raw/01_preprocess_raw_data.R

library(sf)
library(dplyr)
library(stringr)
library(tools)

# Record processing date and data version
processing_date <- Sys.Date()
geofabric_version <- "3.3"  # Update this when source data version changes

#' Load and preprocess Geofabric data
#' 
#' @param data_dir Path to directory containing Geofabric .gdb files
#' @return List of preprocessed sf objects
preprocess_geofabric_data <- function(data_dir) {
  message("Loading raw data...")
  
  # Load raw data
  AWRADrainageDivision <- sf::st_read(
    dsn = file.path(data_dir, "HR_Regions_GDB/HR_Regions.gdb"),
    layer = "AWRADrainageDivision"
  )
  
  RiverRegion <- sf::st_read(
    dsn = file.path(data_dir, "HR_Regions_GDB/HR_Regions.gdb"),
    layer = "RiverRegion"
  )
  
  AHGFNetworkStream <- sf::st_read(
    dsn = file.path(data_dir, "SH_Network_GDB/SH_Network.gdb"),
    layer = "AHGFNetworkStream"
  )
  
  AHGFWaterbody <- sf::st_read(
    dsn = file.path(data_dir, "SH_Network_GDB/SH_Network.gdb"),
    layer = "AHGFWaterbody"
  )
  
  message("Cleaning geometries...")
  
  # Clean geometries
  AWRADrainageDivision <- st_make_valid(AWRADrainageDivision)
  RiverRegion <- st_make_valid(RiverRegion)
  AHGFNetworkStream <- st_make_valid(AHGFNetworkStream)
  AHGFWaterbody <- st_make_valid(AHGFWaterbody)
  
  # Function to standardize division names
  standardize_name <- function(x) {
    x %>%
      str_trim() %>%                                    # Remove leading/trailing whitespace
      str_replace_all("\\s*-\\s*", "-") %>%            # Standardize hyphens
      str_replace_all("\\s+", " ") %>%                 # Standardize internal spaces
      str_replace_all("\\((.+?)\\)", {                 # Handle parenthetical text
        function(match) {
          # Capitalize first letter of each word in parentheses, rest lowercase
          inner <- str_match(match, "\\((.+?)\\)")[2]
          sprintf("(%s)", str_to_title(tolower(inner)))
        }
      })
  }
  
  message("Standardizing division names...")
  
  # Create reference table of original names
  division_names <- data.frame(
    original_awra = unique(AWRADrainageDivision$Division),
    original_river = unique(RiverRegion$Division),
    stringsAsFactors = FALSE
  )
  
  # Print original names for verification
  message("\nOriginal division names:")
  message("\nAWRA Drainage Divisions:")
  print(sort(division_names$original_awra))
  message("\nRiver Regions:")
  print(sort(unique(RiverRegion$Division)))
  
  # Update names in both datasets
  AWRADrainageDivision <- AWRADrainageDivision %>%
    mutate(
      Division_orig = Division,
      Division = standardize_name(Division)
    )
  
  RiverRegion <- RiverRegion %>%
    mutate(
      Division_orig = Division,
      Division = standardize_name(Division)
    )
  
  # Validate standardization
  message("\nValidating division name standardization...")
  awra_divisions <- sort(unique(AWRADrainageDivision$Division))
  river_divisions <- sort(unique(RiverRegion$Division))
  
  # Print standardized names for verification
  message("\nStandardized division names:")
  message("\nAWRA Drainage Divisions:")
  print(awra_divisions)
  message("\nRiver Regions:")
  print(river_divisions)
  
  # Print standardization results
  name_comparison <- data.frame(
    Division = unique(c(awra_divisions, river_divisions)),
    In_AWRA = unique(c(awra_divisions, river_divisions)) %in% awra_divisions,
    In_River = unique(c(awra_divisions, river_divisions)) %in% river_divisions
  ) %>%
    arrange(Division)
  
  # Check for mismatches
  mismatches <- name_comparison %>%
    filter(!(In_AWRA & In_River))
  
  if (nrow(mismatches) > 0) {
    warning("Found division name mismatches after standardization:")
    print(mismatches)
    
    # Add detailed mismatch analysis
    if (any(!mismatches$In_AWRA)) {
      message("\nNames only in RiverRegion:")
      missing_awra <- mismatches %>%
        filter(!In_AWRA) %>%
        pull(Division)
      for (name in missing_awra) {
        message(sprintf("  %s\n    Original AWRA names:", name))
        print(division_names$original_awra[grep(name, standardize_name(division_names$original_awra), ignore.case = TRUE)])
      }
    }
    
    if (any(!mismatches$In_River)) {
      message("\nNames only in AWRADrainageDivision:")
      missing_river <- mismatches %>%
        filter(!In_River) %>%
        pull(Division)
      for (name in missing_river) {
        message(sprintf("  %s\n    Original River Region names:", name))
        print(division_names$original_river[grep(name, standardize_name(division_names$original_river), ignore.case = TRUE)])
      }
    }
  }
  
  # Create enhanced mapping file
  division_mapping <- data.frame(
    awra_original = division_names$original_awra,
    awra_standardized = standardize_name(division_names$original_awra),
    river_original = division_names$original_river,
    river_standardized = standardize_name(division_names$original_river),
    matched = standardize_name(division_names$original_awra) %in% 
      standardize_name(division_names$original_river)
  ) %>%
    arrange(awra_standardized)
  
  # Basic validation
  message("\nPerforming data validation...")
  
  validation_results <- list(
    n_divisions = length(unique(AWRADrainageDivision$Division)),
    n_river_regions = nrow(RiverRegion),
    n_streams = nrow(AHGFNetworkStream),
    n_waterbodies = nrow(AHGFWaterbody),
    divisions_match = all(sort(unique(AWRADrainageDivision$Division)) == 
                            sort(unique(RiverRegion$Division))),
    crs_match = all(st_crs(AWRADrainageDivision) == st_crs(RiverRegion) &&
                      st_crs(RiverRegion) == st_crs(AHGFNetworkStream) &&
                      st_crs(AHGFNetworkStream) == st_crs(AHGFWaterbody))
  )
  
  # Create summary of river regions by division
  division_summary <- RiverRegion %>%
    group_by(Division) %>%
    summarize(
      n_regions = n(),
      region_names = paste(sort(unique(RivRegName)), collapse = "; ")
    )
  
  # Save validation report
  validation_report <- sprintf(
    "Geofabric Data Preprocessing Report
    
    Processing Date: %s
    Geofabric Version: %s
    
    Summary Statistics:
    - Number of Drainage Divisions: %d
    - Number of River Regions: %d
    - Number of Stream Segments: %d
    - Number of Waterbodies: %d
    
    Validation Checks:
    - Division names match between datasets: %s
    - CRS consistent across all datasets: %s
    
    Division Name Standardization:
    %s
    
    River Regions by Division:
    %s",
    processing_date,
    geofabric_version,
    validation_results$n_divisions,
    validation_results$n_river_regions,
    validation_results$n_streams,
    validation_results$n_waterbodies,
    ifelse(validation_results$divisions_match, "PASS", "FAIL"),
    ifelse(validation_results$crs_match, "PASS", "FAIL"),
    paste(capture.output(print(division_mapping)), collapse = "\n"),
    paste(capture.output(print(division_summary)), collapse = "\n")
  )
  
  # Create data-raw directory if it doesn't exist
  dir.create("data-raw/validation", showWarnings = FALSE, recursive = TRUE)
  
  # Save validation artifacts
  writeLines(validation_report, "data-raw/validation/preprocessing_report.txt")
  write.csv(division_mapping, "data-raw/validation/division_name_mapping.csv", 
            row.names = FALSE)
  write.csv(division_summary, "data-raw/validation/division_summary.csv",
            row.names = FALSE)
  
  message("\nPreprocessing complete. See data-raw/validation/ for detailed report.")
  
  # Return preprocessed data
  list(
    AWRADrainageDivision = AWRADrainageDivision,
    RiverRegion = RiverRegion,
    AHGFNetworkStream = AHGFNetworkStream,
    AHGFWaterbody = AHGFWaterbody,
    division_mapping = division_mapping,
    validation_results = validation_results,
    division_summary = division_summary
  )
}

# Run preprocessing
dataDir <- "/Users/brau0037/spatial_data/spatial_geofabric_ftp/"
processed_data <- preprocess_geofabric_data(dataDir)

# Access individual datasets
AWRADrainageDivision <- processed_data$AWRADrainageDivision
RiverRegion <- processed_data$RiverRegion
AHGFNetworkStream <- processed_data$AHGFNetworkStream
AHGFWaterbody <- processed_data$AHGFWaterbody


# Save preprocessed data for next steps
save(processed_data,
     file = "/Users/brau0037/spatial_data/spatial_geofabric_ftp/preprocessed_data.RData")
remove(processed_data)
gc()

# restart R to purge memory allocation
rstudioapi::restartSession()


# split data by drainage division
# Create directory for split data
dir.create("inst/extdata/drainage_divisions", recursive = TRUE, showWarnings = FALSE)

# Process drainage divisions
drainage_divisions <- unique(AWRADrainageDivision$Division)
# Create division metadata with basic counts
# Create division metadata with basic counts
division_metadata <- data.frame(
  division = drainage_divisions,
  processed_date = processing_date,
  river_regions = NA_integer_,
  n_waterbodies = NA_integer_,
  n_streams = NA_integer_,
  bbox = NA_character_,
  stringsAsFactors = FALSE
)


# Process each division
for (i in seq_along(drainage_divisions)) {
  div <- drainage_divisions[i]
  message("Processing metadata for division: ", div)
  
  # Get division boundary and ensure valid geometry
  div_boundary <- AWRADrainageDivision[AWRADrainageDivision$Division == div,] %>%
    st_make_valid()
  
  # Get river regions in this division and ensure valid geometry
  div_regions <- RiverRegion[RiverRegion$Division == div,] %>%
    st_make_valid()
  
  # Create division polygon for spatial operations with validation
  div_polygon <- div_regions %>%
    st_union() %>%
    st_make_valid()
  
  # Ensure valid geometries for feature layers within this division's extent
  div_bbox <- st_bbox(div_polygon)
  
  # Count features with geometry validation
  division_metadata$river_regions[i] <- nrow(div_regions)
  
  # Process waterbodies
  waterbodies_subset <- AHGFWaterbody[st_intersects(AHGFWaterbody, st_buffer(div_polygon, 0), sparse = FALSE),] %>%
    st_make_valid()
  division_metadata$n_waterbodies[i] <- nrow(waterbodies_subset)
  
  # Process streams
  streams_subset <- AHGFNetworkStream[st_intersects(AHGFNetworkStream, st_buffer(div_polygon, 0), sparse = FALSE),] %>%
    st_make_valid()
  division_metadata$n_streams[i] <- nrow(streams_subset)
  
  # Get bounding box
  division_metadata$bbox[i] <- paste(round(st_bbox(div_boundary), 3), collapse = ",")
  
  # Clear memory
  rm(waterbodies_subset, streams_subset)
  gc()
}

# Sort by number of river regions
division_metadata <- division_metadata %>%
  arrange(desc(river_regions))

print(division_metadata %>%
        select(division, river_regions, n_waterbodies, n_streams))

# Save metadata
saveRDS(division_metadata, "data-raw/validation/division_metadata.rds")

# Create base directory structure first
base_dir <- "inst/extdata/drainage_divisions"
dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

# Process and save data by division
for (division in drainage_divisions) {
  message("Processing ", division)
  
  # Create standardized suffix from division name
  division_suffix <- division %>%
    stringr::str_replace_all("[[:space:]-]", "_") %>%   # Replace spaces and hyphens with underscores
    stringr::str_replace_all("[^[:alnum:]_]", "") %>%   # Remove any other non-alphanumeric characters
    tolower()                                           # Convert to lowercase
  
  # Create division directory
  division_dir <- file.path(base_dir, division_suffix)
  dir.create(division_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Verify directory creation
  if (!dir.exists(division_dir)) {
    stop("Failed to create directory: ", division_dir)
  }
  
  message("  Created directory: ", division_dir)
  
  # Process data with tryCatch for error handling
  tryCatch({
    # Process AWRADrainageDivision
    AWRADrainageDivision_subset <- AWRADrainageDivision[AWRADrainageDivision$Division == division,] %>%
      st_make_valid()
    
    # Get RiverRegion data and create union for spatial operations
    RiverRegion_subset <- RiverRegion[RiverRegion$Division == division,] %>%
      st_make_valid()
    
    division_polygon <- RiverRegion_subset %>%
      st_union() %>%
      st_make_valid()
    
    # Process AHGFNetworkStream
    message("  Processing AHGFNetworkStream...")
    AHGFNetworkStream_subset <- AHGFNetworkStream[st_intersects(AHGFNetworkStream, 
                                                                st_buffer(division_polygon, 0), 
                                                                sparse = FALSE),] %>%
      st_make_valid()
    
    # Process AHGFWaterbody
    message("  Processing AHGFWaterbody...")
    AHGFWaterbody_subset <- AHGFWaterbody[st_intersects(AHGFWaterbody, 
                                                        st_buffer(division_polygon, 0), 
                                                        sparse = FALSE),] %>%
      st_make_valid()
    
    # Define file paths
    awra_file <- file.path(division_dir, paste0("AWRADrainageDivision_", division_suffix, ".rds"))
    river_file <- file.path(division_dir, paste0("RiverRegion_", division_suffix, ".rds"))
    stream_file <- file.path(division_dir, paste0("AHGFNetworkStream_", division_suffix, ".rds"))
    waterbody_file <- file.path(division_dir, paste0("AHGFWaterbody_", division_suffix, ".rds"))
    
    # Save files with message output
    message("  Saving AWRADrainageDivision...")
    saveRDS(AWRADrainageDivision_subset, awra_file, compress = "xz")
    
    message("  Saving RiverRegion...")
    saveRDS(RiverRegion_subset, river_file, compress = "xz")
    
    message("  Saving AHGFNetworkStream...")
    saveRDS(AHGFNetworkStream_subset, stream_file, compress = "xz")
    
    message("  Saving AHGFWaterbody...")
    saveRDS(AHGFWaterbody_subset, waterbody_file, compress = "xz")
    
    # Create division README
    readme_content <- sprintf(
      "Drainage Division: %s\nProcessed: %s\n\nFeature Counts:\n--------------\nAWRADrainageDivision: %d\nRiverRegion: %d\nAHGFNetworkStream: %d\nAHGFWaterbody: %d\n\nGeometry Validation:\n------------------\nAll geometries checked and validated during processing.\n\nRiver Regions:\n%s",
      division,
      Sys.Date(),
      nrow(AWRADrainageDivision_subset),
      nrow(RiverRegion_subset),
      nrow(AHGFNetworkStream_subset),
      nrow(AHGFWaterbody_subset),
      paste("-", sort(unique(RiverRegion_subset$RivRegName)), collapse = "\n")
    )
    
    writeLines(readme_content, file.path(division_dir, "README.txt"))
    
    message("  Successfully processed and saved data for: ", division)
    
  }, error = function(e) {
    message("Error processing division ", division, ": ", e$message)
  }, finally = {
    # Clean up to manage memory
    rm(list = ls(pattern = "_subset$|^division_polygon$"))
    gc()
  })
}





