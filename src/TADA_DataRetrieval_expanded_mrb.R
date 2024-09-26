startDate = "2018-10-01"
# startDate = "2022-09-30"
endDate = "2022-09-30"
statecode = "UT"
maxrecs <- 250000









# Check for incomplete or inconsistent inputs -----------------------------

# If both an sf object and tribe information are provided it's unclear what
# the priority should be for the query
if( !is.null(aoi_sf) &
    ( (tribal_area_type != "null") | (tribe_name_parcel != "null") ) ){
  stop(
    paste0(
      "Both sf data and tribal information have been provided. ",
      "Please use only one of these query options."
    )
  )
} 

# Check for other arguments that indicate location. Function will ignore
# these inputs but warn the user
if( 
  # sf object provided
  (!is.null(aoi_sf) & inherits(aoi_sf, "sf")) & 
  # with additional location info
  any( (countrycode != "null"), (countycode != "null"), (huc != "null"),
       (siteid != "null"), (statecode != "null") )
){
  warning(
    paste0(
      "Location information has been provided in addition to an sf object. ",
      "Only the sf object will be used in the query."
    )
  )
} else if(
  # Tribe info provided
  (tribal_area_type != "null") & 
  # with additional location info
  any( (countrycode != "null"), (countycode != "null"), (huc != "null"),
       (siteid != "null"), (statecode != "null") )
){
  warning(
    paste0(
      "Location information has been provided in addition to tribal information. ",
      "Only the tribal information will be used in the query."
    )
  )
}

# Insufficient tribal info provided
if( (tribal_area_type == "null") & (tribe_name_parcel != "null") ){
  stop("A tribal_area_type is required if tribe_name_parcel is provided.")
}


# Note query type ---------------------------------------------------------

# Note what kind of query this will be, to reduce code needed later:
if ( (tribal_area_type != "null") | (tribe_name_parcel != "null") ) {
  query_type <- "tribal"
} else if ( !is.null(aoi_sf) ) {
  query_type <- "spatial"
} else if (
  # Non-spatial & non-tribal
  any( (countrycode != "null"), (countycode != "null"), (huc != "null"),
       (siteid != "null"), (statecode != "null") ) & 
  is.null(aoi_sf) &
  (tribal_area_type != "null") &
  (tribe_name_parcel != "null") ) {
  query_type <- "standard"
} else {
  # Stop to check inputs
  stop("Unexpected combination of WQP inputs.")
}


# Prepare basic query components ------------------------------------------

# Set query parameters
WQPquery <- list()

# Build the non-sf part of the query:
# StartDate
if (length(startDate) > 1) {
  if (is.na(suppressWarnings(lubridate::parse_date_time(startDate[1], orders = "ymd")))) {
    stop("Incorrect date format. Please use the format YYYY-MM-DD.")
  }
  WQPquery <- c(WQPquery, startDate = list(startDate))
} else if (startDate != "null") {
  if (is.na(suppressWarnings(lubridate::parse_date_time(startDate, orders = "ymd")))) {
    stop("Incorrect date format. Please use the format YYYY-MM-DD.")
  }
  WQPquery <- c(WQPquery, startDate = startDate)
}
# SiteType
if (length(siteType) > 1) {
  WQPquery <- c(WQPquery, siteType = list(siteType))
} else if (siteType != "null") {
  WQPquery <- c(WQPquery, siteType = siteType)
}
# CharacteristicName
if (length(characteristicName) > 1) {
  WQPquery <- c(WQPquery, characteristicName = list(characteristicName))
} else if (characteristicName != "null") {
  WQPquery <- c(WQPquery, characteristicName = characteristicName)
}
# CharacteristicType
if (length(characteristicType) > 1) {
  WQPquery <- c(WQPquery, characteristicType = list(characteristicType))
} else if (characteristicType != "null") {
  WQPquery <- c(WQPquery, characteristicType = characteristicType)
}
# SampleMedia
if (length(sampleMedia) > 1) {
  WQPquery <- c(WQPquery, sampleMedia = list(sampleMedia))
} else if (sampleMedia != "null") {
  WQPquery <- c(WQPquery, sampleMedia = sampleMedia)
}
# Project
if (length(project) > 1) {
  WQPquery <- c(WQPquery, project = list(project))
} else if (project != "null") {
  WQPquery <- c(WQPquery, project = project)
}
# Provider
if (length(providers) > 1) {
  WQPquery <- c(WQPquery, providers = list(providers))
} else if (providers != "null") {
  WQPquery <- c(WQPquery, providers = providers)
}
# Organization
if (length(organization) > 1) {
  WQPquery <- c(WQPquery, organization = list(organization))
} else if (organization != "null") {
  WQPquery <- c(WQPquery, organization = organization)
}
# EndDate
if (length(endDate) > 1) {
  if (is.na(suppressWarnings(lubridate::parse_date_time(endDate[1], orders = "ymd")))) {
    stop("Incorrect date format. Please use the format YYYY-MM-DD.")
  }
  WQPquery <- c(WQPquery, endDate = list(endDate))
} else if (endDate != "null") {
  if (is.na(suppressWarnings(lubridate::parse_date_time(endDate, orders = "ymd")))) {
    stop("Incorrect date format. Please use the format YYYY-MM-DD.")
  }
  WQPquery <- c(WQPquery, endDate = endDate)
}


# Prepare sf/tribal specific components -----------------------------------

# If an sf object OR tribal info are provided they will be the basis of the
# query. (The tribal data handling uses sf objects as well)
if( query_type %in% c("tribal", "spatial") ) {
  
  # sf AOI prep for query
  
  # If tribe info is provided then grab the corresponding sf object:
  if(tribal_area_type != "null"){
    
    # Make a reference table for tribal area type + url matching
    # (options that don't return results are commented out for now)
    map_service_urls <- tibble::tribble(
      ~tribal_area,                            ~url,
      "Alaska Native Allotments",              "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/0",
      # "Alaska Native Villages",                "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/1",
      "American Indian Reservations",          "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2",
      "Off-reservation Trust Lands",           "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/3",
      "Oklahoma Tribal Statistical Areas",     "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4"# ,
      # "Virginia Federally Recognized Tribes",  "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/5"
    )
    
    # Keep to a single type:
    if(length(tribal_area_type) > 1){
      stop("tribal_area_type must be of length 1.")
    }
    
    # These two layers will not return any data when used for bboxes
    if(tribal_area_type == "Alaska Native Villages"){
      stop("Alaska Native Villages data are centroid points, not spatial boundaries.")
    } else if(tribal_area_type == "Virginia Federally Recognized Tribes") {
      stop("Federally recognized tribal entities in Virginia do not have any available spatial boundaries.")
    }
    
    # These area types allow filtering by TRIBE_NAME (unique within each type)
    if(tribal_area_type %in% c(
      # "Alaska Native Villages",
      "American Indian Reservations",
      "Off-reservation Trust Lands",
      "Oklahoma Tribal Statistical Areas"#,
      # "Virginia Federally Recognized Tribes"
    )
    ){
      
      # Get the relevant url
      aoi_sf <- filter(map_service_urls,
                       tribal_area == tribal_area_type)$url %>%
        # Pull data
        arcgislayers::arc_open() %>%
        # Return sf
        arcgislayers::arc_select() %>%
        # If a value provided, then filter
        {if ((tribe_name_parcel != "null") & (tribe_name_parcel != "null")) {
          filter(., TRIBE_NAME %in% tribe_name_parcel)
        } else {
          .
        }}
      
      # Otherwise filter by PARCEL_NO (Note that values in this col are not unique)
    } else if(tribal_area_type == "Alaska Native Allotments"){
      
      aoi_sf <- filter(map_service_urls,
                       tribal_area == tribal_area_type)$url %>%
        arcgislayers::arc_open() %>%
        arcgislayers::arc_select() %>%
        {if ((tribe_name_parcel != "null") & (tribe_name_parcel != "null")) {
          filter(., PARCEL_NO %in% tribe_name_parcel)
        } else {
          .
        }}
      
    } else {
      stop("Tribal area type not recognized. Refer to TADA_TribalOptions() for query options.")
    }
    
  }
  
  # Check and/or fix geometry
  aoi_sf <- sf::st_make_valid(aoi_sf)
  
  # Match CRS
  if(sf::st_crs(aoi_sf) != 4326){
    aoi_sf <- sf::st_transform(aoi_sf, crs = 4326)
  }
  
  # Get bbox of the sf object
  input_bbox <- sf::st_bbox(aoi_sf)
  
  # Query site info within the bbox
  bbox_sites <- dataRetrieval::whatWQPsites(
    WQPquery,
    bBox = c(input_bbox$xmin, input_bbox$ymin, input_bbox$xmax, input_bbox$ymax)
  )
  
  # Check if any sites are within the aoi
  if ( (nrow(bbox_sites) > 0 ) == FALSE) {
    stop("No monitoring sites were returned within your area of interest (no data available).")
  }
  
  # Reformat returned info as sf
  bbox_sites_sf <- TADA_MakeSpatial(bbox_sites, crs = 4326)
  
  # Subset sites to only within shapefile and get IDs
  clipped_sites_sf <- bbox_sites_sf[aoi_sf, ]
  
  clipped_site_ids <- clipped_sites_sf$MonitoringLocationIdentifier
  
  
  # Prepare non-sf/tribal components ----------------------------------------
  
} else if ( query_type == "standard" ) {
  
  if (!"null" %in% statecode) {
    load(system.file("extdata", "statecodes_df.Rdata", package = "EPATADA"))
    statecode <- as.character(statecode)
    statecodes_sub <- statecodes_df %>% dplyr::filter(STUSAB %in% statecode)
    statecd <- paste0("US:", statecodes_sub$STATE)
    if (nrow(statecodes_sub) == 0) {
      stop("State code is not valid. Check FIPS state/territory abbreviations.")
    }
    if (length(statecode) >= 1) {
      WQPquery <- c(WQPquery, statecode = list(statecd))
    }
  }
  
  if (length(huc) > 1) {
    WQPquery <- c(WQPquery, huc = list(huc))
  } else if (huc != "null") {
    WQPquery <- c(WQPquery, huc = huc)
  }
  
  if (length(countrycode) > 1) {
    WQPquery <- c(WQPquery, countrycode = list(countrycode))
  } else if (countrycode != "null") {
    WQPquery <- c(WQPquery, countrycode = countrycode)
  }
  
  if (length(countycode) > 1) {
    WQPquery <- c(WQPquery, countycode = list(countycode))
  } else if (countycode != "null") {
    WQPquery <- c(WQPquery, countycode = countycode)
  }
  
  if (length(siteid) > 1) {
    WQPquery <- c(WQPquery, siteid = list(siteid))
  } else if (siteid != "null") {
    WQPquery <- c(WQPquery, siteid = siteid)
  }
  
}

# Check record counts -----------------------------------------------------

# Retrieve record summary data
df_summary <- dataRetrieval::whatWQPdata(WQPquery) %>%
  # May return 0 counts; remove
  filter(resultCount > 0)

# Total number of records that would be requested
record_count <- sum(df_summary$resultCount)


# Query -------------------------------------------------------------------

# NOTE: if query brings back no results, function returns empty
# dataRetrieval profile, not empty summary
if ( record_count <= maxrecs ){
  # Query as normal; BigDataRetrieval not needed
  
  # <Insert normal code>
  
} else if ( record_count > maxrecs ) {
  
  # Get total number of results per site and separate out sites with
  # >maxrecs results ("bigsites")
  tot_sites <- sites %>%
    dplyr::group_by(MonitoringLocationIdentifier) %>%
    dplyr::summarise(tot_n = sum(resultCount)) %>%
    dplyr::arrange(tot_n)
  
  smallsites <- tot_sites %>% dplyr::filter(tot_n < maxrecs)
  bigsites <- tot_sites %>% dplyr::filter(tot_n >= maxrecs)
  
  df <- data.frame()
  
  # Bin query groups for smallsites
  if (dim(smallsites)[1] > 0) {
    smallsitesgrp <- smallsites %>%
      mutate(group = MESS::cumsumbinning(
        x = tot_n,
        threshold = maxrecs,
        maxgroupsize = 300
      ))
    
    print(
      paste0("Downloading data from sites with fewer than ",
             maxrecs,
             " results by grouping them together.")
    )
    
    # Loop over groups to query
    for (i in 1:max(smallsitesgrp$group)) {
      site_chunk <- subset(smallsitesgrp$MonitoringLocationIdentifier, smallsitesgrp$group == i)
      joins <- TADA_DataRetrieval(
        startDate = startDate,
        endDate = endDate,
        siteid = site_chunk,
        characteristicName = characteristicName,
        characteristicType = characteristicType,
        sampleMedia = sampleMedia,
        applyautoclean = FALSE
      )
      if (dim(joins)[1] > 0) {
        df <- dplyr::bind_rows(df, joins)
      }
    }
    
    rm(smallsites, smallsitesgrp)
  }
  
} else {
  warning("Query returned no data. Function returns an empty dataframe.")
  return(df_summary)
}



# NOTE: if query brings back no results, function returns empty
# dataRetrieval profile, not empty summary
if (nrow(df_summary) > 0) {
  # get total number of results per site and separate out sites with >maxrecs results
  tot_sites <- sites %>%
    dplyr::group_by(MonitoringLocationIdentifier) %>%
    dplyr::summarise(tot_n = sum(resultCount)) %>%
    dplyr::arrange(tot_n)
  smallsites <- tot_sites %>% dplyr::filter(tot_n < maxrecs)
  bigsites <- tot_sites %>% dplyr::filter(tot_n >= maxrecs)
  
  df <- data.frame()
  
  if (dim(smallsites)[1] > 0) {
    smallsitesgrp <- smallsites %>%
      mutate(group = MESS::cumsumbinning(
        x = tot_n,
        threshold = maxrecs,
        maxgroupsize = 300
      ))
    
    print(
      paste0("Downloading data from sites with fewer than ",
             maxrecs,
             " results by grouping them together.")
    )
    
    for (i in 1:max(smallsitesgrp$group)) {
      # Site subset
      site_chunk <- subset(smallsitesgrp$MonitoringLocationIdentifier, smallsitesgrp$group == i)
      # Data query
      results.DR <- dataRetrieval::readWQPdata(
        discard_at(WQPquery, "siteid"),
        siteid = site_chunk,
        dataProfile = "resultPhysChem",
        ignore_attributes = TRUE
      )
      # Get site metadata
      sites.DR <- dataRetrieval::whatWQPsites(discard_at(WQPquery, "siteid"),
                                              siteid = site_chunk)
      # Get project metadata
      projects.DR <- dataRetrieval::readWQPdata(discard_at(WQPquery, "siteid"),
                                                siteid = site_chunk,
                                                ignore_attributes = TRUE,
                                                service = "Project")
      # Join results, sites, projects
      TADAprofile <- TADA_JoinWQPProfiles(
        FullPhysChem = results.DR,
        Sites = sites.DR,
        Projects = projects.DR
      )
      
      # need to specify this or throws error when trying to bind rows.
      # Temporary fix for larger issue where data structure for all columns
      # should be specified.
      TADAprofile <- TADAprofile %>% dplyr::mutate(
        across(everything(), as.character)
      )
      
      # run TADA_AutoClean function
      if (applyautoclean == TRUE) {
        print("Data successfully downloaded. Running TADA_AutoClean function.")
        
        TADAprofile.clean <- TADA_AutoClean(TADAprofile)
      } else {
        TADAprofile.clean <- TADAprofile
      }
      
      if (dim(TADAprofile.clean)[1] > 0) {
        df <- dplyr::bind_rows(df, TADAprofile.clean)
      }
    }
    
    rm(smallsites, smallsitesgrp)
  }
  
  if (dim(bigsites)[1] > 0) {
    print(paste0("Downloading data from sites with greater than ", maxrecs, " results, chunking queries by shorter time intervals..."))
    
    bsitesvec <- unique(bigsites$MonitoringLocationIdentifier)
    
    for (i in 1:length(bsitesvec)) {
      mlidsum <- subset(sites, sites$MonitoringLocationIdentifier == bsitesvec[i])
      mlidsum <- mlidsum %>%
        dplyr::group_by(MonitoringLocationIdentifier, YearSummarized) %>%
        dplyr::summarise(tot_n = sum(resultCount))
      site_chunk <- unique(mlidsum$MonitoringLocationIdentifier)
      
      bigsitegrps <- make_groups(mlidsum, maxrecs)
      
      for (i in 1:max(bigsitegrps$group)) {
        yearchunk <- subset(bigsitegrps$YearSummarized, bigsitegrps$group == i)
        startD <- paste0(min(yearchunk), "-01-01")
        endD <- paste0(max(yearchunk), "-12-31")
        
        joins <- TADA_DataRetrieval(
          startDate = startD,
          endDate = endD,
          siteid = site_chunk,
          characteristicName = characteristicName,
          characteristicType = characteristicType,
          sampleMedia = sampleMedia,
          applyautoclean = FALSE
        )
        
        if (dim(joins)[1] > 0) {
          df <- dplyr::bind_rows(df, joins)
        }
      }
    }
    rm(bigsites, bigsitegrps)
  }
} else {
  warning("Query returned no data. Function returns an empty dataframe.")
  return(sites)
}
} else {
  warning("Query returned no data. Function returns an empty dataframe.")
  return(df_summary)
}




































# Goal: Integrate normal TADA_DataRetrieval with sf, tribal, and BigDataRetrieval
TADA_BigDataRetrieval_expanded <- function(startDate = "null",
                                           endDate = "null",
                                           countrycode = "null",
                                           statecode = "null",
                                           countycode = "null",
                                           huc = "null",
                                           siteid = "null",
                                           siteType = "null",
                                           characteristicName = "null",
                                           characteristicType = "null",
                                           sampleMedia = "null",
                                           organization = "null",
                                           maxrecs = 250000,
                                           applyautoclean = FALSE) {
  start_T <- Sys.time()
  
  if (!"null" %in% statecode & !"null" %in% huc) {
    stop("Please provide either state code(s) OR huc(s) to proceed.")
  }
  
  if (!startDate == "null") {
    startDat <- lubridate::ymd(startDate)
    startYearLo <- lubridate::year(startDat)
  } else { # else: pick a date before which any data are unlikely to be in WQP
    startDate <- "1800-01-01"
    startDat <- lubridate::ymd(startDate)
    startYearLo <- lubridate::year(startDat)
  }
  
  # Logic: if the input endDate is not null, convert to date and obtain year
  # for summary
  if (!endDate == "null") {
    endDat <- lubridate::ymd(endDate)
    endYearHi <- lubridate::year(endDat)
  } else { # else: if not populated, default to using today's date/year for summary
    endDate <- as.character(Sys.Date())
    endDat <- lubridate::ymd(endDate)
    endYearHi <- lubridate::year(endDat)
  }
  
  # Create readWQPsummary query
  WQPquery <- list()
  if (length(characteristicName) > 1) {
    WQPquery <- c(WQPquery, characteristicName = list(characteristicName))
  } else if (characteristicName != "null") {
    WQPquery <- c(WQPquery, characteristicName = characteristicName)
  }
  if (length(characteristicType) > 1) {
    WQPquery <- c(WQPquery, characteristicType = list(characteristicType))
  } else if (characteristicType != "null") {
    WQPquery <- c(WQPquery, characteristicType = characteristicType)
  }
  if (length(siteType) > 1) {
    WQPquery <- c(WQPquery, siteType = list(siteType))
  } else if (siteType != "null") {
    WQPquery <- c(WQPquery, siteType = siteType)
  }
  
  if (!"null" %in% statecode) {
    load(system.file("extdata", "statecodes_df.Rdata", package = "EPATADA"))
    statecode <- as.character(statecode)
    statecodes_sub <- statecodes_df %>% dplyr::filter(STUSAB %in% statecode)
    statecd <- paste0("US:", statecodes_sub$STATE)
    if (nrow(statecodes_sub) == 0) {
      stop("State code is not valid. Check FIPS state/territory abbreviations.")
    }
    if (length(statecode) > 1) {
      for (i in 1:length(statecode)) {
        WQPquery <- c(WQPquery, statecode = list(statecd))
      }
      WQPquery <- c(WQPquery, statecode = list(statecd))
    } else {
      WQPquery <- c(WQPquery, statecode = statecd)
    }
  }
  
  if (length(huc) > 1) {
    WQPquery <- c(WQPquery, huc = list(huc))
  } else if (huc != "null") {
    WQPquery <- c(WQPquery, huc = huc)
  }
  
  if (length(countrycode) > 1) {
    WQPquery <- c(WQPquery, countrycode = list(countrycode))
  } else if (countrycode != "null") {
    WQPquery <- c(WQPquery, countrycode = countrycode)
  }
  
  if (length(countycode) > 1) {
    WQPquery <- c(WQPquery, countycode = list(countycode))
  } else if (countycode != "null") {
    WQPquery <- c(WQPquery, countycode = countycode)
  }
  
  if (length(organization) > 1) {
    WQPquery <- c(WQPquery, organization = list(organization))
  } else if (organization != "null") {
    WQPquery <- c(WQPquery, organization = organization)
  }
  
  # cut down on summary query time if possible based on big data query
  diffdat <- lubridate::time_length(difftime(Sys.Date(), startDat), "years")
  
  if (diffdat <= 1) {
    WQPquery <- c(WQPquery, summaryYears = 1)
  }
  
  if (diffdat > 1 & diffdat <= 5) {
    WQPquery <- c(WQPquery, summaryYears = 5)
  }
  
  print("Building site summary table for chunking result downloads...")
  # df_summary <- dataRetrieval::readWQPsummary(WQPquery)
  df_summary <- dataRetrieval::whatWQPdata(statecode = WQPquery$statecode,
                                           startDate = startDate,
                                           endDate = endDate) %>%
    # Returns 0 counts; remove
    filter(resultCount > 0)
  
  ## NOTE: if query brings back no results, function returns empty
  # dataRetrieval profile, not empty summary
  if (nrow(df_summary) > 0) {
    # narrow down to years of interest from summary
    sites <- df_summary #%>%
    # dplyr::filter(
    #   YearSummarized >= startYearLo,
    #   YearSummarized <= endYearHi
    # )
    
    rm(df_summary)
    # if there are still site records when filtered to years of interest....
    if (dim(sites)[1] > 0) {
      
      # get total number of results per site and separate out sites with >maxrecs results
      tot_sites <- sites %>%
        dplyr::group_by(MonitoringLocationIdentifier) %>%
        dplyr::summarise(tot_n = sum(resultCount)) %>%
        dplyr::arrange(tot_n)
      smallsites <- tot_sites %>% dplyr::filter(tot_n < maxrecs)
      bigsites <- tot_sites %>% dplyr::filter(tot_n >= maxrecs)
      
      df <- data.frame()
      
      if (dim(smallsites)[1] > 0) {
        smallsitesgrp <- smallsites %>%
          mutate(group = MESS::cumsumbinning(
            x = tot_n,
            threshold = maxrecs,
            maxgroupsize = 300
          ))
        
        print(
          paste0("Downloading data from sites with fewer than ",
                 maxrecs,
                 " results by grouping them together.")
        )
        
        for (i in 1:max(smallsitesgrp$group)) {
          site_chunk <- subset(smallsitesgrp$MonitoringLocationIdentifier, smallsitesgrp$group == i)
          joins <- TADA_DataRetrieval(
            startDate = startDate,
            endDate = endDate,
            siteid = site_chunk,
            characteristicName = characteristicName,
            characteristicType = characteristicType,
            sampleMedia = sampleMedia,
            applyautoclean = FALSE
          )
          if (dim(joins)[1] > 0) {
            df <- dplyr::bind_rows(df, joins)
          }
        }
        
        rm(smallsites, smallsitesgrp)
      }
      
      if (dim(bigsites)[1] > 0) {
        print(paste0("Downloading data from sites with greater than ", maxrecs, " results, chunking queries by shorter time intervals..."))
        
        bsitesvec <- unique(bigsites$MonitoringLocationIdentifier)
        
        for (i in 1:length(bsitesvec)) {
          mlidsum <- subset(sites, sites$MonitoringLocationIdentifier == bsitesvec[i])
          mlidsum <- mlidsum %>%
            dplyr::group_by(MonitoringLocationIdentifier, YearSummarized) %>%
            dplyr::summarise(tot_n = sum(resultCount))
          site_chunk <- unique(mlidsum$MonitoringLocationIdentifier)
          
          bigsitegrps <- make_groups(mlidsum, maxrecs)
          
          for (i in 1:max(bigsitegrps$group)) {
            yearchunk <- subset(bigsitegrps$YearSummarized, bigsitegrps$group == i)
            startD <- paste0(min(yearchunk), "-01-01")
            endD <- paste0(max(yearchunk), "-12-31")
            
            joins <- TADA_DataRetrieval(
              startDate = startD,
              endDate = endD,
              siteid = site_chunk,
              characteristicName = characteristicName,
              characteristicType = characteristicType,
              sampleMedia = sampleMedia,
              applyautoclean = FALSE
            )
            
            if (dim(joins)[1] > 0) {
              df <- dplyr::bind_rows(df, joins)
            }
          }
        }
        rm(bigsites, bigsitegrps)
      }
    } else {
      warning("Query returned no data. Function returns an empty dataframe.")
      return(sites)
    }
  } else {
    warning("Query returned no data. Function returns an empty dataframe.")
    return(df_summary)
  }
  
  df <- subset(df, as.Date(df$ActivityStartDate, "%Y-%m-%d") >= startDat & as.Date(df$ActivityStartDate, "%Y-%m-%d") <= endDat)
  
  if (applyautoclean == TRUE) {
    print("Applying TADA_AutoClean function...")
    df <- TADA_AutoClean(df)
  }
  
  # timing function for efficiency tests.
  difference <- difftime(Sys.time(), start_T, units = "mins")
  print(difference)
  
  return(df)
}





