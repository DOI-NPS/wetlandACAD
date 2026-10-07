#' @title importWaterLevel: Imports and compiles views for wetland water level data package
#'
#' @description This function imports water level-related tables in the wetland RAM backend and
#' combines them into flattened views for the data package. Each view is added to a VIEWS_WL
#' environment in your workspace, or to your global environment based on whether new_env = TRUE or FALSE.
#'
#' @importFrom dplyr arrange filter mutate 
#' @importFrom purrr reduce
#'
#' @param type Select whether to use the default Data Source Named database (DSN) to import data or a
#' different database.
#' If "DSN" is selected, must specify name in odbc argument.
#' \describe{
#' \item{"DSN"}{Default. DSN database. If odbc argument is not specified, will default to "RAM_BE"}
#' \item{"file"}{A different database than default DSN}
#' }
#'
#' @param odbc DSN of the database when using type = DSN. If not specified will default to "RAM_BE", which
#' is the back end of the MS Access RAM database.
#'
#' @param db_path Quoted path of database back end file, including the name of the backend. Only needed
#' if type = 'file'.
#' 
#' @param wl_path Path and file name of csv containing corrected water level and precipitation data. 
#' These are generated using a script each year. Data and script are stored on the NETN Z:/ drive
#' under ".../Freshwater_Wetlands/ACAD_FW_Wetlands/5_Data/Wetland_Wells/Data_Analysis/"
#'
#' @param export_data Logical. If TRUE, writes views to disk. If FALSE (Default), views are only
#' stored in specified R environment.
#'
#' @param export_path Quoted path to export views to. If blank, exports to working directory.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in VIEWS_WL environment. If \code{FALSE}, stores views in global environment
#'
#' @param zip Logical. If TRUE, exports a zip file. If FALSE (Default), exports individual csvs.
#'
#' @examples
#' \dontrun{
#' # Import tables from database in specific folder:
#' importWaterLevel(type = 'file', db_path = '../data/NETN_RAM_20260731.accdb',
#'   wl_path = "../data/well_prec_data_2013-2025.csv")
#'
#' # Import well and well visit data from default RAM_BE database
#' importWaterLevel(wl_path = "../data/well_prec_data_2013-2025.csv",
#'                  export_data = T, export_path = "../data/wetland_data_package/")
#'                  
#' #' # Import well and well visit data from default RAM_BE database, and export views as zip 
#' importWaterLevel(wl_path = "../data/well_prec_data_2013-2025.csv",
#'                  export_data = T, export_path = "../data/wetland_data_package/", zip = T)
#' }
#'
#' @return Assigns RAM views to specified environment
#' @export

importWaterLevel <- function(type = 'DSN', odbc = 'RAM_BE', db_path = NA, wl_path = NA,
                             new_env = TRUE, export_data = FALSE, export_path = NA, 
                             zip = FALSE){

  #---- error handling ----
  type <- match.arg(type, c("DSN", "file"))
  stopifnot(class(new_env) == 'logical')
  stopifnot(class(export_data) == 'logical')
  stopifnot(class(zip) == 'logical')

  # Check that export path exists and add closing / if not present
  if(export_data == TRUE){
    if(is.na(export_path)){export_path <- getwd()
    } else if(!dir.exists(export_path)){stop("Specified export_path does not exist.")}

    # Normalize path for zip
    export_pathn <- normalizePath(export_path)

    # Add / to end of path if it wasn't specified.
    export_pathn <- if(!grepl("/$", export_pathn)){paste0(export_pathn, "\\")}
  }

  # Check that wl_path exists
  wl_pathn1 <- normalizePath(wl_path)
  wl_pathn <- sub('(.*)[\\](.*)', "\\1", wl_pathn1)
  
  # Read in wl data and add tryCatch if not found
  wl_data <- tryCatch(read.csv(wl_path),
                      error = function(e){stop("Water level csv not found. Please check that path and file name are correct.")}
  )
  
  if(!requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)
  }
  if(!requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("sf", quietly = T)){
    stop("Package 'sf' needed to generate lat/long coordinates. Please install it.", call. = FALSE)}
  
  # make sure db is on dsn list if type == DSN
  dsn_list <- odbc::odbcListDataSources()

  if(type == 'DSN' & !any(dsn_list$name %in% odbc)){
    stop(paste0("Specified DSN ", odbc, " is not a named database source." ))}

  # check for db if type = file
  if(type == "file"){
    if(is.na(db_path)){stop("Must specify a path to the database for type = file option.")
    } else {
      if(file.exists(db_path) == FALSE){stop("Specified path or database does not exist.")}}
  }

  #---- import db tables ----
  tryCatch(
    db <- if (type == 'DSN'){
    db <- DBI::dbConnect(drv = odbc::odbc(), dsn = odbc)
    }
    else if (type == 'file'){
      db <- DBI::dbConnect(drv=odbc::odbc(),
                           .connection_string =
                           paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", db_path))
    },
      error = function(e){stop(e)},
      warning = function(w){stop(w)
    }
    )

  tbl_list1 <- DBI::dbListTables(db)[grepl("tbl|tlu|xref", DBI::dbListTables(db))]
  tbl_list <- tbl_list1[grepl("tbl_Well|tbl_Well_Visit|tbl_Location|tlu_Predominant_Category|tlu_Class|tlu_Sub_Class", 
                              tbl_list1)] # drops well tbls and queries

  pb = txtProgressBar(min = 0, max = length(tbl_list) + 2, style = 3)

  tbl_import <- lapply(seq_along(tbl_list),
                       function(x){
                         setTxtProgressBar(pb, x)
                         tab1 <- tbl_list[x]
                         tab <- dplyr::tbl(db, tab1) |> dplyr::collect() |> as.data.frame()
                         return(tab)
                       })

  DBI::dbDisconnect(db)

  tbl_import <- setNames(tbl_import, tbl_list)

  if(new_env == TRUE){VIEWS_WL <<- new.env()}
  env <- if(new_env == TRUE){VIEWS_WL} else {.GlobalEnv}

  list2env(tbl_import, envir = environment()) # all tables into fxn env
  setTxtProgressBar(pb, length(tbl_list) + 1)

  #---- Combine tables into views ----
  #--- tbl_wells
  tbl_Well$Launch_Year <- format(as.Date(tbl_Well$Launch_Date, format = "%Y-%m-%d"), "%Y")
  tbl_well1 <- tbl_Well[tbl_Well$Status == "A",] # A = active 
                           
  names(tbl_well1)[names(tbl_well1) == "ID"] <- "Well_ID"
  names(tbl_well1)[names(tbl_well1) == "Easting"] <- "xCoordinate"
  names(tbl_well1)[names(tbl_well1) == "Northing"] <- "yCoordinate"
  names(tbl_well1)[names(tbl_well1) == "Site_Code"] <- "WellCode"
  names(tbl_well1)[names(tbl_well1) == "Note"] <- "Well_Note"
  
  names(tbl_Location)[names(tbl_Location) == "Code"] <- "SiteCode"
  names(tlu_Predominant_Category)[names(tlu_Predominant_Category) == "Code"] <- "FWS_Class_Code"
  # names(tlu_Predominant_Category)[names(tlu_Predominant_Category) == "Description"] <- "FWS_Description"
  
  names(tlu_Class)[names(tlu_Class) == "Class"] <- "HGM_Class"  
  names(tlu_Sub_Class)[names(tlu_Sub_Class) == "Sub_Class"] <- "HGM_Sub_Class"  
  
  tbl_locs <- tbl_Location[tbl_Location$Panel == -1 &
                             tbl_Location$Location_ID > 1, # number for baro logger 
                           c("SiteCode", "Location_ID", #"Directions", 
                             "Predominant_Category_ID", "Class_ID", "Sub_Class_ID")]
  
  tbl_locs2 <- left_join(tbl_locs, tlu_Predominant_Category, by = "Predominant_Category_ID")
  
  tbl_locs3 <- left_join(tbl_locs2, tlu_Class, by = "Class_ID")
  
  tbl_locs4 <- left_join(tbl_locs3, tlu_Sub_Class, by = c("Class_ID", "Sub_Class_ID")) |> 
    select(-Predominant_Category_ID, -Class_ID, -Sub_Class_ID)
  
  loc_well <- left_join(tbl_well1, tbl_locs4, by = "Location_ID")
  
  loc_well$GroupCode <- "NETN"
  loc_well$GroupName <- "Northeast Temperate Network"
  loc_well$UnitCode <- "ACAD"
  loc_well$UnitName <- "Acadia National Park"
  loc_well$UTM_Zone <- "19N"
  
  # add lat longs
  latlon <- sf::st_as_sf(loc_well, coords = c("xCoordinate", "yCoordinate"), crs = 26919) |>
    sf::st_transform(crs = 4269) #NAD83; WGS84 is 4326
  
  latlon_df <- data.frame(Well_ID = latlon$Well_ID,
                          Latitude = sf::st_coordinates(latlon)[,2],
                          Longitude = sf::st_coordinates(latlon)[,1])

  loc_well2 <- left_join(loc_well, latlon_df, by = "Well_ID")
  
  view_wells <- loc_well2[,c(
    "GroupCode", "GroupName", "UnitCode", "UnitName",
    "SiteCode", "Location_ID", "Well_ID", "Well_Name", "WellCode",
    "Sample_Order", "Launch_Date", "Launch_Year", 
    "xCoordinate", "yCoordinate", "UTM_Zone",
    "Latitude", "Longitude", "FWS_Class_Code", "Description",
    "HGM_Class", "HGM_Sub_Class", "Logger_Length", "MP_to_Bolt",
    "Well_Note")]
  
  # view_wells data complete

  #--- tbl_well_visits
  tbl_Well_Visit$Visit_Date <- as.Date(tbl_Well_Visit$Visit_Date, format = "%Y-%m-%d", tz = "America/New_York")
  tbl_Well_Visit$Logger_Start <- format(as.POSIXct(tbl_Well_Visit$Time, 
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                        tz = "America/New_York", "%H:%M%:%S")
  tbl_Well_Visit$Logger_Deploy_Time <- as.POSIXct(paste0(tbl_Well_Visit$Visit_Date, " ",
                                                         tbl_Well_Visit$Logger_Start),
                                                  format = "%Y-%m-%d %H:%M:%S",
                                                  tz = "America/New_York")
  
  names(tbl_Well_Visit)[names(tbl_Well_Visit) == "ID"] <- "Well_Visit_ID"
  names(tbl_Well_Visit)[names(tbl_Well_Visit) == "Note"] <- "Well_Visit_Note"
  
  tbl_Well_Visit$Water_Depth_Time <- as.POSIXct(paste0(tbl_Well_Visit$Visit_Date, " ", 
                                                       substr(as.character(tbl_Well_Visit$Water_Depth_Time), 12, 19)), 
                                                format = "%Y-%m-%d %H:%M:%S",
                                                tz = "America/New_York")
  
  tbl_well_vis <- tbl_Well_Visit[,c("Well_Visit_ID", "Well_ID", "Logger_SN", 'Battery_Status',
                                    "Visit_Date", "Logger_Deploy_Time", "Water_Depth_Time", "Stick_Up_at_MP", 
                                    "Post_1_Height", "Post_1_Distance", "Post_2_Height", "Post_2_Distance",
                                    "Water_Depth", "Reset", "Well_Visit_Note", "Certification_Level")]
  
  well_loc_vis <- left_join(view_wells, tbl_well_vis, by = "Well_ID")
  well_loc_vis$Visit_Month <- as.numeric(format(well_loc_vis$Visit_Date, "%m"))
  well_loc_vis$Visit_Year <- as.numeric(format(well_loc_vis$Visit_Date, "%Y"))
  well_loc_vis$Visit_Season <- ifelse(well_loc_vis$Visit_Month %in% c(3, 4, 5, 6, 7), "Spring", "Fall")
  
  # Ended here- sort by Sample_Order, Year, then decide column order
  first_cols <- names(view_wells)
  
  view_well_visits <- well_loc_vis[, c(first_cols, "Well_Visit_ID",
                                      "Visit_Date", "Visit_Year", "Visit_Month", "Visit_Season",
                                      "Logger_SN", "Battery_Status",
                                      "Logger_Deploy_Time", "Water_Depth_Time", "Water_Depth",
                                      "Stick_Up_at_MP", "Post_1_Height", "Post_1_Distance", 
                                      "Post_2_Height", "Post_2_Distance", 
                                      "Reset", "Well_Visit_Note", "Certification_Level")]
  # view_well_visits view complete
  
  #--- tbl_water_level
  wl_data$timestamp1 <- as.POSIXct(ifelse(wl_data$hr == 0, 
                                          paste0(wl_data$timestamp, " 00:00:00"), 
                                          wl_data$timestamp),
                                  format = "%Y-%m-%d %H:%M:%S")  
  wl_data$GroupCode <- "NETN"
  wl_data$GroupName <- "Northeast Temperate Network"
  wl_data$UnitCode <- "ACAD"
  wl_data$UnitName <- "Acadia National Park"
  wl_data$Certification_Level <- ifelse(wl_data$Year < 2022, "A", "C") # stored in raw WL data in database,
  # but not easy to grab and attach to here. This follows the exact pattern in the database though.
  
  names(wl_data)[names(wl_data) == "lag.precip"] <- "lag_precip_cm"
  wl_data$timestamp <- as.character(format(wl_data$timestamp1)) # so midnight hours aren't dropped in write to csv
  
  view_wl_data <- wl_data[,c("GroupCode", "GroupName", "UnitCode", "UnitName", "timestamp", "Date", "doy", 
                             "Year", "hr", "doy_h", "precip_cm", "lag_precip_cm", 
                             "BIGH_WL", "DUCK_WL", "GILM_WL", "HEBR_WL", 
                             "HODG_WL", "LIHU_WL", "NEMI_WL", "WMTN_WL",
                             "Certification_Level")]
  
  setTxtProgressBar(pb, length(tbl_list) + 2)
  close(pb)

  # final tables to add to new env or global env and print to disk
  final_tables <- list(view_well_visits, view_wl_data)

  final_tables <- setNames(final_tables,
                           c("well_visit_data", "water_level_data"))

  list2env(final_tables, envir = env)

  if(export_data == TRUE){
    # Export files
    if(zip == FALSE){
      invisible(lapply(seq_along(final_tables),
                       function(x){
                         dtbl = final_tables[[x]]
                         write.csv(dtbl, paste0(export_pathn, names(final_tables)[[x]], ".csv"),
                                   row.names = FALSE)
                       }))
    } else if(zip == TRUE){ #create tmp dir to export csvs, bundle to zip, then delete tmp folder

      dir.create(tmp <- tempfile())

      invisible(lapply(seq_along(final_tables),
                       function(x){
                         dtbl = final_tables[[x]]
                         write.csv(dtbl,
                                   paste0(tmp, "\\", names(final_tables)[[x]], ".csv"),
                                   row.names = FALSE)}))

      file_list <- list.files(tmp)

      zip::zipr(zipfile = paste0(export_pathn, "NETN_Wetland_Water_Level_Data_", format(Sys.Date(), "%Y%m%d"), ".zip"),
                root = tmp,
                files = file_list)
      # csvs will be deleted as soon as R session is closed b/c tempfile
    }
  }

  end_mess1 <- "Data package complete. Views are located in VIEWS_WL environment. "
  end_mess2 <- "Data package complete. Views are located in global environment. "

  if(export_data == FALSE){
    if(new_env == TRUE){print(end_mess1)
    } else if(new_env == FALSE){print(end_mess2)}
  } else if(export_data == TRUE){
    end_mess3 <- paste0("Files saved to: ", export_pathn, " ")
    end_mess4 <- paste0("Zip file saved to: ", export_pathn,
                        "NETN_Wetland_Water_Level_Data_", format(Sys.Date(), "%Y%m%d"), ".zip ")

    if(new_env == TRUE & zip == TRUE){
      print(paste0(end_mess1, end_mess4))
    } else if(new_env == FALSE & zip == TRUE){
      print(paste0(end_mess2, end_mess4))
    } else if(new_env == TRUE & zip == FALSE){
      print(paste0(end_mess1, end_mess3))
    } else if(new_env == FALSE & zip == FALSE){
      print(paste0(end_mess2, end_mess3))
    }
  }
  } # End of function





