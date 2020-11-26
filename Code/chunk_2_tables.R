################################################################################
#####
##### Code Chunk to create the tables 3 - 5 of the metadata description
#####
################################################################################


##### TABLE 3. References matriz

table_3 <- data.frame(
      Variable = names(references),
      Description = c(
            "Identification of each reference. This code links the reference matrix to the other matrices",
            
            "Short name of the author(s), if there are three or more authors, we use et al.", "Year of publication.",
            
            "Extended reference."
      ),
      Levels = c("BPR001 to BPR168", "...", "1957 to 2007", "..."),
      Example = as.character(references[66,1:4])
)


##### TABLE 4. Sites matriz

table_4 <- data.frame(
      Variable = names(sites),
      Description = c(
            "Identification of each sampling site. This code links the site matrix to the record matrix. In many cases, a paper can have more than one sampling site.",
            
            "Identification of each reference. This code links the reference matrix to the other matrices.",
            
            "Locality where fieldwork was carried out, based on information reported in the paper. We checked all names using the Google Earth database.",
            
            "State, Department or Province of the study site based on the geographic coordinates.",
            
            "Country where fieldwork was carried out (English name).",
            
            "Corrected latitude in decimal degrees (Projection WGS84 EPSG:4326). In studies with two or more sampling sites with less than 5 km of linear distance between them we used the centroid coordinate.",
            
            "Corrected longitude in decimal degrees. See Latitude for more information.",
            
            "In some cases, papers reported the precise coordinates of each sampling site. Other papers reported only geographic references (basins, rivers, municipality, or distance from a village). In these cases, we validated these references with Google Earth satellite images. We consider Not Precise when coordinates mismatch the written information in the paper, or when the paper only reported the coordinates of the municipality or region.",
            
            "The year in which sampling started.",
            
            "The year in which sampling ended.",
            
            "Unstandardized duration of the sampling period (in months).",
            
            "For studies based on feces collection: Total number of fecal samples. For studies based on the observation of plant visitation events: Total number of events recorded.",
            
            "The type of study according to the focus reported in the reference paper. Bat diet refers to study focused on describing the diet of a bat species or assemblage. Plant visitation refers to studies aimed at describing the visitors of a plant species or assemblage.",
            
            "The ecological scale studied. When there were more than one species of bat (when the Study type is Bat diet) or plant (when the study type is Plant visitation), we considered as Assemblage.",
            
            "The sampling method as described in the reference paper. We have standardized the levels to five broad methods. Some studies have more than one sampling method",
            
            "Climatic season in which sampling was performed. Some studies were conducted in both, dry and wet season.",
            
            "Vegetation type as described in the reference paper.",
            
            "Vegetation type corrected according to Oliveira-Filho (2017).",
            
            "Ecological region according to Olson et al. (2001).",
            
            "Phytogeographic domain according to NatureServe (2013) and Oliveira-Filho (2017).",
            
            "Meters above sea level reported in the reference paper.",
            
            "Meters above sea level, from the Hydro-1K dataset (United States Geological Survey-USGS, 2001. Global 30 arc-seconds Elevation (GTOPO30).",
            
            "Annual Rainfall in mm from WorldClim 2.0 with 30 arc seconds resolution.",
            
            "Mean annual temperature in Celsius degrees from WorldClim 2.0 with 30 arc seconds resolution.",
            
            "Global Potential Evapo-Transpiration (annual average in mm) from CGIAR-CSI (Trabucco and Zomer 2009), with resolution of 30 arc seconds.",
            
            "Global Aridity Index model from CGIAR-CSI (Trabucco and Zomer 2009), with resolution of 30 arc seconds."
      ),
      
      Levels = c(
            "BPA001 to BPA200", "BPR001 to BPR168", "...", "...", 
            paste(sort(unique(sites$Country)), collapse = "\n"),
            "Decimal degrees", "Decimal degrees", "Precise\nNot Precise",
            "1960 to 2006", "1960 to 2006", "1 to 37", "6 to 6809",
            "Bat diet\nPlant visitation", "Population\nAssemblage",
            "Direct observation\nExperimental\nFeces collection\nPollen collection\nRoost inspection",
            "Dry\nWet", "...", paste(sort(unique(sites$VegType)), collapse = "\n"),
            "...", paste(sort(unique(sites$Domain)), collapse = "\n"),
            "2 to 2700", "0 to 2686", "66 to 3912", "10.3 to 27.9",
            "1021 to 2588", "0 to 3"
      ),
      
      Example = as.character(sites[74,1:26])
)


##### TABLE 5. Records matrix

table_5 <- data.frame(
      Variable = names(records),
      
      Description = c(
            "Identification code of each interaction record.",
            
            "Identification of each sampling site. This code links the site matrix to the record matrix. In many cases, a paper can have more than one sampling site.",
            
            "Identification of each reference. This code links the reference matrix to the other matrices.",
            
            "Current scientific name of the bat genus.",
            
            "Scientific name of the bat as reported in the reference paper.",
            
            "Current scientific name of the bat species",
            
            "Trophic guild of the bat species",
            
            "Current scientific name of the plant family.",
            
            "Current scientific name of the plant genus.",
            
            "Scientific name of the plant as reported in the reference paper.",
            
            "Current scientific name of the plant, validated with The Plant List database (http://www.theplantlist.org) and the REFLORA database (http://reflora.jbrj.gov.br).",
            
            "Life form of the plant species.",
            
            "Successional stage of the plant species.",
            
            "Type of interaction described.",
            
            "Frugivory: Number of fecal samples containing the plant seed.Nectarivory: Number of visits per sampling unit."
      ),
      
      Levels = c(
            "BPI0001 to BPI2574", "BPA001 to BPA200", "BPR001 to BPR168", "...", "...",  "...",
            paste(sort(unique(records$TrophicGuild)), collapse = "\n"), "...", "...",  "...",
            "...", paste(sort(unique(records$LifeForm)), collapse = "\n"),
            paste(sort(unique(records$SuccessionalStage)), collapse = "\n"),
            paste(sort(unique(records$Interaction)), collapse = "\n"), "1 to 219"
      ),
      
      Example = as.character(records[77,1:15])
)

