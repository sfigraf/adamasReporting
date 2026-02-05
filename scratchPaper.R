##scratch paper

input <- list("waterNameSearch" == "HORSETOOTH RESERVOIR")
data <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
  filter(WaterName == input$waterNameSearch, 
         year(SampleDate)==input$yearsSearch)

singleWaterOnly <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
  filter(WaterName == "HORSETOOTH RESERVOIR")#, 
         #year(SampleDate) == 2025)
#dbplyr doesn't pull data into R unless explicitly asked so that's why it shows up as a list
show_query(singleWaterOnly)
allyears <- singleWaterOnly %>%
  distinct(year(SampleDate)) %>%
  pull()
#print(allyears)

years <- unique(year(singleWaterOnly$SampleDate))

x <- as.data.frame(singleWaterOnly)

data <- tbl(CPW_AqDatAnalysis, "CurrentSummary")
allYears <- data %>%
  distinct(year(SampleDate)) %>%
  pull()

x <- data %>%
  filter(year(SampleDate) == 2025) %>%
  as.data.frame() #%>%
  #select(1)
#library(blob)
blob_cols <- sapply(x, inherits, "blob")
x1 <- x %>%
  mutate(across(where(~inherits(., "blob")), 
                ~sapply(., function(x) paste(as.character(x), collapse = ""))))
#install.packages("blob")
datatable(x)
show_query(x)

x1 <- as.data.frame(saveDat)
datatable(x1)

#coln names 
dbListFields(CPW_AqDatAnalysis, "SampleFView")

allBios <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
  distinct(AreaBio) %>%
  show_query() %>%
  pull() 

allSPBios <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
  distinct(SpConBio) %>%
  show_query() %>%
  pull() 

allyears <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
  distinct(year(SampleDate)) %>%
  show_query() %>%
  pull() 



observeEvent(inputsToListen(), {
  
  waterNames <- input$waterNameSearch
  areaBios <- input$areaBioSearch
  spConBios <- input$SpConBioSearch
  
  selectedBios <- tbl(dbConection, "tableName") %>%
    filter(AreaBio %in% areaBios)
  selectedWaterNames <- selectedBios %>%
    distinct(WaterName) %>%
    pull()
  
  #print(selectedWaterNames)
  updateVirtualSelect(
    session,
    "waterNameSearch", 
    choices = selectedWaterNames()
    
  )

  
}, ignoreInit = TRUE)
#######

data <- tbl(CPW_AqDatAnalysis, "SampleFView") 
areaBios <- "brandtm"
data <- data %>%
  #!! bang bang operator tells it to evaluate this statement instead of looking for a column named areaBios; not sure if 100% needed but ok
  filter(AreaBio %in% !!areaBios)

x <- data %>%
  filter(WaterName == input$waterNameSearch, 
         year(SampleDate)==input$yearsSearch)
#coln names 
dbListFields(CPW_AqDatAnalysis, "CurrentSummary")
dbListFields(CPW_AqDatAnalysis, "SampleFView")

sampleFData <- tbl(CPW_AqDatAnalysis, "SampleFView")

allLengths <- sampleFData %>%
  distinct(Length_mm) %>%
  show_query() %>%
  pull() %>%
  sort()

x <- sampleFData %>%
  filter(as.numeric(Length_mm) > 500) %>%
  #count(Length_mm) %>%
  collect() #%>%
  #sort(Length_mm) 

NALengths <- sampleFData %>%
  filter(is.na(Length_mm)) %>%
  collect()
NASurveyIDs <- sampleFData %>%
  filter(is.na(SurveyID)) %>%
  collect()
cat(dbListTables(CPW_AqDatAnalysis), sep = ", ")
##strain translation table
StrainTranslationTable <- tbl(CPW_AqDatAnalysis, "StrainTranslationTable") %>%
  collect()
stockingRecordsView <- tbl(CPW_AqDatAnalysis, "StockingRecordsView") %>%
  collect()
# have any of the rainbow strains been stocked 
stockedStrains <- stockingRecordsView %>%
  filter(#SpeciesCode %in% c(StrainTranslationTable$SpeciesCode), 
         WaterTypeName == "Stream", 
         year(Planted) >= "2024", 
         AreaBioName %in% "Kendall Bakich")
######
sampleFData <- tbl(CPW_AqDatAnalysis, "SampleFView") 
survey1239 <- sampleFData %>%
  filter(SurveyID == 1239) %>%
  collect()

survey1239 %>%
  ggplot(aes(x = RSD)) +
  geom_histogram(stat = "count")

plot <- survey1239 %>%
  ggplot(aes(x = RSD, fill = CommonName
             
  ))

plot1 <- plot +
  geom_histogram(stat = "count")

#"Memorable" "Preferred" "Quality"   "Stock"     "Trophy"   
survey1239Data <- survey1239 %>%
  count(RSD, CommonName, name = "Count") %>%
  left_join(rsdLimits, by = "CommonName") %>%
  mutate(hoverText = case_when(RSD == "Stock" ~ paste0(CommonName, " 'Stock' Range (mm): ", SLEN, " - ", QLEN), 
                               RSD == "Quality" ~ paste0(CommonName, " 'Quality' Range (mm): ", QLEN, " - ", PLEN), 
                               RSD == "Preferred" ~ paste0(CommonName, " 'Preferred' Range (mm): ", PLEN, " - ", MLEN), 
                               RSD == "Memorable" ~ paste0(CommonName, " 'Memorable' Range (mm): ", MLEN, " - ", TLEN), 
                               RSD == "Trophy" ~ paste0(CommonName, " 'Trophy' Range (mm): ", TLEN, "+"), 
                               TRUE ~ paste0(CommonName, ": No RSD Assigned")
                               
                               ), 
         RSD = factor(RSD, levels = c("Stock", "Quality", "Preferred", "Memorable", "Trophy"))) %>%
  ungroup()
plot1 <- ggplot(survey1239Data, aes(x = RSD, y = Count, fill = CommonName, text = hoverText)) +
  geom_col()
# plot1 <- ggplot(survey1239Data, aes(x = RSD, fill = commonName)) +
#   geom_histogram(
#     stat = "count",
#     aes(
#       # Map custom text and labels to 'dummy' aesthetics so they persist
#       label = hoverText,
#       text = paste0(after_stat(label), "<br>Count: ", after_stat(count))
#     )
#   )
ggplotly(plot1, tooltip = "text")

plot <- survey1239Data %>%
  ggplot(aes(x = RSD, y = Count,
             fill = CommonName)) +
  theme_classic() +
  labs(title = "Length Frequencies", caption = "Binwidth = 20mm")

plot1 <- plot +
  geom_histogram(#stat = "count",
                 aes(
                   # group = CommonName,
                   # #label = .data[[RSD]],
                   # fill = CommonName,
                   # label = paste0( 'RSD: ', RSD,
                   #                 '<br>Species: ', CommonName
                   # ),
                   text = paste0(#'RSD: ', RSD,
                                 '<br>Species: ', CommonName,
                                 "<br>Count: ", after_stat(count)
                   )
                 )
                 
  )
ggplotly(plot1, tooltip = "text")

sampleFData <- tbl(CPW_AqDatAnalysis, "SampleFView")

allDistinctRSD <- sampleFData %>%
  distinct(RSD) %>%
  show_query() %>%
  pull() %>%
  sort()
#rsd limits 
individvualrsd <- tbl(CPW_AqDatAnalysis, "IndividualRSDView") %>%
  collect()

rsdLimits <- tbl(CPW_AqDatAnalysis, "RSDLimitsView") %>%
  collect()
species <- tbl(CPW_AqDatAnalysis, "SpeciesView") %>%
  collect()
species1 <- tbl(CPW_AqDatAnalysis, "SpeciesOccursWhereView") %>%
  collect()
### combined summaries table 
singlesurvey <- sampleFData %>%
  filter(SurveyID == "42671") %>%
  collect()

#proportinal stock density table
proportionalstockdensitytable <- singlesurvey %>%
  group_by(CommonName) %>%
  summarize(`Total Catch` = sum(NumFish),
            #not entirely sure why using mean works but it does
            `Percent Stock size` = round(mean(RSD == "Stock", na.rm = TRUE) *100, 2), 
            `Percent Quality size` = round(mean(RSD == "Quality", na.rm = TRUE) *100, 2), 
            `Percent Preferred size` = round(mean(RSD == "Preferred", na.rm = TRUE) *100, 2), 
            `Percent Memorable size` = round(mean(RSD == "Memorable", na.rm = TRUE) *100, 2), 
            `Percent Trophy size` = round(mean(RSD == "Trophy", na.rm = TRUE) *100, 2), 
            `Max Length (mm)` = max(Length_mm, na.rm = TRUE)
            )
### mean min max length and weight
meanMinMaxLengthWeights <- singlesurvey %>%
  group_by(CommonName) %>%
  summarize(`Total Catch` = sum(NumFish), 
            `Average Length (mm)` = round(mean(Length_mm, na.rm = TRUE), 2), 
            `Median Length (mm)` = round(median(Length_mm, na.rm = TRUE), 2), 
            `Min Length (mm)` = round(min(Length_mm, na.rm = TRUE), 2),
            `Max Length (mm)` = round(max(Length_mm, na.rm = TRUE), 2), 
            `Standard Deviation (mm)` = round(sd(Length_mm, na.rm = TRUE), 2), 
            
            `Average Weight (g)` = round(mean(Weight_g, na.rm = TRUE), 2), 
            `Median Weight (g)` = round(median(Weight_g, na.rm = TRUE), 2), 
            `Min Weight (g)` = round(min(Weight_g, na.rm = TRUE), 2),
            `Max Weight (g)` = round(max(Weight_g, na.rm = TRUE), 2), 
            `Standard Deviation (mm)` = round(sd(Weight_g, na.rm = TRUE), 2), 
  )

#### relative abundace and CPUE
relAbundanceCPue <- singlesurvey %>%
  group_by(CommonName) %>%
  summarize(`Total Catch` = sum(NumFish), 
            `Weight Kg` = round(sum(Weight_g, na.rm = TRUE)/1000, 2), 
            #sums whole column numfish but ignores group_by()
            #could also do the same thing with mutating after
            `Percent Total Catch` = round(sum(NumFish, na.rm = TRUE)/sum(.$NumFish, na.rm = TRUE) *100, 2), 
            `Percent Total Weight` = round(sum(Weight_g, na.rm = TRUE)/sum(.$Weight_g, na.rm = TRUE) *100, 2), 
  )
###
abundanceBiomass <- singlesurvey %>%
  group_by(CommonName) %>%
  summarize(`Total Catch` = sum(NumFish), 
            `Weight Kg` = round(sum(Weight_g, na.rm = TRUE)/1000, 2), 
            #sums whole column numfish but ignores group_by()
            #could also do the same thing with mutating after
            `Percent Total Catch` = round(sum(NumFish, na.rm = TRUE)/sum(.$NumFish, na.rm = TRUE) *100, 2), 
            `Percent Total Weight` = round(sum(Weight_g, na.rm = TRUE)/sum(.$Weight_g, na.rm = TRUE) *100, 2), 
  )
sampleFData <- singlesurvey
# styles
# text coolor in virtualSelectINputs
# /* Styling individual options */
#   .vscomp-option {
#     color: #245d38 !important;
#   }

# /* Background of the main input box */
#   .vscomp-wrapper {
#     border: 1px solid #245d38 !important;
#   }

.navbar-default {
  background-color: #245d38 !important; /* CPW Green */
    border-color: #1e4d2e !important;
}

/* The text/labels in the navbar */
  .navbar-default .navbar-nav > li > a, 
.navbar-default .navbar-brand {
  color: #FFFFFF !important; /* White text */
}

/* Hover and Active states */
  .navbar-default .navbar-nav > li > a:hover,
.navbar-default .navbar-nav > .open > a:hover {
  background-color: #1e4d2e !important; /* Slightly darker green on hover */
    color: #ffd100 !important; /* CPW Yellow on hover */
}

/* Fix the dropdown arrow color to match white text */
  .navbar-default .navbar-nav .dropdown .caret {
    border-top-color: #FFFFFF !important;
      border-bottom-color: #FFFFFF !important;
  }

.dropdown-menu {
  background-color: #245d38 !important;
    border: 1px solid #1e4d2e;
}

/* Dropdown text color */
  .dropdown-menu > li > a {
    color: #FFFFFF !important;
      padding: 10px 20px;
  }

/* Hover effect inside the dropdown */
  .dropdown-menu > li > a:hover {
    background-color: #1e4d2e !important;
      color: #ffd100 !important; /* CPW Yellow */
  }

.navbar-default .navbar-nav > .dropdown > a,
.navbar-default .navbar-nav > .dropdown > a:focus,
.navbar-default .navbar-nav > .active > a {
  background-color: #245d38 !important; /* Matches your main CPW Green */
    color: #FFFFFF !important;
}

/* If you want it to stay the darker green while open (to show it's active) */
.navbar-default .navbar-nav > .open > a {
    background-color: #1e4d2e !important; 
}

.dropdown-menu > .active > a, 
.dropdown-menu > .active > a:hover, 
.dropdown-menu > .active > a:focus {
    background-color: #1e4d2e !important; /* Darker CPW Green */
    color: #ffd100 !important;            /* CPW Yellow text for the active item */
}

.navbar {
    border-bottom: 3px solid #ffd100 !important; /* 3px thick, solid CPW Yellow */
}

.nav-tabs > li.active > a,
.btn-default {
    
    
    background-color: #001970 !important; /* CPW Blue */
    color: #FFFFFF !important;
    border-color: #000c35 !important;
    font-weight: bold !important;
}

.nav-tabs > li.active > a:hover,
.btn-default:hover {
    
    background-color: #000c35 !important;
    color: #ffd100 !important;
    border-color: #ffd100 !important;
}

.btn-cpw-sidebar {
    background-color: #245d38 !important; /* CPW Green */
    color: #FFFFFF !important;
    border-color: #1e4d2e !important;
    font-weight: bold;
}

/* Define the hover state for that custom class */
.btn-cpw-sidebar:hover {
    background-color: #1e4d2e !important;
    color: #ffd100 !important; /* Yellow text on hover */
    border-color: #ffd100 !important;
}

/* 1. Sidebar Background */
.well {
    background-color: #f1f8f3 !important; /* Very light green wash */
    border: 1px solid #245d38 !important;  /* CPW Green border */
    border-radius: 10px;
}

/* 2. Slider - The Bar (the background of the slider) */
.irs-bar, .irs-bar-edge {
    background-color: #245d38 !important; /* CPW Green for the 'filled' part */
    border-top: 1px solid #245d38 !important;
    border-bottom: 1px solid #245d38 !important;
}

.irs-line {
    background: #e2f0d9 !important; /* Light green for the 'unfilled' part */
    border: 1px solid #cbdcc0 !important;
}

/* 3. Slider - The Handle (the circle you grab) */
.irs-single, .irs-bar-edge, .irs-from, .irs-to {
    background: #245d38 !important; /* Tooltip backgrounds */
}

.irs-handle {
    background-color: #ffd100 !important; /* CPW Yellow handle to make it pop */
    border: 1px solid #245d38 !important;
    border-radius: 50% !important; /* Makes the handle a perfect circle */
}

/* 4. Slider - Text Labels (The numbers) */
.irs-grid-text {
    color: #245d38 !important;
    font-weight: bold;
}

/* --- Active Tab (The one currently selected) --- */
.nav-tabs > li.active > a, 
.nav-tabs > li.active > a:hover, 
.nav-tabs > li.active > a:focus {
    background-color: #245d38 !important; /* CPW Green */
    color: #FFFFFF !important;            /* White text */
    border: 1px solid #245d38 !important;
    border-radius: 4px 4px 0 0;           /* Rounds only the top corners */
}

/* --- Inactive Tabs (The ones waiting to be clicked) --- */
.nav-tabs > li > a {
    color: #245d38 !important;           /* CPW Green text */
    background-color: #f1f8f3 !important; /* Very light green background */
    border: 1px solid #e2f0d9 !important;
    margin-right: 2px;
}

/* --- Hover State for Inactive Tabs --- */
.nav-tabs > li > a:hover {
    background-color: #e2f0d9 !important; /* Slightly darker green wash */
    color: #1e4d2e !important;           /* Darker green text */
    border-color: #245d38 !important;
}


/* Background of the actual dropdown list (the 'dropbox') */
.vscomp-dropbox-container {
    background-color: #FFFFFF !important;
    border: 1px solid #245d38 !important;
}



/* Active/Hover state inside the dropdown */
.vscomp-option.active, .vscomp-option.focused {
    background-color: #245d38 !important; /* CPW Green */
    color: #FFFFFF !important;            /* White text */
}
