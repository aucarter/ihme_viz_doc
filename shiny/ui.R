# Setup
library(shiny);library(data.table); library(ggplot2); library(RColorBrewer); library(plotly); library(shinythemes)

# Paths
data.dir <- "data/"
data.path <- paste0(data.dir, "GBD16_results_all2.csv")

# Data
dt <- fread(data.path)
# Calculate other category
all.dt <- copy(dt[cause == "All causes"])
combined.dt <- dt[cause != "All causes", .(sum_val = sum(val)), by = .(age, metric, year, measure, location)]
merge.dt <- merge(all.dt, combined.dt, by = c("age", "metric", "year", "measure", "location"))                 
merge.dt[, diff := val - sum_val]
merge.dt[, c("val", "upper", "lower", "sum_val") := NULL]
setnames(merge.dt, "diff", "val")
merge.dt[, cause := "Other"]
bound.dt <- rbind(dt, merge.dt, fill = T)

# Combine diphtheria, whooping cough, tetanus into DPT
dpt.causes <- c("Diphtheria", "Whooping cough", "Tetanus")
dpt.hold <- dt[cause %in% dpt.causes]
dpt.dt <- dt[cause %in% dpt.causes, .(val = sum(val)), by = .(age, metric, year, measure, location)]
dpt.dt[, cause := "DPT"]
bound.dt <- rbind(bound.dt[!(cause %in% dpt.causes)], dpt.dt, fill = T)

# Define UI for application that plots random distributions 
shinyUI(
  fluidPage(
  # theme = shinythemes::shinytheme(theme = "lumen"),
  
  # Application title
  headerPanel("EBI viz"),
  
  sidebarPanel(
    # Location
    selectInput(inputId = "cloc", 
                label = "Location", 
                choices = sort(unique(bound.dt$location)),
                selected = "Nepal"),
    # Age
    selectInput(inputId = "cage", 
                label = "Age group", 
                choices = sort(unique(bound.dt$age)),
                selected = "Under 5"),
    
    # Cause
    selectInput(inputId = "ccause", 
                label = "Cause", 
                choices = sort(unique(bound.dt$cause)),
                selected = "All causes"),
    
    # Year range
    sliderInput(inputId = "range", 
                label = "Year Range:",
                min = 1990, max = 2015,
                step = 5,
                value = c(2000,2015),
                sep = ""),
    
    # 5-year average bar
    checkboxInput(inputId = "bar", 
                  label = "5-year Avg. (Bar Chart)",
                  value = FALSE),
    
    # Download
    downloadButton("Download"),
    width = 3
  ),
  

  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("plot")
  )
))