library(shiny);library(data.table); library(ggplot2); library(RColorBrewer); library(plotly); library(shinythemes)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  fn.plot <- function(input) {
    ## Arguments
    cmetric <- "Rate"
    cage <- input$cage
    cmeasure <- "Deaths"
    cloc <- input$cloc
    start.year <- input$range[1]
    end.year <- input$range[2]
    ccause <- input$ccause
    
    all.plots <- T
    cause.plots <- T
    bar <- input$bar
    
    
    
    ## Paths
    data.dir <- "data/"
    data.path <- paste0(data.dir, "GBD16_results_all2.csv")
    policy.path <- paste0(data.dir, "ebi_policies.csv")
    ebi.path <- paste0(data.dir, "20180731_STAT Compiler All Countries (Prepped).csv")
    
    plot.dir <- "C:/Users/AustinC/OneDrive - bgC3/Documents/plots/U5M_plots/ebi_summary/"
    dir.create(plot.dir, showWarnings = F)
    
    ### Code
    ## Data prep
    # Read data
    dt <- fread(data.path)[year %in% start.year:end.year]
    policy.dt <- fread(policy.path)[start_year %in% start.year:end.year & outlier == 0]
    ebi.dt <- fread(ebi.path)[!is.na(value) & year %in% start.year:(end.year + 1)]
    
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
    
    # Calculate 5 year averages
    bound.dt[year %%5 != 0, period := paste0((year - year%%5), "-", (year - year%%5 + 5))]
    bound.dt[year %%5 == 0, period := paste0((year - year%%5 - 5), "-", (year - year%%5))]
    period.dt <- bound.dt[, .(five_avg = mean(val)), by = .(age, metric, period, measure, location, cause)]
    mort.dt <- merge(bound.dt, period.dt)
    
    # Nepal DPT3 coverage from WHO
    dpt3 <- data.table(location = "Nepal", Category = "DPT", Indicator = "DPT3 Vaccine", year = 2017:1980,
                       value = c(90, 87, 91, 92, 92, 90, 92, 82, 89, 82, 82, 89, 75, 80, 78, 72, 72, 74, 65, 71, 78, 65, 54, 54, 51, 49, 46, 43, 44, 45, 46, 38, 32, 27, 23, 18, 16, 8))
    ebi.dt <- rbind(ebi.dt, dpt3[year %in% start.year:end.year])
    
    # Order policies
    policy.dt <- policy.dt[order(start_year)]
    
    ### Stats
    # Change between 2000 and 2015
    start.val <- mort.dt[metric == "Rate" & age == "Under 5" & year == start.year & measure == "Deaths" & location == cloc, .(cause, val)]
    setnames(start.val, "val", "start_val")
    end.val <- mort.dt[metric == "Rate" & age == "Under 5" & year == end.year & measure == "Deaths" & location == cloc, .(cause, val)]
    setnames(end.val, "val", "end_val")
    change.dt <- merge(start.val, end.val)
    change.dt[, change := (start_val - end_val) / start_val * 100]
    
    # Rankings
    start.val[order(start_val)]
    
    ## Table of improvement given uncertainty
    bound2.dt <- rbind(mort.dt, dpt.hold, fill = T)
    yr1 <- start.year
    yr2 <- end.year
    subset.mort <- bound2.dt[metric == "Rate" & age == "Under 5" & year %in% c(yr1, yr2) & measure == "Deaths", .(location, cause, year, val, upper, lower)]
    # mean.dt <- dcast(subset.mort, location + cause ~ year, value.var = "val")
    subset.mort[, cast_val := ifelse(year == start.year, lower, upper)]
    cast.dt <- dcast(subset.mort, location + cause ~ year, value.var = "cast_val")
    cast.dt[, sig := ifelse(get(as.character(start.year)) > get(as.character(end.year)), 1, 0)]
    cast.dt[is.na(sig), sig := 0]
    
    ### Plots
    ## All cause with EBI Timeline
    if(bar) {
      plot.path <- paste0(plot.dir, cloc, "_bar_plots.pdf")
    } else {
      plot.path <- paste0(plot.dir, cloc, "_line_plots.pdf")
    }
    
    ## Set preferences
    # Subset datasets to desired level of specificity
    subset.mort <- copy(mort.dt[metric == cmetric & age == cage & measure == cmeasure & location == cloc])
    subset.ebi <- copy(ebi.dt[location == cloc])
    subset.policy <- copy(policy.dt[location == cloc])
    
    # Set cause order
    cause.order <- copy(subset.mort[year == start.year, .(cause, val)])
    causes <- cause.order[rev(order(val))]$cause
    subset.mort$cause <- factor(subset.mort$cause, levels= rev(causes))
    
    # Set color
    color <- brewer.pal(11, "Spectral")
    names(color) <- unique(subset.mort[cause != "All causes"]$cause)
    
    ## All-cause plot
    if(ccause == "All causes") {
      decline <- round(change.dt[cause == "All causes", change], 1)
      sig <- cast.dt[location == cloc & cause == "All causes", sig]
      # Mortality
      if(bar) {
        gg <- ggplot() + geom_bar(data = subset.mort[cause != "All causes" & year > start.year], stat = "identity", width = 1, aes(x = year, y = five_avg, fill = cause))
      } else {
        gg <- ggplot() + geom_area(data = subset.mort[cause != "All causes" & year %in% seq(start.year, end.year, 5)], stat = "identity", aes(x = year, y = val, fill = cause))    
      }
      # Policies
      if(nrow(subset.policy) > 0) {
        policy.y <- 1.1 * rev(seq(subset.mort[cause == "All causes" & year == max(policy.dt$start_year), val], max(subset.mort$val), length = nrow(policy.dt)))
        gg <- gg + geom_segment(data = policy.dt, aes(x = start_year, y = 0, xend = start_year, yend = policy.y - 5), linetype = "dashed") +
          geom_text(data = policy.dt, aes(x = start_year, y = policy.y, label = policy_name), size = 3, hjust = "left", nudge_x = 0.05)
      }
      # Format
      gg <- gg + ggtitle(paste0(cage, " mortality in ", cloc, ":\n", abs(decline), "% ",ifelse(decline > 0, "decline", "increase"), " ", start.year, "-", end.year,  ifelse(sig == 1, "*", ""))) + ylab(paste(cmeasure, cmetric)) + xlab("Year") + labs(fill = "Cause") + theme_bw() + 
        theme(text = element_text(size=15), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + expand_limits(y = 1.2 * max(subset.mort$val)) +
        scale_fill_manual(values = color) + guides(fill = guide_legend(reverse = T))
      if(sig == 1) {
        gg <- gg + labs(caption="* statistically significant change over the time period")
      }
      # print(ggplotly(gg))
      print(gg)
    }
    
    ## Cause-specific plot
    if(ccause != "All causes") {
      max.val <- max(subset.mort[cause != "All causes"]$val)
      # Prep policy height
      if(nrow(subset.policy) > 0) {
        subset.mort[, start_year := year]
        policy.dt <- merge(policy.dt, subset.mort[, .(location, cause, start_year, val)], by = c("location", "cause", "start_year"))      
      }
      decline <- round(change.dt[cause == ccause, change], 1)
      sig <- cast.dt[location == cloc & cause == ccause, sig]
      plot.dt <- mort.dt[year >= 1990 & metric == cmetric & age == cage & measure == cmeasure & location == cloc & cause == ccause]
      if(bar) {
        gg <- ggplot() + geom_bar(data = plot.dt[year > start.year], stat = "identity", width = 1, aes(x = year, y = five_avg, fill = cause))
      } else {
        gg <- ggplot() + geom_area(data = plot.dt[year %in% seq(start.year, end.year, 5)], aes(x = year, y = val, fill = cause), alpha = 0.8, size = 2) 
      }
      gg <- gg + ggtitle(paste0(ccause, " mortality in ", cloc, ":\n", abs(decline), "% ", ifelse(decline > 0, "decline", "increase"), " 2000-2015",  ifelse(sig == 1, "*", ""))) + 
        ylab(paste(cmeasure, cmetric)) + xlab("Year") + labs(fill = "Cause") + theme_bw() + 
        theme(text = element_text(size=15), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + expand_limits(y = c(0, max.val)) +
        scale_fill_manual(values = color, guide = FALSE)
      if(ccause %in% unique(subset.ebi$Category)) {
        gg <- gg + geom_line(data = subset.ebi[Category == ccause], aes(x = year, y = value * max.val / 100, group = Indicator)) +
          geom_point(data = subset.ebi[Category == ccause], aes(x = year, y = value * max.val / 100, shape = Indicator), size = 3) +
          scale_y_continuous(sec.axis = sec_axis(~. *100 / max.val, name = "Coverage")) +
          guides(shape=guide_legend(nrow=length(unique(subset.ebi[Category == ccause]$Indicator)),byrow=TRUE, title = "EBI Coverage Indicators"))
      }
      if(ccause %in% subset.policy$cause) {
        policy.y <- 20 + rev(seq(plot.dt[year == max(policy.dt[cause == ccause]$start_year), val], max(plot.dt$val), length = nrow(policy.dt[cause == ccause])))
        gg <- gg + geom_segment(data = policy.dt[cause == ccause], aes(x = start_year, y = 0, xend = start_year, yend = policy.y), linetype = "dashed") +
          geom_text(data = policy.dt[cause == ccause], aes(x = start_year, y = policy.y, label = policy_name), size = 3, hjust = "left", nudge_x = 0.1)
      }
      if(sig == 1) {
        gg <- gg + labs(caption="* statistically significant change over the time period")
      }
      # gg <- ggplotly(gg)
      gg
    }
  }
  
  output$plot <- renderPlot({
    fn.plot(input)
  })
  
  plotInput <- function() {
    fn.plot(input)
  }
  
  output$Download <- downloadHandler(
    function() {
      paste0(input$cloc,"_", input$ccause, ".png")
    },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png", height = 8.5, width = 11)
    }  
  )
})