# ---- Download WDI package and install

# --- Install World Development Indicators API if not already installed
install.packages("WDI")

# --- Clear the workspace
remove(list = ls())

# --- Load libraries & set working directory
libs <- c ("ggplot2", "dplyr", "RColorBrewer", "grid", "WDI", "zoo", "lubridate")

# --- Load required libraries
lapply(libs, require, character.only=T)

# --- Search WDI database for urban population variables
WDIsearch('urban population')

# --- Extract urban population variable into new data frame
# Select extra = TRUE to extract extra variables and use tbl_df for dplyr
d <- tbl_df(WDI(country = "all", indicator = "SP.URB.TOTL.IN.ZS",
         start = 1960, end = 2010, extra = TRUE, cache = NULL))

# Remove "(developing only)" from country name
d$country <- gsub("(developing only)", "", d$country, fixed = TRUE) 

# --- Browse the names to get a sense of what is in the data
head(d)
tail(d)
unique(d$iso3c)

# --- Looking for EAP, ECA, LAC, MENA, SA, SSA using dplyr and ISO3 codes
#     see http://data.worldbank.org/developers/api-overview?print&book_recurse
fd <- filter(d, iso3c == "EAP" | iso3c == "ECA" | iso3c == "LAC" |
          iso3c == "MNA" | iso3c == "SSA" ) %>%
          arrange(country, year)

# Define Global Development Lab Colors for potential use
# Lab RGB colors
redL   	<- c("#B71234")
dredL 	<- c("#822443")
dgrayL 	<- c("#565A5C")
lblueL 	<- c("#7090B7")
dblueL 	<- c("#003359")
lgrayL	<- c("#CECFCB")

# --- Make first basic spaghetti plot of data
p <- ggplot(fd, aes(y = SP.URB.TOTL.IN.ZS, x = year, colour = country)) + 
       geom_path(alpha = 0.1) + geom_point(aes(size = (SP.URB.TOTL.IN.ZS/10))) 

# --- Customize plot 
pp <- p + theme(legend.position = "top", legend.title=element_blank(), 
          panel.border = element_blank(), legend.key = element_blank(), 
          legend.text = element_text(size = 13), #Customize legend
          plot.title = element_text(hjust = 0, size = 16, face = "bold"), # Adjust plot title
          panel.background = element_rect(fill = "white"), # Make background white 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #remove grid    
          axis.ticks.y = element_blank(), #Remove axis
          axis.text.y = element_text(hjust = -0.5, size = 12, colour = dgrayL), #soften axis text
          axis.text.x = element_text(hjust = 1, size = 12, colour = dgrayL),
          axis.ticks.x=element_blank(), # remove x-axis ticks
          #plot.margin = unit(c(1,1,1,1), "cm"),
          plot.title = element_text(lineheight = .8)) + # Move plot title up
          scale_x_continuous(breaks = seq(1960, 2015, 5), expand = c(0.02,0.02)) + #customize x-axis
          scale_y_continuous(breaks = seq(10, 80, 10)) + # customize y-axis
          labs(x = "", y = "Urban population (% of total)\n", 
          title = "Urbanization rose the fatest in East Asia and the Pacific", size = 14) +
          scale_colour_brewer(palette="Set1")
pp

# --- Make first basic spaghetti plot of data
p <- ggplot(fd, aes(y = SP.URB.TOTL.IN.ZS, x = year, colour = country)) + 
  geom_line(alpha = 0.1) + geom_point(size = 3.5) 
p <- p + theme(legend.position = "top", legend.title=element_blank(), 
               panel.border = element_blank(), legend.key = element_blank(), 
               legend.text = element_text(size = 13), #Customize legend
               plot.title = element_text(hjust = 0, size = 16, face = "bold"), # Adjust plot title
               panel.background = element_rect(fill = "white"), # Make background white 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #remove grid    
               axis.ticks.y = element_blank(), #Remove axis
               axis.text.y = element_text(hjust = -0.5, size = 12, colour = dgrayL), #soften axis text
               axis.text.x = element_text(hjust = 1, size = 12, colour = dgrayL),
               axis.ticks.x=element_blank(), # remove x-axis ticks
               #plot.margin = unit(c(1,1,1,1), "cm"),
               plot.title = element_text(lineheight = .8)) + # Move plot title up
               scale_x_continuous(breaks = seq(1960, 2010, 10), expand = c(0.02,0.02)) + #customize x-axis
               scale_y_continuous(breaks = seq(0, 80, 10)) + # customize y-axis
               labs(x = "", y = "Urban population (% of total)\n", 
               title = "Urbanization rose the fatest in East Asia and the Pacific", size = 14) +
               scale_colour_brewer(palette="Set1") + facet_wrap(~country, ncol =5)




# --- Look at regional values
p <- ggplot(d, aes(y = SP.URB.TOTL.IN.ZS, x = year, colour = region)) + 
  geom_path(alpha = 0.9) + facet_wrap(~region) + theme(legend.position = "top")

p








