#################
## Trappers Summit Poster Figures
#################

#packages
library(ggplot2)
library(plyr)


#Set WD to Regional Posters Folder
setwd("~/Google Drive/Shared drives/Crab Team/Capacity Building/Partner Trapper Support/Trappers Summit/5. 2025 Dec Trappers Summit/Regional Posters/Columbia River/")



#################
#Biometics Plot
#################

#Change to appropriate regional folder 
biom <- read.csv("Demographics.csv", header = T)

biom$CaptureDate <- as.POSIXct(biom$CaptureDate, format = "%m/%d/%y")
biom$CW_mm <- as.numeric(biom$CW_mm)
biom$Sex <- as.factor(biom$Sex)

pdf("Biometrics.pdf", width = 10, height = 5)
ggplot(biom, aes(x = CaptureDate, y = CW_mm, group = Sex)) +
  geom_point(aes(x = CaptureDate, 
                 y = CW_mm, 
                 shape = Sex, 
                 color = factor(SiteName), 
                 alpha = 0.7)) +
  theme_bw(base_size = 16) +
  guides(color=guide_legend("Site Name")) +
  guides(alpha = "none") +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ylab("Carapace Width (mm)") +
  xlab("Capture Date")
dev.off()


#################
#Seasonal Trends Plot
#################

seasonal <- read.csv("SeasonalTrends.csv", header = T)

seasonal$EffortEndDate <- as.POSIXct(seasonal$EffortEndDate, format = "%m/%d/%y")
seasonal$WeekEndDate <- as.POSIXct(seasonal$WeekEndDate, format = "%m/%d/%y")
seasonal$SiteName <- as.factor(seasonal$SiteName)

pdf("Seasonal.pdf", width = 10, height = 5)
ggplot(seasonal, aes(x = WeekEndDate, y = EffortCPUE, group = SiteName)) + 
  geom_line(aes(x = WeekEndDate, y = EffortCPUE, 
                color = factor(SiteName))) + 
  geom_point(aes(x = WeekEndDate, y = EffortCPUE, 
                color = factor(SiteName))) + 
  theme_bw(base_size = 16) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  guides(color=guide_legend("Site")) +
  ylab("Effort CPUE (per 100 trap sets)") +
  xlab("Week Ending")
dev.off()

#################
#Annual Trends Plot
#################

annual <- read.csv("AnnualTrends.csv", header = T)

annual$SiteName <- as.factor(annual$SiteName)

#To group by OptionalFactor
#annual.of <- ddply(annual, c("OptionalFactor", "Year"),
                   function(df) {
                     return(
                       c(
                         TotalCAMA = sum(df$AnnualCAMATotal),
                         TotalTraps = sum(df$TrapSets)
                       )
                     )
                   }
  
)
#annual.of$CPUE <- 100*annual.of$TotalCAMA/annual.of$TotalTraps

pdf("Annual.pdf", width = 19, height = 5)
ggplot(annual.of, aes(x = Year, y = CPUE, group = OptionalFactor)) + 
  geom_line(aes(x = Year, y = CPUE, 
                color = factor(OptionalFactor))) + 
  geom_point(aes(x = Year, y = CPUE, 
                color = factor(OptionalFactor))) + 
  theme_bw(base_size = 16) +
  guides(color=guide_legend("Coordination Area")) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ylab("Effort CPUE (per 100 trap sets)") +
  xlab("Year")
dev.off()
