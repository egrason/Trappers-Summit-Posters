#################
## Trappers Summit Poster Figures
#################

#packages
library(ggplot2)
library(plyr)


#Set WD to Regional Posters Folder
setwd("~/Google Drive/Shared drives/Crab Team/Capacity Building/Partner Trapper Support/Trappers Summit/5. 2025 Dec Trappers Summit/Regional Posters/Hood Canal/")



#################
#Biometrics Plot
#################

#Change to appropriate regional folder 
biom <- read.csv("Demographics.csv", header = T)

biom$CaptureDate <- as.POSIXct(biom$CaptureDate, format = "%m/%d/%Y")
biom$CW_mm <- as.numeric(biom$CW_mm)
biom$Sex <- as.factor(biom$Sex)
biom$OptionalFactor <- as.factor(biom$OptionalFactor)

pdf("Biometrics.pdf", width = 10, height = 5)
ggplot(biom, aes(x = CaptureDate, y = CW_mm, group = Sex)) +
  geom_point(aes(x = CaptureDate, 
                 y = CW_mm, 
                 shape = Sex, 
                 color = factor(OptionalFactor), 
                 alpha = 0.7)) +
  theme_bw(base_size = 16) +
  guides(color=guide_legend("Coordination Area")) +
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

seasonal$EffortEndDate <- as.POSIXct(seasonal$EffortEndDate, format = "%m/%d/%Y")
#seasonal$WeekEndDate <- as.POSIXct(seasonal$WeekEndDate, format = "%m/%d/%y")
seasonal$MonthEndDate <- as.POSIXct(seasonal$MonthEndDate, format = "%m/%d/%Y")
seasonal$SiteName <- as.factor(seasonal$SiteName)

#To group by OptionalFactor
seasonal.of <- ddply(seasonal, c("OptionalFactor", "MonthEndDate"),
function(df) {
  return(
    c(
      TotalEGC = sum(df$EGC),
      TotalTraps = sum(df$TrapSets)
    )
  )
}
)

seasonal.of$CPUE <- 100*seasonal.of$TotalEGC/seasonal.of$TotalTraps

pdf("Seasonal.pdf", width = 10, height = 5)
ggplot(seasonal.of, aes(x = MonthEndDate, y = CPUE, group = OptionalFactor)) + 
  geom_line(aes(x = MonthEndDate, y = CPUE, 
                color = factor(OptionalFactor))) + 
  geom_point(aes(x = MonthEndDate, y = CPUE, 
                color = factor(OptionalFactor))) + 
  theme_bw(base_size = 16) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  guides(color=guide_legend("Coordination Area")) +
  ylab("Effort CPUE (per 100 trap sets)") +
  xlab("Month")
dev.off()

#################
#Annual Trends Plot
#################

annual <- read.csv("AnnualTrends.csv", header = T)

annual$SiteName <- as.factor(annual$SiteName)
annual$OptionalFactor <- as.factor(annual$OptionalFactor)


#To group by OptionalFactor
annual.of <- ddply(annual, c("OptionalFactor", "Year"),
                   function(df) {
                     return(
                       c(
                         TotalEGC = sum(df$AnnualEGCTotal),
                         TotalTraps = sum(df$TrapSets)
                       )
                     )
                   }
  
)
annual.of$CPUE <- 100*annual.of$TotalEGC/annual.of$TotalTraps

pdf("Annual.pdf", width = 10, height = 5)
ggplot(annual.of, aes(x = Year, y = CPUE, group = OptionalFactor)) + 
  geom_line(aes(x = Year, y = CPUE, 
                color = factor(OptionalFactor))) + 
  geom_point(aes(x = Year, y = CPUE, 
                color = factor(OptionalFactor))) + 
  theme_bw(base_size = 16) +
  guides(color=guide_legend("Coordination Area")) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(limits = c(2012, 2026)) +
  ylab("Effort CPUE (per 100 trap sets)") +
  xlab("Year")
dev.off()
