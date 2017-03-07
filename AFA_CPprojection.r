# AFA_CPprojection.r
# Steven Martell
# Feb 16, 2017.
library(tidyverse)
library(tidyr)
library(lubridate)
library(dplyr)
library(data.table)

# ---------------------------------------------------------------------------- #
#	GET DATA FROM LOCAL SOURCE OR FROM ACCT
# ---------------------------------------------------------------------------- #
# load("../../../database/AnalysisHaulTable.Rdata")

library("RODBC")
conn <- odbcConnect(dsn="acct",uid="steve",pwd="seattle20160104")

sql_AHT_data <- "Select * From AnalysisHaulTable	Where year(HaulDate) = year(GetDate())"
AnalysisHaulTable <- sqlQuery(conn,sql_AHT_data)

odbcClose(conn)

# ---------------------------------------------------------------------------- #
# FILTER BSAI Pollock
# ---------------------------------------------------------------------------- #
	# Filter Table for directed pollock
	A <- AnalysisHaulTable %>% 
		dplyr::filter(ManagementProgram %like% "pollock") %>%
		dplyr::filter(TripTarget == "Pollock") %>%
		dplyr::filter(year(HaulDate) == year(today())) %>%
		dplyr::select(HarvestingVessel,
		              VesselId,
		              ManagementProgram,
		              QuotaHolder,
		              QuotaHolderEntityRelationshipId,
		              PollockWt,
		              ChinookSalmonNo) %>%
		dplyr::group_by(Vessel=HarvestingVessel,
		                VesselId,
		                ManagementProgram,
		                QuotaHolder,
		                QuotaHolderEntityRelationshipId) %>%
		dplyr::summarize(ChinookSalmonNo = sum(ChinookSalmonNo,na.rm=TRUE),
		                 PollockWt = sum(PollockWt,na.rm=TRUE)) %>%
		dplyr::mutate(VesselCompanyEntityRelationshipId=paste0(VesselId,
	                               QuotaHolderEntityRelationshipId))

	CatchToDate <- A

	Pcc5DayRate <- AnalysisHaulTable %>% 
		dplyr::filter(ManagementProgram %like% "pollock") %>%
		dplyr::filter(TripTarget == "Pollock") %>%
		dplyr::filter(HaulDate == today()-5,grepl("AFA",ManagementProgram)) %>%
		dplyr::select(HarvestingVessel,
		              VesselId,
		              ManagementProgram,
		              QuotaHolder,
		              QuotaHolderEntityRelationshipId,
		              PollockWt,
		              ChinookSalmonNo) %>%
		# dplyr::group_by(Vessel=HarvestingVessel,
		#                 VesselId,
		#                 ManagementProgram,
		#                 QuotaHolder,
		#                 QuotaHolderEntityRelationshipId) %>%
		dplyr::summarize(PccRate = sum(ChinookSalmonNo,na.rm=TRUE)/sum(PollockWt,na.rm=TRUE)) 



# ---------------------------------------------------------------------------- #
# FUNCTIONAL Programming 
# ---------------------------------------------------------------------------- #
library(purrr)

# trying to make short term prediction using purr package and trends
AHT <- AnalysisHaulTable %>%
		dplyr::filter(ManagementProgram %like% "pollock") %>%
		dplyr::filter(TripTarget == "Pollock") %>%
		dplyr::filter(year(HaulDate) == year(today())) %>%
		dplyr::mutate(Year=year(HaulDate)) %>%
		dplyr::select(ManagementProgram,QuotaHolder,Vessel=HarvestingVessel,HaulDate,ChinookSalmonNo,PollockWt) %>%
		dplyr::mutate(Vessel=factor(Vessel)) %>%
		# dplyr::group_by(QuotaHolder) %>%
		dplyr::mutate(Rate=(ChinookSalmonNo)/(PollockWt),
		              doy =(yday(ymd(HaulDate)))) 

df <- data.frame(AHT)
M1 <- df %>% 
			split(.$Vessel) %>% 
			map( ~ loess(Rate ~ doy, data = .))




		# %>%
		# split(.$QuotaHolder) %>% 
		# map(~ lowess(Rate ~ HaulDate, data = .))

		# dplyr::group_by(HaulDate) 


# tmp <- left_join(A,PccAllocationTable,by="VesselCompanyEntityRelationshipId")



# 	A1<- A %>%
# 		dplyr::group_by(Year = year(HaulDate), WeekNumber, ManagementProgram) %>%
# 		dplyr::summarise(ChinookSalmonNo = sum(ChinookSalmonNo),
# 		                 PollockWt = sum(PollockWt)) %>%
# 		dplyr::mutate(Rate = ChinookSalmonNo/PollockWt)

# 	# 3. Calculate catch to date by Company.
# 	CatchToDate <- A1 %>%
# 		dplyr::filter(Year == year(today())) %>% 
# 		dplyr::group_by(ManagementProgram) %>%
# 		dplyr::summarise(Chinook=sum(ChinookSalmonNo),Pollock=sum(PollockWt)) %>%
# 		dplyr::mutate(Rate=round(Chinook/Pollock,3))

# 	# 4. Allocations for A-season
# 	allocation <- readxl::read_excel("CPSalmonRateProjections.xlsx",
# 	                                 sheet="Allocations")
# 	Table1 <- allocation %>%
# 		left_join(CatchToDate)


# # ---------------------------------------------------------------------------- #
# # Calculate daily rates for each year and Season
# # ---------------------------------------------------------------------------- #

# 	A2 <- A %>% 
# 		dplyr::group_by(Year=year(HaulDate)) %>% 
# 		dplyr::select(Year,HaulDate,WeekNumber,
# 		              VesselId,
# 		              PollockWt,ChinookSalmonNo)

#  p1 <-	ggplot(A2,aes(yday(HaulDate),(ChinookSalmonNo/PollockWt),color=factor(Year))) + 
#  				geom_point(size=0.4,alpha=0.2) + 
#  				facet_wrap(~Year) + 
#  				geom_smooth() + 
#  				coord_cartesian(ylim = c(0,0.1),xlim=c(160,300)) + 
#  				# coord_cartesian(ylim = c(0,0.2),xlim=c(20,90)) + 
#  				# ylim(c(0,0.05)) 
#  				# xlim(c(20,100))
#  				# xlim(c(160,300))
#  				labs(x="Day of Year",y="Chinook rate (#/mt)",color="Year")+ 
#  				ggtitle("B-Season Chinook bycatch rates") + 
#  				theme_bw(18) + 
#  				geom_hline(yintercept=c(0.0032,0.0106,0.0147),size=c(0.325))
#  				# geom_hline(yintercept=c(0.0584,0.0448,0.0667),size=c(0.325))


#  A2 <- A %>% 
#  			 dplyr::group_by(Year=year(HaulDate),WeekNumber) %>%
#  			 dplyr::summarise(PollockCPUE = sum(PollockWt)/sum(Duration),
#  			                  ChinookCPUE = sum(ChinookSalmonNo)/sum(Duration),
#  			                  Rate = sum(ChinookSalmonNo)/sum(PollockWt))

#   p2 <- ggplot(A2,aes(WeekNumber,ChinookCPUE/PollockCPUE,
#                       color=factor(Year))) +
#   			geom_point(size=0.2,alpha=0.4) + 
#   			geom_smooth(alpha=0.5,span=0.4) + 
#   			# xlim(c(1,26)) + 
#   			ylim(c(0,0.1)) + 
#   			facet_wrap(~Year) +
#   			labs(x="Week of year",
#   			     y="Trends in Chinook abundance relative to Pollock biomass\n(Chinook CPUE:Pollock CPUE)")

# # ---------------------------------------------------------------------------- #
# # Vessel Level Reports.
#  .VESSELS <- unique(A$HarvestingVessel)
#  .POLLOCK_A <- c(13036) 			
#  .POLLOCK_B <- c(15756)
#  .CHINOOK_A <- c(647)
#  .CHINOOK_B <- c(162)

#  Table1 <- A %>% 
#  				dplyr::filter(year(HaulDate) == year(Sys.Date()),
#  				              HarvestingVessel == .VESSELS[6]) %>%
#  				dplyr::group_by(WeekNumber) %>%
#  				dplyr::summarize(PollockWt = sum(PollockWt),
#  				                 ChinookSalmonNo = sum(ChinookSalmonNo)) %>%
#  				dplyr::mutate(WeekRate = ChinookSalmonNo/PollockWt) %>%
#  				dplyr::mutate(SeasonPollock=cumsum(PollockWt),
#  				              SeasonChinook=cumsum(ChinookSalmonNo)) %>%
#  				dplyr::mutate(SeassonRate=SeasonChinook/SeasonPollock)
 
 

#  Table2 <- Table1 %>% 
#  				dplyr::mutate(Allocation=.POLLOCK_A,
#  				              Balance = .POLLOCK_A - SeasonPollock) %>%
#  				dplyr::mutate(PollockBalance = .POLLOCK_A - SeasonPollock) %>%
#  				dplyr::mutate(ChinookBalance = .CHINOOK_A - SeasonChinook) %>%
#  				dplyr::mutate(ThresholdRate = ChinookBalance / PollockBalance)


# # ---------------------------------------------------------------------------- #
# # Company level with individual vessel accounts 				
# # ---------------------------------------------------------------------------- # 				
# 	# Allocation <- read.csv("VesselAllocations.csv")  %>% 
# 								# filter(Vessel != "Unassigned Allocation")


# 	AFA <- A %>%
# 		dplyr::filter(year(HaulDate) == year(Sys.Date())) %>%
# 		# dplyr::filter(ManagementProgram %like% "AFA") %>%
# 		dplyr::select(ManagementProgram,
# 		              # QuotaHolder,
# 		              HarvestingVessel,
# 		              HaulDate,
# 		              WeekNumber,
# 		              PollockWt,
# 		              ChinookSalmonNo) %>%
# 		dplyr::group_by(ManagementProgram,HarvestingVessel) %>%
# 		dplyr::summarize(Chinook=sum(ChinookSalmonNo),
# 		                 Pollock=sum(PollockWt))


# 	# Company <- factor(unique(AFA$QuotaHolder))
# 	Program <- factor(unique(AFA$ManagementProgram))


# 	# AFA.allocation <- Allocation %>% filter(Company %like% "AFA") %>%
# 	# 	dplyr::group_by(Company=ManagementProgram,Vessel=HarvestingVessel) %>%
# 	# 	dplyr::summarise(PollockA = sum(PollockA))




# 	tmp <- Allocation %>% 
# 		filter(ManagementProgram %in% unique(AFA$ManagementProgram))

# 	tmp$PollockWt = AFA$Pollock
# 	tmp$ChinookNo = AFA$Chinook

# 	T1 <- tmp %>% 
# 		mutate(ChinookBalance = ChinookA - ChinookNo,
# 		       PollockBalance = PollockA - PollockWt,
# 		       YtdRate=ChinookNo/PollockWt,
# 		       ThresholdRate = (ChinookA-ChinookNo)/(PollockA-PollockWt))













