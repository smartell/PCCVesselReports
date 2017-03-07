# VesselQuota.r

library("RODBC")
library("dplyr")

# create a connection to acct
bcp <- odbcConnect(dsn="acct",uid="steve",pwd="seattle20160104")
print(bcp)

test <- " Select * From AnalysisHaulTable	Where year(HaulDate) = year(GetDate())
				 "

TMP <- sqlQuery(bcp,test)				 

oldquery <- "   select vesselE.Name VesselName, v.VesselFfp, qhEr.Description QuotaHolder,  a.QuotaHolderEntityRelationshipId, a.AccountHolderEntityRelationshipId,
		a.QuotaCategoryCode, sum(isnull(transIn.amount,0)) - sum(isnull(transOut.amount,0)) Allocation

		from account a

		join EntityRelationship vesselEr
		on a.AccountHolderEntityRelationshipId = vesselEr.id
		
		join Entity vesselE 
		on vesselEr.EntityId = vesselE.Id
		
		join Vessel v 
		on vesselE.Id = v.EntityId

		join EntityRelationship qhEr
		on a.QuotaHolderEntityRelationshipId = qhEr.id
	
		left join
		Transfer transIn 
		on transIn.ToAccountId = a.Id 
		
		left join
		Transfer transOut
		on transOut.FromAccountId = a.Id 
		
		--current year
		where a.Year = year(GetDate())
		and a.QuotaCategoryCode in ('500CHINOOK_A','500CHINOOK_B','500POLLOCK_A','500POLLOCK_B') 
		--PCC + CDQ pollock pools
		and a.QuotaHolderEntityRelationshipId in (select Id from entityrelationship where ParentEntityRelationshipid = 739 or id in (4, 50, 53, 56, 59, 62))

		group by
		vesselE.Name, v.VesselFfp, qhEr.Description, a.QuotaHolderEntityRelationshipId, a.AccountHolderEntityRelationshipId, a.QuotaCategoryCode

		order by 
	        vesselE.Name, v.VesselFfp, qhEr.Description, a.QuotaCategoryCode "


query <- "                 select ahE.Name, v.VesselFfp, ahE.EntityTypeCode AccountHolderEntityType, a.Id AccountId, ahEr.Description AccountHolder, 
		qhEr.Description QuotaHolder,  a.AccountHolderEntityRelationshipId, a.QuotaHolderEntityRelationshipId, 
		a.QuotaCategoryCode, isnull(transIn.amount,0) TransIn, isnull(transOut.amount,0) TransOut, 
		isnull(transIn.amount,0) - isnull(transOut.amount,0) CurrentAllocation

		from account a

		join EntityRelationship ahEr
		on a.AccountHolderEntityRelationshipId = ahEr.id
		
		join Entity ahE 
		on ahEr.EntityId = ahE.Id
		
		left join Vessel v
		on v.EntityId = ahE.Id

		join EntityRelationship qhEr
		on a.QuotaHolderEntityRelationshipId = qhEr.id
	
		left join
		(select ToAccountId, sum(amount) amount from Transfer group by ToAccountId) transIn 
		on transIn.ToAccountId = a.Id 
		
		left join
		(select FromAccountId, sum(amount) amount from Transfer group by FromAccountId) transOut
		on transOut.FromAccountId = a.Id 
		
		--current year
		where a.Year = year(GetDate())
		and a.QuotaCategoryCode in ('500CHINOOK_A','500CHINOOK_B','500POLLOCK_A','500POLLOCK_B') 
		--PCC + CDQ pollock pools
		and a.QuotaHolderEntityRelationshipId in (select Id from entityrelationship where ParentEntityRelationshipid = 739 or id in (4, 50, 53, 56, 59, 62))

		and ahE.EntityTypeCode = 'VESSEL' 

		order by 
	        ahE.Name, ahEr.Description, ahE.EntityTypeCode, qhEr.Description, a.QuotaCategoryCode; "


companyquery <- "  select ahE.Name, v.VesselFfp, ahE.EntityTypeCode AccountHolderEntityType, a.Id AccountId, ahEr.Description AccountHolder, 
		qhEr.Description QuotaHolder,  a.AccountHolderEntityRelationshipId, a.QuotaHolderEntityRelationshipId, 
		a.QuotaCategoryCode, isnull(transIn.amount,0) TransIn, isnull(transOut.amount,0) TransOut, 
		isnull(transIn.amount,0) - isnull(transOut.amount,0) CurrentAllocation

		from account a

		join EntityRelationship ahEr
		on a.AccountHolderEntityRelationshipId = ahEr.id
		
		join Entity ahE 
		on ahEr.EntityId = ahE.Id
		
		left join Vessel v
		on v.EntityId = ahE.Id

		join EntityRelationship qhEr
		on a.QuotaHolderEntityRelationshipId = qhEr.id
	
		left join
		(select ToAccountId, sum(amount) amount from Transfer group by ToAccountId) transIn 
		on transIn.ToAccountId = a.Id 
		
		left join
		(select FromAccountId, sum(amount) amount from Transfer group by FromAccountId) transOut
		on transOut.FromAccountId = a.Id 
		
		--current year
		where a.Year = year(GetDate())
		and a.QuotaCategoryCode in ('500CHINOOK_A','500CHINOOK_B','500POLLOCK_A','500POLLOCK_B') 
		--PCC + CDQ pollock pools
		and a.QuotaHolderEntityRelationshipId in (select Id from entityrelationship where ParentEntityRelationshipid = 739 or id in (4, 50, 53, 56, 59, 62))

		and ahE.EntityTypeCode = 'COMPANY' 

		order by 
	        ahE.Name, ahEr.Description, ahE.EntityTypeCode, qhEr.Description, a.QuotaCategoryCode; "


# deprecate -----
O <- sqlQuery(bcp,oldquery)
# ----- deprecate

allocations <- sqlQuery(bcp,query)
companytotal <- sqlQuery(bcp,companyquery)




odbcClose(bcp)

# deprecate -----
OLDPccAllocationTable <- O %>% 
	dplyr::select(
	              Vessel   = VesselName,
	              # Vessel   = Name,
	              VesselId = VesselFfp,
	              ManagementProgram = QuotaHolder,
	              QuotaHolderEntityRelationshipId,
	              AccountHolderEntityRelationshipId,
	              QuotaCategoryCode,Allocation) %>%
	tidyr::spread(QuotaCategoryCode,Allocation) %>% 
	dplyr::arrange(ManagementProgram,Vessel) %>%
	dplyr::mutate(VesselCompanyEntityRelationshipId=paste0(VesselId,
	                               QuotaHolderEntityRelationshipId))

# ----- deprecate

PccAllocationTable <- allocations %>% 
	dplyr::select(
	              # Vessel   = VesselName,
	              Vessel   = Name,
	              VesselId = VesselFfp,
	              ManagementProgram = QuotaHolder,
	              QuotaHolderEntityRelationshipId,
	              AccountHolderEntityRelationshipId,
	              QuotaCategoryCode,
	              Allocation = CurrentAllocation) %>%
	tidyr::spread(QuotaCategoryCode,Allocation) %>% 
	dplyr::arrange(ManagementProgram,Vessel) %>%
	dplyr::mutate(VesselCompanyEntityRelationshipId=paste0(VesselId,
	                               QuotaHolderEntityRelationshipId))

UnassignedChinook_A<- companytotal %>%
	dplyr::select(Vessel=Name,
	              AccountHolder,
	              QuotaHolder,
	              QuotaCategoryCode,
	              TransIn,
	              TransOut,
	              CurrentAllocation) %>%
	group_by(QuotaHolder,QuotaCategoryCode) %>% 
	summarise(Unassigned=sum(CurrentAllocation)) %>% 
	filter(QuotaCategoryCode=="500CHINOOK_A")















