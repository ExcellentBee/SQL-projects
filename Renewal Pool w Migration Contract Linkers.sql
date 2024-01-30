DROP TABLE IF EXISTS #CPQSubs

SELECT    S.SubscriptionSFID
         ,S.SubscriptionSFID_ProgramSub
         ,S.StandardContractSFID
         ,S.AccountSFID
         ,S.RecordType_Sub
         ,S.StartDate
         ,S.EndDate
         ,S.CustomerAmount
         ,S.ListPrice
         ,S.ProgramSFID
         ,S.ProgramAcronym
         ,S.Class_1
         ,S.CreatedDate
         ,S.CPQ_Type
         ,S.ContractStartDate
         ,S.ContractEndDate
         ,O.SO_RenewedContract AS StandardContractSFID_Prior
         ,PO.SO_RenewedContract AS StandardContractSFID_Prior_PHXMig
INTO      #CPQSubs
FROM      DBAnalytics..CPQ_Subscriptions AS S
          INNER JOIN DBMigration.SBQQ__Subscription__c AS sS 
						ON S.SubscriptionSFID = sS.Id
          INNER JOIN DBMigration.Opportunity AS O 
						ON S.OppSFID_Source = O.Id
          LEFT JOIN  DBMigration.Opportunity AS PO 
						ON O.Parent_Opportunity__c = PO.Id

WHERE     S.CustomerAmount != 0
          OR ( S.CPQ_Type = 'NewMigration' AND SS.RevenueListPrice__c > 0 )
          OR ( S.CPQ_Type = 'Legacy' AND SS.SBQQ__ListPrice__c > 0 )

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
DROP TABLE IF EXISTS #Pool;

SELECT    PR.Class_3 AS Vertical
         ,PR.Class_2 AS ProductGroup
         ,PR.SourceObjectSFID
         ,PR.SourceObjectName
         ,PR.AccountSFID
         ,PR.AccountName
         
		 ,CASE WHEN A.AccountSegment LIKE 'KDA%' THEN 'KDA'
               WHEN A.AccountSegment = 'Other%' THEN 'Others'
               ELSE A.AccountSegment END AS AccountSegment

         ,CASE WHEN UG.Shared_Ind = 1 THEN 'Shared'
               ELSE 'Standalone' END AS SharedUG_Ind

         ,CASE WHEN PR.ReportingGroup LIKE '%Moonshot%'
                    AND  PR.AccountSegment = '2 Year' THEN 'Tier 1 Hub'
               WHEN PR.ReportingGroup LIKE '%Moonshot%'
                    AND  PR.AccountSegment != '2 Year' THEN 'Tier 2 Hub'
               ELSE PR.ReportingGroup END AS Program
         
		 ,PR.ProgramAcronym AS Acronym

         ,CASE WHEN PR.DecisionType LIKE 'Fixed Term%' THEN 'Fixed Term'
               ELSE PR.DecisionType END AS DecisionType
         
		 ,CASE WHEN PR.DecisionType = 'Need Renewal Contract'
                    AND  sfSP.Autorenew__c = 'Yes' THEN 1
               ELSE 0 END AS AutoRenew
         
		 ,PR.Stage
         ,PR.PreviousYearCredit
         
		 ,CASE WHEN PR.Class_3 = 'Research'
                    AND  PR.Stage = 'Pending'
                    AND  PR.DecisionType LIKE 'Fixed Term%' THEN 0
               WHEN PR.Class_3 = 'Research'
                    AND  PR.Stage = 'Pending' THEN PR.ProposalValue
               ELSE PR.AnticipatedRenewalCredit END AS ProposalValue

         ,CASE WHEN PR.DecisionType LIKE 'Fixed Term%'
                    AND  PR.Stage = 'Pending'
                    AND  PR.Class_3 = 'Research'
                    AND  PR.Projection_80_50 = '80' THEN PR.ProposalValue
               WHEN PR.DecisionType LIKE 'Fixed Term%'
                    AND  PR.Stage = 'Pending'
                    AND  PR.Class_3 = 'Research'
                    AND  PR.Projection_80_50 = '50' THEN PR.ProposalValue * .5

               WHEN PR.Class_2 = 'VTVT'
                    AND  PR.DecisionType = 'Need Renewal Contract'
                    AND  PR.Stage NOT LIKE '%Drop%'
                    AND  ( PR.RenewalHealth IS NULL OR PR.RenewalHealth != 'Drop' )
                    AND  sfSP.Autorenew__c = 'Yes'
                    AND  PR.RenewalCredit = 0
                    AND  ( CASE WHEN sfSP.Autorenewal_Notification_Date__c = '//' THEN NULL
                                WHEN sfSP.Autorenewal_Notification_Date__c IS NULL THEN NULL
                                ELSE CAST(sfSP.Autorenewal_Notification_Date__c AS DATE)END ) < GETDATE() THEN PR.ProposalValue
               ELSE PR.RenewalCredit END AS RenewalCredit
         
		 ,sfSP.Account_Health_YV__c AS AccountHealth
         
		 ,CASE WHEN PR.Stage = 'Pending' THEN PR.RenewalHealth ELSE NULL END AS RenewalHealth
         
		 ,PR.PoolDate
         ,PR.PooLYear_FY
         ,PR.PoolHalf_FY
         ,PR.PoolQuarter_FY
         ,PR.PeriodStartDate

		 ,PS.ProgramSubName AS CPQ_SubscriptionID
		 ,CAST(Subscription.DB_ContractNumber AS TEXT) AS StandardContract

         ,S.SubscriptionSFID AS SubscriptionSFID_Program

         ,CAST( NULL AS VARCHAR(18) ) AS StandardContractSFID_Proof
         ,CAST( NULL AS TEXT ) AS StandardContractNum_Proof
         ,CAST( NULL AS VARCHAR(100) ) AS SCProof_Mapping

INTO      #Pool
FROM      DBAnalytics..MSAM_ProgramRenewals AS PR
          LEFT JOIN DBAnalytics..MS_Accounts AS A 
					ON PR.AccountSFID = A.AccountSFID
          LEFT JOIN DBAnalytics..sf_Program AS P 
					ON PR.ProgramSFID = P.ProgramSFID
          LEFT JOIN DBAnalytics..sf_Predecessor AS Contracts 
					ON PR.PredecessorSFID_PreviousContract = Contracts.PredecessorSFID
          LEFT JOIN DBAnalytics.CPQ_Subscriptions AS S 
					ON PR.SubscriptionSFID = S.SubscriptionSFID
          LEFT JOIN DBMigration.Subscription_Period__c AS sfSP 
					ON PR.SubPeriodSFID = sfSP.Id

		  ---- To get CPQ_SubscriptionID  
		  LEFT JOIN  DBAnalytics..sf_ProgramSubscription AS PS
                    ON PR.SubscriptionSFID = PS.ProgramSubSFID

		  --- To get Standard Contract ID on Subscription page
		  LEFT JOIN DBMigration.SBQQ__Subscription__c AS Subscription
					ON PR.SubscriptionSFID = Subscription.ID

		  LEFT JOIN (SELECT	REP.AccountID
                             ,REP.FiscalYear 
                             ,(1) AS Shared_Ind
                      
                      FROM	DataAnalysis..Royall_EnrollmentPackages AS REP
                      
                      WHERE   REP.Audience = 'UG accounts'
                              AND REP.AccountType IN ('renewal', 'first year free')
          
                      GROUP BY REP.AccountID
                              ,REP.FiscalYear ) AS UG

					ON UG.AccountID = PR.AccountSFID
					AND UG.FiscalYear = PR.PoolYear_FY

WHERE     PR.PooLYear_FY IN ( 2022, 2023, 2024, 2025, 2026, 2027 )
          AND PR.Class_3 IN ( 'Research', 'Technology', 'Consulting')
          AND PR.Stage != 'void';

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Making sure we don't have any duplicates
/*
SELECT    P.SourceObjectSFID
FROM      #Pool AS P
GROUP BY  P.SourceObjectSFID
HAVING    SUM(1) > 1
*/

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

DROP TABLE IF EXISTS #MatchEfforts

SELECT    P.SourceObjectSFID
         ,S.StandardContractSFID
         ,CASE WHEN PS.StandardContractSFID = S.StandardContractSFID AND PS.ProgramSFID = S.ProgramSFID THEN '1A: same contract + program'
               WHEN PS.StandardContractSFID = S.StandardContractSFID THEN '1B: same contract'
               WHEN PS.StandardContractSFID = S.StandardContractSFID_Prior AND PS.ProgramSFID = S.ProgramSFID THEN '2A: prior contract + program'
               WHEN PS.StandardContractSFID = S.StandardContractSFID_Prior_PHXMig AND PS.ProgramSFID = S.ProgramSFID THEN '2A: prior contract (PHX) + program'
               WHEN PS.StandardContractSFID = S.StandardContractSFID_Prior THEN '2B: prior contract + program'
               WHEN PS.StandardContractSFID = S.StandardContractSFID_Prior_PHXMig THEN '2B: prior contract (PHX) + program'
               WHEN PS.ProgramSFID = S.ProgramSFID AND P.PeriodStartDate = S.StartDate THEN '3A: same account + program + date'
               WHEN PS.ProgramSFID = S.ProgramSFID THEN '3B: same account + program'
               ELSE '3C: same account' END AS MatchCat
INTO      #MatchEfforts
FROM      #Pool AS P
          INNER JOIN DBAnalytics..CPQ_Subscriptions AS PS 
						ON P.SubscriptionSFID_Program = PS.SubscriptionSFID
          INNER JOIN #CPQSubs AS S
						ON P.PeriodStartDate <= S.StartDate
                        AND PS.Class_1 = S.Class_1
                        AND PS.AccountSFID = S.AccountSFID

DROP TABLE IF EXISTS #BestMatch;

WITH #Base AS (
     SELECT    *
              ,ROW_NUMBER() OVER( PARTITION BY MM.SourceObjectSFID ORDER BY MM.MatchCat ASC ) AS MatchPlace
     FROM      #MatchEfforts AS MM )

SELECT    *
INTO      #BestMatch
FROM      #Base AS B
WHERE     B.MatchPlace = 1

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

UPDATE    #Pool 
SET       StandardContractSFID_Proof = SC.StandardContractSFID
         ,StandardContractNum_Proof = CAST( SC.ContractNumber AS TEXT )
         ,SCProof_Mapping = BM.MatchCat
FROM      #Pool AS P
          INNER JOIN #BestMatch AS BM 
					ON P.SourceObjectSFID = BM.SourceObjectSFID
          INNER JOIN DBAnalytics..sf_StandardContract AS SC 
					ON BM.StandardContractSFID = SC.StandardContractSFID

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

SELECT * FROM #Pool;
