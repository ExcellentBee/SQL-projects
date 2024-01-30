-- REVENUE EXTRACT BUILD for FY24 - Aligned each campaign with its FY23 CV

-- Create temp tables to store data during iteration of line categorization & Set Signature Date to cut off query
IF OBJECT_ID('tempdb.dbo.#SF_PP_Raw_Data', 'U') IS NOT NULL
   DROP TABLE #SF_PP_Raw_Data

IF OBJECT_ID('tempdb.dbo.#SF_PP_Data', 'U') IS NOT NULL
   DROP TABLE #SF_PP_Data

IF OBJECT_ID('tempdb.dbo.#SF_PP_Final_Cut', 'U') IS NOT NULL
   DROP TABLE #SF_PP_Final_Cut

IF OBJECT_ID('tempdb.dbo.#PP_FY24Contracted', 'U') IS NOT NULL
DROP TABLE #PP_FY24Contracted

IF OBJECT_ID('tempdb.dbo.#PP_FY23Pool', 'U') IS NOT NULL
DROP TABLE #PP_FY23Pool

IF OBJECT_ID('tempdb.dbo.#PP_FY24_RevExtract', 'U') IS NOT NULL
DROP TABLE #PP_FY24_RevExtract

-- HD has components in both HD stand-alone package and UG package ---> Campaign names and codes need to be consistent
IF OBJECT_ID('tempdb.dbo.#HD_ProductGroup', 'U') IS NOT NULL DROP TABLE #HD_ProductGroup  

CREATE TABLE #HD_ProductGroup( HDCampaignCode nvarchar(2), HDProductgroup nvarchar(30))
INSERT INTO  #HD_ProductGroup( HDCampaignCode , HDProductgroup )  VALUES
('1181',   'Product1181'),
('1182',   'Product1182'),
('1184',   'Product1184'),
('1185',   'Product1185'),
('1187',   'Product1187'),
('1199',   'Discount (HD)'),
('1183',   'Other (HD)');
--SELECT * FROM #HD_ProductGroup;


-- Product Name, Product Parent Name are not always populated or correct
IF OBJECT_ID('tempdb.dbo.#ALR_ProductGroup', 'U') IS NOT NULL DROP TABLE #ALR_ProductGroup  

CREATE TABLE #ALR_ProductGroup( ALR_CampaignCode nvarchar(2), ALR_CampaignName nvarchar(30), ALR_ProductGroup nvarchar(30))
INSERT INTO  #ALR_ProductGroup( ALR_CampaignCode, ALR_CampaignName , ALR_ProductGroup )  VALUES
('1199',  'Discount - No Campaign',		'Discount'),
('1139',  'Product1139',				'Digital'),
('1138',  'Product1938',				'Digital'),
('1175',  'Product1175',				'Digital'),
('1130',  'Product1130',				'PPIQ'),
('1176',  'Product1176',				'PPIQ'),
('1131',  'Product1131',				'SSJR'),
('1137',  'Product1137',				'SSJR'),
('1135',  'Product1135',				'AWSS'),
('1133',  'Product1133',				'AWSS'),
('1132',  'Product1132',			    'Other ALR'),
('1136',  'Product1136',				'Other ALR'),
('1177',  'Product1177',				'Other ALR'),
('1178',  'Product1178',				'Other ALR'),
('1179',  'Product1179',				'Other ALR'),
('1180',  'Product1180',			    'Other ALR'),
('1186',  'Product1186',				'Other ALR');
--SELECT * FROM #ALR_ProductGroup;


-- Adv has many campaigns sharing a name but with different codes ---> Campaign names and codes need to be consistent
IF OBJECT_ID('tempdb.dbo.#Adv_ProductGroup', 'U') IS NOT NULL DROP TABLE #Adv_ProductGroup	

CREATE TABLE #Adv_ProductGroup( Adv_CampaignCode nvarchar(2), Adv_Campaign nvarchar(30), Adv_Productgroup nvarchar(30))
INSERT INTO  #Adv_ProductGroup( Adv_CampaignCode ,			   Adv_Campaign ,			  Adv_Productgroup )  VALUES
('1140',   'Product1140',		 'AdvGroup1'),
('1141',   'Product1141',		 'AdvGroup2'),
('1142',   'Product1142',		 'AdvGroup2'),
('1143',   'Product1143',        'AdvGroup2'),
('1144',   'Product1144',		 'AdvGroup3'),
('1145',   'Product1145',		 'AdvGroup4'),
('1146',   'Product1146',		 'AdvGroup4'),
('1147',   'Product1147',	     'AdvGroup4'),
('1148',   'Product1148',		 'AdvGroup5'),
('1149',   'Product1149',		 'AdvGroup5'),
('1150',   'Product1150',		 'AdvOthers'),
('1151',   'Product1151',		 'AdvOthers'),
('1152',   'Product1152',		 'AdvOthers'),
('1153',   'Product1153',		 'AdvOthers'),
('1186',   'Product1186',		 'AdvOthers'),
('1199',   'Discount',			 'Discount');
--SELECT * FROM #Adv_ProductGroup;


-- BoA contract is booked differently
IF OBJECT_ID('tempdb.dbo.#BOA', 'U') IS NOT NULL DROP TABLE #BOA	

CREATE TABLE	#BOA( BoA_Client_Code nvarchar(30), BoA_Client_Name varchar(200))
INSERT INTO		#BOA( BoA_Client_Code,				BoA_Client_Name)  VALUES
('5DL', 'BoA (NMSU)'),
('5LA', 'BoA (SC)'),
('5KV', 'BoA (CNMCC)'),
('5KW', 'BoA (CCoD)'),
('5KN', 'BoA (UoCFL)'),
('5KP', 'BoA (SDS)'),
('5KX', 'BoA (UCC)'),
('5KO', 'BoA (MSoD)'),
('5KQ', 'BoA (SEU)'),
('5KZ', 'BoA (UoTRGV)'),
('5LB', 'BoA (STC)'),
('5KY', 'BoA (VC)');
--SELECT * FROM #BOA;


DECLARE @LookBackDate DATETIME
SET @LookBackDate = Getdate()
DECLARE @Control_Year AS INT = 2024

-------ROUND 1----------------------------------------------------------------------------------------------------------------------------------------------------
------ Pull out all lines in SF as is to compared with Accounting records when there's descrepancy  ------------

Select  RAP.FiscalYear,
		RAP.DataSource As DataSource, 
		Cast (RAP.CreatedDate AS DATE) As CreatedDate, 
		RAP.FYVertAccountStatus AS AccountStatus,
		RAP.ProgramID,
		RAP.AccountID,
		
		(CASE WHEN RAP.Account LIKE 'BoA'	THEN	BOA.BoA_Client_Name		ELSE	RAP.Account END) AS Account,	
		
		RAP.SystemParent As AccountParent, 
		(CASE WHEN	RAP.Audience =  'Adv Group'	  THEN		SF_Ed_Acc.Client_Code_Adv__c
			  WHEN  RAP.Account LIKE 'BoA'		  THEN		LEFT(RAP.JobNumber,3)
			  ELSE	SF_Ed_Acc.r_Client_Code__c		END)		AS		ClientCode,			
		
		SF_Ed_Acc.Counter_ID__c As BinderClientCode,
		
		(CASE WHEN RAP.Account LIKE 'BoA' AND RAP.Campaign LIKE 'Other ALR' THEN 'STPL' ELSE RAP.Campaign END) as Campaign, 
		
		Cast (Right(RAP.JobNumber, 2)  as nvarchar(30))		AS CampaignCode,

		RAP.ProductGroup AS DB_ProductGroup,

		RAP.Audience as DB_Audience,
		
		---- Accounting classifies product group differently vs Pool
		CASE WHEN  RAP.Campaign		LIKE  '%Discount%'									THEN	'Discount'
			  WHEN	RAP.Audience		LIKE 'ALR Group'									THEN	ALR.ALR_ProductGroup
			  WHEN	RAP.Audience		LIKE 'Adv Group'									THEN	Adv.Adv_Productgroup
			  WHEN  RAP.Audience		LIKE 'HD Group'										THEN	HD.HDProductgroup	  		  		  			   	  
			  ELSE  RAP.ProductGroup	END						AS		 Model_ProductGroup, 
		
		---- Some ALR Clients have Audience as UG?
		CASE WHEN  RAP.Audience IN ('ALR Group', 'Adv Group')		THEN	RAP.Audience
			  ELSE	(CASE WHEN		SF_Ed_Acc.Record_Type_Text__c  LIKE 'ALR'	THEN	'ALR Group'	  ELSE     'UG Group'  END)	  
			  END      AS			Audience,
					
		RAP.JobNumber As JobNumber,
		
		RAP.Full_CVExclude,
		ProgPack.Name As ProgramPackageName,
		RAP.SignatureDate,
		RAP.ContractSentDate,
		RAP.ProposalSentDate,
		RAP.Renewal_Package_FirstSigned,
		RAP.Renewal_Campaign_FirstSigned,
		RAP.TermStartDate,
		RAP.TermEndDate,		
		RAP.PType,
		RAP.NBB,
		RAP.SystemCrossSellType,

		---- Create Revenue Accrual Type
		CASE WHEN		RAP.PType LIKE 'Discount Allocation'		THEN		'Discount Allocation'
			 WHEN		RAP.Campaign LIKE '%Discount%'				THEN
																			(Case When  PType  LIKE		'Cross-sell'   Then		'Cross-sell Discount'
																				  When  PType  LIKE 	'Up-sell'	   Then		'Up-sell Discount'		
																				  Else		'Discount'		END)
			
			 ELSE		CASE WHEN		(RAP.PType LIKE 'Modification'	  AND    RAP.DiscountReason LIKE 'NA-Cost/Volume Movement')	THEN 		'Cost Movement'   		 
							 WHEN		RAP.PType IN ('Up-sell', 'Cross-sell')	  AND	 ISNULL(RAP.ProgramCost, 0) < 0				THEN		RAP.PType +' Discount'
							 
							 WHEN       (RAP.PType IN ('Re-engagement', 'Line of Business Transfer') 
														AND  SF_Ed_Acc.r_Client_Code__c		=		LEFT(RAP.JobNumber, LEN(RAP.JobNumber) - 4)
														AND  ISNULL(RAP.ProgramCost, 0) < 0		)									THEN       'Discount'						
							 
							 WHEN		RAP.PType LIKE 'Line of Business Transfer'													THEN		'Re-engagement'
							 
							 WHEN       (RAP.PType LIKE 'Modification' AND  SF_Ed_Acc.r_Client_Code__c	!=	LEFT(RAP.JobNumber, LEN(RAP.JobNumber) - 4)    )	THEN	'Re-engagement'

							 ELSE		RAP.PType	END
			 END		AS		RevAccrual_Type,

		---- Create Opp Type
          CASE WHEN ( RAP.NBB = 1 OR RAP.SystemCrossSellType = 'UG to ALR' ) AND RAP.OppType = 'Discount'	    	THEN 'New Sale-Discount'
               WHEN ( RAP.NBB = 1 OR RAP.SystemCrossSellType = 'UG to ALR' )						                THEN 'New Sale-NBB'
               WHEN RAP.OppType = 'New Sale' AND RAP.NBB = 0														THEN 'New Sale-Shadow'
               ELSE RAP.OppType			END			AS OppType,

		RAP.PackageID,
		RAP.ProgramStatus,
		RAP.FYVertEngagementStatus As Package_Engagement_Status,
		
		ISNULL(RAP.ProgramCost, 0) As ProgramCost,
		ISNULL(RAP.ContactQuantity, 0) As ContactQuantity,
		ISNULL(RAP.Extra1Cost, 0) As Extra1Cost,
		ISNULL(RAP.Extra2Cost, 0) As Extra2Cost,

		RAP.CurrentSL,
		
		RAP.DiscountReason,
		MA.AccountSegment,
		OptOuts_CY.OptOut_Ind

INTO	#SF_PP_Raw_Data
	   	  
FROM			DataAnalysis.dbo.RY_AllPrograms				    AS RAP

LEFT JOIN		#BOA											AS BOA				ON				Left(RAP.JobNumber, 3) = BOA.BoA_Client_Code

LEFT JOIN		#ALR_ProductGroup								AS ALR				ON				Right(RAP.JobNumber, 2) = ALR.ALR_CampaignCode

LEFT JOIN		#HD_ProductGroup								AS HD				ON				Right(RAP.JobNumber, 2) = HD.HDCampaignCode

LEFT JOIN		#Adv_ProductGroup								AS Adv				ON				Right(RAP.JobNumber, 2) = Adv.Adv_CampaignCode

---- client codes & Binder client codes
LEFT JOIN		DataMartEDSO.dbo.Account						AS SF_Ed_Acc		ON				RAP.AccountID = SF_Ed_Acc.ID

---- Account Segment
LEFT JOIN		EDSOAnalytics.dbo.MS_Accounts					AS MA			    ON				RAP.AccountID = MA.AccountSFID

---- Program Package Name
LEFT JOIN		DataMartEDSO.dbo.Program_Package__c				AS ProgPack			ON				RAP.PackageID = ProgPack.ID

-----Within Decision Type of "Decision", need to separate "Opt-Out" from "NNLOA"  - For Current Fiscal Year, connect to RAP at Campaign Level/ProgramID
LEFT JOIN		( SELECT	distinct RPC.Id,	 
							SUM(CASE WHEN RPC.Opt_Out__c = 1 THEN 1 ELSE 0 END)		AS			OptOut_Ind        
				  FROM		DataMartEDSO.dbo.Royall_program__c		RPC
				  WHERE		Fiscal_Year__c >= @Control_Year-1
				  GROUP BY	RPC.Id )		AS	OptOuts_CY							ON			RAP.ProgramID = OptOuts_CY.Id

WHERE			---- EXCLUDE Line of Business Transfer for HD Group
				RAP.Audience	IN		('UG Group', 'HD Group', 'ALR Group',  'Agency',  'Adv Group')
				AND	(RAP.Audience  +   RAP.PType)      NOT IN		('HD GroupLine of Business Transfer')
				
				---- EXCLUDE 'Discount - Program / Inquiries' entries that are $0
				AND RAP.Campaign NOT IN ('Discount - Program / Inquiries')
				
				---- EXCLUDE dummy NBB record in renewal pool
				AND RAP.JobNumber IS NOT NULL   
						--OR (RAP.JobNumber IS NULL  AND   RAP.NBB = 0)  ) --this is to Include CPQ units
				
				---- FILTER for those with signature date before Look_Back_Date
				AND (RAP.SignatureDate <= @LookBackDate  OR  RAP.SignatureDate IS NULL)
				
				---- FILTER for campaigns in current year OR contracted in Prior years
				AND  (		(RAP.FiscalYear	LIKE @Control_Year-1   AND	RAP.ProgramStatus	LIKE	'Contracted')	
						OR  (RAP.FiscalYear LIKE @Control_Year	   AND	RAP.ProgramStatus	IN 		('Contracted', 'Declined', 'Pending - Opt Out' )	)
						OR  (RAP.FiscalYear LIKE @Control_Year	   AND	RAP.ProgramStatus	LIKE 'Pending Signature'   AND   FYVertAccountStatus  LIKE 'Existing Client'))

ORDER BY		FiscalYear, DataSource, AccountID, Audience, Model_ProductGroup, Campaign, SignatureDate; 

--Select * FROM #SF_PP_Raw_Data;

-----ROUND 2---------------------------------------------------------------------------------------------------------------------------------------------------------
---- Summarize SF Flat file by RevAccrualType, skip PType, add Campaign_First_Sign, separate Initial renewal from Incr. renewal for OppType
---- Change HD & Adv campaigns to HD's Rev Accrual file's LoB naming convention - didn't do above because not all campaigns have matching codes

Select		A.FiscalYear,
			A.DataSource,
			A.AccountStatus,
			A.AccountID,
			A.Account,
			A.AccountParent, 
			A.ClientCode,
			A.BinderClientCode,
			A.Campaign,
            A.CampaignCode,		
			A.Model_ProductGroup, 
			A.Audience,
			
			---- JobNumber is not always correct
			(CASE WHEN A.ClientCode IS NULL		THEN	A.BinderClientCode +  right(A.FiscalYear,2)  +  right(JobNumber, 2)
				  WHEN A.Campaign   LIKE 'Discount%'   then   left(JobNumber, len(JobNumber) - 2) +   '1199'
				  WHEN len(A.JobNumber) < 5		THEN	A.ClientCode + right(A.FiscalYear,2)  +  right(JobNumber, 2)
				  ELSE    A.JobNumber			END)				As			JobNumber,
			
			A.ProgramPackageName,
			A.PackageID,
			A.Package_Engagement_Status,

			A.SignatureDate,
			A.Renewal_Package_FirstSigned,
			A.Renewal_Campaign_FirstSigned,
			A.RevAccrual_Type,
			A.NBB,

			---- Pull in campaign first signed date
			Cast (CampaignFirstSign.FirstSigned	 as  datetime)  AS   Campaign_First_Sign, 

			---- Separate Initial Renewal from Incr. Renewal using first signature date in a FY as indication of 'Initial Renewal' -----------------        
			(CASE WHEN		A.OppType = 'Renewal'		THEN	
							(CASE WHEN	 CampaignFirstSign.FirstSigned = A.SignatureDate	 THEN		'Initial Renewal'      
								  WHEN	 (A.SignatureDate  IS NULL   AND   CampaignFirstSign.FirstSigned  IS NULL)			THEN   'Initial Renewal'        
								  ELSE		'Incr. Renewal'		END  )
				  ELSE		A.OppType		END)			AS		OppType,

			A.ProgramStatus,
			
			
			SUM(A.ProgramCost)					As		ProgramCost,
			SUM(A.ContactQuantity)				As		ContactQuantity,
			SUM(A.Extra1Cost)					As		Extra1Cost,
			SUM(A.Extra2Cost)					As		Extra2Cost,
			
			A.CurrentSL,

			---- Adjust RevAccrualType based on value sums
			(CASE WHEN		RevAccrual_Type IN ('Modification') 	  AND	 ISNULL(sum(A.ProgramCost), 0) < 0	 THEN		'Discount Modification'
				  WHEN      RevAccrual_Type IN ('Re-engagement')	  AND	 ISNULL(sum(A.ProgramCost), 0) < 0	 THEN		'Discount'
							ELSE		RevAccrual_Type		END)		AS		RevAccrualType,
			
			A.AccountSegment,

			---- Renewal Type: Opt Out vs NNLOA vs Secured for Current FY
				-- For Adv, HD
			(CASE WHEN	A.Audience IN  ('Adv Group', 'HD Group')   
				  THEN	(CASE WHEN	ROP_CY.FirstSigned = ROP_PY.FirstSigned		THEN	'Secured' 
							  WHEN	SUM(A.OptOut_Ind) > 0  THEN 'Opt Out'
							  ELSE	'Decision'		END ) 
				-- For UG/ALR
				  ELSE	(CASE WHEN	REP_CY.RenewalType = 'Fixed Term'			THEN	'Secured'
							  WHEN	REP_CY.RenewalType = 'Decision'		AND		SUM(A.OptOut_Ind) > 0 THEN 'Opt Out' 
							  ELSE	REP_CY.RenewalType END)	
				  END)  AS   Client_Current_RenewalType,

			----- Current Year Client's Tenure Index
			CASE WHEN	A.Audience IN  ('Adv Group', 'HD Group')		THEN		ROP_CY.Tenure
				ELSE   REP_CY.Tenure			END			As			Client_Current_Tenure_Index,
		
			---- Current Year Client's Tenure Category
			CASE WHEN	A.Audience IN  ('Adv Group', 'HD Group')
				 THEN	(CASE WHEN  ROP_CY.Tenure = 0	THEN  	'1st Year'		WHEN   ROP_CY.Tenure = 1		THEN	'2nd Year'	ELSE 'Tenured' END)			
				 ELSE	(CASE WHEN	REP_CY.Tenure = 0	THEN	'1st Year'		WHEN   REP_CY.Tenure = 1		THEN	'2nd Year'	ELSE 'Tenured' END)						 
				 END		  		AS		   Client_Current_Tenure,
		
			---- Next Year Client's Tenure Index
			CASE WHEN	A.Audience IN  ('Adv Group', 'HD Group')	 THEN	ROP_NY.Tenure	
				 ELSE	REP_NY.Tenure		END					As			Client_NY_Tenure_Index,
		
			---- Next Year Client's Tenure Category
			CASE WHEN	A.Audience IN  ('Adv Group', 'HD Group')
				 THEN	(CASE WHEN  ROP_NY.Tenure = 0	THEN  	'1st Year'		WHEN   ROP_NY.Tenure = 1		THEN  '2nd Year'	ELSE 'Tenured' END)			
				 ELSE	(CASE WHEN	REP_NY.Tenure = 0	THEN	'1st Year'		WHEN   REP_NY.Tenure = 1		THEN  '2nd Year'	ELSE 'Tenured' END)						 
				 END		  		AS		   Client_NY_Tenure

INTO		#SF_PP_Data

FROM		#SF_PP_Raw_Data	as   A

-----Renewal Type and Tenure by Account ID for UG+ALR	 --->	 REP_CY for current fiscal year -------
LEFT JOIN		DataAnalysis.dbo.RYLL_ENRPackages		AS REP_CY		    ON				REP_CY.AccountID		= A.AccountID
																							AND REP_CY.FiscalYear	= A.FiscalYear
																							AND REP_CY.Audience		= A.Audience		
-----Renewal Type and Tenure by Account ID for UG+/ALR	 --->	 REP_NY for next fiscal year -------
LEFT JOIN		DataAnalysis.dbo.RYLL_ENRPackages		AS REP_NY			ON				REP_NY.AccountID		= A.AccountID
																							AND REP_NY.FiscalYear	= A.FiscalYear + 1
																							AND REP_NY.Audience		= A.Audience


-----Renewal Type and Tenure by Account ID for Adv+HD    --->	 ROP_PY for prior fiscal year  -------
LEFT JOIN		DataAnalysis.dbo.RYLL_OtherPackages		AS ROP_PY            ON				ROP_PY.AccountID		= A.AccountID
																							AND ROP_PY.FiscalYear	= A.FiscalYear - 1
																							AND ROP_PY.Audience		= A.Audience

-----Renewal Type and Tenure by Account ID for Adv+HD    --->	 ROP_CY for current fiscal year  -------
LEFT JOIN		DataAnalysis.dbo.RYLL_OtherPackages		AS ROP_CY            ON				ROP_CY.AccountID		= A.AccountID
																							AND ROP_CY.FiscalYear	= A.FiscalYear
																							AND ROP_CY.Audience		= A.Audience 

-----Renewal Type and Tenure by Account ID for Adv+HD    --->	 ROP_CY for NEXT fiscal year  -------    
LEFT JOIN		DataAnalysis.dbo.RYLL_OtherPackages		AS ROP_NY            ON				ROP_NY.AccountID		= A.AccountID
																							AND ROP_NY.FiscalYear	= A.FiscalYear + 1
																							AND ROP_NY.Audience		= A.Audience 

LEFT JOIN		-- CampaignFirstSign:			Look for first signature date of each campaign&client/product
			(  SELECT		EB_SF.FiscalYear,   EB_SF.AccountID,         EB_SF.CampaignCode,		EB_SF.Audience,      MIN( EB_SF.SignatureDate ) AS FirstSigned
			   FROM			#SF_PP_Raw_Data			AS			EB_SF
			   WHERE		FiscalYear  =   @Control_Year
			   GROUP BY		EB_SF.FiscalYear,    EB_SF.AccountID,		EB_SF.Audience,	 		EB_SF.CampaignCode)	     AS CampaignFirstSign

			   ON		    A.AccountID		=	CampaignFirstSign.AccountID
						AND A.CampaignCode	=	CampaignFirstSign.CampaignCode
						AND A.Audience		=	CampaignFirstSign.Audience
						--AND A.SignatureDate	=	CampaignFirstSign.FirstSigned

GROUP BY	  A.FiscalYear							, A.DataSource
			, A.AccountStatus						, A.AccountID			
			, A.Account								, A.AccountParent
			, A.ClientCode							, A.BinderClientCode
			, A.Campaign							, A.CampaignCode
			, A.DB_ProductGroup						, A.Model_ProductGroup
			, A.Audience							, A.JobNumber		
			, A.ProgramPackageName					, A.PackageID			
			, A.Package_Engagement_Status			, A.SignatureDate		
			, A.Renewal_Package_FirstSigned			, A.Renewal_Campaign_FirstSigned 		
			, A.RevAccrual_Type						, A.NBB					
			, CampaignFirstSign.FirstSigned			, A.OppType				
			, A.ProgramStatus						, A.CurrentSL			
			, A.AccountSegment						, ROP_CY.FirstSigned	
			, ROP_PY.FirstSigned					, REP_CY.RenewalType	
			, REP_CY.Tenure							, ROP_CY.Tenure
			, REP_NY.Tenure							, ROP_NY.Tenure;

--Select * from #SF_PP_Data;

-------ROUND 3-------------------------------------------------------------------------------------------------------------------------------------------------------
-------If there are only Up-sell Discount lines for a Campaign & their sum is negative --> change RevAccrualType back to Up-sell
-------Turn Discount Allocation into a separate Model_ProductGroup

SELECT		FiscalYear,				
			DataSource,					
			AccountStatus,			
			AccountID,				
			Account, 				
			AccountParent, 			
			BinderClientCode,		
			ClientCode,				
			Campaign,				
			CampaignCode,  					
			(CASE When RevAccrualType LIKE 'Discount Allocation' Then 'Discount Allocation' Else  Model_ProductGroup End)	As	Model_ProductGroup,			
			Audience,					
			JobNumber,
			ProgramPackageName,			
			Package_Engagement_Status,
			SignatureDate, 				
			Renewal_Package_FirstSigned,			
			Renewal_Campaign_FirstSigned,
			Dummy.RevAccrualType_Updated		As		RevAccrualType,
			OppType,				
			ProgramStatus,			
			ProgramCost,			
			ContactQuantity,			
			Extra1Cost,				
			Extra2Cost,
			CurrentSL,
			AccountSegment,			
			Client_Current_RenewalType,			
			Client_Current_Tenure_Index,			
			Client_Current_Tenure,
			Client_NY_Tenure_Index,				
			Client_NY_Tenure,
			Campaign_First_Sign,

			-------- Summarize amount for buckets in Accounting Pool --------------------------	
			(Case When ProgramStatus LIKE 'Declined'							Then	'Declined'
				   When OppType	  IN   ('New Sale-NBB', 'New Sale-Discount')	Then    'New Sale'
				   When OppType   LIKE	'New Sale-Shadow'		Then	(Case when  RevAccrualType   Like  'Up-Sell%'    Then  'Up-sell on New Sale'
																			  when  RevAccrualType   Like  'Cross-Sell%' Then  'Cross-Sell on New Sale'
																			  else  'Modification on New Sale'			 End)
				   
				   When RevAccrualType	IN	('Up-sell', 'Up-sell Discount', 'Final Invoice adjustment')		Then   'Up-sell'								
				   When OppType			IN  ('Cross-Sell', 'Cross-Sell Discount')							Then   'Cross-Sell'
				   When RevAccrualType	LIKE 'Modification%'												Then   'Modification'
				   Else						'Re-engagement'       END)			AS	 SecuredCV_Classification

INTO		#SF_PP_Final_Cut

FROM 

	(Select #SF_PP_Data.*, 
			(CASE WHEN	(#SF_PP_Data.RevAccrualType LIKE 'Up-sell Discount'		AND		RevTypeAlign.USell_Count < 1    AND	RevTypeAlign.USell_Sum <= 0)	THEN	'Up-sell'
				  ELSE   #SF_PP_Data.RevAccrualType		END)		AS		RevAccrualType_Updated
				  	
	FROM  #SF_PP_Data
	LEFT JOIN   (SELECT		EB_SF.FiscalYear,		EB_SF.AccountID,	 EB_SF.CampaignCode,	EB_SF.Audience,		EB_SF.ProgramStatus
							, SUM(Case when EB_SF.RevAccrualType LIKE 'Up-sell' then EB_SF.ProgramCost else 0 end)			AS	USell_Sum
							, SUM(Case when EB_SF.RevAccrualType LIKE 'Up-sell' then 1 else 0 end)							AS	USell_Count
				 FROM			#SF_PP_Data		AS		EB_SF 
				 GROUP BY		EB_SF.FiscalYear,   EB_SF.AccountID,	  EB_SF. CampaignCode,		EB_SF.Audience,   EB_SF.ProgramStatus)		AS		RevTypeAlign

	ON			      #SF_PP_Data.AccountID		= RevTypeAlign.AccountID
				AND   #SF_PP_Data.CampaignCode	= RevTypeAlign.CampaignCode
				AND	  #SF_PP_Data.Audience		= RevTypeAlign.Audience
				AND   #SF_PP_Data.FiscalYear	= RevTypeAlign.FiscalYear
				AND   #SF_PP_Data.ProgramStatus	= RevTypeAlign.ProgramStatus				)		As		Dummy

--select  *	from  #SF_PP_Final_Cut;
 
--------ROUND 4----------------------------------------------------------------------------------------------------------------------------------------------
------- Pick out the most appropriate line in FY24 to match FY23 CV lines to ----------------------------

SELECT		EB_SF.*,
			
			FY23Align.Re_engagement_Count,
			FY23Align.Contracted_Re_engage_Count,
			FY23Align.Cross_Sell_Count,
			FY23Align.Modification_Count,
			FY23Align.DA_Count,
			FY23Align.Contracted_Discount_Count,
			FY23Align.Pending_Discount_Count,
			FY23Align.All_Discount_Count,
			FY23Align.First_Re_engagement,
			FY23Align.First_Cross_Sell,			
			FY23Align.First_Modification,
			FY23Align.First_DA,
			FY23Align.First_Discount,  
						
	      ------------- Sum CV_23 into the 1st line of Re-engagement or 1st line of Cross-sell or 1st line of Modification if prev. ones are not available
          CASE	WHEN	EB_SF.Campaign LIKE '%Discount%'	AND		EB_SF.RevAccrualType = 'Discount Allocation'	
							THEN	(CASE WHEN   EB_SF.ProgramStatus  LIKE 'Declined'		THEN   0
									  ELSE   ISNULL( Contracted_FY23_Pool.CV_23, 0) / 	NULLIF(FY23Align.DA_Count,0)	 END)
		  
				WHEN	EB_SF.Campaign LIKE '%Discount%'	AND		EB_SF.RevAccrualType     IN		('Discount' , 'Discount Modification', 'Cross-Sell Discount')
						THEN     (CASE WHEN  FY23Align.Contracted_Discount_Count  >  0		AND    EB_SF.ProgramStatus LIKE 'Contracted'
																				THEN  ISNULL( Contracted_FY23_Pool.CV_23, 0) / NULLIF(FY23Align.Contracted_Discount_Count,0)
									   WHEN  FY23Align.Pending_Discount_Count     >  0		AND    EB_SF.ProgramStatus LIKE 'Pending%'     AND    FY23Align.Contracted_Discount_Count = 0
																				THEN  ISNULL( Contracted_FY23_Pool.CV_23, 0) / NULLIF(FY23Align.Pending_Discount_Count,0)
									   WHEN  FY23Align.All_Discount_Count		  >  0      AND    EB_SF.ProgramStatus	NOT IN ('Contracted', 'Pending%')
									         AND    FY23Align.Contracted_Discount_Count = 0    AND   FY23Align.Pending_Discount_Count = 0
																				THEN  ISNULL( Contracted_FY23_Pool.CV_23, 0) / NULLIF(FY23Align.All_Discount_Count,0)
									   ELSE  0				END)

				ELSE	   (CASE WHEN  EB_SF.RevAccrualType LIKE 'Re-engagement'  
										THEN	(CASE	WHEN  FY23Align.Re_engagement_Count  =  1	THEN     ISNULL( Contracted_FY23_Pool.CV_23, 0)		
														ELSE  (CASE  WHEN	FY23Align.First_Re_engagement IS NULL	THEN	ISNULL( Contracted_FY23_Pool.CV_23, 0) /	NULLIF(FY23Align.Re_engagement_Count, 0)
																		WHEN	EB_SF.SignatureDate = FY23Align.First_Re_engagement  THEN	ISNULL( Contracted_FY23_Pool.CV_23, 0) / FY23Align.Contracted_Re_engage_Count	ELSE	0 END)   END)
										 
									WHEN  EB_SF.RevAccrualType LIKE 'Cross-sell'	
										THEN    (CASE WHEN	  FY23Align.Re_engagement_Count  >  0  THEN     0
												        ELSE    (CASE WHEN 	FY23Align.Cross_Sell_Count  =  1	   THEN  ISNULL( Contracted_FY23_Pool.CV_23, 0)	
																	ELSE	(CASE WHEN   EB_SF.SignatureDate = FY23Align.First_Cross_Sell  THEN	ISNULL( Contracted_FY23_Pool.CV_23, 0) ELSE 0 END)   END)	END)
										 
									WHEN  EB_SF.RevAccrualType IN ('Modification', 'Accounting Adjustment')	
										THEN    (CASE WHEN    FY23Align.Re_engagement_Count  +  FY23Align.Cross_Sell_Count >  0	THEN     0
														ELSE	(CASE WHEN 	FY23Align.Modification_Count  =  1	THEN     ISNULL(Contracted_FY23_Pool.CV_23, 0)	
														ELSE    (CASE WHEN	 EB_SF.SignatureDate = FY23Align.First_Modification  THEN	ISNULL(Contracted_FY23_Pool.CV_23, 0) ELSE 0 END)   END)	 END)			  

									WHEN  EB_SF.RevAccrualType LIKE 'Discount Allocation'	
										THEN    (CASE WHEN	  FY23Align.Re_engagement_Count +  FY23Align.Cross_Sell_Count + FY23Align.Modification_Count >  0  THEN     0
												        ELSE    (CASE WHEN 	FY23Align.DA_Count  =  1	THEN     ISNULL(Contracted_FY23_Pool.CV_23, 0)	
																ELSE		(CASE WHEN	 EB_SF.SignatureDate = FY23Align.First_DA  THEN	ISNULL( Contracted_FY23_Pool.CV_23, 0) ELSE 0 END)   END)	END)
									ELSE  0  END)			
				END			 AS			CV_23,
		   
		    0 As ContactQuant23  		   
 
INTO		#PP_FY24Contracted

FROM		#SF_PP_Final_Cut  AS	EB_SF

-------------To determine which line at Client & Campaign & Audience in FY24 to deposit CV23 in  
LEFT JOIN	(SELECT		EB_SF.FiscalYear,			EB_SF.AccountID,					EB_SF.CampaignCode,				
						EB_SF.Audience,				EB_SF.Model_ProductGroup,			EB_SF.JobNumber, 
						--EB_SF.ProgramStatus,		---EB_SF.FYVertEngagementStatus,		EB_SF.RevAccrualType,	
						SUM(Case when EB_SF.RevAccrualType LIKE 'Re-engagement' then 1 else 0 end)				AS	Re_engagement_Count,

						SUM(Case when EB_SF.RevAccrualType LIKE 'Re-engagement'		
										AND		EB_SF.ProgramStatus LIKE 'Contracted'
										AND		EB_SF.SignatureDate = EB_SF.Campaign_First_Sign		then 1  else 0 END)		AS	Contracted_Re_engage_Count,  

						SUM(Case when EB_SF.RevAccrualType LIKE		'Cross-sell' then 1 else 0 end)					AS	Cross_Sell_Count,
						SUM(Case when EB_SF.RevAccrualType IN		('Modification') then 1 else 0 end)				AS	Modification_Count,
							
						SUM(Case when EB_SF.RevAccrualType LIKE		'Discount Allocation' AND  EB_SF.ProgramStatus NOT LIKE 'Declined'  then 1 else 0 end)		AS	 DA_Count,
							
						SUM(Case when EB_SF.RevAccrualType IN	('Discount', 'Discount Modification', 'Cross-Sell Discount')	AND  EB_SF.ProgramStatus LIKE 'Contracted'
														   then		1 else 0  end)		AS		Contracted_Discount_Count,

						SUM(Case when EB_SF.RevAccrualType IN	('Discount', 'Discount Modification', 'Cross-Sell Discount')	AND  EB_SF.ProgramStatus LIKE 'Pending%'
														   then		1 else 0  end)		AS		Pending_Discount_Count,
						
						SUM(Case when EB_SF.RevAccrualType IN	('Discount' , 'Discount Modification', 'Cross-Sell Discount')	AND  EB_SF.ProgramStatus NOT IN ('Contracted', 'Pending%')   
														   then		1 else 0 end)		AS		All_Discount_Count,

						--SUM(Case when EB_SF.RevAccrualType LIKE 'Cross-sell Discount' then 1 else 0 end)		AS	CSell_Discount_Count,
						--SUM(Case when EB_SF.RevAccrualType LIKE 'Up-sell Discount' then 1 else 0 end)			AS	PSell_Discount_Count,
						MIN(Case when EB_SF.RevAccrualType LIKE  'Re-engagement'		then	EB_SF.SignatureDate		else NULL end)		As  First_Re_engagement,
						MIN(Case when EB_SF.RevAccrualType LIKE  'Cross-sell'			then	EB_SF.SignatureDate		else NULL end)		As  First_Cross_Sell,			
						MIN(Case when EB_SF.RevAccrualType LIKE	 'Modification'			then	EB_SF.SignatureDate		else Null end)		As  First_Modification,
						MIN(Case when EB_SF.RevAccrualType LIKE  'Discount Allocation'	then	EB_SF.SignatureDate		else NULL end)		As  First_DA,
						MIN(Case when EB_SF.RevAccrualType   IN	 ('Discount', 'Discount Modification', 'Cross-Sell Discount')		then	EB_SF.SignatureDate		else NULL end)		As  First_Discount
							
			FROM		#SF_PP_Final_Cut		AS			EB_SF
			WHERE		EB_SF.FiscalYear = @Control_Year			
			GROUP BY	EB_SF.FiscalYear,   EB_SF.AccountID,	  EB_SF.CampaignCode,		EB_SF.Audience,			--EB_SF.ProgramStatus,		
						EB_SF.Model_ProductGroup,   EB_SF.JobNumber 							)		AS		FY23Align

ON			      EB_SF.AccountID			=	FY23Align.AccountID
			AND	  EB_SF.Audience			=	FY23Align.Audience
			AND   EB_SF.Model_ProductGroup	=	FY23Align.Model_ProductGroup
		  --AND	  EB_SF.ProgramStatus		=	FY23Align.ProgramStatus
			AND   EB_SF.ClientCode			=	LEFT(FY23Align.JobNumber, len(FY23Align.JobNumber) -4)		-- client code
			AND   EB_SF.CampaignCode		=	RIGHT(FY23Align.JobNumber, 2)								-- campaign code
                   
LEFT JOIN	----  Contracted_FY23_Pool		---- need to check if first signature col is unique for each client&campaign&audience
            (  SELECT    distinct EB_SF.AccountID,  
						 EB_SF.ClientCode,
						 EB_SF.CampaignCode		As		CampaignCode,
						 EB_SF.JobNumber,
						 LEFT(EB_SF.JobNumber, len(EB_SF.JobNumber) - 4)  AS	JobNumber_ClientCode,
						 EB_SF.Audience, 
						 EB_SF.Model_ProductGroup,
                         SUM( ISNULL( EB_SF.ProgramCost, 0) )		AS		CV_23, 
                         SUM( ISNULL( EB_SF.ContactQuantity, 0) )	AS		ContactQuant23
               FROM      #SF_PP_Final_Cut	AS		EB_SF
               WHERE		      EB_SF.ProgramStatus = 'Contracted'
                                  AND EB_SF.FiscalYear = @Control_Year - 1
								  
               GROUP BY  EB_SF.AccountID,	EB_SF.ClientCode,		EB_SF.Audience,		EB_SF.CampaignCode,	
						 EB_SF.JobNumber,	EB_SF.Model_ProductGroup)	AS	Contracted_FY23_Pool

ON					EB_SF.AccountID									= Contracted_FY23_Pool.AccountID
			AND		EB_SF.Audience									= Contracted_FY23_Pool.Audience
			AND     EB_SF.Model_ProductGroup						= Contracted_FY23_Pool.Model_ProductGroup
			AND     EB_SF.ClientCode								= Contracted_FY23_Pool.ClientCode
			AND     EB_SF.CampaignCode								= Contracted_FY23_Pool.CampaignCode
			AND		LEFT(EB_SF.JobNumber, len(EB_SF.JobNumber) - 4)	= Contracted_FY23_Pool.JobNumber_ClientCode

WHERE		EB_SF.FiscalYear = @Control_Year;
			
--Select * from #PP_FY24Contracted;


-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-----Build #EB_FY24Pool from FY23 program (Client & Audience & CampaignID) that don't have FY24 Contracted/Pending Signature/Pending_Opt_out corresponding product yet

SELECT			FY23Pool.*,	
				FY23Pool.Account + FY23Pool.Audience	  AS	FY23Contracted_Account_Audience,		
				
				RAPackage_24.PackageId		As		FY24_PackageId,	

				(CASE WHEN FY23Pool.Audience IN ('Adv Group') 	THEN    ROP_24.Adj_Renewal_Status
					  ELSE ISNULL(REP_24.Renewal_Status, 'blank')			END	)		AS		FY24_Package_Engagement_Status,
				
				EB_FY24Contracted.Account +  EB_FY24Contracted.Audience + EB_FY24Contracted.Model_ProductGroup  AS	FY24Contracted_Account_Audience_ProductGroup,		
				
				EB_FY24Contracted.DiscountFY24_Count, 
				
				EB_FY24_distinct_unit.Account + EB_FY24_distinct_unit.Audience +  EB_FY24_distinct_unit.Model_ProductGroup + EB_FY24_distinct_unit.CampaignCode	
																				AS						FY24Contracted_Account_Audience_ProductGroup_CampaignCode,

				EB_FY24_distinct_unit.AccountID       As     FY24_distinct_AccountID,

				SUM (Case When FY23Pool.Account + FY23Pool.Audience + FY23Pool.CampaignCode   NOT LIKE   '%1199%'		
								 Then  1   Else	0	End )  OVER (PARTITION BY FY23Pool.Account + FY23Pool.Audience)		AS		FY23_Non_Discount_Count
			
INTO #PP_FY23Pool

FROM			(SELECT	DISTINCT			EB_SF_FY23.FiscalYear,
											EB_SF_FY23.DataSource,
											EB_SF_FY23.AccountID,
											EB_SF_FY23.Account,
											EB_SF_FY23.AccountParent,
											EB_SF_FY23.ClientCode,
											EB_SF_FY23.BinderClientCode,
											EB_SF_FY23.Last_yr_Campaign,
											EB_SF_FY23.CampaignCode,
											EB_SF_FY23.Model_ProductGroup,
											EB_SF_FY23.Audience,
											--EB_SF_FY23.Renewal_Package_FirstSigned,
											--EB_SF_FY23.Renewal_Campaign_FirstSigned,
											SUM(EB_SF_FY23.ProgramCost)			As		CV_23,
											SUM(EB_SF_FY23.ContactQuantity)		As		ContactQuant23,
											EB_SF_FY23.CurrentSL,
											EB_SF_FY23.Package_Engagement_Status	AS		FY23_PackageStatus,
											EB_SF_FY23.AccountSegment,
											EB_SF_FY23.Upcoming_Tenure_Index,
											EB_SF_FY23.Upcoming_Tenure										
																	
					FROM		(----------------------------------------Condense all Discount lines into 1------------------------------ 
								Select FiscalYear, DataSource, AccountID, Account, AccountParent, ClientCode, BinderClientCode, 
										CASE WHEN CampaignCode like '1199' Then 'Discount' Else Campaign End		AS		Last_yr_Campaign,
										CampaignCode,		Model_ProductGroup,			Audience,		
										ProgramCost,		ContactQuantity,			ProgramStatus,		
										CurrentSL,			Package_Engagement_Status,				AccountSegment,		
										Client_NY_Tenure_Index		AS      Upcoming_Tenure_Index,
										Client_NY_Tenure			AS		Upcoming_Tenure
		
								 FROM	#SF_PP_Final_Cut
								 WHERE	FiscalYear = @Control_Year - 1		AND		ProgramStatus = 'Contracted'	)		As		EB_SF_FY23 
					
					GROUP BY				FiscalYear,			DataSource,				AccountID,				Account,			AccountParent,
											ClientCode,			BinderClientCode,		Last_yr_Campaign,		CampaignCode,		Model_ProductGroup,
											Audience,			CurrentSL,				Package_Engagement_Status,
											AccountSegment,		Upcoming_Tenure_Index,	Upcoming_Tenure			)			As			FY23Pool

--- FY24 PackageID is from RYLL_AllPackages database, mapped to RAP Table by AccountID, FY, Audience
			LEFT JOIN		DataAnalysis.dbo.RYLL_AllPackages			AS	RAPackage_24		ON	FY23Pool.AccountID		= RAPackage_24.AccountID																							
																								AND FY23Pool.Audience		= RAPackage_24.Audience
																								AND RAPackage_24.Rk			= 1
																								AND RAPackage_24.FiscalYear	= @Control_Year
--- Pull in status of FY24 Package using ROP for HD/Adv & REP for UG/ALR
			LEFT JOIN		DataAnalysis.dbo.RYLL_OtherPackages			AS  ROP_24				ON	RAPackage_24.PackageId	= ROP_24.PackageID
																								AND ROP_24.FiscalYear		= @Control_Year

			LEFT JOIN		DataAnalysis.dbo.RYLL_ENRPackages			AS  REP_24				ON	FY23Pool.AccountId		= REP_24.AccountID
																								AND FY23Pool.Audience		= REP_24.Audience
																								AND REP_24.FiscalYear		= @Control_Year
																			
			LEFT JOIN		(SELECT FiscalYear, DataSource, AccountID, Account, Model_ProductGroup,	Audience, 
									SUM (	Case When  Campaign LIKE '%Discount%' Then 1 Else 0	End   )		As		DiscountFY24_Count
							FROM		#PP_FY24Contracted
							GROUP BY	FiscalYear, DataSource, AccountID, Account, Model_ProductGroup, Audience)		AS		EB_FY24Contracted		
																					
							ON				FY23Pool.AccountID				=  EB_FY24Contracted.AccountID
									AND		FY23Pool.Audience				=  EB_FY24Contracted.Audience
									AND		FY23Pool.Model_ProductGroup		=  EB_FY24Contracted.Model_ProductGroup

			LEFT JOIN		(SELECT DISTINCT FiscalYear, DataSource, AccountID, Account, Model_ProductGroup,  Audience, CampaignCode	
							 FROM	#PP_FY24Contracted  )		As		EB_FY24_distinct_unit	--- match against FY24 Contracted opportunity Client & Campaign & Audience	
							 ON				FY23Pool.AccountID				=  EB_FY24_distinct_unit.AccountID
									AND		FY23Pool.Audience				=  EB_FY24_distinct_unit.Audience
									AND		FY23Pool.CampaignCode			=  EB_FY24_distinct_unit.CampaignCode
									AND     FY23Pool.Model_ProductGroup		=  EB_FY24_distinct_unit.Model_ProductGroup
														
			WHERE			EB_FY24_distinct_unit.AccountID		IS NULL;			

--SELECT * FROM #PP_FY23Pool ;

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------- Join FY23PP_Pool with PP_FY24Contracted-----------------------------------------------------------------

SELECT		*    INTO		#PP_FY24_RevExtract
FROM		(  
Select				CASE WHEN FY23PP_Pool.FY24_Package_Engagement_Status LIKE 'Lost' THEN    'FY24Declined_PP'   ELSE  'NoFY24PP_FY23PP'	 END	AS	SourceTracker,
					'Existing Client'						AS	AccountStatus,
					@Control_Year							AS	FiscalYear,
					FY23PP_Pool.DataSource					AS	DataSource,
					'N/A - In PP prior yr'					AS  ProgramPackageName,
					(Case When FY23PP_Pool.FY24_Package_Engagement_Status  IS NULL Then 'blank' Else FY23PP_Pool.FY24_Package_Engagement_Status  End)   As  FY24_Package_Engagement_Status,			
					FY23PP_Pool.AccountID					AS	AccountID,
					FY23PP_Pool.Account						AS	Account,
					FY23PP_Pool.AccountParent				AS	AccountParent,					
					FY23PP_Pool.BinderClientCode			AS  BinderClientCode,
					FY23PP_Pool.ClientCode					AS	ClientCode,
					FY23PP_Pool.Last_yr_Campaign			AS	Campaign,
					FY23PP_Pool.CampaignCode				AS	CampaignCode,
					FY23PP_Pool.Model_ProductGroup			AS	Model_ProductGroup,
					FY23PP_Pool.Audience					AS	Audience,
					'N/A - In PP prior yr'					AS	JobNumber,			
					NULL									AS	SignatureDate,	
					'N/A - In PP prior yr'					AS	OppType,		
					(Case When	(FY23PP_Pool.Last_yr_Campaign LIKE '%Discount%'  OR  FY23PP_Pool.CV_23  <  0)  
						  Then  'Discount_FY23PP'  Else 'Re-engagement_from_FY23PP' End) 	As	RevAccrualType,					
					'N/A - In PP prior yr'					As	ProgramStatus,
					0										As	CV_24,
					0										As	ContactQuant24,
					0										As	Extra1Cost_24,				
					0										As	Extra2Cost_24,
					
					FY23PP_Pool.CurrentSL,
					FY23PP_Pool.AccountSegment,
					'Decision'								As Current_Renewal_Type,
					FY23PP_Pool.Upcoming_Tenure_Index		As Current_Tenure_Index ,
					(CASE When	FY23PP_Pool.Upcoming_Tenure_Index = 1		Then  '1st Year'
						  When	FY23PP_Pool.Upcoming_Tenure_Index = 2		Then  '2nd Year'
						  Else	'Tenured'	End	)			As Current_Tenure,
					
					FY23PP_Pool.CV_23,
					FY23PP_Pool.ContactQuant23,
			
					(CASE WHEN		FY24_Package_Engagement_Status  = 'Lost'	 		Then		'Lost Client'
						  WHEN		Model_ProductGroup	 = 'Discount Allocation'		Then		'Discount Drop'			  				  
						  
						  ---- if the only campaign from FY23 not showing up in FY24 Package is Discount --> don't pull Discount into FY24 as Pending Renewal
						  WHEN		FY23_Non_Discount_Count = 0		 					Then		'Discount Drop'
						 
						  ---- if campaign from FY23 belongs to an Audience that already has FY24 ProductGroup Secured/Contracted status  --> considered this Campaign Drop
						  WHEN		FY24Contracted_Account_Audience_ProductGroup  IS NOT NULL  Then		'Campaign Drop'
						  			  
						  ELSE		'Pending Renewal'									END		)	EB_RevType,
					
					(CASE WHEN FY23PP_Pool.FY24_Package_Engagement_Status LIKE 'Lost' THEN    'Declined'   ELSE  'Not Secured'	 END)	As	Secured_Classification				
						
FROM		#PP_FY23Pool	AS		FY23PP_Pool			
 
UNION ALL

Select			CASE WHEN ProgramStatus = 'Contracted'			THEN 	'FY24Contracted_PP'
					 WHEN ProgramStatus = 'Declined'			THEN 	'FY24Declined_PP'		
					 ELSE 'FY24Pending_PP'					END				AS		SourceTracker,
				AccountStatus,			FiscalYear,			DataSource,				
				ProgramPackageName,		Package_Engagement_Status,
				AccountID,				Account,			AccountParent,			BinderClientCode,
				ClientCode,				Campaign,			CampaignCode,			Model_ProductGroup,		
				Audience,				JobNumber,			SignatureDate,			OppType,		
				RevAccrualType,
				ProgramStatus,						 
				ProgramCost				AS		CV_24, 	 
				ContactQuantity			AS		ContactQuant24,	
				Extra1Cost				AS		Extra1Cost_24,
				Extra2Cost				AS		Extra2Cost_24,
				CurrentSL,
				AccountSegment,
				Client_Current_RenewalType		AS	 Current_Renewal_Type,
				Client_Current_Tenure_Index     AS   Current_Tenure_Index,
				Client_Current_Tenure			AS	 Current_Tenure,
				CV_23, 	 
				ContactQuant23,
				CASE WHEN		Package_Engagement_Status  = 'Lost'						Then		'Lost Client'
					 WHEN		ProgramStatus	= 'Declined'							Then		'Campaign Drop' 
					 
					 WHEN		ProgramStatus = 'Contracted'	THEN
										(Case When	 OppType  IN		('New Sale-Discount' , 'New Sale-NBB')		then		  'New Sale'
											  When	 OppType  IN		('Cross-sell')								then		  'Cross-sell'
											  When   OppType  NOT IN	('New Sale-Discount','New Sale-NBB','Cross-sell')	AND  RevAccrualType LIKE 'Up-sell%'		then	'Up-sell'
											  Else	 OppType		End)

					 WHEN		ProgramStatus IN ('Pending Signature', 'Pending - Opt Out')    Then
										(Case When   OppType  IN		('New Sale-Discount' , 'New Sale-NBB')		then		'New Sale - Pending'
					 						  When	 OppType  IN		('Cross-sell') 								then		'Cross-sell - Pending'
											  When   OppType  IN		('Incr. Renewal')							then		'Up-sell - Pending'
											  When   OppType  NOT IN	('New Sale-Discount' , 'New Sale-NBB','Cross-sell')	  AND  RevAccrualType LIKE 'Up-sell%'  then 'Up-sell - Pending'
											  Else   'Pending Renewal'	End)
					 
					 ELSE		OppType		END			As		EB_RevType,
				SecuredCV_Classification
	
				FROM		#PP_FY24Contracted		)		AS		PP_FY24_Wrap	

Select * from #PP_FY24_RevExtract;