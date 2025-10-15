# R/globals.R - Global variables and constants

SECTOR_COLORS <- list(
  "Payments" = "#FFD700",
  "Credit" = "#FFA07A",
  "Insurance" = "#98FB98",
  "Investments" = "#87CEFA",
  "Pensions" = "#DDA0DD",
  "Several/other" = "#FFC0CB",
  "Savings" = "#B0C4DE"
)

# Globals
MND_OBJ <- 
  list(
    "Central banking" = c("Currency management & cash handling"), 
    "Statistics & research" = c("Statistics & research"), 
    "Macroprudential supervision" = c("Stability"), 
    "Microprudential supervision" = c("Credit risk", "Market risk", "Operational risk", "Liquidity risk", "Solvency", "Soundness", "Reputational and legal risk", "AML/CFT"), 
    "Competition" = c("Competition"), 
    "Capital markets development" = c("Capital markets development"), 
    "Sustainability" = c("Climate and environmental objectives", "Gender equality"), 
    "Consumer protection" = c("Data privacy and protection", "Fair treatment", "Complaints handling", "Safety and security", "Suitability", "Impact", "Transparency"), 
    "Financial inclusion" = c("Access", "Uptake (account ownership)", "Usage", "Quality", "Outcomes"), 
    "Financial safety net" = c("Depositor protection")
  )

# NEED to figure out where to put central banking and statistics and research

MND_OBJ_2 <- 
  list(
    "Financial inclusion" = c("Access", "Uptake (account ownership)", "Usage", "Quality", "Outcomes"), 
    "Consumer protection" = c("Data privacy and protection", "Fair treatment", "Complaints handling", "Safety and security", "Suitability", "Impact", "Transparency", "Depositor protection"), 
    "Market development" = c("Capital markets development", "Competition"), 
    "Sustainability" = c("Climate and environmental objectives", "Gender equality"), 
    "Prudential supervision" = c("Stability", "Credit risk", "Market risk", "Operational risk", "Liquidity risk", "Solvency", "Soundness", "Reputational and legal risk", "AML/CFT"), 
    "Other" = c("Currency management & cash handling", "Statistics & research")
  )


COLUMN_DESCRIPTIONS <- c(
  "indicator_id"= "Unique indicator id",
  "indicator_order"= "Indicator order",
  "indicator_name" = "Indicator name",
  "indicator_description" = "Description", 
  "indicator_long_description" = "Detailed description", 
  "unit_of_analysis" = "Unit of analysis", 
  "measurement_type" = "Measurement type",       
  "gender_questions" = "Exploratory questions that can help guide analysis by gender",         
  "formula1" = "Formula 1",        
  "formula2" = "Formula 2",            
  "formula3" = "Formulas 3",          
  "main_mandate" = "Main mandate",           
  "secondary_mandates"  = "Other applicable mandates",      
  "main_objectives"  = "Main objectives",         
  "secondary_objectives"  = "Other applicable objectives",    
  "main_mandate_objective" = "Main mandate (main objective)", 
  "secondary_mandate_objective" = "Secondary mandate (secondary objectives)", 
  "main_sector" = "Main sector",               
  "secondary_sectors" = "Other applicable sectors",      
  "preset_foundation" = "Basic preset", 
  "IMF" = "IMF-FAS",                  
  "GPFI" = "GPFI",                  
  "AFI" = "AFI", 
  "WEF" = "WE Finance Code",                  
  "references" = "References",               
  "disaggregation_vars" = "Suggested breakdowns for segmentation analysis"
)

BREAKDOWNS <- list(
  "Customer gender" = c("Female", "Male", "Non-binary", "Transgender"), 
  "Customer type" = c("Natural person-individual", "Legal person-entity/organization"), 
  "Customer age" = c("< 30 years", "30-55 years", "55-65 years", "65+ years"), 
  "Customer location" = c("Urban", "Rural", "By region", "By district", "By administrative unit level (State/province; city/municipality)"), 
  "Customer income category" = c("By income brackets (eg. monthly income)", "By government categorization (eg. GP2 beneficiary)", "By account balance brackets"), 
  
  "MSME gender" = c("Female", "Male", "Non-binary", "Transgender"), 
  
  "Financial service provider (FSP) gender diversity" = c("Here the intention is to use indicators of FSP gender diversity, for example at Board-level, C-level (CEO, CFO, COO), , Middle manager (Director of compliance, Director of risk management, Director of marketing, Director of strategy, Director of HR, Other) to examine whether differences in this indicator across institutions is related to the gender diversity of the institution."), 
  "Financial service provider (FSP) type" = c("Commercial banks", "Credit unions and cooperatives", "Deposit taking MFI", "Other deposit taking institution (ODTI)", "Nonbank credit providers", "-Non-deposit taking MFI", "-Fintech credit providers", "Payment service providers (PSP)", "-E-money issuers (EMI)", "-Money transfer operatore (MTO)", "Mobile money providers", "Crowdfunding provider", "Payment initiation service provider (PISP)", "Account information service provider (AISP)", "Microinsureres-life", "Microinsurers-non life", "Insurers-life", "Insurers-non life", "Pension providers"), 
  "Financial service provider (FSP) main activity" = c("All FSPs giving credit", "All FSPs taking deposits", "All FSPs offering payment services", "..."), 
  
  "Product type" = c("PAYMENT INSTRUMENTS", "-Debit card", "-Credit card", "-Prepaid card", "-E-money (includes mobile money)", "-Cash", "-EFT (Electronic funds transfer)", "-Fast payments", "-Cheque", "CREDIT", "-Loans-all retail", "-Loans-consumer", "-Loans-housing", "-Loans-mortgage", "-Credit card", "-Revolving credit", "-P2P loans", "-Line of credit (except credit cards)", "DEPOSITS", "-Checking accounts", "--Basic/no-frills accounts", "-Savings accounts", "-Time/term deposit accounts", "INSURANCE", "-Life insuarance", "-Non-life insurance", "-Microinsurance-life", "-Microinsurance-non life", "-Climate disaster risk insurance", "PENSIONS", "-Micro pensions", "-Other pensions", "-Other long term investments for old age", "INVESTMENT", "-Equity", "-Bonds", "-Crowdfunding-debt (P2P lending)", "-Crowdfunding-equity"),  
  "Channel type" = c("PHYSICAL", "-Branch", "-ATM", "-POS", "-Agents (banking agents)", "-Agents (mobile money agent)", "-Merchants (physical merchants) (POS/QR)", "-Insurance agents/intermediaries/brokers", "-Pension agents/brokers", "-Investment broker/intermediary"), 
  "Transaction type" = c("DIGITAL/CASHLESS TRANSACTIONS", "-Payments and transfers", "-Deposits (digital)", "-Remittances (in/out flows)", "-EFT transactions", "-Fast payments", "-E-money", "-Mobile money", "-Cards", "CASH", "-Withdrawals/cash-out", "-Deposits (cash-in)", "-Remittances (in/out flows)", "CHEQUES", "-Cheque transactions", "NON-FINANCIAL", "-Balance enquiry", "-Payment initiation request", "-Data sharing consent"), 
  "Insurance type" = c("Life insuarance", "Non-life insurance", "Microinsurance-life", "Microinsurance-non life", "Climate disaster risk insurance"), 
  "Account type" = c("TRANSACTION ACCOUNT", "- Deposit account", "- Payment account", "-- E-money account", "--- Mobile money account", "-- Other payment accounts", "- Investment account", "NON-TRANSACTION ACCOUNT", "- Pension accounts", "- Time/term deposits"), 
  "Deposit account type" = c("Demand deposit accounts", "-Checking accounts", "--Basic/no-frills accounts", "Savings accounts", "Time/term deposit accounts"), 
  "Pension type" = c("Micro pensions", "Other pensions", "Other long term investments for old age", "Public", "Private", "Mandatory", "Voluntary"), 
    
  "Loan size" = c("Nano", "Micro-natural person", "Micro-legal person", "Small", "Medium"),  
  "Loan term" = c("A day or less (<= 1 day)", "A week or less (=<1w)", "30 days or less (=<1m)", "180 days or less (=<6m)", "A year or less (=<1y)"), 
  "Loan type" = c("Loans-all retail", "Loans-consumer", "Loans-housing", "Loans-mortgage", "Credit card", "Revolving credit", "P2P loans", "Line of credit (except credit cards)"), 
  
  "Interest rate category"= c("Variable", "Fixed", "Combination of fixed and variable"), 
  
  "Complaints issue category" = c("Fraudulent or unauthorized transactions", "Unfair treatment", "Unexpected fees", "Statement errors"), 
  "Complaints processing status" = c("Dismissed (rejected without being processed)", "Open", "Accepted - in process (no incidences)", "Accepted - escalated", "Resolved in favor of the customer", "Resolved against the customer"), 
  "Complaints filing channel" = c("Phone", "Branch location", "Agent location (where applicable)", "Email", "Social media", "Text message or chatbot"), 
  "Complaints resolution outcome" = c("Unfavorable to the customer - no financial losses", "Unfavorable to the customer - with financial losses", "Favorable to the customer - no financial losses", "Favorable to the customer - financial losses"), 
  "Complaints resolution time" = c("Less than 1 week", "2 weeks up to 1 month", "Between 1 and 3 months", "More than 3 months"), 
  
  "Collateral type" = c("INMOVABLE", "-Land", "-House", "- Other commercial property", "MOVABLE", "-Vehicles and equipment", "-Livestock", "-Inventory and Stock", "-Receivables and Contracts", "-Cash Flow-based Collateral", "INTANGIBLE/ALTERNATIVE", "-Reputation/Social Capital", "-Digital data")

)

