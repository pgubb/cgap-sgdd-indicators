# R/globals.R - Global variables and constants

SECTOR_COLORS <- list(
  "Payments" = "#FFD700",
  "Credit" = "#FFA07A",
  "Insurance" = "#98FB98",
  "Investments" = "#87CEFA",
  "Pensions" = "#DDA0DD",
  "Several/Others" = "#FFC0CB",
  "Savings" = "#B0C4DE"
)

USE_CASES <- c(
  "Access to financial services" = "Identify and assess disparities in terms of physical and digital access to financial services, in order to expand and improve points of service, including digital and physical infrastructure, and account ownership, ensuring that women and gender minorities have convenient and equitable access to financial service points, channels and owning an account, thus reducing barriers to entry into the financial system.", 
  "Usage of financial services" = "Assess the usage of financial sevices by women and gender minorities - identify patterns, gaps and disparities, to inform the creation of tailored financial products that cater to their specific needs and usage patterns, encouraging higher and more effective use of financial services.", 
  "Quality of financial services" = "Assess the quality of financial services provided to women and gender minorities, aiming to enhance service delivery through better product designs, improved financial literacy programs, and robust consumer protection.", 
  "Outcomes of the use of financial services" = "Improve the outcomes of the use of financial services by women and gender minorities, including on financial health,  by leveraging SGDD to monitor and adjust financial services to better meet the needs of the target groups, as well as to monitor the FSP conduct to ensure fair treatment of women and gender minorities.", 
  "Public dissemination of information and statistics" = "Improve public disssemination of information and statistics about gender patterns and gender criticality in the financial sector.", 
  "Management and mitigation of consumer risks and foster competition" = "Improve identification, assessment, management and mitigation of prudential and stability risks in the financial sector, by analyzing gender patterns and gender criticality in macro-prudential and micro-prudential regulation and supervision.", 
  "Management and mitigation of prudential and stability risks" = "Improve identification, assessment, management and mitigation of consumer risks and foster competition in the financial sector, by analyzing gender patterns and gender criticality in market conduct regulation and supervision focused on consumer protection and in the competitive dynamics of the financial sector that impacts consumer risks. The ultimate objective is to produce positive intermediate outcomes for consumers that contribute to their financial health.", 
  "Develop capital markets" = "Develop capital markets by increasing their breadth and depth, effiency, innovation and diversity, by analyzing the role played by gender in the investor and the issuance/debtor sides. The purpose is to suppport economic growth, job creation, and attract investments that could particularly support diverse MSMEs (those led by women and diverse gender).", 
  "Building a sustainable and equitable financial sector" = "Improve effectiveness of policies aimed at building a sustainable and equitable financial sector via inclusive sustainable/green finance, and greater gender diversity at FSPs and at financial authorities.", 
  "Increase efficiency and effectiveness of currency management" = "Increase efficiency and effectiveness of currency management at central banks, by considering the impact of gender patterns in the demand and use of central bank currency."
)

COLUMN_DESCRIPTIONS <- c(
  "indicator_id"= "Unique indicator id",
  "indicator_name" = "Indicator name",
  "indicator_description" = "Description", 
  "indicator_long_description" = "Long description", 
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
  "use_cases" = "Applicable use cases of indicator for FSAs",             
  "high_priority" = "High priority indicator for mandate", 
  "IMF" = "IMF-FAS",                  
  "GPFI" = "GPFI",                  
  "AFI" = "AFI", 
  "WEF" = "WE Finance Code",                  
  "references" = "References",               
  "essential_disagg" = "Breakdowns recommended by CGAP",      
  "nonessential_disagg" = "Other breakdowns"
)

BREAKDOWNS <- list(
  "Customer gender" = c("Female", "Male", "Non-binary", "Transgender"), 
  "Financial service provider (FSP) gender diversity" = c("Here the intention is to use indicators of FSP gender diversity, for example at Board-level, C-level (CEO, CFO, COO), , Middle manager (Director of compliance, Director of risk management, Director of marketing, Director of strategy, Director of HR, Other) to examine whether differences in this indicator across institutions is related to the gender diversity of the institution."), 
  "Type of customer" = c("Individual", "MSMEs", "Other"), 
  "Age of customer" = c("< 30 years", "30-55 years", "55-65 years", "65+ years"), 
  "Financial service provider (FSP) type" = c("Commercial banks", "Credit unions and cooperatives", "Deposit taking MFI", "Other deposit taking institution (ODTI)", "Nonbank credit providers", "-Non-deposit taking MFI", "-Fintech credit providers", "Payment service providers (PSP)", "-E-money issuers (EMI)", "-Money transfer operatore (MTO)", "Mobile money providers", "Crowdfunding provider", "Payment initiation service provider (PISP)", "Account information service provider (AISP)", "Microinsureres-life", "Microinsurers-non life", "Insurers-life", "Insurers-non life", "Pension providers"), 
  "Product type" = c("PAYMENT INSTRUMENTS", "-Debit card", "-Credit card", "-Prepaid card", "-E-money (includes mobile money)", "-Cash", "-EFT (Electronic funds transfer)", "-Fast payments", "-Cheque", "CREDIT", "-Loans-all retail", "-Loans-consumer", "-Loans-housing", "-Loans-mortgage", "-Credit card", "-Revolving credit", "-P2P loans", "-Line of credit (except credit cards)", "DEPOSITS", "-Checking accounts", "--Basic/no-frills accounts", "-Savings accounts", "-Time/term deposit accounts", "INSURANCE", "-Life insuarance", "-Non-life insurance", "-Microinsurance-life", "Microinsurance-non life", "Climate disaster risk insurance", "PENSIONS", "-Micro pensions", "-Other pensions", "-Other long term investments for old age", "INVESTMENT", "-Equity", "-Bonds", "-Crowdfunding-debt (P2P lending)", "-Crowdfunding-equity"),  
  "Channel type" = c("PHYSICAL", "-Branch", "-ATM", "-POS", "-Agents (banking agents)", "-Agents (mobile money agent)", "-Merchants (physical merchants) (POS/QR)", "-Insurance agents/intermediaries/brokers", "-Pension agents/brokers", "-Investment broker/intermediary"), 
  "Transaction type" = c("DIGITAL/CASHLESS TRANSACTIONS", "-Payments and transfers", "-Deposits (digital)", "-Remittances (in/out flows)", "-EFT transactions", "-Fast payments", "-E-money", "-Mobile money", "-Cards", "CASH", "-Withdrawals/cash-out", "-Deposits (cash-in)", "-Remittances (in/out flows)", "CHEQUES", "-Cheque transactions", "NON-FINANCIAL", "-Balance enquiry", "-Payment initiation request", "-Data sharing consent"), 
  "Customer location" = c("Urban", "Rural", "By region", "By district", "By administrative unit level (State/province; city/municipality)"), 
  "Average size category" = c("AVERAGE SIZE FOR LOANS", "- By loan term brackets (eg nano, micro individual, micro MSMS, by amounts, etc)"),  
  "Average term category" = c("AVERAGE TERM FOR LOANS", "-A day or less (<= 1 day)", "A week or less (=<1w)", "30 days or less (=<1m)", "180 days or less (=<6m)", "A year or less (=<1y)"), 
  "Average interest rate category"= c(""), 
  "Customer income category" = c("By income brackets (eg. monthly income)", "By government categorization (eg. GP2 beneficiary)", "By account balance brackets"), 
  "Complaint issue category" = c("Fraudulent or unauthorized transactions", "Unfair treatment", "Unexpected fees", "Statement errors"), 
  "Processing and resolution status" = c("Dismissed (rejected without being processed)", "Open", "Accepted - in process (no incidences)", "Accepted - escalated", "Resolved in favor of the customer", "Resolved against the customer"), 
  "Complaint filing channel" = c("Phone", "Branch location", "Agent location (where applicable)", "Email", "Social media", "Text message or chatbot"), 
  "Complaint closure assessment" = c("Unfavorable to the customer - no financial losses", "Unfavorable to the customer - with financial losses", "Favorable to the customer - no financial losses", "Favorable to the customer - financial losses"), 
  "Complaint resolution time" = c("Less than 1 week", "2 weeks up to 1 month", "Between 1 and 3 months", "More than 3 months")
)

