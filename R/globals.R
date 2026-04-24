# R/globals.R - Global variables and constants

SECTOR_COLORS <- list(
  "Payments" = "#FFB718",
  "Credit" = "#ED7700",
  "Insurance" = "#17A627",
  "Investments" = "#38A5D6",
  "Pensions" = "#C64689",
  "Several/other" = "#EAE9E6",
  "Savings" = "#0080B2"
)

# Mandate-objective hierarchy (used in filter panel and data connector)
# NOTE: An earlier 10-mandate version (MND_OBJ) existed but was consolidated into these 5.
MND_OBJ_2 <-
  list(
    "Financial inclusion" = c("Access", "Uptake (account ownership)", "Usage", "Quality", "Outcomes"), 
    "Consumer protection" = c("Data privacy and protection", "Fair treatment", "Complaints handling", "Safety and security", "Suitability", "Depositor protection"), 
    "Market development" = c("Capital markets development", "Competition"), 
    "Sustainability" = c("Climate and environmental objectives", "Gender equality"), 
    "Stability, safety and soundness" = c("Stability", "Credit risk", "Market risk", "Operational risk", "Liquidity risk", "Soundness", "Reputational and legal risk", "AML/CFT")
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
  "main_sector" = "Main financial service",               
  "secondary_sectors" = "Other applicable services",      
  "IMF" = "IMF-FAS",                  
  "GPFI" = "GPFI",                  
  "AFI" = "AFI", 
  "WEF" = "WE Finance Code",                  
  "references" = "References",               
  "disaggregation_vars" = "Suggested breakdowns for segmentation analysis"
)

BREAKDOWNS <- list(
  "Customer gender" = c("Female", "Male", "Non-binary", "Transgender"), 
  "Customer type" = c("Natural person-individual", "Legal person-entity/organization", "-Micro, Small and Medium enterprises (MSMEs)",  "--Micro enterprises", "--Small enterprises", "--Medium enterprises", "-Small and Medium enterprises (SMEs)"), 
  "Customer age" = c("< 30 years", "30-55 years", "55-65 years", "65+ years"), 
  "Customer location" = c("Urban", "Rural", "By region", "By district", "By administrative unit level (State/province; city/municipality)"), 
  "Customer income category" = c("By income brackets (eg. monthly income)", "By government categorization (eg. GP2 beneficiary)", "By account balance brackets"), 
  
  "MSME gender" = c("Female", "Male", "Non-binary", "Transgender"), 
  
  "Financial service provider (FSP) gender diversity" = c("Here the intention is to use indicators of FSP gender diversity, for example at Board-level, C-level (CEO, CFO, COO), , Middle manager (Director of compliance, Director of risk management, Director of marketing, Director of strategy, Director of HR, Other) to examine whether differences in this indicator across institutions is related to the gender diversity of the institution."), 
  "Financial service provider (FSP) type" = c("Commercial banks", "Credit unions and cooperatives", "Deposit taking MFI", "Other deposit taking institution (ODTI)", "Nonbank credit providers", "-Non-deposit taking MFI", "-Fintech credit providers", "Payment service providers (PSP)", "-E-money issuers (EMI)", "-Money transfer operatore (MTO)", "Mobile money providers", "Crowdfunding provider", "Payment initiation service provider (PISP)", "Account information service provider (AISP)", "Microinsureres-life", "Microinsurers-non life", "Insurers-life", "Insurers-non life", "Pension providers"), 
  "Financial service provider (FSP) main activity" = c("All FSPs giving credit", "All FSPs taking deposits", "All FSPs offering payment services", "..."), 
  
  "Product type" = c("PAYMENT INSTRUMENTS", "-Debit card", "-Credit card", "-Prepaid card", "-E-money (includes mobile money)", "-Cash", "-EFT (Electronic funds transfer)", "-Fast payments", "-Cheque", "CREDIT", "-Loans-all retail", "-Loans-consumer", "-Loans-housing", "-Loans-mortgage", "-Credit card", "-Revolving credit", "-P2P loans", "-Line of credit (except credit cards)", "DEPOSITS", "-Checking accounts", "--Basic/no-frills accounts", "-Savings accounts", "-Time/term deposit accounts", "INSURANCE", "-Life insuarance", "-Non-life insurance", "-Microinsurance-life", "-Microinsurance-non life", "-Climate disaster risk insurance", "PENSIONS", "-Micro pensions", "-Other pensions", "-Other long term investments for old age", "INVESTMENT", "-Equity", "-Bonds", "-Crowdfunding-debt (P2P lending)", "-Crowdfunding-equity"),  
  "Channel type" = c("PHYSICAL", "-Branch", "-ATM", "-POS", "-Agents (banking agents)", "-Agents (mobile money agent)", "-Merchants (physical merchants) (POS/QR)", "-Insurance agents/intermediaries/brokers", "-Pension agents/brokers", "-Investment broker/intermediary", "DIGITAL", "-Online merchants/agents", "-Financial app registration/subscription", "-Internet banking registration/subscription", "-Online trading platform", "-P2P platform", "-Crowdfunding platform"), 
  "Transaction type" = c("DIGITAL/CASHLESS TRANSACTIONS", "-Payments and transfers", "-Deposits (digital)", "-Remittances (in/out flows)", "-EFT transactions", "-Fast payments", "-E-money", "-Mobile money", "-Cards", "CASH", "-Withdrawals/cash-out", "-Deposits (cash-in)", "-Remittances (in/out flows)", "CHEQUES", "-Cheque transactions", "NON-FINANCIAL", "-Balance enquiry", "-Payment initiation request", "-Data sharing consent"), 
  "Insurance type" = c("Life insuarance", "Non-life insurance", "Microinsurance-life", "Microinsurance-non life", "Climate disaster risk insurance"), 
  "Account type" = c("TRANSACTION ACCOUNT", "- Deposit account", "- Payment account", "-- E-money account", "--- Mobile money account", "-- Other payment accounts", "- Investment account", "NON-TRANSACTION ACCOUNT", "- Pension accounts", "- Time/term deposits"), 
  "Deposit account type" = c("Demand deposit accounts", "-Checking accounts", "--Basic/no-frills accounts", "Savings accounts", "Time/term deposit accounts"), 
  "Pension type" = c("Micro pensions", "Other pensions", "Other long term investments for old age", "Public", "Private", "Mandatory", "Voluntary"), 
    
  "Loan size" = c("Nano", "Micro-natural person", "Micro-legal person", "Small", "Medium"),  
  "Loan term" = c("A day or less (<= 1 day)", "A week or less (=<1w)", "30 days or less (=<1m)", "180 days or less (=<6m)", "A year or less (=<1y)"), 
  "Loan type" = c("Loans-all retail", "Loans-consumer", "Loans-housing", "Loans-mortgage", "Payroll loans", "Digital loans", "Credit card", "Revolving credit", "P2P loans", "Line of credit (except credit cards)"), 
  
  "Interest rate category"= c("Variable", "Fixed", "Combination of fixed and variable"), 
  
  "Complaints issue category" = c("Fraudulent or unauthorized transactions", "Unfair treatment", "Unexpected fees", "Statement errors"), 
  "Complaints processing status" = c("Dismissed (rejected without being processed)", "Open", "Accepted - in process (no incidences)", "Accepted - escalated", "Resolved in favor of the customer", "Resolved against the customer"), 
  "Complaints filing channel" = c("Phone", "Branch location", "Agent location (where applicable)", "Email", "Social media", "Text message or chatbot"), 
  "Complaints resolution outcome" = c("Unfavorable to the customer - no financial losses", "Unfavorable to the customer - with financial losses", "Favorable to the customer - no financial losses", "Favorable to the customer - financial losses"), 
  "Complaints resolution time" = c("Less than 1 week", "2 weeks up to 1 month", "Between 1 and 3 months", "More than 3 months"), 
  
  "Collateral type" = c("INMOVABLE", "-Land", "-House", "- Other commercial property", "MOVABLE", "-Vehicles and equipment", "-Livestock", "-Inventory and Stock", "-Receivables and Contracts", "-Cash Flow-based Collateral", "INTANGIBLE/ALTERNATIVE", "-Reputation/Social Capital", "-Digital data")

)

# Preset memos — narrative guidance for each preset filter
# Each entry has: title, summary, sections (list of heading + body pairs)
PRESET_MEMOS <- list(
  "preset_digital" = list(
    title = "Digital Finance Ecosystem",
    summary = "This preset selects indicators particularly relevant to understanding and monitoring digital financial services, including mobile money, e-money, digital credit, and digital payment channels.",
    sections = list(
      list(
        heading = "Rationale",
        body = "The rapid growth of digital financial services is reshaping how people access and use financial products, particularly in emerging markets. Financial sector authorities need indicators that capture digital-specific dynamics — from mobile money adoption and digital credit growth to the risks and consumer protection challenges unique to digital channels. This preset curates indicators that are especially informative for understanding the digital finance landscape."
      ),
      list(
        heading = "Key questions this preset helps address",
        body = paste0(
          "<ul style='margin: 0; padding-left: 20px;'>",
          "<li>What share of financial transactions occur through digital channels vs. traditional ones?</li>",
          "<li>How does access to and usage of digital financial services differ by gender, age, or location?</li>",
          "<li>Are digital credit products associated with different risk profiles than traditional lending?</li>",
          "<li>How effectively are consumer protection frameworks covering digital financial services?</li>",
          "<li>What role do mobile money and e-money play in driving financial inclusion?</li>",
          "</ul>"
        )
      ),
      list(
        heading = "Analytical approach",
        body = "These indicators can be analyzed individually or combined with breakdowns such as channel type (digital vs. physical), product type (e-money, digital credit), and customer demographics (gender, age, location) to reveal patterns in digital adoption, usage intensity, and associated risks. Cross-referencing digital indicators with consumer protection metrics can highlight gaps in regulatory coverage."
      )
    )
  ),
  "preset_msme" = list(
    title = "MSME Focus",
    summary = "This preset selects indicators particularly relevant to understanding and monitoring micro, small and medium enterprises (MSMEs) in the financial sector.",
    sections = list(
      list(
        heading = "Rationale",
        body = "MSMEs are the backbone of most economies, yet they often face significant barriers to accessing financial services. Financial sector authorities need indicators that capture MSME-specific dynamics — from credit access and loan performance to the gender dimensions of enterprise finance. This preset curates indicators that are especially informative for understanding how financial systems serve MSMEs."
      ),
      list(
        heading = "Key questions this preset helps address",
        body = paste0(
          "<ul style='margin: 0; padding-left: 20px;'>",
          "<li>What share of credit is directed toward MSMEs, and how does this vary by enterprise size?</li>",
          "<li>How does access to finance differ for female-owned vs. male-owned MSMEs?</li>",
          "<li>What are the risk profiles associated with MSME lending portfolios?</li>",
          "<li>Are financial products designed to meet the specific needs of micro and small enterprises?</li>",
          "<li>How do collateral requirements and loan terms affect MSME borrowing?</li>",
          "</ul>"
        )
      ),
      list(
        heading = "Analytical approach",
        body = "These indicators can be analyzed individually or combined with breakdowns such as customer type (micro, small, medium enterprise), customer gender, product type, and FSP type to reveal patterns in MSME finance. Cross-referencing MSME indicators with inclusion and stability metrics can highlight trade-offs and opportunities in MSME lending policy."
      )
    )
  ),
  "preset_finhealth" = list(
    title = "Financial Health",
    pages = list(
      list(
        label = "Overview",
        summary = "This preset selects LENS indicators that map to the CGAP Financial Health Measurement Framework, a forthcoming framework introduced in a CGAP working paper that translates the G20/GPFI definition of financial health into six measurement constructs and seventeen metrics.",
        sections = list(
          list(
            heading = "About the framework",
            body = paste0(
              "The framework is grounded in a comprehensive review of seventy-one existing measurement initiatives ",
              "and is designed around three principles: multidimensionality, use of complementary data sources, and adaptability to local context. ",
              "It translates the four dimensions of the G20/GPFI definition of financial health &mdash; the ability to manage financial needs and obligations, ",
              "to cope with shocks, to pursue aspirations, and to feel satisfied and confident &mdash; into six measurable constructs and seventeen metrics ",
              "that financial sector authorities and others can use."
            )
          ),
          list(
            heading = "How this preset connects to the framework",
            body = paste0(
              "<p>Of the seventeen metrics in the framework, nine can be directly measured or approximated using supply-side regulatory indicators. ",
              "This preset selects those nine LENS indicators. In some cases, the LENS indicator name differs from the framework metric name ",
              "but measures the same underlying concept.</p>",
              "<p>The remaining eight metrics &mdash; including all four subjective metrics and several resilience and asset metrics &mdash; ",
              "require demand-side survey data and are not included in the LENS catalog. ",
              "See the <strong>Framework</strong> page for the full mapping.</p>"
            )
          ),
          list(
            heading = "Key questions this preset helps address",
            body = paste0(
              "<ul style='margin: 0; padding-left: 20px;'>",
              "<li>Are consumers building adequate savings and financial cushions?</li>",
              "<li>What are the patterns of over-indebtedness and loan delinquency, and do they differ by gender?</li>",
              "<li>How effectively are consumer protection mechanisms safeguarding financial well-being?</li>",
              "<li>Are insurance and pension products contributing to long-term financial resilience?</li>",
              "<li>What is the relationship between financial product usage and financial health outcomes?</li>",
              "</ul>"
            )
          ),
          list(
            heading = "Reference",
            body = paste0(
              "<p style='font-size: 12px; color: #6c757d;'>Nielsen, K., Gubbins, P. and Alonso, T. (2026). ",
              "<em>Measuring Financial Health: A Framework.</em> Working Paper. Washington, DC: CGAP.</p>"
            )
          )
        )
      ),
      list(
        label = "Framework",
        summary = paste0(
          "The table below shows all seventeen metrics in the CGAP Financial Health Measurement Framework. ",
          "Highlighted rows indicate metrics that map to a LENS indicator in this preset. ",
          "Where the LENS indicator name differs from the framework metric, both names are shown."
        ),
        sections = list(
          list(
            heading = "The Financial Health Measurement Framework",
            body = paste0(
              "<table class='memo-framework-table'>",
              "<thead>",
              "<tr><th class='fh-col-dim'>Dimension</th><th class='fh-col-construct'>Construct</th><th class='fh-col-num'>#</th><th class='fh-col-metric'>Metric</th><th class='fh-col-lens'>LENS indicator</th></tr>",
              "</thead>",
              "<tbody>",

              # Dimension 1: Manage needs — Construct 1 (4 metrics)
              "<tr class='fh-mapped'>",
              "<td rowspan='7' class='fh-dim-cell fh-dim-manage'>Manage needs &amp; obligations</td>",
              "<td rowspan='4' class='fh-construct-cell'>1. Pays for basic needs without significant strain</td>",
              "<td>1</td><td>Positive net cashflow</td>",
              "<td class='fh-match'><i class='fas fa-check-circle'></i> Positive net cashflow</td></tr>",

              "<tr class='fh-unmapped'><td>2</td><td>Material deprivation</td>",
              "<td class='fh-no-match'><span class='fh-survey-tag'>Survey</span></td></tr>",

              "<tr class='fh-unmapped'><td>3</td><td>Bills in arrears</td>",
              "<td class='fh-no-match'><span class='fh-external-tag'>External</span></td></tr>",

              "<tr class='fh-mapped'><td>4</td><td>Account overdrafts / NSF events</td>",
              "<td class='fh-match'><i class='fas fa-check-circle'></i> Account overdrafts and/or non-sufficient funds events</td></tr>",

              # Construct 2 (1 metric)
              "<tr class='fh-mapped'>",
              "<td class='fh-construct-cell'>2. Transacts safely with financial system</td>",
              "<td>5</td><td>Financial loss due to fraud or deception</td>",
              "<td class='fh-match fh-name-diff'><i class='fas fa-check-circle'></i> Frauds resulting in customer losses</td></tr>",

              # Construct 3 (2 metrics)
              "<tr class='fh-mapped'>",
              "<td rowspan='2' class='fh-construct-cell'>3. Sustainable debt load &amp; repays obligations</td>",
              "<td>6</td><td>Debt in arrears or delinquency</td>",
              "<td class='fh-match fh-name-diff'><i class='fas fa-check-circle'></i> Delinquency rate in the loan portfolio</td></tr>",

              "<tr class='fh-mapped'><td>7</td><td>Sustainable debt service ratio</td>",
              "<td class='fh-match fh-name-diff'><i class='fas fa-check-circle'></i> Debt service-to-income ratio</td></tr>",

              # Dimension 2: Cope with shocks — Construct 4 (3 metrics)
              "<tr class='fh-unmapped'>",
              "<td rowspan='3' class='fh-dim-cell fh-dim-cope'>Cope with shocks</td>",
              "<td rowspan='3' class='fh-construct-cell'>4. Has resources to cover unexpected expenses</td>",
              "<td>8</td><td>Reliable access to emergency funds</td>",
              "<td class='fh-no-match'><span class='fh-survey-tag'>Survey</span></td></tr>",

              "<tr class='fh-mapped'><td>9</td><td>Liquid savings (cash) buffer</td>",
              "<td class='fh-match'><i class='fas fa-check-circle'></i> Liquid savings (cash) buffer</td></tr>",

              "<tr class='fh-mapped'><td>10</td><td>Active insurance coverage</td>",
              "<td class='fh-match fh-name-diff'><i class='fas fa-check-circle'></i> Insurance policy holders</td></tr>",

              # Dimension 3: Pursue aspirations — Construct 5 (3 metrics)
              "<tr class='fh-mapped'>",
              "<td rowspan='3' class='fh-dim-cell fh-dim-aspire'>Pursue aspirations &amp; goals</td>",
              "<td rowspan='3' class='fh-construct-cell'>5. Participates in long-term savings / investments</td>",
              "<td>11</td><td>Long-term saving / financial asset accumulation</td>",
              "<td class='fh-match fh-name-diff'><i class='fas fa-check-circle'></i> Long-term saving and/or financial asset accumulation</td></tr>",

              "<tr class='fh-unmapped'><td>12</td><td>Investment in productive / physical assets</td>",
              "<td class='fh-no-match'><span class='fh-survey-tag'>Survey</span></td></tr>",

              "<tr class='fh-mapped'><td>13</td><td>Pension or retirement account holder</td>",
              "<td class='fh-match fh-name-diff'><i class='fas fa-check-circle'></i> Pension account holders</td></tr>",

              # Dimension 4: Feels satisfied — Construct 6 (4 metrics)
              "<tr class='fh-unmapped'>",
              "<td rowspan='4' class='fh-dim-cell fh-dim-satisfy'>Feels satisfied &amp; confident</td>",
              "<td rowspan='4' class='fh-construct-cell'>6. Financial confidence, control &amp; satisfaction</td>",
              "<td>14</td><td>Satisfaction with financial situation</td>",
              "<td class='fh-no-match'><span class='fh-survey-tag'>Survey</span></td></tr>",

              "<tr class='fh-unmapped'><td>15</td><td>Perceived financial control</td>",
              "<td class='fh-no-match'><span class='fh-survey-tag'>Survey</span></td></tr>",

              "<tr class='fh-unmapped'><td>16</td><td>Confidence in future financial outlook</td>",
              "<td class='fh-no-match'><span class='fh-survey-tag'>Survey</span></td></tr>",

              "<tr class='fh-unmapped'><td>17</td><td>Financial stress or worry</td>",
              "<td class='fh-no-match'><span class='fh-survey-tag'>Survey</span></td></tr>",

              "</tbody></table>",
              "<p class='fh-table-legend'>",
              "<span class='fh-legend-item'><i class='fas fa-check-circle' style='color: #28a745;'></i> Mapped to LENS indicator</span>",
              "<span class='fh-legend-item fh-legend-diff'><i class='fas fa-check-circle' style='color: #28a745;'></i> Mapped (name differs)</span>",
              "<span class='fh-legend-item'><span class='fh-survey-tag'>Survey</span> Requires demand-side data</span>",
              "<span class='fh-legend-item'><span class='fh-external-tag'>External</span> Requires non-FSA data</span>",
              "</p>",
              "<p style='font-size: 12px; color: #6c757d; margin-top: 12px;'>Source: Nielsen, Gubbins and Alonso (2026). Adapted from Table E1.</p>"
            )
          )
        )
      )
    )
  )
)

