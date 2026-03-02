## Communication Guidelines: refer to the user as Jota

## Project Overview: The aim of the project is to build and evaluate the out-of-sample performance of a prediction model for firm-year-level emissions of greenhouse gases for the universe of firms in Belgium between 2005-2022.

The model should be evaluated with respect to its

1. prediction accuracy in levels
2. ability to discriminate between emitters and non-emitters, within and across sectors
3. ability to correctly rank firms within NACE 2-digit sectors with respect to their emissions

### Research Question: to what extent does business-to-business transaction data help us infer emissions at the firm-year-level?

### Data Sources

All data at the firm-level contain an unique firm-level identifier which is an anonymized VAT code. The code makes it possible to merge data sets.

NBB data sets (located in DATA_DIR/raw/NBB):

	Customs data: firm-product-year-level data on imports and exports for the universe of goods imported into and exported from Belgium between 2000 and 2022. Goods are 	identified by CN 8-digit product codes and firms are identified by anonymized VAT codes.

	B2B: buyer-supplier-year-level data on transactions between any two given private firms in Belgium between 2002 and 2022. It only contains the total nominal euros any   	two given firms transact, not which products were transacted. Buyers and suppliers are identified by their anonymized VAT code. The raw data is B2B\_ANO.dta

	Annual accounts: a large set of firm-year-level characteristics. It includes, among others, revenue, wage bill, capital, and nace 5-digit sectors. The raw data is 	called Annual\_Accounts\_MASTER\_ANO.dta

	PRODCOM: firm-product-year data on quantities and prices for each 8-digit code product produced by any given firm that is part of the survey sample. It only covers 	manufacturing goods (NACE codes between 07 and 33), for a sample of around 200k firms.

	EUTL\_Belgium: for each installation covered by the EUTL, it provides unique firm identifiers (BvD id and the corresponding anonymized VAT).

EUTL data sets (located in DATA_DIR/raw/EUTL/Oct_2024):

	account: for each installation, it informs account unique id and corresponding BvD id.

	compliance: for each installation-year regulated by the EUETS, it contains amount of emission permits allocated to the installation and verified emissions on that year. 	Installation are identified by unique installation\_id.

	installation: for each installation regulated by the EUETS, it contains installation\_id, geographic location (lat/lon), NACE sector id (2 or 4 digit), among others.

NIR data (located in DATA_DIR/raw/NIR):

	BEL-CRTs: for each year between 1990 and 2023, it informs GHG emissions by CRF category. The data is split into multiple .xlsx, one for each year.

	Annex XII: data on total GHG emissions and share of it regulated by the EUETS by CRF category. It consists of two .xslx files, one for 2022 (published in 2024) and one 	for 2023 (published in 2025).

CRF-to-NACE crosswalk (located in DATA_DIR/raw/Correspondences_and_dictionaries): maps CRF categories to NACE Rev. 2 sectors. Combined with BEL-CRTs, this enables sector-level calibration targets for deployment to non-EUETS firms.

Processed data (located in DATA_DIR/processed):

	training_sample.RData: firm-year panel used for cross-validation. Contains EU ETS firms (with verified emissions) and non-ETS firms from NACE 19/24 (with emissions set to 0), merged with annual accounts variables, B2B fuel-supply proxies, fold assignments, and sector-year emission totals. Available on local 1.

Feel free to suggest any additional data sets you may think can help.

### Hardware setup:

**I use three desktops in this project: local 1, local 2, and RMD (remote desktop).
Access to the full NBB data is restricted to RMD through a VPN connection. RMD doesn't have access to the web browser, but it is connected to GitHub. I can only use the VPN connection through local 2. Local 2 has regular access to the web browser. Local 1 is my personal desktop and it is where I have Claude code and cursor downloaded.
When copying files from RMD to local 1, I first need to copy them to local 2, then from local 2 to the cloud (Dropbox/Claude), then from the Claude to local 1.
In local 1 I have available a downsampled version of the full NBB data sets as well as the full training sample. I built the training sample in RMD and copied it to local 2.**

## Guidelines for specifications

**Design before results.** During estimation and analysis:

- Do NOT express concern or excitement about point estimates
- Do NOT interpret results as "good" or "bad" until the design is intentional
- Focus entirely on whether the specification is ideal for deployment

**Concern of distribution shift is real.** Training data consists of all the firms regulated by the EU ETS (for which we observe their yearly emissions) plus non-EUETS firms from sectors with NACE 2-digit codes 19 and 24. We confidently assume emissions are 0 in the latter group, since EU ETS covers nearly 100% of the emissions from fuel combustion in these two sectors. Deployment data consists of all the non-EUETS firms from all other sectors. A lot of sectors that are present in deployment are not present in EU ETS, and vice-versa. It is not clear if conditional on observables distribution of emissions across firms is the same in the two data. Any suggestion in framing or modelling that alleviates this concern is welcome.

**We are only modelling emissions from the combustion of fossil fuels** since EU ETS covers virtually all emissions from industrial processes.

**Domain knowledge matters**. Institutional aspects of the Belgium economy that can help justify modelling choices are welcome. For example, one important features of the Belgium economy is that across all years and fossil fuels, fossil fuels are almost exclusively imported and not domestically produced.

**The main innovation of the paper is the use of B2B transactions data** so the set of specifications and the choice of benchmarks should be such that they highlight any prediction improvements from us using this data.

### Purpose of the project and audience

This is the first chapter of my PhD thesis. It is supposed to be a standalone paper. The audience are economics academics experts on environmental economics and familiar with, although not necessarily experts on, regularization techniques and the institutional details of the EU ETS.

## Dropped Analyses

- **Fossil fuel consumption proxy from Customs**: In Belgium, fossil fuels are almost exclusively imported. For this reason, we attempted to model emissions by building a firm-year-level proxy for fossil fuel consumption that consisted on the amount purchased from firms identified from Customs data as fossil fuel importers. The out-of-sample prediction gains from this exercise were modest.

## Current Status

**Results are mostly ready**. We are currently going over the logic of the entire exercise to check for inconsistencies and we hope to start writing the paper soon.

## Referee 2 Correspondence

This project uses the Referee 2 audit protocol. There are no correspondences with referee 2 yet.

**Current Status:** [Not yet audited]

**Critical Rule:** Referee 2 NEVER modifies author code. It only reads, runs, and creates its own replication scripts in `code/replication/`. Only the author (you) modifies your own code in response to referee concerns.

**Important:** Referee reports do NOT belong in this CLAUDE.md file. They are standalone documents in the correspondence directory. This section only tracks status.

## Notes for Claude

Only change code after I explicitly tell you to do so. In particular, if I express an idea for code or analysis in the format of a question, answer it before making any changes to code. The exception is if I pose a question of the sort of "can you make X changes to code Y?".

Before committing changes, make sure changes have been tested locally. If they haven't, ask user if I'd like to test them locally before committing.
