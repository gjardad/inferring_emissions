# LLM Prompts for AI-Generated Crosswalks and Classifications

This file contains the 5 prompts used to generate classification/crosswalk
datasets via Claude Opus 4.6. Each prompt is self-contained and embeds
authoritative ground-truth codes so the LLM only needs to apply domain
knowledge — not recall arbitrary alphanumeric codes from memory.

**Workflow for each prompt:**
1. Open a fresh Claude Opus 4.6 conversation (no project context)
2. Paste the prompt verbatim
3. Copy the LLM's R tribble output
4. Paste it into the corresponding R script

---

## Prompt 1 — CN8 fossil fuel codes for stationary combustion (script 18)

**Corresponding script:** `preprocess/18_build_cn8_fossil_fuel_list.R`
**Output object:** `cn8digit_codes_for_fossil_fuels` (columns: `cn_code`, `description`)

**Authoritative source for CN8 codes:** Commission Implementing Regulation
(EU) 2021/1832, Annex I (Combined Nomenclature applicable from 1 January 2022).
Codes retrieved from https://www.conex.net/nc8/2022/en/27.html

```
ROLE: You are an expert in international trade classification systems
(HS/CN nomenclature) and greenhouse gas emissions reporting (IPCC,
EU ETS).

TASK: From the complete list of 8-digit Combined Nomenclature (CN8)
product codes from Chapter 27 below, identify which codes correspond
to fossil fuels that could be combusted for energy in stationary
installations (e.g. boilers, furnaces, kilns, CHP plants, backup
generators).

The CN8 codes and descriptions below are taken from the 2022 EU
Combined Nomenclature (Commission Implementing Regulation (EU)
2021/1832, Annex I).

INPUT — All CN8 codes in Chapter 27:

  2701 — Coal; briquettes, ovoids and similar solid fuels:
    27011100  Anthracite, whether or not pulverised, but not agglomerated
    27011210  Coking coal, whether or not pulverised, but not agglomerated
    27011290  Other bituminous coal, whether or not pulverised, but not agglomerated
    27011900  Other coal (incl. sub-bituminous), whether or not pulverised, but not agglomerated
    27012000  Briquettes, ovoids and similar solid fuels manufactured from coal

  2702 — Lignite:
    27021000  Lignite, whether or not pulverised, but not agglomerated
    27022000  Agglomerated lignite

  2703 — Peat:
    27030000  Peat (including peat litter), whether or not agglomerated

  2704 — Coke and semi-coke:
    27040010  Coke and semi-coke of coal
    27040030  Coke and semi-coke of lignite
    27040090  Other coke and semi-coke (incl. retort carbon)

  2705 — Coal gas, water gas, producer gas:
    27050000  Coal gas, water gas, producer gas and similar gases (other than petroleum gases)

  2706 — Tar:
    27060000  Tar distilled from coal, lignite or peat, and other mineral tars

  2707 — Oils and products of high-temperature coal tar distillation:
    27071000  Benzol (benzene)
    27072000  Toluol (toluene)
    27073000  Xylol (xylenes)
    27074000  Naphthalene
    27075000  Other aromatic hydrocarbon mixtures (>= 65% by vol. distils at 250°C)
    27079100  Creosote oils
    27079911  Crude light oils (>= 90% by vol. distils at up to 200°C)
    27079919  Other crude light oils
    27079920  Sulphuretted toppings; anthracene
    27079950  Basic products
    27079980  Phenols
    27079991  Products for manufacture of heading 2803
    27079999  Other products of distillation of high-temperature coal tar

  2708 — Pitch and pitch coke:
    27081000  Pitch obtained from coal tar or from other mineral tars
    27082000  Pitch coke

  2709 — Crude petroleum oils:
    27090010  Natural gas condensates
    27090090  Other crude petroleum oils

  2710 — Petroleum oils (not crude), preparations, waste oils:
    27101211  Light oils for undergoing a specific process
    27101215  Light oils for undergoing chemical transformation
    27101221  White spirit
    27101225  Other special spirits
    27101231  Aviation spirit (avgas)
    27101241  Motor spirit (gasoline), RON < 95
    27101245  Motor spirit (gasoline), RON 95–98
    27101249  Motor spirit (gasoline), RON >= 98
    27101250  Motor spirit (gasoline), leaded
    27101270  Spirit type jet fuel
    27101290  Other light oils and preparations
    27101911  Medium oils for undergoing a specific process
    27101915  Medium oils for undergoing chemical transformation
    27101921  Jet fuel (kerosene type)
    27101925  Other kerosene
    27101929  Other medium oils and preparations
    27101931  Gas oils for undergoing a specific process
    27101935  Gas oils for undergoing chemical transformation
    27101943  Gas oils, sulphur <= 0.001%
    27101946  Gas oils, sulphur > 0.001% and <= 0.002%
    27101947  Gas oils, sulphur > 0.002% and <= 0.1%
    27101948  Gas oils, sulphur > 0.1%
    27101951  Fuel oils for undergoing a specific process
    27101955  Fuel oils for undergoing chemical transformation
    27101962  Fuel oils, sulphur <= 0.1%
    27101966  Fuel oils, sulphur > 0.1% and <= 0.5%
    27101967  Fuel oils, sulphur > 0.5%
    27101971  Lubricating oils for undergoing a specific process
    27101975  Lubricating oils for undergoing chemical transformation
    27101981  Motor oils, compressor lube oils, turbine lube oils
    27101983  Hydraulic oils
    27101985  White oils, liquid paraffin
    27101987  Gear oils and reductor oils
    27101991  Metal-working compounds, mould-release oils, anti-corrosion oils
    27101993  Electrical insulating oils
    27101999  Other lubricating oils and other oils
    27102011  Gas oils containing biodiesel, sulphur <= 0.001%
    27102016  Gas oils containing biodiesel, sulphur > 0.001% and <= 0.1%
    27102019  Gas oils containing biodiesel, sulphur > 0.1%
    27102032  Fuel oils containing biodiesel, sulphur <= 0.5%
    27102038  Fuel oils containing biodiesel, sulphur > 0.5%
    27102090  Other petroleum oils containing biodiesel
    27109100  Waste oils containing PCBs/PCTs/PBBs
    27109900  Other waste oils

  2711 — Petroleum gases and other gaseous hydrocarbons:
    27111100  Liquefied natural gas (LNG)
    27111211  Propane (purity >= 99%), for power or heating fuel
    27111219  Propane (purity >= 99%), for other purposes
    27111291  Propane (purity > 90% but < 99%), for specific process
    27111293  Propane (purity > 90% but < 99%), for chemical transformation
    27111294  Propane (purity > 90% but < 99%), for other purposes
    27111297  Other propane
    27111310  Butanes, for undergoing a specific process
    27111330  Butanes, for undergoing chemical transformation
    27111391  Butanes (purity > 90% but < 95%), for other purposes
    27111397  Other butanes
    27111400  Ethylene, propylene, butylene and butadiene
    27111900  Other liquefied petroleum gases (LPG)
    27112100  Natural gas in gaseous state
    27112900  Other petroleum gases and gaseous hydrocarbons in gaseous state

  2712 — Petroleum jelly, paraffin wax, mineral waxes:
    27121010  Crude petroleum jelly
    27121090  Other petroleum jelly
    27122010  Synthetic paraffin wax (mol. weight 460–1560)
    27122090  Other paraffin wax (< 0.75% oil)
    27129011  Crude ozokerite, lignite wax or peat wax
    27129019  Other ozokerite, lignite wax or peat wax
    27129031  Crude mineral waxes, for specific process
    27129033  Crude mineral waxes, for chemical transformation
    27129039  Crude mineral waxes, for other purposes
    27129091  Blends of 1-alkenes (>= 80%, chain C24–C28)
    27129099  Other mineral waxes and similar products

  2713 — Petroleum coke, petroleum bitumen, other residues:
    27131100  Petroleum coke, not calcined
    27131200  Petroleum coke, calcined
    27132000  Petroleum bitumen
    27139010  Other residues, for manufacture of heading 2803
    27139090  Other residues of petroleum oils

  2714 — Bituminous minerals, natural bitumen:
    27141000  Bituminous or oil-shale and tar sands
    27149000  Other natural bitumen and natural asphalt

  2715 — Bituminous mixtures:
    27150000  Bituminous mixtures based on natural asphalt/bitumen/petroleum bitumen/mineral tar

  2716 — Electrical energy:
    27160000  Electrical energy

CLASSIFICATION CRITERIA:

Include codes for:
  - Solid fossil fuels: coal (anthracite, bituminous, sub-bituminous),
    lignite, coke, semi-coke, briquettes
  - Liquid fossil fuels: crude oil, fuel oils, gas oils / diesel,
    kerosene (heating), naphtha, LPG, petroleum coke, and other
    petroleum products burned for heat or power in STATIONARY
    installations
  - Gaseous fossil fuels: natural gas, petroleum gases (propane, butane,
    ethylene, etc.) in gaseous or liquefied form
  - Peat and peat products

Exclude codes for:
  - Electricity (2716)
  - Transport fuels: motor gasoline / motor spirit (27101231–27101250),
    aviation gasoline (27101231), jet fuels — both gasoline-type
    (27101270) and kerosene-type (27101921)
  - Products primarily NOT fuels: lubricating oils, greases, paraffin
    wax, petroleum jelly, bitumen / asphalt, mineral waxes
  - Mineral tar pitch (27081000) — used as binder, not fuel

Edge cases: If a product (e.g. white spirit, certain naphtha grades,
waste oils) is sometimes burned for energy in industrial settings but
primarily used as a solvent or feedstock, INCLUDE it and note the
ambiguity in the description column with "[EDGE CASE: ...]".

OUTPUT FORMAT: Return an R tribble() with exactly two columns:
  - cn_code: character, the 8-digit CN code (e.g., "27011100")
  - description: character, the CN 2022 product description
    (use the descriptions provided above; append edge-case notes
    in square brackets where applicable)

Group the codes by HS4 heading and include a brief R comment above
each group.

Do not include any codes outside Chapter 27.
```

---

## Prompt 2 — NACE fuel supplier likelihood (script 11)

**Corresponding script:** `preprocess/11_fuel_supplier_likelihood_by_nace.R`
**Output object:** `nace_fuel_supplier_rules` (columns: `nace4d`, `likelihood`, `comment`)

**Authoritative source for NACE codes:** Regulation (EC) No 1893/2006
(NACE Rev. 2), as consolidated. Codes retrieved from
https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:02006R1893-20190726

```
ROLE: You are an expert in industrial classification systems (NACE
Rev. 2) and European energy markets, with knowledge of which economic
sectors are involved in the supply chain for fossil fuels.

TASK: For each NACE Rev. 2 four-digit code listed below, classify it
by the likelihood that a firm in that sector acts as a fuel supplier
(i.e. sells fossil fuels — coal, oil products, natural gas, LPG,
petroleum coke, etc. — to downstream industrial or commercial
customers).

The NACE codes below are taken from Regulation (EC) No 1893/2006
(NACE Rev. 2, consolidated version).

INPUT — NACE 4-digit codes from relevant divisions:

  Division 05 — Mining of coal and lignite:
    0510  Mining of hard coal
    0520  Mining of lignite

  Division 06 — Extraction of crude petroleum and natural gas:
    0610  Extraction of crude petroleum
    0620  Extraction of natural gas

  Division 19 — Manufacture of coke and refined petroleum products:
    1910  Manufacture of coke oven products
    1920  Manufacture of refined petroleum products

  Division 35 — Electricity, gas, steam and air conditioning supply:
    3511  Production of electricity
    3512  Transmission of electricity
    3513  Distribution of electricity
    3514  Trade of electricity
    3521  Manufacture of gas
    3522  Distribution of gaseous fuels through mains
    3523  Trade of gas through mains
    3530  Steam and air conditioning supply

  Division 46 — Wholesale trade (selected groups):
    4611  Agents in the sale of agricultural raw materials, live animals, etc.
    4612  Agents in the sale of fuels, ores, metals and industrial chemicals
    4613  Agents in the sale of timber and building materials
    4614  Agents in the sale of machinery, industrial equipment, ships and aircraft
    4615  Agents in the sale of furniture, household goods, hardware and ironmongery
    4616  Agents in the sale of textiles, clothing, fur, footwear and leather goods
    4617  Agents in the sale of food, beverages and tobacco
    4618  Agents specialised in the sale of other particular products
    4619  Agents in the sale of a variety of goods
    4671  Wholesale of solid, liquid and gaseous fuels and related products
    4672  Wholesale of metals and metal ores
    4673  Wholesale of wood, construction materials and sanitary equipment
    4674  Wholesale of hardware, plumbing and heating equipment and supplies
    4675  Wholesale of chemical products
    4676  Wholesale of other intermediate products
    4677  Wholesale of waste and scrap
    4690  Non-specialised wholesale trade

  Division 47 — Retail trade (selected):
    4730  Retail sale of automotive fuel in specialised stores

  Division 49 — Land transport and transport via pipelines:
    4910  Passenger rail transport, interurban
    4920  Freight rail transport
    4931  Urban and suburban passenger land transport
    4932  Taxi operation
    4939  Other passenger land transport n.e.c.
    4941  Freight transport by road
    4942  Removal services
    4950  Transport via pipeline

  Division 50 — Water transport:
    5010  Sea and coastal passenger water transport
    5020  Sea and coastal freight water transport
    5030  Inland passenger water transport
    5040  Inland freight water transport

  Division 51 — Air transport:
    5110  Passenger air transport
    5121  Freight air transport
    5122  Space transport

  Division 52 — Warehousing and support activities for transportation:
    5210  Warehousing and storage
    5221  Service activities incidental to land transportation
    5222  Service activities incidental to water transportation
    5223  Service activities incidental to air transportation
    5224  Cargo handling
    5229  Other transportation support activities

  Division 53 — Postal and courier activities:
    5310  Postal activities under universal service obligation
    5320  Other postal and courier activities

CLASSIFICATION CRITERIA:

  HIGH likelihood: The sector's core business model plausibly involves
    selling fossil fuels to other firms. Examples: fuel wholesalers,
    refiners, pipeline operators, gas distributors.

  MEDIUM likelihood: Intermediaries where fuel can be bundled,
    pass-through, or where fuel sales occur as a secondary activity.
    Examples: retail fuel stations, fuel agents/brokers, transport firms
    that may resell fuel.

  LOW likelihood: Sectors that do not typically sell fuel even if they
    consume it. Includes pure consumers of fuel (airlines, trucking),
    non-fuel wholesalers, postal services, etc.

IMPORTANT: The classification should reflect whether the sector SELLS
fuel, not whether it CONSUMES fuel. A trucking company (4941) consumes
fuel but does not sell it, so it should be LOW. A gas distributor (3522)
sells fuel, so it should be HIGH.

OUTPUT FORMAT: Return an R tribble() with exactly three columns:
  - nace4d: character, the 4-digit NACE code (e.g., "1920")
  - likelihood: character, one of "high", "medium", or "low"
  - comment: character, brief justification for the classification

Only include codes classified as HIGH or MEDIUM. Do not list LOW codes
(they are the default for any NACE code not in the output).
```

---

## Prompt 3 — SIEC to IPCC fuel category mapping (script 13)

**Corresponding script:** `preprocess/13_build_siec_to_ipcc_mapping.R`
**Output object:** `siec_to_ipcc` (columns: `siec_code`, `siec_name`, `ipcc_fuel`, `mapping_quality`, `notes`)

**Authoritative sources:**
- SIEC codes: Eurostat SIEC codelist (SIEC Version 1.0),
  https://dd.eionet.europa.eu/vocabulary/eurostat/siec/view
- IPCC fuel names: IPCC 2006 Guidelines, Volume 2, Chapter 2, Table 2.2

```
ROLE: You are an expert in greenhouse gas emissions reporting (IPCC
2006 Guidelines for National Greenhouse Gas Inventories) and
international energy statistics classifications.

TASK: For each SIEC (Standard International Energy Product
Classification) code listed below, identify the closest matching
IPCC fuel category from the IPCC 2006 Guidelines, Volume 2 (Energy),
Chapter 2, Table 2.2 (Default Emission Factors for Stationary
Combustion).

The SIEC codes and names below are taken verbatim from the Eurostat
SIEC codelist (SIEC Version 1.0, as implemented by Eurostat;
source: https://dd.eionet.europa.eu/vocabulary/eurostat/siec/view).

INPUT — SIEC codes to map:

  Solid fossil fuels:
    C0110  Anthracite
    C0121  Coking coal
    C0129  Other bituminous coal
    C0210  Sub-bituminous coal
    C0220  Lignite
    C0311  Coke oven coke
    C0312  Gas coke
    C0320  Patent fuel
    C0330  Brown coal briquettes
    C0340  Coal tar

  Manufactured gases:
    C0350  Coke oven gas
    C0360  Gas works gas
    C0371  Blast furnace gas
    C0372  Basic oxygen steel furnace gas
    C0379  Other recovered gases

  Natural gas:
    G3000  Natural gas

  Oil — crude and feedstocks:
    O4100_TOT  Crude oil
    O4200      Natural gas liquids
    O4300      Refinery feedstocks
    O4500      Other hydrocarbons

  Oil products:
    O4610              Refinery gas
    O4620              Ethane
    O4630              Liquefied petroleum gases
    O4640              Naphtha
    O4651              Aviation gasoline
    O4652              Motor gasoline
    O4652XR5210B       Motor gasoline (excl. biofuel portion)
    O4653              Gasoline-type jet fuel
    O4661              Kerosene-type jet fuel
    O4661XR5230B       Kerosene-type jet fuel (excl. biofuel portion)
    O4669              Other kerosene
    O4671              Gas oil and diesel oil
    O4671XR5220B       Gas oil and diesel oil (excl. biofuel portion)
    O4680              Fuel oil
    O4691              White spirit and SBP industrial spirits
    O4692              Lubricants
    O4693              Paraffin waxes
    O4694              Petroleum coke
    O4695              Bitumen
    O4699              Other oil products n.e.c.

  Oil shale:
    S2000  Oil shale and oil sands

  Peat:
    P1100  Peat
    P1200  Peat products

  Biofuels — solid:
    R5110-5150_W6000RI  Primary solid biofuels
    R5160               Charcoal

  Biofuels — liquid:
    R5210B  Biogasoline (blended)
    R5210P  Biogasoline (pure)
    R5220B  Biodiesels (blended)
    R5220P  Biodiesels (pure)
    R5230B  Bio jet kerosene (blended)
    R5230P  Bio jet kerosene (pure)
    R5290   Other liquid biofuels

  Biofuels — gaseous:
    R5300   Biogases

REFERENCE — IPCC 2006 Table 2.2 fuel names (use these exactly):

  Liquid Fossil Fuels:
    Crude Oil, Orimulsion, Natural Gas Liquids, Motor Gasoline,
    Aviation Gasoline, Jet Gasoline, Jet Kerosene, Other Kerosene,
    Shale Oil, Gas/Diesel Oil, Residual Fuel Oil,
    Liquefied Petroleum Gases, Ethane, Naphtha, Bitumen, Lubricants,
    Petroleum Coke, Refinery Feedstocks, Refinery Gas, Paraffin Waxes,
    White Spirit and SBP, Other Petroleum Products

  Solid Fossil Fuels:
    Anthracite, Coking Coal, Other Bituminous Coal, Sub-Bituminous Coal,
    Lignite, Oil Shale and Tar Sands, Brown Coal Briquettes, Patent Fuel,
    Coke Oven Coke and Lignite Coke, Gas Coke, Coal Tar, Gas Works Gas,
    Coke Oven Gas, Blast Furnace Gas, Oxygen Steel Furnace Gas

  Gaseous Fossil Fuels:
    Natural Gas

  Other Fossil Fuels:
    Municipal Wastes (non-biomass fraction), Industrial Wastes,
    Waste Oils, Peat

  Biomass:
    Wood / Wood Waste, Sulphite lyes (Black Liquor),
    Other Primary Solid Biomass, Charcoal, Biogasoline, Biodiesels,
    Other Liquid Biofuels, Landfill Gas, Sludge Gas, Other Biogas,
    Municipal Wastes (biomass fraction)

MAPPING INSTRUCTIONS:
  - Use IPCC fuel names exactly as they appear above
  - Where a 1:1 match exists, mark mapping_quality = "exact"
  - Where the SIEC code is broader or narrower than any single IPCC
    fuel, map to the closest IPCC fuel and mark
    mapping_quality = "approx", with a note explaining the mismatch
  - Flag products primarily used for non-energy purposes
    (lubricants, bitumen, paraffin waxes) in the notes column

OUTPUT FORMAT: Return an R tribble() with exactly five columns:
  - siec_code: character (use exactly the codes listed above)
  - siec_name: character (use exactly the names listed above)
  - ipcc_fuel: character (IPCC 2006 fuel name from Table 2.2)
  - mapping_quality: character ("exact" or "approx")
  - notes: character (NA_character_ if exact; explanation if approx)
```

---

## Prompt 4 — HS6/CN8 to IPCC fuel category mapping (script 19)

**Corresponding script:** `preprocess/19_build_hs_cn_to_ipcc_mapping.R`
**Output objects:**
- `hs6_to_ipcc_fuel_categories` (columns: `hs6`, `ipcc_fuel`, `energy_use`, `needs_cn8`, `comment`)
- `cn8_to_ipcc_fuel_categories` (columns: `cn8`, `ipcc_fuel`, `include`, `comment`)

**Authoritative sources:**
- CN8 codes: EU Combined Nomenclature 2022 (see Prompt 1 for full list)
- IPCC fuel names: IPCC 2006 Guidelines, Volume 2, Chapter 2, Table 2.2

```
ROLE: You are an expert in international trade classification systems
(HS/CN nomenclature) and greenhouse gas emissions reporting (IPCC 2006
Guidelines).

TASK: Build a mapping from HS 6-digit codes (Chapter 27: mineral fuels)
to IPCC fuel categories. Where HS6 is too coarse to assign a single
IPCC fuel, also provide CN 8-digit overrides.

REFERENCE — IPCC 2006 Table 2.2 fuel names (use these exactly):

  Liquid Fossil Fuels:
    Crude Oil, Orimulsion, Natural Gas Liquids, Motor Gasoline,
    Aviation Gasoline, Jet Gasoline, Jet Kerosene, Other Kerosene,
    Shale Oil, Gas/Diesel Oil, Residual Fuel Oil,
    Liquefied Petroleum Gases, Ethane, Naphtha, Bitumen, Lubricants,
    Petroleum Coke, Refinery Feedstocks, Refinery Gas, Paraffin Waxes,
    White Spirit and SBP, Other Petroleum Products

  Solid Fossil Fuels:
    Anthracite, Coking Coal, Other Bituminous Coal, Sub-Bituminous Coal,
    Lignite, Oil Shale and Tar Sands, Brown Coal Briquettes, Patent Fuel,
    Coke Oven Coke and Lignite Coke, Gas Coke, Coal Tar, Gas Works Gas,
    Coke Oven Gas, Blast Furnace Gas, Oxygen Steel Furnace Gas

  Gaseous Fossil Fuels:
    Natural Gas

  Other Fossil Fuels:
    Municipal Wastes (non-biomass fraction), Industrial Wastes,
    Waste Oils, Peat

  Biomass:
    Biogasoline, Biodiesels, Other Liquid Biofuels

INPUT — HS6 codes to map (Chapter 27):

    270111  Anthracite coal
    270112  Bituminous coal
    270119  Other coal (incl. sub-bituminous)
    270120  Briquettes/ovoids from coal
    270210  Lignite, not agglomerated
    270220  Agglomerated lignite
    270300  Peat
    270400  Coke and semi-coke (coal, lignite, peat); retort carbon
    270500  Coal gas, water gas, producer gas, similar gases
    270600  Tar from coal, lignite, peat; other mineral tars
    270710  Benzol (benzene)
    270720  Toluol (toluene)
    270730  Xylol (xylenes)
    270740  Naphthalene
    270750  Other aromatic hydrocarbon mixtures
    270791  Creosote oils
    270799  Other coal-tar distillates
    270810  Pitch from coal tar or mineral tars
    270820  Pitch coke
    270900  Crude petroleum oils
    271012  Light petroleum oils (not crude)
    271019  Other petroleum oils (medium/heavy; not crude)
    271020  Petroleum oils containing biodiesel
    271091  Waste oils containing PCBs/PCTs/PBBs
    271099  Other waste oils
    271111  Liquefied natural gas (LNG)
    271112  Liquefied propane
    271113  Liquefied butanes
    271114  Ethylene, propylene, butylene, butadiene
    271119  Other liquefied petroleum gases
    271121  Natural gas in gaseous state
    271129  Other gaseous hydrocarbons
    271210  Petroleum jelly
    271220  Paraffin wax (< 0.75% oil)
    271290  Other mineral waxes
    271311  Petroleum coke, not calcined
    271312  Petroleum coke, calcined
    271320  Petroleum bitumen
    271390  Other residues of petroleum oils
    271410  Bituminous or oil-shale and tar sands
    271490  Natural bitumen, asphaltites, asphaltic rocks
    271500  Bituminous mixtures

INPUT — CN8 codes for headings where HS6 is too coarse:

  CN8 codes under 2704 (coke/semi-coke):
    27040010  Coke and semi-coke of coal
    27040030  Coke and semi-coke of lignite
    27040090  Other (incl. retort carbon)

  CN8 codes under 271012 (light oils, not crude):
    27101211  Light oils for specific process
    27101215  Light oils for chemical transformation
    27101221  White spirit
    27101225  Other special spirits
    27101231  Aviation spirit (avgas)
    27101241  Motor gasoline, RON < 95
    27101245  Motor gasoline, RON 95–98
    27101249  Motor gasoline, RON >= 98
    27101250  Motor gasoline, leaded
    27101270  Spirit type jet fuel
    27101290  Other light oils and preparations

  CN8 codes under 271019 (other petroleum oils):
    27101911  Medium oils for specific process
    27101915  Medium oils for chemical transformation
    27101921  Jet fuel (kerosene type)
    27101925  Other kerosene
    27101929  Other medium oils
    27101931  Gas oils for specific process
    27101935  Gas oils for chemical transformation
    27101943  Gas oils, sulphur <= 0.001%
    27101946  Gas oils, sulphur > 0.001% to 0.002%
    27101947  Gas oils, sulphur > 0.002% to 0.1%
    27101948  Gas oils, sulphur > 0.1%
    27101951  Fuel oils for specific process
    27101955  Fuel oils for chemical transformation
    27101962  Fuel oils, sulphur <= 0.1%
    27101966  Fuel oils, sulphur > 0.1% to 0.5%
    27101967  Fuel oils, sulphur > 0.5%
    27101971  Lubricating oils for specific process
    27101975  Lubricating oils for chemical transformation
    27101981  Motor/compressor/turbine lube oils
    27101983  Hydraulic oils
    27101985  White oils, liquid paraffin
    27101987  Gear oils and reductor oils
    27101991  Metal-working compounds, mould-release oils
    27101993  Electrical insulating oils
    27101999  Other lubricating oils

  CN8 codes under 271020 (biodiesel blends):
    27102011  Gas oils w/ biodiesel, sulphur <= 0.001%
    27102016  Gas oils w/ biodiesel, sulphur > 0.001% to 0.1%
    27102019  Gas oils w/ biodiesel, sulphur > 0.1%
    27102032  Fuel oils w/ biodiesel, sulphur <= 0.5%
    27102038  Fuel oils w/ biodiesel, sulphur > 0.5%
    27102090  Other petroleum oils w/ biodiesel

  CN8 codes under 2711 (petroleum gases):
    27111100  LNG
    27111211  Propane (>= 99%), for power/heating
    27111219  Propane (>= 99%), other purposes
    27111291  Propane (90-99%), for specific process
    27111293  Propane (90-99%), for chemical transformation
    27111294  Propane (90-99%), other purposes
    27111297  Other propane
    27111310  Butanes, for specific process
    27111330  Butanes, for chemical transformation
    27111391  Butanes (90-95%), other purposes
    27111397  Other butanes
    27111400  Ethylene, propylene, butylene, butadiene
    27111900  Other LPG
    27112100  Natural gas in gaseous state
    27112900  Other gaseous hydrocarbons

  CN8 codes under 2713 (petroleum coke, bitumen, residues):
    27131100  Petroleum coke, not calcined
    27131200  Petroleum coke, calcined
    27132000  Petroleum bitumen
    27139010  Residues for manufacture of heading 2803
    27139090  Other residues

INSTRUCTIONS:

Part A — HS6-level mapping:
  For each HS6 code, assign ONE IPCC fuel name as the best match.
  Columns:
    - hs6: character, 6-digit HS code
    - ipcc_fuel: character, IPCC fuel name (or NA_character_ if
      non-energy product with no combustion use)
    - energy_use: character, one of "core" (clearly a fuel),
      "borderline" (sometimes burned, sometimes feedstock/non-energy),
      or "non-energy" (not a fuel)
    - needs_cn8: logical, TRUE if HS6 is too coarse and CN8 codes are
      needed to properly split into IPCC categories
    - comment: character, brief note explaining the mapping

Part B — CN8-level overrides:
  For each CN8 code under headings flagged as needs_cn8 = TRUE above,
  assign the specific IPCC fuel. Columns:
    - cn8: character, 8-digit CN code
    - ipcc_fuel: character, IPCC fuel name
    - include: logical, TRUE if this CN8 code should be included in
      energy-use calculations (FALSE for lubricants, non-energy products)
    - comment: character, brief note

OUTPUT FORMAT:
  Return TWO R tribble() objects:
  1. hs6_to_ipcc_fuel_categories (Part A)
  2. cn8_to_ipcc_fuel_categories (Part B — bind all CN8 tables together)
```

---

## Prompt 5 — HS to SIEC crosswalk (script 30)

**Corresponding script:** `preprocess/30_build_hs_to_siec_map.R`
**Output object:** `hs_to_siec_map` (columns: `hs_code`, `siec_code`, `siec_description`)

**Authoritative sources:**
- HS/CN codes: EU Combined Nomenclature 2022 / UN Harmonized System
- SIEC codes: Eurostat SIEC codelist (SIEC Version 1.0),
  https://dd.eionet.europa.eu/vocabulary/eurostat/siec/view
- Crosswalk reference: UN International Recommendations for Energy
  Statistics (IRES), 2018, Table 3.1

```
ROLE: You are an expert in international trade classification systems
(HS nomenclature) and energy statistics classifications (SIEC, as used
by Eurostat and the UN).

TASK: Build a many-to-many crosswalk between HS codes (at 4- or
6-digit level) and SIEC v1.0 fuel categories, following the structure
of Table 3.1 in the UN International Recommendations for Energy
Statistics (IRES, 2018).

Some HS codes map to multiple SIEC categories (e.g. HS 2704 covers
coke oven coke, gas coke, coke breeze, and semi-cokes). Some SIEC
categories are covered by multiple HS codes.

REFERENCE — SIEC v1.0 codes (from Eurostat SIEC codelist):

  Coal and coal products:
    C0110   Anthracite
    C0121   Coking coal
    C0129   Other bituminous coal
    C0210   Sub-bituminous coal
    C0220   Lignite
    C0311   Coke oven coke
    C0312   Gas coke
    C0313   Coke breeze
    C0314   Semi-cokes
    C0320   Patent fuel
    C0330   Brown coal briquettes (BKB)
    C0340   Coal tar
    C0350   Coke oven gas
    C0360   Gas works gas
    C0371   Blast furnace gas
    C0372   Basic oxygen steel furnace gas
    C0379   Other recovered gases
    C0390   Other coal products

  Peat:
    P1100   Peat (sod peat)
    P1110   Sod peat
    P1120   Milled peat
    P1200   Peat products
    P1210   Peat briquettes
    P1290   Other peat products

  Oil shale:
    S2000   Oil shale / oil sands

  Natural gas:
    G3000   Natural gas

  Oil — crude:
    O4100_TOT   Crude oil (conventional)
    O4200       Natural gas liquids (NGL)
    O4400       Additives and oxygenates
    O4500       Other hydrocarbons

  Oil products:
    O4610   Refinery gas
    O4620   Ethane
    O4630   Liquefied petroleum gases (LPG)
    O4640   Naphtha
    O4651   Aviation gasoline
    O4652   Motor gasoline
    O4653   Gasoline-type jet fuel
    O4661   Kerosene-type jet fuel
    O4669   Other kerosene
    O4671   Gas oil and diesel oil
    O4672   Heavy gas oil
    O4680   Fuel oil
    O4691   White spirit and SBP
    O4692   Lubricants
    O4693   Paraffin waxes
    O4694   Petroleum coke
    O4695   Bitumen
    O4699   Other oil products n.e.c.

INPUT — HS codes to map (Chapter 27, at 4- or 6-digit level):

    270111  Coal; anthracite
    270112  Coal; bituminous
    270119  Coal; other (incl. sub-bituminous)
    270120  Briquettes/ovoids from coal
    270210  Lignite, not agglomerated
    270220  Lignite, agglomerated
    2703    Peat
    2704    Coke and semi-coke
    2705    Coal gas, water gas, producer gas
    2706    Tar from coal, lignite, peat
    2707    Oils from coal tar distillation
    270810  Pitch from coal tar
    270820  Pitch coke
    2709    Crude petroleum oils
    271011  Light petroleum oils (HS 2002/07 edition)
    271012  Light petroleum oils (HS 2012+ edition, replaces 271011)
    271019  Other petroleum oils (medium/heavy)
    271020  Petroleum oils containing biodiesel
    271111  LNG
    271112  Liquefied propane
    271113  Liquefied butanes
    271114  Ethylene, propylene, butylene, butadiene
    271119  Other liquefied petroleum gases
    271121  Natural gas in gaseous state
    271129  Other gaseous hydrocarbons
    271210  Petroleum jelly
    271220  Paraffin wax
    271290  Other mineral waxes
    271311  Petroleum coke, not calcined
    271312  Petroleum coke, calcined
    271320  Petroleum bitumen
    271410  Bituminous or oil-shale and tar sands
    280410  Hydrogen (sometimes bundled with other hydrocarbons)
    220720  Denatured ethyl alcohol (additive/oxygenate)
    290511  Methanol (additive/oxygenate)
    290919  Other acyclic ethers (additive/oxygenate, e.g. MTBE)

MAPPING INSTRUCTIONS:
  - This is a many-to-many crosswalk: one HS code can map to multiple
    SIEC codes, and one SIEC code can map to multiple HS codes
  - Use SIEC codes and names exactly as listed above
  - For HS codes like 271011/271012 that cover multiple SIEC products
    (naphtha, gasoline, jet fuel, etc.), include all plausible mappings
  - For HS codes like 271019 that span kerosene, gas oil, fuel oil,
    lubricants, etc., include all plausible SIEC mappings
  - The reference for this crosswalk is IRES 2018 Table 3.1

OUTPUT FORMAT: Return an R tribble() with exactly three columns:
  - hs_code: character, the HS code (4 or 6 digits, as appropriate)
  - siec_code: character, the SIEC code
  - siec_description: character, the SIEC product name
```

---

## Summary

| # | Dataset | Script | Prompt above |
|---|---------|--------|-------------|
| 1 | CN8 fossil fuel codes | `preprocess/18_build_cn8_fossil_fuel_list.R` | Prompt 1 |
| 2 | NACE fuel supplier likelihood | `preprocess/11_fuel_supplier_likelihood_by_nace.R` | Prompt 2 |
| 3 | SIEC → IPCC mapping | `preprocess/13_build_siec_to_ipcc_mapping.R` | Prompt 3 |
| 4 | HS6/CN8 → IPCC mapping | `preprocess/19_build_hs_cn_to_ipcc_mapping.R` | Prompt 4 |
| 5 | HS → SIEC crosswalk | `preprocess/30_build_hs_to_siec_map.R` | Prompt 5 |
