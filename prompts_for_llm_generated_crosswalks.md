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
**Output object:** `cn8digit_codes_for_fossil_fuels` (columns: `cn_code`, `description`, `edge_case`)

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
  - Solid, liquid, and gaseous fossil fuels (including peat) that
    could be combusted for energy in stationary installations

Exclude codes for:
  - Electricity
  - Transport fuels
  - Products primarily used for non-energy purposes

Edge cases: If a product (e.g. white spirit, certain naphtha grades,
waste oils) is sometimes burned for energy in industrial settings but
primarily used as a solvent or feedstock, INCLUDE it but flag it as
an edge case.

OUTPUT FORMAT: Return an R tribble() with exactly three columns:
  - cn_code: character, the 8-digit CN code (e.g., "27011100")
  - description: character, the CN 2022 product description
    (use the descriptions provided above)
  - edge_case: logical, TRUE if the product is an edge case
    (borderline energy/non-energy use), FALSE otherwise

Group the codes by HS4 heading and include a brief R comment above
each group.

Do not include any codes outside Chapter 27.
```

---

## Prompt 2 — NACE fuel supplier likelihood (script 11)

**Corresponding script:** `preprocess/11_fuel_supplier_likelihood_by_nace.R`
**Output object:** `fuel_supplier_classification` (columns: `nace4d`, `likelihood`, `comment`)

**Authoritative source for NACE codes:** Regulation (EC) No 1893/2006
(NACE Rev. 2), as consolidated. Four-digit class codes retrieved from
the official Eurostat NACE Rev. 2 statistical classification
(KS-RA-07-015-EN).

```
ROLE: You are an expert in industrial classification systems (NACE
Rev. 2) and European energy markets, with deep knowledge of how
fossil fuels (coal, petroleum products, natural gas, LPG, petroleum
coke, etc.) flow through the economy from extraction to end use.

TASK: For each NACE Rev. 2 four-digit code listed below, classify it
by the likelihood that a firm registered under that code acts as a
fuel supplier — meaning it sells fossil fuels to other firms as part
of a business-to-business transaction.

COUNTRY CONTEXT: This classification will be applied to Belgian
firm-level data. Belgium has no domestic fossil fuel extraction, so
nearly all fossil fuels enter the country via imports (crude oil,
refined products, natural gas, coal). Key features of the Belgian
energy landscape:
  - Belgium is a major European gas transit hub (the Zeebrugge hub
    is one of Europe's key natural gas trading points)
  - Pipeline operators in Belgium (e.g. Fluxys) are closely
    integrated with gas trading and balancing
  - The port of Antwerp hosts one of Europe's largest petrochemical
    and refining clusters
  - Coal and solid fuels are imported via ports and distributed by
    specialised traders
Your classification should reflect the Belgian institutional context
where relevant.

The complete NACE Rev. 2 classification is listed below (all 615
four-digit classes across all 21 sections). Classify every code.

INPUT — Complete NACE Rev. 2 four-digit codes:

  Section A — AGRICULTURE, FORESTRY AND FISHING

    Division 01 — Crop and animal production, hunting and related service activities:
      0111  Growing of cereals (except rice), leguminous crops and oil seeds
      0112  Growing of rice
      0113  Growing of vegetables and melons, roots and tubers
      0114  Growing of sugar cane
      0115  Growing of tobacco
      0116  Growing of fibre crops
      0119  Growing of other non-perennial crops
      0121  Growing of grapes
      0122  Growing of tropical and subtropical fruits
      0123  Growing of citrus fruits
      0124  Growing of pome fruits and stone fruits
      0125  Growing of other tree and bush fruits and nuts
      0126  Growing of oleaginous fruits
      0127  Growing of beverage crops
      0128  Growing of spices, aromatic, drug and pharmaceutical crops
      0129  Growing of other perennial crops
      0130  Plant propagation
      0141  Raising of dairy cattle
      0142  Raising of other cattle and buffaloes
      0143  Raising of horses and other equines
      0144  Raising of camels and camelids
      0145  Raising of sheep and goats
      0146  Raising of swine/pigs
      0147  Raising of poultry
      0149  Raising of other animals
      0150  Mixed farming
      0161  Support activities for crop production
      0162  Support activities for animal production
      0163  Post-harvest crop activities
      0164  Seed processing for propagation
      0170  Hunting, trapping and related service activities

    Division 02 — Forestry and logging:
      0210  Silviculture and other forestry activities
      0220  Logging
      0230  Gathering of wild growing non-wood products
      0240  Support services to forestry

    Division 03 — Fishing and aquaculture:
      0311  Marine fishing
      0312  Freshwater fishing
      0321  Marine aquaculture
      0322  Freshwater aquaculture

  Section B — MINING AND QUARRYING

    Division 05 — Mining of coal and lignite:
      0510  Mining of hard coal
      0520  Mining of lignite

    Division 06 — Extraction of crude petroleum and natural gas:
      0610  Extraction of crude petroleum
      0620  Extraction of natural gas

    Division 07 — Mining of metal ores:
      0710  Mining of iron ores
      0721  Mining of uranium and thorium ores
      0729  Mining of other non-ferrous metal ores

    Division 08 — Other mining and quarrying:
      0811  Quarrying of ornamental and building stone, limestone, gypsum, chalk and slate
      0812  Operation of gravel and sand pits; mining of clays and kaolin
      0891  Mining of chemical and fertiliser minerals
      0892  Extraction of peat
      0893  Extraction of salt
      0899  Other mining and quarrying n.e.c.

    Division 09 — Mining support service activities:
      0910  Support activities for petroleum and natural gas extraction
      0990  Support activities for other mining and quarrying

  Section C — MANUFACTURING

    Division 10 — Manufacture of food products:
      1011  Processing and preserving of meat
      1012  Processing and preserving of poultry meat
      1013  Production of meat and poultry meat products
      1020  Processing and preserving of fish, crustaceans and molluscs
      1031  Processing and preserving of potatoes
      1032  Manufacture of fruit and vegetable juice
      1039  Other processing and preserving of fruit and vegetables
      1041  Manufacture of oils and fats
      1042  Manufacture of margarine and similar edible fats
      1051  Operation of dairies and cheese making
      1052  Manufacture of ice cream
      1061  Manufacture of grain mill products
      1062  Manufacture of starches and starch products
      1071  Manufacture of bread; manufacture of fresh pastry goods and cakes
      1072  Manufacture of rusks and biscuits; manufacture of preserved pastry goods and cakes
      1073  Manufacture of macaroni, noodles, couscous and similar farinaceous products
      1081  Manufacture of sugar
      1082  Manufacture of cocoa, chocolate and sugar confectionery
      1083  Processing of tea and coffee
      1084  Manufacture of condiments and seasonings
      1085  Manufacture of prepared meals and dishes
      1086  Manufacture of homogenised food preparations and dietetic food
      1089  Manufacture of other food products n.e.c.
      1091  Manufacture of prepared feeds for farm animals
      1092  Manufacture of prepared pet foods

    Division 11 — Manufacture of beverages:
      1101  Distilling, rectifying and blending of spirits
      1102  Manufacture of wine from grape
      1103  Manufacture of cider and other fruit wines
      1104  Manufacture of other non-distilled fermented beverages
      1105  Manufacture of beer
      1106  Manufacture of malt
      1107  Manufacture of soft drinks; production of mineral waters and other bottled waters

    Division 12 — Manufacture of tobacco products:
      1200  Manufacture of tobacco products

    Division 13 — Manufacture of textiles:
      1310  Preparation and spinning of textile fibres
      1320  Weaving of textiles
      1330  Finishing of textiles
      1391  Manufacture of knitted and crocheted fabrics
      1392  Manufacture of made-up textile articles, except apparel
      1393  Manufacture of carpets and rugs
      1394  Manufacture of cordage, rope, twine and netting
      1395  Manufacture of non-wovens and articles made from non-wovens, except apparel
      1396  Manufacture of other technical and industrial textiles
      1399  Manufacture of other textiles n.e.c.

    Division 14 — Manufacture of wearing apparel:
      1411  Manufacture of leather clothes
      1412  Manufacture of workwear
      1413  Manufacture of other outerwear
      1414  Manufacture of underwear
      1419  Manufacture of other wearing apparel and accessories
      1420  Manufacture of articles of fur
      1431  Manufacture of knitted and crocheted hosiery
      1439  Manufacture of other knitted and crocheted apparel

    Division 15 — Manufacture of leather and related products:
      1511  Tanning and dressing of leather; dressing and dyeing of fur
      1512  Manufacture of luggage, handbags and the like, saddlery and harness
      1520  Manufacture of footwear

    Division 16 — Manufacture of wood and of products of wood and cork, except furniture:
      1610  Sawmilling and planing of wood
      1621  Manufacture of veneer sheets and wood-based panels
      1622  Manufacture of assembled parquet floors
      1623  Manufacture of other builders' carpentry and joinery
      1624  Manufacture of wooden containers
      1629  Manufacture of other products of wood; manufacture of articles of cork, straw and plaiting materials

    Division 17 — Manufacture of paper and paper products:
      1711  Manufacture of pulp
      1712  Manufacture of paper and paperboard
      1721  Manufacture of corrugated paper and paperboard and of containers of paper and paperboard
      1722  Manufacture of household and sanitary goods and of toilet requisites
      1723  Manufacture of paper stationery
      1724  Manufacture of wallpaper
      1729  Manufacture of other articles of paper and paperboard

    Division 18 — Printing and reproduction of recorded media:
      1811  Printing of newspapers
      1812  Other printing
      1813  Pre-press and pre-media services
      1814  Binding and related services
      1820  Reproduction of recorded media

    Division 19 — Manufacture of coke and refined petroleum products:
      1910  Manufacture of coke oven products
      1920  Manufacture of refined petroleum products

    Division 20 — Manufacture of chemicals and chemical products:
      2011  Manufacture of industrial gases
      2012  Manufacture of dyes and pigments
      2013  Manufacture of other inorganic basic chemicals
      2014  Manufacture of other organic basic chemicals
      2015  Manufacture of fertilisers and nitrogen compounds
      2016  Manufacture of plastics in primary forms
      2017  Manufacture of synthetic rubber in primary forms
      2020  Manufacture of pesticides and other agrochemical products
      2030  Manufacture of paints, varnishes and similar coatings, printing ink and mastics
      2041  Manufacture of soap and detergents, cleaning and polishing preparations
      2042  Manufacture of perfumes and toilet preparations
      2051  Manufacture of explosives
      2052  Manufacture of glues
      2053  Manufacture of essential oils
      2059  Manufacture of other chemical products n.e.c.
      2060  Manufacture of man-made fibres

    Division 21 — Manufacture of basic pharmaceutical products and pharmaceutical preparations:
      2110  Manufacture of basic pharmaceutical products
      2120  Manufacture of pharmaceutical preparations

    Division 22 — Manufacture of rubber and plastic products:
      2211  Manufacture of rubber tyres and tubes; retreading and rebuilding of rubber tyres
      2219  Manufacture of other rubber products
      2221  Manufacture of plastic plates, sheets, tubes and profiles
      2222  Manufacture of plastic packing goods
      2223  Manufacture of builders' ware of plastic
      2229  Manufacture of other plastic products

    Division 23 — Manufacture of other non-metallic mineral products:
      2311  Manufacture of flat glass
      2312  Shaping and processing of flat glass
      2313  Manufacture of hollow glass
      2314  Manufacture of glass fibres
      2319  Manufacture and processing of other glass, including technical glassware
      2320  Manufacture of refractory products
      2331  Manufacture of ceramic tiles and flags
      2332  Manufacture of bricks, tiles and construction products, in baked clay
      2341  Manufacture of ceramic household and ornamental articles
      2342  Manufacture of ceramic sanitary fixtures
      2343  Manufacture of ceramic insulators and insulating fittings
      2344  Manufacture of other technical ceramic products
      2349  Manufacture of other ceramic products
      2351  Manufacture of cement
      2352  Manufacture of lime and plaster
      2361  Manufacture of concrete products for construction purposes
      2362  Manufacture of plaster products for construction purposes
      2363  Manufacture of ready-mixed concrete
      2364  Manufacture of mortars
      2365  Manufacture of fibre cement
      2369  Manufacture of other articles of concrete, plaster and cement
      2370  Cutting, shaping and finishing of stone
      2391  Production of abrasive products
      2399  Manufacture of other non-metallic mineral products n.e.c.

    Division 24 — Manufacture of basic metals:
      2410  Manufacture of basic iron and steel and of ferro-alloys
      2420  Manufacture of tubes, pipes, hollow profiles and related fittings, of steel
      2431  Cold drawing of bars
      2432  Cold rolling of narrow strip
      2433  Cold forming or folding
      2434  Cold drawing of wire
      2441  Precious metals production
      2442  Aluminium production
      2443  Lead, zinc and tin production
      2444  Copper production
      2445  Other non-ferrous metal production
      2446  Processing of nuclear fuel
      2451  Casting of iron
      2452  Casting of steel
      2453  Casting of light metals
      2454  Casting of other non-ferrous metals

    Division 25 — Manufacture of fabricated metal products, except machinery and equipment:
      2511  Manufacture of metal structures and parts of structures
      2512  Manufacture of doors and windows of metal
      2521  Manufacture of central heating radiators and boilers
      2529  Manufacture of other tanks, reservoirs and containers of metal
      2530  Manufacture of steam generators, except central heating hot water boilers
      2540  Manufacture of weapons and ammunition
      2550  Forging, pressing, stamping and roll-forming of metal; powder metallurgy
      2561  Treatment and coating of metals
      2562  Machining
      2571  Manufacture of cutlery
      2572  Manufacture of locks and hinges
      2573  Manufacture of tools
      2591  Manufacture of steel drums and similar containers
      2592  Manufacture of light metal packaging
      2593  Manufacture of wire products, chain and springs
      2594  Manufacture of fasteners and screw machine products
      2599  Manufacture of other fabricated metal products n.e.c.

    Division 26 — Manufacture of computer, electronic and optical products:
      2611  Manufacture of electronic components
      2612  Manufacture of loaded electronic boards
      2620  Manufacture of computers and peripheral equipment
      2630  Manufacture of communication equipment
      2640  Manufacture of consumer electronics
      2651  Manufacture of instruments and appliances for measuring, testing and navigation
      2652  Manufacture of watches and clocks
      2660  Manufacture of irradiation, electromedical and electrotherapeutic equipment
      2670  Manufacture of optical instruments and photographic equipment
      2680  Manufacture of magnetic and optical media

    Division 27 — Manufacture of electrical equipment:
      2711  Manufacture of electric motors, generators and transformers
      2712  Manufacture of electricity distribution and control apparatus
      2720  Manufacture of batteries and accumulators
      2731  Manufacture of fibre optic cables
      2732  Manufacture of other electronic and electric wires and cables
      2733  Manufacture of wiring devices
      2740  Manufacture of electric lighting equipment
      2751  Manufacture of electric domestic appliances
      2752  Manufacture of non-electric domestic appliances
      2790  Manufacture of other electrical equipment

    Division 28 — Manufacture of machinery and equipment n.e.c.:
      2811  Manufacture of engines and turbines, except aircraft, vehicle and cycle engines
      2812  Manufacture of fluid power equipment
      2813  Manufacture of other pumps and compressors
      2814  Manufacture of other taps and valves
      2815  Manufacture of bearings, gears, gearing and driving elements
      2821  Manufacture of ovens, furnaces and furnace burners
      2822  Manufacture of lifting and handling equipment
      2823  Manufacture of office machinery and equipment (except computers and peripheral equipment)
      2824  Manufacture of power-driven hand tools
      2825  Manufacture of non-domestic cooling and ventilation equipment
      2829  Manufacture of other general-purpose machinery n.e.c.
      2830  Manufacture of agricultural and forestry machinery
      2841  Manufacture of metal forming machinery
      2849  Manufacture of other machine tools
      2891  Manufacture of machinery for metallurgy
      2892  Manufacture of machinery for mining, quarrying and construction
      2893  Manufacture of machinery for food, beverage and tobacco processing
      2894  Manufacture of machinery for textile, apparel and leather production
      2895  Manufacture of machinery for paper and paperboard production
      2896  Manufacture of plastics and rubber machinery
      2899  Manufacture of other special-purpose machinery n.e.c.

    Division 29 — Manufacture of motor vehicles, trailers and semi-trailers:
      2910  Manufacture of motor vehicles
      2920  Manufacture of bodies (coachwork) for motor vehicles; manufacture of trailers and semi-trailers
      2931  Manufacture of electrical and electronic equipment for motor vehicles
      2932  Manufacture of other parts and accessories for motor vehicles

    Division 30 — Manufacture of other transport equipment:
      3011  Building of ships and floating structures
      3012  Building of pleasure and sporting boats
      3020  Manufacture of railway locomotives and rolling stock
      3030  Manufacture of air and spacecraft and related machinery
      3040  Manufacture of military fighting vehicles
      3091  Manufacture of motorcycles
      3092  Manufacture of bicycles and invalid carriages
      3099  Manufacture of other transport equipment n.e.c.

    Division 31 — Manufacture of furniture:
      3101  Manufacture of office and shop furniture
      3102  Manufacture of kitchen furniture
      3103  Manufacture of mattresses
      3109  Manufacture of other furniture

    Division 32 — Other manufacturing:
      3211  Striking of coins
      3212  Manufacture of jewellery and related articles
      3213  Manufacture of imitation jewellery and related articles
      3220  Manufacture of musical instruments
      3230  Manufacture of sports goods
      3240  Manufacture of games and toys
      3250  Manufacture of medical and dental instruments and supplies
      3291  Manufacture of brooms and brushes
      3299  Other manufacturing n.e.c.

    Division 33 — Repair and installation of machinery and equipment:
      3311  Repair of fabricated metal products
      3312  Repair of machinery
      3313  Repair of electronic and optical equipment
      3314  Repair of electrical equipment
      3315  Repair and maintenance of ships and boats
      3316  Repair and maintenance of aircraft and spacecraft
      3317  Repair and maintenance of other transport equipment
      3319  Repair of other equipment
      3320  Installation of industrial machinery and equipment

  Section D — ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY

    Division 35 — Electricity, gas, steam and air conditioning supply:
      3511  Production of electricity
      3512  Transmission of electricity
      3513  Distribution of electricity
      3514  Trade of electricity
      3521  Manufacture of gas
      3522  Distribution of gaseous fuels through mains
      3523  Trade of gas through mains
      3530  Steam and air conditioning supply

  Section E — WATER SUPPLY; SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES

    Division 36 — Water collection, treatment and supply:
      3600  Water collection, treatment and supply

    Division 37 — Sewerage:
      3700  Sewerage

    Division 38 — Waste collection, treatment and disposal activities; materials recovery:
      3811  Collection of non-hazardous waste
      3812  Collection of hazardous waste
      3821  Treatment and disposal of non-hazardous waste
      3822  Treatment and disposal of hazardous waste
      3831  Dismantling of wrecks
      3832  Recovery of sorted materials

    Division 39 — Remediation activities and other waste management services:
      3900  Remediation activities and other waste management services

  Section F — CONSTRUCTION

    Division 41 — Construction of buildings:
      4110  Development of building projects
      4120  Construction of residential and non-residential buildings

    Division 42 — Civil engineering:
      4211  Construction of roads and motorways
      4212  Construction of railways and underground railways
      4213  Construction of bridges and tunnels
      4221  Construction of utility projects for fluids
      4222  Construction of utility projects for electricity and telecommunications
      4291  Construction of water projects
      4299  Construction of other civil engineering projects n.e.c.

    Division 43 — Specialised construction activities:
      4311  Demolition
      4312  Site preparation
      4313  Test drilling and boring
      4321  Electrical installation
      4322  Plumbing, heat and air-conditioning installation
      4329  Other construction installation
      4331  Plastering
      4332  Joinery installation
      4333  Floor and wall covering
      4334  Painting and glazing
      4339  Other building completion and finishing
      4391  Roofing activities
      4399  Other specialised construction activities n.e.c.

  Section G — WHOLESALE AND RETAIL TRADE; REPAIR OF MOTOR VEHICLES AND MOTORCYCLES

    Division 45 — Wholesale and retail trade and repair of motor vehicles and motorcycles:
      4511  Sale of cars and light motor vehicles
      4519  Sale of other motor vehicles
      4520  Maintenance and repair of motor vehicles
      4531  Wholesale trade of motor vehicle parts and accessories
      4532  Retail trade of motor vehicle parts and accessories
      4540  Sale, maintenance and repair of motorcycles and related parts and accessories

    Division 46 — Wholesale trade, except of motor vehicles and motorcycles:
      4611  Agents involved in the sale of agricultural raw materials, live animals, textile raw materials and semi-finished goods
      4612  Agents involved in the sale of fuels, ores, metals and industrial chemicals
      4613  Agents involved in the sale of timber and building materials
      4614  Agents involved in the sale of machinery, industrial equipment, ships and aircraft
      4615  Agents involved in the sale of furniture, household goods, hardware and ironmongery
      4616  Agents involved in the sale of textiles, clothing, fur, footwear and leather goods
      4617  Agents involved in the sale of food, beverages and tobacco
      4618  Agents specialised in the sale of other particular products
      4619  Agents involved in the sale of a variety of goods
      4621  Wholesale of grain, unmanufactured tobacco, seeds and animal feeds
      4622  Wholesale of flowers and plants
      4623  Wholesale of live animals
      4624  Wholesale of hides, skins and leather
      4631  Wholesale of fruit and vegetables
      4632  Wholesale of meat and meat products
      4633  Wholesale of dairy products, eggs and edible oils and fats
      4634  Wholesale of beverages
      4635  Wholesale of tobacco products
      4636  Wholesale of sugar and chocolate and sugar confectionery
      4637  Wholesale of coffee, tea, cocoa and spices
      4638  Wholesale of other food, including fish, crustaceans and molluscs
      4639  Non-specialised wholesale of food, beverages and tobacco
      4641  Wholesale of textiles
      4642  Wholesale of clothing and footwear
      4643  Wholesale of electrical household appliances
      4644  Wholesale of china and glassware and cleaning materials
      4645  Wholesale of perfume and cosmetics
      4646  Wholesale of pharmaceutical goods
      4647  Wholesale of furniture, carpets and lighting equipment
      4648  Wholesale of watches and jewellery
      4649  Wholesale of other household goods
      4651  Wholesale of computers, computer peripheral equipment and software
      4652  Wholesale of electronic and telecommunications equipment and parts
      4661  Wholesale of agricultural machinery, equipment and supplies
      4662  Wholesale of machine tools
      4663  Wholesale of mining, construction and civil engineering machinery
      4664  Wholesale of machinery for the textile industry and of sewing and knitting machines
      4665  Wholesale of office furniture
      4666  Wholesale of other office machinery and equipment
      4669  Wholesale of other machinery and equipment
      4671  Wholesale of solid, liquid and gaseous fuels and related products
      4672  Wholesale of metals and metal ores
      4673  Wholesale of wood, construction materials and sanitary equipment
      4674  Wholesale of hardware, plumbing and heating equipment and supplies
      4675  Wholesale of chemical products
      4676  Wholesale of other intermediate products
      4677  Wholesale of waste and scrap
      4690  Non-specialised wholesale trade

    Division 47 — Retail trade, except of motor vehicles and motorcycles:
      4711  Retail sale in non-specialised stores with food, beverages or tobacco predominating
      4719  Other retail sale in non-specialised stores
      4721  Retail sale of fruit and vegetables in specialised stores
      4722  Retail sale of meat and meat products in specialised stores
      4723  Retail sale of fish, crustaceans and molluscs in specialised stores
      4724  Retail sale of bread, cakes, flour confectionery and sugar confectionery in specialised stores
      4725  Retail sale of beverages in specialised stores
      4726  Retail sale of tobacco products in specialised stores
      4729  Other retail sale of food in specialised stores
      4730  Retail sale of automotive fuel in specialised stores
      4741  Retail sale of computers, peripheral units and software in specialised stores
      4742  Retail sale of telecommunications equipment in specialised stores
      4743  Retail sale of audio and video equipment in specialised stores
      4751  Retail sale of textiles in specialised stores
      4752  Retail sale of hardware, paints and glass in specialised stores
      4753  Retail sale of carpets, rugs, wall and floor coverings in specialised stores
      4754  Retail sale of electrical household appliances in specialised stores
      4759  Retail sale of furniture, lighting equipment and other household articles in specialised stores
      4761  Retail sale of books in specialised stores
      4762  Retail sale of newspapers and stationery in specialised stores
      4763  Retail sale of music and video recordings in specialised stores
      4764  Retail sale of sporting equipment in specialised stores
      4765  Retail sale of games and toys in specialised stores
      4771  Retail sale of clothing in specialised stores
      4772  Retail sale of footwear and leather goods in specialised stores
      4773  Dispensing chemist in specialised stores
      4774  Retail sale of medical and orthopaedic goods in specialised stores
      4775  Retail sale of cosmetic and toilet articles in specialised stores
      4776  Retail sale of flowers, plants, seeds, fertilisers, pet animals and pet food in specialised stores
      4777  Retail sale of watches and jewellery in specialised stores
      4778  Other retail sale of new goods in specialised stores
      4779  Retail sale of second-hand goods in stores
      4781  Retail sale via stalls and markets of food, beverages and tobacco products
      4782  Retail sale via stalls and markets of textiles, clothing and footwear
      4789  Retail sale via stalls and markets of other goods
      4791  Retail sale via mail order houses or via Internet
      4799  Other retail sale not in stores, stalls or markets

  Section H — TRANSPORTATION AND STORAGE

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

  Section I — ACCOMMODATION AND FOOD SERVICE ACTIVITIES

    Division 55 — Accommodation:
      5510  Hotels and similar accommodation
      5520  Holiday and other short-stay accommodation
      5530  Camping grounds, recreational vehicle parks and trailer parks
      5590  Other accommodation

    Division 56 — Food and beverage service activities:
      5610  Restaurants and mobile food service activities
      5621  Event catering activities
      5629  Other food service activities
      5630  Beverage serving activities

  Section J — INFORMATION AND COMMUNICATION

    Division 58 — Publishing activities:
      5811  Book publishing
      5812  Publishing of directories and mailing lists
      5813  Publishing of newspapers
      5814  Publishing of journals and periodicals
      5819  Other publishing activities
      5821  Publishing of computer games
      5829  Other software publishing

    Division 59 — Motion picture, video and television programme production, sound recording and music publishing activities:
      5911  Motion picture, video and television programme production activities
      5912  Motion picture, video and television programme post-production activities
      5913  Motion picture, video and television programme distribution activities
      5914  Motion picture projection activities
      5920  Sound recording and music publishing activities

    Division 60 — Programming and broadcasting activities:
      6010  Radio broadcasting
      6020  Television programming and broadcasting activities

    Division 61 — Telecommunications:
      6110  Wired telecommunications activities
      6120  Wireless telecommunications activities
      6130  Satellite telecommunications activities
      6190  Other telecommunications activities

    Division 62 — Computer programming, consultancy and related activities:
      6201  Computer programming activities
      6202  Computer consultancy activities
      6203  Computer facilities management activities
      6209  Other information technology and computer service activities

    Division 63 — Information service activities:
      6311  Data processing, hosting and related activities
      6312  Web portals
      6391  News agency activities
      6399  Other information service activities n.e.c.

  Section K — FINANCIAL AND INSURANCE ACTIVITIES

    Division 64 — Financial service activities, except insurance and pension funding:
      6411  Central banking
      6419  Other monetary intermediation
      6420  Activities of holding companies
      6430  Trusts, funds and similar financial entities
      6491  Financial leasing
      6492  Other credit granting
      6499  Other financial service activities, except insurance and pension funding n.e.c.

    Division 65 — Insurance, reinsurance and pension funding, except compulsory social security:
      6511  Life insurance
      6512  Non-life insurance
      6520  Reinsurance
      6530  Pension funding

    Division 66 — Activities auxiliary to financial services and insurance activities:
      6611  Administration of financial markets
      6612  Security and commodity contracts brokerage
      6619  Other activities auxiliary to financial services, except insurance and pension funding
      6621  Risk and damage evaluation
      6622  Activities of insurance agents and brokers
      6629  Other activities auxiliary to insurance and pension funding
      6630  Fund management activities

  Section L — REAL ESTATE ACTIVITIES

    Division 68 — Real estate activities:
      6810  Buying and selling of own real estate
      6820  Rental and operating of own or leased real estate
      6831  Real estate agencies
      6832  Management of real estate on a fee or contract basis

  Section M — PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES

    Division 69 — Legal and accounting activities:
      6910  Legal activities
      6920  Accounting, bookkeeping and auditing activities; tax consultancy

    Division 70 — Activities of head offices; management consultancy activities:
      7010  Activities of head offices
      7021  Public relations and communication activities
      7022  Business and other management consultancy activities

    Division 71 — Architectural and engineering activities; technical testing and analysis:
      7111  Architectural activities
      7112  Engineering activities and related technical consultancy
      7120  Technical testing and analysis

    Division 72 — Scientific research and development:
      7211  Research and experimental development on biotechnology
      7219  Other research and experimental development on natural sciences and engineering
      7220  Research and experimental development on social sciences and humanities

    Division 73 — Advertising and market research:
      7311  Advertising agencies
      7312  Media representation
      7320  Market research and public opinion polling

    Division 74 — Other professional, scientific and technical activities:
      7410  Specialised design activities
      7420  Photographic activities
      7430  Translation and interpretation activities
      7490  Other professional, scientific and technical activities n.e.c.

    Division 75 — Veterinary activities:
      7500  Veterinary activities

  Section N — ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES

    Division 77 — Rental and leasing activities:
      7711  Rental and leasing of cars and light motor vehicles
      7712  Rental and leasing of trucks
      7721  Rental and leasing of recreational and sports goods
      7722  Rental of video tapes and disks
      7729  Rental and leasing of other personal and household goods
      7731  Rental and leasing of agricultural machinery and equipment
      7732  Rental and leasing of construction and civil engineering machinery and equipment
      7733  Rental and leasing of office machinery and equipment (including computers)
      7734  Rental and leasing of water transport equipment
      7735  Rental and leasing of air transport equipment
      7739  Rental and leasing of other machinery, equipment and tangible goods n.e.c.
      7740  Leasing of intellectual property and similar products, except copyrighted works

    Division 78 — Employment activities:
      7810  Activities of employment placement agencies
      7820  Temporary employment agency activities
      7830  Other human resources provision

    Division 79 — Travel agency, tour operator and other reservation service and related activities:
      7911  Travel agency activities
      7912  Tour operator activities
      7990  Other reservation service and related activities

    Division 80 — Security and investigation activities:
      8010  Private security activities
      8020  Security systems service activities
      8030  Investigation activities

    Division 81 — Services to buildings and landscape activities:
      8110  Combined facilities support activities
      8121  General cleaning of buildings
      8122  Other building and industrial cleaning activities
      8129  Other cleaning activities
      8130  Landscape service activities

    Division 82 — Office administrative, office support and other business support activities:
      8211  Combined office administrative service activities
      8219  Photocopying, document preparation and other specialised office support activities
      8220  Activities of call centres
      8230  Organisation of conventions and trade shows
      8291  Activities of collection agencies and credit bureaus
      8292  Packaging activities
      8299  Other business support service activities n.e.c.

  Section O — PUBLIC ADMINISTRATION AND DEFENCE; COMPULSORY SOCIAL SECURITY

    Division 84 — Public administration and defence; compulsory social security:
      8411  General public administration activities
      8412  Regulation of the activities of providing health care, education, cultural services and other social services, excluding social security
      8413  Regulation of and contribution to more efficient operation of businesses
      8421  Foreign affairs
      8422  Defence activities
      8423  Justice and judicial activities
      8424  Public order and safety activities
      8425  Fire service activities
      8430  Compulsory social security activities

  Section P — EDUCATION

    Division 85 — Education:
      8510  Pre-primary education
      8520  Primary education
      8531  General secondary education
      8532  Technical and vocational secondary education
      8541  Post-secondary non-tertiary education
      8542  Tertiary education
      8551  Sports and recreation education
      8552  Cultural education
      8553  Driving school activities
      8559  Other education n.e.c.
      8560  Educational support activities

  Section Q — HUMAN HEALTH AND SOCIAL WORK ACTIVITIES

    Division 86 — Human health activities:
      8610  Hospital activities
      8621  General medical practice activities
      8622  Specialist medical practice activities
      8623  Dental practice activities
      8690  Other human health activities

    Division 87 — Residential care activities:
      8710  Residential nursing care activities
      8720  Residential care activities for mental retardation, mental health and substance abuse
      8730  Residential care activities for the elderly and disabled
      8790  Other residential care activities

    Division 88 — Social work activities without accommodation:
      8810  Social work activities without accommodation for the elderly and disabled
      8891  Child day-care activities
      8899  Other social work activities without accommodation n.e.c.

  Section R — ARTS, ENTERTAINMENT AND RECREATION

    Division 90 — Creative, arts and entertainment activities:
      9001  Performing arts
      9002  Support activities to performing arts
      9003  Artistic creation
      9004  Operation of arts facilities

    Division 91 — Libraries, archives, museums and other cultural activities:
      9101  Library and archives activities
      9102  Museums activities
      9103  Operation of historical sites and buildings and similar visitor attractions
      9104  Botanical and zoological gardens and nature reserves activities

    Division 92 — Gambling and betting activities:
      9200  Gambling and betting activities

    Division 93 — Sports activities and amusement and recreation activities:
      9311  Operation of sports facilities
      9312  Activities of sports clubs
      9313  Fitness facilities
      9319  Other sports activities
      9321  Activities of amusement parks and theme parks
      9329  Other amusement and recreation activities

  Section S — OTHER SERVICE ACTIVITIES

    Division 94 — Activities of membership organisations:
      9411  Activities of business and employers membership organisations
      9412  Activities of professional membership organisations
      9420  Activities of trade unions
      9491  Activities of religious organisations
      9492  Activities of political organisations
      9499  Activities of other membership organisations n.e.c.

    Division 95 — Repair of computers and personal and household goods:
      9511  Repair of computers and peripheral equipment
      9512  Repair of communication equipment
      9521  Repair of consumer electronics
      9522  Repair of household appliances and home and garden equipment
      9523  Repair of footwear and leather goods
      9524  Repair of furniture and home furnishings
      9525  Repair of watches, clocks and jewellery
      9529  Repair of other personal and household goods

    Division 96 — Other personal service activities:
      9601  Washing and (dry-)cleaning of textile and fur products
      9602  Hairdressing and other beauty treatment
      9603  Funeral and related activities
      9604  Physical well-being activities
      9609  Other personal service activities n.e.c.

  Section T — ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS

    Division 97 — Activities of households as employers of domestic personnel:
      9700  Activities of households as employers of domestic personnel

    Division 98 — Undifferentiated goods- and services-producing activities of private households for own use:
      9810  Undifferentiated goods-producing activities of private households for own use
      9820  Undifferentiated service-producing activities of private households for own use

  Section U — ACTIVITIES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES

    Division 99 — Activities of extraterritorial organisations and bodies:
      9900  Activities of extraterritorial organisations and bodies

CLASSIFICATION CRITERIA:

  The classification reflects whether a firm in this sector plausibly
  SELLS fossil fuels to other firms in business-to-business (B2B)
  transactions, not whether it consumes fossil fuels as an input.

  "Fossil fuels" means: coal, lignite, peat, coke, crude oil, refined
  petroleum products (gasoline, diesel, fuel oil, kerosene, LPG,
  naphtha, petroleum coke, etc.), natural gas, and manufactured gases.

  HIGH likelihood: The sector's defining economic activity involves
    purchasing fossil fuels from upstream sources and reselling them
    to downstream customers, or the sector produces fossil fuels as
    its primary output. A firm in this sector would routinely appear
    as the seller in B2B transactions where the product is fuel.

  MEDIUM likelihood: The sector does not primarily exist to sell fuel,
    but firms in this sector could plausibly appear as a fuel seller
    in B2B data — either because fuel sales are a common secondary
    activity, or because the sector acts as an intermediary or
    broker through which fuel transactions may pass.

  LOW likelihood: Firms in this sector do not typically sell fossil
    fuels to other firms, even if the sector is a significant
    consumer of fuel.

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
    271091  Waste oils containing PCBs/PCTs/PBBs
    271099  Other waste oils
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
    271390  Other residues of petroleum oils
    271410  Bituminous or oil-shale and tar sands
    271490  Natural bitumen, asphaltites, asphaltic rocks
    271500  Bituminous mixtures
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
