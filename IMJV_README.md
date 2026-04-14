# IMJV to EUTL Matching Procedure

## What is IMJV?

IMJV (Integraal Milieujaarverslag) is Flanders' mandatory environmental reporting system. Firms in Flanders consuming more than 0.1 PJ of primary energy per year must report annual emissions. The data covers 178 unique firms with CO2 emissions over 2004–2023.

IMJV identifies firms by CBB numbers (Centraal Bedrijvenbestand, the Flemish environmental agency's own registry) and firm names, not by the anonymized VAT codes used in NBB data. See the "CBB number structure" section below for details on how CBB numbers relate to KBO/CBE enterprise numbers.

## CBB number structure and matching to KBO/CBE

### What is a CBB number?

CBB stands for **Centraal Bedrijvenbestand** (Central Business Register), maintained by the Flemish Department of Environment (Departement Omgeving). Each Flemish firm in the environmental reporting system receives a 14-digit CBB number (e.g., `01853961014649`). The CBB number encodes two levels:

- **Exploitant** (operator/firm): the first 8 digits + `000000` suffix (e.g., `01853961000000` for Electrabel)
- **Exploitatie** (operation/site): the full 14 digits, identifying a specific site of that firm

### CBB numbers are NOT KBO/CBE numbers

The 14-digit CBB number is an entirely separate identifier from the KBO system. It does **not** contain the KBO enterprise number (`ondernemingsnummer`, 10 digits, format `9999.999.999`) or the KBO establishment number (`vestigingseenheidsnummer`, 10 digits, format `9.999.999.999`) as a substring. No arithmetic or positional transformation converts one to the other.

This was verified empirically: for 136 IMJV firms matched to EUTL installations (with known BvD IDs / enterprise numbers), no 10-character window of the 14-digit CBB matched either the enterprise number or the CBE establishment number.

### How to build the crosswalk

The Flemish government's **CBB Linked Data portal** exposes a SPARQL endpoint that provides the mapping:

- **Portal**: https://data.cbb.omgeving.vlaanderen.be/
- **SPARQL endpoint**: https://data.cbb.omgeving.vlaanderen.be/sparql

The data model links:
1. Each `exploitatie` (14-digit CBB) → `org:siteOf` → parent `exploitant`
2. Each `exploitant` → `org:identifier` (with datatype `imjv#KBONummer`) → **KBO enterprise number**

Example SPARQL query for a single CBB number:
```sparql
SELECT ?kbo ?label WHERE {
  ?s <http://purl.org/dc/elements/1.1/identifier>
     "01853961014649"^^<http://www.w3.org/2001/XMLSchema#string> .
  ?s <http://www.w3.org/ns/org#siteOf> ?parent .
  ?parent <http://www.w3.org/ns/org#identifier> ?kbo .
  OPTIONAL { ?parent <http://www.w3.org/2000/01/rdf-schema#label> ?label }
}
```
Returns: `kbo = "0403.170.701"`, `label = "ELECTRABEL"`.

Batch queries using `VALUES` clauses work (with `^^xsd:string` typed literals). All 178 IMJV CBB numbers were successfully matched to KBO enterprise numbers via this method. The crosswalk is saved in `{PROC_DATA}/imjv_cbb_to_kbo.csv`. The script that built it is `analysis/scratch/imjv_cbb_to_kbo_sparql.R`.

### From KBO enterprise number to anonymized VAT

Once you have the KBO enterprise number (e.g., `0403.170.701`), the Belgian VAT number is `BE` + enterprise number without dots (e.g., `BE0403170701`). The BvD ID for Belgian firms follows the same format. Mapping from VAT/BvD to anonymized VAT codes requires the NBB crosswalk available on RMD.

## Why match IMJV to EUTL?

To assess the marginal value of IMJV — how many firms with real, reported emissions it adds beyond what EU ETS already covers. Unlike Climate TRACE (which assigns uniform top-down allocations to non-ETS facilities), IMJV provides actual facility-level reported emissions.

## Matching procedure

### Step 1: Algorithmic name-based matching

For each IMJV firm, we search all Belgian EUTL installations and compute:

**Name similarity** (same functions as CT-to-EUTL matching, see analysis/README.md):
- Bigram Jaccard index
- Token overlap score (with stopword removal)
- Combined: S_name = max(bigram Jaccard, token overlap)

**Substring check**: whether one firm name (stripped to lowercase alphanumeric) is contained within the other. Catches cases like TEEPAK inside "ViskoTeepak".

**City match**: whether the IMJV gemeente matches the EUTL city field (exact or substring).

**NACE match**: at 2-digit or 4-digit level.

Auto-accept tiers (in priority order):

| Tier | Name sim | City | NACE 2d | Label |
|------|----------|------|---------|-------|
| 1 | >= 0.4 | any | Yes | name_nace |
| 2 | >= 0.4 | any | any | name |
| 3 | >= 0.2 | Yes | Yes | name_city_nace |
| 4 | >= 0.2 | Yes | any | name_city |
| 5 | substring | Yes | Yes | substr_city_nace |

### Step 2: Flagging ambiguous cases

Unmatched IMJV firms that have an EUTL installation in the same city with the same NACE code (but zero name similarity) are flagged for manual review. These could be:
- Corporate name changes (the same physical facility under a new company name)
- Genuinely different firms that happen to share a city and sector

We deliberately do NOT auto-accept city+NACE matches because of false positive risk, especially in cities with multiple firms in the same sector (e.g., Antwerp chemicals).

### Step 3: Manual verification of flagged cases

Flagged cases were investigated via web search to determine whether the IMJV firm and EUTL installation are the same physical facility. Confirmed matches are hard-coded in the script.

## Hard-coded matches (verified corporate name changes)

The following IMJV-to-EUTL links were verified as corporate name changes, mergers, or acquisitions:

| IMJV name | EUTL name | Explanation | Source |
|-----------|-----------|-------------|--------|
| BP CHEMBEL | INEOS Aromatics Belgium | BP sold its global petrochemicals business to INEOS in 2020. Same plant in Geel. | [BP press release](https://www.bp.com/en/global/corporate/news-and-insights/press-releases/bp-agrees-to-sell-its-petrochemicals-business-to-ineos.html) |
| OUDEGEM PAPIER | VPK Paper | VPK Group was founded as a paper factory in Oudegem in 1936. Same site at Oude Baan 120, Dendermonde. | [VPK Group Wikipedia](https://en.wikipedia.org/wiki/VPK_Group) |
| CYTEC SURFACE SPECIALTIES | ALLNEX Belgium | Cytec acquired UCB's Surface Specialties in 2005. Solvay acquired Cytec in 2015. The coating resins business was spun off as Allnex. Same address: Anderlechtstraat 33, Drogenbos. | [Allnex announcement](https://allnex.com/en/info-hub/news/former-cytec-industries-coating-resins-business-be) |
| SOLVIN | INOVYN BELGIUM | Solvin was a 75/25 Solvay/BASF PVC joint venture. BASF sold its stake in 2015 and Solvay merged chlorovinyls with INEOS to form INOVYN. | [ICIS report](https://www.icis.com/explore/resources/news/2015/07/01/9899962/basf-sells-solvin-stake-to-solvay-as-inovyn-chlorvinyls-jv-starts-up/) |
| SOLVIC | BASF Antwerpen - 127c | Solvic was the VCM-PVC production unit at the BASF Antwerp Verbund site. Operations were shut down and absorbed into the BASF complex. | [BASF Antwerpen partners](https://www.basf.com/be/en/who-we-are/Group-Companies/BASF-Antwerpen/About-the-site/Partners-on-site) |
| LATEXCO | Novaya Belgium | Latexco (founded 1953, Tielt) merged with Artilat in 2023 to form Novaya. Same site. | [KennisWest](https://www.kenniswest.be/organisatie/latexco-novaya-nv-tielt/22938) |
| MISA ECO | Nesar | MISA ECO produced sulfuric acid in Sint-Kruis-Winkel (Gent). After bankruptcy, Nesar took over the site. Nesar also subsequently closed (2012). Same physical location. | [VRT NWS](https://www.vrt.be/vrtnws/nl/2012/05/04/53_banen_verlorenbijchemischbedrijfnesar-1-1291315/) |
| NORTH EUROPEAN SULFERIC ACID REGENERATION | Nesar | Same site as MISA ECO — predecessor company at the same location. | Same as above |
| ORRION CHEMICALS REGEN | Nesar | Intermediate operator between MISA ECO and Nesar at the same site. | Same as above |
| V.B.G. | VBG nv | Same company — "V.B.G." is the abbreviated form of "VBG nv". Same city (Wijnegem), same NACE (23.99). | User-confirmed |
| ASFALT PRODUCTIE LIMBURG | APL nv | Same company — "APL" is the acronym for "Asfalt Productie Limburg". Same city (Heusden-Zolder), same NACE (23.99). | User-confirmed |
| TEEPAK | ViskoTeepak | Same company — "Teepak" is embedded in "ViskoTeepak". Same city (Lommel). NACE differs (IMJV: 22.29 plastics vs EUTL: 20.16 primary plastics) due to classification differences. | User-confirmed, substring match |

**EDF LUMINUS / S.P.E. entries**: S.P.E. was renamed EDF Luminus in 2011 (after EDF acquired Centrica's stake in SPE-Luminus in 2009), then renamed Luminus in 2019. EDFL refers to Electrabel-Luminus joint operations. These entries are matched by CBE establishment suffix:

| CBB suffix | Location | EUTL installation | Source |
|------------|----------|-------------------|--------|
| 0137 | Harelbeke | EDFL - Centrale Harelbeke | [Luminus Wikipedia](https://en.wikipedia.org/wiki/Luminus_(company)) |
| 0238 | Gent | Electrabel - Langerbrugge | [EDF Luminus thermal](https://edfluminus.edf.com/en/edf-luminus/activities/produce-energy/thermal-power) |
| 0339 | Gent | Electrabel - Langerbrugge | Same source |
| 0743 | Izegem | EDFL - Izegem | Same source |

## Cases verified as NOT the same firm

The following flagged pairs were confirmed to be different firms:

| IMJV name | EUTL candidate | Reason |
|-----------|----------------|--------|
| COMINBEL | Inex | Both in Sint-Lievens-Houtem but different sectors: Cominbel = meat processing (NACE 10.41), Inex = dairy (NACE 10.51). |
| ADPO GHENT | Rousselot | ADPO is a chemical tank storage terminal. Rousselot is a gelatin manufacturer. Different companies. |
| HALTERMANN | 3M Belgium | Haltermann Carless operates blending sites in Ghent, not in Zwijndrecht where 3M is located. Different firms. |
| COGEBI | Terca Beerse | Different cities (Beersel vs Beerse) and different NACE (23.99 vs 23.32). |

## Systematic review of remaining unmatched firms

After all hard-coded matches, 39 IMJV firms remain unmatched. A systematic pass checked each against all EUTL installations for substring matches, token overlap, and city+NACE co-occurrence. Conclusion: **none of the 39 are obvious matching failures.** They fall into three groups:

**Waste incinerators (4 firms, ~391k t/yr total):** ISVAG Intercommunale (Antwerpen), Huisvuilverwerking Meetjesland (Eeklo), IVAGO (Gent), IMOG (Harelbeke). Municipal waste incinerators classified under NACE 38.21. Not in EU ETS.

**Potentially ambiguous (2 firms, ~59k t/yr total):**
- HALTERMANN (50k t/yr, Beveren-Zwijndrecht): specialty chemicals. No EUTL in same city. Haltermann Carless operates in Ghent per web search; may be a different site or below EU ETS threshold.
- DU PONT DE NEMOURS - MECHELEN (8.6k t/yr): only EUTL in Mechelen is a food manufacturer (Wimble). DuPont may have closed this site or it operates below the 20 MW threshold.

**Small non-ETS manufacturers (33 firms, ~72k t/yr total):** Plastics (Recticel ×4, Proseat ×2, Lemahieu, Seuropak, etc.), food (Cloetta, Vaco's Kitchen, La Lorraine), paper (Cartomills, Van Genechten Biermans), asphalt (Topasfalt, Colpin-De Meester), textiles (Vetex), water treatment (AWW), waste recycling (Sita ×2, Vanheede), and others. All below 10k t/yr, no plausible EUTL match.

## Output

`{PROC_DATA}/imjv_eutl_match.RData` contains:
- `imjv_eutl` — matching table with columns: cbb_number, imjv_name, imjv_gemeente, imjv_nace, mean_annual_co2_t, eutl_name, eutl_city, eutl_nace, name_sim, substr_match, city_match, nace_match_2d, nace_match_4d, matched, match_type
- `imjv_firms` — unique IMJV firms with summary statistics

Match types: `hardcoded`, `name_nace`, `name`, `name_city_nace`, `name_city`, `substr_city_nace`, `none`

## Marginal value of IMJV beyond EU ETS

Final matching result: 139 of 178 IMJV firms (78%) match to EUTL installations. The remaining 39 firms are genuinely non-ETS.

The unmatched firms' emissions as a share of total Belgian EUTL verified emissions vary by year:

| Period | IMJV unmatched / EUTL total | Reporting firms | Driver |
|--------|----------------------------|-----------------|--------|
| 2005–2012 | 0.2–0.3% | 12–16 | Many small firms, low individual emissions |
| 2013–2016 | 0.5–0.9% | 11–12 | Waste incinerators growing throughput |
| 2017–2018 | ~1.1% | 10–11 | Peak coverage period |
| 2019–2022 | 0.7–0.9% | 8–10 | Fewer reporting firms |
| 2023 | 1.2% | 10 | EUTL total declined while IMJV stable |

At peak, IMJV adds ~1% of additional emissions beyond EU ETS. The increase over time is driven by waste incinerators increasing throughput — the number of reporting firms actually decreases but the surviving reporters are larger.

The main sectors covered by IMJV but not EU ETS are:
- Waste incineration (NACE 38.21): ~391k t/yr, 4 firms — the dominant contributor
- Specialty chemicals (NACE 20.xx): ~75k t/yr, 5 firms
- Plastics (NACE 22.xx): ~50k t/yr, 11 firms
- Ceramics/asphalt (NACE 23.xx): ~29k t/yr, 7 firms

IMJV's value relative to Climate TRACE: unlike CT (which assigns identical emissions to all firms in a subsector via top-down allocation), IMJV provides actual facility-level reported emissions with real cross-firm variation. This makes it strictly more informative for the ~39 firms it covers.

## Validation of imputed emissions against IMJV

Script: [`facts-emissions-across-network/analysis/validate_imjv.R`](../facts-emissions-across-network/analysis/validate_imjv.R)

### Setup

Of the 178 IMJV CBBs (140 unique firms), 39 CBBs are unmatched to EUTL by name-based matching. Of those 39, one (HALTERMANN / Monument Chemical, CBB `01852140000105`) is matched to EUTL via the crosswalk BvD route. This leaves **38 CBBs = 33 unique firms** that are genuinely non-ETS.

The non-ETS CBB list is stored in `{RAW_DATA}/IMJV/crosswalk/non_ets_cbbs.csv`. These are compared against the GLO allocation output (`allocation_glo_balanced/alloc_YYYY.RData`) produced by `b_allocation_glo.R`.

### Coverage gap: IMJV firms absent from B2B

Of the 33 non-ETS firms, only 17 appear in the deployment panel (i.e., as buyers in the B2B data) at all. The other **14 firms (62 firm-years)** have no B2B purchase transactions and are therefore invisible to the prediction pipeline — no proxy is ever computed for them.

Among the 17 firms that do appear in B2B, **5 have zero year-overlap** between their B2B years and their IMJV reporting years. For example, Huisvuilverwerking Meetjesland appears in B2B for 2005–2008 but reports to IMJV only from 2013 onward. These 5 firms are classified as emitters with high p_i (~1.0) in the years they appear in B2B, but those years don't overlap with their IMJV data, so they appear as "never detected" when comparing to IMJV year-by-year.

The B2B absence could be driven by:
- **M&A activity.** A firm acquired by another entity may have its B2B purchases recorded under the absorbing firm's VAT after the merger. The IMJV data, maintained by the Flemish environmental agency (CBB registry), continues to use the original firm's identifier. This creates a mismatch: the firm exists in IMJV under one VAT but its economic transactions appear under a different VAT in B2B. The appendix of Bisztray, Marzec and Poncet (2023) documents this phenomenon in Belgian B2B data and describes how the CBE cessation reason codes (`021` = merger by absorption, `022` = merger by creation of new entity) and financial links can be used to trace VAT reassignments. We have not yet investigated whether M&A linkages explain the IMJV–B2B gaps.
- **Small firms below B2B reporting thresholds.** The 14 absent firms are mostly small manufacturers (plastics, ceramics, food), many with IMJV emissions below 10k t/yr.
- **Public-sector entities.** Several waste incinerators (IVAGO, ISVAG) are intercommunales that may not participate in standard B2B reporting.

### Performance conditional on B2B presence

Restricting to the 58 firm-years where the firm appears in the allocation for that specific year:

**Extensive margin:**
- Detection rate: **96.6%** (56 / 58 correctly classified as emitters)
- 2 FN firm-years: SITA RECYCLING SERVICES (2005, 584 t) and KINGSPAN TAREC (2013, 649 t). Both have p_i ≈ 1 and positive proxy values, but receive scope1 = 0 — likely because their CRF group's E_deploy was zero or near-zero in that year.

**Intensive margin** (56 TP firm-years, 12 unique firms):

| Metric | Value |
|--------|-------|
| Mean APD | 1.03 |
| Median APD | 0.97 |
| Mean log(imputed/IMJV) | 0.59 (imputed ≈ 1.8× IMJV) |
| Median log(imputed/IMJV) | 0.66 (imputed ≈ 1.9× IMJV) |
| Spearman rank correlation | 0.54 |

The imputed emissions systematically exceed IMJV-reported values. This is expected: the allocation distributes the full non-ETS share of NIR sector totals, which includes emissions from all non-ETS firms in the CRF group — not just the IMJV reporters. Each IMJV firm receives a share of a budget that covers many more firms, so the per-firm imputed value tends to exceed the firm's own reported emissions.

Firm-level detail (sorted by mean IMJV emissions):

| Firm | NACE | IMJV t/yr | Imputed t/yr | APD | Years |
|------|------|-----------|-------------|-----|-------|
| ISVAG Intercommunale | 38.21 | 118,965 | 50,391 | 1.29 | 7 |
| IVAGO | 38.21 | 93,223 | 5,085 | 1.79 | 3 |
| Du Pont de Nemours | 20.30 | 7,605 | 128,919 | 1.78 | 1 |
| AWW Broechem | 36.00 | 6,627 | 31,876 | 1.31 | 1 |
| Recticel | 22.21 | 3,738 | 9,332 | 0.89 | 14 |
| Leaf België | 10.82 | 3,297 | 6,751 | 0.78 | 2 |
| Cambrex Profarmaco | 21.10 | 1,570 | 13,276 | 1.58 | 1 |
| Cogebi | 23.99 | 1,386 | 2,498 | 0.57 | 5 |
| Cloetta | 10.82 | 1,347 | 5,228 | 1.19 | 4 |
| Rezinal | 24.43 | 1,258 | 853 | 0.80 | 11 |
| Sita Recyper | 38.22 | 918 | 1,718 | 0.61 | 1 |
| Sita Recycling Services | 38.22 | 674 | 14,822 | 1.81 | 2 |
| Kingspan Insulation | 20.16 | 572 | 973 | 0.52 | 1 |
| Kingspan Tarec | 20.16 | 534 | 2,046 | 1.07 | 3 |

### Summary

The primary bottleneck is **coverage**: only 58 of 183 non-ETS IMJV firm-years (32%) have the firm present in B2B for that year. Among the firm-years that the pipeline can see, the extensive margin works well (97% detection rate). The intensive margin has median APD of 0.97 with systematic over-prediction, which is mechanically expected given that the NIR budget is shared across all classified emitters in the CRF group, not just IMJV reporters.
