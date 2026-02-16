# Data dimensions for fuel-supplier identification

## LHS sample (emissions)
- EU ETS firms per year: ~250
- Non-ETS firms in NACE 19 & 24 per year: ~300-400
- Total LHS firms: 640 (unique across all years)
- Year range (EU ETS): 2005-2022 (18 years)
- Total firm-year observations: 6,642

## B2B selected sample
- Total rows: 120,617,126
- Unique sellers (vat_i): 280,837
- Unique buyers (vat_j): 307,780
- Year range: 2002-2022
- Usable year range (overlap with EU ETS): 2005-2022
- Rows per year: ~4.8M (2002) to ~6.2M (2019), dip in 2020 (~5.7M)

## B2B filtered to LHS buyers only
- B2B rows involving LHS buyers: 2,846,976
- ID field used for matching: vat (direct match, 640/640 overlap)
- Unique sellers supplying to at least 1 LHS firm: 90,311
- ... at least 2 LHS firms: 53,980
- ... at least 3 LHS firms: 39,094
- ... at least 5 LHS firms: 25,578
- ... at least 10 LHS firms: 13,411
- ... at least 20 LHS firms: 6,531

## B2B downsampled (local desktop, for development)
- Total rows: 555,399
- Unique sellers: 1,846
- Unique buyers: 1,983
- Year range: 2002-2022

## Design matrix (after pre-filtering, threshold >= 3 LHS buyers)
- Rows (firm-year obs): 6,642
- Columns (candidate suppliers): 39,094
- Non-zero entries: 2,314,435
- Total cells: 259,662,348
- Sparsity: 99.11% zeros

## Leontief inverse dimensions (selected-sample B2B network)

### Per-year network summary

| Year | N firms | Sellers | Buyers | Both sides | Edges     | Density   |
|------|---------|---------|--------|------------|-----------|-----------|
| 2005 | 120,389 | 102,158 | 120,047| 101,816    | 5,242,720 | 0.000362  |
| 2006 | 124,035 | 107,148 | 123,708| 106,821    | 5,593,647 | 0.000364  |
| 2007 | 126,324 | 109,215 | 126,041| 108,932    | 5,710,799 | 0.000358  |
| 2008 | 127,358 | 110,601 | 127,063| 110,306    | 5,861,609 | 0.000361  |
| 2009 | 127,673 | 110,892 | 127,432| 110,651    | 5,728,758 | 0.000351  |
| 2010 | 126,188 | 110,183 | 125,960| 109,955    | 5,781,778 | 0.000363  |
| 2011 | 127,240 | 111,382 | 127,007| 111,149    | 5,863,230 | 0.000362  |
| 2012 | 128,992 | 113,230 | 128,823| 113,061    | 5,938,568 | 0.000357  |
| 2013 | 127,581 | 112,333 | 127,382| 112,134    | 5,865,873 | 0.000360  |
| 2014 | 129,534 | 114,312 | 129,366| 114,144    | 5,993,835 | 0.000357  |
| 2015 | 129,249 | 114,222 | 129,097| 114,070    | 6,006,403 | 0.000360  |
| 2016 | 129,971 | 114,925 | 129,765| 114,719    | 6,160,305 | 0.000365  |
| 2017 | 128,883 | 114,074 | 128,690| 113,881    | 6,134,927 | 0.000369  |
| 2018 | 130,268 | 115,422 | 130,068| 115,222    | 6,199,453 | 0.000365  |
| 2019 | 130,429 | 115,305 | 130,236| 115,112    | 6,210,169 | 0.000365  |
| 2020 | 123,436 | 108,173 | 123,261| 107,998    | 5,688,378 | 0.000373  |
| 2021 | 123,435 | 108,620 | 123,296| 108,481    | 5,883,144 | 0.000386  |
| 2022 | 121,299 | 106,895 | 121,179| 106,775    | 5,923,870 | 0.000403  |

### Max degree per year

| Year | Max out-degree | Max in-degree |
|------|----------------|---------------|
| 2005 | 19,803         | 3,664         |
| 2006 | 23,042         | 3,659         |
| 2007 | 26,209         | 3,465         |
| 2008 | 24,522         | 3,407         |
| 2009 | 34,665         | 2,913         |
| 2010 | 32,839         | 2,973         |
| 2011 | 31,455         | 2,998         |
| 2012 | 30,271         | 3,098         |
| 2013 | 28,421         | 3,120         |
| 2014 | 27,261         | 3,227         |
| 2015 | 27,993         | 3,941         |
| 2016 | 39,322         | 3,970         |
| 2017 | 38,654         | 3,920         |
| 2018 | 38,290         | 4,351         |
| 2019 | 38,294         | 4,637         |
| 2020 | 35,754         | 4,324         |
| 2021 | 34,194         | 4,249         |
| 2022 | 34,987         | 3,623         |

### Connected components per year

| Year | Components | Largest  | 2nd | 3rd |
|------|------------|----------|-----|-----|
| 2005 | 1          | 120,389  | 0   | 0   |
| 2006 | 4          | 124,030  | 2   | 2   |
| 2007 | 2          | 126,322  | 2   | 0   |
| 2008 | 1          | 127,358  | 0   | 0   |
| 2009 | 2          | 127,671  | 2   | 0   |
| 2010 | 2          | 126,187  | 1   | 0   |
| 2011 | 4          | 127,237  | 1   | 1   |
| 2012 | 1          | 128,992  | 0   | 0   |
| 2013 | 2          | 127,580  | 1   | 0   |
| 2014 | 1          | 129,534  | 0   | 0   |
| 2015 | 1          | 129,249  | 0   | 0   |
| 2016 | 3          | 129,969  | 1   | 1   |
| 2017 | 1          | 128,883  | 0   | 0   |
| 2018 | 1          | 130,268  | 0   | 0   |
| 2019 | 1          | 130,429  | 0   | 0   |
| 2020 | 3          | 123,433  | 2   | 1   |
| 2021 | 3          | 123,433  | 1   | 1   |
| 2022 | 1          | 121,299  | 0   | 0   |

Network is essentially one giant connected component every year (largest component covers >99.99% of firms). Isolated components have at most 2 firms. This means Leontief inversion cannot exploit block-diagonal structure — the full N x N matrix must be inverted (or approximated).
