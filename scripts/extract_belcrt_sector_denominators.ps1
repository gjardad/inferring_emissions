# Extract sector-specific CO2 values from BEL-CRT files for years 2005, 2010, 2015, 2020
# Table1: 1.A.2.d. (Pulp, paper), 1.A.1.b. (Petroleum refining), 1.A.2.a. (Iron & steel)
# Table2(I): 2.H.1 or 2.H (Other - for paper), 2.C.1. (Iron & steel production)
$years = @(2005, 2010, 2015, 2020, 2022, 2023)
$nirDir = "C:\Users\jota_\Documents\NBB_data\raw\NIR"
$outFile = "C:\Users\jota_\Documents\NBB_data\processed\belcrt_sector_denominators.tsv"

$excel = New-Object -ComObject Excel.Application
$excel.Visible = $false
$excel.DisplayAlerts = $false

$results = @()
$results += "year`tcrf`tco2_kt"

foreach ($y in $years) {
    $f = Join-Path $nirDir "BEL-CRT-2025-V1.0-$y-20250411-104305_awaiting submission.xlsx"
    if (-not (Test-Path $f)) {
        Write-Host "File not found: $f"
        continue
    }
    $wb = $excel.Workbooks.Open($f)

    # Table1: labels in col B (2), CO2 in col C (3)
    $ws1 = $wb.Sheets.Item("Table1")
    $maxRow = [Math]::Min($ws1.UsedRange.Rows.Count, 70)

    $targets1 = @{
        "1.A.2.d." = "^1\.A\.2\.d\."
        "1.A.1.b." = "^1\.A\.1\.b\."
        "1.A.2.a." = "^1\.A\.2\.a\."
    }

    for ($r = 1; $r -le $maxRow; $r++) {
        $cellB = $ws1.Cells.Item($r, 2).Text.Trim()
        foreach ($key in $targets1.Keys) {
            if ($cellB -match $targets1[$key]) {
                $co2val = $ws1.Cells.Item($r, 3).Text.Trim()
                $results += "$y`t$key`t$co2val"
                Write-Host "$y Table1 row $r : $cellB -> CO2 = $co2val"
                break
            }
        }
    }

    # Table2(I): labels in col B (2), CO2 in col C (3)
    $ws2 = $wb.Sheets.Item("Table2(I)")
    $maxRow2 = [Math]::Min($ws2.UsedRange.Rows.Count, 80)

    $targets2 = @{
        "2.C.1." = "^2\.C\.1"
        "2.H.1." = "^2\.H\.1"
    }

    # Also try matching just "2.H" if 2.H.1 is not found
    $found2H1 = $false

    for ($r = 1; $r -le $maxRow2; $r++) {
        $cellB = $ws2.Cells.Item($r, 2).Text.Trim()
        foreach ($key in $targets2.Keys) {
            if ($cellB -match $targets2[$key]) {
                $co2val = $ws2.Cells.Item($r, 3).Text.Trim()
                $results += "$y`t$key`t$co2val"
                Write-Host "$y Table2(I) row $r : $cellB -> CO2 = $co2val"
                if ($key -eq "2.H.1.") { $found2H1 = $true }
                break
            }
        }
    }

    # If 2.H.1. not found, try "2.H" (some years use "2.H Other" instead of "2.H.1")
    if (-not $found2H1) {
        for ($r = 1; $r -le $maxRow2; $r++) {
            $cellB = $ws2.Cells.Item($r, 2).Text.Trim()
            if ($cellB -match "^2\.H\s" -or $cellB -match "^2\.H$") {
                $co2val = $ws2.Cells.Item($r, 3).Text.Trim()
                $results += "$y`t2.H.1.`t$co2val"
                Write-Host "$y Table2(I) row $r : $cellB -> CO2 = $co2val (mapped to 2.H.1.)"
                break
            }
        }
    }

    $wb.Close($false)
}

$excel.Quit()
[System.Runtime.Interopservices.Marshal]::ReleaseComObject($excel) | Out-Null

$results | Out-File -FilePath $outFile -Encoding UTF8
Write-Host "`nSaved: $outFile"
