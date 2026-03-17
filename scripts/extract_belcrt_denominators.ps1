# Extract CO2 values for 1.A.1., 1.A.2., 1.A.4., 1.A.4.b. from Table1
# and 2. Total from Table2(I) for years 2005, 2010, 2015, 2020
# Labels are in column B (col 2), CO2 values in column C (col 3)
$years = @(2005, 2010, 2015, 2020)
$nirDir = "C:\Users\jota_\Documents\NBB_data\raw\NIR"
$outFile = "C:\Users\jota_\Documents\NBB_data\raw\NIR\belcrt_denominators.tsv"

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

    $targets = @{
        "1.A.1." = "^1\.A\.1\.\s"
        "1.A.2." = "^1\.A\.2\.\s"
        "1.A.4." = "^1\.A\.4\.\s"
        "1.A.4.b." = "^1\.A\.4\.b\."
    }

    for ($r = 1; $r -le $maxRow; $r++) {
        $cellB = $ws1.Cells.Item($r, 2).Text.Trim()
        foreach ($key in $targets.Keys) {
            if ($cellB -match $targets[$key]) {
                $co2val = $ws1.Cells.Item($r, 3).Text.Trim()
                $results += "$y`t$key`t$co2val"
                Write-Host "$y Table1 row $r : $cellB -> CO2 = $co2val"
                break
            }
        }
    }

    # Table2(I): labels in col B (2), CO2 in col C (3)
    $ws2 = $wb.Sheets.Item("Table2(I)")
    $maxRow2 = [Math]::Min($ws2.UsedRange.Rows.Count, 30)
    for ($r = 1; $r -le $maxRow2; $r++) {
        $cellB = $ws2.Cells.Item($r, 2).Text.Trim()
        if ($cellB -match "^2\.\s*Total") {
            $co2val = $ws2.Cells.Item($r, 3).Text.Trim()
            $results += "$y`t2.`t$co2val"
            Write-Host "$y Table2(I) row $r : $cellB -> CO2 = $co2val"
            break
        }
    }

    $wb.Close($false)
}

$excel.Quit()
[System.Runtime.Interopservices.Marshal]::ReleaseComObject($excel) | Out-Null

$results | Out-File -FilePath $outFile -Encoding UTF8
Write-Host "`nSaved: $outFile"
