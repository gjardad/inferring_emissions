# Extract key rows from the 2024 Annex XII (covering 2022)
# Looking for "stationary combustion" row in the CO2 section
$nirDir = "C:\Users\jota_\Documents\NBB_data\raw\NIR"
$f = Join-Path $nirDir "BE_2024_Art14_AnnexXII_Consistency_with_ETS_280224.xlsx"
$outFile = Join-Path $nirDir "annex_xii_2024.tsv"

if (-not (Test-Path $f)) {
    Write-Host "File not found: $f"
    exit 1
}

$excel = New-Object -ComObject Excel.Application
$excel.Visible = $false
$excel.DisplayAlerts = $false

$wb = $excel.Workbooks.Open($f)

# List sheet names to find the right one
Write-Host "Sheets in workbook:"
for ($s = 1; $s -le $wb.Sheets.Count; $s++) {
    Write-Host "  Sheet $s : $($wb.Sheets.Item($s).Name)"
}

# Try first sheet (typical for Annex XII)
$ws = $wb.Sheets.Item(1)
$maxRow = [Math]::Min($ws.UsedRange.Rows.Count, 120)
$maxCol = [Math]::Min($ws.UsedRange.Columns.Count, 10)

Write-Host "`nDumping first $maxRow rows, cols 1-$maxCol :"
$results = @()
$results += "row`tcol_A`tcol_B`tcol_C`tcol_D`tcol_E"

for ($r = 1; $r -le $maxRow; $r++) {
    $a = $ws.Cells.Item($r, 1).Text.Trim()
    $b = $ws.Cells.Item($r, 2).Text.Trim()
    $c = $ws.Cells.Item($r, 3).Text.Trim()
    $d = $ws.Cells.Item($r, 4).Text.Trim()
    $e = $ws.Cells.Item($r, 5).Text.Trim()
    if ($b -ne "" -or $c -ne "" -or $a -ne "") {
        $results += "$r`t$a`t$b`t$c`t$d`t$e"
        Write-Host "Row $r : A=[$a] B=[$b] C=[$c] D=[$d] E=[$e]"
    }
}

$wb.Close($false)
$excel.Quit()
[System.Runtime.Interopservices.Marshal]::ReleaseComObject($excel) | Out-Null

$results | Out-File -FilePath $outFile -Encoding UTF8
Write-Host "`nSaved: $outFile"
