param(
  [string]$ProjectDir = $PSScriptRoot
)

$ErrorActionPreference = 'Stop'
$iconsDir = Join-Path $ProjectDir 'icons'
$manifest = Join-Path $iconsDir 'icons.ini'
$lazres = 'C:\Lazarus\tools\lazres.exe'

if (-not (Test-Path -LiteralPath $manifest)) {
  throw "Icon manifest not found: $manifest"
}
if (-not (Test-Path -LiteralPath $lazres)) {
  throw "lazres not found: $lazres"
}

$files = New-Object System.Collections.Generic.List[string]
$inSection = $false
foreach ($line in Get-Content -LiteralPath $manifest) {
  $t = $line.Trim()
  if ($t -eq '[Icons]') { $inSection = $true; continue }
  if ($t.StartsWith('[')) { $inSection = $false; continue }
  if ($inSection -and ($t -ne '') -and $t.Contains('=')) {
    $parts = $t.Split('=', 2)
    $fileName = $parts[1].Trim()
    $full = Join-Path $iconsDir $fileName
    if (Test-Path -LiteralPath $full) {
      $files.Add($full)
    } else {
      Write-Warning "Icon file not found: $full"
    }
  }
}

if ($files.Count -eq 0) {
  throw 'No icon files found in icons.ini'
}

$out = Join-Path $ProjectDir 'icons.lrs'
if (Test-Path -LiteralPath $out) {
  Remove-Item -LiteralPath $out -Force
}

& $lazres $out @files
if ($LASTEXITCODE -ne 0) {
  throw "lazres failed with exit code $LASTEXITCODE"
}
Write-Output "Generated $out ($($files.Count) icons)"

$mainLfm = Join-Path $ProjectDir 'main.lfm'
$mainLrs = Join-Path $ProjectDir 'main.lrs'
$buildLfm = Join-Path $ProjectDir 'main_build.lfm'
if (Test-Path -LiteralPath $mainLfm) {
  if (Test-Path -LiteralPath $mainLrs) {
    Remove-Item -LiteralPath $mainLrs -Force
  }
  # main.lfm may be locked by the IDE; lazres reads an unlocked copy.
  Copy-Item -LiteralPath $mainLfm -Destination $buildLfm -Force
  try {
    & $lazres $mainLrs $buildLfm
    if ($LASTEXITCODE -ne 0) {
      throw "lazres failed for main.lfm with exit code $LASTEXITCODE"
    }
    Write-Output "Generated $mainLrs from main.lfm"
  } finally {
    if (Test-Path -LiteralPath $buildLfm) {
      Remove-Item -LiteralPath $buildLfm -Force
    }
  }
}
