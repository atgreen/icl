$ErrorActionPreference = 'Stop'

# ICL - Interactive Common Lisp
# Chocolatey Uninstall Script
#
# SPDX-License-Identifier: MIT
# Copyright (C) 2025 Anthony Green <green@moxielogic.com>

$toolsDir = "$(Split-Path -Parent $MyInvocation.MyCommand.Definition)"

# Remove the executable
$iclExe = Join-Path $toolsDir 'icl.exe'
if (Test-Path $iclExe) {
  Remove-Item $iclExe -Force
  Write-Host "Removed: $iclExe"
}

# Remove LICENSE if present
$license = Join-Path $toolsDir 'LICENSE'
if (Test-Path $license) {
  Remove-Item $license -Force
}

# Remove README if present
$readme = Join-Path $toolsDir 'README.md'
if (Test-Path $readme) {
  Remove-Item $readme -Force
}

Write-Host "ICL has been uninstalled."
