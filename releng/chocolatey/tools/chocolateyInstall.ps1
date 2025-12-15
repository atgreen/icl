$ErrorActionPreference = 'Stop'

# ICL - Interactive Common Lisp
# Chocolatey Install Script
#
# SPDX-License-Identifier: MIT
# Copyright (C) 2025 Anthony Green <green@moxielogic.com>

$toolsDir = "$(Split-Path -Parent $MyInvocation.MyCommand.Definition)"

# Package parameters - version and checksum are replaced during CI build
$packageArgs = @{
  packageName    = $env:ChocolateyPackageName
  unzipLocation  = $toolsDir
  url64bit       = 'https://github.com/atgreen/icl/releases/download/v__VERSION__/icl-__VERSION__-windows.zip'
  checksum64     = '__CHECKSUM__'
  checksumType64 = 'sha256'
}

Install-ChocolateyZipPackage @packageArgs

# Create shim for icl.exe
# The exe is extracted directly to tools directory
$iclExe = Join-Path $toolsDir 'icl.exe'
if (Test-Path $iclExe) {
  Write-Host "ICL installed successfully to: $iclExe"
} else {
  throw "Installation failed: icl.exe not found at $iclExe"
}
