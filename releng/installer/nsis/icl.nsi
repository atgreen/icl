; ICL - Interactive Common Lisp Installer
; NSIS Script
;
; SPDX-License-Identifier: MIT
; Copyright (C) 2025 Anthony Green <green@moxielogic.com>

!include "MUI2.nsh"
!include "FileFunc.nsh"
!include "WordFunc.nsh"

; ============================================================================
; General Configuration
; ============================================================================

!define PRODUCT_NAME "ICL"
!define PRODUCT_FULL_NAME "Interactive Common Lisp"
!define PRODUCT_PUBLISHER "Anthony Green"
!define PRODUCT_WEB_SITE "https://github.com/atgreen/icl"
!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}"
!define PRODUCT_UNINST_ROOT_KEY "HKLM"

; Version is passed from command line: makensis /DVERSION=1.4.2 icl.nsi
!ifndef VERSION
  !define VERSION "0.0.0"
!endif

Name "${PRODUCT_FULL_NAME} ${VERSION}"
OutFile "icl-${VERSION}-setup.exe"
InstallDir "$PROGRAMFILES64\ICL"
InstallDirRegKey HKLM "Software\${PRODUCT_NAME}" "InstallDir"
RequestExecutionLevel admin
SetCompressor /SOLID lzma

; ============================================================================
; Modern UI Configuration
; ============================================================================

!define MUI_ABORTWARNING
!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\modern-install.ico"
!define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\modern-uninstall.ico"

; Welcome page
!insertmacro MUI_PAGE_WELCOME

; License page
!insertmacro MUI_PAGE_LICENSE "..\..\..\LICENSE"

; Directory page
!insertmacro MUI_PAGE_DIRECTORY

; Instfiles page
!insertmacro MUI_PAGE_INSTFILES

; Finish page
!define MUI_FINISHPAGE_SHOWREADME ""
!define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
!define MUI_FINISHPAGE_SHOWREADME_TEXT "Add ICL to system PATH"
!define MUI_FINISHPAGE_SHOWREADME_FUNCTION AddToPath
!insertmacro MUI_PAGE_FINISH

; Uninstaller pages
!insertmacro MUI_UNPAGE_INSTFILES

; Language
!insertmacro MUI_LANGUAGE "English"

; ============================================================================
; Installer Sections
; ============================================================================

Section "ICL Core" SEC_CORE
  SectionIn RO  ; Required section

  SetOutPath "$INSTDIR"

  ; Install main executable
  File "..\..\..\icl.exe"

  ; Install license and readme
  File "..\..\..\LICENSE"
  File "..\..\..\THIRD-PARTY-LICENSES.txt"
  File "..\..\..\assets\WEB-LICENSES"
  File "..\..\..\README.md"

  ; Install bundled ASDF (for Lisps that don't bundle it)
  SetOutPath "$INSTDIR\3rd-party\asdf"
  File "..\..\..\3rd-party\asdf\asdf.lisp"
  SetOutPath "$INSTDIR"

  ; Store installation folder
  WriteRegStr HKLM "Software\${PRODUCT_NAME}" "InstallDir" "$INSTDIR"

  ; Create uninstaller
  WriteUninstaller "$INSTDIR\uninstall.exe"

  ; Add to Add/Remove Programs
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "${PRODUCT_FULL_NAME}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "Publisher" "${PRODUCT_PUBLISHER}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_SITE}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\uninstall.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "InstallLocation" "$INSTDIR"
  WriteRegDWORD ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "NoModify" 1
  WriteRegDWORD ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "NoRepair" 1

  ; Calculate installed size
  ${GetSize} "$INSTDIR" "/S=0K" $0 $1 $2
  IntFmt $0 "0x%08X" $0
  WriteRegDWORD ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "EstimatedSize" "$0"
SectionEnd

Section "Start Menu Shortcuts" SEC_SHORTCUTS
  CreateDirectory "$SMPROGRAMS\${PRODUCT_NAME}"
  CreateShortcut "$SMPROGRAMS\${PRODUCT_NAME}\ICL.lnk" "$INSTDIR\icl.exe" "" "$INSTDIR\icl.exe" 0
  CreateShortcut "$SMPROGRAMS\${PRODUCT_NAME}\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
SectionEnd

; ============================================================================
; Functions
; ============================================================================

Function AddToPath
  ; Add to system PATH
  ReadRegStr $0 HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "Path"
  StrCpy $0 "$0;$INSTDIR"
  WriteRegExpandStr HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "Path" "$0"

  ; Broadcast environment change
  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000
FunctionEnd

Function un.RemoveFromPath
  ; Read current PATH
  ReadRegStr $0 HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "Path"

  ; Remove our directory from PATH (simple string replacement)
  ; This is a simplified version - production code should handle edge cases
  ${WordReplace} $0 ";$INSTDIR" "" "+" $0
  ${WordReplace} $0 "$INSTDIR;" "" "+" $0
  ${WordReplace} $0 "$INSTDIR" "" "+" $0

  WriteRegExpandStr HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "Path" "$0"
  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000
FunctionEnd

; ============================================================================
; Uninstaller Section
; ============================================================================

Section "Uninstall"
  ; Remove from PATH
  Call un.RemoveFromPath

  ; Remove files
  Delete "$INSTDIR\icl.exe"
  Delete "$INSTDIR\LICENSE"
  Delete "$INSTDIR\THIRD-PARTY-LICENSES.txt"
  Delete "$INSTDIR\WEB-LICENSES"
  Delete "$INSTDIR\README.md"
  Delete "$INSTDIR\uninstall.exe"

  ; Remove bundled ASDF
  Delete "$INSTDIR\3rd-party\asdf\asdf.lisp"
  RMDir "$INSTDIR\3rd-party\asdf"
  RMDir "$INSTDIR\3rd-party"

  ; Remove shortcuts
  Delete "$SMPROGRAMS\${PRODUCT_NAME}\ICL.lnk"
  Delete "$SMPROGRAMS\${PRODUCT_NAME}\Uninstall.lnk"
  RMDir "$SMPROGRAMS\${PRODUCT_NAME}"

  ; Remove installation directory
  RMDir "$INSTDIR"

  ; Remove registry keys
  DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"
  DeleteRegKey HKLM "Software\${PRODUCT_NAME}"
SectionEnd
