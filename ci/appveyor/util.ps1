$ErrorActionPreference = "Stop"

Function Info([string]$info) {
  Write-Host Current time: (Get-Date -format r)
  Write-Host $info
}

Function Exec([string]$info, [string]$cmd) {
  Write-Host Current time: (Get-Date -format r)
  Write-Host $info
  Write-Host $cmd
  $cmd
}

Function Prepare() {
  cd $env:APPVEYOR_BUILD_FOLDER
  Exec 'Set the paths appropriately' "'C:\Program Files (x86)\Microsoft Visual Studio 2017\Community\VC\Auxiliary\Build\vcvarsall.bat' $env:PLATFORM"
  $env:PATH='$env:MSYS2_ROOT\mingw64\bin;$env:MSYS2_ROOT\usr\bin;$env:PATH'
}
