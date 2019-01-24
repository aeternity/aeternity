$ErrorActionPreference = "Stop"

. "$env:APPVEYOR_BUILD_FOLDER\ci\appveyor\util.ps1"

Prepare

if (-not (Test-Path '_build\default_$env:ERTS_VERSION')) {
  Exec 'Use cached build artifacts' 'robocopy "_build\default_$env:ERTS_VERSION" "_build\default" /MIR /COPYALL /NP /NS /NC /NFL /NDL'
}

switch ( $env:BUILD_STEP ) {
  'build' {
    Exec 'Run build: build' 'bash -lc "cd $env:BUILD_PATH && make KIND=test local-build"'
  }
  default {
    Write-Host 'Unknown build step'
  }
}

Exec 'Mirror build artifacts' 'robocopy "_build\default" "_build\default_$env:ERTS_VERSION" /MIR /COPYALL /NP /NS /NC /NFL /NDL'

Info 'Finished build phase'
