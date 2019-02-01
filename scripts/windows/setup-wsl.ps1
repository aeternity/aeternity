echo "This script might require to be re-executed with elevated administrator permissions."
Pause

If (-NOT ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")) {
  $arguments = "& '" + $myinvocation.mycommand.definition + "'"
  Start-Process powershell -Verb runAs -ArgumentList $arguments
  Break
}

if ((Get-WindowsOptionalFeature -FeatureName Microsoft-Windows-Subsystem-Linux -Online).State -eq "Disabled") {
  echo "Enabling Windows Subsystem Linux: This action will require a restart."
  Enable-WindowsOptionalFeature `
    -FeatureName Microsoft-Windows-Subsystem-Linux `
    -Online `
    -NoRestart:$False ;
}

$canBashRun = bash -c "exit 0;"

if (-NOT ($?)) {
  echo "You need to set up Ubuntu for WSL. Follow these steps:"
  echo "  1. Install Ubuntu 18.04 from the Microsoft Store"
  echo "  2. Start Ubuntu 18.04 once, which will finalize the installation"
  echo ""
  echo "Then you may re-run this script."
  Pause
  Exit
}

echo ""
echo "You can find a list of Aeternity node releases at https://github.com/aeternity/aeternity/releases"
echo ""

$myPath = Split-Path -parent $PSCommandPath
$releaseVersion = Read-Host 'Which Aeternity node release version do you want to install (Format: x.xx.x)?'

echo ""
echo "NOTE: The following commands are running inside Ubuntu."
echo ""
cd $myPath
bash ./../install.sh $releaseVersion

echo "Congratulations. You can now start the Aeternity node miner by executing the script:"
echo ""
echo "  wsl-run.ps1"
Pause
