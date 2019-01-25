# Install Aeternity node on Windows using the Windows Subsystem Linux

NOTE: These steps describe the setup for a basic CPU mining configuration.
      Windows Subsystem Linux has no GPU-passthrough support yet. Therefore,
      a native Windows setup will be required to leverage GPU mining.

## Prerequisites

- An up-to-date Windows 10 installation which supports Windows Subsystem linux
- A working Microsoft Store
- PowerShell (which usually comes with Windows 10)
- Execution of unsigned PowerShell scripts must be allowed.
  This can usually be achieved by running `Set-ExecutionPolicy -ExecutionPolicy Unrestricted`.
  Read [Microsoft TechNet](https://technet.microsoft.com/en-us/library/bb613481.aspx)
  for more information

## Step 1: Get helper scripts

The helper scripts are located in the `scripts/` folder in the `aeternity` Git repository.
You may download the whole repository by

1. Downloading the [Git repository](https://github.com/aeternity/aeternity) itself;
2. Or downloading and unpacking a
   [Zip archive](https://github.com/aeternity/aeternity/archive/master.zip)
   provided by Github.

Alternatively, you can also download the individual scripts:

- `scripts/windows/setup-wsl.ps1`
- `scripts/windows/wsl-run.ps1`
- `scripts/install.sh`

## Step 2: Preparing Windows and ubuntu

Most of the setup is automated by the helper scripts. You need to execute the
script `scripts/windows/setup-wsl.ps1` within PowerShell. You can
do so by right-clicking on the script in the Windows Explorer, then selecting
`Run with PowerShell`. The output of the script will guide you through the
process.

NOTE: You might have to repeat this process up to 3 times, depending on the
      level of configuration required for your system.

## Step 3: Running Aeternity node

Once all configuration has succeeded you can run the Aeternity node node by executing
the script `scripts/windows/wsl-run.ps1`.

The running node can be further inspected within the running Ubuntu system like
described in the rest of the Aeternity node documentation.
