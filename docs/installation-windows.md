# Install Aeternity node on Windows Subsystem for Linux (WSL)

NOTE: These steps describe the setup for a basic CPU mining configuration.
      WSL has no GPU-passthrough support yet. Therefore,
      a native Windows setup will be required to leverage GPU mining.

## Prerequisites

- An up-to-date Windows 10 installation which supports WSL
- A working Microsoft Store
- PowerShell (which usually comes with Windows 10)
- Execution of unsigned PowerShell scripts must be allowed.
  This can usually be achieved by running `Set-ExecutionPolicy -ExecutionPolicy Unrestricted`.
  Read [Microsoft TechNet](https://technet.microsoft.com/en-us/library/bb613481.aspx)
  for more information

## Step 1: Get the installer scripts

The helper scripts are located in the [Installer Git repository](https://github.com/aeternity/installer)

You may get them by:

1. Downloading and unpacking the latest
    [`Source code (zip)`](https://github.com/aeternity/installer/releases/latest)
2. or by cloning the [Git repository](https://github.com/aeternity/installer) itself

Alternatively, you can download only the related scripts:

- `windows/setup-wsl.ps1`
- `windows/wsl-run.ps1`
- `install.sh`

## Step 2: Preparing WSL and installing the Aeternity node

Most of the setup is automated by the helper scripts. You need to execute the
script `windows/setup-wsl.ps1` within PowerShell. You can
do so by right-clicking on the script in the Windows Explorer, then selecting
`Run with PowerShell`. The output of the script will guide you through the
process.

NOTE: You might have to repeat this process up to 3 times, depending on the
      level of configuration required for your system.

## Step 3: Running Aeternity node

Once all configuration has succeeded you can run the Aeternity node by executing
the script `windows/wsl-run.ps1`.

The running node can be further inspected within the running Ubuntu system like
described in the rest of the Aeternity node documentation.
