#!/usr/bin/env pwsh
##############################################################################################################

Function PrivClipper {
    Write-Output "
Usage: pwsh -File make.ps1 [OPTIONS]
Options:
    build   Build program
"
}

Function PrivPrepare {
    Start-Process -Wait -FilePath 'choco' -ArgumentList 'install git -y'
    Start-Process -Wait -FilePath 'choco' -ArgumentList 'install fpc -y'
    Start-Process -Wait -FilePath 'choco' -ArgumentList 'install lazarus -y'
    Start-Process -Wait -FilePath 'refreshenv'
}

Function PrivPkgsearch {
    ForEach ($REPLY in $args) {
        If (-not (Start-Process -Wait -FilePath 'lazbuild' -ArgumentList "--verbose-pkgsearch $($REPLY)")) {
            Start-Process -Wait -FilePath 'lazbuild' -ArgumentList "--add-package $($REPLY)"
        }
    }
}

Function PrivPackages {
    If ( Test-Path -Path 'use' ) {
        Start-Process -Wait -FilePath 'git' -ArgumentList 'submodule update --init --recursive'
        Start-Process -Wait -FilePath 'git' -ArgumentList 'submodule update --recursive --remote'
    } Else {
        New-Item -ItemType Directory -Name 'use'
    }
    If ($args.count -gt 0) {
        ForEach ($REPLY in $args) {
            $params = @{
                Uri = "https://packages.lazarus-ide.org/$($REPLY).zip"
                OutFile = "$($REPLY).zip"
            }
            $ProgressPreference = 'SilentlyContinue'
            Invoke-WebRequest @params
            $ProgressPreference = 'Continue'
            Expand-Archive -Path $params.OutFile -DestinationPath $REPLY -Force
            Remove-Item $params.OutFile
        }
    }
    Get-ChildItem -Filter '*.lpk' -Recurse -File –Path 'use' | ForEach-Object {
        Start-Process -Wait -FilePath 'lazbuild' -ArgumentList "--add-package-link $($_.Name)"
    }
}

Function PrivMain {
    Invoke-ScriptAnalyzer -EnableExit -Path $PSCommandPath
    Set-PSDebug -Strict
    If ($args.count -gt 0) {
        PrivPrepare
        Switch ($args[0]) {
            'build' {
                PrivPkgsearch
                PrivPackages
                Get-ChildItem -Filter '*.lpi' -Recurse -File –Path 'src' | ForEach-Object {
                    Start-Process -Wait -FilePath 'lazbuild' -ArgumentList "--no-write-project --recursive --no-write-project --build-mode=release $($_.Name)"
                }
            }
            Default {PrivClipper}
        }
    } Else {
        Write-Output $args.count
    }
}

##############################################################################################################
PrivMain @args
