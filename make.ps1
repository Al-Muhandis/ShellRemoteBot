#!/usr/bin/env pwsh
##############################################################################################################

Function Show-Usage {
    Return "
Usage: pwsh -File $($PSCommandPath) [OPTIONS]
Options:
    build   Build program
"
}

Function Request-File {
    ForEach ($REPLY in $args) {
        $OutFileName = (Split-Path -Path $REPLY -Leaf).Split('?')[0]
        
        # Delete old file if exists
        If (Test-Path $OutFileName) {
            Remove-Item $OutFileName -Force
        }
        
        $params = @{
            Uri = $REPLY
            OutFile = $OutFileName
        }
        
        Write-Host "Download: $OutFileName"
        Invoke-WebRequest @params -TimeoutSec 600
        
        # Check if file does not exists or empty
        If (-not (Test-Path $OutFileName) -or (Get-Item $OutFileName).Length -lt 1MB) {
            Throw "Error: $OutFileName invalid or empty"
        }
        
        Return $OutFileName
    }
}

Function Install-Program {
    While ($Input.MoveNext()) {
        Switch ((Split-Path -Path $Input.Current -Leaf).Split('.')[-1]) {
            'msi' {
                & msiexec /passive /package $Input.Current | Out-Host
            }
            'exe' {
                & ".\$($Input.Current)" /SP- /VERYSILENT /SUPPRESSMSGBOXES /NORESTART | Out-Host
            }
        }
        Remove-Item $Input.Current
    }
}

Function Build-Project {
    $VAR = @{
        Cmd = 'lazbuild'
        Url = 'https://netix.dl.sourceforge.net/project/lazarus/Lazarus%20Windows%2064%20bits/Lazarus%203.6/lazarus-3.6-fpc-3.2.2-win64.exe?viasf=1'
        Path = "C:\Lazarus"
    }
    Try {
        Get-Command $VAR.Cmd
    } Catch {
        Request-File $VAR.Url | Install-Program
        $env:PATH+=";$($VAR.Path)"
        Get-Command $VAR.Cmd
    }
    If ( Test-Path -Path 'use\components.txt' ) {
        & git submodule update --recursive --init | Out-Host
        & git submodule update --recursive --remote | Out-Host
        Get-Content -Path 'use\components.txt' | ForEach-Object {
            If ((-not (& lazbuild --verbose-pkgsearch $_ | Out-Null)) -and
                (-not (& lazbuild --add-package $_ | Out-Null)) -and
                (-not (Test-Path -Path 'use\components.txt'))) {
                    $OutFile = Request-File "https://packages.lazarus-ide.org/$($_).zip"
                    Expand-Archive -Path $OutFile -DestinationPath "use\$($_)" -Force
                    Remove-Item $OutFile
                }
        }
        Get-ChildItem -Filter '*.lpk' -Recurse -File –Path 'use' | ForEach-Object {
            & lazbuild --add-package-link $_ | Out-Host
        }
    }
    Get-ChildItem -Filter '*.lpi' -Recurse -File –Path 'src' | ForEach-Object {
        & lazbuild --no-write-project --recursive --build-mode=release $_ | Out-Host
    }
}

Function Switch-Action {
    $ErrorActionPreference = 'stop'
    Set-PSDebug -Strict -Trace 1
    Invoke-ScriptAnalyzer -EnableExit -Path $PSCommandPath
    If ($args.count -gt 0) {
        Switch ($args[0]) {
            'build' {
                Build-Project
            }
            Default {
                Show-Usage
            }
        }
    } Else {
        Show-Usage
    }
}

##############################################################################################################
Switch-Action @args | Out-Null
