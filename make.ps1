#!/usr/bin/env pwsh
##############################################################################################################

Function Show-Usage {
    Return "
Usage: pwsh -File $($PSCommandPath) [OPTIONS]
Options:
    build   Build program
    test    Build and run the FPCUnit test suite
"
}

Function Request-File {
    ForEach ($REPLY in $args) {
        $params = @{
            Uri = $REPLY
            OutFile = (Split-Path -Path $REPLY -Leaf).Split('?')[0]
        }
        Invoke-WebRequest @params | Out-Null
        Return $params.OutFile
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

Function Initialize-Lazbuild {
    $VAR = @{
        Cmd = 'lazbuild'
        Url = 'https://netix.dl.sourceforge.net/project/lazarus/Lazarus%20Windows%2064%20bits/Lazarus%203.6/lazarus-3.6-fpc-3.2.2-win64.exe?viasf=1'
        Path = "C:\Lazarus"
    }
    # A previous process (e.g. the CI build step) may have installed Lazarus
    # already: its PATH change does not survive, so re-add the install folder
    If ( Test-Path -Path "$($VAR.Path)\$($VAR.Cmd).exe" ) {
        $env:PATH+=";$($VAR.Path)"
    }
    Try {
        Get-Command $VAR.Cmd | Out-Null
    } Catch {
        Request-File $VAR.Url | Install-Program
        $env:PATH+=";$($VAR.Path)"
        Get-Command $VAR.Cmd | Out-Null
    }
}

Function Register-Packages {
    If ( Test-Path -Path 'use\components.txt' ) {
        & git submodule update --recursive --init | Out-Host
        Get-Content -Path 'use\components.txt' | ForEach-Object {
            If ((-not (& lazbuild --verbose-pkgsearch $_ | Out-Null)) -and
                (-not (& lazbuild --add-package $_ | Out-Null)) -and
                (-not (Test-Path -Path "use\$($_)"))) {
                    $OutFile = Request-File "https://packages.lazarus-ide.org/$($_).zip"
                    Expand-Archive -Path $OutFile -DestinationPath "use\$($_)" -Force
                    Remove-Item $OutFile
                }
        }
        Get-ChildItem -Filter '*.lpk' -Recurse -File –Path 'use' | ForEach-Object {
            & lazbuild --add-package-link $_ | Out-Host
        }
    }
}

Function Build-Project {
    Initialize-Lazbuild
    Register-Packages
    Get-ChildItem -Filter '*.lpi' -Recurse -File –Path 'src' | ForEach-Object {
        & lazbuild --no-write-project --recursive --build-mode=release $_ | Out-Host
    }
}

Function Test-Project {
    Initialize-Lazbuild
    Register-Packages
    & lazbuild --no-write-project tests\tgshdtests.lpi | Out-Host
    & .\tests\tgshdtests.exe --all --format=plain | Out-Host
    If ($LASTEXITCODE -ne 0) {
        Throw "FPCUnit tests failed with exit code $LASTEXITCODE"
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
            'test' {
                Test-Project
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
