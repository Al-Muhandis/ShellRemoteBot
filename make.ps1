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

Function Test-PeFile {
    # SourceForge mirrors may silently serve an HTML error page or a truncated
    # download in place of the file: require the minimal size and the MZ
    # signature of a Windows executable before running it
    Param([string]$Path, [long]$MinSize)
    $File = Get-Item -Path $Path -ErrorAction SilentlyContinue
    If ( $Null -eq $File -or $File.Length -lt $MinSize ) {
        Return $False
    }
    $Stream = [System.IO.File]::OpenRead($Path)
    Try {
        $Head = [byte[]]::new(2)
        If ( $Stream.Read($Head, 0, $Head.Length) -lt $Head.Length ) {
            Return $False
        }
        Return $Head[0] -eq 0x4D -and $Head[1] -eq 0x5A
    } Finally {
        $Stream.Close()
    }
}

Function Initialize-Lazbuild {
    $VAR = @{
        Cmd = 'lazbuild'
        File = 'lazarus-3.6-fpc-3.2.2-win64.exe'
        Path = "C:\Lazarus"
    }
    # A previous process (e.g. the CI build step) may have installed Lazarus
    # already: its PATH change does not survive, so re-add the install folder
    If ( Test-Path -Path "$($VAR.Path)\$($VAR.Cmd).exe" ) {
        $env:PATH+=";$($VAR.Path)"
    }
    If ( -not (Get-Command -Name $VAR.Cmd -ErrorAction SilentlyContinue) ) {
        # The first entry is the SourceForge mirror selector, the rest are
        # concrete mirrors to fall back on when it serves a broken download
        $Urls = 'downloads.sourceforge.net', 'netix.dl.sourceforge.net',
            'netcologne.dl.sourceforge.net', 'freefr.dl.sourceforge.net',
            'deac-riga.dl.sourceforge.net' | ForEach-Object {
                "https://$($_)/project/lazarus/Lazarus%20Windows%2064%20bits/$($VAR.File)?viasf=1"
            }
        ForEach ($Url in $Urls) {
            Try {
                Request-File $Url | Out-Null
                If ( -not (Test-PeFile -Path $VAR.File -MinSize 100MB) ) {
                    Throw 'downloaded file is not a Windows executable'
                }
                $VAR.File | Install-Program
                If ( Test-Path -Path "$($VAR.Path)\$($VAR.Cmd).exe" ) {
                    $env:PATH+=";$($VAR.Path)"
                    Return
                }
                Throw "no $($VAR.Cmd).exe in $($VAR.Path) after installation"
            } Catch {
                Write-Warning "$($Url): $($_.Exception.Message)"
                Remove-Item -Path $VAR.File -ErrorAction SilentlyContinue
            }
        }
        Throw 'Failed to download and install Lazarus from SourceForge'
    }
}

Function Register-Package {
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
        Get-ChildItem -Filter '*.lpk' -Recurse -File -Path 'use' | ForEach-Object {
            & lazbuild --add-package-link $_ | Out-Host
        }
    }
}

Function Build-Project {
    Initialize-Lazbuild
    Register-Package
    Get-ChildItem -Filter '*.lpi' -Recurse -File -Path 'src' | ForEach-Object {
        & lazbuild --no-write-project --recursive --build-mode=release $_ | Out-Host
    }
}

Function Test-Project {
    Initialize-Lazbuild
    Register-Package
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
