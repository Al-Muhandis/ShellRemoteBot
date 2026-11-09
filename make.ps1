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

Function Request-SourceForgeFile {
    # SourceForge hands the real download out via signed URLs: a bare request
    # (no browser User-Agent) gets an HTTP redirect to it, while browser-like
    # clients get an HTML interstitial with the same URL in its meta refresh.
    # Try curl.exe first, then Invoke-WebRequest with the mirrors until a
    # real executable lands
    Param([string]$Project, [string]$Path, [string]$File, [string[]]$Mirrors,
        [long]$MinSize = 1KB)
    ForEach ($Mirror in $Mirrors) {
        & curl.exe --silent --show-error --location --max-redirs 10 `
            --connect-timeout 20 --max-time 600 --output $File (
                "https://sourceforge.net/projects/$Project/files/$Path/$File/download" +
                $(If ($Mirror) { "?use_mirror=$Mirror" })
            )
        If ($LASTEXITCODE -eq 0 -and (Test-PeFile -Path $File -MinSize $MinSize)) {
            Return
        }
        Write-Warning "curl.exe mirror '$Mirror': exit code $LASTEXITCODE, $((Get-Item $File -ErrorAction SilentlyContinue).Length) bytes received"
        Remove-Item -Path $File -ErrorAction SilentlyContinue
    }
    ForEach ($Mirror in ($Mirrors | Where-Object { $_ })) {
        Try {
            $Page = Invoke-WebRequest -TimeoutSec 60 -Uri (
                "https://$Mirror.dl.sourceforge.net/project/$Project/$Path/$File?viasf=1"
            )
            $Signed = [regex]::Match($Page.Content, 'http-equiv="refresh"[^>]+url=([^">]+)').Groups[1].Value
            If (-not $Signed) {
                Throw 'no signed download URL on the interstitial page'
            }
            Invoke-WebRequest -TimeoutSec 300 -Uri $Signed.Replace('&amp;', '&') -OutFile $File
            If (Test-PeFile -Path $File -MinSize $MinSize) {
                Return
            }
            Throw "received $((Get-Item $File).Length) bytes, not an executable"
        } Catch {
            Write-Warning "Invoke-WebRequest mirror '$Mirror': $($_.Exception.Message)"
            Remove-Item -Path $File -ErrorAction SilentlyContinue
        }
    }
    Throw "Failed to download $File from SourceForge"
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
        # Empty mirror name asks SourceForge to pick one itself, the rest are
        # concrete mirrors to fall back on
        Request-SourceForgeFile -Project 'lazarus' -Path 'Lazarus%20Windows%2064%20bits/Lazarus%203.6' `
            -File $VAR.File -Mirrors @('', 'phoenixnap', 'netix', 'deac-riga', 'freefr') -MinSize 100MB
        $VAR.File | Install-Program
        If ( -not (Test-Path -Path "$($VAR.Path)\$($VAR.Cmd).exe") ) {
            Throw "no $($VAR.Cmd).exe in $($VAR.Path) after installation"
        }
        $env:PATH+=";$($VAR.Path)"
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
