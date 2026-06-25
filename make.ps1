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
        $OutFile = (Split-Path -Path $REPLY -Leaf).Split('?')[0]
        $OutFilePath = Join-Path -Path (Get-Location) -ChildPath $OutFile
        
        Try {
            Write-Host "Downloading: $REPLY"
            $ProgressPreference = 'SilentlyContinue'
            Invoke-WebRequest -Uri $REPLY `
                -OutFile $OutFilePath `
                -TimeoutSec 600 `
                -UserAgent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36" `
                -MaximumRedirection 5 | Out-Null
            
            $fileSize = (Get-Item $OutFilePath).Length
            Write-Host "Downloaded: $OutFile (Size: $([Math]::Round($fileSize/1MB, 2))MB)"
            
            If ((Test-Path $OutFilePath) -and $fileSize -gt 100MB) {
                Return $OutFile
            } Else {
                Throw "File is too small ($fileSize bytes) or doesn't exist"
            }
        } Catch {
            Write-Error "Failed to download: $_"
            If (Test-Path $OutFilePath) { Remove-Item $OutFilePath -Force }
            Return $null
        }
    }
}

Function Install-Program {
    While ($Input.MoveNext()) {
        $FilePath = $Input.Current
        If (Test-Path $FilePath) {
            Write-Host "Installing: $FilePath"
            $ext = (Split-Path -Path $FilePath -Leaf).Split('.')[-1]
            Switch ($ext) {
                'msi' {
                    & msiexec /passive /package $FilePath | Out-Host
                }
                'exe' {
                    & $FilePath /SP- /VERYSILENT /SUPPRESSMSGBOXES /NORESTART | Out-Host
                }
            }
            Remove-Item $FilePath -Force
            Write-Host "Installation completed"
        }
    }
}

Function Build-Project {
    $VAR = @{
        Cmd = 'lazbuild'
        Url = 'https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Lazarus%204.8/lazarus-4.8-fpc-3.2.2-win64.exe/download'
        Path = "C:\Lazarus"
    }
    Try {
        Get-Command $VAR.Cmd | Out-Null
        Write-Host "Lazarus is already installed"
    } Catch {
        Write-Host "Lazarus not found, installing..."
        $OutFile = Request-File $VAR.Url
        If ($OutFile) {
            $OutFile | Install-Program
            $env:PATH+=";$($VAR.Path)"
            Get-Command $VAR.Cmd | Out-Null
            Write-Host "Lazarus installed successfully"
        } Else {
            Throw "Failed to download Lazarus"
        }
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
