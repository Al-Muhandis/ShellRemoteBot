#!/usr/bin/env pwsh
##############################################################################################################

Function PrivClipper {
    Return "
Usage: pwsh -File $($PSCommandPath) [OPTIONS]
Options:
    build   Build program
"
}

Function PrivWget {
    ForEach ($REPLY in $args) {
        $params = @{
            Uri = $REPLY
            OutFile = (Split-Path -Path $REPLY -Leaf).Split('?')[0]
        }
        Invoke-WebRequest @params | Out-Null
        Return $params.OutFile
    }
}

Function PrivMsiexec {
    While ($Input.MoveNext()) {
        $params = @{}
        Switch ((Split-Path -Path $Input.Current -Leaf).Split('.')[-1]) {
            'msi' {
                $params = @{
                    FilePath = 'msiexec'
                    ArgumentList = '/passive', '/package', $Input.Current
                }
            }
            'exe' {
                $params = @{
                    FilePath = $Input.Current
                    ArgumentList = '/SP-', '/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART'
                }
            }
        }
        Start-Process -PassThru -Wait @params
        Remove-Item $Input.Current
    }
}

Function PrivLazBuild {
    $VAR = @{
        Cmd = 'lazbuild'
        Url = 'https://netix.dl.sourceforge.net/project/lazarus/Lazarus%20Windows%2064%20bits/Lazarus%203.6/lazarus-3.6-fpc-3.2.2-win64.exe?viasf=1'
        Path = "C:\Lazarus"
    }
    If (-not (Get-Command $VAR.Cmd -ea 'continue')) {
        PrivWget $VAR.Url | PrivMsiexec
        $env:PATH+=";$($VAR.Path)"
        Get-Command $VAR.Cmd
    }
    If ( Test-Path -Path 'use\components.txt' ) {
        Start-Process -Wait -FilePath 'git' -ArgumentList 'submodule', 'update', '--recursive', '--init'
        Start-Process -Wait -FilePath 'git' -ArgumentList 'submodule', 'update', '--recursive', '--remote'
        Get-Content -Path 'use\components.txt' | ForEach-Object {
            If ((-not (Start-Process -ea 'continue' -Wait -FilePath 'lazbuild' -ArgumentList '--verbose-pkgsearch', $_)) {
                If (-not (Start-Process -ea 'continue' -Wait -FilePath 'lazbuild' -ArgumentList '--add-package', $_)) {
                    If (-not (Test-Path -Path 'use\components.txt'))) {
                        $OutFile = PrivWget "https://packages.lazarus-ide.org/$($_).zip"
                        Expand-Archive -Path $OutFile -DestinationPath "use\$($_)" -Force
                        Remove-Item $OutFile
                    }
                }
            }
        }
        Get-ChildItem -Filter '*.lpk' -Recurse -File –Path 'use' | ForEach-Object {
            Start-Process -Wait -FilePath 'lazbuild' -ArgumentList '--add-package-link', $_.Name
        }
    }
    Get-ChildItem -Filter '*.lpi' -Recurse -File –Path 'src' | ForEach-Object {
        Start-Process -Wait -FilePath 'lazbuild' -ArgumentList '--no-write-project', '--recursive', '--build-mode=release', $_.Name
    }
}

Function PrivMain {
    $ErrorActionPreference = 'stop'
    Set-PSDebug -Strict -Trace 1
    Invoke-ScriptAnalyzer -EnableExit -Path $PSCommandPath
    If ($args.count -gt 0) {
        Switch ($args[0]) {
            'build' {
                PrivLazBuild
            }
            Default {
                PrivClipper
            }
        }
    } Else {
        PrivClipper
    }
}

##############################################################################################################
PrivMain @args
