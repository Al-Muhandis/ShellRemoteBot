#!/usr/bin/env pwsh
##############################################################################################################

Function PrivClipper {
    Return "
Usage: pwsh -File $($PSCommandPath) [OPTIONS]
Options:
    build   Build program
"
}

Function PrivPrepare {
    $VAR = @(
        @{
            Cmd = 'lazbuild'
            Url = 'https://netix.dl.sourceforge.net/project/lazarus/Lazarus%20Windows%2064%20bits/Lazarus%203.6/lazarus-3.6-fpc-3.2.2-win64.exe?viasf=1'
            Path = "C:\Lazarus"
        }
    )
    ForEach ($REPLY in $VAR) {
        If (-not (Get-Command $REPLY.Cmd -ea 'continue')) {
            $params = @{
                Uri = $REPLY.Url
                OutFile = (Split-Path -Path $REPLY.Url -Leaf).Split('?')[0]
            }
            Invoke-WebRequest @params
            Start-Process -PassThru -Wait -FilePath $params.OutFile -ArgumentList '/SP-', '/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART'
            Remove-Item $params.OutFile
            $env:PATH+=";$($REPLY.Path)"
            Get-Command $REPLY.Cmd
        }
    }
}

Function PrivMain {
    $ErrorActionPreference = 'stop'
    Set-PSDebug -Strict -Trace 1
    Invoke-ScriptAnalyzer -EnableExit -Path $PSCommandPath
    If ($args.count -gt 0) {
        Switch ($args[0]) {
            'build' {
                PrivPrepare
                If (Test-Path -Path 'use') {
                    Start-Process -Wait -FilePath 'git' -ArgumentList 'submodule', 'update', '--recursive', '--init'
                    Start-Process -Wait -FilePath 'git' -ArgumentList 'submodule', 'update', '--recursive', '--remote'
                    Get-ChildItem -Filter '*.lpk' -Recurse -File –Path 'use' | ForEach-Object {
                        Start-Process -PassThru -Wait -FilePath 'lazbuild' -ArgumentList '--add-package-link', $_.Name
                    }
                }
                Get-ChildItem -Filter '*.lpi' -Recurse -File –Path 'src' | ForEach-Object {
                    Start-Process -Wait -FilePath 'lazbuild' -ArgumentList '--no-write-project', '--recursive', '--build-mode=release', $_.Name
                }
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
