$ErrorActionPreference = 'Stop'

# Save old path so we don't mangle everything
$oldpath = ${ENV:PATH}
try {
    # Cmake doesn't like sh.exe to be in the PATH for MinGW Makefiles, so remove
    # anything that adds it. (Git is an offender here.)
    [System.Collections.ArrayList] $paths = ${ENV:PATH} -Split ";"
    $searching = 0
    while (0 -le $searching -and $searching -lt 20) {
        try {
            $sh = Get-Command sh.exe
            $dir = (Get-Item $sh.Source).Directory.FullName
            Write-Host "Found sh.exe in $dir, removing from path"
            $paths.Remove($dir)
            ${ENV:PATH} = $paths -join ";"
            $searching -= 1
        } catch {
            Write-Host "$searching sh.exe instances removed"
            $searching = -1
        }
    }

    if ($searching -ne -1) {
        throw "Could not remove sh.exe from path (tried $searching times)"
    }

    # Add MinGW64 to the path
    ${ENV:PATH}="C:\msys64\mingw64\bin;${ENV:PATH}"

    if (-not (Test-Path ".\build-mingw64")) {
        New-Item -Type Directory ".\build-mingw64" | Out-Null
    }

    Push-Location ".\build-mingw64"; try {
        &cmake .. -G "MinGW Makefiles" -DCMAKE_INSTALL_PREFIX=install
        if (-not $?) { throw "CMake error" }
        &mingw32-make.exe package
        if (-not $?) { throw "Make error" }
    } finally { Pop-Location }
} finally {
    ${ENV:PATH} = $oldpath
}

