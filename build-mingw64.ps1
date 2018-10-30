${ENV:PATH}="C:\msys64\mingw64\bin;${ENV:PATH}"

if (-not (Test-Path ".\build-mingw64")) {
    New-Item -Type Directory ".\build-mingw64" | Out-Null
}

Push-Location ".\build-mingw64"; try {
    &cmake .. -G "MinGW Makefiles" -DCMAKE_INSTALL_PREFIX=install
    &mingw32-make.exe package
} finally { Pop-Location }
