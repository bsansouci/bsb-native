Write-Output "Installing MSYS2 32-bit..."
# download installer
$zipPath = "$($env:USERPROFILE)\msys2-i686-latest.tar.xz"
$tarPath = "$($env:USERPROFILE)\msys2-i686-latest.tar"
Write-Output "Downloading MSYS installation package..."
(New-Object Net.WebClient).DownloadFile('http://repo.msys2.org/distrib/msys2-i686-latest.tar.xz', $zipPath)

Write-Output "Untaring installation package..."
7z x $zipPath -y -o"$env:USERPROFILE" | Out-Null

Write-Output "Unzipping installation package..."
7z x $tarPath -y -oC:\ | Out-Null
del $zipPath
del $tarPath

function bash($command) {
    Write-Output $command
    cmd /c start /wait C:\msys32\usr\bin\sh.exe --login -c $command
    Write-Output "Done"
}

[Environment]::SetEnvironmentVariable("MSYS2_PATH_TYPE", "inherit", "Machine")

# install latest pacman
bash 'pacman -Sy --noconfirm pacman pacman-mirrors'

# update core packages
bash 'pacman -Syu --noconfirm'
bash 'pacman -Syu --noconfirm'

# install packages
bash 'pacman --sync --noconfirm base-devel'
bash 'pacman --sync --noconfirm msys2-devel'
bash 'pacman --sync --noconfirm mingw-w64-{x86_64,i686}-toolchain'

Write-Output "MSYS2 32-bit installed"
[Environment]::Exit(0)
