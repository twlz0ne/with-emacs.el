#!/usr/bin/env bash

set -e

emacs_bins=(
    /Applications/Emacs-24.4.app/Contents/MacOS/Emacs
    /Applications/Emacs-24.5.3-z-mac-5.18.app/Contents/MacOS/Emacs
    /Applications/Emacs-25.1.1.app/Contents/MacOS/Emacs
    /Applications/Emacs-25.2.app/Contents/MacOS/Emacs
    /Applications/Emacs-25.3.1.app/Contents/MacOS/Emacs
    /Applications/Emacs-26.1.app/Contents/MacOS/Emacs
    /usr/local/bin/emacs
)

for bin in "${emacs_bins[@]}"; do
    ver=$($bin --batch --eval '(princ (format "%s.%s" emacs-major-version emacs-minor-version))')
    echo "==> $ver: $bin"
    export EMACS=$bin
    # test -d .cask/$ver/elpa/archives || cask install
    make clean ; make test
done

