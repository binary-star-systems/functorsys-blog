default: sync-typst-package
    cabal run website -- rebuild

dev:
    cabal run website -- watch

sync-typst-package:
    rsync -a ./typst/pkgs/html-shim ~/.cache/typst/packages/preview

# Run hoogle
hoogle:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ ARGS }}
