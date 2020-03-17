for h in $(git log 1657ce0^..1d68425 --format=%H)
do
    echo -n "$h"
    git checkout -q "$h"
    rm *.cabal
    stack build --ghc-options='-O2'
    echo -n ":"
    rm -f stats/$h.txt
    for i in $(seq 1 5)
    do
        stack --silent run -- assets/small.txt +RTS -t >/dev/null 2>> stats/$h.txt
        echo -n .
    done
    echo
done
