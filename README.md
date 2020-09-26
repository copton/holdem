# holdem

## Develop
ghcid -c="stack ghci holdem:lib holdem:test:holdem-test" -T=main
ghcid -c="stack ghci holdem:lib holdem:holdem-exe" -r="main"
