## Generate key

```
curl "https://raw.githubusercontent.com/AlexandreZani/home_config/main/.setup/keygen.sh" -sSf | bash
```

## Setup Home Config

```
curl "https://raw.githubusercontent.com/AlexandreZani/home_config/main/.setup/setup.sh" -sSf | bash
```

## Lean

Install elan

```
curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh
```

## Install lean4

```
elan default leanprover/lean4:stable
```

## F-star

Install opam

```
sudo apt-get install opam
```

Install fstar
```
opam install fstar
```

## Buildifier

```
go get -u github.com/bazelbuild/buildtools/buildifier
```
