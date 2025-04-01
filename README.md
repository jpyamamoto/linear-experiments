![Logo](https://socialify.git.ci/jpyamamoto/linear-types/image?font=Rokkitt&language=1&logo=https%3A%2F%2Fraw.githubusercontent.com%2Fjpyamamoto%2Flinear-types%2Frefs%2Fheads%2Fmain%2Flogo.svg&name=1&owner=1&pattern=Plus&theme=Auto)

# Linear Types in Haskell

[![DOI](https://zenodo.org/badge/887581222.svg)](https://doi.org/10.5281/zenodo.15116852)

Haskell project containing the code I developed to accompany my Bachelor's thesis: "Tipos Lineales en Haskell" (Linear types in Haskell).

The project uses the GHC extension `LinearTypes` that enables the use of linear types.

For more information on the `LinearTypes` extension, make sure to check the paper by Bernardy, Boespflug, Newton, Peyton-Jones and Spiwack: _"Linear Haskell: practical linearity in a higher-order polymorphic language"_.

## Run Locally

Clone the project

```bash
  git clone https://github.com/jpyamamoto/linear-types
```

Go to the project directory

```bash
  cd linear-types
```

Update cabal dependencies

```bash
  cabal update
```

Build the project

```bash
  cabal build
```

Run an executable

```bash
  cabal run <app>
```

The following are the available apps:

- `unsafe`: Use different optimization levels to get a different result.
- `destination-arrays`: An example that uses destionation arrays to populate an array by segments.
- `polarized-arrays`: Perform mergesort on an array using a polarized flow.
- `summed-area`: Build a [summed area table](https://en.wikipedia.org/wiki/Summed-area_table) using linear matrices.

## License

Distributed under the [MIT](https://choosealicense.com/licenses/mit/) License. See [LICENSE](LICENSE) for more information.

## Authors

- [@jpyamamoto](https://www.github.com/jpyamamoto) - [jpyamamoto.com](https://jpyamamoto.com/)
