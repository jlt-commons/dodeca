# dodeca

A Clojure port of [A5](https://github.com/felixpalmer/a5), the equal-area pentagonal
discrete global grid, running on [Jolt](https://github.com/jolt-lang/jolt) (no JVM).

Ported from A5 commit `8af4c27e701efdf46abcb2f9c1595769f85ec637` (v1.0.0-beta.4,
which pya5 1.0.0 matches). It covers indexing, the cell hierarchy, cell measures and
grid disks; coverings and set operations, polygon fill, spherical caps and lines,
subcells and migration are not ported yet.

## Using it

```clojure
;; deps.edn
{:deps {io.github.jlt-commons/dodeca {:git/url "https://github.com/jlt-commons/dodeca"
                                       :git/sha "..."}}}
```

The library has no dependencies.

```clojure
(require '[dodeca.core :as a5])

(def cell (a5/lon-lat->cell [-0.1276 51.5074] 10))
(a5/cell->hex cell)                      ;=> "6397248000000000"
```

A cell ID is a `long` holding A5's unsigned 64 bits, so IDs at or above 2^63 read
as negative numbers. Compare and sort them with
`dodeca.utils.unsigned/unsigned-compare`, not `<`, and exchange them as hex.

## API

All in `dodeca.core`. Points are `[longitude latitude]` in degrees.

### Indexing

| Function | |
|---|---|
| `(lon-lat->cell lon-lat resolution)` | The cell containing a point, resolution -1 (the world cell) to 30. |
| `(cell->lon-lat cell)` | The center of a cell. |
| `(cell->boundary cell)`, `(cell->boundary cell {:closed-ring true :segments :auto})` | The boundary, counter-clockwise; `:segments` points per edge. |
| `(hex->cell hex)`, `(cell->hex cell)` | A5's hex spelling: lowercase, no leading zeros. |

```clojure
(a5/cell->lon-lat cell)                  ;=> [-0.15971839880376137 51.511842921513406]
(a5/cell->boundary cell {:segments 1})
;=> [[-0.19250141916916164 51.51946462334751] [-0.2007861594699989 51.475581918376406]
;    [-0.13985206278334772 51.48472345047164] [-0.11104984728308409 51.52438151810816]
;    [-0.1543657759712005 51.5550384732005] [-0.19250141916916164 51.51946462334751]]
(a5/hex->cell "6397248000000000")         ;=> 7176244663412588544
```

### Hierarchy

| Function | |
|---|---|
| `(cell->parent cell)`, `(cell->parent cell resolution)` | The ancestor, one resolution up by default. |
| `(cell->children cell)`, `(cell->children cell resolution)` | The descendants, one resolution down by default, in ascending ID order. |
| `(resolution cell)` | -1 for the world cell, else 0-30. |
| `(valid-cell? x)` | Whether a 64-bit value is a cell ID (the world cell is one). |
| `(res0-cells)` | The 12 dodecahedron faces. |
| `max-resolution`, `world-cell` | 30, and 0. |

```clojure
(a5/cell->hex (a5/cell->parent cell))    ;=> "6397260000000000"
(a5/cell->hex (a5/cell->parent cell 5))  ;=> "6396000000000000"
(map a5/cell->hex (a5/cell->children cell))
;=> ("6397242000000000" "6397246000000000" "639724a000000000" "639724e000000000")
(a5/resolution cell)                     ;=> 10
```

### Measures

| Function | |
|---|---|
| `(num-cells resolution)` | Cells at a resolution: a long, a BigInt at 30. |
| `(num-children parent-resolution child-resolution)` | Descendants per cell. |
| `(cell-area resolution)` | Square metres; every cell of a resolution has the same area. |
| `(cell-edge-length-avg resolution)` | Metres; individual edges vary by about ±10%. |

```clojure
(a5/num-cells 10)                        ;=> 15728640
(a5/num-cells 30)                        ;=> 17293822569102704640N
(a5/num-children 10 12)                  ;=> 16
(a5/cell-area 10)                        ;=> 3.2429099068923894E7
(a5/cell-edge-length-avg 10)             ;=> 4675.881472821423
```

### Neighbours and coverings

| Function | |
|---|---|
| `(grid-disk cell k)` | The cells within k edge-sharing hops, as a covering. |
| `(grid-disk-vertex cell k)` | The same, counting cells that share only a vertex. |
| `(uncompact covering)` | Every cell of a covering at its resolution. |
| `(compact cells)` | The coarsest cells covering a set, in curve order. |
| `(compaction-marker? x)` | Whether a value is a covering's closing marker. |

A covering is A5's form for a set of cells: compacted, in curve order, then a
compaction marker that records the resolution the cells stand for. `uncompact` reads
it back.

```clojure
(map a5/cell->hex (a5/grid-disk cell 1))
;=> ("6397228000000000" "6397238000000000" "6397248000000000" "6397258000000000"
;    "6397268000000000" "6397368000000000" "f00a000000000040")
(count (a5/uncompact (a5/grid-disk cell 2)))  ;=> 17
```

The namespaces under `dodeca.*` mirror A5's modules one for one (`modules/core/cell.ts`
is `dodeca.core.cell`), so A5's internals are there too, with the same names in
kebab-case.

## Exactness

dodeca ports A5's arithmetic operation for operation and is checked against A5's own
fixtures, copied unchanged into `test/fixtures/`:

- Integer outputs (cell IDs, resolutions, counts) match bit for bit, at every fixture
  point, the poles and the cell boundaries included.
- Floating-point outputs match at A5's tolerances, 1e-10 or tighter. Where A5 has no
  fixture, probes recorded what A5's own TypeScript returns, and the results
  agree to 1e-13.
- `lon-lat->cell` agrees with pya5 1.0.0 on 100,000 seeded points at resolutions
  0-12, including points within 1e-9 degrees of the poles and the antimeridian, and
  `grid-disk` on 1,000 cells for k = 1, 2 and 3 (`test/fixtures/crosscheck/`).

The trigonometric functions come from the platform's libm, as in pya5, and from
LLVM libc's correctly rounded ones in V8, so they differ from A5's TypeScript in the
last bit now and then. No cell differs because of it; the one place it shows is
A5's equal-area round-trip test, which this port meets at 0.0027mm where A5 asks for
0.0024mm.

## Running

```bash
jolt -A:test -M           # the tests, about 15s
jolt -A:writ -M           # the writ specs
```

`clojure.data.json` and `jolt-lang/time` are test-only (`:test` alias), the latter
because `data.json` names `java.time.format.DateTimeFormatter` as it loads and Jolt
core does not carry it.

## Throughput

On 100,000 seeded random points, Jolt v0.8.23 on an Apple M1 Max:

| | per second |
|---|---|
| `lon-lat->cell`, resolution 5 | 48,800 |
| `lon-lat->cell`, resolution 12 | 40,500 |
| `lon-lat->cell`, resolution 20 | 38,400 |
| `cell->lon-lat`, resolution 12 | 77,900 |
| `grid-disk` k = 1, resolution 12 | 13,500 |
| `grid-disk` k = 3, resolution 12 | 2,550 |

## Attribution

A5 is Copyright (c) A5 contributors and licensed under the Apache License 2.0. dodeca
is a derivative work under the same licence; see `NOTICE` and `LICENSE`. The authalic
latitude conversion adapts code from the DGGAL project, Copyright (c) Ecere
Corporation, under the BSD 3-Clause License, also reproduced in `NOTICE`.
