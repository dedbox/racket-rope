- [Layout](#org84e4222)
- [Running](#org5e5e09d)
- [Comparing before/after a change](#org05b5151)
- [Reading the equality benchmarks specifically](#org21c6fed)
- [Timer resolution and batching](#org3c64750)
- [Options](#orge391b8f)
- [Visualizing results](#orgc11b01f)
  - [Design](#orgbd8951d)
  - [Options](#org42ad9e6)



<a id="org84e4222"></a>

# Layout

| File                     | Responsibility                                                                                           |
|--------------------------|----------------------------------------------------------------------------------------------------------|
| `bench-core.rkt`         | Timing/statistics primitives. Knows nothing about ropes.                                                 |
| `suite.rkt`              | The `bench` fixture/op abstraction; warm vs. cold execution.                                             |
| `type-ops.rkt`           | Generic rope-type vocabulary (`rope-type-ops`) + shape/perturbation helpers.                             |
| `generators.rkt`         | Random raw-content generators + concrete `string-ops~/~bytes-ops`.                                       |
| `suites/generic-ops.rkt` | build/append/split/splice/slice/cursor/fold/sequence/shape benchmarks.                                   |
| `suites/equality.rkt`    | `equal?~/~equal-hash-code` benchmark matrix (7 scenarios × warm/cold).                                   |
| `suites/comparison.rkt`  | `rope-compare`/`rope=?` (+ string-only `ci-compare`) benchmark matrix, reusing equality.rkt's scenarios. |
| `suites/all.rkt`         | Suite registry main.rkt drives.                                                                          |
| `report.rkt`             | JSON I/O + A/B comparison + regression report.                                                           |
| `main.rkt`               | CLI entry point.                                                                                         |

Every benchmark is written once, generically, against `rope-type-ops` (see `type-ops.rkt`), and instantiated for both `rope/string` and `rope/bytes` in `generators.rkt`. A new rope type only needs one more `rope-type-ops` value to pick up every benchmark in this suite unmodified. This also means the exact same code runs, unmodified, before and after an implementation change, which is what makes an A/B comparison meaningful.


<a id="org5e5e09d"></a>

# Running

```sh
racket benchmark/main.rkt
racket benchmark/main.rkt --sizes 0,100,10000 --trials 30 --suites string-equality
```


<a id="org05b5151"></a>

# Comparing before/after a change

```sh
git stash                                   # back to the old implementation
racket benchmark/main.rkt --label before --save /tmp/before.json

git stash pop                               # bring the change back
racket benchmark/main.rkt --label after --baseline /tmp/before.json \
                           --save /tmp/after.json
```

The second run prints a table of every benchmark with its percent change in mean time, tagged `REGRESSION` / `IMPROVED` / `same` / `NEW` / `REMOVED`, plus a `(SEMANTIC CHANGE)` marker on any `equal?` / hash scenario whose boolean result differs between the two runs. The process exits non-zero if there is any regression beyond `--threshold` percent (default 10) or any semantic change, so it's usable as a CI gate.


<a id="org21c6fed"></a>

# Reading the equality benchmarks specifically

Each of the 7 scenarios (`identical-object`, `same-content-same-shape`, `same-content-typed-shape`, `same-content-fragmented-shape`, `differ-at-start`, `differ-at-end`, `differ-in-middle`) contributes three benchmarks:

-   `equal?/<type>/<scenario>` &#x2013; the comparison itself.
-   `hash-code.warm/<type>/<scenario>` &#x2013; `equal-hash-code` on one fixture reused across all trials. This is where a memoization cache's steady-state benefit shows up, from the second trial on.
-   `hash-code.cold/<type>/<scenario>` &#x2013; `equal-hash-code` on a **fresh** fixture every trial, no warmup. This is the honest first-call cost; a memoizing implementation cannot hide behind its own cache here.

Comparing `hash-code.warm` vs. `hash-code.cold` within a single run already tells you whether an implementation is memoizing at all. Comparing either one across a before/after pair of runs tells you the actual cost of switching to content-based hashing.


<a id="org3c64750"></a>

# Timer resolution and batching

Early versions of this suite timed each trial with `time-apply`, which only resolves to whole milliseconds. Any single call faster than `1ms` was therefore invisible to it: nearly every trial read back exactly `0`, with an occasional `1` from scheduling jitter crossing the rounding boundary, and the reported "mean" ended up measuring nothing but how often that jitter happened (visible as means like `0.067ms` built entirely out of `0~s and one ~1`) — not the actual cost of the operation.

The fix, in `bench-core.rkt`:

-   Timing now uses `current-inexact-monotonic-milliseconds`, a flonum clock with sub-millisecond resolution, instead of the integer-ms `time-apply~/~current-process-milliseconds~/~current-gc-milliseconds` deltas.
-   For warm benchmarks, `run-trials` first calibrates a repeat count (`calibrate-reps`): it doubles the number of back-to-back calls per measurement until one batch takes at least `--min-batch-ms` (default `5ms`), then uses that same batch size for every trial, dividing the batch's total time by the repeat count to get a per-call estimate. This is the standard fix for operations too fast for a clock's own resolution or call overhead to be negligible against — amortize a fixed cost over enough repetitions and it disappears.
-   Cold (`#:fresh?`) benchmarks are deliberately not batched — batching would call the operation on the same fixture more than once, and every call after the first would no longer be cold. They still benefit from the higher-resolution clock, just as single unbatched measurements, so they remain the more precision-limited of the two (accurately: a cold call's cost really does carry more run-to-run variance than a warm, batched one).

If results still look suspiciously flat, raise `--min-batch-ms`; if a full run takes too long, lower it (at some point precision trades off against wall-clock time, same as any calibrated microbenchmark).


<a id="orge391b8f"></a>

# Options

| Flag             | Meaning                                                         |
|------------------|-----------------------------------------------------------------|
| `--sizes`        | Comma-separated input sizes (default `0,8,64,512,4096,32768`)   |
| `--trials`       | Measured trials per benchmark (default 15)                      |
| `--warmup`       | Untimed priming calls per benchmark (default 5)                 |
| `--suites`       | Comma-separated suite names (default: all)                      |
| `--label`        | Label recorded with this run                                    |
| `--save`         | Write results to a JSON file                                    |
| `--baseline`     | Compare against a previously `--save`'d JSON file               |
| `--threshold`    | Percent mean-time increase counted as a regression (default 10) |
| `--no-gc`        | Skip the forced GC before each trial (faster, noisier)          |
| `--min-batch-ms` | Min ms per calibrated warm-benchmark batch (default 5)          |
| `--quiet`        | Suppress per-benchmark progress lines on stderr                 |


<a id="orgc11b01f"></a>

# Visualizing results

`visualize.rkt` turns a `--save`'d JSON run into a single SVG chart:

```sh
racket benchmark/main.rkt --save /tmp/run.json
racket benchmark/visualize.rkt --in /tmp/run.json --out /tmp/run.svg
```


<a id="orgbd8951d"></a>

## Design

-   **One row per operation** (every distinct `group~/~name` pair &#x2013; roughly 70 for the default suites, not the 600+ individual group/name/size records), **one dot per input size** it was measured at, all placed on **one shared log-scale time axis**. Time spans nanoseconds to hundreds of milliseconds across this suite &#x2013; 7-8 orders of magnitude &#x2013; so a linear axis would be useless:
    -   **absolute speed**: read a dot's x-position straight off the axis.
    -   **relative speed**: rows are sorted fastest-to-slowest (by geometric mean across sizes), and every row shares the exact same scale, so scanning top-to-bottom **is** the full ranking.
    -   Dots within a row are connected by a thin line, so a row's **horizontal spread** also shows scaling behavior at a glance: a tight cluster is `O(1)`-ish, a wide rightward smear across sizes is closer to `O(n)`.
-   **Precision**: each dot carries a thin whisker spanning its observed `[min, max]` sample range (not just the mean point estimate), and the fastest and slowest dot in each row are labeled with their exact duration in an adaptive unit (ns/us/ms/s) at 3 significant figures. Dot **position** on the continuous log scale retains full precision regardless; the text labels are there for the reader who wants an exact number without eyeballing the axis.
-   **Color** = operation category (`group`, e.g. `core/build`, `equality/string`), via a small swatch to the left of each row's label and a wrapped legend at the top.
-   Output is **SVG**.


<a id="org42ad9e6"></a>

## Options

| Flag           | Meaning                               |
|----------------|---------------------------------------|
| `--in`         | Input JSON file (required)            |
| `--out`        | Output SVG path (default: `<in>.svg`) |
| `--width`      | Canvas width in px (default 1400)     |
| `--row-height` | Pixels per operation row (default 20) |
