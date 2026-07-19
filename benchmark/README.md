- [Layout](#orga938071)
- [Running](#org9b68e1c)
- [Comparing before/after a change](#org39fd5e2)
- [Reading the equality benchmarks specifically](#org1339079)
- [Timer resolution and batching](#orgd7907cd)
- [Options](#orgbd98a9c)



<a id="orga938071"></a>

# Layout

| File                     | Responsibility                                                               |
|--------------------------|------------------------------------------------------------------------------|
| `bench-core.rkt`         | Timing/statistics primitives. Knows nothing about ropes.                     |
| `suite.rkt`              | The `bench` fixture/op abstraction; warm vs. cold execution.                 |
| `type-ops.rkt`           | Generic rope-type vocabulary (`rope-type-ops`) + shape/perturbation helpers. |
| `generators.rkt`         | Random raw-content generators + concrete `string-ops~/~bytes-ops`.           |
| `suites/generic-ops.rkt` | build/append/split/splice/slice/cursor/fold/sequence/shape benchmarks.       |
| `suites/equality.rkt`    | `equal?~/~equal-hash-code` benchmark matrix (7 scenarios × warm/cold).       |
| `suites/all.rkt`         | Suite registry main.rkt drives.                                              |
| `report.rkt`             | JSON I/O + A/B comparison + regression report.                               |
| `main.rkt`               | CLI entry point.                                                             |

Every benchmark is written once, generically, against `rope-type-ops` (see `type-ops.rkt`), and instantiated for both `rope/string` and `rope/bytes` in `generators.rkt`. A new rope type only needs one more `rope-type-ops` value to pick up every benchmark in this suite unmodified. This also means the exact same code runs, unmodified, before and after an implementation change, which is what makes an A/B comparison meaningful.


<a id="org9b68e1c"></a>

# Running

```sh
racket benchmark/main.rkt
racket benchmark/main.rkt --sizes 0,100,10000 --trials 30 --suites string-equality
```


<a id="org39fd5e2"></a>

# Comparing before/after a change

```sh
git stash                                   # back to the old implementation
racket benchmark/main.rkt --label before --save /tmp/before.json

git stash pop                               # bring the change back
racket benchmark/main.rkt --label after --baseline /tmp/before.json \
                           --save /tmp/after.json
```

The second run prints a table of every benchmark with its percent change in mean time, tagged `REGRESSION` / `IMPROVED` / `same` / `NEW` / `REMOVED`, plus a `(SEMANTIC CHANGE)` marker on any `equal?` / hash scenario whose boolean result differs between the two runs. The process exits non-zero if there is any regression beyond `--threshold` percent (default 10) or any semantic change, so it's usable as a CI gate.


<a id="org1339079"></a>

# Reading the equality benchmarks specifically

Each of the 7 scenarios (`identical-object`, `same-content-same-shape`, `same-content-typed-shape`, `same-content-fragmented-shape`, `differ-at-start`, `differ-at-end`, `differ-in-middle`) contributes three benchmarks:

-   `equal?/<type>/<scenario>` &#x2013; the comparison itself.
-   `hash-code.warm/<type>/<scenario>` &#x2013; `equal-hash-code` on one fixture reused across all trials. This is where a memoization cache's steady-state benefit shows up, from the second trial on.
-   `hash-code.cold/<type>/<scenario>` &#x2013; `equal-hash-code` on a **fresh** fixture every trial, no warmup. This is the honest first-call cost; a memoizing implementation cannot hide behind its own cache here.

Comparing `hash-code.warm` vs. `hash-code.cold` within a single run already tells you whether an implementation is memoizing at all. Comparing either one across a before/after pair of runs tells you the actual cost of switching to content-based hashing.


<a id="orgd7907cd"></a>

# Timer resolution and batching

Early versions of this suite timed each trial with `time-apply`, which only resolves to whole milliseconds. Any single call faster than `1ms` was therefore invisible to it: nearly every trial read back exactly `0`, with an occasional `1` from scheduling jitter crossing the rounding boundary, and the reported "mean" ended up measuring nothing but how often that jitter happened (visible as means like `0.067ms` built entirely out of `0~s and one ~1`) — not the actual cost of the operation.

The fix, in `bench-core.rkt`:

-   Timing now uses `current-inexact-monotonic-milliseconds`, a flonum clock with sub-millisecond resolution, instead of the integer-ms `time-apply~/~current-process-milliseconds~/~current-gc-milliseconds` deltas.
-   For warm benchmarks, `run-trials` first **calibrates** a repeat count (`calibrate-reps`): it doubles the number of back-to-back calls per measurement until one batch takes at least `--min-batch-ms` (default `5ms`), then uses that same batch size for every trial, dividing the batch's total time by the repeat count to get a per-call estimate. This is the standard fix for operations too fast for a clock's own resolution or call overhead to be negligible against — amortize a fixed cost over enough repetitions and it disappears.
-   Cold (`#:fresh?`) benchmarks are deliberately **not** batched — batching would call the operation on the same fixture more than once, and every call after the first would no longer be cold. They still benefit from the higher-resolution clock, just as single unbatched measurements, so they remain the more precision-limited of the two (accurately: a cold call's cost really does carry more run-to-run variance than a warm, batched one).

If results still look suspiciously flat, raise `--min-batch-ms`; if a full run takes too long, lower it (at some point precision trades off against wall-clock time, same as any calibrated microbenchmark).


<a id="orgbd98a9c"></a>

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
