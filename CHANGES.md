## Release v0.17.0
- New functions introduced:
  - `Incr_map.merge_disjoint` merges two maps with disjoint keys
  - `Incr_map.observe_changes_exn` observes changes in a map across stabilizations

## Release v0.16.0

Incr_map

- New functions
  * `cutoff` - applies a cutoff to the values in an incremental map
  * `partition_mapi'` - like `partition_mapi`, but the callback function has an
    incremental input and output
  * `unordered_fold_with_extra` - like `unordered_fold`, but depends on an arbitrary extra
    incremental value that can be factored into the folding computation
  * `merge_both_some` - like `merge`, but the mapping callback function is only called on
     keys contained in both maps

- New arguments
  * Add `Instrumentation` module and optional `instrumentation` argument to all `Incr_map`
    functions, which can be used for performance profiling
  * Add optional `finalize` argument to `unordered_fold`, which can be used to change the
    order in which fold operations are processed

- Bug fixes
  * Fix function `subrange_by_rank` to handle unbounded lower bound correctly
  * Fix typo in function name `mapi_mn` to `map_min`

Erase_key

- Small new library for type-erasing the key from an incremental map without introducing
  an existential type

Collate

- Remove the `Map_list` library
  * Switch all prior uses of `Map_list` to the more general `Incr_map_erase_key`

- Make `Incr_map_collate` no longer a functor

- Fold
  * Add module `Fold` defining a fold callback function
  * Add function `collate_and_fold`, like `collate` but allows you to perform a fold
    over the post-filtered, pre-range-restricted data
  * Add function `collate_and_fold__sort_first`, like `collate__sort_first` but allows you
    to perform a fold over the post-filtered, pre-range-restricted data
