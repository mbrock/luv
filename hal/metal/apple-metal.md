# Apple Metal documentation

Luv targets the Metal 4 API shipped in the selected Xcode SDK.  The SDK's
framework headers are the local source of truth for Objective-C selectors,
argument and result types, availability, constants, and Apple's API comments.
They are already installed with Xcode, require no network crawl, and stay in
step with `xcrun`.

Use the repo-native reader from the project root:

```sh
scripts/metal-doc path
scripts/metal-doc list MTL4
scripts/metal-doc find barrierAfterQueueStages
scripts/metal-doc show MTL4CommandEncoder.h
```

The tool resolves the active SDK with
`xcrun --sdk macosx --show-sdk-path`; it does not assume that Xcode lives in
`/Applications`.  Search results include both declarations and nearby Apple
comments.  This gives local access to the whole framework corpus without
vendoring Apple's SDK headers or waiting on documentation-site rate limits.

For conceptual material that isn't present in headers, use Apple's live
documentation:

- [Understanding the Metal 4 core API](https://developer.apple.com/documentation/metal/understanding-the-metal-4-core-api)
- [Resource synchronization](https://developer.apple.com/documentation/metal/resource-synchronization)
- [Metal feature set tables](https://developer.apple.com/metal/capabilities/)

## Luv's currently relevant contracts

- `MTL4CommandEncoder.h` defines explicit producer, consumer, and intra-pass
  barriers.  Work on `MTL4CommandQueue` does not inherit the older automatic
  resource-hazard behavior.
- `MTL4ArgumentTable.h` defines the resource binding table used by Metal 4
  encoders.
- `MTL4CommandQueue.h` and `MTL4CommandBuffer.h` define allocator lifetime,
  queue submission, drawable waits and signals, and shared-event ordering.
- `MTL4RenderCommandEncoder.h` and `MTL4ComputeCommandEncoder.h` define luv's
  render and blit command vocabulary.

When an implementation decision depends on one of these contracts, cite the
header and selector in the code comment.  Keep copied excerpts short; the
installed SDK remains the complete reference.
