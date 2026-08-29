# Knuth reference gallery (private annex)

This directory is a private visual-study collection. The PNG page
rasterizations and JPG contact sheets are git-annex symlinks; their bytes must
never be committed to ordinary Git or copied to a public/shared annex remote.
`SOURCES.md` files remain ordinary Git objects so provenance and reuse caveats
survive without the raster content.

On `swa`, the `swa-private` directory special remote stores the durable content
copy under `/srv/luv-private-annex` (mode 0700, owned by `mbrock`). `origin` is
configured with `annex-ignore=true`. The root-run deployment helper obtains and
verifies the content from that remote, resolves the annex symlinks into a
release under `/srv/luv-private-gallery`, and atomically switches `current`.
Caddy can read only the materialized gallery, not the annex remote, and serves
it only behind the existing private Basic Auth gate at `/private/knuth/`.

To refresh the server after changing the tracked gallery or annex pointers:

```sh
sudo /usr/local/libexec/luv-private-gallery
```

Normal wiki deployment invokes the same helper before building the inactive
slot. An absent `swa-private` remote or missing annex key fails deployment
rather than publishing a partial gallery.
