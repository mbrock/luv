# Equity fonts (private annex)

The OTFs in this directory are personally licensed assets. Git tracks only
git-annex symlinks; the font bytes must never be committed to ordinary Git or
uploaded to a public or shared annex remote. The only configured content copy
is the private annex on `swa`.

On `swa`, Typst can discover the present annex content with:

```sh
typst compile --font-path fonts/equity INPUT.typ OUTPUT.pdf
```

An authorized machine without the content needs a separately approved private
transfer arrangement; `git annex get fonts/equity` cannot fetch these fonts
from GitHub.
