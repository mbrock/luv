(in-package #:luft.atlas)
(named-readtables:in-readtable luv.css:syntax)

;;; Only the atlas component is new.  The surrounding page, palette, type,
;;; spacing, navigation, and dark scheme all come from the wiki stylesheet.

(css:define-style star-atlas
  "The selectable grid and geometry detail inside the wiki's star atlas."
  (".atlas-layout"
   :display grid
   :grid-template-columns (css:minmax 20rem 31rem) (css:minmax 0 1fr)
   :gap 1.4rem
   :align-items start)
  (".detail"
   :position sticky
   :top 3rem
   :border 1px solid --rule
   :border-radius 1rem
   :overflow hidden
   :background --paper
   :box-shadow 0 18px 45px (css:color-mix --ink 10% "transparent"))
  (".detail-heading"
   :padding 1rem 1.1rem 0.7rem
   :display flex
   :justify-content space-between
   :align-items baseline
   :gap 1rem)
  (".mask"
   :margin 0
   :font 700 1.6rem/1 --mono-font)
  (".bits"
   :color --muted
   :font 500 0.8rem/1 --mono-font
   :letter-spacing 0.12em)
  (".view-label"
   :display block
   :margin-top 0.35rem
   :color --muted
   :font 650 0.68rem/1 --display-font
   :letter-spacing 0.04em)
  ("#selected-canvas"
   :display block
   :width 100%
   :aspect-ratio 1.18
   :cursor grab
   :touch-action none
   :background --code-bg)
  ("#selected-canvas:active" :cursor grabbing)
  (".detail-controls"
   :padding 0.9rem 1.1rem 1.1rem
   :border-top 1px solid --rule
   :display grid
   :gap 0.85rem)
  (".view-modes"
   :margin 0
   :padding 0
   :border 0
   :display flex
   :gap 0.35rem
   ("legend"
    :position absolute
    :width 1px
    :height 1px
    :overflow hidden
    :clip-path "inset(50%)")
   ("label"
    :position relative
    :cursor pointer)
   ("input"
    :position absolute
    :opacity 0)
   ("span"
    :display block
    :padding 0.42rem 0.65rem
    :border 1px solid --rule
    :border-radius 0.45rem
    :color --muted
    :font 680 0.7rem/1 --display-font)
   ("input:checked + span"
    :border-color --accent
    :color inherit
    :background --code-bg))
  (".view-explanation"
   :max-width none
   :margin 0
   :color --muted
   :font-size 0.72rem
   :line-height 1.35)
  (".layers"
   :display flex
   :flex-wrap wrap
   :gap 0.55rem 1rem)
  (".layers label"
   :display inline-flex
   :align-items center
   :gap 0.38rem
   :font-size 0.78rem
   :font-weight 680)
  (".layers input" :accent-color --accent)
  (".swatch"
   :width 0.72rem
   :height 0.72rem
   :border-radius 50%)
  (".face-swatch" :background "#2d8fbd")
  (".band-swatch" :background "#e6aa35")
  (".junction-swatch" :background "#db6555")
  (".facts"
   :display grid
   :grid-template-columns auto 1fr auto auto auto
   :gap 0.75rem
   :align-items center)
  (".occupancy" :display flex :gap 0.45rem)
  (".occupancy-layer"
   :display grid
   :grid-template-columns (css:repeat 2 0.72rem)
   :grid-template-rows (css:repeat 2 0.72rem)
   :gap 2px)
  (".occupancy-cell"
   :border 1px solid --muted
   :background transparent)
  (".occupancy-cell.occupied"
   :background --accent
   :border-color --accent)
  (".cell-key"
   :display flex
   :flex-wrap wrap
   :gap 0.25rem 0.55rem
   :color --muted
   :font 650 0.62rem/1 --display-font
   :text-transform uppercase
   :letter-spacing 0.05em)
  (".cell-key span"
   :display inline-flex
   :align-items center
   :gap 0.25rem)
  (".cell-key i"
   :display inline-block
   :width 0.65rem
   :height 0.65rem
   :border 1px solid --muted)
  (".cell-key .solid-cell"
   :border-color --accent
   :background --accent)
  (".cell-key .air-cell" :background transparent)
  (".fact"
   :text-align center
   :font 700 0.85rem/1 --mono-font)
  (".fact small"
   :display block
   :margin-top 0.15rem
   :color --muted
   :font 600 0.61rem/1 --display-font
   :text-transform uppercase
   :letter-spacing 0.08em)
  (".stepper" :display flex :gap 0.35rem)
  (".stepper button"
   :border 1px solid --rule
   :background transparent
   :color inherit
   :border-radius 0.45rem
   :padding 0.35rem 0.6rem
   :cursor pointer
   ("&:hover" :border-color --accent))
  (".atlas-grid"
   :display grid
   :grid-template-columns (css:repeat 'auto-fill (css:minmax 10.5rem 1fr))
   :gap 0.65rem)
  (".star-family"
   :min-width 0
   :border 1px solid --rule
   :border-radius 0.7rem
   :overflow hidden
   :background --paper
   :transition "border-color .12s, transform .12s, background .12s"
   ("&:hover"
    :transform (css:translate 0 -1px)
    :border-color --accent)
   ("&.selected-family"
    :border-color --accent
    :background --code-bg
    :box-shadow 0 0 0 1px --accent))
  (".star-card"
   :display block
   :width 100%
   :min-width 0
   :padding 0
   :border 0
   :color inherit
   :background transparent
   :text-align left
   :cursor pointer
   ("canvas"
    :display block
    :width 100%
    :aspect-ratio 1.2))
  (".card-caption"
   :padding 0.42rem 0.55rem 0.5rem
   :border-top 1px solid --rule
   :display flex
   :justify-content space-between
   :align-items baseline
   :gap 0.5rem)
  (".card-mask"
   :font 720 0.78rem/1 --mono-font)
  (".card-orbit"
   :color --muted
   :font 600 0.62rem/1 --mono-font)
  (".family-orbit"
   :border-top 1px solid --rule
   ("summary"
    :padding 0.42rem 0.55rem
    :color --muted
    :font 650 0.68rem/1 --display-font
    :cursor pointer
    :user-select none))
  (".family-members"
   :padding 0 0.5rem 0.55rem
   :display grid
   :grid-template-columns (css:repeat 4 1fr)
   :gap 0.28rem)
  (".star-member"
   :border 1px solid --rule
   :border-radius 0.35rem
   :padding 0.28rem 0.2rem
   :color --muted
   :background transparent
   :font 650 0.64rem/1 --mono-font
   :cursor pointer
   ("&:hover" :border-color --accent :color inherit)
   ("&.selected"
    :border-color --accent
    :color inherit
    :background --paper))
  ("p.atlas-note"
   :max-width none
   :margin 1rem 0 0
   :color --muted
   :font-size 0.76rem
   :text-align start)
  (:media "screen and (max-width: 52rem)"
   (".atlas-layout" :grid-template-columns 1fr)
   (".detail" :position relative :top auto)
   (".atlas-grid"
    :grid-template-columns (css:repeat 'auto-fill (css:minmax 8.5rem 1fr)))))
