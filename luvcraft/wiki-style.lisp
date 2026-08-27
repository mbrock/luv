(in-package #:luvcraft.web)
(named-readtables:in-readtable luv.css:syntax)

(css:define-style showcase
  "The capture catalog inside the wiki frame."
  (".showcase-page main" :max-width 76rem)
  (".showcase-page .source-revision" :color --muted)
  (".showcase-grid"
   :display grid
   :grid-template-columns (css:repeat 'auto-fit (css:minmax "min(100%, 28rem)" 1fr))
   :gap 1.5rem)
  (".showcase-grid article"
   :overflow hidden
   :border 1px solid --rule
   :border-radius 0.8rem
   :background --code-bg)
  (".showcase-media"
   :aspect-ratio 3/2
   :background "#080a08"
   :display grid
   :place-items center)
  ((".showcase-media .original" ".showcase-media img" ".showcase-media video")
   :display block :width 100% :height 100%)
  ((".showcase-media img" ".showcase-media video") :object-fit cover)
  (".showcase-media.portrait"
   :aspect-ratio 9/16 :width "min(100%, 27rem)" :margin-inline auto)
  ((".showcase-media.portrait img" ".showcase-media.portrait video")
   :object-fit contain)
  ((".showcase-grid article h2" ".showcase-grid .capture-figure")
   :margin-left 1.2rem :margin-right 1.2rem)
  (".showcase-grid .capture-figure"
   :font 700 0.78rem/1 --mono-font :color --accent :margin-bottom 0.3rem)
  (".showcase-grid article h2"
   :margin-top 0 :margin-bottom 1.2rem :font-size 1.1rem))

(css:define-style body-playground
  "The live analytic-body cabinet inside the wiki frame."
  (".body-playground main" :max-width none :padding 0 :margin 0)
  (".body-layout"
   :min-height "calc(100vh - 10rem)"
   :display grid
   :grid-template-columns (css:minmax 0 1.65fr) (css:minmax 19rem 0.75fr))
  (".body-stage"
   :position sticky :top 2.1rem :height "calc(100vh - 2.1rem)"
   :min-height 34rem :overflow hidden :background "#242824"
   :border-right 1px solid --rule)
  (".body-stage::after"
   :content "\"\"" :position absolute :inset 0 :pointer-events none
   :background "linear-gradient(180deg, rgb(0 0 0 / 18%), transparent 26%, transparent 70%, rgb(0 0 0 / 28%))")
  ("#body-canvas"
   :width 100% :height 100% :display block :touch-action none)
  (".body-stage-copy" :position absolute :z-index 2 :top 2.5rem :left 2.8rem)
  (".body-stage-copy h1"
   :margin 0.15rem 0 0.25rem
   :font 700 "clamp(2.3rem, 6vw, 5.8rem)/.92" --display-font
   :letter-spacing -0.055em)
  (".body-stage-copy #status"
   :margin 0.8rem 0 0 :color --muted :font-size 0.78rem :text-align start)
  (".body-playground .eyebrow"
   :margin 0 :color --accent :text-transform uppercase
   :letter-spacing 0.16em :font-size 0.68rem :font-weight 700 :text-align start)
  (".orbit-hint"
   :position absolute :z-index 2 :left 2.8rem :bottom 2rem :margin 0
   :color --muted :font-size 0.7rem :letter-spacing 0.06em)
  (".body-workbench"
   :min-height "calc(100vh - 2.1rem)" :display flex :flex-direction column
   :padding 2.5rem "clamp(1.4rem, 3vw, 3rem)" 1.5rem)
  (".body-workbench header" :padding-bottom 1.75rem :border-bottom 1px solid --rule)
  ("#body-picker" :display flex :flex-wrap wrap :gap 0.55rem :margin-top 1rem)
  (".body-playground button" :color inherit :font inherit)
  (("#body-picker button" "#reset")
   :border 1px solid --rule :border-radius 999px :background transparent
   :padding 0.58rem 1rem :cursor pointer)
  ("#body-picker button[aria-current=\"true\"]"
   :border-color --accent :background --accent :color --paper)
  (".body-playground button:hover" :border-color --accent)
  (".knobs" :flex 1 :padding 0.5rem 0 2rem)
  (".knob" :padding 1rem 0 0.9rem :border-bottom 1px solid --rule)
  (".knob-line"
   :display grid :grid-template-columns 1fr auto :gap 1rem :align-items baseline)
  (".knob label" :font 650 1rem/1.2 --display-font)
  (".knob output" :color --accent :font 650 0.72rem/1 --mono-font)
  (".knob p"
   :margin 0.35rem 0 0.75rem :color --muted :font-size 0.7rem
   :line-height 1.45 :text-align start)
  ("input[type=\"range\"]" :width 100% :accent-color --accent :cursor ew-resize)
  (".body-workbench footer"
   :position sticky :bottom 0 :display flex :align-items center
   :justify-content space-between :gap 1rem :padding-top 1rem
   :background --paper :color --muted :font-size 0.65rem)
  ("#reset" :padding 0.45rem 0.8rem :color --muted)
  (:media "screen and (max-width: 760px)"
   (".body-layout" :display block)
   (".body-stage"
    :position relative :top auto :height 62vh :min-height 25rem
    :border-right 0 :border-bottom 1px solid --rule)
   (".body-stage-copy" :top 1.5rem :left 1.4rem)
   (".orbit-hint" :left 1.4rem :bottom 1rem)
   (".body-workbench" :min-height auto :padding 1.5rem 1.4rem)))
