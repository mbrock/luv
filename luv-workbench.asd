(asdf:defsystem "luv-workbench"
  :description "The complete interactive Luv development world."
  ;; This is deliberately the one compatibility manifest for today's system
  ;; graph. As the product roots become honest umbrellas, only this list needs
  ;; to shrink; Sly and the dependency-core builder load LUV-WORKBENCH alone.
  :depends-on ("luv" "luvcraft" "luvcraft/agent" "luvcraft/birthday"
               "luv-wiki" "luft/render" "rove"))
