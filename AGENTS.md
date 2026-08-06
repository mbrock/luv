# Working style

This is a fast-moving experimental project. Optimize for iteration speed,
playful hacking, and frequent save points rather than ceremony.

After finishing a coherent round of work, commit the changes by default. Treat
`git commit` like **File → Save** for the project: a commit does not imply that
the work is polished, exhaustively reviewed, or ready for release. Small,
imperfect, exploratory commits are welcome and preferable to leaving work
uncommitted where it can be forgotten.

Before committing, do the checks that are quick and relevant to the change,
but do not turn every commit into a heavyweight verification or review cycle.
Use a short commit message that describes the iteration. Preserve unrelated
user changes, and never rewrite or discard existing work just to make a clean
commit.

# Live Lisp interaction

Use `./luv eval 'FORM'` to evaluate code in the project SLY image. The Common
Lisp client opens a fresh Slynk connection for every command; there is
intentionally no persistent protocol client. Start the `luv` SLY implementation
in Emacs first when the listener is not already running.
