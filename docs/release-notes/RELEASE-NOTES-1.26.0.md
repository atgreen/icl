# ICL 1.26.0 Release Notes

## Summary

Feature release: `--notebook` now scaffolds a notebook straight from a data
file, and the scaffolding is user-extensible per file extension.

## New Features

### Data-file notebook templates

- Pointing `--notebook` (or `-b`) at a **data file** — e.g. `icl --notebook
  data.csv` — scaffolds a fresh notebook wired to load it, chosen by the
  file's extension, and saves it as a sibling `.iclnb` (the data file itself
  is never touched).
- `csv` and `tsv` ship built-in: the generated notebook loads
  [Lisp-Stat](https://lisp-stat.dev/) and reads the file into `*df*`, which
  renders as an interactive, pivotable grid.
- **Extensible.** Register your own template for any extension in `~/.iclrc`
  with `icl:register-notebook-template`. A template is just a function of the
  data file's pathname returning a list of cell specs, each a plist
  `(:kind :code|:markdown :source STRING)`:

  ```lisp
  (icl:register-notebook-template "json"
    (lambda (path)
      (list (list :kind :code
                  :source (format nil "(defparameter *data* (load-json ~S))"
                                  (namestring path))))))
  ```

- A notebook-openable positional (an `.iclnb`, or a data file with a
  registered template) now turns on notebook mode even under `-b`, and is no
  longer forwarded to the inferior Lisp — so `icl -b data.csv` just works
  instead of failing to start.

## Public API

- `icl:register-notebook-template`, `icl:notebook-template-for`,
  `icl:make-notebook-from-template`, and `icl:*notebook-templates*` are
  exported for use from `~/.iclrc`.
