# ICL 1.26.0 Release Notes

## Summary

Feature release: `--notebook` scaffolds a notebook straight from a data file
(extensible per extension), and a new `,sql` command queries data with DuckDB —
files, session data frames, and attached SQLite/Postgres/DuckDB databases,
joinable in one query. The browser interface can be embedded in a local page
(`--embed-origin`), and `,dis`, `,viz` and the class graph work with
applications' own packages.

## New Features

### Data-file notebook templates

- Pointing `--notebook` (or `-b`) at a **data file** — e.g. `icl --notebook
  data.csv` — scaffolds a fresh notebook wired to load it, chosen by the
  file's extension, and saves it as a sibling `.iclnb` (the data file itself
  is never touched).
- `csv` and `tsv` ship built-in: the generated notebook reads the file into
  `*df*` through the `,sql` read adaptor (`icl-sql:read-source`), so it becomes a
  Lisp-Stat `data-frame`, a vellum table, or plists depending on which library
  the first cell loads (Lisp-Stat by default) — rendered as an interactive,
  pivotable grid either way.
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

### `,sql` — query data with DuckDB

- `,sql <query>` runs SQL via DuckDB. It reads CSV/Parquet/JSON files directly
  (`FROM 'data.csv'`) and can reference session data frames by name. It evaluates
  in the backend, so the terminal prints a table and leaves the result in `*` for
  your next form (`-o NAME` also binds `*NAME*`); in a notebook cell the result
  renders as an interactive grid. Cells are syntax-highlighted as SQL.
- **Result adaptors.** `--adaptor NAME` (short `-a`) chooses the result type:
  `auto` (the default) yields a [Lisp-Stat](https://lisp-stat.dev/) `data-frame`
  or a [vellum](https://github.com/sirherrbatka/vellum) table if either is loaded
  (ready to pipe straight into those libraries), else a list of plists; `df`,
  `vellum`, and `plist` force a specific form. All render as the same grid.
  Neither library is a dependency — availability is detected at runtime.
- **Adaptors are an extension point.** The adaptors are a small CLOS protocol
  (`icl-sql:build-result` / `result->rows` / `read-source`) that ICL injects into
  the backend on first use. Register your own from `~/.iclrc` with
  `icl:register-sql-adaptor` — the shortest form is a one-liner via
  `icl-sql:define-simple-adaptor`:

  ```lisp
  (icl:register-sql-adaptor "names"
    "(icl-sql:define-simple-adaptor \"names\" (rows)
       (mapcar (lambda (r) (getf r :name)) rows))")
  ```
- **Parameterize with Lisp values.** `${LISP-FORM}` in a query is evaluated in
  the backend and spliced as a SQL literal (numbers inline, strings quoted, lists
  as `(a, b)` for `IN`) — always a literal, so a string value can't inject SQL:
  `,sql SELECT * FROM 'people.csv' WHERE age > ${*min-age*}`.
- **Attach other databases.** DuckDB federates, so one query can reach SQLite,
  Postgres, and DuckDB files alongside your files and data frames — joinable
  together. Register sources two ways:
  - **Global** (all sessions), in `~/.iclrc`:
    ```lisp
    (icl:register-sql-source "cache" :sqlite "/var/tmp/cache.db")
    (icl:register-sql-source "pg" :postgres "host=localhost dbname=app user=me")
    ```
    or the `,attach` command: `,attach cache sqlite /var/tmp/cache.db`.
  - **Notebook-local**, as `,attach` cells saved with the notebook and scoped to
    it:
    ```
    ,attach pg postgres host=localhost dbname=app user=me
    ,sql SELECT u.name, o.total FROM pg.public.users u
         JOIN cache.orders o USING (user_id)
    ```
  Conninfo may use `${ENV_VARS}`; passwords come from the environment
  (`PGPASSWORD`) or `~/.pgpass`, never the notebook. Requires the `duckdb` CLI on
  PATH; the `sqlite`/`postgres` extensions auto-install on first use.

### Notebook cell highlighting

- Code cells now stay **syntax-highlighted when not being edited** — the resting
  display is a colorized `<pre>` (click it to open the ICL editor as before), so
  a cell looks the same whether or not it's focused.
- Highlighting uses the **ICL editor's own theme colors** (Lisp *and* SQL) and
  font, so the resting and editing views match. `,sql` cells are highlighted as
  SQL. No Monaco dependency — a self-contained tokenizer.

### Embedding the browser interface

- `--embed-origin ORIGIN` (repeatable) lets a page at that loopback origin show
  ICL's browser interface in a frame, such as a local dashboard that hosts ICL
  in a tab (`icl -b --no-open --embed-origin http://127.0.0.1:8080`). ICL
  otherwise allows only itself to frame the page. Only `127.0.0.1`,
  `localhost` and `[::1]` origins are accepted, so no remote site can be let
  frame a REPL.

## Bug Fixes

- `,dis` works on symbols of packages only the backend has, such as an
  application's (`,dis 'myapp.server:main`). ICL read the name in its own image
  and failed with "Package ... does not exist"; the backend now reads it.
- `,viz` of an object of an application's type says it doesn't know how to
  visualize it instead of failing: the type no longer comes back as a symbol
  ICL can't read.
- Class graph: two classes with one name (`TRIVIAL-GRAY-STREAMS:FUNDAMENTAL-STREAM`
  and the `SB-GRAY` class it refines) are separate nodes, labelled with their
  packages, instead of one node with an edge to itself; and the graph shows every
  ancestor, up to `T`.
- Class graph: a node's slot tooltip stays inside the window near the bottom or
  right edge.
- Browser REPL: the symbol under the cursor includes `.` and every other symbol
  constituent, so `my.package:sym` is one symbol.

## Build

- Windows builds again: Lisp sources are now checked out with LF line endings
  on every platform. With CRLF, a `FORMAT` string ending a line in `~` failed to
  compile.
- The Makefile and the `embedded-assets` ASDF component now treat
  `assets/*.js`/`*.css` as build inputs, so editing a browser asset (e.g.
  `notebook.js`) re-embeds it on the next `make` — previously a JS-only edit was
  silently missed.

## Dependencies

- Browser components: dockview 8.9.1, Vega-Lite 6.5.0, Vega-Embed 7.4.0 and
  Monaco 0.57.0. Mermaid stays at 11.16.1.
- Lisp dependencies brought to their latest ocicl versions: chipz, cl-cancel,
  float-features, idna, ironclad, pure-tls, serapeum, slynk, trivial-garbage,
  trivial-indent and tuition.

## Public API

- `icl:register-notebook-template`, `icl:notebook-template-for`,
  `icl:make-notebook-from-template`, and `icl:*notebook-templates*` are
  exported for use from `~/.iclrc`.
- `icl:register-sql-source` and `icl:*sql-sources*` register global `,sql`
  sources from `~/.iclrc`.
- `icl:register-sql-adaptor`, `icl:list-sql-adaptors`, and `icl:*sql-adaptors*`
  register and inspect `,sql` result adaptors.
