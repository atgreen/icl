# ICL 1.26.0 Release Notes

## Summary

Feature release: `--notebook` scaffolds a notebook straight from a data file
(extensible per extension), and a new `,sql` command queries data with DuckDB —
files, session data frames, and attached SQLite/Postgres/DuckDB databases,
joinable in one query.

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

### `,sql` — query data with DuckDB

- `,sql <query>` runs SQL via DuckDB. It reads CSV/Parquet/JSON files directly
  (`FROM 'data.csv'`) and can reference session data frames by name. In the
  terminal it prints a table (`-o NAME` also binds the result to `*NAME*`); in a
  notebook cell it returns rows that render as an interactive grid. Cells are
  syntax-highlighted as SQL.
- **Attach other databases.** DuckDB federates, so one query can reach SQLite,
  Postgres, and DuckDB files alongside your files and data frames — joinable
  together. Register sources two ways:
  - **Global** (all sessions), in `~/.iclrc`:
    ```lisp
    (icl:register-sql-source "cache" :sqlite "/var/tmp/cache.db")
    (icl:register-sql-source "pg" :postgres "host=localhost dbname=app user=me")
    ```
    or the `,source` command: `,source cache sqlite /var/tmp/cache.db`.
  - **Notebook-local**, as `,source` cells saved with the notebook and scoped to
    it:
    ```
    ,source pg postgres host=localhost dbname=app user=me
    ,sql SELECT u.name, o.total FROM pg.public.users u
         JOIN cache.orders o USING (user_id)
    ```
  Conninfo may use `${ENV_VARS}`; passwords come from the environment
  (`PGPASSWORD`) or `~/.pgpass`, never the notebook. Requires the `duckdb` CLI on
  PATH; the `sqlite`/`postgres` extensions auto-install on first use.

## Public API

- `icl:register-notebook-template`, `icl:notebook-template-for`,
  `icl:make-notebook-from-template`, and `icl:*notebook-templates*` are
  exported for use from `~/.iclrc`.
- `icl:register-sql-source` and `icl:*sql-sources*` register global `,sql`
  sources from `~/.iclrc`.
