# CV

The CV is rendered as a normal Quarto page in the parent website. The maintained presentation files are:

- `index.qmd`
- `cv.css`

The section fragments in `content_html/` are committed so normal site renders do not require R, Google Drive, or network access.

## CV Data Sources

Google Drive is canonical for structured CV data. The repo-local files in
`data/snapshots/` are committed cache files used by the render pipeline, and the
files in `content_html/` are generated fragments.

Do not hand-edit `data/snapshots/*` or `content_html/*` during normal work.
Edit the source record in Google Drive first, then sync snapshots and regenerate
fragments. Emergency repo repairs are okay only when they are immediately
backfilled to Drive.

`data/paths.yml` records the canonical Drive file for each source. Most
snapshots are raw synced from Drive CSV/BibTeX files. Funding and work
experience are CV projection snapshots derived from richer Drive records, so the
Drive records remain canonical even when the committed snapshot is shaped for
CV rendering.

## Sync CV Snapshots

From the repository root:

```bash
Rscript --vanilla cv/scripts/sync_drive_snapshots.R --check
```

Use `--apply` to refresh raw synced snapshots from Drive:

```bash
Rscript --vanilla cv/scripts/sync_drive_snapshots.R --apply
```

The sync script does not regenerate HTML fragments.

## Regenerate CV Fragments

The R generator reads repo-local snapshots from `data/snapshots/` by default. Google Drive and absolute local paths are kept in `data/paths.yml` only as refresh metadata.

If the local R environment has not been restored yet:

```bash
Rscript --vanilla -e 'renv::restore(project = "cv")'
```

Regenerate the committed fragments from the repository root:

```bash
Rscript --vanilla cv/scripts/render_cv_fragments.R
```

Render just the CV page:

```bash
quarto render cv/index.qmd --no-cache
```

Render the full website:

```bash
quarto render
```

## Hygiene

Do not put the old standalone CV site back into this directory. The Quarto CV pipeline intentionally excludes the legacy HTML shell, Node package tree, browser loader, and markdown prebuild script.
