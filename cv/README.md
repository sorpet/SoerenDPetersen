# CV

The CV is rendered as a normal Quarto page in the parent website. The maintained presentation files are:

- `index.qmd`
- `cv.css`

The section fragments in `content_html/` are committed so normal site renders do not require R, Google Drive, or network access.

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
