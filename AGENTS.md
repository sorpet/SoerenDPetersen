# Repository Guidance

## CV Data Source Of Truth

Google Drive is the canonical source for structured CV data. In this repo,
`cv/data/snapshots/` is a committed cache used for offline and reproducible
renders, and `cv/content_html/` is generated output.

When changing CV records:

1. Edit the source record in Google Drive first.
2. Run `Rscript --vanilla cv/scripts/sync_drive_snapshots.R --check` from the
   repository root to check whether snapshots drift from Drive.
3. Run `Rscript --vanilla cv/scripts/sync_drive_snapshots.R --apply` to refresh
   raw synced snapshots from Drive.
4. Run `Rscript --vanilla cv/scripts/render_cv_fragments.R` to regenerate
   committed HTML fragments.
5. Render the CV with `quarto render cv/index.qmd --no-cache` when validating
   the final page.

Do not hand-edit files in `cv/data/snapshots/` or `cv/content_html/` except for
emergency repairs. Any emergency repair must be backfilled to the canonical
Drive source immediately, then re-synced and regenerated.

Some CV sections are projections from richer Drive data rather than raw copies:
funding is based on Drive funding/proposal records, and work experience is based
on the Drive jobs/work history records. Treat the Drive records as canonical and
the repo snapshot as the rendered CV projection cache.
