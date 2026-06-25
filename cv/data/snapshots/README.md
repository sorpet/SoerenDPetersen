# CV Snapshot Cache

Files in this directory are committed cache files for CV rendering. Google
Drive is canonical for the structured CV source records.

Normal workflow:

```bash
Rscript --vanilla cv/scripts/sync_drive_snapshots.R --check
Rscript --vanilla cv/scripts/sync_drive_snapshots.R --apply
Rscript --vanilla cv/scripts/render_cv_fragments.R
```

Do not hand-edit these snapshots during normal work. Edit the corresponding
Google Drive source first, then sync the snapshot cache.

Projection snapshots such as `funding.csv` and `work_experience.csv` are CV
presentation caches derived from richer Drive records. Keep the Drive source
records canonical and regenerate the projection intentionally.
