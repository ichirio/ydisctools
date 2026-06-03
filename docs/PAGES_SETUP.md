# GitHub Pages Setup

This repository includes a workflow to publish `docs/` to GitHub Pages:

- Workflow file: `.github/workflows/pages.yaml`
- Publish source: GitHub Actions artifact (`docs/`)

## One-time repository setting

1. Open repository settings on GitHub.
2. Go to **Settings > Pages**.
3. In **Build and deployment**, set **Source** to **GitHub Actions**.

## Publish/update docs

- Push any change under `docs/` to `main`.
- Or run the **Deploy Pages** workflow manually from Actions.

## Expected site URL

- <https://ichirio.github.io/ydisctools/>
