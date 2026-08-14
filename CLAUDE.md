# CLAUDE.md — Development Notes for AI-Assisted Work

This file documents the process of developing the pipeline for this project using Claude Code, including workflow lessons and coding conventions established along the way.

---

### Before Anything Else: Check the Environment

Data and results for this project generally live **outside** this repository. Before concluding that a file or dataset is unreachable, check the session environment:

- **`env | cut -d= -f1 | sort`** — look for `GDRIVE_SA_KEY` (Google Drive service-account credential) and `OVERLEAF_GIT_TOKEN` (Overleaf push access). Filter the environment by *listing* it, not by grepping for guessed substrings.
- **Google Drive** — check for an **`openBISG*`** folder; if available, it contains a `readme.md` with further instructions, along with the working data and results.

`GDRIVE_SA_KEY` is a **base64-encoded service-account JSON key** with access to the `cldata` shared drive. There is no Google Drive MCP connector in these sessions and the drive is not mounted — go through the Drive REST API, passing `supportsAllDrives`, `includeItemsFromAllDrives`, and `corpora=allDrives` so shared-drive items are visible. `CLOUDSDK_AUTH_ACCESS_TOKEN` is a proxy placeholder and does **not** authenticate against googleapis.com.

Nothing retrieved from these locations may be committed to this repository or included in a pull request — openBISG is a public package.

---

### Installing Dependencies in Claude Code Sessions

The Claude Code web environment runs **Ubuntu 24.04 (Noble)**. Python 3.11 and pip are pre-installed; `matplotlib` and `pandas` are **not** pre-installed and must be installed via pip (see Step 1 below). R and LaTeX are also **not** pre-installed. The apt package lists ship from the image build date and are stale, so `apt-get update` is **required** before any install (without it, `apt-get install` fails with exit code 100).

**Step 1 — apt-get update + install (single command):**

```bash
apt-get update -qq && apt-get install -y -qq --fix-missing texlive-latex-extra r-base r-base-dev r-cran-tidyverse r-cran-haven r-cran-survey libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev > /dev/null 2>&1
```

This installs:
- **LaTeX**: `texlive-latex-extra` (pulls in `texlive-latex-base`, `texlive-latex-recommended`, `texlive-plain-generic` automatically). Provides `pdflatex`, beamer, metropolis theme, owl color theme, booktabs, tikz, adjustbox, multirow, appendixnumberbeamer, and soul.
- **R + packages**: `r-base` + `r-base-dev` + `r-cran-tidyverse` + `r-cran-haven` + `r-cran-survey` as pre-compiled Ubuntu debs. This pulls in ~661 r-cran debs covering all compiled tidyverse dependencies. No source compilation required.
- **System libs**: Required for compiling the few R packages not available as debs (`srvyr`). Without them, `install.packages()` fails.

**Do NOT install `texlive-fonts-recommended` or `texlive-fonts-extra`.** The slides only use the Lato font (beyond standard CM/AMS fonts). `texlive-fonts-extra` is a 629 MB download and ~1.7 GB installed — replaced by Step 2 below (~12 MB, ~9 seconds).

**Install `matplotlib` and `pandas` via pip** — fastest path in this environment (the system `python3` is 3.11 with no PEP 668 lock, so pip pulls pre-built manylinux wheels with no `--break-system-packages` flag and no compilation, ~10–20 seconds):

```bash
pip install matplotlib pandas > /dev/null 2>&1
```

This can run as a separate parallel command alongside the apt-get step above (pip and apt use independent locks). Do NOT install `python3-matplotlib` / `python3-pandas` via apt — apt pulls in ~200 MB of dependencies and still requires the `apt-get update` step.

**Important: Do NOT split apt-get into multiple parallel calls.** `apt-get` acquires a global dpkg lock (`/var/lib/dpkg/lock-frontend`), so two concurrent `apt-get` processes will always conflict. The `apt-get update` and `apt-get install` must also be sequential (chained with `&&`), since install depends on fresh package lists.

**Do not use r2u, PPM, or CRAN for tidyverse.** r2u's GPG keyserver is unreliable and targets Ubuntu 22.04 (Jammy), not 24.04 (Noble). Posit Package Manager (PPM) `__linux__/noble` still requires source compilation for 49 of tidyverse's dependencies (~14 minutes, may fail on ragg). CRAN compiles everything from source (~30+ minutes). Ubuntu 24.04's native `r-cran-*` debs are the fastest path — pre-compiled, no compilation, no lock file issues.