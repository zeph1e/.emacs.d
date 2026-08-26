# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Requirements

Emacs 30 or newer is recommended.

On Emacs 30 and newer, Emacs loads `use-package` from its built-in copy. The built-in `package-vc` package provides the `:vc` keyword, and `config/claude.el` uses `:vc` to fetch packages from GitHub. On older Emacs, `init.el` bootstraps `straight.el` from `radian-software/straight.el` and installs `use-package` through it. The `:vc` keyword still works on older Emacs, because straight.el handles VC-installed packages.

## First launch

Several packages declare `:ensure-system-package` rules that install system binaries on first launch. Some need sudo, for example `python3-pylsp` through apt or `epdfinfo` through `elpa-pdf-tools-server`. Others install globally through npm, for example `typescript-language-server` and `vscode-langservers-extracted`. `plugins/use-package-ensure-system-package+` serializes all installs through one persistent `/bin/bash` worker, so sudo prompts appear once per session, not once per package. A blocked sudo prompt during startup can look like a hang. It is not a hang.

## Architecture

The configuration loads in a fixed sequence from `init.el`:

1. **Locale / UI / server** — early options, font, Emacs server start
2. **Package manager and use-package** — On Emacs versions before 30, `init.el` bootstraps `straight.el` (it clones into `straight/` under `user-emacs-directory`) and uses it to install `use-package`. straight.el also serves `:vc` requests on this path. On every Emacs version, `init.el` initializes the built-in `package.el` with the GNU, NonGNU, and MELPA archives, and sets `use-package-always-ensure` to `t`. For packages not on an archive, `init.el` pulls them in with `:vc` — `package-vc-install` on Emacs 30 and newer, straight.el on older versions.
3. **`workaround.el`** — Loads immediately after step 2. It holds targeted fixes for upstream Emacs and package bugs.
4. **`plugins/`** — Local packages not on ELPA or MELPA. Emacs byte-compiles them and generates autoloads on first load (see below).
5. **`config/*.el`** — One file per feature domain. Emacs loads the files in filesystem (alphabetical) order and byte-compiles them when Emacs quits.
6. **`custom.el`** — `M-x customize` generates this file automatically. Git excludes it from version control.

### `plugins/` load mechanism

On first load, Emacs byte-compiles each subdirectory and generates an autoloads file (`<dir>-autoloads.el`). A `.installed` sentinel file prevents Emacs from repeating this on later startups. **Delete `.installed` to force a rebuild after you edit any plugin `.el` file.**

### `config/*.el` conventions

Every config file uses `use-package`. Put all custom keybindings into `:map my:global-key-map`. Do not use `global-set-key` directly. `my:global-key-mode` is a minor mode that always sits at the head of `minor-mode-map-alist`, so custom bindings win over major-mode and minor-mode defaults. `my:reorder-keybindings-priority`, hooked onto `after-change-major-mode-hook`, re-asserts that order on every major-mode change, so the priority survives later package loads.

Emacs byte-compiles `config/*.el` files lazily, on `kill-emacs-hook`, only when the `.elc` file is missing or older than the `.el` file. **As a result, edits to a config file do not take effect until the next Emacs start.** Restart Emacs, or manually run `byte-compile-file` and reload, to pick up changes during a session.

### Default minor modes

`init.el` installs three lists onto every buffer:

- `my:default-minor-mode-list` (both prog and text buffers): `display-line-numbers-mode`, `my:whitespace-mode`
- `my:default-prog-minor-mode-list`: `flyspell-prog-mode`, `display-fill-column-indicator-mode`, `goto-address-prog-mode`, `indent-bars-mode`
- `my:default-text-minor-mode-list`: `visual-line-mode`, `flyspell-mode`, `goto-address-mode`

`global-hl-line-mode` is off in shell, eshell, and term buffers. `display-fill-column-indicator-mode` is off in `helm-major-mode`.

Two companion lists extend these minor modes to major modes that do **not** derive from `prog-mode` or `text-mode`:

- `my:custom-prog-mode-list` — currently `nil`. Add a major-mode symbol here (not a hook symbol) to give that mode the prog-mode minor modes.
- `my:custom-text-mode-list` — currently `'(conf-mode)`. Add a major-mode symbol here to give that mode the text-mode minor modes.

`config/company.el` also reads `my:custom-prog-mode-list` directly, so a mode you add there also gets the code-aware company backend split (see `config/company.el`).

### `plugins/use-package-ensure-system-package+`

This is the most complex local plugin. It serializes every `:ensure-system-package` install command through one persistent `/bin/bash` process, to avoid races between concurrent package managers. It exposes `upesp+:command-executed-hook` and calls it with the completed command string after each install. `config/vterm.el` uses this hook to block vterm module compilation until its system dependencies (gcc, cmake, libtool) finish installing. See its own `CLAUDE.md` for internals. `init.el` installs it directly through `:vc`, ahead of the `plugins/` and `config/` load, so it is always available before any `config/*.el` file that uses `:ensure-system-package`.

## Forcing Plugin Recompilation

```sh
rm plugins/<name>/.installed
```

Emacs will recompile and regenerate autoloads for that plugin on the next launch.

## Notable dependencies

`init.el` installs `block-travel` (VS Code-style block navigation, bound to `M-p` and `M-n` in `config/editor.el`), `rfcview` (`config/rfcview.el`, fetched from the author's own fork), and `use-package-ensure-system-package+` through `:vc`, like any other package. They land in `elpa/` (gitignored). Some `:vc`-installed packages under `elpa/` ship their own `CLAUDE.md` file, for example `elpa/use-package-ensure-system-package+/CLAUDE.md`, with package-internal guidance. Emacs loads that guidance automatically when you work in those directories.

## Building / Compiling

There is no top-level build step for the Emacs config itself. For in-Emacs compilation:

| Key | Command |
|-----|---------|
| `<f7>` | `my:compile` — calls `recompile` if a previous compilation buffer exists, otherwise prompts interactively |
| `C-<f7>` | Always opens the interactive `compile` prompt |

Defined in `config/compile.el`.

## Search keybindings

| Key | Command | Defined in |
|-----|---------|------------|
| `M-r` | `helm-occur` — in-buffer incremental search | `config/helm.el` |
| `M-R` | `helm-do-grep-ag` — grep across a directory tree | `config/helm.el` |
| `C-M-r` | `my:helm-do-grep-vc-root-ag` — same, rooted at the VCS repo root | `config/helm.el` |
| `C-M-S-r` | `helm-grep-do-git-grep` — `git grep` through helm | `config/helm.el` |
| `C-M-R` | `ag` (standalone `ag.el`) — **shadowed** by `C-M-S-r` above; same keysym, and `helm.el` loads after `ag.el` alphabetically, so this binding never takes effect | `config/ag.el` |

`xcscope` (loaded by `config/xcscope.el`) installs its own `C-c s` keymap for C/C++ symbol cross-referencing via `cscope-setup` — no entry in `my:global-key-map`.

## Key Files

| File | Purpose |
|------|---------|
| `init.el` | Entry point — orchestrates the entire load sequence |
| `workaround.el` | Early-loaded upstream bug fixes |
| `config/theme.el` | Mode line customization |
| `config/lsp.el` | LSP configuration; language server install hooks |
| `config/vterm.el` | vterm setup; blocks module compilation via `upesp+:command-executed-hook` until system deps install |
| `config/editor.el` | Small ergonomic editing commands |
| `config/window.el` | All window/frame navigation bindings |
| `config/fileviewer.el` | External file/URL opener integration; detects WSL / SSH-remote / local host and routes dired `V`, `browse-url`, and `mailcap` viewers accordingly |
| `config/claude.el` | Claude Code integration via `claude-code.el`; keybinding prefix `C-'`; uses `monet` for IDE server bridging (its `monet-diff-tool`/`monet-ediff-tool` custom block is commented out, so monet keeps its own default diff tool) and `inheritenv` for environment propagation; opens Claude in a right side window |
| `config/pdf.el` | PDF viewing via `pdf-tools`; requires system package `epdfinfo` (installed via `sudo apt install -y elpa-pdf-tools-server`) |
| `config/rust.el` | rust-mode config; includes cargo dependency-management commands (`my:rust-add-dependency`, searches the cargo registry) |
| `config/agent-shell.el` | agent-shell integration |
| `config/flycheck.el` | flycheck setup |
| `misc/edit` | Smart `emacsclient` wrapper; set `$EDITOR` to this |
| `.dir-locals.el` | Sets `fill-column` to 80 globally; in `emacs-lisp-mode`, registers a `write-contents-functions` hook that strips trailing whitespace on every save |

## Conventions to remember when editing this repo

- **Trailing whitespace is auto-stripped on save in `.el` files** (via `.dir-locals.el`). Expect diffs that touch only line endings.
- **Projectile's project search root is `~/Workspace`.** `helm-projectile` discovery starts there.
- **The repository has no git submodules.** Earlier revisions used submodules for `block-travel`, `magit-gerrit`, and `company-tern`; all of them were dropped in favor of `:vc`-installed packages. Do not reintroduce `.gitmodules`.
- **`rfcview` is a normal `:vc` package**, declared in `config/rfcview.el` and fetched from `https://github.com/zeph1e/rfcview.el` (the author's own fork). It is no longer a developer-local block at the tail of `init.el`.
