# CLAUDE.md

This file gives instructions to Claude Code (claude.ai/code) for work in this repository.

## Requirements

Use Emacs 30 or a later version.

On Emacs 30 and later versions, Emacs loads `use-package` from the built-in copy. The built-in `package-vc` package gives the `:vc` keyword. `config/claude.el` uses `:vc` to fetch packages from GitHub.

On older Emacs versions, `init.el` bootstraps `straight.el`, from `radian-software/straight.el`. Then `init.el` uses `straight.el` to install `use-package`. The `:vc` keyword still works on older Emacs, because `straight.el` installs packages from a VC too.

## First launch

Several packages declare `:ensure-system-package` rules that install system binaries on first launch. Some need sudo, for example `python3-pylsp` through apt or `epdfinfo` through `elpa-pdf-tools-server`. Others install globally through npm, for example `typescript-language-server` and `vscode-langservers-extracted`.

`plugins/use-package-ensure-system-package+` serializes all installs through one persistent `/bin/bash` worker. As a result, sudo prompts appear once for each session, not once for each package.

A blocked sudo prompt during startup can look like a hang. It is not a hang.

## Architecture

The configuration loads in a fixed sequence from `init.el`:

1. **Locale / UI / server**: early options, font, and Emacs server start.
2. **Package manager and use-package**: On Emacs versions before 30, `init.el` bootstraps `straight.el`, which clones into `straight/` under `user-emacs-directory`. Then `init.el` uses `straight.el` to install `use-package`. `straight.el` also serves `:vc` requests on this path. On every Emacs version, `init.el` starts the built-in `package.el` with the GNU, NonGNU, and MELPA archives. `init.el` also sets `use-package-always-ensure` to `t`. For packages that are not on an archive, `init.el` gets them with `:vc`: `package-vc-install` on Emacs 30 and later, `straight.el` on older versions.
3. **`workaround.el`**: Loads immediately after step 2. It holds targeted fixes for bugs in upstream Emacs and bugs in installed packages.
4. **`plugins/`**: Local packages not on ELPA or MELPA. Emacs byte-compiles them and makes autoloads for them on first load. See the `plugins/` load mechanism section that follows.
5. **`config/*.el`**: One file for each feature area. Emacs loads these files in alphabetical order by file name, and byte-compiles them when Emacs quits.
6. **`custom.el`**: `M-x customize` makes this file automatically. Git ignores this file.

### `plugins/` load mechanism

On first load, Emacs byte-compiles each subdirectory. Emacs also makes an autoloads file for it, named `<dir>-autoloads.el`. A `.installed` sentinel file stops Emacs from doing this again on later startups.

**After you edit any plugin `.el` file, delete `.installed` to force a rebuild.**

### `config/*.el` conventions

Every file in `config/*.el` uses `use-package`. Put all custom keybindings into `:map my:global-key-map`. Do not use `global-set-key` directly.

`my:global-key-mode` is a minor mode. It always sits at the head of `minor-mode-map-alist`. As a result, custom keybindings win over major-mode and minor-mode default keybindings.

`my:reorder-keybindings-priority` is hooked onto `after-change-major-mode-hook`. It sets this order again on every major-mode change. As a result, the priority survives later package loads.

Emacs byte-compiles `config/*.el` files only on `kill-emacs-hook`, and only when the `.elc` file is missing or older than the `.el` file.

**As a result, edits to a config file do not work until the next time you start Emacs.**

To use changes during the same session, restart Emacs. Or run `byte-compile-file` by hand, then reload the file.

### Default minor modes

`init.el` uses three lists to turn on minor modes for every buffer:

- `my:default-minor-mode-list` (both prog and text buffers): `display-line-numbers-mode`, `my:whitespace-mode`
- `my:default-prog-minor-mode-list`: `flyspell-prog-mode`, `display-fill-column-indicator-mode`, `goto-address-prog-mode`, `indent-bars-mode`
- `my:default-text-minor-mode-list`: `visual-line-mode`, `flyspell-mode`, `goto-address-mode`

`global-hl-line-mode` is off in shell, eshell, and term buffers. `display-fill-column-indicator-mode` is off in `helm-major-mode`.

Two companion lists extend these minor modes to major modes that do **not** derive from `prog-mode` or `text-mode`:

- `my:custom-prog-mode-list`: `nil` by default. Add a major-mode symbol to this list, not a hook symbol, to give that mode the prog-mode minor modes.
- `my:custom-text-mode-list`: `'(conf-mode)` by default. Add a major-mode symbol to this list to give that mode the text-mode minor modes.

`config/company.el` also reads `my:custom-prog-mode-list` directly. As a result, a mode that you add to this list also gets the code-aware company backend split. See `config/company.el` for the implementation.

### `plugins/use-package-ensure-system-package+`

This is the most complex local plugin. It serializes every `:ensure-system-package` install command through one persistent `/bin/bash` process. This prevents races between package managers that run at the same time.

The plugin exposes the hook `upesp+:command-executed-hook`. It calls this hook with the completed command string after each install. `config/vterm.el` uses this hook to block vterm module compilation until its system dependencies (`gcc`, `cmake`, `libtool`) finish their install.

For internal details, see `elpa/use-package-ensure-system-package+/CLAUDE.md`.

`init.el` installs this plugin directly through `:vc`, before the `plugins/` load and the `config/` load. As a result, the plugin is always available before any `config/*.el` file that uses `:ensure-system-package`.

## Forcing Plugin Recompilation

```sh
rm plugins/<name>/.installed
```

Emacs will recompile and make new autoloads for that plugin on the next launch.

## Notable dependencies

`init.el` installs three packages through `:vc`, in the same way as any other package:

- `block-travel`: VS Code-style block navigation. `config/editor.el` binds it to `M-p` and `M-n`.
- `rfcview`: declared in `config/rfcview.el`. `init.el` fetches it from the author's own fork.
- `use-package-ensure-system-package+`.

These packages go into `elpa/`. Git ignores this directory.

Some `:vc`-installed packages under `elpa/` ship their own `CLAUDE.md` file. One example is `elpa/use-package-ensure-system-package+/CLAUDE.md`, which gives guidance for that package only. Claude Code loads that guidance automatically when you work in one of those directories.

## Building / Compiling

This Emacs configuration has no top-level build step. For compilation inside Emacs, use these keys:

| Key | Command |
|-----|---------|
| `<f7>` | `my:compile`: If a compilation buffer already exists, calls `recompile`. If not, opens the interactive prompt. |
| `C-<f7>` | Always opens the interactive `compile` prompt. |

`config/compile.el` defines these commands.

## Search keybindings

| Key | Command | Defined in |
|-----|---------|------------|
| `M-r` | `helm-occur`: in-buffer incremental search | `config/helm.el` |
| `M-R` | `helm-do-grep-ag`: grep across a directory tree | `config/helm.el` |
| `C-M-r` | `my:helm-do-grep-vc-root-ag`: same, rooted at the VCS repo root | `config/helm.el` |
| `C-M-S-r` | `helm-grep-do-git-grep`: `git grep` through helm | `config/helm.el` |
| `C-M-R` | `ag` (standalone `ag.el`): `C-M-S-r` shadows this binding. Both use the same key. `helm.el` loads after `ag.el` in alphabetical order. As a result, this binding never takes effect. | `config/ag.el` |

`config/xcscope.el` loads `xcscope`. `xcscope` installs its own `C-c s` keymap for C/C++ symbol cross-reference, through `cscope-setup`. `my:global-key-map` has no entry for this keymap.

## Key Files

| File | Purpose |
|------|---------|
| `init.el` | Entry point. It manages the whole load order. |
| `workaround.el` | Early-loaded fixes for upstream bugs. |
| `config/theme.el` | Mode line settings. |
| `config/lsp.el` | LSP settings. It also holds hooks that install language servers. |
| `config/vterm.el` | vterm setup. It stops module compilation until system dependencies install, through `upesp+:command-executed-hook`. |
| `config/editor.el` | Small commands that make editing easier. |
| `config/window.el` | All window and frame navigation keybindings. |
| `config/fileviewer.el` | Integration for external file and URL viewers. It finds the host type (WSL, SSH-remote, or local) and sends dired's `V` command, `browse-url`, and `mailcap` viewers to the correct tool. |
| `config/claude.el` | Claude Code integration, through `claude-code.el`. The keybinding prefix is `C-'`. It uses `monet` for IDE server communication and `inheritenv` for environment propagation. It opens Claude in a window on the right side. Note: the `:custom` block for `monet-diff-tool` and `monet-diff-cleanup-tool` is commented out. As a result, `monet` keeps its own default diff tool. |
| `config/pdf.el` | PDF viewing, through `pdf-tools`. It needs the system package `epdfinfo`. Install this package with `sudo apt install -y elpa-pdf-tools-server`. |
| `config/rust.el` | rust-mode settings. It includes commands to manage cargo dependencies, for example `my:rust-add-dependency`, which searches the cargo registry. |
| `config/agent-shell.el` | agent-shell integration. |
| `config/flycheck.el` | flycheck settings. |
| `misc/edit` | A smart `emacsclient` wrapper. Set `$EDITOR` to this file. |
| `.dir-locals.el` | It sets `fill-column` to 80 for the whole project. In `emacs-lisp-mode`, it registers a `write-contents-functions` hook. This hook removes trailing whitespace on every save. |

## Conventions to remember when editing this repo

- **Trailing whitespace is removed automatically when you save an `.el` file**, through `.dir-locals.el`. Expect diffs that change only line endings.
- **Projectile's project search root is `~/Workspace`.** `helm-projectile` starts its search there.
- **The repository has no git submodules.** Earlier versions of this repository used submodules for `block-travel`, `magit-gerrit`, and `company-tern`. The project later dropped all of these, for `:vc`-installed packages. Do not add `.gitmodules` again.
- **`rfcview` is a normal `:vc` package.** `config/rfcview.el` declares it. `init.el` fetches it from `https://github.com/zeph1e/rfcview.el`, the author's own fork. `rfcview` is no longer a developer-local block at the end of `init.el`.
