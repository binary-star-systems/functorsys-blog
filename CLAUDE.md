# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

**Development (with Nix):**

```sh
nix develop              # Enter dev shell with all tools (cabal, hlint, HLS, etc.)
nix build                # Build the SSG binary (output: result/bin/website)
result/bin/website watch # Hot-reloading dev server for Typst content
nix run                  # Preview production site at localhost:8000 (no hot reload)
```

**Development (in dev shell or with just):**

```sh
just dev                 # Run `cabal run website -- watch`
just                     # Rebuild: sync typst package + `cabal run website -- rebuild`
just sync-typst-package  # Sync local html-shim package to ~/.cache/typst/packages
```

**Checks and Formatting:**

```sh
nix build .#checks.x86_64-linux.pre-commit-check  # Run all checks (hlint, treefmt, etc.)
nix fmt                  # Format all files (Nix, Haskell, Typst, etc.)
```

Note: `nix flake check` fails due to haskell.nix IFD issues; use pre-commit-check instead.

## Architecture

This is a Hakyll-based static site generator template that uses Typst for content authoring.

**Core Pipeline:**

1. **Typst files** in `root/` and `posts/` are compiled to HTML via `typst-html-wrapper`
2. **Blaze-html templates** (`src/Templates.hs`) wrap the Typst HTML output
3. **Tailwind CSS** processes `css/main.css` at build time
4. **minhtml** optimizes all final HTML output

**Source Modules:**

- `Main.hs`: Hakyll rules defining routes and compilation pipeline
- `Compilers.hs`: Typst compiler integration, Tailwind processor, HTML minification
- `Templates.hs`: Blaze-html page templates (defaultTemplate, archivePage, indexPage)
- `Utils.hs`: Routing helpers, context utilities, feed generation
- `Constants.hs`: Site configuration (name, root URL, feed config) - edit this to customize
- `BlazeSupport.hs`: Custom HTML attributes for blaze-html
- `Types.hs`: Type aliases for feed rendering

**Content Structure:**

- `root/*.typ`: Top-level pages (compiled to `/*.html`)
- `posts/*.typ`: Blog posts (compiled to `/postname/index.html`)
- `css/main.css`: Tailwind source CSS
- `static/`: Static assets copied as-is

**Key Patterns:**

- Routes use `reroute` helper with transformers like `toRootHTML`
- Index and archive pages are generated in Haskell (see `indexPage`, `archivePage` in Templates.hs)
- LiquidHaskell is enabled as a GHC plugin for type refinements

## External Tools Required

The SSG binary needs these in PATH (provided by Nix wrapper):

- `typst` with packages: fletcher, cetz, oxifmt, cmarker, showybox, html-shim, bullseye
- `typst-html-wrapper` (generated shell script)
- `tailwindcss` (v4)
- `minhtml`
