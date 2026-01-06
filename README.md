# Hakyll + Typst Static Site Template

A static site generator template using [Hakyll](https://jaspervdj.be/hakyll/)
with [Typst](https://typst.app) for content authoring. Derived with technology
from my [own website](https://code.functor.systems/youwen/web), but fully
generic and stripped down.

Especially suited for websites with a lot of long-form, possibly (but not
necessarily) technical text, such as a technical blog, hence its name.
Extremely hackable and customizable.

This is not a turnkey, batteries-included template. If you want to use it
beyond very simple customizations, you will need familiarity with Haskell, as
well as a working understanding of [hakyll](https://jaspervdj.be/hakyll/)
(skimming the short tutorials should be enough).

## Features

- **Typst** for content - write pages and blog posts in Typst's modern markup
- **Hakyll** for build - Haskell-based static site generator
- **Blaze-html** for templates - type-safe HTML generation in Haskell
- **Tailwind CSS** for styling - utility-first CSS with dark mode support
- **Automatic feeds** - RSS, Atom, and JSON feeds generated from posts

## Quick Start

Requires [Nix](https://nixos.org/) with flakes enabled.

```sh
# Build the SSG binary
nix build

# Run hot-reloading dev server (for editing Typst content)
result/bin/website watch

# Preview production build at localhost:8000
nix run
```

## Development

```sh
# Enter development shell with all tools
nix develop

# Or use direnv
direnv allow
```

In the dev shell:

```sh
# Rebuild site
cabal run website -- rebuild

# Watch mode (hot reload, at localhost:8000)
cabal run website -- watch
```

### Justfile

A `justfile` is provided for common tasks. Run `just --list` to see all commands.

```sh
just                     # Sync typst package + rebuild site
just dev                 # Watch mode with hot reload
just sync-typst-package  # Sync html-shim to local Typst cache
just repl                # Start cabal repl
just hoogle              # Run local Hoogle server
```

**Important:** Run `just sync-typst-package` after cloning the repo. This installs
the local `html-shim` Typst package to your cache, which is required for Typst
language servers (e.g. tinymist) to provide completions and live preview.

## Project Structure

```
root/           # Top-level pages (*.typ → /*.html)
posts/          # Blog posts (*.typ → /postname/index.html)
src/            # Haskell source for the SSG
  Main.hs       # Hakyll build rules
  Templates.hs  # Blaze-html page templates
  Constants.hs  # Site configuration
css/main.css    # Tailwind CSS source
```

## Configuration

Edit `src/Constants.hs` to set:
- `siteName` - your site's name
- `siteRoot` - your site's URL
- `siteDescription` - description for feeds
- Feed author name and email

## Adding Content

### Pages

Create `root/pagename.typ`:

```typst
---
title: "Page Title"
---

#import "@preview/html-shim:0.1.0": *
#show: html-shim

Your content here.
```

### Blog Posts

Create `posts/post-slug.typ`:

```typst
---
title: Post Title
description: Brief description for feeds
published: 2025-01-01
---

#import "@preview/html-shim:0.1.0": *
#show: html-shim

Post content here.
```

## Checks

```sh
# Run all checks (formatting, linting, etc.)
nix build .#checks.x86_64-linux.pre-commit-check
```

## License

GPL-3.0-or-later
