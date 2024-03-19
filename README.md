# letterblasteR <a href="https://letterblasterpress.github.io/letterblasteR/"><img src="man/figures/logo.png" alt="letterblasteR website" align="right" height="139"/></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/LetterblasterPress/letterblasteR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LetterblasterPress/letterblasteR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/LetterblasterPress/letterblasteR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/LetterblasterPress/letterblasteR?branch=main)

<!-- badges: end -->

The goal of letterblasteR is to facilitate typesetting ready-to-print editions
of public domain texts. The templates and utility functions provided by
letterblasteR are generic and modular enough to be used in any way you see fit,
but they have been designed with the following workflow in mind.

1.  Online repositories like the [Internet Archive](https://archive.org) and
    [Google Books](https://books.google.com) host scans of physical book pages
    as well as optical character recognition (OCR) data for each page.

2.  As works enter the public domain in the United States, [Distributed
    Proofreaders](https://www.pgdp.net) volunteers proofread the text multiple
    times to correct scanning errors and standardize basic formatting.

3.  [Project Gutenberg](https://www.gutenberg.org) volunteers convert proofread
    public domain works into standard formats (e.g. plain text, HTML, epub).

    *At this stage, the text has usually undergone several rounds of
    proofreading by multiple people. Generally, texts accurately represent the
    source material and are free of scanning errors and typos. However,
    headings, tables, figures, and other elements require additional markup
    before typesetting the work for print. This is where* letterblasteR *comes
    in!*

4.  Details are subject to change while letterblasteR is under early
    development, but the general idea is that a volunteer...

    -   Selects a Project Gutenberg work to convert and requests the creation of
        a new [Letterblaster Press](https://github.com/LetterblasterPress)
        repository for the project.
    -   Uses letterblasteR templates and utilities to combine multiple versions
        of the work (e.g. HTML, epub; different editions) into a single source
        document formatted in [Pandoc's
        Markdown](https://pandoc.org/MANUAL.html#pandocs-markdown). This ensures
        that all available markup (e.g. emphasized text) appears in the final
        text.
    -   Adds, edits, and standardizes semantic markup for headings, tables,
        figures, inline formatting, and special environments like poetry,
        letters, & mathematics. This is a manual and iterative process, but
        letterblasteR templates and utilities help streamline the process.
    -   Extends letterblasteR templates to define the structure and formatting
        of the finished book(s).
    -   Renders PDF(s) and uses letterblasteR utilities to
        [impose](https://en.wikipedia.org/wiki/Imposition) ready-to-print
        versions of each PDF.

## Guiding principles

The following principles will guide early design decisions as this project gets
off the ground:

-   Objective: produce ready-to-print PDFs of public domain works with the
    amateur bookbinder in mind

    -   One work can have many designs, each with its own physical dimensions,
        formatting, and style. Provide a page-level (no imposition) PDF for each
        design. Use vdiff tools to proofread every two-page spread of the design
        PDF.

    -   Each design can have many imposed PDFs to accommodate different paper
        stock sizes. Generally, the amateur bookbinder requires imposed PDFs for
        stock sizes of roughly A3–A4. Imposed PDFs should note ideal grain
        direction, providing options for short- and long-grain paper if
        possible. Signature sizes should be optimized for each imposition.

    -   Design PDFs for printing.

        -   Forgo markup intended for electronic formats such as hyperlinks and
            screen reader hints; disable PDF features to reduce bloat. (For
            example, marking "Mr." as an abbreviation is useful for a
            screen-reader, but serves no purpose in print. However, marking up
            "NASA" as an acronym or "FBI" as an initialism is desired because
            they may require special typesetting like small caps.)

        -   Use embedable fonts and embed them into PDFs. Do not add fonts to
            source control.

-   Use [Pandoc's Markdown](https://pandoc.org/MANUAL.html#pandocs-markdown) as
    the source of truth. Favor semantic markup over formatting markup.

    -   If it can be represented using Pandoc's Markdown, do so.

    -   Otherwise, wrap the semantic element in a [fenced
        div](https://pandoc.org/MANUAL.html#extension-fenced_divs) or [bracketed
        span](https://pandoc.org/MANUAL.html#extension-bracketed_spans) and use
        a [Pandoc filter](https://pandoc.org/filters.html) to wrap the element
        in a custom LaTeX environment or pass the element to a custom LaTeX
        function, respectively.

    -   Otherwise, consider using [Pandoc template
        partials](https://pandoc.org/MANUAL.html#templates). For example, a book
        with a complex chapter page containing pre-title text, a title,
        subtitle, epigraph, and image could be broken into chapter-level files,
        each with a yaml metadata block containing the aforementioned elements.
        A root template would then include each chapter file, rendering it with
        a partial template that takes care of typesetting each complex chapter
        page.

    -   As a last resort – and let's really try to avoid this – a raw LaTeX
        chunk can be used as long as there is a corresponding non-LaTeX chunk to
        use as a fallback in the event that our source markdown files are
        rendered to another format.

    -   The ideal markdown document is free of external markup (HTML, LaTeX,
        etc.), free of formatting markup (e.g. small caps), and limited to UTF-8
        text and semantic markup (e.g. letter environment, acronym). All
        formatting details are factored out into Pandoc template(s) for each
        design. letterblasteR should provide generators to customize boilerplate
        templates for common patterns.

-   Aim to produce a professional-quality typeset. Use the most advanced fonts
    and typographic features available, and get fussy with hyphenation.

-   Let The Elements of Typographic Style and The Visual Display of Quantitative
    Information guide design decisions. Design decisions should honor the
    original work and the era in which it was produced, but favor good
    typography over precise historical accuracy or facsimile of the original
    work. Allow typesetting software to alter and fine-tune spacing, but
    otherwise honor archaic punctuation, spelling, and formatting.

-   Report text errors back to the source ([Distributed
    Proofreaders](https://www.pgdp.net), [Project
    Gutenberg](https://www.gutenberg.org), etc.).

-   Follow conventions of reproducible research so that new versions can be
    recompiled with minimal effort. Capture common transformations in modular
    semantic functions (e.g. `drop_div(".book-metadata")`,
    `drop_null_anchors()`). Create custom Pandoc filters, and use them liberally
    before checking in new source to avoid massive diffs for inconsequential
    differences like spacing and word wrapping.

## Installation

You can install the development version of letterblasteR like so:

``` r
devtools::install_github("LetterblasterPress/letterblasteR")
```

## Example

Coming soon...

``` r
library(letterblasteR)
## basic example code
```
