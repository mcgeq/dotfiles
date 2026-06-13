+++
title = "Links outside the same post"
tags = ["links"]
draft = false
+++

`ox-hugo` Issue #[30](https://github.com/kaushalmodi/ox-hugo/issues/30)


## External links with search options <span class="tag"><span class="external_links">external-links</span></span> {#external-links-with-search-options}

Links between documents can contain some search options. Only links
to a heading with a **:CUSTOM_ID** property will be resolved to the
appropriate location in the linked file. Links to headings and
links to targets will be resolved to the containing file.

-   [Link to CUSTOM_ID]({{< relref "issue-556#heading-xyz" >}})
-   [Link to a heading]({{< relref "issue-556#heading-abc" >}})
-   Links to Org Targets: [here]({{< relref "issue-556#org-target--paragraph-2" >}}) and [here]({{< relref "issue-556#paragraph-3" >}})


## Internal links <span class="tag"><span class="internal_links">internal-links</span></span> {#internal-links}

Internal links point to targets in the current subtree that will be
exported to the same Hugo post as the link source. To handle links to
an **:ID** property, the `org-id` feature must first be loaded, either
through `org-customize` or by adding `(require 'org-id)` in your Emacs
init file.


## Cross-post links <span class="tag"><span class="crosspost_links">crosspost-links</span></span> {#cross-post-links}

Cross-post links are internal links pointing to targets in a different
subtree that will be exported to another Hugo post than the link
source in subtree-based exports. The Hugo's `ref` and `relref`
shortcodes only supports anchors to headings, so links to a heading,
a **:CUSTOM_ID** property, or an **:ID** property will be resolved to the
appropriate location in the linked file, but links to targets will be
resolved to the containing post.

<span class="timestamp-wrapper"><span class="timestamp">&lt;2025-02-11 Tue&gt; </span></span> Below section throws this error

```text
Error: user-error ("Org export aborted.  Unable to resolve link: \"posts/link-destination.pre-processed.org::#external-target\"
```


## Internal target {#internal-target}

<span class="org-target" id="org-target--internal-target-link"></span>
