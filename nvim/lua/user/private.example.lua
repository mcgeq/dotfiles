-- Copy this file to `private.lua` (git-ignored) and fill in your own
-- machine-specific values. Anything in this table overrides the defaults
-- from `user.lang`. Keep secrets and local paths out of git here.
return {
  -- Hugo blogging: enable the feature and point at your Hugo site root.
  -- Kept private so blog activation + directory paths never land in git.
  hugo = {
    enabled = true,
    blog_path = "D:\\path\\to\\your-hugo-blog",
  },
}
