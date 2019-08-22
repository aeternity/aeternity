# About this release

[This][this-release] is the first Lima release candidate. It marks the freeze of the Lima consensus protocol and of the user API.
It:

* Does all the things mentioned temporarily in files [/docs/release-notes/next/PT-*.md](/docs/release-notes/next/).

TODO: When preparing the release, concatenate all `/docs/release-notes/next/*` Markdown files and place them in this file. (Hint: you can use auxiliary script `scripts/cat-files-in-directory-sorted-by-committer-date` and command `git log -p -w --color-moved`.)

[this-release]: https://github.com/aeternity/aeternity/releases/tag/v5.0.0-rc.1

The node is able to start from a database produced by v4.* releases. For the rest, this release is not backward compatible with v4.* releases.

Please join the mainnet by following the instructions below, and let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
