## Release
### Synopsis
We created ```release``` to simplify, standardize and to speed up creation of
branches, tags, commit-messages and ```pom.xml``` modifications.
It also checks your ```pom.xml``` for problems and suggests hints to fix them.

### Features
* no -SNAPSHOTS in releases
* matching release major versions of ishop-core
* create branches
* modify GAV (GroupId ArtifactId Version)
* suggest rebase
* smart suggest of next/current version
* handle local changes
* support different shell variants (gitbash, cygwin, linux, ...)
* ...
* release from master/feature/detached HEAD/...

### Usage
This projects contains an executable called ```release```. To use it, checkout this
project and execute it in your project.

e.g.
```
cd the/project
${HOME}/git/release/release
```
This assumes you checked out "release" in ```${HOME}/git```. It is also possible
to add ```release``` to your ```PATH``` and call it without an absolute path.
```
cd the/project
release
```

If you want to learn other options of ```release``` try ```release --help```.
