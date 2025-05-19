# Ironclad's release procedure

As [our changelog][CHANGELOG.md] says, we follow
[semantic versioning](https://semver.org/).

## Release steps

- Update [our changelog][CHANGELOG.md] to move the `upcoming release` to a
new version, do not make another upcoming release, we make it for the first
release change that would be part of an upcoming release.

- Make a tag with the version number by doing `git tag vX.X.X` and `git push --tags`.

- Make a tarball with `make dist` and sign it and all for distribution, this can
  be done with:

```bash
make dist
gpg -b ironclad-X.X.X.tar.gz
gpg --verify ironclad-X.X.X.tar.gz.sig ironclad-X.X.X.tar.gz # to check
chmod a+r ironclad-X.X.X.tar.gz.sig ironclad-X.X.X.tar.gz
```

- Make announcements at https://blog.ironclad-os.org and other announcements.
